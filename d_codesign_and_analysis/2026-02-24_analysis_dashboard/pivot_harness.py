#!/usr/bin/env python3
"""
Headless-browser harness for analysis_pivot_v9.html.

Serves the dashboard directory over HTTP (the page uses fetch() for data/*.json,
so it MUST be served, not opened via file://), launches headless Chromium via
Playwright, and drives the REAL page JavaScript to verify pivot behavior without a
manual click-through in VS Code.

It drives the page by setting the page's own globals (pivotConfig, weight-select)
and calling generatePivot(), then reading PIVOT_RESULTS — i.e. it exercises the
same code path the UI uses, just without drag-and-drop.

Checks:
  1. Page loads and auto-loads the default dataset (CURRENT_DATA populated).
  2. "No results" reproduction: empty Rows + empty Columns -> the page's own guard
     message "Add fields to Rows or Columns." (this is what an empty-Rows pivot hits).
  3. Weighted-mean verification: temperature, control (0 Tg), years.elapsed=0,
     months.elapsed=1, the 3 Scandinavian countries, surface.temp weighted by
     country.land.area.sq.km -> expected ~1.4022 (and simple mean ~1.7629).

Run:
  <venv>/bin/python d_codesign_and_analysis/2026-02-24_analysis_dashboard/pivot_harness.py
where <venv> has `playwright` installed and `playwright install chromium` done.

Exit code 0 = all checks passed, 1 = a check failed.
"""

import functools
import http.server
import socketserver
import threading
from pathlib import Path

from playwright.sync_api import sync_playwright

DASHBOARD_DIR = Path(__file__).resolve().parent
PAGE = "analysis_pivot_v9.html"
PORT = 8099

# Expected values for the Scandinavia weighted-mean example (temperature, 0 Tg,
# years.elapsed=0, months.elapsed=1), computed independently from the standardized
# CSV: sum(surface.temp * land.area) / sum(land.area).
EXPECT_WEIGHTED = 1.402218
EXPECT_SIMPLE = 1.762902
TOL = 0.01


def start_server():
    handler = functools.partial(http.server.SimpleHTTPRequestHandler,
                                directory=str(DASHBOARD_DIR))
    httpd = socketserver.ThreadingTCPServer(("127.0.0.1", PORT), handler)
    httpd.daemon_threads = True
    threading.Thread(target=httpd.serve_forever, daemon=True).start()
    return httpd


def build_and_read(page, rows, values, filters, weight_field):
    """Set pivotConfig + weight on the real page, run generatePivot(), return
    {info, cells} where cells maps rowKey -> first-measure numeric result.

    generatePivot() defers the actual buildPivot() into a setTimeout, so we must
    AWAIT completion (result-info updates to 'Table:'/'Add '/'Error') before reading
    PIVOT_RESULTS -- otherwise we'd read stale/empty state."""
    return page.evaluate(
        """async ({rows, values, filters, weightField}) => {
            pivotConfig.rows = rows;
            pivotConfig.columns = [];
            pivotConfig.values = values;             // [{field, agg}]
            pivotConfig.filters = {};
            for (const f in filters) pivotConfig.filters[f] = new Set(filters[f]);
            const ws = document.getElementById('weight-select');
            ws.value = weightField || '';
            const weightUsed = ws.value;
            // Sentinel so we don't read a STALE result-info from a prior call:
            // generatePivot() only updates result-info inside its deferred setTimeout
            // (or synchronously to 'Add fields...' if the empty-zone guard fires).
            const ri = document.getElementById('result-info');
            const SENTINEL = '__harness_waiting__';
            ri.textContent = SENTINEL;
            generatePivot();
            await new Promise((resolve) => {
                const t0 = Date.now();
                (function poll() {
                    if (ri.textContent !== SENTINEL || Date.now() - t0 > 6000) resolve();
                    else setTimeout(poll, 25);
                })();
            });
            const info = document.getElementById('result-info').textContent;
            const out = {};
            if (PIVOT_RESULTS && PIVOT_RESULTS.data) {
                const cols = PIVOT_RESULTS.colKeys;
                for (const rk in PIVOT_RESULTS.data) {
                    const cell = PIVOT_RESULTS.data[rk][cols[0]];
                    out[rk] = cell ? cell.result : null;
                }
            }
            return {info, cells: out, weightUsed};
        }""",
        {"rows": rows, "values": values, "filters": filters, "weightField": weight_field},
    )


def main():
    httpd = start_server()
    base = f"http://127.0.0.1:{PORT}/{PAGE}"
    results = []

    def check(name, ok, detail=""):
        results.append((name, ok, detail))
        print(f"  [{'PASS' if ok else 'FAIL'}] {name}" + (f" -- {detail}" if detail else ""))

    try:
        with sync_playwright() as p:
            browser = p.chromium.launch()
            page = browser.new_page()
            console_errors = []
            page.on("console", lambda m: console_errors.append(m.text) if m.type == "error" else None)
            page.goto(base, wait_until="load")

            # 1. default dataset auto-loads.
            # NOTE: the page declares its globals with `let`/`const` (e.g.
            # `let CURRENT_DATA`), which do NOT become window properties, so we must
            # reference the bare names (they resolve via the global lexical scope).
            page.wait_for_function(
                "() => typeof CURRENT_DATA !== 'undefined' && CURRENT_DATA && CURRENT_DATA.length > 0",
                timeout=20000)
            default_n = page.evaluate("() => CURRENT_DATA.length")
            check("page loads + default dataset populates CURRENT_DATA", default_n > 0, f"{default_n} rows")

            # switch to temperature and wait for it to load
            page.evaluate("() => loadDataset('temperature')")
            page.wait_for_function(
                "() => typeof CURRENT_DATA !== 'undefined' && CURRENT_DATA && CURRENT_DATA.length > 100000 && ALL_FIELDS.includes('surface.temp')",
                timeout=25000)
            page.wait_for_function("() => document.getElementById('weight-select').options.length > 0", timeout=10000)
            temp_n = page.evaluate("() => CURRENT_DATA.length")
            check("temperature dataset loads", temp_n > 0, f"{temp_n} rows")

            # NOTE on filter values: the dashboard keys filter values as
            # String(r[f] || ''), so the *value 0 becomes an empty string* (0 is
            # falsy). The control scenario (0 Tg) and years.elapsed=0 therefore match
            # '' here, not '0' -- and show as "(blank)" in the UI filter list. That
            # is a real dashboard bug (0 conflated with missing); we match the
            # dashboard's actual behavior so the numeric check exercises the real path.
            filters = {
                "soot.injection.scenario": [""],   # 0 Tg (control) -> '' via r[f]||''
                "years.elapsed": [""],             # year 0 -> '' via r[f]||''
                "months.elapsed": ["1"],
                "scandinavian_countries": ["Scandinavian Countries"],
            }

            # 2. reproduce "no results": empty rows + empty columns
            r_empty = build_and_read(
                page, rows=[], values=[{"field": "surface.temp", "agg": "weighted_mean"}],
                filters=filters, weight_field="country.land.area.sq.km")
            check("empty Rows+Columns hits the guard (root cause of 'no results')",
                  "Add fields to Rows or Columns." in r_empty["info"], repr(r_empty["info"]))

            # 3a. weighted mean with a Rows field -> the fix, and the numeric check
            r_w = build_and_read(
                page, rows=["scandinavian_countries"],
                values=[{"field": "surface.temp", "agg": "weighted_mean"}],
                filters=filters, weight_field="country.land.area.sq.km")
            wcell = r_w["cells"].get("Scandinavian Countries")
            check("weighted mean returns a result (interface works with a Rows field)",
                  wcell is not None, f"cell={wcell}")
            check(f"weighted mean == {EXPECT_WEIGHTED} (weight=land.area)",
                  wcell is not None and abs(wcell - EXPECT_WEIGHTED) < TOL,
                  f"got {wcell}")

            # 3b. simple mean for contrast (proves weighting actually changes the value)
            r_s = build_and_read(
                page, rows=["scandinavian_countries"],
                values=[{"field": "surface.temp", "agg": "mean"}],
                filters=filters, weight_field="")
            scell = r_s["cells"].get("Scandinavian Countries")
            check(f"simple mean == {EXPECT_SIMPLE} (contrast)",
                  scell is not None and abs(scell - EXPECT_SIMPLE) < TOL, f"got {scell}")

            check("no uncaught console errors during run", len(console_errors) == 0,
                  f"{len(console_errors)} error(s)" + (f": {console_errors[:2]}" if console_errors else ""))

            browser.close()
    finally:
        httpd.shutdown()

    passed = sum(1 for _, ok, _ in results if ok)
    print(f"\n{passed}/{len(results)} checks passed")
    return 0 if passed == len(results) else 1


if __name__ == "__main__":
    raise SystemExit(main())
