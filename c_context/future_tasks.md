# Future Tasks for Nuclear Winter Data Commons

Short-horizon worklist plus a memory-jogging backlog. Each backlog item records
**what** to do, **why** it matters, and **where/when it first came up** (traced to
the session summaries) so it's easy to recall the original motivation.

**Last Updated:** 2026-07-15
**Maintainer:** William Faulkner

---

## This session (~1 hour, do these)

### 1. fish_catch — drop absolute measures (dashboard + UPSTREAM + OSF)
- **Status (dashboard):** ✅ Done 2026-07-15 — dropped the 5 absolute columns in the
  generator; regenerated `data/fish_catch.json` (22,464 rows, 10 fields).
- **Status (upstream):** ✅ Code done 2026-07-15 — edited the `select()` in
  `b_scripts/3_standardize/fish_catch_cleaning.R` so future pipeline runs emit the
  relative-only 9-column standardized dataset. Corrected standardized CSV produced
  as a column projection of `fish_catch_v2026-02-13.csv` (byte-faithful; strings
  quoted, numbers bare) — staged in the scratchpad as `fish_catch_v2026-07-15.csv`.
- **Status (OSF upload):** ✅ DONE 2026-07-15 — uploaded
  `3_standardized/fish_catch_v2026-07-15.csv` to OSF project e28gq and deleted the
  old `fish_catch_v2026-02-13.csv` (via `osf_manager.py upload --replace-old-versions`).
  Verified on OSF: new file present (9 cols, 22,464 rows, absolutes absent), old gone.
- **Columns:** DROP `mean.catch`, `mean.catch.per.1000.sq.km`, `mean.catch.change`,
  `std.dev.catch`, `std.dev.catch.change`. KEEP dims (`eez.name`, `eez.num`,
  `eez.area`, `years.elapsed`, `soot.injection.scenario`), relative measures
  (`mean.pct.catch.change`, `std.dev.pct.catch.change`) + their 2 outlier flags.
- **Why / origin:** logged 2026-07-09 while cleaning up the dashboard field list —
  absolute catch numbers aren't the analysis focus and clutter the variable picker.

### 2. Pivot filter — preserve scroll position on select/deselect
- **Status:** Not started (logged 2026-07-09).
- **What:** When toggling a value inside a filter variable, don't jump the filter
  list's scroll back to the top. Update only the toggled item's `.selected` class
  (or capture/restore `scrollTop` around the re-render) instead of rebuilding the
  whole list.
- **Files:** `toggleFilterValue`, `selectAllFilterValues`, `deselectAllFilterValues`,
  `renderZone` in `analysis_pivot_v9.html`.
- **Why now / origin:** you hit this on 2026-07-09 while selecting several countries
  in a long alphabetical filter (the exact workflow behind the Scandinavia+Finland
  charts, now done) — each click scrolled you back to the top, making
  multi-selection tedious. `renderZone('filters')` currently rebuilds the entire
  list on every toggle. Pure UI polish, no data dependency.

### 3. Headless-browser harness for the dashboard  ✅ DONE (2026-07-15)
- **Status:** Built and passing 7/7. Script:
  `d_codesign_and_analysis/2026-02-24_analysis_dashboard/pivot_harness.py` — serves
  the dashboard over HTTP, launches headless Chromium (Playwright), drives the real
  page JS (sets `pivotConfig` + weight, calls `generatePivot()`, reads
  `PIVOT_RESULTS`), and asserts behavior.
- **"No results" root cause FOUND:** not a dashboard bug — the pivot bails out when
  **both Rows and Columns are empty** ([analysis_pivot_v9.html:626]) with "Add fields
  to Rows or Columns." My earlier spot-check told you to leave both empty; that was
  the cause. Fix = put a field in Rows.
- **Env note:** the harness runs from a **scratchpad venv** (Playwright + Chromium),
  which is session-ephemeral. To keep it, give it a permanent home + a
  requirements/README (see pending tasks). System libs were installed via
  `sudo playwright install-deps chromium`.
- **Origin:** logged 2026-06-05 / 2026-07-09 (every dashboard fix ended "needs user's
  in-browser retest" because this env had no browser).

### 4. Manual verification of weighted-average calculations  ✅ VERIFIED (2026-07-15)
- **Status:** Done — verified through the real interface via the harness. Temperature,
  0 Tg, year 0, month 1, the 3 Scandinavian countries, `surface.temp` weighted by
  `country.land.area.sq.km`: dashboard returned **1.40222**, matching the independent
  hand calculation **1.402218** (and simple mean **1.76290** vs 1.762902). The
  weighted-aggregation math is correct.
- **Still worth doing manually once:** a second dataset (e.g. fish_catch weighted by
  `eez.area`) for breadth — instructions provided in chat; accounts for the filter
  quirk below.
- **Origin:** logged 2026-02-24 as "Manual Verification of Weighted Calculations".

---

## Backlog (described for recall; not planned for this hour)

### Filter bug — value 0 conflated with blank (FOUND 2026-07-15)
- **What:** The dashboard keys filter values as `String(r[f] || '')`
  ([analysis_pivot_v9.html] lines 506, 568, 602, 633), so the numeric value **0
  becomes an empty string**. Result: the control scenario (0 Tg) and `years.elapsed=0`
  show up as **"(blank)"** in filter lists and can't be told apart from genuinely
  missing values; filtering on them matches `''`, not `'0'`.
- **Why it matters:** users filtering to the control/baseline (a very common op) get a
  confusing "(blank)" entry, and 0 is indistinguishable from missing data.
- **Fix idea:** key filter values as `r[f] == null ? '' : String(r[f])` (only
  null/undefined → blank), and render actual 0 as "0" not "(blank)".
- **Origin:** discovered while building the harness (Task 3) — the weighted-mean check
  returned nothing until filters were keyed as `''` for the 0-valued fields.

### Possible bug — weight dropdown stale after dataset switch (LOW CONFIDENCE, 2026-07-15)
- **What:** After switching datasets programmatically via `loadDataset()`, the
  `weight-select` options still listed the *previous* dataset's fields (starvation
  fields were shown while temperature was loaded). Weighting by a shared field
  (`country.land.area.sq.km`) still worked.
- **Uncertainty:** `loadDataset` calls `initializeInterface()` which should repopulate;
  this may be a timing artifact of the harness's programmatic call rather than a real
  UI bug. **Confirm in the actual dropdown UI next session** before treating as real.



### fish_catch — 37 Tg scenario integration
- **What:** Re-run the standardization pipeline on the newer aggregated fish_catch
  source so the dataset includes the 37 Tg soot scenario, then regenerate the CSV
  and dashboard JSON.
- **Why it matters:** every other dataset already spans all 7 scenarios
  (0/5/16/27/37/47/150 Tg); fish_catch is the lone gap. 37 Tg is the "very large
  regional conflict" case, so its absence makes fish_catch inconsistent with the
  other 8 datasets in the dashboard's scenario controls.
- **Origin:** logged 2026-02-24 — a new aggregated fish_catch file *containing* the
  37 Tg scenario was dropped into `.../2_aggregated/fish_catch` that day, but the
  pipeline was never re-run against it, so published `fish_catch_v2026-02-13.csv`
  still omits 37 Tg. Re-flagged as still-open on 2026-06-05 and 2026-07-09.
- **Note:** larger, upstream-data effort (touches the standardization pipeline, not
  just the generator) — not a one-hour task.

### Give the headless harness a permanent home (2026-07-15)
- **What:** The harness (`pivot_harness.py`) is committed, but its Playwright+Chromium
  venv lives in the session scratchpad (ephemeral). Add a `requirements-harness.txt`
  (playwright) + a short README with setup (`python -m venv`, `pip install`,
  `playwright install chromium`, `sudo playwright install-deps chromium`) so it's
  reproducible, and consider wiring it into a test command.
- **Origin:** built during Task 3, 2026-07-15.

### Chart.js offline embedding
- **What:** Inline the Chart.js library into the dashboard HTML instead of loading
  it from a CDN.
- **Why it matters:** v9 is otherwise fully self-contained/offline, but it pulls
  Chart.js from `cdn.jsdelivr.net` — so charts silently break with no internet.
  Removes a demo-day failure mode; low effort.
- **Origin:** flagged 2026-06-05 as an open caveat while prepping the offline
  conference build (the whole reason v9 was made a single self-contained file).

### Large-dataset in-browser performance
- **What:** Pagination / lazy-loading / web workers for the datasets over ~100 MB;
  short-term, pre-load each once before any live demo.
- **Why it matters:** the precipitation (~174 MB), temperature (~160 MB), and
  surface_solar_radiation (~190 MB) JSONs parse fine server-side but can freeze or
  crash a browser tab on load — a real risk during a live talk.
- **Origin:** first noted 2026-02-24 ("Performance Optimization"), re-flagged
  2026-06-05 and 2026-07-09 as an unverified live-demo risk.

### Refactor / modularize analysis_pivot_v9.html
- **What:** Split the dashboard's JavaScript into modules (data loading / UI
  rendering / chart generation) and abstract the pivot-table component, ideally
  behind a build step that still emits one self-contained file.
- **Why it matters:** the dashboard is a single ~1.1 MB HTML file, and editing it
  safely is getting harder (the 2026-07-09 bug fixes required brace-balancing the
  whole file by hand). **Tension:** the single-file design is deliberate for offline
  portability, so any refactor must still produce a buildable self-contained artifact.
- **Origin:** logged 2026-02-24 as "Code Refactoring and Modularization."

---

## Older standing tasks (from the original 2026-01-16 backlog, still open)
- **Real `.xlsx` export** — the CSV export still triggers Excel's "convert to latest
  format" nag; a native `.xlsx` export is the guaranteed fix (noted as the caveat to
  the 2026-07-09 CSV-export fix). Also considered: ODS/Parquet, metadata sidecars.
- **OSF upload protocol** — versioning strategy, pre-upload validation, manual
  approval + rollback, a manifest of what's on OSF, and auto-generated changelogs
  for the standardized outputs.
- **Pipeline hardening** — automated tests for the cleaning scripts, data-quality
  reports after each run, run-time/error logging.
- **Documentation** — data dictionary, written outlier-detection methodology,
  workflow diagrams.

---

## Completed (recent)
- 2026-07-09/07-15 — Dataset-switching fix **confirmed working** in the analysis
  pivot; **Scandinavia + Finland** time × scenario charts **produced** using the new
  per-dataset grouping variable. (Both previously "future"; now done.)
- 2026-07-09 — Restored all 9 dashboard datasets; added `scandinavia_plus_finland`
  grouping var; confirmed Denmark excludes Greenland; CSV-export fix
  (BOM/CRLF/numeric-aware quoting).
- 2026-01-16 — Starvation data integration (7,632 standardized rows).
