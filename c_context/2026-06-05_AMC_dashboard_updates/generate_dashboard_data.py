#!/usr/bin/env python3
"""
Generate dashboard JSON datasets WITH conference demo derived variables.

Reads standardized CSVs from a_data/osf_data_current/3_standardized/ (pulled from
OSF project e28gq) and writes typed JSON to the analysis dashboard data/ dir,
adding the derived variables requested for the 2026-06-05 AMC conference demo:

(a) scandinavian_countries  -- on every dataset that has a `country.name` column
    "Scandinavian Countries" if country.name in Scandinavia proper
    (Denmark, Sweden, Norway), else "Other Countries".
(b) sea_ice only:
    port_of_interest   -- "Port of Interest" if port in {St Petersburg, Hamburg}, else "Other Port"
    ice_rating_binary  -- 0 if sea.ice.thickness.meters < 0.4, else 1
    sea_ice_class      -- DNV ordinal class from thickness (floors / b2-consistent):
                            0            -> "None"
                            0   < t <0.4 -> "E"
                            0.4 <=t <0.6 -> "E1"
                            0.6 <=t <0.8 -> "E2"
                            0.8 <=t <1.0 -> "E3"
                            t  >=1.0     -> "E4"
(c) fish_catch only:
    scandinavian_eez   -- "Scandinavian EEZ" if eez.name in the listed Nordic EEZs, else "Other EEZ"

Column drops (relative-only fish_catch, 2026-07-15):
    fish_catch drops its absolute-magnitude measures so only relative/percent-change
    variables remain in the dashboard field list (see DROP_COLUMNS). Dimension/key
    columns and the *.pct.* measures + their outlier flags are kept.

Re-runnable: refresh the CSVs (b_scripts/0_sync_osf or the OSF API) and re-run.
"""

import csv
import json
from pathlib import Path

BASE_DIR = Path(__file__).resolve().parent.parent.parent
STD_DIR = BASE_DIR / "a_data/osf_data_current/3_standardized"
OUT_DIR = BASE_DIR / "d_codesign_and_analysis/2026-02-24_analysis_dashboard/data"

# dashboard dataset name -> standardized CSV filename
DATASETS = {
    "agriculture_agmip": "agriculture_agmip_v2026-02-13.csv",
    "agriculture_clm": "agriculture_clm_v2026-02-13.csv",
    "fish_catch": "fish_catch_v2026-07-15.csv",
    "precipitation": "precipitation_v2026-02-13.csv",
    "sea_ice": "sea_ice_v2026-02-13.csv",
    "starvation": "starvation_v2026-02-13.csv",
    "surface_solar_radiation": "surface_solar_radiation_v2026-02-20.csv",
    "temperature": "temperature_v2026-02-13.csv",
    "uv": "uv_radiation_v2026-02-13.csv",
}

# Scandinavia proper = Denmark, Sweden, Norway (Finland & Iceland are Nordic, not Scandinavian)
SCANDINAVIAN_COUNTRIES = {"Norway", "Sweden", "Denmark"}
# Scandinavia + Finland = Denmark, Sweden, Norway, Finland (excludes Iceland; Greenland
# is not present in any country-level dataset, so nothing to exclude there).
SCANDINAVIA_PLUS_FINLAND = {"Norway", "Sweden", "Denmark", "Finland"}
PORTS_OF_INTEREST = {"St Petersburg", "Hamburg"}
# Scandinavia proper = Denmark, Sweden, Norway; joint regime area excluded.
SCANDINAVIAN_EEZ = {"Danish EEZ", "Norwegian EEZ", "Swedish EEZ"}

# Per-dataset columns to drop before writing JSON.
# fish_catch: keep only relative/percent-change measures (+ dimensions & outlier
# flags); drop the absolute-magnitude measures. `mean.catch.per.1000.sq.km` is a
# catch density (absolute per unit area), not a percent change, so it is dropped too.
# `eez.area` is a dimension (unit descriptor) and is kept.
DROP_COLUMNS = {
    "fish_catch": {
        "mean.catch",
        "mean.catch.per.1000.sq.km",
        "mean.catch.change",
        "std.dev.catch",
        "std.dev.catch.change",
    },
}


def convert_value(value):
    """Match the original convert_csv_to_json.py typing: float/int/str/None."""
    if value == '' or value is None:
        return None
    try:
        num = float(value)
        if num.is_integer() and abs(num) < 1e10:
            return int(num)
        return num
    except ValueError:
        return value


def sea_ice_class(t):
    if t == 0:
        return "None"
    if t < 0.4:
        return "E"
    if t < 0.6:
        return "E1"
    if t < 0.8:
        return "E2"
    if t < 1.0:
        return "E3"
    return "E4"


def add_derived(name, row):
    """Add the demo derived variables to a single (typed) row dict."""
    if "country.name" in row:
        row["scandinavian_countries"] = (
            "Scandinavian Countries" if row["country.name"] in SCANDINAVIAN_COUNTRIES
            else "Other Countries"
        )
        row["scandinavia_plus_finland"] = (
            "Scandinavia + Finland" if row["country.name"] in SCANDINAVIA_PLUS_FINLAND
            else "Other Countries"
        )
    if name == "sea_ice":
        row["port_of_interest"] = (
            "Port of Interest" if row.get("port") in PORTS_OF_INTEREST else "Other Port"
        )
        t = row.get("sea.ice.thickness.meters")
        t = float(t) if t is not None else 0.0
        row["ice_rating_binary"] = 0 if t < 0.4 else 1
        row["sea_ice_class"] = sea_ice_class(t)
    if name == "fish_catch":
        row["scandinavian_eez"] = (
            "Scandinavian EEZ" if row.get("eez.name") in SCANDINAVIAN_EEZ else "Other EEZ"
        )
    return row


def main():
    OUT_DIR.mkdir(exist_ok=True)
    print(f"Source: {STD_DIR}\nOutput: {OUT_DIR}\n")
    for name, fname in DATASETS.items():
        src = STD_DIR / fname
        if not src.exists():
            print(f"  SKIP {name}: missing {fname}")
            continue
        drop = DROP_COLUMNS.get(name, set())
        rows = []
        with open(src, newline='', encoding='utf-8') as f:
            for r in csv.DictReader(f):
                row = {k: convert_value(v) for k, v in r.items() if k not in drop}
                rows.append(add_derived(name, row))
        out = OUT_DIR / f"{name}.json"
        with open(out, 'w', encoding='utf-8') as f:
            json.dump(rows, f, separators=(',', ':'))
        mb = out.stat().st_size / (1024 * 1024)
        print(f"  OK {name:<26} {len(rows):>8,} rows  -> {mb:7.2f} MB")
    print("\nDONE")


if __name__ == "__main__":
    main()
