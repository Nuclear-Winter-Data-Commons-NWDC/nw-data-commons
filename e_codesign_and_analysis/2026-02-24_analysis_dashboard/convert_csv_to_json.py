#!/usr/bin/env python3
"""
Convert standardized CSV datasets to JSON for browser-based analysis dashboard.

This script reads CSV files from b_data/osf_data_current/3_standardized/
and converts them to JSON format for client-side loading in the analysis dashboard.

Output: JSON files in e_codesign_and_analysis/2026-02-24_analysis_dashboard/data/

Dataset sizes (rows):
- fish_catch: 18 rows
- agriculture_agmip: 4,290 rows
- starvation: 7,584 rows
- agriculture_clm: 10,080 rows
- sea_ice: 10,752 rows
- uv: 84,960 rows
- surface_solar_radiation: 257,076 rows
- precipitation: 282,552 rows
- temperature: 282,552 rows
"""

import csv
import json
import os
from pathlib import Path

# Configuration
BASE_DIR = Path(__file__).parent.parent.parent
STANDARDIZED_DIR = BASE_DIR / "b_data/osf_data_current/3_standardized"
OUTPUT_DIR = Path(__file__).parent / "data"

# Dataset mappings (use most recent version)
DATASETS = {
    "agriculture_agmip": "agriculture_agmip_v2026-02-23.csv",
    "agriculture_clm": "agriculture_clm_v2026-02-23.csv",
    "fish_catch": "fish_catch_v2026-02-23.csv",
    "precipitation": "precipitation_v2026-02-23.csv",
    "sea_ice": "sea_ice_v2026-02-23.csv",
    "starvation": "starvation_v2026-02-23.csv",
    "surface_solar_radiation": "surface_solar_radiation_v2026-02-20.csv",
    "temperature": "temperature_v2026-02-23.csv",
    "uv": "uv_v2026-02-23.csv",
}

def convert_value(value):
    """Convert string values to appropriate types (float, int, or string)."""
    if value == '' or value is None:
        return None

    # Try to convert to float
    try:
        num = float(value)
        # If it's a whole number, convert to int
        if num.is_integer() and abs(num) < 1e10:
            return int(num)
        return num
    except ValueError:
        # Keep as string if not a number
        return value

def csv_to_json(csv_path, json_path):
    """Convert CSV file to JSON array of objects."""
    data = []

    with open(csv_path, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            # Convert values to appropriate types
            converted_row = {k: convert_value(v) for k, v in row.items()}
            data.append(converted_row)

    with open(json_path, 'w', encoding='utf-8') as f:
        json.dump(data, f, separators=(',', ':'))

    return len(data)

def main():
    # Create output directory
    OUTPUT_DIR.mkdir(exist_ok=True)

    print("Converting standardized CSV files to JSON...")
    print(f"Source: {STANDARDIZED_DIR}")
    print(f"Output: {OUTPUT_DIR}\n")

    results = []

    for dataset_name, csv_filename in DATASETS.items():
        csv_path = STANDARDIZED_DIR / csv_filename
        json_path = OUTPUT_DIR / f"{dataset_name}.json"

        if not csv_path.exists():
            print(f"⚠️  SKIP: {dataset_name} - CSV file not found: {csv_path}")
            continue

        print(f"Processing {dataset_name}...")

        # Get file size before conversion
        csv_size_mb = csv_path.stat().st_size / (1024 * 1024)

        # Convert
        row_count = csv_to_json(csv_path, json_path)

        # Get JSON file size
        json_size_mb = json_path.stat().st_size / (1024 * 1024)

        results.append({
            'name': dataset_name,
            'rows': row_count,
            'csv_mb': csv_size_mb,
            'json_mb': json_size_mb
        })

        print(f"  ✓ {row_count:,} rows | CSV: {csv_size_mb:.2f} MB → JSON: {json_size_mb:.2f} MB\n")

    # Summary
    print("=" * 70)
    print("CONVERSION COMPLETE")
    print("=" * 70)
    print(f"{'Dataset':<25} {'Rows':>10} {'CSV (MB)':>12} {'JSON (MB)':>12}")
    print("-" * 70)

    total_rows = 0
    total_csv_mb = 0
    total_json_mb = 0

    for r in sorted(results, key=lambda x: x['rows']):
        print(f"{r['name']:<25} {r['rows']:>10,} {r['csv_mb']:>12.2f} {r['json_mb']:>12.2f}")
        total_rows += r['rows']
        total_csv_mb += r['csv_mb']
        total_json_mb += r['json_mb']

    print("-" * 70)
    print(f"{'TOTAL':<25} {total_rows:>10,} {total_csv_mb:>12.2f} {total_json_mb:>12.2f}")
    print("=" * 70)

if __name__ == "__main__":
    main()
