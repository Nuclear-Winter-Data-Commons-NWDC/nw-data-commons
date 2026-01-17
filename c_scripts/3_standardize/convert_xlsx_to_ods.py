#!/usr/bin/env python3
"""
Convert XLSX file to ODS format using pandas.
Reads an Excel file and writes it to ODS format, preserving all sheets.
"""
import sys
import argparse
from pathlib import Path
import pandas as pd


def convert_xlsx_to_ods(xlsx_path, ods_path=None):
    """
    Convert XLSX file to ODS format.

    Args:
        xlsx_path: Path to input XLSX file
        ods_path: Path to output ODS file (defaults to same name with .ods extension)
    """
    xlsx_path = Path(xlsx_path)

    if not xlsx_path.exists():
        raise FileNotFoundError(f"Input file not found: {xlsx_path}")

    if ods_path is None:
        ods_path = xlsx_path.with_suffix('.ods')
    else:
        ods_path = Path(ods_path)

    print(f"[INFO] Converting {xlsx_path} to {ods_path}")

    # Read all sheets from XLSX
    print("[INFO] Reading Excel file...")
    xlsx_file = pd.ExcelFile(xlsx_path, engine='openpyxl')
    sheet_names = xlsx_file.sheet_names
    print(f"[INFO] Found {len(sheet_names)} sheets: {', '.join(sheet_names)}")

    # Write to ODS format
    print("[INFO] Writing to ODS format...")
    with pd.ExcelWriter(ods_path, engine='odf') as writer:
        for sheet_name in sheet_names:
            print(f"[INFO] Converting sheet: {sheet_name}")
            df = pd.read_excel(xlsx_file, sheet_name=sheet_name)
            df.to_excel(writer, sheet_name=sheet_name, index=False)

    print(f"[SUCCESS] Conversion complete: {ods_path}")
    print(f"[INFO] Output file size: {ods_path.stat().st_size / (1024*1024):.2f} MB")

    return ods_path


def main():
    parser = argparse.ArgumentParser(
        description="Convert XLSX file to ODS format"
    )
    parser.add_argument(
        "xlsx_file",
        help="Path to input XLSX file"
    )
    parser.add_argument(
        "--output", "-o",
        help="Path to output ODS file (default: same name with .ods extension)"
    )

    args = parser.parse_args()

    try:
        convert_xlsx_to_ods(args.xlsx_file, args.output)
    except Exception as e:
        print(f"[ERROR] Conversion failed: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
