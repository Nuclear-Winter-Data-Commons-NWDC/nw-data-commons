#!/usr/bin/env python3
"""
Upload files to OSF storage using osfclient.
Supports uploading individual files or all standardized outputs.
"""
import os
import json
import argparse
from pathlib import Path

# Optional .env support
try:
    from dotenv import load_dotenv
    load_dotenv(dotenv_path=".env")
except Exception:
    pass

from osfclient import OSF


def upload_file(storage, local_path, remote_path):
    """Upload a single file to OSF storage."""
    local_path = Path(local_path)
    if not local_path.exists():
        print(f"[ERROR] Local file does not exist: {local_path}")
        return False

    print(f"[INFO] Uploading {local_path} -> {remote_path}")
    try:
        with open(local_path, "rb") as fp:
            storage.create_file(remote_path, fp, force=True)
        print(f"[SUCCESS] Uploaded {remote_path}")
        return True
    except Exception as e:
        print(f"[ERROR] Failed to upload {local_path}: {e}")
        return False


def main():
    parser = argparse.ArgumentParser(description="Upload data files to OSF.")
    parser.add_argument("--project", required=True, help="OSF project id")
    parser.add_argument("--component", default="", help="OSF component id (optional)")
    parser.add_argument("--token", default="", help="OSF token (uses env OSF_TOKEN if empty)")
    parser.add_argument("--file", help="Single file to upload (local path)")
    parser.add_argument("--remote-path", help="Remote path on OSF for single file upload")
    parser.add_argument("--standardized-dir", help="Upload all files from standardized output directory")
    parser.add_argument("--remote-dir", default="4_standardized", help="Remote directory on OSF (default: 4_standardized)")
    args = parser.parse_args()

    token = args.token or os.environ.get("OSF_TOKEN")
    if not token:
        raise SystemExit("OSF token missing. Provide via --token or OSF_TOKEN env.")

    osf = OSF(token=token)

    try:
        node = osf.project(args.component) if args.component else osf.project(args.project)
    except Exception as e:
        raise SystemExit(f"Failed to access OSF node. Check IDs and token. Details: {e}")

    storage = node.storage("osfstorage")

    # Single file upload mode
    if args.file:
        if not args.remote_path:
            raise SystemExit("--remote-path is required when uploading a single file")
        upload_file(storage, args.file, args.remote_path)
        return

    # Standardized directory upload mode
    if args.standardized_dir:
        standardized_dir = Path(args.standardized_dir)
        if not standardized_dir.exists():
            raise SystemExit(f"Standardized directory does not exist: {standardized_dir}")

        # Get the timestamp folder name (e.g., "2026-01-16_164901")
        timestamp = standardized_dir.name

        files_to_upload = [
            ("0_standardized_data.xlsx", f"{args.remote_dir}/{timestamp}/0_standardized_data.xlsx"),
            ("starvation.csv", f"{args.remote_dir}/{timestamp}/starvation.csv"),
            ("temperature.csv", f"{args.remote_dir}/{timestamp}/temperature.csv"),
            ("precipitation.csv", f"{args.remote_dir}/{timestamp}/precipitation.csv"),
            ("uv.csv", f"{args.remote_dir}/{timestamp}/uv.csv"),
            ("agriculture.agmip.csv", f"{args.remote_dir}/{timestamp}/agriculture.agmip.csv"),
            ("agriculture.clm.csv", f"{args.remote_dir}/{timestamp}/agriculture.clm.csv"),
            ("fish.catch.csv", f"{args.remote_dir}/{timestamp}/fish.catch.csv"),
            ("sea.ice.csv", f"{args.remote_dir}/{timestamp}/sea.ice.csv"),
        ]

        success_count = 0
        for local_file, remote_path in files_to_upload:
            local_path = standardized_dir / local_file
            if upload_file(storage, local_path, remote_path):
                success_count += 1

        print(f"\n[INFO] Upload complete: {success_count}/{len(files_to_upload)} files uploaded successfully")
        return

    print("[ERROR] Must specify either --file or --standardized-dir")


if __name__ == "__main__":
    main()
