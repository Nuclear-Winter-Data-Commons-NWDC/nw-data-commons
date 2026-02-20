#!/usr/bin/env python3
"""
Delete a file from OSF storage.
"""
import os
import argparse

try:
    from dotenv import load_dotenv
    load_dotenv(dotenv_path=".env")
except Exception:
    pass

from osfclient import OSF


def main():
    parser = argparse.ArgumentParser(description="Delete file from OSF.")
    parser.add_argument("--project", required=True, help="OSF project id")
    parser.add_argument("--remote-path", required=True, help="Remote file path to delete")
    args = parser.parse_args()

    token = os.environ.get("OSF_TOKEN")
    if not token:
        raise SystemExit("OSF_TOKEN not found in environment")

    osf = OSF(token=token)
    node = osf.project(args.project)
    storage = node.storage("osfstorage")

    # Find and delete the file
    for file in storage.files:
        if file.path == f"/{args.remote_path}":
            print(f"[INFO] Deleting {file.path}")
            file.remove()
            print(f"[SUCCESS] Deleted {file.path}")
            return

    print(f"[ERROR] File not found: {args.remote_path}")


if __name__ == "__main__":
    main()
