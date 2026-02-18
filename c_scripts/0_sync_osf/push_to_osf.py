#!/home/wnf/code/nw-data-commons/.venv/bin/python3
"""
Push Changes to OSF - Upload new/updated files, delete removed files

Takes selections JSON and pushes all changes to OSF:
- Uploads new files
- Uploads updated files (overwrites)
- Deletes files that were removed locally

Usage:
    python push_to_osf.py --selections selections.json --local b_data/osf_data_current --remote /
"""

import os
import sys
import json
import argparse
import logging
from pathlib import Path
from typing import Dict, List

# Add parent directory to path for osf_manager import
sys.path.insert(0, str(Path(__file__).parent.parent / "1_download_or_extract"))

try:
    from dotenv import load_dotenv
    load_dotenv(dotenv_path=".env")
except ImportError:
    pass

from osf_manager import OSFManager

logging.basicConfig(
    level=logging.INFO,
    format='%(levelname)s: %(message)s'
)
logger = logging.getLogger(__name__)


def push_changes(manager: OSFManager, selections: Dict, local_dir: Path, remote_base: str, dry_run: bool = False):
    """
    Push all selected changes to OSF

    Args:
        manager: OSFManager instance
        selections: User selections dictionary
        local_dir: Local base directory
        remote_base: Remote OSF base path
        dry_run: If True, simulate without executing
    """
    new_files = selections.get('new', [])
    updated_files = selections.get('updated', [])
    deleted_files = selections.get('deleted', [])

    remote_base = remote_base.rstrip("/")
    total_operations = len(new_files) + len(updated_files) + len(deleted_files)

    print("\n" + "=" * 80)
    print("PUSHING CHANGES TO OSF")
    print("=" * 80)
    print(f"\n📊 Summary:")
    print(f"   New files to upload: {len(new_files)}")
    print(f"   Updated files to upload: {len(updated_files)}")
    print(f"   Deleted files to remove: {len(deleted_files)}")
    print(f"   Total operations: {total_operations}")
    print("")

    if dry_run:
        print("🔍 DRY RUN MODE - No actual changes will be made")
        print("")

    success_count = 0
    fail_count = 0

    # Upload new files
    if new_files:
        print("✨ Uploading new files...")
        print("-" * 80)
        for file_path in new_files:
            local_file = local_dir / file_path
            remote_path = f"{remote_base}/{file_path}"

            # Always replace old versions: prevents stale dated files accumulating on OSF
            if manager.upload_file(local_file, remote_path, dry_run=dry_run,
                                   replace_old_versions=True):
                success_count += 1
            else:
                fail_count += 1
        print("")

    # Upload updated files
    if updated_files:
        print("🔄 Uploading updated files...")
        print("-" * 80)
        for file_path in updated_files:
            local_file = local_dir / file_path
            remote_path = f"{remote_base}/{file_path}"

            # Always replace old versions: prevents stale dated files accumulating on OSF
            if manager.upload_file(local_file, remote_path, dry_run=dry_run,
                                   replace_old_versions=True):
                success_count += 1
            else:
                fail_count += 1
        print("")

    # Delete removed files
    if deleted_files:
        print("🗑️  Deleting removed files...")
        print("-" * 80)
        for file_path in deleted_files:
            remote_path = f"{remote_base}/{file_path}"

            if dry_run:
                logger.info(f"[DRY RUN] Would delete {remote_path}")
                success_count += 1
            else:
                if manager.delete_file(remote_path, confirm=True):  # Auto-confirm in script mode
                    success_count += 1
                else:
                    fail_count += 1
        print("")

    # Summary
    print("=" * 80)
    print("PUSH COMPLETE")
    print(f"  ✓ Successful operations: {success_count}")
    print(f"  ✗ Failed operations: {fail_count}")
    print("=" * 80 + "\n")

    return success_count, fail_count


def main():
    parser = argparse.ArgumentParser(
        description="Push changes to OSF repository",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    parser.add_argument("--selections", required=True, help="Selections JSON file")
    parser.add_argument("--local", required=True, help="Local base directory")
    parser.add_argument("--remote", default="/", help="Remote OSF base path (default: /)")
    parser.add_argument("--project", default="e28gq", help="OSF project ID")
    parser.add_argument("--component", default="", help="OSF component ID")
    parser.add_argument("--token", default="", help="OSF token (uses OSF_TOKEN env if empty)")
    parser.add_argument("--dry-run", action="store_true", help="Simulate without executing")

    args = parser.parse_args()

    # Load selections
    selections_path = Path(args.selections)
    if not selections_path.exists():
        logger.error(f"Selections file not found: {args.selections}")
        sys.exit(1)

    with open(selections_path, 'r') as f:
        selections = json.load(f)

    # Get token
    token = args.token or os.environ.get("OSF_TOKEN")
    if not token:
        logger.error("OSF token required. Set OSF_TOKEN environment variable or use --token")
        sys.exit(1)

    # Create manager
    try:
        manager = OSFManager(args.project, token, args.component or None)
    except Exception as e:
        logger.error(f"Failed to initialize OSF manager: {e}")
        sys.exit(1)

    # Push changes
    try:
        success, failed = push_changes(
            manager,
            selections,
            Path(args.local),
            args.remote,
            dry_run=args.dry_run
        )

        if failed > 0:
            sys.exit(1)

    except Exception as e:
        logger.error(f"Push failed: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
