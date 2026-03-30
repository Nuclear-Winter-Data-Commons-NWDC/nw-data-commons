#!/home/wnf/code/nw-data-commons/.venv/bin/python3
"""
Manual Push to OSF - Upload specific files you select

Use this for one-off pushes when you need to upload specific files to OSF
that aren't part of the normal sync workflow (e.g., new directories created
during development).

Usage:
    # Interactive mode - select from list
    python manual_push.py --local b_data/osf_data_current --remote /

    # Push specific files
    python manual_push.py --files file1.txt file2.csv --local b_data/osf_data_current --remote /

    # Push from a text file list (one file per line)
    python manual_push.py --file-list files_to_push.txt --local b_data/osf_data_current --remote /
"""

import os
import sys
import json
import argparse
import logging
from pathlib import Path
from typing import List, Optional

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


def load_comparison_deleted_files(comparison_json: str) -> List[str]:
    """Load deleted files list from comparison JSON"""
    with open(comparison_json, 'r') as f:
        data = json.load(f)

    deleted = data.get('deleted', [])
    return [f['path'] for f in deleted]


def interactive_selection(files: List[str]) -> List[str]:
    """
    Interactive file selection interface

    Args:
        files: List of file paths to choose from

    Returns:
        List of selected file paths
    """
    print("\n" + "=" * 80)
    print("MANUAL FILE SELECTION FOR OSF PUSH")
    print("=" * 80)
    print(f"\nTotal files available: {len(files)}")
    print("\nFiles:")
    print("-" * 80)

    for i, f in enumerate(files, 1):
        print(f"{i:3}. {f}")

    print("\n" + "=" * 80)
    print("SELECTION OPTIONS:")
    print("  Enter numbers (comma-separated): e.g., 1,3,5-8,12")
    print("  'all' - Select all files")
    print("  'none' - Cancel (select nothing)")
    print("=" * 80)

    user_input = input("\nYour selection: ").strip().lower()

    if user_input == 'none' or not user_input:
        return []

    if user_input == 'all':
        return files

    # Parse number ranges and individual numbers
    selected_indices = set()
    parts = user_input.split(',')

    for part in parts:
        part = part.strip()
        if '-' in part:
            # Range like "5-8"
            try:
                start, end = part.split('-')
                start, end = int(start), int(end)
                selected_indices.update(range(start, end + 1))
            except ValueError:
                logger.warning(f"Invalid range: {part}")
        else:
            # Single number
            try:
                selected_indices.add(int(part))
            except ValueError:
                logger.warning(f"Invalid number: {part}")

    # Convert indices to file paths (1-indexed)
    selected = []
    for idx in sorted(selected_indices):
        if 1 <= idx <= len(files):
            selected.append(files[idx - 1])
        else:
            logger.warning(f"Index {idx} out of range (1-{len(files)})")

    print(f"\n✓ Selected {len(selected)} files")
    return selected


def push_files(manager: OSFManager, files: List[str], local_dir: Path, remote_base: str, dry_run: bool = False):
    """
    Push selected files to OSF

    Args:
        manager: OSFManager instance
        files: List of relative file paths to push
        local_dir: Local base directory
        remote_base: Remote OSF base path
        dry_run: If True, simulate without executing
    """
    remote_base = remote_base.rstrip("/")

    print("\n" + "=" * 80)
    print("PUSHING FILES TO OSF")
    print("=" * 80)
    print(f"\nFiles to upload: {len(files)}")
    if dry_run:
        print("🔍 DRY RUN MODE - No actual changes will be made")
    print("")

    success_count = 0
    fail_count = 0

    for file_path in files:
        local_file = local_dir / file_path
        remote_path = f"{remote_base}/{file_path}"

        if not local_file.exists():
            logger.error(f"Local file not found: {local_file}")
            fail_count += 1
            continue

        if manager.upload_file(local_file, remote_path, dry_run=dry_run):
            success_count += 1
        else:
            fail_count += 1

    print("\n" + "=" * 80)
    print("PUSH COMPLETE")
    print(f"  ✓ Successful: {success_count}")
    print(f"  ✗ Failed: {fail_count}")
    print("=" * 80 + "\n")

    return success_count, fail_count


def main():
    parser = argparse.ArgumentParser(
        description="Manual push of selected files to OSF",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Interactive selection from deleted files in comparison
  python manual_push.py --from-comparison diff_report.json --local b_data/osf_data_current --remote /

  # Push specific files
  python manual_push.py --files "4_3rd_party_metadata/countries.csv" "4_3rd_party_metadata/ports.csv" \\
      --local b_data/osf_data_current --remote /

  # Push from file list
  python manual_push.py --file-list my_files.txt --local b_data/osf_data_current --remote /
        """
    )

    parser.add_argument("--local", required=True, help="Local base directory")
    parser.add_argument("--remote", default="/", help="Remote OSF base path (default: /)")

    # File selection options (mutually exclusive)
    selection_group = parser.add_mutually_exclusive_group(required=True)
    selection_group.add_argument("--from-comparison", metavar="JSON",
                                help="Load deleted files from comparison JSON for selection")
    selection_group.add_argument("--files", nargs='+', metavar="FILE",
                                help="Specific files to push (space-separated)")
    selection_group.add_argument("--file-list", metavar="TXT",
                                help="Text file with file paths (one per line)")

    parser.add_argument("--project", default="e28gq", help="OSF project ID")
    parser.add_argument("--component", default="", help="OSF component ID")
    parser.add_argument("--token", default="", help="OSF token (uses OSF_TOKEN env if empty)")
    parser.add_argument("--dry-run", action="store_true", help="Simulate without executing")
    parser.add_argument("--non-interactive", action="store_true",
                       help="Don't prompt (use with --files or --file-list)")

    args = parser.parse_args()

    # Get token
    token = args.token or os.environ.get("OSF_TOKEN")
    if not token:
        logger.error("OSF token required. Set OSF_TOKEN environment variable or use --token")
        sys.exit(1)

    # Determine files to push
    files_to_push = []

    if args.from_comparison:
        # Load from comparison JSON
        if not Path(args.from_comparison).exists():
            logger.error(f"Comparison file not found: {args.from_comparison}")
            sys.exit(1)

        deleted_files = load_comparison_deleted_files(args.from_comparison)

        if not deleted_files:
            print("No deleted files found in comparison")
            sys.exit(0)

        # Interactive selection
        files_to_push = interactive_selection(deleted_files)

        if not files_to_push:
            print("No files selected")
            sys.exit(0)

    elif args.files:
        # Direct file list from command line
        files_to_push = args.files

    elif args.file_list:
        # Load from text file
        if not Path(args.file_list).exists():
            logger.error(f"File list not found: {args.file_list}")
            sys.exit(1)

        with open(args.file_list, 'r') as f:
            files_to_push = [line.strip() for line in f if line.strip()]

    # Confirm selection if interactive
    if not args.non_interactive and not args.from_comparison:
        print(f"\nFiles to push ({len(files_to_push)}):")
        for f in files_to_push:
            print(f"  • {f}")

        response = input("\nProceed with push? [y/N]: ").strip().lower()
        if response != 'y':
            print("Cancelled")
            sys.exit(0)

    # Create OSF manager
    try:
        manager = OSFManager(args.project, token, args.component or None)
    except Exception as e:
        logger.error(f"Failed to initialize OSF manager: {e}")
        sys.exit(1)

    # Push files
    try:
        success, failed = push_files(
            manager,
            files_to_push,
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
