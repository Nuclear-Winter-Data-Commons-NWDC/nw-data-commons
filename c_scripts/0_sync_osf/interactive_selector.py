#!/home/wnf/code/nw-data-commons/.venv/bin/python3
"""
Interactive File Selector - Choose which files to process through pipeline

Reads comparison JSON and presents interactive interface for users to select:
- Which new files to download and process
- Which updated files to update
- Which deleted files to remove

Default behavior:
- All new files: SELECTED
- All updated files: SELECTED
- All deleted files: SELECTED (will be removed after backup)

Usage:
    python interactive_selector.py --input diff_report.json --output selections.json
"""

import json
import sys
import argparse
from pathlib import Path
from typing import Dict, List


def print_file_list(files: List[Dict], category: str, default_selected: bool = True):
    """Print categorized file list with selection status"""
    if not files:
        return

    status = "SELECTED" if default_selected else "NOT SELECTED"
    print(f"\n{category} ({len(files)} files) - Default: {status}")
    print("-" * 80)

    for i, f in enumerate(files[:10], 1):
        path = f['path']
        print(f"  {i}. {path}")

    if len(files) > 10:
        print(f"  ... and {len(files) - 10} more")


def get_user_selection(comparison: Dict, interactive: bool = True) -> Dict:
    """
    Get user selection for which files to process

    Args:
        comparison: Comparison result from compare_osf_local.py
        interactive: If False, use defaults without prompting

    Returns:
        Selection dictionary with user choices
    """
    new_files = comparison.get('new', [])
    updated_files = comparison.get('updated', [])
    deleted_files = comparison.get('deleted', [])

    print("\n" + "=" * 80)
    print("FILE SELECTION FOR PIPELINE PROCESSING")
    print("=" * 80)

    # Print summary
    print(f"\n📊 Files to Review:")
    print(f"   New files (will be downloaded & processed): {len(new_files)}")
    print(f"   Updated files (will be updated & reprocessed): {len(updated_files)}")
    print(f"   Deleted files (will be removed after backup): {len(deleted_files)}")

    # Print file lists
    print_file_list(new_files, "✨ NEW FILES", default_selected=True)
    print_file_list(updated_files, "🔄 UPDATED FILES", default_selected=True)
    print_file_list(deleted_files, "🗑️  DELETED FILES", default_selected=True)

    # Get user input
    if not interactive:
        print("\n✓ Using default selections (all files will be processed)")
        selection = {
            'new': [f['path'] for f in new_files],
            'updated': [f['path'] for f in updated_files],
            'deleted': [f['path'] for f in deleted_files]
        }
    else:
        print("\n" + "=" * 80)
        print("SELECTION OPTIONS:")
        print("  [Enter] - Accept defaults (process all files)")
        print("  'skip-new' - Skip all new files")
        print("  'skip-updated' - Skip all updated files")
        print("  'skip-deleted' - Skip all deleted files")
        print("  'custom' - Custom file-by-file selection (not yet implemented)")
        print("=" * 80)

        user_input = input("\nYour choice [Enter for default]: ").strip().lower()

        selection = {
            'new': [] if user_input == 'skip-new' else [f['path'] for f in new_files],
            'updated': [] if user_input == 'skip-updated' else [f['path'] for f in updated_files],
            'deleted': [] if user_input == 'skip-deleted' else [f['path'] for f in deleted_files]
        }

        # Handle skip options
        if 'skip-new' in user_input:
            selection['new'] = []
        if 'skip-updated' in user_input:
            selection['updated'] = []
        if 'skip-deleted' in user_input:
            selection['deleted'] = []

    # Print final selection
    print("\n" + "=" * 80)
    print("FINAL SELECTION:")
    print(f"  ✨ New files to download: {len(selection['new'])}")
    print(f"  🔄 Updated files to update: {len(selection['updated'])}")
    print(f"  🗑️  Deleted files to remove: {len(selection['deleted'])}")
    print("=" * 80 + "\n")

    return selection


def main():
    parser = argparse.ArgumentParser(
        description="Interactive file selector for pipeline processing",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    parser.add_argument("--input", required=True, help="Input JSON comparison file")
    parser.add_argument("--output", required=True, help="Output JSON selections file")
    parser.add_argument("--non-interactive", action="store_true",
                       help="Use defaults without prompting")

    args = parser.parse_args()

    # Load comparison
    input_path = Path(args.input)
    if not input_path.exists():
        print(f"ERROR: Input file not found: {args.input}")
        sys.exit(1)

    with open(input_path, 'r') as f:
        comparison = json.load(f)

    # Get user selection
    selection = get_user_selection(comparison, interactive=not args.non_interactive)

    # Save selection
    output_path = Path(args.output)
    with open(output_path, 'w') as f:
        json.dump(selection, f, indent=2)

    print(f"✓ Selection saved to: {args.output}")


if __name__ == "__main__":
    main()
