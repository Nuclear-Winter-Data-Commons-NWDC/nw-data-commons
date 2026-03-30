#!/usr/bin/env python3
"""
Interactive Pipeline Dataset Selector

Detects changes in OSF mirror directory and allows user to select which
datasets to run through the standardization pipeline.

Features:
- Compares local files against last pipeline run timestamp
- Highlights new/modified datasets
- Interactive checkbox selection
- Generates run manifest for 00_run_all.R
"""

import os
import sys
import json
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Tuple

try:
    import inquirer
    from inquirer import Checkbox
except ImportError:
    print("ERROR: 'inquirer' package required. Install with: pip install inquirer")
    sys.exit(1)


class PipelineSelector:
    """Manage dataset selection for pipeline execution"""

    def __init__(self, osf_mirror_dir: str = "a_data/osf_data_current/2_aggregated"):
        self.osf_mirror_dir = Path(osf_mirror_dir)
        self.cache_file = Path("a_data/.pipeline_run_cache.json")
        self.last_run_time = self._load_last_run_time()

    def _load_last_run_time(self) -> datetime:
        """Load timestamp of last pipeline run from cache"""
        if self.cache_file.exists():
            try:
                with open(self.cache_file, 'r') as f:
                    data = json.load(f)
                    return datetime.fromisoformat(data.get('last_run_time', '1970-01-01T00:00:00'))
            except:
                pass
        return datetime(1970, 1, 1)  # Default: treat all as new

    def _save_run_time(self, selected_datasets: List[str]):
        """Save current run time and selected datasets to cache"""
        self.cache_file.parent.mkdir(parents=True, exist_ok=True)
        data = {
            'last_run_time': datetime.now().isoformat(),
            'selected_datasets': selected_datasets
        }
        with open(self.cache_file, 'w') as f:
            json.dump(data, f, indent=2)

    def scan_datasets(self) -> Dict[str, Dict]:
        """
        Scan OSF mirror directory for dataset files

        Returns:
            Dictionary mapping dataset name to metadata:
            {
                'dataset_name': {
                    'files': [Path, ...],
                    'latest_mtime': datetime,
                    'status': 'new' | 'modified' | 'unchanged' | 'mixed_files',
                    'file_type': 'csv' | 'excel' | 'mixed' | 'none'
                }
            }
        """
        if not self.osf_mirror_dir.exists():
            print(f"ERROR: OSF mirror directory not found: {self.osf_mirror_dir}")
            sys.exit(1)

        datasets = {}

        # Scan subdirectories (each represents a dataset theme)
        for theme_dir in sorted(self.osf_mirror_dir.iterdir()):
            if not theme_dir.is_dir() or theme_dir.name.startswith('.'):
                continue

            dataset_name = theme_dir.name

            # Find all data files by type
            csv_files = list(theme_dir.glob('*.csv'))
            excel_files = list(theme_dir.glob('*.xlsx')) + list(theme_dir.glob('*.xls'))
            ods_files = list(theme_dir.glob('*.ods'))

            all_data_files = csv_files + excel_files + ods_files

            if not all_data_files:
                continue  # Skip empty directories

            # Detect file type and check for mixed types
            file_types_present = []
            if csv_files:
                file_types_present.append('csv')
            if excel_files:
                file_types_present.append('excel')
            if ods_files:
                file_types_present.append('ods')

            # Determine file type status
            if len(file_types_present) > 1:
                file_type = 'mixed'
                status = 'mixed_files'
            elif 'csv' in file_types_present:
                file_type = 'csv'
            elif 'excel' in file_types_present:
                file_type = 'excel'
            elif 'ods' in file_types_present:
                file_type = 'ods'
            else:
                file_type = 'none'
                status = 'unchanged'

            # Get latest modification time across all files in dataset
            mtimes = [f.stat().st_mtime for f in all_data_files]
            latest_mtime = datetime.fromtimestamp(max(mtimes))

            # Determine status (only if not already set to mixed_files)
            if file_type != 'mixed':
                if latest_mtime > self.last_run_time:
                    status = 'modified' if self.last_run_time.year > 1970 else 'new'
                else:
                    status = 'unchanged'

            datasets[dataset_name] = {
                'files': all_data_files,
                'latest_mtime': latest_mtime,
                'status': status,
                'file_count': len(all_data_files),
                'file_type': file_type
            }

        return datasets

    def generate_selection_prompt(self, datasets: Dict[str, Dict]) -> List[Tuple]:
        """
        Generate interactive selection prompt

        Returns:
            List of (display_name, dataset_name) tuples for inquirer
        """
        choices = []

        # Sort: new/modified first, then unchanged, mixed files last
        priority_order = {'new': 0, 'modified': 1, 'unchanged': 2, 'mixed_files': 99}
        sorted_datasets = sorted(
            datasets.items(),
            key=lambda x: (priority_order[x[1]['status']], x[0])
        )

        for dataset_name, metadata in sorted_datasets:
            status = metadata['status']
            file_type = metadata.get('file_type', 'unknown')
            mtime_str = metadata['latest_mtime'].strftime('%Y-%m-%d %H:%M')
            file_count = metadata['file_count']

            # Format display string
            if status == 'mixed_files':
                marker = "⚠️ "
                display = f"{marker} {dataset_name:<30} [MIXED FILE TYPES - DISABLED]"
            elif status == 'new':
                marker = "🆕"
                display = f"{marker} {dataset_name:<30} [{file_type}, {file_count} files, modified: {mtime_str}]"
            elif status == 'modified':
                marker = "📝"
                display = f"{marker} {dataset_name:<30} [{file_type}, {file_count} files, modified: {mtime_str}]"
            else:
                marker = "  "
                display = f"{marker} {dataset_name:<30} [{file_type}, {file_count} files, modified: {mtime_str}]"

            # For inquirer, we need just the label and value
            choices.append((display, dataset_name))

        return choices

    def select_datasets(self, interactive: bool = True) -> List[str]:
        """
        Dataset selection with checkbox interface (interactive mode) or auto-selection (non-interactive)

        Args:
            interactive: If True, use checkbox UI. If False, auto-select new/modified datasets

        Returns:
            List of selected dataset names
        """
        print("\n" + "="*80)
        print("NUCLEAR WINTER DATA PIPELINE - DATASET SELECTOR")
        print("="*80)

        datasets = self.scan_datasets()

        if not datasets:
            print("\nNo datasets found in OSF mirror directory.")
            print(f"Expected location: {self.osf_mirror_dir}")
            return []

        print(f"\nLast pipeline run: {self.last_run_time.strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"Datasets found: {len(datasets)}")

        # Count by status
        new_count = sum(1 for d in datasets.values() if d['status'] == 'new')
        modified_count = sum(1 for d in datasets.values() if d['status'] == 'modified')
        unchanged_count = sum(1 for d in datasets.values() if d['status'] == 'unchanged')
        mixed_count = sum(1 for d in datasets.values() if d['status'] == 'mixed_files')

        print(f"  🆕 New: {new_count}")
        print(f"  📝 Modified: {modified_count}")
        print(f"  ✓  Unchanged: {unchanged_count}")
        if mixed_count > 0:
            print(f"  ⚠️  Mixed file types (disabled): {mixed_count}")
        print()

        # Pre-select new and modified datasets (exclude mixed file types)
        default_selections = [
            name for name, meta in datasets.items()
            if meta['status'] in ['new', 'modified']
        ]

        # Display warnings for mixed file type datasets
        mixed_datasets = [name for name, meta in datasets.items() if meta['status'] == 'mixed_files']
        if mixed_datasets:
            print("⚠️  WARNING: The following datasets have mixed file types and are DISABLED:")
            for ds_name in mixed_datasets:
                print(f"     - {ds_name}")
            print("     Please ensure each dataset directory contains only CSV OR only Excel files.")
            print()

        # Check if running in interactive terminal
        is_tty = sys.stdin.isatty() and interactive

        if not is_tty:
            print("Non-interactive mode: Auto-selecting new/modified datasets")
            self._display_dataset_table(datasets)
            return default_selections

        # Interactive mode
        choices = self.generate_selection_prompt(datasets)

        # Interactive prompt
        questions = [
            Checkbox(
                'datasets',
                message='Select datasets to process (Space to toggle, Enter to confirm)',
                choices=choices,
                default=default_selections
            )
        ]

        try:
            answers = inquirer.prompt(questions)
            if answers is None:
                print("\nSelection cancelled.")
                return []

            selected = answers['datasets']

        except (KeyboardInterrupt, Exception) as e:
            print(f"\n\nInteractive selection failed: {e}")
            print("Falling back to auto-selection of new/modified datasets.")
            return default_selections

        return selected

    def _display_dataset_table(self, datasets: Dict[str, Dict]):
        """Display dataset status table"""
        print("\nDataset Status:")
        print("-" * 105)
        print(f"{'STATUS':<12} {'DATASET':<30} {'TYPE':<10} {'FILES':<8} {'LAST MODIFIED':<20}")
        print("-" * 105)

        # Sort: new/modified first, mixed files last
        priority_order = {'new': 0, 'modified': 1, 'unchanged': 2, 'mixed_files': 99}
        sorted_datasets = sorted(
            datasets.items(),
            key=lambda x: (priority_order[x[1]['status']], x[0])
        )

        for dataset_name, metadata in sorted_datasets:
            status = metadata['status']
            file_type = metadata.get('file_type', 'unknown')
            mtime_str = metadata['latest_mtime'].strftime('%Y-%m-%d %H:%M')
            file_count = metadata['file_count']

            if status == 'mixed_files':
                marker = "⚠️  MIXED"
            elif status == 'new':
                marker = "🆕 NEW"
            elif status == 'modified':
                marker = "📝 MODIFIED"
            else:
                marker = "✓  UNCHANGED"

            print(f"{marker:<12} {dataset_name:<30} {file_type:<10} {file_count:<8} {mtime_str:<20}")

        print("-" * 105)

    def save_manifest(self, selected_datasets: List[str], output_file: str = "a_data/.pipeline_manifest.json"):
        """
        Save selected datasets to manifest file for pipeline consumption

        Args:
            selected_datasets: List of dataset names to process
            output_file: Path to manifest JSON file
        """
        manifest = {
            'generated_at': datetime.now().isoformat(),
            'datasets_selected': selected_datasets,
            'dataset_count': len(selected_datasets)
        }

        output_path = Path(output_file)
        output_path.parent.mkdir(parents=True, exist_ok=True)

        with open(output_path, 'w') as f:
            json.dump(manifest, f, indent=2)

        print(f"\n✓ Manifest saved: {output_file}")
        return output_path

    def display_summary(self, selected_datasets: List[str]):
        """Display selection summary"""
        print("\n" + "="*80)
        print("SELECTION SUMMARY")
        print("="*80)

        if not selected_datasets:
            print("No datasets selected. Pipeline will not run.")
            return

        print(f"\n{len(selected_datasets)} dataset(s) selected for processing:\n")
        for i, name in enumerate(selected_datasets, 1):
            print(f"  {i}. {name}")

        print()


def main():
    """Main entry point"""
    selector = PipelineSelector()

    # Run selection
    selected = selector.select_datasets()

    if not selected:
        print("\nExiting without running pipeline.")
        sys.exit(0)

    # Display summary
    selector.display_summary(selected)

    # Save manifest
    manifest_path = selector.save_manifest(selected)

    # Update cache with current run time
    selector._save_run_time(selected)

    print("\n" + "="*80)
    print("Next step: Run the pipeline with selected datasets")
    print("="*80)
    print("\nCommand:")
    print("  Rscript -e \"source('b_scripts/3_standardize/00_run_all.R')\"")
    print()


if __name__ == "__main__":
    main()
