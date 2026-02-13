#!/home/wnf/code/nw-data-commons/.venv/bin/python3
"""
Compare OSF vs Local Repository - Detect New, Updated, and Deleted Files

Compares file timestamps and generates a detailed diff report showing:
- New files (exist on OSF but not locally)
- Updated files (exist in both, OSF version is newer)
- Deleted files (exist locally but not on OSF)
- Unchanged files (exist in both with same or older OSF timestamp)

Usage:
    python compare_osf_local.py --local b_data/osf_data_current --remote / [--json diff_report.json]
"""

import os
import sys
import json
import argparse
import logging
from pathlib import Path
from typing import Dict, List, Optional
from datetime import datetime

# Add parent directory to path for osf_manager import
sys.path.insert(0, str(Path(__file__).parent.parent / "1_download_or_extract"))

try:
    from dotenv import load_dotenv
    load_dotenv(dotenv_path=".env")
except ImportError:
    pass

from osfclient import OSF

logging.basicConfig(
    level=logging.INFO,
    format='%(levelname)s: %(message)s'
)
logger = logging.getLogger(__name__)


class OSFLocalComparer:
    """Compare OSF repository with local directory"""

    def __init__(self, project_id: str, token: str, component_id: Optional[str] = None):
        self.project_id = project_id
        self.token = token
        self.osf = OSF(token=token)

        try:
            self.node = self.osf.project(component_id) if component_id else self.osf.project(project_id)
            self.storage = self.node.storage("osfstorage")
        except Exception as e:
            logger.error(f"Failed to connect to OSF: {e}")
            raise

    def get_osf_files_with_metadata(self, remote_path: str = "/") -> Dict[str, Dict]:
        """
        Get all OSF files with metadata including modification times

        Returns:
            Dictionary mapping relative paths to file metadata
        """
        logger.info(f"Fetching OSF file list from {remote_path}...")

        try:
            files_attr = getattr(self.storage, "files", None)
            if not files_attr:
                raise RuntimeError("Storage has no 'files' attribute")

            files_iter = files_attr() if callable(files_attr) else files_attr
            all_files = list(files_iter)

            # Filter to specified path
            remote_normalized = remote_path.rstrip("/")
            if remote_path != "/":
                all_files = [f for f in all_files if f.path.startswith(remote_normalized)]

            # Build metadata dictionary
            osf_files = {}
            for f in all_files:
                # Get relative path (strip leading remote_path)
                if remote_path == "/":
                    rel_path = f.path.lstrip("/")
                else:
                    rel_path = f.path[len(remote_normalized):].lstrip("/")

                if not rel_path:  # Skip the directory itself
                    continue

                # Extract metadata
                metadata = {
                    'path': f.path,
                    'name': f.name,
                    'size': getattr(f, 'size', None),
                    'modified': None
                }

                # Try to get modification time from various attributes
                for attr in ['date_modified', 'modified', 'last_modified', 'mtime']:
                    if hasattr(f, attr):
                        mod_time = getattr(f, attr)
                        if mod_time:
                            try:
                                # Parse datetime if string
                                if isinstance(mod_time, str):
                                    metadata['modified'] = datetime.fromisoformat(mod_time.replace('Z', '+00:00'))
                                else:
                                    metadata['modified'] = mod_time
                                break
                            except:
                                pass

                osf_files[rel_path] = metadata

            logger.info(f"Found {len(osf_files)} files on OSF")
            return osf_files

        except Exception as e:
            logger.error(f"Failed to fetch OSF files: {e}")
            raise

    def get_local_files_with_metadata(self, local_dir: Path) -> Dict[str, Dict]:
        """
        Get all local files with metadata

        Excludes:
        - .gitkeep files (code-only, not synced to OSF)

        Returns:
            Dictionary mapping relative paths to file metadata
        """
        local_dir = Path(local_dir)

        if not local_dir.exists():
            logger.warning(f"Local directory does not exist: {local_dir}")
            return {}

        logger.info(f"Scanning local directory: {local_dir}")

        local_files = {}
        all_paths = list(local_dir.rglob("*"))
        file_paths = [p for p in all_paths if p.is_file()]

        for file_path in file_paths:
            # Skip .gitkeep files (code-only, not for OSF)
            if file_path.name == ".gitkeep":
                continue

            rel_path = str(file_path.relative_to(local_dir))
            stats = file_path.stat()

            local_files[rel_path] = {
                'path': str(file_path),
                'name': file_path.name,
                'size': stats.st_size,
                'modified': datetime.fromtimestamp(stats.st_mtime)
            }

        logger.info(f"Found {len(local_files)} files locally (excluding .gitkeep)")
        return local_files

    @staticmethod
    def normalize_path(path: str) -> str:
        """
        Normalize file path by removing version date suffix for comparison

        Examples:
            1_standardized_data_v2026-02-13.xlsx -> 1_standardized_data.xlsx
            temperature_v2025-10-31.csv -> temperature.csv
            configs_v2026-01-21.xlsx -> configs.xlsx

        Returns:
            Normalized path without version date
        """
        import re
        # Remove _vYYYY-MM-DD pattern before file extension
        return re.sub(r'_v\d{4}-\d{2}-\d{2}(\.\w+)$', r'\1', path)

    def compare(self, local_dir: Path, remote_path: str = "/") -> Dict:
        """
        Compare OSF and local files, categorize differences

        Files are matched by normalized path (version dates stripped) to recognize
        different versions of the same file. The most recent version wins.

        Returns:
            Dictionary with categorized file lists:
            - new: Files on OSF but not local (need to download)
            - updated: Files on both, OSF is newer (need to update)
            - deleted: Files local but not on OSF (need to remove)
            - unchanged: Files in both, local is same or newer
        """
        osf_files = self.get_osf_files_with_metadata(remote_path)
        local_files = self.get_local_files_with_metadata(local_dir)

        # Build normalized mappings (normalized_path -> actual_path -> metadata)
        osf_normalized = {}
        for path, meta in osf_files.items():
            norm_path = self.normalize_path(path)
            if norm_path not in osf_normalized:
                osf_normalized[norm_path] = []
            osf_normalized[norm_path].append((path, meta))

        local_normalized = {}
        for path, meta in local_files.items():
            norm_path = self.normalize_path(path)
            if norm_path not in local_normalized:
                local_normalized[norm_path] = []
            local_normalized[norm_path].append((path, meta))

        # For each normalized path, pick the most recent version
        osf_latest = {}
        for norm_path, versions in osf_normalized.items():
            # Sort by modification time (newest first)
            versions_sorted = sorted(versions,
                                    key=lambda x: x[1]['modified'] if x[1]['modified'] else datetime.min,
                                    reverse=True)
            osf_latest[norm_path] = versions_sorted[0]  # (actual_path, metadata)

        local_latest = {}
        for norm_path, versions in local_normalized.items():
            # Sort by modification time (newest first)
            versions_sorted = sorted(versions,
                                    key=lambda x: x[1]['modified'],
                                    reverse=True)
            local_latest[norm_path] = versions_sorted[0]  # (actual_path, metadata)

        new_files = []
        updated_files = []
        deleted_files = []
        unchanged_files = []

        # Compare normalized files
        for norm_path in set(osf_latest.keys()) | set(local_latest.keys()):
            osf_entry = osf_latest.get(norm_path)
            local_entry = local_latest.get(norm_path)

            if osf_entry and not local_entry:
                # File on OSF but not local - NEW
                osf_path, osf_meta = osf_entry
                new_files.append({
                    'path': osf_path,
                    'normalized_path': norm_path,
                    'osf_size': osf_meta['size'],
                    'osf_modified': osf_meta['modified'].isoformat() if osf_meta['modified'] else None
                })

            elif local_entry and not osf_entry:
                # File local but not on OSF - DELETED
                local_path, local_meta = local_entry
                deleted_files.append({
                    'path': local_path,
                    'normalized_path': norm_path,
                    'local_size': local_meta['size'],
                    'local_modified': local_meta['modified'].isoformat()
                })

            else:
                # File exists in both - compare timestamps
                osf_path, osf_meta = osf_entry
                local_path, local_meta = local_entry

                if osf_meta['modified'] and local_meta['modified']:
                    osf_time = osf_meta['modified']
                    local_time = local_meta['modified']

                    if osf_time > local_time:
                        # OSF version is newer - UPDATED
                        updated_files.append({
                            'path': osf_path,
                            'local_path': local_path,
                            'normalized_path': norm_path,
                            'local_modified': local_time.isoformat(),
                            'osf_modified': osf_time.isoformat(),
                            'local_size': local_meta['size'],
                            'osf_size': osf_meta['size']
                        })
                    else:
                        # Local is same or newer - UNCHANGED
                        unchanged_files.append({
                            'path': local_path,
                            'osf_path': osf_path,
                            'normalized_path': norm_path,
                            'local_modified': local_time.isoformat(),
                            'osf_modified': osf_time.isoformat()
                        })
                else:
                    # Can't determine timestamp
                    unchanged_files.append({
                        'path': local_path,
                        'osf_path': osf_path,
                        'normalized_path': norm_path,
                        'note': 'Timestamp comparison not available'
                    })

        return {
            'new': new_files,
            'updated': updated_files,
            'deleted': deleted_files,
            'unchanged': unchanged_files,
            'summary': {
                'new_count': len(new_files),
                'updated_count': len(updated_files),
                'deleted_count': len(deleted_files),
                'unchanged_count': len(unchanged_files),
                'total_osf': len(osf_latest),
                'total_local': len(local_latest),
                'note': 'Counts based on normalized paths (version dates stripped)'
            }
        }


def print_comparison_report(comparison: Dict):
    """Print human-readable comparison report"""

    print("\n" + "=" * 80)
    print("OSF vs LOCAL COMPARISON REPORT")
    print("=" * 80)

    summary = comparison['summary']
    print(f"\n📊 Summary:")
    print(f"   Total files on OSF: {summary['total_osf']}")
    print(f"   Total files locally: {summary['total_local']}")
    print(f"   New files (on OSF, not local): {summary['new_count']}")
    print(f"   Updated files (OSF newer): {summary['updated_count']}")
    print(f"   Deleted files (local only): {summary['deleted_count']}")
    print(f"   Unchanged files: {summary['unchanged_count']}")

    # New files
    if comparison['new']:
        print(f"\n✨ NEW FILES (on OSF, not local) - {len(comparison['new'])} files:")
        print("-" * 80)
        for f in comparison['new'][:20]:
            size_str = f"{f['osf_size']:,} bytes" if f['osf_size'] else "unknown"
            print(f"  + {f['path']:<60} {size_str:>15}")
        if len(comparison['new']) > 20:
            print(f"  ... and {len(comparison['new']) - 20} more")

    # Updated files
    if comparison['updated']:
        print(f"\n🔄 UPDATED FILES (OSF version newer) - {len(comparison['updated'])} files:")
        print("-" * 80)
        for f in comparison['updated'][:20]:
            print(f"  ↻ {f['path']}")
            print(f"     Local:  {f['local_modified']} ({f['local_size']:,} bytes)")
            print(f"     OSF:    {f['osf_modified']} ({f['osf_size']:,} bytes)")
        if len(comparison['updated']) > 20:
            print(f"  ... and {len(comparison['updated']) - 20} more")

    # Deleted files
    if comparison['deleted']:
        print(f"\n🗑️  DELETED FILES (local only, not on OSF) - {len(comparison['deleted'])} files:")
        print("-" * 80)
        for f in comparison['deleted'][:20]:
            size_str = f"{f['local_size']:,} bytes" if f['local_size'] else "unknown"
            print(f"  - {f['path']:<60} {size_str:>15}")
        if len(comparison['deleted']) > 20:
            print(f"  ... and {len(comparison['deleted']) - 20} more")

    print("\n" + "=" * 80)
    print("📝 Default behavior: New and Updated files will be processed")
    print("                     Deleted files will be removed after backup")
    print("=" * 80 + "\n")


def main():
    parser = argparse.ArgumentParser(
        description="Compare OSF repository with local directory",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    parser.add_argument("--local", required=True, help="Local directory path")
    parser.add_argument("--remote", default="/", help="Remote OSF path (default: /)")
    parser.add_argument("--project", default="e28gq", help="OSF project ID")
    parser.add_argument("--component", default="", help="OSF component ID")
    parser.add_argument("--token", default="", help="OSF token (uses OSF_TOKEN env if empty)")
    parser.add_argument("--json", help="Output JSON report to file")

    args = parser.parse_args()

    # Get token
    token = args.token or os.environ.get("OSF_TOKEN")
    if not token:
        logger.error("OSF token required. Set OSF_TOKEN environment variable or use --token")
        sys.exit(1)

    # Create comparer
    try:
        comparer = OSFLocalComparer(args.project, token, args.component or None)
    except Exception as e:
        logger.error(f"Failed to initialize: {e}")
        sys.exit(1)

    # Perform comparison
    try:
        comparison = comparer.compare(Path(args.local), args.remote)
    except Exception as e:
        logger.error(f"Comparison failed: {e}")
        sys.exit(1)

    # Print report
    print_comparison_report(comparison)

    # Save JSON if requested
    if args.json:
        with open(args.json, 'w') as f:
            json.dump(comparison, f, indent=2)
        logger.info(f"JSON report saved to: {args.json}")


if __name__ == "__main__":
    main()
