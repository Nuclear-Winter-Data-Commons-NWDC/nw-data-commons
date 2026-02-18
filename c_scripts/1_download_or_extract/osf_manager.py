#!/usr/bin/env python3
"""
OSF Manager - Comprehensive CLI tool for managing OSF repository operations

Provides commands for:
- Listing/browsing OSF structure
- Downloading files and directories
- Uploading files and directories
- Deleting files with safety checks
- Syncing local vs OSF state
- Batch restructuring operations

Usage:
    osf_manager.py list [--path PATH]
    osf_manager.py download --remote PATH --local PATH
    osf_manager.py upload --local PATH --remote PATH [--dry-run]
    osf_manager.py delete --remote PATH [--confirm]
    osf_manager.py sync --local PATH --remote PATH [--direction pull|push|both]
"""

import os
import sys
import json
import argparse
import logging
from pathlib import Path
from typing import List, Dict, Optional, Tuple
from datetime import datetime

try:
    from dotenv import load_dotenv
    load_dotenv(dotenv_path=".env")
except ImportError:
    pass

from osfclient import OSF

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)


class OSFManager:
    """Manager class for OSF operations"""

    def __init__(self, project_id: str, token: str, component_id: Optional[str] = None):
        self.project_id = project_id
        self.component_id = component_id
        self.token = token
        self.osf = OSF(token=token)

        try:
            self.node = self.osf.project(component_id) if component_id else self.osf.project(project_id)
            self.storage = self.node.storage("osfstorage")
        except Exception as e:
            logger.error(f"Failed to connect to OSF project {project_id}: {e}")
            raise

    def list_files(self, path: str = "/", recursive: bool = False) -> List[Dict]:
        """
        List files at specified OSF path

        Args:
            path: OSF path to list (default: root)
            recursive: If True, list all nested files

        Returns:
            List of file info dictionaries
        """
        logger.info(f"Listing files at OSF path: {path}")

        try:
            files_attr = getattr(self.storage, "files", None)
            if files_attr is None:
                raise RuntimeError("Storage has no 'files' attribute")

            files_iter = files_attr() if callable(files_attr) else files_attr
            files_list = list(files_iter)

            if path == "/":
                results = files_list
            else:
                # Filter to files under specified path
                path_normalized = path.rstrip("/")
                results = [f for f in files_list if f.path.startswith(path_normalized)]

            # Format results
            formatted = []
            for f in results:
                formatted.append({
                    'path': f.path,
                    'name': f.name,
                    'kind': getattr(f, 'kind', 'file'),
                    'size': getattr(f, 'size', None)
                })

            return formatted

        except Exception as e:
            logger.error(f"Failed to list files: {e}")
            raise

    def download_file(self, remote_path: str, local_path: Path, overwrite: bool = False) -> bool:
        """
        Download a single file from OSF

        Args:
            remote_path: Path on OSF
            local_path: Local destination path
            overwrite: If True, overwrite existing file

        Returns:
            True if successful
        """
        local_path = Path(local_path)

        if local_path.exists() and not overwrite:
            logger.warning(f"File exists: {local_path}. Use --overwrite to replace.")
            return False

        logger.info(f"Downloading {remote_path} -> {local_path}")

        try:
            files_attr = getattr(self.storage, "files", None)
            files_iter = files_attr() if callable(files_attr) else files_attr

            remote_normalized = remote_path.lstrip("/")
            match = None

            for f in files_iter:
                if f.path.lstrip("/") == remote_normalized:
                    match = f
                    break

            if not match:
                logger.error(f"File not found on OSF: {remote_path}")
                return False

            local_path.parent.mkdir(parents=True, exist_ok=True)

            with open(local_path, "wb") as fp:
                match.write_to(fp)

            logger.info(f"✓ Downloaded: {local_path}")
            return True

        except Exception as e:
            logger.error(f"Download failed: {e}")
            return False

    def delete_old_versions(self, remote_path: str, dry_run: bool = False) -> int:
        """
        Before uploading a versioned file, delete all existing files on OSF that
        share the same basename (minus version date) in the same directory.

        Versioned filenames follow: {basename}_v{YYYY-MM-DD}.{ext}
        This method deletes any file matching {basename}_v*.{ext} at the same remote dir.

        Args:
            remote_path: Full remote path of the file about to be uploaded
                         e.g. /3_standardized/temperature_v2026-02-18.csv
            dry_run: If True, log what would be deleted but don't delete

        Returns:
            Number of old versions deleted
        """
        import re

        remote_path = remote_path.lstrip("/")
        remote_dir = "/".join(remote_path.split("/")[:-1])
        filename = remote_path.split("/")[-1]

        # Strip version date to get base pattern: temperature_v2026-02-18.csv -> temperature
        match = re.match(r'^(.+?)_v\d{4}-\d{2}-\d{2}(\.\w+)$', filename)
        if not match:
            # Not a versioned file; skip
            logger.debug(f"Not a versioned filename, skipping old-version cleanup: {filename}")
            return 0

        base = match.group(1)
        ext = match.group(2)
        pattern = re.compile(rf'^{re.escape(base)}_v\d{{4}}-\d{{2}}-\d{{2}}{re.escape(ext)}$')

        # List all files in the same OSF directory
        try:
            files_attr = getattr(self.storage, "files", None)
            files_iter = files_attr() if callable(files_attr) else files_attr
            all_files = list(files_iter)
        except Exception as e:
            logger.error(f"Failed to list files for old-version cleanup: {e}")
            return 0

        deleted = 0
        for f in all_files:
            f_path = f.path.lstrip("/")
            f_dir = "/".join(f_path.split("/")[:-1])
            f_name = f_path.split("/")[-1]

            if f_dir == remote_dir and pattern.match(f_name) and f_path != remote_path:
                if dry_run:
                    logger.info(f"[DRY RUN] Would delete old version: /{f_path}")
                else:
                    logger.info(f"Deleting old version: /{f_path}")
                    try:
                        f.remove()
                        logger.info(f"✓ Deleted old version: /{f_path}")
                        deleted += 1
                    except Exception as e:
                        logger.error(f"Failed to delete old version /{f_path}: {e}")

        return deleted

    def upload_file(self, local_path: Path, remote_path: str, dry_run: bool = False, replace_old_versions: bool = False) -> bool:
        """
        Upload a single file to OSF.

        If replace_old_versions=True and the filename follows the versioned naming
        convention ({basename}_v{YYYY-MM-DD}.{ext}), all prior versions of that
        file in the same OSF directory are deleted before the upload. This prevents
        accumulation of stale dated files on OSF.

        Args:
            local_path: Local file path
            remote_path: Destination path on OSF
            dry_run: If True, only simulate the upload (and version cleanup)
            replace_old_versions: If True, delete prior dated versions before upload

        Returns:
            True if successful
        """
        local_path = Path(local_path)

        if not local_path.exists():
            logger.error(f"Local file does not exist: {local_path}")
            return False

        if dry_run:
            logger.info(f"[DRY RUN] Would upload {local_path} -> {remote_path}")
            if replace_old_versions:
                self.delete_old_versions(remote_path, dry_run=True)
            return True

        # Delete old versions before uploading new one
        if replace_old_versions:
            n_deleted = self.delete_old_versions(remote_path, dry_run=False)
            if n_deleted:
                logger.info(f"Removed {n_deleted} old version(s) of {remote_path.split('/')[-1]}")

        logger.info(f"Uploading {local_path} -> {remote_path}")

        try:
            with open(local_path, "rb") as fp:
                self.storage.create_file(remote_path, fp, force=True)

            logger.info(f"✓ Uploaded: {remote_path}")
            return True

        except Exception as e:
            logger.error(f"Upload failed: {e}")
            return False

    def delete_file(self, remote_path: str, confirm: bool = False) -> bool:
        """
        Delete a file from OSF

        Args:
            remote_path: Path on OSF to delete
            confirm: If True, skip confirmation prompt

        Returns:
            True if successful
        """
        if not confirm:
            response = input(f"⚠️  Delete {remote_path} from OSF? [y/N]: ")
            if response.lower() != 'y':
                logger.info("Delete cancelled")
                return False

        logger.info(f"Deleting {remote_path}")

        try:
            files_attr = getattr(self.storage, "files", None)
            files_iter = files_attr() if callable(files_attr) else files_attr

            remote_normalized = remote_path.lstrip("/")
            match = None

            for f in files_iter:
                if f.path.lstrip("/") == remote_normalized:
                    match = f
                    break

            if not match:
                logger.error(f"File not found on OSF: {remote_path}")
                return False

            match.remove()
            logger.info(f"✓ Deleted: {remote_path}")
            return True

        except Exception as e:
            logger.error(f"Delete failed: {e}")
            return False

    def upload_directory(self, local_dir: Path, remote_base: str, dry_run: bool = False, replace_old_versions: bool = False) -> Tuple[int, int]:
        """
        Upload all files in a directory to OSF

        Args:
            local_dir: Local directory path
            remote_base: Base path on OSF
            dry_run: If True, only simulate uploads
            replace_old_versions: If True, delete prior dated versions before each upload

        Returns:
            Tuple of (successful_count, failed_count)
        """
        local_dir = Path(local_dir)

        if not local_dir.is_dir():
            logger.error(f"Not a directory: {local_dir}")
            return (0, 0)

        success_count = 0
        fail_count = 0

        # Get all files in directory
        files = list(local_dir.rglob("*"))
        files = [f for f in files if f.is_file()]

        logger.info(f"Uploading {len(files)} files from {local_dir}")

        for file_path in files:
            # Construct remote path
            relative = file_path.relative_to(local_dir)
            remote_path = f"{remote_base.rstrip('/')}/{relative}"

            if self.upload_file(file_path, remote_path, dry_run=dry_run,
                                replace_old_versions=replace_old_versions):
                success_count += 1
            else:
                fail_count += 1

        logger.info(f"Upload complete: {success_count} succeeded, {fail_count} failed")
        return (success_count, fail_count)

    def sync_check(self, local_dir: Path, remote_base: str) -> Dict[str, List[str]]:
        """
        Compare local directory with OSF and identify differences

        Args:
            local_dir: Local directory path
            remote_base: OSF path to compare against

        Returns:
            Dictionary with 'local_only', 'remote_only', 'both' lists
        """
        local_dir = Path(local_dir)

        if not local_dir.is_dir():
            logger.error(f"Not a directory: {local_dir}")
            return {'local_only': [], 'remote_only': [], 'both': []}

        # Get local files
        local_files = list(local_dir.rglob("*"))
        local_files = [f for f in local_files if f.is_file()]
        local_relative = {str(f.relative_to(local_dir)) for f in local_files}

        # Get remote files
        remote_files = self.list_files(remote_base)
        remote_base_normalized = remote_base.rstrip("/")
        remote_relative = set()

        for f in remote_files:
            if f['path'].startswith(remote_base_normalized):
                rel_path = f['path'][len(remote_base_normalized):].lstrip("/")
                if rel_path:
                    remote_relative.add(rel_path)

        # Compare
        local_only = sorted(local_relative - remote_relative)
        remote_only = sorted(remote_relative - local_relative)
        both = sorted(local_relative & remote_relative)

        return {
            'local_only': local_only,
            'remote_only': remote_only,
            'both': both
        }


def cmd_list(args, manager: OSFManager):
    """List files on OSF"""
    files = manager.list_files(args.path)

    if not files:
        print(f"No files found at {args.path}")
        return

    print(f"\nFiles at OSF:{args.path}")
    print("-" * 80)

    for f in files:
        size_str = f"{f['size']:,} bytes" if f['size'] else "N/A"
        print(f"{f['path']:<60} {size_str:>15}")

    print(f"\nTotal: {len(files)} files")


def cmd_download(args, manager: OSFManager):
    """Download file or directory from OSF"""
    local_path = Path(args.local)

    # Check if downloading directory or single file
    if args.remote.endswith('/') or not Path(args.remote).suffix:
        # Directory download
        files = manager.list_files(args.remote)

        if not files:
            logger.error(f"No files found at {args.remote}")
            return

        logger.info(f"Downloading {len(files)} files")

        for f in files:
            rel_path = f['path'][len(args.remote):].lstrip("/")
            local_dest = local_path / rel_path
            manager.download_file(f['path'], local_dest, overwrite=args.overwrite)
    else:
        # Single file download
        manager.download_file(args.remote, local_path, overwrite=args.overwrite)


def cmd_upload(args, manager: OSFManager):
    """Upload file or directory to OSF"""
    local_path = Path(args.local)
    replace_old = getattr(args, 'replace_old_versions', False)

    if local_path.is_dir():
        manager.upload_directory(local_path, args.remote, dry_run=args.dry_run,
                                 replace_old_versions=replace_old)
    else:
        manager.upload_file(local_path, args.remote, dry_run=args.dry_run,
                            replace_old_versions=replace_old)


def cmd_delete(args, manager: OSFManager):
    """Delete file from OSF"""
    manager.delete_file(args.remote, confirm=args.confirm)


def cmd_sync(args, manager: OSFManager):
    """Sync local directory with OSF"""
    local_path = Path(args.local)

    result = manager.sync_check(local_path, args.remote)

    print("\n=== Sync Analysis ===")
    print(f"Local: {args.local}")
    print(f"Remote: {args.remote}")
    print()

    if result['local_only']:
        print(f"📁 Local only ({len(result['local_only'])} files):")
        for f in result['local_only'][:10]:
            print(f"  + {f}")
        if len(result['local_only']) > 10:
            print(f"  ... and {len(result['local_only']) - 10} more")
        print()

    if result['remote_only']:
        print(f"☁️  Remote only ({len(result['remote_only'])} files):")
        for f in result['remote_only'][:10]:
            print(f"  - {f}")
        if len(result['remote_only']) > 10:
            print(f"  ... and {len(result['remote_only']) - 10} more")
        print()

    print(f"✓ Both locations ({len(result['both'])} files)")

    if args.auto_sync:
        if args.direction in ['push', 'both']:
            logger.info("Uploading local-only files...")
            for f in result['local_only']:
                local_file = local_path / f
                remote_path = f"{args.remote.rstrip('/')}/{f}"
                manager.upload_file(local_file, remote_path)

        if args.direction in ['pull', 'both']:
            logger.info("Downloading remote-only files...")
            for f in result['remote_only']:
                remote_path = f"{args.remote.rstrip('/')}/{f}"
                local_file = local_path / f
                manager.download_file(remote_path, local_file)


def main():
    parser = argparse.ArgumentParser(
        description="OSF Manager - Comprehensive CLI for OSF operations",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    parser.add_argument("--project", default="e28gq", help="OSF project ID")
    parser.add_argument("--component", default="", help="OSF component ID")
    parser.add_argument("--token", default="", help="OSF token (uses OSF_TOKEN env if empty)")

    subparsers = parser.add_subparsers(dest='command', help='Command to execute')

    # List command
    list_parser = subparsers.add_parser('list', help='List files on OSF')
    list_parser.add_argument('--path', default='/', help='OSF path to list')

    # Download command
    download_parser = subparsers.add_parser('download', help='Download from OSF')
    download_parser.add_argument('--remote', required=True, help='Remote OSF path')
    download_parser.add_argument('--local', required=True, help='Local destination path')
    download_parser.add_argument('--overwrite', action='store_true', help='Overwrite existing files')

    # Upload command
    upload_parser = subparsers.add_parser('upload', help='Upload to OSF')
    upload_parser.add_argument('--local', required=True, help='Local file or directory')
    upload_parser.add_argument('--remote', required=True, help='Remote OSF path')
    upload_parser.add_argument('--dry-run', action='store_true', help='Simulate upload without executing')
    upload_parser.add_argument('--replace-old-versions', action='store_true',
                               help='Delete prior dated versions of the file before uploading. '
                                    'Applies to files named {basename}_v{YYYY-MM-DD}.{ext}. '
                                    'Use this flag for all standardized output uploads.')

    # Delete command
    delete_parser = subparsers.add_parser('delete', help='Delete from OSF')
    delete_parser.add_argument('--remote', required=True, help='Remote OSF path to delete')
    delete_parser.add_argument('--confirm', action='store_true', help='Skip confirmation prompt')

    # Sync command
    sync_parser = subparsers.add_parser('sync', help='Check sync status')
    sync_parser.add_argument('--local', required=True, help='Local directory')
    sync_parser.add_argument('--remote', required=True, help='Remote OSF path')
    sync_parser.add_argument('--direction', choices=['pull', 'push', 'both'], default='push',
                            help='Sync direction')
    sync_parser.add_argument('--auto-sync', action='store_true', help='Automatically perform sync')

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        return

    # Get token
    token = args.token or os.environ.get("OSF_TOKEN")
    if not token:
        logger.error("OSF token required. Set OSF_TOKEN environment variable or use --token")
        sys.exit(1)

    # Create manager
    try:
        manager = OSFManager(args.project, token, args.component)
    except Exception as e:
        logger.error(f"Failed to initialize OSF manager: {e}")
        sys.exit(1)

    # Execute command
    if args.command == 'list':
        cmd_list(args, manager)
    elif args.command == 'download':
        cmd_download(args, manager)
    elif args.command == 'upload':
        cmd_upload(args, manager)
    elif args.command == 'delete':
        cmd_delete(args, manager)
    elif args.command == 'sync':
        cmd_sync(args, manager)


if __name__ == "__main__":
    main()
