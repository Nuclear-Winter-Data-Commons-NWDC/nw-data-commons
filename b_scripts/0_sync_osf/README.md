# OSF Bidirectional Sync Pipeline

Complete workflow for synchronizing local repository with OSF, processing data through standardization pipeline, and pushing results back to OSF.

## Overview

The sync pipeline handles the full lifecycle of data management:

1. **Compare** - Detect differences between OSF and local repository
2. **Report** - Show new, updated, and deleted files with timestamps
3. **Select** - User chooses which changes to process
4. **Backup** - Preserve current versions before making changes
5. **Download** - Pull new/updated files from OSF
6. **Process** - Run standardization pipeline (manual step)
7. **Push** - Upload all changes back to OSF

## Quick Start

### Run Full Sync Pipeline

```bash
# Interactive mode (recommended for first use)
bash b_scripts/0_sync_osf/sync_pipeline.sh

# Non-interactive mode (uses defaults)
bash b_scripts/0_sync_osf/sync_pipeline.sh --non-interactive

# Dry run (preview without making changes)
bash b_scripts/0_sync_osf/sync_pipeline.sh --dry-run
```

### Run Individual Steps

```bash
# Step 1: Compare OSF vs Local
python3 b_scripts/0_sync_osf/compare_osf_local.py \
    --local a_data/osf_data_current \
    --remote / \
    --json diff_report.json

# Step 2: Interactive file selection
python3 b_scripts/0_sync_osf/interactive_selector.py \
    --input diff_report.json \
    --output selections.json

# Step 3: Backup files before update
bash b_scripts/0_sync_osf/backup_before_update.sh selections.json

# Step 4: Download from OSF (use osf_manager.py for individual files)
python3 b_scripts/1_download_or_extract/osf_manager.py download \
    --remote /path/to/file \
    --local a_data/osf_data_current/path/to/file \
    --overwrite

# Step 5: Process through pipeline (example)
Rscript b_scripts/3_standardize/00_run_all.R

# Step 6: Push changes to OSF
python3 b_scripts/0_sync_osf/push_to_osf.py \
    --selections selections.json \
    --local a_data/osf_data_current \
    --remote /
```

## Scripts

### `sync_pipeline.sh` (Main Orchestrator)

Complete workflow orchestrator that runs all steps in sequence.

**Options:**
- `--non-interactive` - Use default selections without prompting
- `--dry-run` - Preview changes without executing
- `--skip-download` - Skip downloading from OSF (useful for local-only testing)
- `--skip-push` - Skip pushing to OSF (useful for testing pipeline processing)

**Example:**
```bash
# Full sync with defaults
bash b_scripts/0_sync_osf/sync_pipeline.sh --non-interactive

# Test without pushing to OSF
bash b_scripts/0_sync_osf/sync_pipeline.sh --skip-push
```

### `compare_osf_local.py`

Compares OSF repository with local directory using file timestamps.

**Detects:**
- **New files**: Exist on OSF but not locally (need download)
- **Updated files**: Exist in both, OSF version is newer (need update)
- **Deleted files**: Exist locally but not on OSF (need removal)
- **Unchanged files**: Exist in both, local is same or newer

**Output:** JSON report with categorized file lists

### `interactive_selector.py`

Interactive interface for users to select which files to process.

**Default Behavior:**
- All new files: SELECTED
- All updated files: SELECTED
- All deleted files: SELECTED (will be removed after backup)

**Options:**
- Press Enter: Accept defaults (process all)
- `skip-new`: Don't download new files
- `skip-updated`: Don't update existing files
- `skip-deleted`: Don't remove deleted files

### `backup_before_update.sh`

Backs up files to `osf_data_most_recent_previous/` before making changes.

**Backs up:**
- Files about to be updated (overwrites)
- Files about to be deleted (removal)

Preserves directory structure and file timestamps.

### `push_to_osf.py`

Pushes all changes to OSF repository.

**Actions:**
- Uploads new files
- Uploads updated files (overwrites)
- Deletes removed files

**Options:**
- `--dry-run` - Simulate without executing

## File Classification Logic

### New Files
```
IF file exists on OSF AND file does NOT exist locally
   THEN classify as NEW
   ACTION: Download from OSF
```

### Updated Files
```
IF file exists on OSF AND file exists locally AND osf_timestamp > local_timestamp
   THEN classify as UPDATED
   ACTION: Download from OSF (overwrite local)
```

### Deleted Files
```
IF file exists locally AND file does NOT exist on OSF
   THEN classify as DELETED
   ACTION: Remove from local (after backup)
```

### Unchanged Files
```
IF file exists in both AND local_timestamp >= osf_timestamp
   THEN classify as UNCHANGED
   ACTION: No action needed
```

## Workflow Example

```bash
# 1. Start sync pipeline
bash b_scripts/0_sync_osf/sync_pipeline.sh

# Output shows comparison report:
# ✨ NEW FILES (3 files)
#   + 2_aggregated/new_dataset_v2026-02-15.xlsx
# 🔄 UPDATED FILES (2 files)
#   ↻ 2_aggregated/temperature_v2026-02-14.xlsx
#      Local:  2026-02-10T10:30:00
#      OSF:    2026-02-14T15:20:00
# 🗑️ DELETED FILES (1 file)
#   - 2_aggregated/old_dataset_v2026-01-01.xlsx

# 2. User reviews and presses Enter to accept defaults

# 3. Backup runs automatically
# ✓ Backed up 3 files to osf_data_most_recent_previous/

# 4. Download runs automatically
# ✓ Downloaded 5 files from OSF

# 5. Manual step prompt appears:
# "Run standardization pipeline now..."
# User runs: Rscript b_scripts/3_standardize/00_run_all.R

# 6. User presses Enter when processing complete

# 7. Push runs automatically
# ✓ Pushed 10 files to OSF (new + updated standardized outputs)
```

## Safety Features

1. **Backup Before Changes**: All files are backed up before update/deletion
2. **Dry Run Mode**: Preview all operations without executing
3. **Interactive Selection**: User controls which files to process
4. **Timestamp Comparison**: Only download when OSF version is newer
5. **Temp File Preservation**: All intermediate files saved for review

## Troubleshooting

### "OSF token required"
Set environment variable: `export OSF_TOKEN=your_token_here`

Or add to `.env` file:
```
OSF_TOKEN=your_token_here
```

### "Comparison failed"
- Check network connection
- Verify OSF project ID is correct (default: e28gq)
- Ensure OSF token has read access

### "Download failed"
- Check file exists on OSF: `python3 b_scripts/1_download_or_extract/osf_manager.py list --path /`
- Verify local directory exists and is writable
- Check disk space

### "Push failed"
- Ensure OSF token has write access
- Check file size limits (OSF has per-file limits)
- Verify remote path exists

## Integration with Existing Scripts

This sync pipeline replaces `b_scripts/1_download_or_extract/sync_from_osf.sh` with a more comprehensive bidirectional workflow.

**Old approach (one-way download):**
```bash
bash b_scripts/1_download_or_extract/sync_from_osf.sh
```

**New approach (bidirectional sync):**
```bash
bash b_scripts/0_sync_osf/sync_pipeline.sh
```

The new pipeline includes:
- Timestamp-based change detection
- User control over which files to process
- Automatic backup before changes
- Push results back to OSF

## Requirements

- Python 3.6+
- osfclient: `pip install osfclient`
- python-dotenv: `pip install python-dotenv`
- OSF token with read/write access
- Bash shell

## Environment Variables

- `OSF_TOKEN`: OSF authentication token (required)
- Can be set in `.env` file or exported in shell
