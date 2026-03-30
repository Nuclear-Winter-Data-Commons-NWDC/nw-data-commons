#!/bin/bash
# OSF Sync Pipeline - Bidirectional sync workflow
#
# Complete workflow:
# 1. Compare OSF vs local, detect new/updated/deleted files
# 2. Present report to user
# 3. Interactive file selection (or use defaults)
# 4. Backup files before making changes
# 5. Download new/updated files from OSF
# 6. Process through standardization pipeline (user-driven)
# 7. Push all changes back to OSF
#
# Usage:
#   bash sync_pipeline.sh [--non-interactive] [--dry-run] [--skip-download] [--skip-push]

set -e

# Configuration
LOCAL_DIR="a_data/osf_data_current"
REMOTE_PATH="/"
TEMP_DIR=".sync_temp"
DIFF_REPORT="$TEMP_DIR/diff_report.json"
SELECTIONS="$TEMP_DIR/selections.json"

# Parse arguments
NON_INTERACTIVE=false
DRY_RUN=false
SKIP_DOWNLOAD=false
SKIP_PUSH=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --non-interactive)
            NON_INTERACTIVE=true
            shift
            ;;
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --skip-download)
            SKIP_DOWNLOAD=true
            shift
            ;;
        --skip-push)
            SKIP_PUSH=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--non-interactive] [--dry-run] [--skip-download] [--skip-push]"
            exit 1
            ;;
    esac
done

# Create temp directory
mkdir -p "$TEMP_DIR"

echo "================================================================================"
echo "OSF SYNC PIPELINE"
echo "================================================================================"
echo ""
echo "Configuration:"
echo "  Local directory: $LOCAL_DIR"
echo "  Remote OSF path: $REMOTE_PATH"
echo "  Non-interactive: $NON_INTERACTIVE"
echo "  Dry run: $DRY_RUN"
echo "  Skip download: $SKIP_DOWNLOAD"
echo "  Skip push: $SKIP_PUSH"
echo ""
echo "================================================================================"
echo ""

# Step 1: Compare OSF vs Local
echo "STEP 1/7: Comparing OSF vs Local Repository"
echo "================================================================================"
.venv/bin/python3 b_scripts/0_sync_osf/compare_osf_local.py \
    --local "$LOCAL_DIR" \
    --remote "$REMOTE_PATH" \
    --json "$DIFF_REPORT"

if [ $? -ne 0 ]; then
    echo "ERROR: Comparison failed"
    exit 1
fi

# Check if there are any changes
changes=$(.venv/bin/python3 -c "
import json
with open('$DIFF_REPORT') as f:
    data = json.load(f)
    summary = data['summary']
    total = summary['new_count'] + summary['updated_count'] + summary['deleted_count']
    print(total)
")

if [ "$changes" -eq 0 ]; then
    echo ""
    echo "✓ No changes detected. Local and OSF are in sync."
    echo ""
    rm -rf "$TEMP_DIR"
    exit 0
fi

# Step 2: User pauses to review report
echo ""
read -p "Press [Enter] to continue to file selection, or Ctrl+C to abort..."
echo ""

# Step 3: Interactive file selection
echo "STEP 2/7: File Selection"
echo "================================================================================"

if [ "$NON_INTERACTIVE" = true ]; then
    .venv/bin/python3 b_scripts/0_sync_osf/interactive_selector.py \
        --input "$DIFF_REPORT" \
        --output "$SELECTIONS" \
        --non-interactive
else
    .venv/bin/python3 b_scripts/0_sync_osf/interactive_selector.py \
        --input "$DIFF_REPORT" \
        --output "$SELECTIONS"
fi

if [ $? -ne 0 ]; then
    echo "ERROR: File selection failed"
    exit 1
fi

# Step 4: Backup before update
echo "STEP 3/7: Backing Up Files Before Update"
echo "================================================================================"
bash b_scripts/0_sync_osf/backup_before_update.sh "$SELECTIONS"

if [ $? -ne 0 ]; then
    echo "ERROR: Backup failed"
    exit 1
fi

# Step 5: Download new/updated files from OSF
if [ "$SKIP_DOWNLOAD" = false ]; then
    echo "STEP 4/7: Downloading Files from OSF"
    echo "================================================================================"

    # Get list of files to download (new + updated)
    files_to_download=$(.venv/bin/python3 -c "
import json
with open('$SELECTIONS') as f:
    data = json.load(f)
    for path in data.get('new', []) + data.get('updated', []):
        print(path)
")

    if [ -z "$files_to_download" ]; then
        echo "No files to download"
    else
        download_count=0
        while IFS= read -r file; do
            echo "Downloading: $file"
            .venv/bin/python3 b_scripts/1_download_or_extract/osf_manager.py download \
                --remote "/$file" \
                --local "$LOCAL_DIR/$file" \
                --overwrite
            ((download_count++))
        done <<< "$files_to_download"

        echo ""
        echo "✓ Downloaded $download_count files"
    fi
    echo ""
else
    echo "STEP 4/7: Downloading Files from OSF (SKIPPED)"
    echo "================================================================================"
    echo ""
fi

# Step 6: Remove deleted files
echo "STEP 5/7: Removing Deleted Files"
echo "================================================================================"

deleted_files=$(.venv/bin/python3 -c "
import json
with open('$SELECTIONS') as f:
    data = json.load(f)
    for path in data.get('deleted', []):
        print(path)
")

if [ -z "$deleted_files" ]; then
    echo "No files to remove"
else
    delete_count=0
    while IFS= read -r file; do
        file_path="$LOCAL_DIR/$file"
        if [ -f "$file_path" ]; then
            rm "$file_path"
            echo "  ✓ Removed: $file"
            ((delete_count++))
        fi
    done <<< "$deleted_files"

    echo ""
    echo "✓ Removed $delete_count files"
fi
echo ""

# Step 7: Process through pipeline (manual step - just instructions)
echo "STEP 6/7: Process Through Standardization Pipeline"
echo "================================================================================"
echo ""
echo "⚠️  MANUAL STEP REQUIRED"
echo ""
echo "New and updated aggregated data files have been downloaded."
echo "You must now run the appropriate standardization scripts to process them:"
echo ""
echo "  For all datasets:"
echo "    Rscript b_scripts/3_standardize/00_run_all.R"
echo ""
echo "  For specific dataset:"
echo "    Rscript b_scripts/3_standardize/test_<dataset_name>.R"
echo ""
echo "After processing completes, the standardized outputs will be ready to push."
echo ""
read -p "Press [Enter] when pipeline processing is complete, or Ctrl+C to abort..."
echo ""

# Step 8: Push changes to OSF
if [ "$SKIP_PUSH" = false ]; then
    echo "STEP 7/7: Pushing Changes to OSF"
    echo "================================================================================"

    if [ "$DRY_RUN" = true ]; then
        .venv/bin/python3 b_scripts/0_sync_osf/push_to_osf.py \
            --selections "$SELECTIONS" \
            --local "$LOCAL_DIR" \
            --remote "$REMOTE_PATH" \
            --dry-run
    else
        .venv/bin/python3 b_scripts/0_sync_osf/push_to_osf.py \
            --selections "$SELECTIONS" \
            --local "$LOCAL_DIR" \
            --remote "$REMOTE_PATH"
    fi

    if [ $? -ne 0 ]; then
        echo "ERROR: Push to OSF failed"
        exit 1
    fi
else
    echo "STEP 7/7: Pushing Changes to OSF (SKIPPED)"
    echo "================================================================================"
    echo ""
fi

# Cleanup
echo ""
echo "================================================================================"
echo "SYNC PIPELINE COMPLETE"
echo "================================================================================"
echo ""
echo "✓ All changes have been synchronized with OSF"
echo ""
echo "Temp files saved in: $TEMP_DIR"
echo "  - diff_report.json: Comparison results"
echo "  - selections.json: User file selections"
echo ""
echo "To clean up temp files: rm -rf $TEMP_DIR"
echo ""
