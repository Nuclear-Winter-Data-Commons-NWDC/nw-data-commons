# Testing Instructions for OSF Sync Pipeline

## Prerequisites

**Set OSF Token** (required):
```bash
export OSF_TOKEN=your_token_here
```

Or add to `.env` file in project root:
```
OSF_TOKEN=your_token_here
```

**Note:** Python packages (osfclient, python-dotenv) are already installed in `.venv/`

## Quick Test (Recommended First)

**Just see what would change:**
```bash
.venv/bin/python3 c_scripts/0_sync_osf/compare_osf_local.py \
    --local b_data/osf_data_current \
    --remote /
```

This shows you new, updated, and deleted files WITHOUT making any changes.

## Test Individual Components

### Test 1: Comparison
```bash
.venv/bin/python3 c_scripts/0_sync_osf/compare_osf_local.py \
    --local b_data/osf_data_current \
    --remote / \
    --json test_diff_report.json
```

**Verify:**
```bash
cat test_diff_report.json | head -50
```

### Test 2: File Selection
```bash
.venv/bin/python3 c_scripts/0_sync_osf/interactive_selector.py \
    --input test_diff_report.json \
    --output test_selections.json
```

**At prompt:** Press Enter to accept defaults, or type `skip-new`, `skip-updated`, etc.

**Verify:**
```bash
cat test_selections.json
```

### Test 3: Backup
```bash
bash c_scripts/0_sync_osf/backup_before_update.sh test_selections.json
```

**Verify:**
```bash
ls -lh b_data/osf_data_most_recent_previous/
```

### Test 4: Push (Dry Run)
```bash
.venv/bin/python3 c_scripts/0_sync_osf/push_to_osf.py \
    --selections test_selections.json \
    --local b_data/osf_data_current \
    --remote / \
    --dry-run
```

This shows what WOULD be uploaded/deleted without actually doing it.

## Test Full Pipeline

### Option 1: Dry Run (Safe - No Changes Made)
```bash
bash c_scripts/0_sync_osf/sync_pipeline.sh --dry-run
```

**What happens:**
1. Shows comparison report
2. Press Enter to continue
3. Shows file selection (press Enter for defaults)
4. Backs up files
5. Downloads new/updated files (if any)
6. Prompts to run pipeline processing
7. Shows what WOULD be pushed (but doesn't push)

### Option 2: Skip Push (Download Only)
```bash
bash c_scripts/0_sync_osf/sync_pipeline.sh --skip-push
```

**What happens:**
- Same as above but actually downloads files
- Does NOT push anything to OSF
- Safe for testing downloads

### Option 3: Non-Interactive
```bash
bash c_scripts/0_sync_osf/sync_pipeline.sh --non-interactive --dry-run
```

**What happens:**
- Uses defaults for all selections
- No user prompts except pipeline processing
- Dry run = no actual changes

### Option 4: Full Live Run
```bash
bash c_scripts/0_sync_osf/sync_pipeline.sh
```

**⚠️ WARNING:** This WILL push changes to OSF! Only use after testing above options.

## Troubleshooting

### "OSF token required"
```bash
# Check if token is set
echo $OSF_TOKEN

# Set it
export OSF_TOKEN=your_token_here
```

### "Permission denied"
```bash
chmod +x c_scripts/0_sync_osf/*.sh
chmod +x c_scripts/0_sync_osf/*.py
```

### Python import errors
The scripts use `.venv/bin/python3` which has all required packages.

If you see import errors:
```bash
.venv/bin/pip install osfclient python-dotenv
```

### Can't find files
Make sure you're in the project root:
```bash
cd /home/wnf/code/nw-data-commons
pwd  # Should show: /home/wnf/code/nw-data-commons
```

## Expected Output

### Comparison Report
```
================================================================================
OSF vs LOCAL COMPARISON REPORT
================================================================================

📊 Summary:
   Total files on OSF: 42
   Total files locally: 40
   New files (on OSF, not local): 3
   Updated files (OSF newer): 2
   Deleted files (local only): 1
   Unchanged files: 38

✨ NEW FILES (on OSF, not local) - 3 files:
--------------------------------------------------------------------------------
  + 2_aggregated/new_data_v2026-02-15.xlsx
  ...
```

### Selection Interface
```
================================================================================
FILE SELECTION FOR PIPELINE PROCESSING
================================================================================

📊 Files to Review:
   New files (will be downloaded & processed): 3
   Updated files (will be updated & reprocessed): 2
   Deleted files (will be removed after backup): 1

Your choice [Enter for default]:
```

### Backup Output
```
================================================================================
BACKING UP FILES BEFORE UPDATE
================================================================================

📦 Backing up files that will be updated...
  ✓ Backed up: 2_aggregated/temperature_v2026-02-14.xlsx

================================================================================
BACKUP COMPLETE
  Total files backed up: 2
  Backup location: b_data/osf_data_most_recent_previous
================================================================================
```

## Clean Up After Testing

```bash
# Remove test files
rm -f test_diff_report.json test_selections.json

# Remove temp directory
rm -rf .sync_temp
```

## Recommended Test Sequence

1. **First:** Comparison only (see what would change)
   ```bash
   .venv/bin/python3 c_scripts/0_sync_osf/compare_osf_local.py --local b_data/osf_data_current --remote /
   ```

2. **Second:** Dry run (preview all actions)
   ```bash
   bash c_scripts/0_sync_osf/sync_pipeline.sh --dry-run
   ```

3. **Third:** Skip push (test downloads)
   ```bash
   bash c_scripts/0_sync_osf/sync_pipeline.sh --skip-push
   ```

4. **Finally:** Full live run
   ```bash
   bash c_scripts/0_sync_osf/sync_pipeline.sh
   ```

## One-Off Manual Push (Special Cases)

For pushing specific files to OSF that aren't part of normal sync (e.g., new directories created during development):

### Option 1: Interactive Selection from Deleted Files
```bash
# First, run comparison to get deleted files list
.venv/bin/python3 c_scripts/0_sync_osf/compare_osf_local.py \
    --local b_data/osf_data_current \
    --remote / \
    --json diff_report.json

# Then interactively select which "deleted" files to actually push to OSF
.venv/bin/python3 c_scripts/0_sync_osf/manual_push.py \
    --from-comparison diff_report.json \
    --local b_data/osf_data_current \
    --remote /
```

**At the selection prompt:**
- Enter numbers: `1,3,5-8,12` (selects files 1, 3, 5, 6, 7, 8, and 12)
- Enter `all` to push all deleted files
- Enter `none` to cancel

### Option 2: Push Specific Files Directly
```bash
.venv/bin/python3 c_scripts/0_sync_osf/manual_push.py \
    --files "4_3rd_party_metadata/countries.csv" "4_3rd_party_metadata/ports.csv" \
    --local b_data/osf_data_current \
    --remote /
```

### Option 3: Push from File List
```bash
# Create a text file with file paths (one per line)
cat > files_to_push.txt << EOF
4_3rd_party_metadata/countries.csv
4_3rd_party_metadata/fao_crop_indicators.csv
4_3rd_party_metadata/ports.csv
EOF

# Push all files in the list
.venv/bin/python3 c_scripts/0_sync_osf/manual_push.py \
    --file-list files_to_push.txt \
    --local b_data/osf_data_current \
    --remote /
```

### Dry Run First (Recommended)
```bash
# Preview what would be pushed
.venv/bin/python3 c_scripts/0_sync_osf/manual_push.py \
    --from-comparison diff_report.json \
    --local b_data/osf_data_current \
    --remote / \
    --dry-run
```

## Quick Commands Reference

```bash
# Just compare (no changes)
.venv/bin/python3 c_scripts/0_sync_osf/compare_osf_local.py --local b_data/osf_data_current --remote /

# Dry run full pipeline
bash c_scripts/0_sync_osf/sync_pipeline.sh --dry-run

# Download but don't push
bash c_scripts/0_sync_osf/sync_pipeline.sh --skip-push

# Full sync
bash c_scripts/0_sync_osf/sync_pipeline.sh

# Non-interactive with defaults
bash c_scripts/0_sync_osf/sync_pipeline.sh --non-interactive

# One-off push (interactive selection)
.venv/bin/python3 c_scripts/0_sync_osf/manual_push.py --from-comparison diff_report.json --local b_data/osf_data_current --remote /
```
