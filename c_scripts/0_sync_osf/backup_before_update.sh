#!/bin/bash
# Backup files before updating
#
# Copies files about to be updated/deleted to osf_data_most_recent_previous
# maintaining directory structure
#
# Usage:
#   bash backup_before_update.sh selections.json

set -e

if [ $# -lt 1 ]; then
    echo "Usage: $0 <selections.json>"
    exit 1
fi

SELECTIONS_FILE="$1"
CURRENT_DIR="b_data/osf_data_current"
BACKUP_DIR="b_data/osf_data_most_recent_previous"

if [ ! -f "$SELECTIONS_FILE" ]; then
    echo "ERROR: Selections file not found: $SELECTIONS_FILE"
    exit 1
fi

echo "================================================================================"
echo "BACKING UP FILES BEFORE UPDATE"
echo "================================================================================"
echo ""

# Parse JSON to get files to backup (updated and deleted files)
updated_files=$(.venv/bin/python3 -c "
import json
with open('$SELECTIONS_FILE') as f:
    data = json.load(f)
    for path in data.get('updated', []):
        print(path)
" | tr '\n' '\0')

deleted_files=$(.venv/bin/python3 -c "
import json
with open('$SELECTIONS_FILE') as f:
    data = json.load(f)
    for path in data.get('deleted', []):
        print(path)
" | tr '\n' '\0')

backup_count=0

# Backup updated files
if [ -n "$updated_files" ]; then
    echo "📦 Backing up files that will be updated..."
    echo ""

    while IFS= read -r -d '' file; do
        if [ -z "$file" ]; then
            continue
        fi

        source_path="$CURRENT_DIR/$file"
        dest_path="$BACKUP_DIR/$file"

        if [ -f "$source_path" ]; then
            # Create parent directory
            dest_dir=$(dirname "$dest_path")
            mkdir -p "$dest_dir"

            # Copy file with timestamp preservation
            cp -p "$source_path" "$dest_path"
            echo "  ✓ Backed up: $file"
            ((backup_count++))
        else
            echo "  ⚠️  File not found (skipping): $file"
        fi
    done <<< "$updated_files"
fi

# Backup deleted files
if [ -n "$deleted_files" ]; then
    echo ""
    echo "📦 Backing up files that will be deleted..."
    echo ""

    while IFS= read -r -d '' file; do
        if [ -z "$file" ]; then
            continue
        fi

        source_path="$CURRENT_DIR/$file"
        dest_path="$BACKUP_DIR/$file"

        if [ -f "$source_path" ]; then
            # Create parent directory
            dest_dir=$(dirname "$dest_path")
            mkdir -p "$dest_dir"

            # Copy file with timestamp preservation
            cp -p "$source_path" "$dest_path"
            echo "  ✓ Backed up: $file"
            ((backup_count++))
        else
            echo "  ⚠️  File not found (skipping): $file"
        fi
    done <<< "$deleted_files"
fi

echo ""
echo "================================================================================"
echo "BACKUP COMPLETE"
echo "  Total files backed up: $backup_count"
echo "  Backup location: $BACKUP_DIR"
echo "================================================================================"
echo ""
