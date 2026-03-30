#!/bin/bash
# Fast XLSX to ODS conversion using ssconvert (gnumeric)
# Usage: ./convert_to_ods.sh <path/to/file.xlsx> [output.ods]

set -e

if ! command -v ssconvert &> /dev/null; then
    echo "ERROR: ssconvert not found"
    echo ""
    echo "Install gnumeric for fast XLSX to ODS conversion:"
    echo "  sudo apt install gnumeric"
    echo ""
    echo "Alternative: Use LibreOffice (slower but works):"
    echo "  libreoffice --headless --convert-to ods \"\$1\""
    exit 1
fi

if [ $# -lt 1 ]; then
    echo "Usage: $0 <input.xlsx> [output.ods]"
    exit 1
fi

INPUT="$1"
OUTPUT="${2:-${INPUT%.xlsx}.ods}"

if [ ! -f "$INPUT" ]; then
    echo "ERROR: Input file not found: $INPUT"
    exit 1
fi

echo "Converting: $INPUT"
echo "       to: $OUTPUT"
echo ""

START=$(date +%s)
ssconvert "$INPUT" "$OUTPUT"
END=$(date +%s)
ELAPSED=$((END - START))

INPUT_SIZE=$(du -h "$INPUT" | cut -f1)
OUTPUT_SIZE=$(du -h "$OUTPUT" | cut -f1)

echo ""
echo "✓ Conversion complete in ${ELAPSED}s"
echo "  Input:  $INPUT_SIZE"
echo "  Output: $OUTPUT_SIZE"
