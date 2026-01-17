# ODS Format Generation Instructions

The pipeline now includes ODS (Open Document Spreadsheet) format support, but requires additional setup for optimal performance.

## Quick Start

### Option 1: Install Gnumeric (Recommended - FAST)

```bash
sudo apt install gnumeric
```

After installation, the pipeline will automatically generate ODS files alongside XLSX.

**Performance**: Converts 56MB XLSX to ODS in seconds.

### Option 2: Manual Conversion

If you don't have gnumeric installed, convert manually after running the pipeline:

```bash
# Install gnumeric
sudo apt install gnumeric

# Convert existing XLSX to ODS
./convert_to_ods.sh b_data/4_standardized/<timestamp>/0_standardized_data.xlsx
```

### Option 3: Use LibreOffice (Slower Alternative)

```bash
cd b_data/4_standardized/<timestamp>/
libreoffice --headless --convert-to ods 0_standardized_data.xlsx
```

**Performance**: Takes ~30-60 seconds for 56MB files.

## Technical Details

### Why Not Pure R/Python?

We tested multiple approaches:

| Method | Speed | Status |
|--------|-------|--------|
| R `readODS` package | ❌ 10+ minutes (incomplete) | Too slow |
| Python `pandas + odfpy` | ❌ 2+ minutes per sheet | Too slow |
| Gnumeric `ssconvert` | ✅ <10 seconds | **Recommended** |
| LibreOffice CLI | ✅ 30-60 seconds | Good fallback |

**Root cause**: Both R and Python ODS libraries write row-by-row, which is extremely inefficient for large datasets (45M+ rows across sheets).

### Current Pipeline Behavior

The export script (`c_scripts/3_standardize/11_export.R`) now:

1. Checks if `ssconvert` is available
2. If yes: Automatically converts XLSX → ODS (fast)
3. If no: Skips ODS and prints installation instructions

### Files Added

- `convert_to_ods.sh` - Standalone conversion script
- `.claude/ods_conversion_research.md` - Detailed performance analysis
- `c_scripts/3_standardize/create_ods_from_csvs.R` - Legacy (slow, not recommended)

## For System Administrators

Add to your deployment scripts:

```bash
# Debian/Ubuntu
apt-get update && apt-get install -y gnumeric

# RHEL/CentOS
yum install -y gnumeric

# MacOS
brew install gnumeric
```

## Troubleshooting

**Q: Pipeline runs but no ODS file generated?**
A: Install gnumeric: `sudo apt install gnumeric`

**Q: Can I use the CSV files instead?**
A: Yes! All data is available as CSV files, which have better compatibility and performance.

**Q: Why do we need ODS format?**
A: Open Document Format (ODF) is an ISO standard, preferred by some institutions and required for certain grant reporting.

---

**Last Updated**: 2026-01-16
**See Also**: `.claude/ods_conversion_research.md` for performance benchmarks
