# ODS Conversion Research

**Date**: 2026-01-16
**Issue**: Converting large XLSX files (56MB, 10 sheets) to ODS format is extremely slow

## Tested Approaches

### 1. R with readODS package (v2.3.2)
- **Performance**: EXTREMELY SLOW
- **Result**: 46MB file with only 5/10 sheets after >10 minutes
- **Bottleneck**: `write_ods()` function is not optimized for large datasets
- **Verdict**: ❌ Not suitable for production

### 2. Python with pandas + odfpy
- **Performance**: VERY SLOW
- **Result**: Timed out after 2 minutes on single sheet test
- **Bottleneck**: odfpy library writes ODS row-by-row
- **Verdict**: ❌ Not suitable for production

## Recommended Solutions (Fastest to Slowest)

### Option 1: Gnumeric ssconvert (FASTEST) ⭐
```bash
sudo apt install gnumeric
ssconvert input.xlsx output.ods
```
- **Speed**: Converts 56MB files in seconds
- **Quality**: Excellent format preservation
- **Drawback**: Requires system package installation
- **Best for**: Production use, CI/CD pipelines

### Option 2: LibreOffice Headless (FAST)
```bash
libreoffice --headless --convert-to ods --outdir output_dir input.xlsx
```
- **Speed**: Converts in <1 minute typically
- **Quality**: Perfect format preservation (native support)
- **Drawback**: Requires LibreOffice installation (~500MB)
- **Best for**: Desktop environments, occasional conversions

### Option 3: pyexcel + pyexcel-ods3 (MODERATE)
```python
import pyexcel
pyexcel.save_book_as(file_name='input.xlsx', dest_file_name='output.ods')
```
- **Speed**: Faster than pandas/readODS but still slow for large files
- **Quality**: Good
- **Drawback**: Additional dependencies
- **Best for**: Small to medium files (<10MB)

### Option 4: Keep Separate Formats (PRACTICAL) ⭐
- Provide XLSX for Excel/LibreOffice users
- Provide CSV files for maximum compatibility
- Only generate ODS on-demand or for specific use cases
- **Best for**: Avoiding conversion overhead entirely

## Dataset Characteristics
- Size: 56MB XLSX, 167MB total with CSVs
- Sheets: 10 (readme, variables, 8 data sheets)
- Largest sheets: temperature (46MB CSV), precipitation (45MB CSV)
- Rows: Up to 7,632 per sheet with many columns

## Recommendation for This Project

**Use ssconvert if available**, otherwise document ODS generation as an optional/manual step:

1. Check for ssconvert at runtime
2. If available: fast conversion
3. If not available: skip ODS or provide instructions

**Implementation**:
```bash
if command -v ssconvert &> /dev/null; then
    ssconvert input.xlsx output.ods
else
    echo "ODS conversion skipped (install gnumeric for fast conversion)"
fi
```

**Alternative**: Document in README that users can convert manually:
```bash
# For ODS format (requires gnumeric):
sudo apt install gnumeric
ssconvert 0_standardized_data.xlsx 0_standardized_data.ods
```
