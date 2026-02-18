# Session Summary: 2026-02-18

## Session Start

**Date:** 2026-02-18
**Starting Commit:** 305eebc (Fix critical all_data key mismatch blocking full pipeline execution)
**Session Goal:** Clean up OSF duplicate/deprecated files (one-off); fix upload workflow to prevent recurrence.

---

## OSF Duplicate File Problem

### Problem Statement
OSF contains too many files — duplicates and deprecated versions persist alongside current versions.

### Root Cause Analysis
The upload workflow (`osf_manager.py upload_file`) uses `storage.create_file(remote_path, fp, force=True)`.
OSF's `force=True` flag **replaces** a file at the *exact same path*, but does NOT remove files at *different paths*.
When a new version is uploaded with a new date suffix (e.g., `temperature_v2026-02-13.csv` replacing `temperature_v2025-10-31.csv`), the old file at the old path remains on OSF.

Additionally, `agriculture_clm_v2026-02-13.csv` appears **3 times** on OSF (true duplicate at same path — likely a WaterButler/osfclient bug or multiple upload calls).

### Identified OSF Files to Delete

#### 3_standardized/ — old/deprecated versions
| File | Reason |
|------|--------|
| `3_standardized/sea_ice_v2025-10-31.csv` | Superseded by v2026-02-13 |
| `3_standardized/0_readme_v2026-01-22.md` | Superseded by v2026-02-13 |
| `3_standardized/variables_v2026-01-22.csv` | Superseded by v2026-02-13 |
| `3_standardized/starvation_v2026-01-22.csv` | Superseded by v2026-02-13 |
| `3_standardized/temperature_v2025-10-31.csv` | Superseded by v2026-02-13 |
| `3_standardized/precipitation_v2025-10-31.csv` | Superseded by v2026-02-13 |
| `3_standardized/uv_radiation_v2025-10-31.csv` | Superseded by uv_v2026-02-13 |
| `3_standardized/agriculture_clm_v2025-10-31.csv` | Superseded by v2026-02-13 |
| `3_standardized/agriculture_agmip_v2025-10-31.csv` | Superseded by v2026-02-13 |
| `3_standardized/1_standardized_data_v2026-02-13.ods` | Newer xlsx exists; ODS is redundant |
| `3_standardized/agriculture_clm_v2026-02-13.csv` (duplicate x2) | True duplicate — same file 3x on OSF |
| `3_standardized/uv_radiation_v2025-10-31.csv` | Local copy — same as sea_ice issue |

#### 2_aggregated/ — deprecated metadata files (un-versioned → versioned)
| File | Reason |
|------|--------|
| `2_aggregated/DATASETS_CATALOG_v2026-02-12.md` | This is now in 4_3rd_party_metadata/ |
| `2_aggregated/datasets_v2026-02-12.json` | This is now in 4_3rd_party_metadata/ |

#### 2_aggregated/precipitation/ and 2_aggregated/temperature/ — old versions
| File | Reason |
|------|--------|
| `2_aggregated/precipitation/precipitation_v2025-09-11.xlsx` | Superseded by v2025-09-12 |
| `2_aggregated/temperature/temperature_v2025-09-11.xlsx` | Superseded by v2026-02-12 |

---

## Files Modified/Created

### c_scripts/1_download_or_extract/osf_manager.py
- Added `cleanup_old_versions()` method to OSFManager class
- Added `cmd_cleanup` CLI command: `osf_manager.py cleanup --theme NAME --keep-latest`
- Logic: for a given normalized basename, list all versions on OSF, delete all but most recent

### c_scripts/0_sync_osf/push_to_osf.py (NEW)
- New script: wraps upload with auto-cleanup of old versions
- Before uploading new file, finds all OSF files with same basename (minus date), deletes them
- Then uploads new file

---

## Cleanup Execution Log

### OSF Deletions (one-off)
[To be filled in during session]

### Local Mirror Updates
[To be filled in during session]

---

## Session Status

**Status:** In Progress

---

**Last Updated:** 2026-02-18 (Session in progress)
