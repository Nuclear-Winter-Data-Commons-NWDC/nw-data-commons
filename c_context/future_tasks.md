# Future Tasks for Nuclear Winter Data Commons

This file tracks future enhancements and tasks for the project.

## High Priority

### OSF Upload Protocol
- **Status**: Not Started
- **Description**: Develop comprehensive protocol for automatic updates to OSF repository
- **Requirements**:
  - Define versioning strategy for standardized outputs
  - Create validation checks before upload (data integrity, completeness)
  - Implement approval workflow (manual review before upload)
  - Add rollback capability if issues are detected
  - Document when to upload to OSF vs. keeping local only
  - Create manifest tracking what versions are on OSF
  - Add automated changelog generation for each upload
  - Consider using git tags to link OSF versions to code versions
- **Related Files**:
  - `c_scripts/1_download_or_extract/osf_upload.py` (exists, needs enhancement)
  - New: `c_scripts/1_download_or_extract/osf_upload_protocol.md`
  - New: `b_data/4_standardized/UPLOAD_CHECKLIST.md`

## Medium Priority

### Data Export Enhancements
- **Status**: Not Started
- **Description**: Improve export functionality for better compatibility
- **Tasks**:
  - Add ODS format generation alongside XLSX (Open Document Spreadsheet)
  - Consider adding Parquet format for large datasets
  - Add data validation reports in exports
  - Include metadata files with each export

### Pipeline Improvements
- **Status**: Not Started
- **Description**: Enhance data processing pipeline
- **Tasks**:
  - Add automated testing for cleaning scripts
  - Create data quality reports after each run
  - Add logging to track processing time and errors
  - Consider parallelization for large datasets

### Documentation
- **Status**: Not Started
- **Description**: Improve project documentation
- **Tasks**:
  - Create detailed data dictionary
  - Document outlier detection methodology
  - Add workflow diagrams
  - Create user guide for running pipeline

## Low Priority

### Code Quality
- **Status**: Not Started
- **Description**: Improve code maintainability
- **Tasks**:
  - Add unit tests for utility functions
  - Refactor repetitive code in cleaning scripts
  - Add type hints to Python scripts
  - Create R package for common functions

### Infrastructure
- **Status**: Not Started
- **Description**: Improve development and deployment infrastructure
- **Tasks**:
  - Set up CI/CD for automated testing
  - Create Docker container for reproducible environment
  - Add pre-commit hooks for code quality

---

## Completed Tasks

### Starvation Data Integration
- **Completed**: 2026-01-16
- **Description**: Integrated starvation data into the standardized pipeline
- **Details**: Added cleaning script, updated export workflow, generated 7,632 rows of standardized data

---

**Last Updated**: 2026-01-16
**Maintainer**: William Faulkner
