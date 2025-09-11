# Nuclear Winter Data Commons

This repository contains scripts, data, and documentation for the Nuclear Winter simulation dataset and analysis workflow.

## Current Status

- **Data download, extraction, and aggregation workflows are works-in-progress.**
- Downloaded `.tar.gz` files are automatically extracted, but resulting `.nc` files may appear in nested directories.
- Flattening and aggregation steps are not yet fully replicable.
- Users may need to manually locate and move extracted files for analysis.

## Two Data Workflows

### 1. Start with Aggregated Data (.xlsx files)

For users who want to begin with pre-aggregated data:

- Use the provided `osf_manifest_aggregated.json` to download all `.xlsx` files from OSF/3_aggregated.
- Run:
  ```
  python c_scripts/1_download_or_extract/osf_fetch.py --manifest b_data/1_scenario_definitions/osf_manifest_aggregated.json
  ```
- The files will be downloaded to `b_data/3_aggregated/` for use in the R scripts.
- See `c_scripts/3_standardize/01_import_raw_data.R` for import instructions.

### 2. Reproduce Aggregated Data from Model Outputs

For users who wish to reproduce the aggregation process:

- Use `osf_manifest_model_outputs.json` to download model output files.
- Run the Jupyter notebook in `c_scripts/2_aggregate/` to flatten and aggregate the data.
- Output files will be saved in `b_data/3_aggregated/` for further analysis.

## Getting Started

1. Clone the repository.
2. Install dependencies (see `requirements.txt` and `renv.lock`).
3. Choose your workflow and download the appropriate data using the manifest.
4. Follow the analysis scripts in `c_scripts/3_standardize/` and `c_scripts/4_analysis_examples/`.

## Contact

For questions or updates, please see the issues tracker or contact the maintainers.

---

*This README will be updated as the workflow becomes fully replicable.*