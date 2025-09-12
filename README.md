# Nuclear Winter Data Commons

This repository contains scripts, data, and documentation for the Nuclear Winter simulation dataset and analysis workflow.

## Current Status

- **Data download, extraction, and aggregation workflows are works-in-progress.**
- Downloaded `.tar.gz` files are automatically extracted, but resulting `.nc` files may appear in nested directories.
- Flattening and aggregation steps are not yet fully replicable.
- Users may need to manually locate and move extracted files for analysis.

---

## Data Workflows

### 1. **Workflow A: Start with Aggregated Data (.xlsx files)**

For users who want to begin with pre-aggregated data:

1. Ensure your OSF token is set in your `.env` file or as an environment variable (`OSF_TOKEN`).
2. Use the manifest at `b_data/3_aggregated/osf_manifest_aggregated.json`.
3. Download the aggregated Excel files by running:
   ```
   python c_scripts/1_download_or_extract/osf_fetch.py --manifest b_data/3_aggregated/osf_manifest_aggregated.json
   ```
4. The `.xlsx` files will be downloaded to `b_data/3_aggregated/`.
5. Use the R script `c_scripts/3_standardize/01_import_aggregated_data.R` to import and process the Excel files.

---

### 2. **Workflow B: Reproduce Aggregated Data from Model Outputs**

For users who wish to reproduce the aggregation process from raw model outputs:

1. Ensure your OSF token is set in your `.env` file or as an environment variable (`OSF_TOKEN`).
2. Use the manifest at `b_data/1_scenario_definitions/osf_manifest_model_outputs.json`.
3. Download the model output files by running:
   ```
   python c_scripts/1_download_or_extract/osf_fetch.py --manifest b_data/1_scenario_definitions/osf_manifest_model_outputs.json
   ```
4. The model output files will be downloaded to `b_data/2_model_outputs/`.
5. Use the Jupyter notebook at `c_scripts/2_aggregate/nuclear_ocean_calculate_mean_min_max_stdev.ipynb` to flatten and aggregate the data.
6. Aggregated outputs will be saved to `b_data/3_aggregated/` for further analysis.

---

## Getting Started

1. Clone the repository.
2. Install dependencies:
   ```
   pip install -r requirements.txt
   ```
   For R, use:
   ```
   renv::restore()
   ```
3. Choose your workflow and download the appropriate data using the manifest and commands above.
4. Follow the analysis scripts in `c_scripts/3_standardize/` and `c_scripts/4_analysis_examples/`.

## Contact

For questions or updates, please see the issues tracker or contact the maintainers.

---

*This README will be updated as the project develops.*