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
   ```sh
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
   ```sh
   python c_scripts/1_download_or_extract/osf_fetch.py --manifest b_data/1_scenario_definitions/osf_manifest_model_outputs.json
   ```
4. The model output files will be downloaded to `b_data/2_model_outputs/`.
5. Use the Jupyter notebook at `c_scripts/2_aggregate/nuclear_ocean_calculate_mean_min_max_stdev.ipynb` to flatten and aggregate the data.
6. Aggregated outputs will be saved to `b_data/3_aggregated/` for further analysis.

---

## Getting Started

1. **Clone the repository.**

2. **Install Python dependencies:**
   ```sh
   pip install -r requirements.txt
   ```

3. **Install system dependencies (Linux/Ubuntu):**
   ```sh
   bash install_system_deps.sh
   ```
   *If you are on MacOS or Windows, see the comments in `install_system_deps.sh` for guidance or install equivalent libraries using your OS package manager.*

4. **Restore the R environment:**
   ```r
   renv::restore()
   ```
   This will install all required R packages as specified in `renv.lock`.

5. **Choose your workflow and download the appropriate data using the manifest and commands above.**

6. **Run the R pipeline:**
   ```r
   source("c_scripts/3_standardize/run_all.R")
   ```
   This will execute the full data cleaning and analysis pipeline.

---

## Troubleshooting

- If you encounter errors about missing system libraries during R package installation, install them using your OS package manager and let us know so we can update the script.
- For R package issues, ensure you are running `renv::restore()` from the project root directory.

---

## Contact

For questions or updates, please see the issues tracker or contact the maintainers.

---

*This README will be updated as the project develops.*