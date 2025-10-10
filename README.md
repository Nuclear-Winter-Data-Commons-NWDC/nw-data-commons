# Nuclear Winter Data Commons

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://img.shields.io/badge/DOI-10.31223%2FX5XB20-blue)](https://doi.org/10.31223/X5XB20)

This repository contains scripts, data, and documentation for the Nuclear Winter simulation dataset and analysis workflow.

## Citation

If you use this code or data in your research, please cite:

```bibtex
@article{harrison2024accessible,
  title={Accessible Climate and Impact Model Output for Studying the Human and Environmental Impacts of Nuclear Conflict},
  author={Harrison, Cheryl and others},
  year={2024},
  publisher={EarthArXiv},
  doi={10.31223/X5XB20},
  url={https://eartharxiv.org/repository/view/10406/}
}
```

**Preprint:** Harrison, C. et al. (2024). "Accessible Climate and Impact Model Output for Studying the Human and Environmental Impacts of Nuclear Conflict." *EarthArXiv*. https://doi.org/10.31223/X5XB20

The preprint is also included in this repository as `NW_Data_Harrison_preprint.pdf`.

## Data Access

The primary data source for this project is hosted on the Open Science Framework (OSF) at https://osf.io/e28gq/.

**Note:** Access to the data requires submitting an application to OSF with a description of your intended use. Some model outputs are currently available; all outputs will be available by the time of publication.

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

## Repository Structure

```
nw-data-commons/
├── b_data/                    # Data directory
│   ├── 1_scenario_definitions/    # Scenario definitions and model output manifests
│   ├── 2_model_outputs/           # Raw model outputs (downloaded)
│   └── 3_aggregated/              # Aggregated data files
├── c_scripts/                 # Analysis scripts
│   ├── 1_download_or_extract/     # Data download scripts
│   ├── 2_aggregate/               # Aggregation notebooks
│   └── 3_standardize/             # R data processing pipeline
├── install_system_deps.sh     # System dependencies installer
├── requirements.txt           # Python dependencies
└── renv.lock                  # R environment lock file
```

---

## Contributing

We welcome contributions! Please:
1. Fork the repository
2. Create a feature branch
3. Submit a pull request with a clear description of changes

For major changes, please open an issue first to discuss what you would like to change.

---

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

The MIT License allows you to freely use, modify, and distribute this code, provided you include the original copyright notice.

---

## Authors & Contact

**Maintainers:**
- **William Faulkner** (Corresponding Author)
  Science Policy Research Unit, University of Sussex
  Email: william@fluxrme.com

**Contributors:**
- **Victoria Garza**
  Louisiana State University

- **E. Kesse Asante**
  Louisiana State University

For questions, issues, or collaboration inquiries, please:
- Open an issue on the [GitHub repository](https://github.com/wnfaulkner/nw-data-commons)
- Contact the corresponding author via email

---

## Acknowledgments

This repository supports the research presented in Harrison et al. (2024). We thank all contributors to the original climate and impact modeling efforts that produced the underlying datasets.

---

*This README will be updated as the project develops.*