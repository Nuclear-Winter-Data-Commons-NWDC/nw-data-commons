# Accessible Model Outputs for Studying the Human and Environmental Consequences of Nuclear Conflict
# Nuclear Winter Data Commons

This repository contains scripts, data, and documentation for the Nuclear Winter simulation dataset and analysis workflow.

## Current Status

- **Data download, extraction, and aggregation workflows are works-in-progress.**
- Downloaded `.tar.gz` files are automatically extracted, but resulting `.nc` files may appear in nested directories.
- Flattening and aggregation steps are not yet fully replicable.
- Users may need to manually locate and move extracted files for analysis.

## Getting Started

1. Clone the repository.
2. Install dependencies (see `requirements.txt` and `renv.lock`).
3. Use `osf_fetch.py` to download and extract data files from OSF.
4. See `c_scripts/2_aggregate/nuclear_ocean_calculate_mean_min_max_stdev.ipynb` for example analysis (paths may need manual adjustment).

## Contact

For questions or updates, please see the issues tracker or contact the maintainers.

---

*This README will be updated as the workflow becomes fully replicable.*