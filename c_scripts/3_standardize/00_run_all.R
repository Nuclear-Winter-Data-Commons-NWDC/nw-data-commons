# run_all.R
# Master script to execute entire data cleaning pipeline

# 0. Restore renv environment (installs all required packages)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::restore(prompt = FALSE)
}

# 1. Load utility functions
source("c_scripts/3_standardize/00_utils_core.R")
source("c_scripts/3_standardize/00_utils_validate.R")

# 2. Import aggregated .xlsx data
source("c_scripts/3_standardize/00_utils_import.R")

# 3. Cleaning, Reshaping & Adding Useful Variables
source("c_scripts/3_standardize/temperature_cleaning.R")
source("c_scripts/3_standardize/precipitation_cleaning.R")
source("c_scripts/3_standardize/uv_radiation_cleaning.R")
source("c_scripts/3_standardize/agriculture_agmip_cleaning.R")
source("c_scripts/3_standardize/agriculture_clm_cleaning.R")
source("c_scripts/3_standardize/fish_catch_cleaning.R")
source("c_scripts/3_standardize/sea_ice_cleaning.R")
source("c_scripts/3_standardize/starvation_cleaning.R")

# 4. Merge all cleaned datasets and save final output
source("c_scripts/3_standardize/97_final_cleaning_and_consolidation.R")

# 5. Export Cleaned Datasets
source("c_scripts/3_standardize/00_utils_export.R")

# 6. Optionally run example analyses
# source("c_scripts/4_analysis_examples/example_visualization.r")