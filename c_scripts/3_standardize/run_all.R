# run_all.R
# Master script to execute entire data cleaning pipeline

# 0. Restore renv environment (installs all required packages)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::restore(prompt = FALSE)
}

# 1. Load utility functions
source("c_scripts/3_standardize/00_utils.R")

# 2. Import aggregated .xlsx data
source("c_scripts/3_standardize/01_import_aggregated_data.R")

# 3. Cleaning, Reshaping & Adding Useful Variables
source("c_scripts/3_standardize/02_clean_temperature.R")
source("c_scripts/3_standardize/03_clean_precipitation.R")
source("c_scripts/3_standardize/04_clean_uv.R")
source("c_scripts/3_standardize/05_clean_agriculture_agmip.R")
source("c_scripts/3_standardize/06_clean_agriculture_clm.R")
source("c_scripts/3_standardize/07_clean_fish_catch.R")
source("c_scripts/3_standardize/08_clean_sea_ice.R")

# 4. Merge all cleaned datasets and save final output
source("c_scripts/3_standardize/09_final_cleaning_and_consolidation.R")

# 5. Export Cleaned Datasets
source("c_scripts/3_standardize/10_export.R")

# 6. Optionally run example analyses
# source("c_scripts/4_analysis_examples/example_visualization.r")