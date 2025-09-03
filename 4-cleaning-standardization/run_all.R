#00000000000000000000000000000000000000000000000000000000000#
#0000       2024-11 NW Data Commons                     0000#
#00000000000000000000000000000000000000000000000000000000000#

# run_all.R
# Master script to execute entire data cleaning pipeline

# 1. Load utility functions
source("4-cleaning-standardization/00_utils.R")

# 2. Import raw data
source("4-cleaning-standardization/01_import_raw_data.R")

# 3. Cleaning, Reshaping & Adding Useful Variables
source("4-cleaning-standardization/02_clean_temperature.R")
source("4-cleaning-standardization/03_clean_precipitation.R")
source("4-cleaning-standardization/03_clean_uv.R")
source("4-cleaning-standardization/03_clean_agriculture_agmip.R")
source("4-cleaning-standardization/03_clean_agriculture_clm.R")
source("4-cleaning-standardization/03_clean_fish_catch.R")
source("4-cleaning-standardization/08_clean_sea_ice.R")

# 4. Merge all cleaned datasets and save final output
source("4-cleaning-standardization/09_final_cleaning_and_consolidation.R")

# 5. Export Cleaned Datasets
source("4-cleaning-standardization/10_export.R")

# 5. Optionally run example analyses
# source("5-examples-paper/example_analysis.R")
