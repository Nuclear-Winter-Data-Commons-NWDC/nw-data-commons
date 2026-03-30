# TEST DOWNWELLING SHORTWAVE RADIATION PIPELINE ----
# End-to-end test of downwelling shortwave radiation cleaning and export

# Note: The cleaning script sources all necessary utilities internally
source("b_scripts/3_standardize/downwelling_shortwave_radiation_cleaning.R")

# 4. Create clean.tables.ls for export
clean.tables.ls <- list()
clean.tables.ls[["downwelling.shortwave.radiation"]] <- downwelling.shortwave.radiation.clean.tb

message("Created clean.tables.ls with ", length(clean.tables.ls), " table(s)")
message("Table name: ", names(clean.tables.ls)[1], " ")
message("Rows: ", nrow(clean.tables.ls[[1]]), " ")
message("Columns: ", ncol(clean.tables.ls[[1]]), " \n")

message("Sample of cleaned data:")
print(clean.tables.ls[[1]] %>% head(10))

# 5. Apply final cleaning (filter for indicators of concern if configured)
message("\n\nApplying final cleaning...")
source("b_scripts/3_standardize/97_final_cleaning_and_consolidation.R")

# 6. Export standardized data
export_result <- source("b_scripts/3_standardize/00_utils_export.R")

message("\n\nExport complete!")
message("Output directory: ", export_result$value$dir)
message("Excel file: ", export_result$value$xlsx)
