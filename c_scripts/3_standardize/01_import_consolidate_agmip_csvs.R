# One-off script to combine all AGMIP CSVs into a single Excel workbook with multiple sheets
# Each CSV becomes a separate sheet named after the CSV file (without .csv extension)

library(openxlsx)

# Define the folder containing your CSVs
input_folder <- paste(wd, "/b_data/3_aggregated/4a.agriculture.agmip.csvs/", sep = "")
output_file <- paste(wd, "/b_data/3_aggregated/4a.agriculture.agmip_2025-09-24.xlsx", sep = "")

# List all CSV files in that folder
csv_files <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)

# Create a new Excel workbook
wb <- createWorkbook()

# Loop over CSVs and add each as a new sheet
for (file in csv_files) {
  # Use file name (without .csv) as sheet name
  sheet_name <- 
    file %>%
    basename() %>%
    strsplit("_") %>%
    unlist() %>%
    { paste0(.[1], "_", .[2], "_", .[3], "_", tail(., 2)[1]) }

  # Read CSV
  data <- read.csv(file)

  # Add to workbook
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = data)
}

# Save workbook
saveWorkbook(wb, file = output_file, overwrite = TRUE)

cat("✅ Excel file saved to:", output_file, "\n")
