#!/usr/bin/env Rscript
# Create ODS file from existing CSVs in a standardized output directory
# Usage: Rscript create_ods_from_csvs.R <path/to/standardized/dir>

library(readODS)
library(readr)
library(readxl)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript create_ods_from_csvs.R <standardized_dir>")
}

standardized_dir <- args[1]
if (!dir.exists(standardized_dir)) {
  stop(paste("Directory does not exist:", standardized_dir))
}

ods_path <- file.path(standardized_dir, "0_standardized_data.ods")
xlsx_path <- file.path(standardized_dir, "0_standardized_data.xlsx")

# Data sheets to include
data_sheets <- c(
  "temperature",
  "precipitation",
  "uv",
  "agriculture.agmip",
  "agriculture.clm",
  "fish.catch",
  "sea.ice",
  "starvation"
)

cat("Creating ODS file from CSVs in:", standardized_dir, "\n")

# 1) Add readme from Excel file
if (file.exists(xlsx_path)) {
  cat("Reading readme from Excel file...\n")
  readme_df <- read_excel(xlsx_path, sheet = "readme")
  write_ods(readme_df, ods_path, sheet = "readme")
  cat("Added readme sheet\n")

  # 2) Add variables from Excel file
  cat("Reading variables from Excel file...\n")
  variables_df <- read_excel(xlsx_path, sheet = "variables")
  write_ods(variables_df, ods_path, sheet = "variables", append = TRUE)
  cat("Added variables sheet\n")
}

# 3) Add data sheets from CSVs
for (sheet_name in data_sheets) {
  csv_file <- file.path(standardized_dir, paste0(sheet_name, ".csv"))

  if (file.exists(csv_file)) {
    cat("Adding", sheet_name, "from CSV...\n")
    df <- read_csv(csv_file, show_col_types = FALSE)
    write_ods(df, ods_path, sheet = sheet_name, append = TRUE)
    cat("Added", sheet_name, "sheet\n")
  } else {
    cat("Warning: CSV file not found:", csv_file, "\n")
  }
}

# Get file size
file_info <- file.info(ods_path)
size_mb <- file_info$size / (1024 * 1024)

cat("\n=== ODS Creation Complete ===\n")
cat("Output file:", ods_path, "\n")
cat("File size:", round(size_mb, 2), "MB\n")

# List sheets in the ODS file
sheets <- list_ods_sheets(ods_path)
cat("Sheets (", length(sheets), "):", paste(sheets, collapse = ", "), "\n")
