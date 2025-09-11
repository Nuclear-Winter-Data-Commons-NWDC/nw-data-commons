# 01_import_aggregated_data.R
# Import all .xlsx files from b_data/3_aggregated and load each sheet as a tibble

# INITIAL SETUP
rm(list = ls())
gc()

library(readxl)
library(tibble)
library(dplyr)
library(stringr)
library(purrr)

# Set the directory containing the .xlsx files
xlsx_dir <- "b_data/3_aggregated/"

# List all .xlsx files in the directory
xlsx_files <- list.files(xlsx_dir, pattern = "\\.xlsx$", full.names = TRUE)

# Function to import all sheets from a single .xlsx file
import_all_sheets <- function(file_path) {
  sheet_names <- excel_sheets(file_path)
  sheets_list <- lapply(sheet_names, function(sheet) {
    read_excel(file_path, sheet = sheet)
  })
  names(sheets_list) <- sheet_names
  return(sheets_list)
}

# Import all .xlsx files and their sheets
all_data <- lapply(xlsx_files, import_all_sheets)
names(all_data) <- basename(xlsx_files)

# Optionally, assign each sheet to the global environment as a tibble
for (file in names(all_data)) {
  for (sheet in names(all_data[[file]])) {
    obj_name <- paste0(str_replace_all(tools::file_path_sans_ext(file), "\\.", "_"), "_", str_replace_all(sheet, "\\s+", "_"))
    assign(obj_name, all_data[[file]][[sheet]], envir = .GlobalEnv)
    print(paste("Imported:", obj_name))
  }
}

# all_data is a nested list: all_data[[filename]][[sheetname]]
# Each sheet is also available as a tibble in the global environment