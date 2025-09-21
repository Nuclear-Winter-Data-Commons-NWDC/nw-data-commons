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
library(magrittr)   # for %<>%

# Set the directory containing the .xlsx files
xlsx_dir <- "b_data/3_aggregated/"

# List all .xlsx files in the directory
xlsx_files <- list.files(xlsx_dir, pattern = "\\.xlsx$", full.names = TRUE)

# Function to import all sheets from a single .xlsx file
import_all_sheets <- function(file_path) {
  sheet_names <- readxl::excel_sheets(file_path)
  sheets_list <- lapply(sheet_names, function(sheet) {
    readxl::read_excel(file_path, sheet = sheet)
  })
  names(sheets_list) <- sheet_names
  return(sheets_list)
}

# Import all .xlsx files and their sheets
all_data <- lapply(xlsx_files, import_all_sheets)
names(all_data) <- basename(xlsx_files)

# Assign each sheet to the global environment as a tibble
for (file in names(all_data)) {
  for (sheet in names(all_data[[file]])) {
    obj_name <- paste0(
      stringr::str_replace_all(tools::file_path_sans_ext(file), "\\.", "_"),
      "_",
      stringr::str_replace_all(sheet, "\\s+", "_")
    )
    assign(obj_name, all_data[[file]][[sheet]], envir = .GlobalEnv)
    print(paste("Imported:", obj_name))
  }
}

# Simplify top-level names by dropping ".xlsx"
names(all_data) %<>% gsub("\\.xlsx$", "", .)

# ---------------------------------------------------------------------------
# Ensure UPDATED configs workbook (0.configs.xlsx) is wired correctly:
# - Keep all existing config sheets.
# - Make sure 'variables' sheet exists under all_data[["0.configs"]]$variables
#   with lower-cased headers and a normalized 'range' column.
# ---------------------------------------------------------------------------
configs_path <- file.path("b_data", "3_aggregated", "0.configs.xlsx")
if (!file.exists(configs_path)) {
  stop("Expected configs workbook not found at: ", configs_path)
}

# Create configs container if missing
if (is.null(all_data[["0.configs"]])) {
  all_data[["0.configs"]] <- list()
}

# (Re)load variables sheet explicitly to ensure it reflects the latest file
configs_variables_tb <- readxl::read_excel(configs_path, sheet = "variables")
# Lower-case and trim headers for consistency
names(configs_variables_tb) <- tolower(trimws(names(configs_variables_tb)))

# Normalize header alias: 'range/unique values' -> 'range' (keep both if you like)
if (!"range" %in% names(configs_variables_tb) && "range/unique values" %in% names(configs_variables_tb)) {
  configs_variables_tb$range <- configs_variables_tb[["range/unique values"]]
}

# Store the cleaned variables table back into all_data
all_data[["0.configs"]][["variables"]] <- configs_variables_tb

# Also expose a convenient variables.tb in the global env (optional, matches your pattern)
assign("variables.tb", configs_variables_tb, envir = .GlobalEnv)

# ---------------------------------------------------------------------------
# Convenience references (kept from your current version)
# ---------------------------------------------------------------------------
configs <- all_data[["0.configs"]]

# These will be NULL if the corresponding sheet is missing.
months.tb                <- configs[["months"]]
countries.tb             <- configs[["countries"]]
source.table.configs.tb  <- configs[["source.table.configs"]]
scenarios.tb             <- configs[["scenarios"]]
variables.tb             <- configs[["variables"]]              # (cleaned just above)
fao.crop.indicators.tb   <- configs[["fao.crop.indicators"]]
fish.catch.indicators.tb <- configs[["fish.catch.indicators"]]
fish.catch.eez.tb        <- configs[["fish.catch.eez"]]
ports.tb                 <- configs[["ports"]]

# all_data is a nested list: all_data[[filename]][[sheetname]]
# Each sheet is also available as a tibble in the global environment
