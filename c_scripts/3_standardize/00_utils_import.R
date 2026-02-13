# IMPORT UTILITIES -------------------------------------------------------------
# Import aggregated data and configuration files

# INITIAL SETUP
rm(list = ls())
gc()

library(readxl)
library(tibble)
library(dplyr)
library(stringr)
library(purrr)
library(magrittr)   # for %<>%

# ---------------------------------------------------------------------------
# Load configs workbook (single source of truth for metadata)
# ---------------------------------------------------------------------------
configs_wb_path <- "b_data/osf_data_current/0_configs/configs_v2026-01-21.xlsx"
if (!file.exists(configs_wb_path)) {
  stop("Expected configs workbook not found at: ", configs_wb_path)
}

# Load all sheets from configs workbook
configs_sheets <- readxl::excel_sheets(configs_wb_path)
configs_data <- lapply(configs_sheets, function(sheet) {
  readxl::read_excel(configs_wb_path, sheet = sheet)
})
names(configs_data) <- configs_sheets

# ---------------------------------------------------------------------------
# Import aggregated data files from OSF mirror directory
# ---------------------------------------------------------------------------
# Single source of truth: OSF mirror directory
xlsx_dir <- "b_data/osf_data_current/2_aggregated/"

# List all .xlsx files in the directory (recursive to catch subdirectories)
xlsx_files <- list.files(xlsx_dir, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

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

# Normalize dataset names: strip version dates and .xlsx suffix
# This ensures backward compatibility with cleaning scripts that expect theme names
# Pattern: theme_name_v2026-01-14.xlsx -> theme_name
names(all_data) <- names(all_data) %>%
  gsub("\\.xlsx$", "", .) %>%                    # Remove .xlsx extension
  gsub("_v\\d{4}-\\d{2}-\\d{2}$", "", .)        # Remove _vYYYY-MM-DD version suffix

# Store original file paths with metadata for change detection in Phase 2
# This preserves actual file modification times for diff detection
all_data_metadata <- data.frame(
  dataset_name = names(all_data),
  file_path = xlsx_files,
  file_mtime = file.info(xlsx_files)$mtime,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Add configs to all_data structure
# ---------------------------------------------------------------------------
all_data[["0.configs"]] <- configs_data

# Extract and process variables table
configs_variables_tb <- configs_data[["variables"]]
if (is.null(configs_variables_tb)) {
  stop("'variables' sheet not found in configs workbook")
}

# Lower-case and trim headers for consistency
names(configs_variables_tb) <- tolower(trimws(names(configs_variables_tb)))

# Also expose a convenient variables.tb in the global env
assign("variables.tb", configs_variables_tb, envir = .GlobalEnv)

# ---------------------------------------------------------------------------
# Load source.table.configs from configs workbook
# ---------------------------------------------------------------------------
source.table.configs.tb <- configs_data[["standardization"]]
if (is.null(source.table.configs.tb)) {
  stop("'standardization' sheet not found in configs workbook")
}

# ---------------------------------------------------------------------------
# Load 3rd-party metadata from CSV files
# ---------------------------------------------------------------------------
# 3rd-party metadata is stored separately from internal configs for easier updates
third_party_dir <- "b_data/osf_data_current/4_3rd_party_metadata"

if (!dir.exists(third_party_dir)) {
  stop("3rd-party metadata directory not found: ", third_party_dir)
}

# Load 3rd-party metadata CSVs
countries.tb <- readr::read_csv(file.path(third_party_dir, "countries.csv"), show_col_types = FALSE)
fao.crop.indicators.tb <- readr::read_csv(file.path(third_party_dir, "fao_crop_indicators.csv"), show_col_types = FALSE)
ports.tb <- readr::read_csv(file.path(third_party_dir, "ports.csv"), show_col_types = FALSE)

# ---------------------------------------------------------------------------
# Convenience references for internal configs
# ---------------------------------------------------------------------------
configs <- all_data[["0.configs"]]

# Internal metadata from configs workbook (project-specific)
months.tb                <- configs[["months"]]
scenarios.tb             <- configs[["scenarios"]]
variables.tb             <- configs[["variables"]]              # (cleaned just above)
fish.catch.indicators.tb <- configs[["fish.catch.indicators"]]
fish.catch.eez.tb        <- configs[["fish.catch.eez"]]

# all_data is a nested list: all_data[[filename]][[sheetname]]
# Each sheet is also available as a tibble in the global environment

# ---------------------------------------------------------------------------
# ABSTRACTED IMPORT FUNCTIONS FOR FLEXIBLE FILE FORMAT HANDLING
# ---------------------------------------------------------------------------

#' Import all CSV files from a directory
#'
#' @param dir_path Path to directory containing CSV files
#' @return Named list of tibbles, where names are derived from CSV filenames
#' @note Returns NULL if no CSV files found
ImportCSVsFromDirectory <- function(dir_path) {
  csv_files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)

  if (length(csv_files) == 0) {
    return(NULL)
  }

  csv_data <- lapply(csv_files, function(file) {
    readr::read_csv(file, show_col_types = FALSE)
  })

  # Create clean names from filenames (remove .csv and version suffixes)
  names(csv_data) <- basename(csv_files) %>%
    gsub("\\.csv$", "", .) %>%
    gsub("_v\\d{4}-\\d{2}-\\d{2}$", "", .)

  return(csv_data)
}

#' Import all sheets from an Excel file
#'
#' @param file_path Path to Excel file (.xlsx or .xls)
#' @return Named list of tibbles, where names are sheet names
#' @note Returns NULL if file not found
ImportSheetsFromExcel <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NULL)
  }

  sheet_names <- readxl::excel_sheets(file_path)

  sheets_list <- lapply(sheet_names, function(sheet) {
    readxl::read_excel(file_path, sheet = sheet)
  })

  names(sheets_list) <- sheet_names

  return(sheets_list)
}

#' Detect and import data from directory (CSV or Excel)
#'
#' @param dir_path Path to directory containing data files
#' @return List with: $data (imported data), $file_type ("csv", "excel", "mixed", or "none")
#' @details Detects file types and imports appropriately. If mixed file types found,
#'          returns warning in $file_type field.
DetectAndImportData <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    stop("Directory not found: ", dir_path)
  }

  # Detect file types present
  csv_files <- list.files(dir_path, pattern = "\\.csv$", full.names = FALSE, recursive = FALSE)
  xlsx_files <- list.files(dir_path, pattern = "\\.xlsx$", full.names = FALSE, recursive = FALSE)
  xls_files <- list.files(dir_path, pattern = "\\.xls$", full.names = FALSE, recursive = FALSE)

  has_csv <- length(csv_files) > 0
  has_excel <- (length(xlsx_files) > 0) || (length(xls_files) > 0)

  # Check for mixed file types
  if (has_csv && has_excel) {
    warning("Mixed file types detected in ", dir_path,
            " (CSVs: ", length(csv_files), ", Excel: ", length(xlsx_files) + length(xls_files), ")")
    return(list(data = NULL, file_type = "mixed"))
  }

  # Import based on file type
  if (has_csv) {
    data <- ImportCSVsFromDirectory(dir_path)
    return(list(data = data, file_type = "csv"))
  }

  if (has_excel) {
    # If multiple Excel files, import all sheets from each
    excel_files <- c(
      list.files(dir_path, pattern = "\\.xlsx$", full.names = TRUE, recursive = FALSE),
      list.files(dir_path, pattern = "\\.xls$", full.names = TRUE, recursive = FALSE)
    )

    # For single Excel file, import all sheets
    if (length(excel_files) == 1) {
      data <- ImportSheetsFromExcel(excel_files[1])
      return(list(data = data, file_type = "excel"))
    }

    # For multiple Excel files, import each file's sheets
    all_data <- list()
    for (file in excel_files) {
      file_data <- ImportSheetsFromExcel(file)
      file_key <- basename(file) %>%
        gsub("\\.(xlsx|xls)$", "", .) %>%
        gsub("_v\\d{4}-\\d{2}-\\d{2}$", "", .)

      all_data <- c(all_data, setNames(file_data, paste0(file_key, ".", names(file_data))))
    }
    return(list(data = all_data, file_type = "excel"))
  }

  # No data files found
  return(list(data = NULL, file_type = "none"))
}
