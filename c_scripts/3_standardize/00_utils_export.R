# EXPORT CLEANED DATA -----------------------------------------------------------

  suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
    library(purrr)
    library(tidyr)
    library(openxlsx)
    library(readODS)
  })

# Expect: clean.tables.ls already created by 97_final_cleaning_and_consolidation.R
# Expect: variables.tb loaded by 00_utils_import.R from configs workbook
  if (!exists("variables.tb")) {
    stop("variables.tb not found - ensure 00_utils_import.R was sourced first")
  }

  variables_src <- variables.tb

# -------------------------------------------------------------------------------
# Helpers

  # Sanitize Excel sheet names (safety; current names are fine)
  sanitize_sheet_name <- function(x) {
    x <- gsub("[\\[\\]\\:\\*\\?/\\\\]", "", x)  # invalid chars
    if (nchar(x) > 31) substr(x, 1, 31) else x
  }

  # Normalizer for matching column headers by canonical token
  .norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))

  # Compute ranges for variables table
  compute_range_string <- function(x) {
    if (inherits(x, "Date")) {
      if (all(is.na(x))) return("")
      rng <- range(x, na.rm = TRUE); return(paste(format(rng[1], "%Y-%m-%d"), format(rng[2], "%Y-%m-%d"), sep = ", "))
    }
    if (is.numeric(x)) {
      if (all(is.na(x))) return("")
      rng <- range(x, na.rm = TRUE); return(paste0(signif(rng[1], 6), ", ", signif(rng[2], 6)))
    }
    ux <- unique(na.omit(as.character(x))); if (!length(ux)) return("")
    if (length(ux) <= 10) paste(sort(ux), collapse = ", ") else paste0("[", length(ux), "] unique values")
  }

  # define excel column letter from column order number
  excel_col_letter <- function(n) {
    if (is.na(n) || n < 1) return(NA_character_)
    out <- character()
    while (n > 0) { r <- (n - 1) %% 26; out <- c(LETTERS[r + 1], out); n <- (n - 1) %/% 26 }
    paste0(out, collapse = "")
  }

# -------------------------------------------------------------------------------
# Build VARIABLES sheet aligned to actual exports

# --- Build VARIABLES sheet aligned to actual exports --------------------------

  # Build an ordered grid for ONE dataset: one row per column in the sheet, in sheet order
  build_sheet_grid <- function(df, ds) {
    tibble::tibble(
      dataset         = ds,
      variable.name = names(df),
      excel.column    = openxlsx::int2col(seq_along(df)),  # vectorised -> no length error
      range           = purrr::map_chr(names(df), ~ compute_range_string(df[[.x]]))
    )
}

  # 1) Build the full, ordered grid from the actual exported tables
  ordered_grid <- purrr::imap_dfr(clean.tables.ls, build_sheet_grid)

  # 2) Identify NEW columns (in exports but not in variables_src) and warn nicely
  key_src  <- dplyr::transmute(variables_src, dataset, variable.name)
  key_out  <- dplyr::transmute(ordered_grid,  dataset, variable.name)
  new_rows <- dplyr::anti_join(key_out, key_src, by = c("dataset","variable.name"))

  if (nrow(new_rows)) {
    msg <- new_rows %>%
      dplyr::arrange(dataset, variable.name) %>%
      dplyr::group_by(dataset) %>%
      dplyr::summarise(new_vars = paste0(" - ", variable.name, collapse = "\n"), .groups = "drop") %>%
      dplyr::mutate(line = paste0("* ", dataset, ":\n", new_vars)) %>%
      dplyr::pull(line) %>%
      paste(collapse = "\n")
    warning("Variables not found in variables_src (will be appended in variables_out):\n", msg)
  }

  # 3) Merge metadata from variables_src onto the ordered grid (variables_src stays the base for metadata)
  vars_cols <- names(variables_src)

  # Build updated variables for current datasets
  variables_updated <-
    ordered_grid %>%
    # keep computed columns separate so we can coalesce cleanly
    dplyr::rename(excel.column.computed = excel.column,
                  range.computed        = range) %>%
    dplyr::left_join(
      variables_src,
      by = c("dataset","variable.name"),
      suffix = c(".computed", ".src")
    ) %>%
    dplyr::mutate(
      range.final = dplyr::case_when(
        stringr::str_ends(variable.name, "outlier.flag") ~ dplyr::coalesce(range.or.unique.values, "[blank], outlier"),
        TRUE ~ dplyr::coalesce(range.or.unique.values, range.computed)
      )
    ) %>%
    dplyr::transmute(
      dataset,
      variable.name,
      source,
      format,
      range.or.unique.values = range.final,
      unit,
      definition,
      excel.column = dplyr::coalesce(excel.column.computed, excel.column)  # prefer computed if present
    )

  # Preserve variables for datasets NOT being updated
  datasets_being_updated <- unique(ordered_grid$dataset)
  variables_preserved <- variables_src %>%
    dplyr::filter(!dataset %in% datasets_being_updated)

  # Combine: preserved (other datasets) + updated (current datasets)
  variables_out <- dplyr::bind_rows(variables_preserved, variables_updated) %>%
    dplyr::arrange(dataset, variable.name)


  # 4) (Optional) Write back to configs workbook ----------------------------------
  write_back <- FALSE  # << set to TRUE to overwrite the 'variables' sheet

  if (write_back) {
    # Updated path to OSF configs directory
    configs_path <- "b_data/osf_data_current/0_configs/configs_v2026-01-21.xlsx"
    if(!file.exists(configs_path)) {
      warning("Could not find configs workbook at: ", configs_path)
    } else {
      wb <- loadWorkbook(configs_path)
      if (!"variables" %in% sheets(wb)) addWorksheet(wb, "variables")
      # clear existing 'variables' sheet contents
      removeWorksheet(wb, "variables"); addWorksheet(wb, "variables")
      writeData(wb, sheet = "variables", x = variables_out)
      saveWorkbook(wb, configs_path, overwrite = TRUE)
      message("Updated 'variables' sheet written to: ", configs_path)
    }
  }

# -------------------------------------------------------------------------------
# Output paths: OSF-mirrored structure in osf_data_current/3_standardized/

  version_date <- format(Sys.Date(), "%Y-%m-%d")  # YYYY-MM-DD for file suffixes

  # Use OSF mirror structure for standardized outputs
  standardized_root <- file.path("b_data", "osf_data_current", "3_standardized")
  dir.create(standardized_root, recursive = TRUE, showWarnings = FALSE)

  # Consolidated Excel/ODS workbooks in root of 3_standardized/
  # Will be set after determining which existing workbook to load
  xlsx_filename <- NULL
  xlsx_path <- NULL

  # Data sheets to export - derive from what was actually created
  # (supports dynamic addition/removal of datasets)
  data_sheets <- names(clean.tables.ls)

# -------------------------------------------------------------------------------
# Write Excel - ADD/UPDATE approach (preserve existing sheets)

  # Try to load existing standardized workbook, fallback to configs if not found
  # Look for any numbered standardized data workbook (0_, 1_, 2_, etc.)
  # Prioritize files starting with higher numbers (1_ > 0_) as they are likely more complete
  existing_xlsx <- list.files(standardized_root, pattern = "^[0-9]_standardized_data_v.*\\.xlsx$", full.names = TRUE)

  source_workbook_path <- NULL  # Track which workbook we loaded from

  if (length(existing_xlsx) > 0) {
    # If multiple exist, prefer files with higher number prefixes (1_ over 0_)
    # Extract number prefix and sort descending
    if (length(existing_xlsx) > 1) {
      prefixes <- as.integer(sub("^.*/([0-9])_.*", "\\1", existing_xlsx))
      existing_xlsx <- existing_xlsx[order(prefixes, decreasing = TRUE)]
    }
    # Load existing standardized workbook to preserve other sheets
    source_workbook_path <- existing_xlsx[1]
    wb <- loadWorkbook(source_workbook_path)

    # Extract prefix from source workbook (e.g., "1" from "1_standardized_data_v2026-01-26.xlsx")
    source_prefix <- sub("^.*/([0-9])_.*", "\\1", source_workbook_path)
    xlsx_filename <- paste0(source_prefix, "_standardized_data_v", version_date, ".xlsx")
    xlsx_path <- file.path(standardized_root, xlsx_filename)

    if (interactive()) {
      message("Loaded existing standardized workbook: ", basename(source_workbook_path))
    }
  } else {
    # No existing workbook - start from configs to copy readme sheet
    configs_path <- "b_data/osf_data_current/0_configs/configs_v2026-01-21.xlsx"
    if (!file.exists(configs_path)) {
      stop("Configs workbook not found at: ", configs_path)
    }
    wb <- loadWorkbook(configs_path)

    # Remove all sheets except readme
    all_sheets <- sheets(wb)
    sheets_to_remove <- all_sheets[all_sheets != "readme"]
    for (sheet_name in sheets_to_remove) {
      removeWorksheet(wb, sheet_name)
    }

    # Remove all named ranges to prevent Excel repair issues
    named_regions <- tryCatch(getNamedRegions(wb), error = function(e) NULL)
    if (!is.null(named_regions) && length(named_regions) > 0) {
      for (name in named_regions) {
        tryCatch(deleteNamedRegion(wb, name), error = function(e) NULL)
      }
    }

    # Use prefix "0" for new workbooks created from configs
    xlsx_filename <- paste0("0_standardized_data_v", version_date, ".xlsx")
    xlsx_path <- file.path(standardized_root, xlsx_filename)

    if (interactive()) {
      message("Created new standardized workbook from configs")
    }
  }

  # Update or add variables sheet
  # Remove existing variables data for datasets being updated, keep others
  if ("variables" %in% sheets(wb)) {
    removeWorksheet(wb, "variables")
  }
  addWorksheet(wb, "variables")
  writeData(wb, sheet = "variables", variables_out, keepNA = FALSE)

  # Update or add data sheets (using underscores for sheet names)
  for (sn in data_sheets) {
    if (!sn %in% names(clean.tables.ls)) next

    # Convert dots to underscores for sheet names
    sheet_name <- gsub("\\.", "_", sn)
    sheet_name <- sanitize_sheet_name(sheet_name)

    # Remove existing sheet if it exists (updating)
    if (sheet_name %in% sheets(wb)) {
      removeWorksheet(wb, sheet_name)
      if (interactive()) {
        message("Updating existing sheet: ", sheet_name)
      }
    } else {
      if (interactive()) {
        message("Adding new sheet: ", sheet_name)
      }
    }

    # Add the sheet with data
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, clean.tables.ls[[sn]], keepNA = FALSE)
  }

  # Backup and remove ALL existing Excel workbooks before saving new one
  backup_root <- file.path("b_data", "osf_data_most_recent_previous", "3_standardized")
  dir.create(backup_root, recursive = TRUE, showWarnings = FALSE)

  # Backup ALL standardized workbooks (with any number prefix)
  xlsx_pattern <- "^[0-9]_standardized_data_v.*\\.xlsx$"
  all_existing_xlsx <- list.files(standardized_root, pattern = xlsx_pattern, full.names = TRUE)

  if (length(all_existing_xlsx) > 0) {
    # Remove all old Excel backups first (keep only most recent)
    old_backups <- list.files(backup_root, pattern = xlsx_pattern, full.names = TRUE)
    if (length(old_backups) > 0) {
      file.remove(old_backups)
    }

    # Backup and remove all existing workbooks
    for (file in all_existing_xlsx) {
      backup_path <- file.path(backup_root, basename(file))
      file.copy(file, backup_path, overwrite = TRUE)
      file.remove(file)
      if (interactive()) {
        cat("Backed up and removed:", basename(file), "\n")
      }
    }
  }

  saveWorkbook(wb, xlsx_path, overwrite = TRUE)

# -------------------------------------------------------------------------------
# BACKUP & REPLACE WORKFLOW: Backup existing files, then write new versions
# Only backs up files that are being replaced (dataset-specific)
# Keeps only the most recent backup (removes older backups of same dataset)
# -------------------------------------------------------------------------------

  backup_root <- file.path("b_data", "osf_data_most_recent_previous", "3_standardized")
  dir.create(backup_root, recursive = TRUE, showWarnings = FALSE)

  # Helper: Backup and remove existing file matching pattern
  # Removes old backups of the same pattern before creating new backup
  backup_and_remove <- function(pattern, current_dir, backup_dir) {
    existing_files <- list.files(current_dir, pattern = pattern, full.names = TRUE)
    if (length(existing_files) > 0) {
      # First, remove all old backups matching this pattern (keep only most recent)
      old_backups <- list.files(backup_dir, pattern = pattern, full.names = TRUE)
      if (length(old_backups) > 0) {
        file.remove(old_backups)
      }

      # Then backup current version and remove from current
      for (file in existing_files) {
        backup_path <- file.path(backup_dir, basename(file))
        file.copy(file, backup_path, overwrite = TRUE)
        file.remove(file)  # Remove from current
        if (interactive()) {
          cat("Backed up and removed:", basename(file), "\n")
        }
      }
    }
  }

# -------------------------------------------------------------------------------
# Write CSVs: all files in root directory (no subdirectories)

  csv_paths <- c()

  # Export data sheets with version-date suffixes (convert dots to underscores)
  for (sn in data_sheets) {
    if (!sn %in% names(clean.tables.ls)) next

    # Convert dots to underscores for filenames
    filename_safe <- gsub("\\.", "_", sn)

    # Backup and remove any existing versions of this dataset
    pattern <- paste0("^", filename_safe, "_v.*\\.csv$")
    backup_and_remove(pattern, standardized_root, backup_root)

    # Write new CSV to root standardized directory
    csv_path <- file.path(standardized_root, paste0(filename_safe, "_v", version_date, ".csv"))
    write.csv(clean.tables.ls[[sn]], csv_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    csv_paths[sn] <- csv_path
  }

  # Backup and remove existing variables files
  backup_and_remove("^variables_v.*\\.csv$", standardized_root, backup_root)

  # Export variables table with version-date suffix (in root)
  variables_csv_path <- file.path(standardized_root, paste0("variables_v", version_date, ".csv"))
  write.csv(variables_out, variables_csv_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  csv_paths["variables"] <- variables_csv_path

  # Backup and remove existing readme files
  backup_and_remove("^0_readme_v.*\\.md$", standardized_root, backup_root)

  # Export readme as markdown with version-date suffix (in root)
  readme_template_path <- "d_context/readme_template.md"
  if (file.exists(readme_template_path)) {
    readme_md_path <- file.path(standardized_root, paste0("0_readme_v", version_date, ".md"))
    file.copy(readme_template_path, readme_md_path, overwrite = TRUE)
    csv_paths["readme"] <- readme_md_path
  } else {
    warning("Readme template not found at: ", readme_template_path)
  }

  if (interactive()) {
    message("Wrote ", length(csv_paths), " CSVs and workbook: ", xlsx_path, "\nOutput folder: ", standardized_root)
  }

# -------------------------------------------------------------------------------
# Write ODS (Open Document Spreadsheet) format
# Note: R's readODS is extremely slow for large files. Use ssconvert instead.

  # Backup and remove existing ODS files before conversion (remove old backups first)
  ods_pattern <- "^[0-9]_standardized_data_v.*\\.ods$"
  existing_ods <- list.files(standardized_root, pattern = ods_pattern, full.names = TRUE)
  if (length(existing_ods) > 0) {
    # Remove all old ODS backups first
    old_ods_backups <- list.files(backup_root, pattern = ods_pattern, full.names = TRUE)
    if (length(old_ods_backups) > 0) {
      file.remove(old_ods_backups)
    }

    # Backup current version
    for (file in existing_ods) {
      backup_path <- file.path(backup_root, basename(file))
      file.copy(file, backup_path, overwrite = TRUE)
      file.remove(file)
      if (interactive()) {
        cat("Backed up and removed:", basename(file), "\n")
      }
    }
  }

  # Use same prefix as Excel filename (extract from xlsx_filename)
  ods_filename <- sub("\\.xlsx$", ".ods", xlsx_filename)
  ods_path <- file.path(standardized_root, ods_filename)

  # Check if ssconvert (gnumeric) is available - much faster than R
  ssconvert_available <- system("command -v ssconvert", ignore.stdout = TRUE) == 0

  if (ssconvert_available) {
    if (interactive()) {
      message("Converting to ODS using ssconvert (fast)...")
    }
    conversion_script <- file.path(getwd(), "c_scripts/convert_to_ods.sh")

    # NOTE: ODS conversion via ssconvert may not work reliably for large files.
    # If conversion fails or takes >60 seconds, consider manual conversion:
    # 1. Open 0_standardized_data.xlsx in LibreOffice Calc
    # 2. Save As -> ODS format
    # 3. Upload resulting ODS file to OSF /3_standardized/ directory
    cmd <- paste0(conversion_script, " '", xlsx_path, "' '", ods_path, "'")
    result <- system(cmd)
    if (result == 0 && interactive()) {
      ods_size <- file.info(ods_path)$size / (1024 * 1024)
      message("Wrote ODS file: ", ods_path, " (", round(ods_size, 1), " MB)")
    }
  } else {
    if (interactive()) {
      message("Skipping ODS generation (ssconvert not found)")
      message("To enable fast ODS conversion: sudo apt install gnumeric")
      message("Or convert manually: ./convert_to_ods.sh ", xlsx_path)
    }
    ods_path <- NULL
  }

  invisible(list(dir = standardized_root, xlsx = xlsx_path, ods = ods_path, csvs = csv_paths))
