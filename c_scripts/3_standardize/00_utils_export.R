# EXPORT CLEANED DATA -----------------------------------------------------------

  suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
    library(purrr)
    library(tidyr)
  })

# Expect: clean.tables.ls already created by 97_final_cleaning_and_consolidation.R
# Expect: variables.tb loaded by 00_utils_import.R from configs workbook
  if (!exists("variables.tb")) {
    stop("variables.tb not found - ensure 00_utils_import.R was sourced first")
  }

  variables_src <- variables.tb

# -------------------------------------------------------------------------------
# Helpers

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

# -------------------------------------------------------------------------------
# Build VARIABLES sheet aligned to actual exports

  # Build an ordered grid for ONE dataset: one row per column in the sheet, in sheet order
  build_sheet_grid <- function(df, ds) {
    tibble::tibble(
      dataset         = ds,
      variable.name   = names(df),
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

  # 3) Merge metadata from variables_src onto the ordered grid
  variables_updated <-
    ordered_grid %>%
    dplyr::rename(range.computed = range) %>%
    dplyr::left_join(
      variables_src,
      by = c("dataset","variable.name")
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
      definition
    )

  # Preserve variables for datasets NOT being updated
  datasets_being_updated <- unique(ordered_grid$dataset)
  variables_preserved <- variables_src %>%
    dplyr::filter(!dataset %in% datasets_being_updated)

  # Combine: preserved (other datasets) + updated (current datasets)
  variables_out <- dplyr::bind_rows(variables_preserved, variables_updated) %>%
    dplyr::arrange(dataset, variable.name)

# -------------------------------------------------------------------------------
# Output paths: OSF-mirrored structure in osf_data_current/3_standardized/

  version_date <- format(Sys.Date(), "%Y-%m-%d")  # YYYY-MM-DD for file suffixes

  standardized_root <- file.path("b_data", "osf_data_current", "3_standardized")
  dir.create(standardized_root, recursive = TRUE, showWarnings = FALSE)

  backup_root <- file.path("b_data", "osf_data_most_recent_previous", "3_standardized")
  dir.create(backup_root, recursive = TRUE, showWarnings = FALSE)

  # Data sheets to export
  data_sheets <- names(clean.tables.ls)

# -------------------------------------------------------------------------------
# BACKUP & REPLACE WORKFLOW

  # Helper: Backup and remove existing file matching pattern
  # Removes old backups of the same pattern before creating new backup
  backup_and_remove <- function(pattern, current_dir, backup_dir) {
    existing_files <- list.files(current_dir, pattern = pattern, full.names = TRUE)
    if (length(existing_files) > 0) {
      # Remove all old backups matching this pattern (keep only most recent)
      old_backups <- list.files(backup_dir, pattern = pattern, full.names = TRUE)
      if (length(old_backups) > 0) {
        file.remove(old_backups)
      }
      # Backup current version and remove from current
      for (file in existing_files) {
        backup_path <- file.path(backup_dir, basename(file))
        file.copy(file, backup_path, overwrite = TRUE)
        file.remove(file)
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
    if (interactive()) {
      message("Wrote: ", basename(csv_path))
    }
  }

  # Backup and remove existing variables files
  backup_and_remove("^variables_v.*\\.csv$", standardized_root, backup_root)

  # Export variables table with version-date suffix
  variables_csv_path <- file.path(standardized_root, paste0("variables_v", version_date, ".csv"))
  write.csv(variables_out, variables_csv_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  csv_paths["variables"] <- variables_csv_path

  # Backup and remove existing readme files
  backup_and_remove("^0_readme_v.*\\.md$", standardized_root, backup_root)

  # Export readme as markdown with version-date suffix
  readme_template_path <- "d_context/readme_template.md"
  if (file.exists(readme_template_path)) {
    readme_md_path <- file.path(standardized_root, paste0("0_readme_v", version_date, ".md"))
    file.copy(readme_template_path, readme_md_path, overwrite = TRUE)
    csv_paths["readme"] <- readme_md_path
  } else {
    warning("Readme template not found at: ", readme_template_path)
  }

  if (interactive()) {
    message("Exported ", length(csv_paths), " files to: ", standardized_root)
  }

  invisible(list(dir = standardized_root, csvs = csv_paths))
