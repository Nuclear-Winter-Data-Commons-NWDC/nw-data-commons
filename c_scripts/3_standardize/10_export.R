# EXPORT CLEANED DATA -----------------------------------------------------------

  suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
    library(purrr)
    library(tidyr)
    library(openxlsx)
  })

# Expect: clean.tables.ls already created by 09_final_cleaning_and_consolidation.R
# Pull configs for variables (readme will be cloned directly from source Excel)
  configs <- all_data[["0.configs"]]
  variables_src <- configs[["variables"]]

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

  variables_out <-
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


  # 4) (Optional) Write back to configs workbook ----------------------------------
  write_back <- FALSE  # << set to TRUE to overwrite the 'variables' sheet

  if (write_back) {
    # choose a path; prefer the correctly spelled one if it exists
    configs_path <- "b_data/3_aggregated/0.configs.xlsx"
    if(!file.exists(configs_path)) {
      warning("Could not find 0.configs.xlsx at expected paths: ", configs_path)
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
# Output paths: timestamped run FOLDER; stable filenames inside

  timestamp_str   <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  standardized_root <- file.path("b_data", "4_standardized")
  run_dir         <- file.path(standardized_root, timestamp_str)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  xlsx_path <- file.path(run_dir, "0_standardized_data.xlsx")

  # Data sheets to export (in fixed order)
  data_sheets <- c(
    "temperature",
    "precipitation",
    "uv",
    "agriculture.agmip",
    "agriculture.clm",
    "fish.catch",
    "sea.ice"
  )

# -------------------------------------------------------------------------------
# Write Excel

  # Load source configs workbook to copy readme sheet with formatting
  configs_path <- "b_data/3_aggregated/0.configs.xlsx"
  source_configs_wb <- loadWorkbook(configs_path)

  # 1) readme - start with source workbook (preserves all formatting)
  # Then remove all sheets except readme
  wb <- source_configs_wb

  # Remove all sheets except readme
  all_sheets <- sheets(wb)
  sheets_to_remove <- all_sheets[all_sheets != "readme"]
  for (sheet_name in sheets_to_remove) {
    removeWorksheet(wb, sheet_name)
  }

  # Remove all named ranges to prevent Excel repair issues
  # Named ranges reference removed sheets and cause corruption warnings
  named_regions <- tryCatch(getNamedRegions(wb), error = function(e) NULL)
  if (!is.null(named_regions) && length(named_regions) > 0) {
    for (name in named_regions) {
      tryCatch(deleteNamedRegion(wb, name), error = function(e) NULL)
    }
  }

  # 2) variables (constructed)
  addWorksheet(wb, sanitize_sheet_name("variables"))
  writeData(wb, sheet = "variables", variables_out, keepNA = FALSE)

  # 3) data sheets
  for (sn in data_sheets) {
    if (!sn %in% names(clean.tables.ls)) next
    addWorksheet(wb, sanitize_sheet_name(sn))
    writeData(wb, sheet = sn, clean.tables.ls[[sn]], keepNA = FALSE)
  }

  saveWorkbook(wb, xlsx_path, overwrite = TRUE)

# -------------------------------------------------------------------------------
# Write CSVs (only the seven data sheets) into the same run folder

  csv_paths <- c()
  for (sn in data_sheets) {
    if (!sn %in% names(clean.tables.ls)) next
    csv_path <- file.path(run_dir, paste0(sn, ".csv"))
    write.csv(clean.tables.ls[[sn]], csv_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    csv_paths[sn] <- csv_path
  }

  if (interactive()) {
    message("Wrote ", length(csv_paths), " CSVs and workbook: ", xlsx_path, "\nOutput folder: ", run_dir)
  }

  invisible(list(dir = run_dir, xlsx = xlsx_path, csvs = csv_paths))
