# EXPORT CLEANED DATA -----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(openxlsx)
})

# Expect: clean.tables.ls already created by 09_final_cleaning_and_consolidation.R
# Pull configs for readme & variables
configs <- all_data[["0.configs"]]
readme_tb     <- configs[["readme"]]
variables_src <- configs[["variables"]]

# -------------------------------------------------------------------------------
# Helpers

# Convert 1 -> "A", 26 -> "Z", 27 -> "AA", ...
excel_col_letter <- function(n) {
  if (is.na(n) || n < 1) return(NA_character_)
  letters <- character()
  while (n > 0) {
    r <- (n - 1) %% 26
    letters <- c(LETTERS[r + 1], letters)
    n <- (n - 1) %/% 26
  }
  paste0(letters, collapse = "")
}

# Sanitize Excel sheet names (safety; current names are fine)
sanitize_sheet_name <- function(x) {
  x <- gsub("[\\[\\]\\:\\*\\?/\\\\]", "", x)  # invalid chars
  if (nchar(x) > 31) substr(x, 1, 31) else x
}

# Normalizer for matching column headers by canonical token
.norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# Ensure variables table has required columns ("sheet","name","column","range/unique values")
ensure_variables_columns <- function(df) {
  need <- c("sheet","name","column","range/unique values")
  have <- names(df)
  need_missing <- need[!.norm(need) %in% .norm(have)]
  if (length(need_missing)) {
    for (nm in need_missing) df[[nm]] <- NA_character_
  }
  df[, c(have, need_missing), drop = FALSE]
}

# Compute the "range/unique values" string per your rules
compute_range_string <- function(x) {
  if (inherits(x, "Date")) {
    if (all(is.na(x))) return("")
    rng <- range(x, na.rm = TRUE)
    return(paste(format(rng[1], "%Y-%m-%d"), format(rng[2], "%Y-%m-%d"), sep = ", "))
  }
  if (is.numeric(x)) {
    if (all(is.na(x))) return("")
    rng <- range(x, na.rm = TRUE)
    return(paste0(signif(rng[1], 6), ", ", signif(rng[2], 6)))
  }
  # Treat logical/factor/character as text
  ux <- unique(x[!is.na(x)])
  ux_chr <- if (is.logical(ux)) ifelse(ux, "TRUE", "FALSE") else as.character(ux)
  n <- length(ux_chr)
  if (n == 0) return("")
  if (n <= 10) paste(sort(ux_chr), collapse = ", ") else paste0("[", n, "] unique values")
}

# Build variables block for one sheet, preserving variables_src column order
build_variables_block <- function(sheet_name, df_sheet, variables_src) {
  vs <- ensure_variables_columns(variables_src)
  cn_sheet <- names(df_sheet)

  cols_norm <- .norm(names(vs))
  sheet_col_name <- names(vs)[which(cols_norm == "sheet")[1]]
  name_col_name  <- names(vs)[which(cols_norm == "name")[1]]
  col_col_name   <- names(vs)[which(cols_norm == "column")[1]]
  range_col_name <- names(vs)[which(cols_norm == "rangeuniquevalues")[1]]

  if (any(is.na(c(sheet_col_name, name_col_name, col_col_name, range_col_name)))) {
    stop("The variables config table must contain columns 'sheet', 'name', 'column', and 'range/unique values'.")
  }

  # Source rows for this sheet (may be empty)
  src_rows <- if (nrow(vs)) dplyr::filter(vs, .data[[sheet_col_name]] == sheet_name) else vs

  # Template for creating new rows (preserve column set & order)
  if (nrow(vs) > 0) {
    blank <- vs[1, , drop = FALSE]; blank[1, ] <- NA
  } else {
    blank <- as.data.frame(setNames(replicate(length(names(vs)), NA_character_, simplify = FALSE), names(vs)), stringsAsFactors = FALSE)
  }

  # Build in the **actual column order** of the sheet
  out_rows <- lapply(seq_along(cn_sheet), function(j) {
    colname <- cn_sheet[j]
    hit <- if (nrow(src_rows)) which(src_rows[[name_col_name]] == colname) else integer(0)
    row <- if (length(hit) >= 1) src_rows[hit[1], , drop = FALSE] else {
      r <- blank
      r[[sheet_col_name]] <- sheet_name
      r[[name_col_name]]  <- colname
      r
    }
    row[[col_col_name]]   <- excel_col_letter(j)
    row[[range_col_name]] <- compute_range_string(df_sheet[[colname]])
    row
  })

  dplyr::bind_rows(out_rows)
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
# Build VARIABLES sheet aligned to actual exports

variables_blocks <- list()
for (sn in data_sheets) {
  if (!sn %in% names(clean.tables.ls)) next
  df_sheet <- clean.tables.ls[[sn]]
  variables_blocks[[sn]] <- build_variables_block(sn, df_sheet, variables_src)
}

variables_out <- if (length(variables_blocks)) {
  dplyr::bind_rows(variables_blocks)
} else {
  ensure_variables_columns(variables_src[0, , drop = FALSE])
}

# -------------------------------------------------------------------------------
# Write Excel

wb <- createWorkbook()

# 1) readme (as-is)
addWorksheet(wb, sanitize_sheet_name("readme"))
writeData(wb, sheet = "readme", readme_tb)

# 2) variables (constructed)
addWorksheet(wb, sanitize_sheet_name("variables"))
writeData(wb, sheet = "variables", variables_out)

# 3) data sheets
for (sn in data_sheets) {
  if (!sn %in% names(clean.tables.ls)) next
  addWorksheet(wb, sanitize_sheet_name(sn))
  writeData(wb, sheet = sn, clean.tables.ls[[sn]])
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
