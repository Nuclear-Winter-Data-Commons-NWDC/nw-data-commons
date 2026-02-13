#!/usr/bin/env Rscript
# TEST SCRIPT: fish_catch cleaning pipeline
# Tests the abstracted import functions and produces standardized output

# 1. Load utility functions
source("c_scripts/3_standardize/00_utils_core.R")
source("c_scripts/3_standardize/00_utils_validate.R")

# 2. Import aggregated data and configs
source("c_scripts/3_standardize/00_utils_import.R")

# 3. Clean fish_catch dataset
source("c_scripts/3_standardize/fish_catch_cleaning.R")

# 4. Create clean.tables.ls for export
source.table.configs.tb <- configs[["standardization"]]

# Find fish_catch config
fish_catch_config <- source.table.configs.tb[source.table.configs.tb$object.name == "fish.catch", ]

if (nrow(fish_catch_config) == 0) {
  stop("fish.catch not found in standardization configs")
}

# Create clean.tables.ls with just fish_catch
clean.tables.ls <- list()
clean.tables.ls[["fish.catch"]] <- fish.catch.clean.tb

cat("Created clean.tables.ls with", length(clean.tables.ls), "table(s)\n")
cat("Table name:", names(clean.tables.ls), "\n")
cat("Rows:", nrow(clean.tables.ls[["fish.catch"]]), "\n")
cat("Columns:", ncol(clean.tables.ls[["fish.catch"]]), "\n\n")

# Display sample
cat("Sample of cleaned data:\n")
print(head(clean.tables.ls[["fish.catch"]], 10))

# 5. Apply final cleaning (filter indicators of concern)
cat("\n\nApplying final cleaning...\n")

filter_by_indicators_of_concern <- function(tb, table.name.raw) {
  if (nrow(tb) == 0) return(tb)

  num.rows.initial <- nrow(tb)

  indicators.str <- source.table.configs.tb$indicators.of.concern[
    source.table.configs.tb$object.name == table.name.raw
  ]

  if (is.na(indicators.str)) {
    cat("No indicators of concern specified for", table.name.raw, "\n")
    return(tb)
  }

  indicators <- indicators.str %>%
    strsplit(",\\s*") %>%
    unlist()

  indicators <- indicators[indicators %in% names(tb)]

  if (length(indicators) == 0) return(tb)

  tb <- tb %>%
    filter(if_any(all_of(indicators), ~ !is.na(.)))

  num.rows.final <- nrow(tb)
  percent.removed <- if (num.rows.initial > 0) {
    round(100 * (num.rows.initial - num.rows.final) / num.rows.initial, 1)
  } else { 0 }

  cat(
    "Filtering for rows in '", table.name.raw, "' without data for indicator(s) of concern.\n",
    "Initial number of rows: ", num.rows.initial, "\n",
    "Final number of rows: ", num.rows.final, "\n",
    "Removed ", num.rows.initial - num.rows.final, " rows (", percent.removed, "%)\n\n",
    sep = ""
  )

  return(tb)
}

clean.tables.ls[["fish.catch"]] <- filter_by_indicators_of_concern(
  clean.tables.ls[["fish.catch"]],
  "fish.catch"
)

# 6. Export standardized data
export_result <- source("c_scripts/3_standardize/00_utils_export.R")

cat("\n\nExport complete!\n")
cat("Output directory:", export_result$value$dir, "\n")
cat("Excel file:", export_result$value$xlsx, "\n")
