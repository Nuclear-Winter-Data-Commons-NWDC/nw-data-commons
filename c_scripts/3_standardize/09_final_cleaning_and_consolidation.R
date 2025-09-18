# FINAL CLEANING & CONSOLIDATION ----------------------------------------------------------

library(dplyr)
library(stringr)
library(purrr)
library(magrittr)

# configs
configs <- all_data[["0.configs"]]
source.table.configs.tb <- configs[["source.table.configs"]]

# 2.7 CONSOLIDATE TABLES INTO LIST

clean_object_names <-
  source.table.configs.tb$object.name %>%
  sapply(., function(x) paste0(x, ".clean.tb")) %>%
  as.vector()

clean_table_names <- source.table.configs.tb$object.name %>% as.vector()

# collect existing clean tibbles by name
clean.tables.ls <-
  mget(intersect(clean_object_names, ls()), inherits = TRUE, ifnotfound = list(NULL)) %>%
  purrr::compact()

names(clean.tables.ls) <- clean_table_names[clean_object_names %in% names(clean.tables.ls)]

# 2.8 FINAL CLEANING

# Helper: Drop rows where all indicators of concern are NA
filter_by_indicators_of_concern <- function(tb, table.name.raw) {
  if (nrow(tb) == 0) return(tb)

  num.rows.initial <- nrow(tb)

  indicators.str <- source.table.configs.tb$indicators.of.concern[
    source.table.configs.tb$object.name == table.name.raw
  ]

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

  if (interactive()) {
    cat(
      "Filtering for rows in '", table.name.raw, "' without data for indicator(s) of concern.\n",
      "Initial number of rows: ", num.rows.initial, "\n",
      "Final number of rows: ", num.rows.final, "\n",
      "Removed ", num.rows.initial - num.rows.final, " rows (", percent.removed, "%)\n\n",
      sep = ""
    )
  }

  return(tb)
}

# Apply filter to each clean table
clean.tables.ls <-
  mapply(
    filter_by_indicators_of_concern,
    clean.tables.ls,
    names(clean.tables.ls),
    SIMPLIFY = FALSE
  )

# Optional: quick check (commented)
# print(names(clean.tables.ls))
# lapply(clean.tables.ls, head, 2)
