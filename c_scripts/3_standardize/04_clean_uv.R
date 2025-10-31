#!/usr/bin/env Rscript
# 04_clean_uv.R
# Clean and reshape UV data imported from aggregated sources
# Assumes 00_utils.R has been sourced or is available

# Libraries
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(reshape2)
library(magrittr)

# Utilities (ReplaceNames, FlagOutliers_IQR, ImportSourceData_GoogleSheets, etc.)
source("c_scripts/3_standardize/00_utils.R")

# Configs & reference tables (ensure these are present in all_data from import script)
configs <- all_data[["0.configs"]]
months.tb <- configs[["months"]]
countries.tb <- configs[["countries"]]

# UV aggregated input list (from 01_import_aggregated_data.R)
uv.ls <- all_data[["3.uv"]]
uv.sheet.names <- names(uv.ls)

# Helper to parse scenario strings robustly
parse_scenario <- function(scenario_str) {
  s <- tolower(trimws(scenario_str %||% ""))
  if (s %in% c("control", "ctrl", "0")) return(0L)
  # remove trailing Tg or other non-digits (e.g., "150Tg" -> "150")
  s_num <- str_remove(s, "tg$") %>% str_remove_all("[^0-9\\-]") 
  as.integer(s_num)
}

# Main cleaning function for a single UV sheet
CleanReshape_UV <- function(source_table, source_table_name) {
  # source_table_name expected like "<scenario>_<indicator>" (e.g., "150Tg_ozone")
  parts <- strsplit(as.character(source_table_name), "_")[[1]]
  scenario_raw <- parts[1]
  indicator <- if (length(parts) >= 2) parts[2] else NA_character_

  soot_val <- parse_scenario(scenario_raw)

  result <- source_table %>%
    ReplaceNames(., names(.), tolower(names(.))) %>%
    # defensive: drop country name if present, keep country.id
    select(-any_of(c("country.name", "country_name", "nation"))) %>%
    # normalize possible country identifier column names to country.id if present
    { 
      cols <- names(.)
      if ("id" %in% cols && !("country.id" %in% cols)) {
        Rename <- .; names(Rename)[names(Rename) == "id"] <- "country.id"; Rename
      } else if ("country_iso3" %in% cols && !("country.id" %in% cols)) {
        # don't rename iso3 -> id automatically; prefer country.iso3 for join if needed
        .
      } else {
        .
      }
    } %>%
    # coerce list-columns into atomic vectors where possible
    mutate(across(where(is.list), ~ suppressWarnings(unlist(.)))) %>%
    # melt wide-timeseries columns into long form
    reshape2::melt(id = "country.id") %>%
    mutate(
      soot.injection.scenario = soot_val,
      variable = as.character(variable),
      year.raw = str_extract(variable, "^[^ ]+") %>% as.numeric(),
      month = str_extract(variable, "(?<= - )\\d+") %>% as.integer(),
      indicator = indicator
    ) %>%
    group_by(soot.injection.scenario) %>%
    mutate(
      years.elapsed = year.raw - min(year.raw, na.rm = TRUE),
      months.elapsed = years.elapsed * 12 + month
    ) %>%
    ungroup() %>%
    mutate(
      value = suppressWarnings(as.numeric(value))
    ) %>%
    # join config metadata if country.id exists; otherwise attempt country.iso3
    { 
      if ("country.id" %in% names(.)) {
        left_join(., countries.tb, by = "country.id")
      } else if ("country.iso3" %in% names(.) && "country.iso3" %in% names(countries.tb)) {
        left_join(., countries.tb, by = "country.iso3")
      } else {
        .
      }
    } %>%
    left_join(months.tb, by = "month") %>%
    select(
      country.name, country.iso3, country.hemisphere,	
      country.region, country.sub.region, country.intermediate.region, 
      country.nuclear.weapons, country.nato.member.2024, 
      country.population.2018, country.land.area.sq.km,
      soot.injection.scenario, years.elapsed, months.elapsed, month,
      season.n.hemisphere, season.s.hemisphere,
      indicator, value
    ) %>%
    as_tibble()
  
  message("Processed: ", source_table_name)
  return(result)
}

# Apply to all sheets
uv.clean.tb <- 
  Map(
    CleanReshape_UV,
    uv.ls,
    uv.sheet.names
  ) %>%
  bind_rows(.id = NULL) %>%
  # pivot so each indicator becomes a column (one value per country+scenario+time)
  pivot_wider(
    names_from = indicator,
    values_from = value,
    values_fn = list  # collect duplicates into lists so we can see if duplicates exist
  ) %>%
  # if any list-columns exist (due to duplicates), try to coerce to a single numeric (first)
  mutate(across(where(is.list), ~ {
    v <- .
    # if all elements are length 1, unlist; otherwise take first non-NA numeric
    if (all(sapply(v, length) <= 1)) {
      suppressWarnings(as.numeric(unlist(v)))
    } else {
      sapply(v, function(x) {
        if (length(x) == 0) return(NA_real_)
        suppressWarnings(as.numeric(x[[1]]))
      })
    }
  })) %>%
  # normalize names to lowercase
  ReplaceNames(., names(.), tolower(names(.))) %>%
  # flag outliers using utility function
  FlagOutliers_IQR() %>%
  as_tibble()

# Optional: preview (commented)
# uv.clean.tb %>% as.data.frame() %>% .[sample(1:nrow(.), min(10, nrow(.))), ]

# End of script
