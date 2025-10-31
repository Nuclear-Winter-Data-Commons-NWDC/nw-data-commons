# FISH CATCH

library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(readr)
library(magrittr)

source("c_scripts/3_standardize/00_utils.R")

# configs & inputs
configs <- all_data[["0.configs"]]
fish.catch.indicators.tb <- configs[["fish.catch.indicators"]]
fish.catch.eez.tb <- configs[["fish.catch.eez"]]
fish.catch.ls <- all_data[["5.fish.catch"]]

CleanReshape_FishCatch <- function(source_table, source_table_name) {
  scenario <- tolower(source_table_name)

  # Handle control/cntrl cases explicitly
  scenario <- case_when(
    scenario %in% c("control", "cntrl") ~ "0",
    TRUE ~ scenario
  )

  long <-
    source_table %>%
    select(names(.)[!str_detect(names(.), "(?i)ctrl")]) %>%
    ReplaceNames(., names(.), tolower(names(.))) %>%
    ReplaceNames(., "eez_no", "eez.num") %>%
    mutate(across(where(is.list), ~ suppressWarnings(as.numeric(unlist(.))))) %>%
    reshape2::melt(id = "eez.num") %>%
    mutate(
      soot.injection.scenario = readr::parse_number(scenario),
      years.elapsed = dplyr::coalesce(
        stringr::str_match(variable, "(?i)_\\s*yr(\\d+)")[,2],
        stringr::str_match(variable, "_\\s*(\\d+)\\s*$")[,2],
        stringr::str_match(variable, "(\\d+)\\s*$")[,2]
      ) %>% as.numeric(),
      indicator.raw = str_extract(variable, "(?<=_).*?(?=_[^_]*$)"),
      value = suppressWarnings(as.numeric(value)) / 1e9
    ) %>%
    mutate(
      indicator = IndexMatchToVectorFromTibble(
        indicator.raw,
        fish.catch.indicators.tb,
        "extracted.indicator.name.raw",
        "indicator.name.clean",
        mult.replacements.per.cell = FALSE
      )
    )

  # Collapse duplicates before widening
  collapsed <-
    long %>%
    group_by(soot.injection.scenario, eez.num, years.elapsed, indicator) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

  wide <-
    collapsed %>%
    pivot_wider(
      id_cols = c(soot.injection.scenario, eez.num, years.elapsed),
      names_from = indicator,
      values_from = value
    )

  as_tibble(wide)
}

fish.catch.clean.tb <-
  Map(
    CleanReshape_FishCatch,
    fish.catch.ls,
    names(fish.catch.ls)
  ) %>%
  bind_rows() %>%
  mutate( # ensure numeric, treating "NaN" as NA
    mean.pct.catch.change = readr::parse_number(as.character(mean.pct.catch.change), na = c("", "NA", "N/A", "NaN")),
    std.dev.pct.catch.change = readr::parse_number(as.character(std.dev.pct.catch.change), na = c("", "NA", "N/A", "NaN")),
    mean.catch = readr::parse_number(as.character(mean.catch), na = c("", "NA", "N/A", "NaN")),
    std.dev.catch = readr::parse_number(as.character(std.dev.catch), na = c("", "NA", "N/A", "NaN")),
    mean.catch.change = readr::parse_number(as.character(mean.catch.change), na = c("", "NA", "N/A", "NaN")),
    std.dev.catch.change = readr::parse_number(as.character(std.dev.catch.change), na = c("", "NA", "N/A", "NaN"))
  ) %>%
  left_join(fish.catch.eez.tb, by = "eez.num") %>%
  mutate(
    mean.pct.catch.change = mean.pct.catch.change * 1e9,
    std.dev.pct.catch.change = std.dev.pct.catch.change * 1e9,
    eez.name = gsub("Exclusive Economic Zone", "EEZ", eez.name),
    mean.catch.per.1000.sq.km = mean.catch / (eez.area / 1000)
  ) %>%
  FlagOutliers_IQR() %>%
  select(
    eez.name, eez.num, eez.area,
    years.elapsed,
    soot.injection.scenario,
    mean.catch,
    mean.catch.per.1000.sq.km,
    mean.catch.change,
    mean.pct.catch.change,
    std.dev.catch,
    std.dev.catch.change,
    std.dev.pct.catch.change,
    any_of(c(
      "mean.catch.outlier.flag",
      "mean.catch.per.1000.sq.km.outlier.flag",
      "mean.catch.change.outlier.flag",
      "mean.pct.catch.change.outlier.flag",
      "std.dev.catch.outlier.flag",
      "std.dev.catch.change.outlier.flag",
      "std.dev.pct.catch.change.outlier.flag"
    ))
  ) %>%
  as_tibble()

# Quick spot check
fish.catch.clean.tb %>% slice_sample(n = 10)
