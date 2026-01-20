# STARVATION

library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(readr)
library(magrittr)

source("c_scripts/3_standardize/00_utils.R")

# configs & inputs
configs <- all_data[["0.configs"]]
countries.tb <- configs[["countries"]]
starvation.ls <- all_data[["7.starvation"]]

CleanReshape_Starvation <- function(source_table, source_table_name) {
  # Parse sheet name to extract trade status and livestock type
  # Expected formats: "num_starve_notrade_livestock", "num_starve_trade_plivestock", etc.

  trade.status <- if (str_detect(source_table_name, "notrade")) {
    "no trade"
  } else if (str_detect(source_table_name, "trade")) {
    "trade"
  } else {
    NA_character_
  }

  livestock.type <- case_when(
    str_detect(source_table_name, "plivestock") ~ "partial livestock",
    str_detect(source_table_name, "nlivestock") ~ "no livestock",
    str_detect(source_table_name, "livestock") ~ "livestock",
    TRUE ~ NA_character_
  )

  # Standardize column names (handle inconsistent Population vs population_2010)
  source_table <- source_table %>%
    ReplaceNames(., names(.), tolower(names(.)))

  if ("population_2010" %in% names(source_table)) {
    source_table <- source_table %>%
      ReplaceNames(., "population_2010", "population")
  }

  # Convert to long format
  long <- source_table %>%
    reshape2::melt(id = c("nation", "population")) %>%
    ReplaceNames(., c("nation", "variable", "value"),
                 c("country.name", "scenario.raw", "num.starving.millions")) %>%
    mutate(
      # Extract soot injection scenario from column names like "5 Tg", "150 Tg +hw", etc.
      # Use str_extract to get the number directly (more robust than case_when)
      soot.injection.scenario = as.numeric(str_extract(scenario.raw, "^\\d+")),
      # Extract food waste reduction scenario (e.g., "+hw", "+ tw")
      food.waste.reduction.scenario = case_when(
        str_detect(scenario.raw, "(?i)\\+\\s*hw") ~ "50% reduction",
        str_detect(scenario.raw, "(?i)\\+\\s*tw") ~ "100% reduction",
        TRUE ~ "0% reduction"
      ),
      # Add trade status and livestock type from sheet name
      trade.status = trade.status,
      livestock.type = livestock.type,
      # Convert population from millions to actual count (if needed for consistency)
      population = as.numeric(population),
      num.starving.millions = as.numeric(num.starving.millions)
    ) %>%
    select(-scenario.raw)

  return(long)
}

starvation.clean.tb <-
  Map(
    CleanReshape_Starvation,
    starvation.ls,
    names(starvation.ls)
  ) %>%
  bind_rows() %>%
  # Remove 'Total' rows (aggregate row not needed for country-level analysis)
  filter(!grepl("^Total$", country.name, ignore.case = TRUE)) %>%
  # Join with country metadata
  left_join(
    countries.tb %>%
      select(country.name, country.iso3, country.hemisphere,
             country.region, country.sub.region, country.intermediate.region,
             country.nuclear.weapons, country.nato.member.2024,
             country.population.2018, country.land.area.sq.km) %>%
      mutate(country.population.2018 = as.numeric(country.population.2018)),
    by = "country.name"
  ) %>%
  # Calculate derived metrics
  mutate(
    population = as.numeric(population),
    num.starving.millions = as.numeric(num.starving.millions),
    pct.population.starving = (num.starving.millions / population) * 100,
    # Use country.population.2018 as backup if population column is missing/NA
    pct.population.starving.2018 = (num.starving.millions / country.population.2018) * 100
  ) %>%
  # Apply outlier detection
  FlagOutliers_IQR(source.table.list.name = starvation.ls) %>%
  # Select and order final variables
  select(
    country.name, country.iso3, country.hemisphere,
    country.region, country.sub.region, country.intermediate.region,
    country.nuclear.weapons, country.nato.member.2024,
    country.population.2018, country.land.area.sq.km,
    population,
    soot.injection.scenario, food.waste.reduction.scenario,
    trade.status, livestock.type,
    num.starving.millions,
    pct.population.starving,
    pct.population.starving.2018,
    any_of(c(
      "num.starving.millions.outlier.flag",
      "pct.population.starving.outlier.flag",
      "pct.population.starving.2018.outlier.flag"
    ))
  ) %>%
  as_tibble()

# Quick spot check
starvation.clean.tb %>% slice_sample(n = 10)
