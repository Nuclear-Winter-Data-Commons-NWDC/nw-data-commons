# AGRICULTURE AGMIP (Multi-Model Aggregates)

library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(readr)
library(magrittr)

# Import utility functions
source("c_scripts/3_standardize/00_utils.R")

# Reference config tables from all_data[["0.configs"]]
configs <- all_data[["0.configs"]]
countries.tb <- configs[["countries"]]
fao.crop.indicators.tb <- configs[["fao.crop.indicators"]]

# The agriculture.agmip tables are in all_data[["4a.agriculture.agmip"]]
agriculture.agmip.ls <- all_data[["4a.agriculture.agmip"]]

# Clean FAO crop indicators for mean yield
fao.crop.indicators.clean.tb <-
  fao.crop.indicators.tb %>%
  ReplaceNames(., names(.), tolower(names(.))) %>%
  ReplaceNames(., "item", "crop") %>%
  select(any_of(c("country.iso3", "crop", "year", "value"))) %>%
  mutate(crop = tolower(crop)) %>%
  mutate(
    crop = case_when(
      crop == "maize (corn)" ~ "corn",
      crop == "soya beans" ~ "soya.beans",
      TRUE ~ crop
    )
  ) %>%
  group_by(country.iso3, crop) %>%
  summarise(mean.yield = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = crop,
    values_from = mean.yield,
    names_glue = "mean.yield.{crop}"
  )

# Clean & Reshape Agriculture.AGMIP Data
CleanReshape_AgricultureAGMIP <- function(source_table, source_table_name) {
  print(paste("Working on:", source_table_name))

  split_parts <- strsplit(source_table_name, "_")[[1]]
  cesm.model.configuration <- tolower(split_parts[1])
  scenario <- split_parts[2]
  crop <- tolower(split_parts[3])

  result <-
    source_table %>%
    ReplaceNames(., names(.), tolower(names(.))) %>%
    ReplaceNames(., "country_iso3", "country.iso3") %>%
    select(-any_of(c("country_name", "...1"))) %>%
    mutate(across(where(is.list), ~ suppressWarnings(as.character(unlist(.))))) %>%
    reshape2::melt(id = "country.iso3") %>%
    mutate(
      crop = crop,
      cesm.model.configuration = cesm.model.configuration,
      soot.injection.scenario = readr::parse_number(scenario),
      years.elapsed = str_extract(variable, "(?<=_)\\d+$") %>% as.numeric(),
      pct.change.harvest.yield = readr::parse_number(value)
    ) %>%
    left_join(countries.tb, by = "country.iso3") %>%
    left_join(fao.crop.indicators.clean.tb, by = "country.iso3") %>%
    filter(!is.na(pct.change.harvest.yield)) %>%
    as_tibble()

  return(result)
}

# Assemble full clean table
agriculture.agmip.clean.tb <-
  Map(
    CleanReshape_AgricultureAGMIP,
    agriculture.agmip.ls,
    names(agriculture.agmip.ls)
  ) %>%
  do.call(rbind, .) %>%
  mutate(
    crop = case_when(
      crop == "maize" ~ "corn",
      crop == "soy" ~ "soya.beans",
      TRUE ~ crop
    ),
    cesm.model.configuration = case_when(
      cesm.model.configuration == "bardeen" ~ "toon",
      TRUE ~ cesm.model.configuration
    )
  ) %>%
  pivot_wider(
    names_from = crop,
    values_from = pct.change.harvest.yield,
    names_glue = "pct.change.harvest.yield.{crop}",
    values_fn = dplyr::first
  ) %>%
  dplyr::select(
    country.name, country.iso3, country.hemisphere,
    country.region, country.sub.region, country.intermediate.region,
    country.nuclear.weapons, country.nato.member.2024,
    country.population.2018, country.land.area.sq.km,
    mean.yield.corn, mean.yield.rice, mean.yield.wheat, mean.yield.soya.beans,
    soot.injection.scenario, years.elapsed,
    cesm.model.configuration,
    starts_with("pct.change.harvest.yield.")
  ) %>%
  as_tibble()

# Preview random sample (optional, can be removed)
agriculture.agmip.clean.tb %>% as.data.frame() %>% .[sample(1:nrow(.), 10),]
