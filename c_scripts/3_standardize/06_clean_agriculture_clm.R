# AGRICULTURE CLM (Community Land Model)

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(reshape2)
library(readr)
library(magrittr)

# utils
source("c_scripts/3_standardize/00_utils.R")

# configs & inputs
configs <- all_data[["0.configs"]]
countries.tb <- configs[["countries"]]
fao.crop.indicators.tb <- configs[["fao.crop.indicators"]]

# list of CLM sheets
agriculture.clm.ls <- all_data[["4b.agriculture.clm"]]

# FAO mean yields (small, deterministic helper)
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

# Clean & reshape a single CLM sheet
CleanReshape_AgricultureCLM <- function(source_table, source_table_name) {
  crop <-
    source_table_name %>%
    strsplit("-", fixed = TRUE) %>%
    unlist() %>%
    .[1] %>%
    tolower()

  years.elapsed <-
    source_table_name %>%
    strsplit("-", fixed = TRUE) %>%
    unlist() %>%
    .[2] %>%
    as.numeric()

  result <-
    source_table %>%
    ReplaceNames(., names(.), tolower(names(.))) %>%
    ReplaceNames(., c("nation-id", "nation-name"), c("country.id", "country.name")) %>%
    select(-any_of(c("id", "country.name"))) %>%
    mutate(across(where(is.list), ~ suppressWarnings(as.character(unlist(.))))) %>%
    reshape2::melt(id = "country.id") %>%
    mutate(
      variable_chr = tolower(as.character(variable)),
      soot.injection.scenario = readr::parse_number(variable_chr),
      crop = crop,
      years.elapsed = years.elapsed,
      pct.change.harvest.yield = na_if(as.numeric(value), 9.96920996838686e+36)
    ) %>%
    left_join(countries.tb, by = "country.id") %>%              # add country metadata
    left_join(fao.crop.indicators.clean.tb, by = "country.iso3") %>% # add FAO means
    select(
      country.name, country.iso3, country.hemisphere,
      country.region, country.sub.region, country.intermediate.region,
      country.nuclear.weapons, country.nato.member.2024,
      country.population.2018, country.land.area.sq.km,
      mean.yield.corn, mean.yield.rice, mean.yield.wheat, mean.yield.soya.beans,
      soot.injection.scenario,
      years.elapsed,
      crop,
      pct.change.harvest.yield
    ) %>%
    as_tibble()

  return(result)
}

# Assemble full clean table
agriculture.clm.clean.tb <-
  Map(
    CleanReshape_AgricultureCLM,
    agriculture.clm.ls,
    names(agriculture.clm.ls)
  ) %>%
  do.call(rbind, .) %>%
  mutate(
    crop = case_when(
      crop == "grass" ~ "livestock.pasture.grass",
      crop == "swheat" ~ "spring.wheat",
      TRUE ~ crop
    )
  ) %>%
  pivot_wider(
    names_from = crop,
    values_from = pct.change.harvest.yield,
    names_glue = "pct.change.harvest.yield.{crop}",
    values_fn = dplyr::first   # avoid list-columns on duplicate keys
  ) %>%
  as_tibble()

# Optional quick peek (can remove)
# agriculture.clm.clean.tb %>% as.data.frame() %>% .[sample(1:nrow(.), 10),]
