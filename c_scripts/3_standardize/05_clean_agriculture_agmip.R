# AGRICULTURE AGMIP (Multi-Model Aggregates)

library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(readr)
library(magrittr)

# Import utility functions
source("c_scripts/3_standardize/00_utils.R")

# --- helpers (small & focused) ------------------------------------------------
normalize_crop <- function(x) {
  x <- tolower(x)
  case_when(
    x %in% c("maize", "maize (corn)", "corn") ~ "corn",
    x %in% c("soy", "soybeans", "soya beans", "soya.beans") ~ "soya.beans",
    x %in% c("wheat") ~ "wheat",
    x %in% c("rice") ~ "rice",
    TRUE ~ x
  )
}

parse_scenario_tg <- function(s) {
  s <- tolower(s)
  tg <- suppressWarnings(readr::parse_number(s))
  if (is.na(tg) && s %in% c("control","baseline","0","0tg")) tg <- 0
  tg
}

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
  mutate(crop = normalize_crop(crop)) %>%
  group_by(country.iso3, crop) %>%
  summarise(mean.yield = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = crop,
    values_from = mean.yield,
    names_glue = "mean.yield.{crop}"
  )

# Clean & Reshape Agriculture.AGMIP Data
CleanReshape_AgricultureAGMIP <- function(source_table, source_table_name) {
  message("Working on: ", source_table_name)

  # Expected like "..._Bardeen_5Tg_maize" or "..._Mills_control_wheat"
  parts <- strsplit(gsub("\\.", "_", source_table_name), "_")[[1]]
  model_raw    <- parts[1] %>% tolower()
  scenario_raw <- parts[2] %>% tolower()
  crop_raw     <- parts[3] %>% tolower()

  cesm.model.configuration <- ifelse(model_raw == "bardeen", "toon", model_raw)
  soot.injection.scenario  <- parse_scenario_tg(scenario_raw)
  crop_norm                <- normalize_crop(crop_raw)

  df <-
    source_table %>%
    ReplaceNames(., names(.), tolower(names(.))) %>%
    ReplaceNames(., "country_iso3", "country.iso3") %>%
    select(-any_of(c("country_name", "...1"))) %>%
    mutate(across(where(is.list), ~ suppressWarnings(as.character(unlist(.)))))

  # Melt wide → long; years.elapsed0 is whatever index is present
  long0 <-
    reshape2::melt(df, id = "country.iso3") %>%
    mutate(
      crop                     = crop_norm,
      cesm.model.configuration = cesm.model.configuration,
      soot.injection.scenario  = soot.injection.scenario,
      years.elapsed0           = suppressWarnings(readr::parse_number(stringr::str_extract(variable, "(?<=_)\\d+$"))),
      pct.change.harvest.yield = suppressWarnings(readr::parse_number(value))
    ) %>%
    select(-variable, -value)

  # Ensure 1-indexed years: if a sheet appears 0-indexed (min==0), add +1
  min_idx <- suppressWarnings(min(long0$years.elapsed0, na.rm = TRUE))
  long <- long0 %>%
    mutate(years.elapsed = if (is.finite(min_idx) && min_idx == 0) years.elapsed0 + 1 else years.elapsed0) %>%
    select(-years.elapsed0)

  result <-
    long %>%

    filter(!is.na(pct.change.harvest.yield)) %>%
    as_tibble()

  return(result)
}

# Assemble full clean table (wide on PK so crops are across columns)
agriculture.agmip.clean.tb <-
  Map(
    CleanReshape_AgricultureAGMIP,
    agriculture.agmip.ls,
    names(agriculture.agmip.ls)
  ) %>%
  do.call(rbind, .) %>%
  # Wide by crop on the correct PK to avoid stacking
  pivot_wider(
    id_cols    = c(country.iso3, soot.injection.scenario, years.elapsed, cesm.model.configuration),
    names_from = crop,
    values_from = pct.change.harvest.yield,
    names_glue = "pct.change.harvest.yield.{crop}",
    values_fn  = dplyr::first  # guard against accidental duplicates
  ) %>%
  # Join metadata
  left_join(countries.tb, by = "country.iso3") %>%
  left_join(fao.crop.indicators.clean.tb, by = "country.iso3") %>%
  # Bring common metadata to the front (keeps rest as is)
  dplyr::select(
    country.name, country.iso3, country.hemisphere,
    country.region, country.sub.region, country.intermediate.region,
    country.nuclear.weapons, country.nato.member.2024,
    country.population.2018, country.land.area.sq.km,
    mean.yield.corn, mean.yield.rice, mean.yield.wheat, mean.yield.soya.beans,
    soot.injection.scenario, years.elapsed, cesm.model.configuration,
    everything()
  ) %>%
    dplyr::select(-dplyr::any_of(c("country.id", "country.ggcmi.id", "country.agricultural.land.area.sq.km"))
  ) %>%
  as_tibble()

# (Optional) quick peek
# agriculture.agmip.clean.tb %>% slice_sample(n = 10)
