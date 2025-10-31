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
      x %in% c("soy", "soybean", "soybeans", "soya beans", "soya.beans") ~ "soya.beans",
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
  # Sheet-name pattern: Model_Scenario_Crop_(mean|SD), e.g.:
  #   Bardeen_5Tg_maize_mean, Bardeen_5Tg_wheat_SD, Mills_5Tg_rice_mean
  CleanReshape_AgricultureAGMIP <- function(source_table, source_table_name) {
    message("Working on: ", source_table_name)

    parts <- strsplit(gsub("\\.", "_", source_table_name), "_")[[1]]
    parts <- tolower(parts)

    model_raw    <- dplyr::coalesce(parts[1], NA_character_)
    scenario_raw <- dplyr::coalesce(parts[2], NA_character_)
    crop_raw     <- dplyr::coalesce(parts[3], NA_character_)
    stat_raw     <- dplyr::coalesce(parts[4], NA_character_)  # "mean" or "sd"

    cesm.model.configuration <- ifelse(model_raw == "bardeen", "toon", model_raw)
    soot.injection.scenario  <- parse_scenario_tg(scenario_raw)
    crop_norm                <- normalize_crop(crop_raw)
    stat_norm                <- ifelse(stat_raw %in% c("sd","stdev","std"), "sd", "mean")

    df <-
      source_table %>%
      ReplaceNames(., names(.), tolower(names(.))) %>%
      ReplaceNames(., "country_iso3", "country.iso3") %>%
      select(-any_of(c("country_name", "...1", "x"))) %>%
      mutate(across(where(is.list), ~ suppressWarnings(as.character(unlist(.)))))

    # Long reshape (force character before parsing to avoid readr error)
    long0 <-
      reshape2::melt(df, id = "country.iso3") %>%
      dplyr::mutate(
        variable_chr = as.character(variable),
        value_chr    = as.character(value),
        years.elapsed0 = suppressWarnings(
          readr::parse_number(stringr::str_extract(variable_chr, "(?<=_)\\d+$"))
        ),
        value_num = suppressWarnings(readr::parse_number(value_chr))
      ) %>%
      dplyr::select(-variable, -value, -variable_chr, -value_chr)


    # Ensure 1-indexed years: if sheet appears 0-indexed (min==0), add +1
    min_idx <- suppressWarnings(min(long0$years.elapsed0, na.rm = TRUE))
    long <- long0 %>%
      mutate(years.elapsed = if (is.finite(min_idx) && min_idx == 0) years.elapsed0 + 1 else years.elapsed0) %>%
      select(-years.elapsed0)

    # Map to final variable name depending on stat (mean vs sd)
    # mean  -> pct.change.harvest.yield.{crop}
    # sd    -> stdev.pct.change.harvest.yield.{crop}
    long <- long %>%
      mutate(
        cesm.model.configuration = cesm.model.configuration,
        soot.injection.scenario  = soot.injection.scenario,
        crop                     = crop_norm,
        stat                     = stat_norm,
        variable_name = ifelse(
          stat == "sd",
          paste0("stdev.pct.change.harvest.yield.", crop),
          paste0("pct.change.harvest.yield.",       crop)
        )
      ) %>%
      filter(!is.na(value_num))

    # Return a tidy frame keyed by PK + variable_name
    long %>%
      transmute(
        country.iso3,
        soot.injection.scenario,
        years.elapsed,
        cesm.model.configuration,
        variable_name,
        value = value_num
      )
  }

# Assemble full clean table: pivot variable_name wide, then join metadata
  agriculture.agmip.clean.tb <-
    Map(
      CleanReshape_AgricultureAGMIP,
      agriculture.agmip.ls,
      names(agriculture.agmip.ls)
    ) %>%
    dplyr::bind_rows() %>%
    tidyr::pivot_wider(
      id_cols     = c(country.iso3, soot.injection.scenario, years.elapsed, cesm.model.configuration),
      names_from  = variable_name,
      values_from = value,
      values_fn   = dplyr::first
    ) %>%
    # Join metadata after pivot so they aren't dropped
    left_join(countries.tb, by = "country.iso3") %>%
    left_join(fao.crop.indicators.clean.tb, by = "country.iso3") %>%
    # Flag outliers
    FlagOutliers_IQR() %>%
    # Bring common metadata to the front (keeps rest as is)
    dplyr::select(
      country.name, country.iso3, country.hemisphere,
      country.region, country.sub.region, country.intermediate.region,
      country.nuclear.weapons, country.nato.member.2024,
      country.population.2018, country.land.area.sq.km,
      soot.injection.scenario, years.elapsed, cesm.model.configuration,
      mean.yield.corn, mean.yield.rice, mean.yield.wheat, mean.yield.soya.beans,
      dplyr::everything()
    ) %>%
    # Drop IDs you don't want to carry
    dplyr::select(-dplyr::any_of(c(
      "country.id", "country.ggcmi.id", "country.agricultural.land.area.sq.km"
    ))) %>%
    tibble::as_tibble()

  # (Optional) quick peek
  # agriculture.agmip.clean.tb %>% slice_sample(n = 10)
