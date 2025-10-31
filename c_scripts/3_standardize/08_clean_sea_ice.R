# SEA ICE

library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(readr)
library(magrittr)

# utils
source("c_scripts/3_standardize/00_utils.R")

# configs & inputs
configs <- all_data[["0.configs"]]
months.tb <- configs[["months"]]
ports.tb <- configs[["ports"]]

# list of sea ice sheets
sea.ice.ls <- all_data[["6.sea.ice"]]

# Clean & reshape a single sea ice sheet
CleanReshape_SeaIce <- function(source_table, source_table_name) {
  scenario <- 
    stringr::str_extract(source_table_name, "[0-9.]+(?=Tg)") %>%
    as.numeric() %>%
    dplyr::recode(`46.8` = 47)

  result <-
    source_table %>%
    .[-1, ] %>%
    ReplaceNames(., names(source_table)[1], "port") %>%
    reshape2::melt(id = "port") %>%
    ReplaceNames(., c("variable", "value"), c("month", "sea.ice.thickness.meters")) %>%
    mutate(
      months.elapsed = as.numeric(gsub("\\.", "", month)) - 1,
      month = (months.elapsed - 1) %% 12 + 1,
      years.elapsed = (months.elapsed - 1) %/% 12,
      soot.injection.scenario = scenario
    ) %>%
    left_join(months.tb, by = "month") %>%
    left_join(ports.tb, by = "port") %>%
    ReplaceNames(., "country", "country.name") %>%
    select(
      port, country.name, container.traffic, latitude, longitude,
      soot.injection.scenario,
      months.elapsed, years.elapsed, month,
      season.n.hemisphere, season.s.hemisphere,
      sea.ice.thickness.meters
    ) %>%
    as_tibble()

  return(result)
}

# Assemble full clean table
sea.ice.clean.tb <-
  Map(
    CleanReshape_SeaIce,
    sea.ice.ls,
    names(sea.ice.ls)
  ) %>%
  do.call(rbind, .) %>%
  as_tibble()

# Quick spot check
sea.ice.clean.tb %>% slice_sample(n = 10)
