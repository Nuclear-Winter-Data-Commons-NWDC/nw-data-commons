# PRECIPITATION

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(reshape2)
library(magrittr)

# Import utility functions (including FlagOutliers_IQR)
source("c_scripts/3_standardize/00_utils.R")

# Reference config tables from all_data[["0.configs"]]
configs <- all_data[["0.configs"]]
months.tb <- configs[["months"]]
countries.tb <- configs[["countries"]]
source.table.configs.tb <- configs[["source.table.configs"]]

# The precipitation tables are in all_data[["2.precipitation"]]
precipitation.ls <- all_data[["2.precipitation"]]
precipitation.sheet.names <- names(precipitation.ls)

CleanReshape_Precip <- function(source_table, source_table_name) {
  scenario <- 
    source_table_name %>%
    strsplit("_") %>%
    lapply(`[`, 1) %>%
    unlist() %>%
    as.numeric()
  
  indicator <- 
    source_table_name %>%
    strsplit("_") %>%
    lapply(`[`, 2) %>%
    unlist()
  
  result <- 
    source_table %>%
    ReplaceNames(., names(.), tolower(names(.))) %>%
    ReplaceNames(., c("id", "nation"), c("country.id", "country.name")) %>%
    mutate(across(where(is.list), ~ suppressWarnings(as.numeric(unlist(.))))) %>%
    select(-country.name) %>%
    reshape2::melt(id = "country.id") %>%
    mutate(
      soot.injection.scenario = scenario,
      variable = as.character(variable),
      indicator = indicator,
      years.elapsed.raw = str_extract(variable, "^[^ ]+") %>% as.numeric(),
      month = str_extract(variable, "(?<= - )\\d+") %>% as.numeric()
    ) %>%
    group_by(soot.injection.scenario) %>%
    mutate(
      years.elapsed = years.elapsed.raw - min(years.elapsed.raw, na.rm = TRUE),
      months.elapsed = years.elapsed * 12 + month
    ) %>%
    ungroup() %>%
    mutate(
      start.date = case_when(
        soot.injection.scenario == 0 ~ as.Date("01/31/2018", format = "%m/%d/%Y"),
        soot.injection.scenario %in% c(5, 16, 27, 37, 47, 150) ~ as.Date("01/31/2020", format = "%m/%d/%Y"),
        TRUE ~ NA_Date_
      ),
      date = start.date %m+% months(months.elapsed)  # months.elapsed starts at 0
    ) %>%
    as_tibble()
  
  print(source_table_name)
  return(result)
}

precipitation.clean.tb <-
  Map(
    CleanReshape_Precip,
    precipitation.ls,
    precipitation.sheet.names
  ) %>%
  do.call(rbind, .) %>%
  pivot_wider(
    names_from = indicator,
    values_from = value
  ) %>%
  mutate( # converting unit from m/s to mm/month
    precip.rate = precip.rate * 1000 * 86400 * 30.4375,
    precip.stdev = precip.stdev * 1000 * 86400 * 30.4375
  ) %>%
  left_join( # add months metadata (seasons in n & s hemisphere)
    ., 
    months.tb,
    by = "month"
  ) %>%
  left_join( # add country metadata from configs table
    ., 
    countries.tb,
    by = "country.id"
  ) %>%
  FlagOutliers_IQR(source.table.list.name = precipitation.ls) %>%
  dplyr::select( # select & order final variables
    country.name, country.iso3, country.hemisphere,
    country.region, country.sub.region, country.intermediate.region,
    country.nuclear.weapons, country.nato.member.2024,
    country.population.2018, country.land.area.sq.km,
    soot.injection.scenario,
    years.elapsed, months.elapsed, date, month, season.n.hemisphere, season.s.hemisphere,
    precip.rate, any_of("precip.rate.outlier.flag"),
    precip.stdev, any_of("precip.stdev.outlier.flag")
  ) %>%
  as_tibble()

# Preview a random sample of 10 rows
precipitation.clean.tb %>% as.data.frame() %>% .[sample(1:nrow(.), 10),]