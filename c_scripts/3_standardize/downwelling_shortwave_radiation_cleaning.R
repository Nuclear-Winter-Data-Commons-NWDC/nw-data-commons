# DOWNWELLING SHORTWAVE RADIATION ----
# Clean and reshape downwelling shortwave radiation data imported from CSV files

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(reshape2)
library(magrittr)

# Import aggregated data using abstracted import function
# (This sources 00_utils_core internally)
source("c_scripts/3_standardize/00_utils_import.R")

# IMPORTANT: Re-source utilities AFTER import to ensure functions are available
source("c_scripts/3_standardize/00_utils_core.R")
source("c_scripts/3_standardize/00_utils_validate.R")

downwelling_shortwave_radiation_dir <- "b_data/osf_data_current/2_aggregated/downwelling_shortwave_radiation"
dsr_import <- DetectAndImportData(downwelling_shortwave_radiation_dir)

if (dsr_import$file_type == "mixed") {
  stop("Mixed file types detected in downwelling_shortwave_radiation directory. ",
       "Please ensure directory contains ONLY CSVs OR ONLY Excel files.")
}

downwelling.shortwave.radiation.ls <- dsr_import$data
dsr.sheet.names <- names(downwelling.shortwave.radiation.ls)

# Function to clean and reshape a single downwelling shortwave radiation table
CleanReshape_DSR <- function(source_table, source_table_name) {
  # Parse scenario from filename using scenarios.tb config table
  # Match file prefix patterns in source_table_name to scenarios.tb$file.prefix
  # scenarios.tb loaded by 00_utils_import.R from configs workbook

  # Find matching scenario from config table
  matched_row <- scenarios.tb %>%
    filter(stringr::str_detect(source_table_name, stringr::str_remove(file.prefix, "^nw_"))) %>%
    slice(1)  # take first match if multiple

  scenario <- if (nrow(matched_row) > 0) {
    matched_row$soot.injection.scenario
  } else {
    warning("No matching scenario found in scenarios.tb for: ", source_table_name)
    NA_real_
  }

  # Parse indicator from filename (mean, min, max, stdev)
  indicator <- dplyr::case_when(
    grepl("mean_max", source_table_name) ~ "surface.radiation.max",
    grepl("mean_min", source_table_name) ~ "surface.radiation.min",
    grepl("mean_stdev", source_table_name) ~ "surface.radiation.stdev",
    grepl("_mean$", source_table_name) ~ "surface.radiation.mean",  # matches after version suffix stripped by ImportCSVsFromDirectory
    TRUE ~ NA_character_
  )

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
      date = start.date %m+% months(months.elapsed)
    ) %>%
    as_tibble()

  if (interactive()) {
    print(paste("Imported:", source_table_name))
  }
  return(result)
}

# Process all tables
downwelling.shortwave.radiation.clean.tb <-
  Map(
    CleanReshape_DSR,
    downwelling.shortwave.radiation.ls,
    dsr.sheet.names
  ) %>%
  do.call(rbind, .) %>%
  pivot_wider(
    names_from = indicator,
    values_from = value,
    values_fn = list
  ) %>%
  mutate(across(where(is.list), ~ map_dbl(., ~if(is.null(.x) || length(.x) == 0) NA_real_ else as.numeric(.x[[1]])))) %>%
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
  FlagOutliers_IQR(source.table.list.name = downwelling.shortwave.radiation.ls) %>%
  dplyr::select( # select & order final variables
    any_of(c(
      "country.name", "country.iso3", "country.hemisphere",
      "country.region", "country.sub.region", "country.intermediate.region",
      "country.nuclear.weapons", "country.nato.member.2024",
      "country.population.2018", "country.land.area.sq.km",
      "soot.injection.scenario",
      "years.elapsed", "months.elapsed", "date", "month", "season.n.hemisphere", "season.s.hemisphere",
      "surface.radiation.mean", "surface.radiation.mean.outlier.flag",
      "surface.radiation.min", "surface.radiation.min.outlier.flag",
      "surface.radiation.max", "surface.radiation.max.outlier.flag",
      "surface.radiation.stdev", "surface.radiation.stdev.outlier.flag"
    ))
  ) %>%
  as_tibble()

# Preview a random sample of 10 rows
if (interactive()) {
  downwelling.shortwave.radiation.clean.tb %>% as.data.frame() %>% .[sample(1:nrow(.), 10), ]
}
