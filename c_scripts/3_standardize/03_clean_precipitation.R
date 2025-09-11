 #2.2 PRECIPITATION ----
    ImportSourceData_GoogleSheets("2.precipitation")

    #source_table_list <- precipitation.ls[[1]]
    #source_table_names <- precipitation.ls %>% names %>% .[1] %>% list(.)
    CleanReshape_Precip <- function(source_table_list, source_table_names) {
      
      scenario <- 
        source_table_names %>%
        strsplit("_") %>%
        lapply(`[`, 1) %>%
        unlist() %>%
        as.numeric()
      
      indicator <- 
        source_table_names %>%
        strsplit("_") %>%
        lapply(`[`, 2) %>%
        unlist()
      
      result <- 
        source_table_list %>%
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
            soot.injection.scenario %in% c(5, 16, 150) ~ as.Date("01/31/2020", format = "%m/%d/%Y"),
            TRUE ~ NA_Date_
          ),
          date = start.date %m+% months(months.elapsed - 1)
        ) %>%
        as_tibble()
      
      print(source_table_names)
      
      return(result)
    }

    precipitation.clean.tb <-
      Map(
          CleanReshape_Precip,
          precipitation.ls,
          names(precipitation.ls)
      ) %>%
      do.call(rbind, .) %>%
      pivot_wider(
        names_from = indicator,
        values_from = value
      ) %>%
      mutate( #converting unit from m/s to mm/month
        precip.rate = precip.rate * 1000 * 86400 * 30.4375,
        precip.stdev = precip.stdev * 1000 * 86400 * 30.4375
      ) %>%
      left_join( #add months metadata (seasons in n & s hemisphere)
        ., 
        months.tb,
        by = "month"
      ) %>%
      left_join( #add country metadata from configs table
        ., 
        countries.tb,
        by = "country.id"
      ) %>%
      # mutate(
      #   precip.rate.convective.weighted.by.land.area = precip.rate.convective * country.land.area.sq.km,
      #   precip.rate.convective.weighted.by.population = precip.rate.convective * country.population.2018
      # ) %>%
      FlagOutliers_IQR() %>%
      dplyr::select( #select & order final variables
        country.name, country.iso3,	country.hemisphere,	
        country.region,	country.sub.region,	country.intermediate.region, 
        country.nuclear.weapons, country.nato.member.2024, 
        country.population.2018, country.land.area.sq.km,
        soot.injection.scenario, 
        years.elapsed, months.elapsed, date, month, season.n.hemisphere, season.s.hemisphere,
        precip.rate, precip.rate.outlier.flag,
        precip.stdev, precip.stdev.outlier.flag
      ) %>%
      as_tibble()

    precipitation.clean.tb %>% as.data.frame %>% .[sample(1:nrow(.), 10),]

    # precipitation.clean.tb %>%
    #   filter(precip.rate == precip.stdev) %>%
    #   group_by(years.elapsed, months.elapsed, soot.injection.scenario) %>%
    #   summarise(num_countries = n_distinct(country.name), .groups = "drop") %>%
    #   as.data.frame %>%
    #   arrange(soot.injection.scenario)


