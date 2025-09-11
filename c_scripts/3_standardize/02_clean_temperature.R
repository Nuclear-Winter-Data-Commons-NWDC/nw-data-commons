  #2.1 TEMPERATURE ----
    
    ImportSourceData_GoogleSheets("1.temperature")

    #source_table_list <- temperature.ls[[1]]
    #source_table_names <- temperature.ls %>% names %>% .[1] #%>% list(.)
    CleanReshape_Temp <- function(source_table_list, source_table_names) {

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
          date = start.date %m+% months(months.elapsed)  # months.elapsed starts at 0
        ) %>%
        as_tibble()

      print(source_table_names)

      return(result)
    }
    
    temperature.clean.tb <-
      Map(
          CleanReshape_Temp,
          temperature.ls,
          names(temperature.ls)
      ) %>%
      do.call(rbind, .) %>%
      pivot_wider(
        names_from = indicator,
        values_from = value
      ) %>%
      mutate( #converting units from kelvin to celsius
        surface.temp = surface.temp - 273.15,
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
      #   surface.temp.weighted.by.land.area = surface.temp * country.land.area.sq.km,
      #   surface.temp.weighted.by.population = surface.temp * country.population.2018
      # ) %>%
      FlagOutliers_IQR() %>%
      dplyr::select( #select & order final variables
        country.name, country.iso3,	country.hemisphere,	
        country.region,	country.sub.region,	country.intermediate.region, 
        country.nuclear.weapons, country.nato.member.2024, 
        country.population.2018, country.land.area.sq.km,
        soot.injection.scenario, 
        years.elapsed, months.elapsed, date, month, season.n.hemisphere, season.s.hemisphere,
        surface.temp, surface.temp.outlier.flag, #surface.temp.weighted.by.land.area, surface.temp.weighted.by.population
        surface.temp.stdev, surface.temp.stdev.outlier.flag
      ) %>%
      as_tibble()

    temperature.clean.tb %>% as.data.frame %>% .[sample(1:nrow(.), 10),]

