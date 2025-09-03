  #2.3 UV ----
    
    ImportSourceData_GoogleSheets("3.uv")
    
    CleanReshape_UV <- function(source_table_list, source_table_names) {
      
      scenario <- 
        source_table_names %>%
        strsplit(., "_") %>% 
        unlist %>%
        .[1] %>%
        ifelse(. != "control", paste0(., "Tg"), .)
      
      indicator <- 
        source_table_names %>%
        strsplit(., "_") %>% 
        unlist %>%
        .[2]
      
      result <- 
        source_table_list %>% 
        ReplaceNames(., names(.), tolower(names(.))) %>%
        ReplaceNames(., c("id", "nation"), c("country.id", "country.name")) %>%
        mutate(across(where(is.list), ~ suppressWarnings(as.numeric(unlist(.))))) %>%
        select(-country.name) %>%
        reshape2::melt(id = "country.id") %>%
        mutate(
          soot.injection.scenario = recode(
            scenario,
            "control" = 0,
            "150Tg" = 150
          ),
          variable = as.character(variable),
          year.raw = str_extract(variable, "^[^ ]+") %>% as.numeric(),
          month = str_extract(variable, "(?<= - )\\d+") %>% as.numeric(),
          indicator = indicator
        ) %>%
        group_by(soot.injection.scenario) %>%
        mutate(
          years.elapsed = year.raw - min(year.raw, na.rm = TRUE),
          months.elapsed = years.elapsed * 12 + month
        ) %>%
        ungroup() %>%
        mutate(value = as.numeric(value)) %>%
        left_join(countries.tb, by = "country.id") %>%
        left_join(months.tb, by = "month") %>%
        select(
          country.name, country.iso3, country.hemisphere,	
          country.region, country.sub.region, country.intermediate.region, 
          country.nuclear.weapons, country.nato.member.2024, 
          country.population.2018, country.land.area.sq.km,
          soot.injection.scenario, years.elapsed, months.elapsed, month, 
          season.n.hemisphere, season.s.hemisphere,
          indicator, value
        ) %>%
        as_tibble()
      
      print(source_table_names)
      
      return(result)
    }
    
    uv.clean.tb <-
      Map(
        CleanReshape_UV,
        uv.ls,
        names(uv.ls)
      ) %>%
      bind_rows() %>%
      pivot_wider(
        names_from = indicator,
        values_from = value
      ) %>%
      ReplaceNames(., names(.), tolower(names(.))) %>%
      FlagOutliers_IQR() %>%
      as_tibble()

    #uv.clean.tb %>%
    #  select(-value) %>%
    #  apply(., 2, TableWithNA)
    
