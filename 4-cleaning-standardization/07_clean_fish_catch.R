 #2.5 FISH CATCH ----
    
    ImportSourceData_GoogleSheets("5.fish.catch")
    
    CleanReshape_FishCatch <- function(source_table_list, source_table_names){

      scenario <- 
        source_table_names
      
      result <- 
        source_table_list %>% 
        select(names(.)[!str_detect(names(.), "ctrl")]) %>%
        ReplaceNames(., names(.),tolower(names(.))) %>% #lower-case all table names
        ReplaceNames(., c("eez_no"), c("eez.num")) %>%
        mutate(across(where(is.list), ~ suppressWarnings(as.numeric(unlist(.))))) %>% #convert all list variables into character
        reshape2::melt(., id = "eez.num") %>% #reshape to long
        mutate( #add/rename variables
          soot.injection.scenario = recode(
            scenario, 
            "cntrl" = 0,
            "control" = 0,
            "5Tg" = 5,
            "16Tg" = 16,
            "27Tg" = 27,
            "37Tg" = 37,
            "47Tg" = 47,
            "150Tg" = 150
          ),
          years.elapsed = variable %>% str_extract(., "(?<=_)[^_]+$") %>% str_remove(., "yr") %>% as.numeric,
          indicator.raw = 
            variable %>% 
            str_extract(., "(?<=_).*?(?=_[^_]*$)"),
          value = value %>% divide_by(10^9),
        ) %>%
        mutate(
          indicator = 
            IndexMatchToVectorFromTibble(
              indicator.raw, 
              fish.catch.indicators.tb,
              "extracted.indicator.name.raw",
              "indicator.name.clean",
              mult.replacements.per.cell = FALSE
            )
        ) %>%
        pivot_wider(
          id_cols = c(soot.injection.scenario, eez.num, years.elapsed),
          names_from = indicator,
          values_from = value
        )%>%
        as_tibble #ensure final result is a tibble
      
      print(source_table_names)
      
      return(result)
      
    }

    fish.catch.clean.tb <- 
      Map(
        CleanReshape_FishCatch,
        fish.catch.ls,
        names(fish.catch.ls)
      ) %>%
      bind_rows() %>%
      left_join(fish.catch.eez.tb, by = "eez.num") %>%
      mutate(
        mean.pct.catch.change = mean.pct.catch.change * 10^9,
        std.dev.pct.catch.change = std.dev.pct.catch.change * 10^9,
        eez.name = eez.name %>% gsub("Exclusive Economic Zone", "EEZ", .),
        mean.catch.per.1000.sq.km = mean.catch / (eez.area / 1000)
      ) %>%
      FlagOutliers_IQR() %>%
      select(
        eez.name, eez.num, eez.area, 
        years.elapsed, 
        soot.injection.scenario,
        mean.catch,  
        mean.catch.per.1000.sq.km,
        mean.catch.change, 
        mean.pct.catch.change, 
        std.dev.catch,
        std.dev.catch.change,
        std.dev.pct.catch.change,
        mean.catch.outlier.flag,
        mean.catch.per.1000.sq.km.outlier.flag,
        mean.catch.change.outlier.flag,
        mean.pct.catch.change.outlier.flag,
        std.dev.catch.outlier.flag,
        std.dev.catch.change.outlier.flag,
        std.dev.pct.catch.change.outlier.flag
      )

    #fish.catch.clean.tb %>%
    #  select(
    #    eez, eez.num, 
    #    soot.injection.scenario, 
    #    years.elapsed
    #  ) %>%
    #  apply(., 2, TableWithNA)
    
  