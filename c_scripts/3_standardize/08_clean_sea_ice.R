 #2.6 SEA ICE ----
    
    ImportSourceData_GoogleSheets("6.sea.ice")
    
    CleanReshape_SeaIce <- function(source_table){
    
      scenario <- names(source_table)[1] %>% str_extract(., "(?<=NW-).*")
      
      result <-
        source_table %>%
        .[-1,] %>%
        ReplaceNames(., names(source_table)[1], "port") %>%
        reshape2::melt(
          .,
          id = "port"
        ) %>%
        ReplaceNames(., c("variable","value"), c("month","sea.ice.thickness.meters")) %>%
        mutate(
          months.elapsed = as.character(month) %>% gsub("\\.", "", .) %>% as.numeric %>% subtract(1),  # Clean and convert month strings
          month = (months.elapsed - 1) %% 12 + 1,  # Calculate the month (1-12)
          years.elapsed = (months.elapsed - 1) %/% 12 # Calculate the year (0, 1, 2, ...)
        ) %>%
        mutate(
          soot.injection.scenario =
            recode(scenario, 
              "37Tg" = 37,
              "46.8Tg" = 47,
              "150Tg" = 150
            )
        ) %>%
        left_join( #add months metadata (seasons in n & s hemisphere)
          ., 
          months.tb,
          by = "month"
        ) %>%
        left_join( #add port metadata
          ., 
          ports.tb,
          by = "port"
        ) %>%
        select(
          port, country, container.traffic, latitude, longitude,
          soot.injection.scenario, 
          months.elapsed, years.elapsed, month, season.n.hemisphere, season.s.hemisphere, 
          sea.ice.thickness.meters
        ) %>%
        as_tibble
      
      return(result)
    
    }
    
    sea.ice.clean.tb <-
      lapply(
        sea.ice.ls,
        CleanReshape_SeaIce
      ) %>%
      do.call(rbind, .) %>%
      FlagOutliers_IQR() %>%
      as_tibble()
    
    #sea.ice.clean.tb %>%
    #  select(-sea.ice.thickness.meters) %>%
    #  apply(., 2, TableWithNA)
  
