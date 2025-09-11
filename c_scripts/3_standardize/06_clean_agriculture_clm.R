 #2.4b AGRICULTURE CLM (Community Land Model) ----
    
    ImportSourceData_GoogleSheets("4b.agriculture.clm")
    
    CleanReshape_AgricultureCLM <- function(source_table_list, source_table_names){
      
      crop <- 
        source_table_names %>%
        strsplit(., "-") %>% 
        unlist %>%
        .[1]
      
      years.elapsed <- 
        source_table_names %>%
        strsplit(., "-") %>% 
        unlist %>%
        .[2] %>% 
        as.numeric
      
      result <- 
        source_table_list %>% 
        ReplaceNames(., names(.),tolower(names(.))) %>% #lower-case all table names
        ReplaceNames(., c("nation-id", "nation-name"), c("country.id","country.name")) %>%  #standardize geographic variable names
        select(-id, -country.name) %>%
        reshape2::melt(., id = "country.id") %>% #reshape to long
        mutate( #add/rename variables
          soot.injection.scenario = recode(
            variable, 
            "5tg" = 5,
            "16tg" = 16,
            "27tg" = 27,
            "37tg" = 37,
            "47tg" = 47,
            "150tg" = 150
          ),
          crop = crop,
          years.elapsed = years.elapsed,
          pct.change.harvest.yield = na_if(value, 9.96920996838686e+36)
        ) %>%
        left_join( #add country metadata from configs table
          ., 
          countries.tb,
          by = "country.id"
        ) %>%
        left_join(
          fao.crop.indicators.clean.tb,
          by = "country.iso3"
        ) %>%
        select( #select & order final variables
          country.name, country.iso3,	country.hemisphere,	
          country.region,	country.sub.region,	country.intermediate.region, 
          country.nuclear.weapons, country.nato.member.2024, 
          country.population.2018, country.land.area.sq.km,
          mean.yield.corn, mean.yield.rice, mean.yield.wheat, mean.yield.soya.beans,
          soot.injection.scenario, 
          years.elapsed,
          crop, 
          pct.change.harvest.yield
        ) %>% 
        as_tibble #ensure final result is a tibble
      
      print(source_table_names)
      
      return(result)

    }
    
    agriculture.clm.clean.tb <- 
      Map(
        CleanReshape_AgricultureCLM,
        agriculture.clm.ls,
        names(agriculture.clm.ls)
      ) %>%
      do.call(rbind, .) %>%
      mutate(
        crop = case_when(
          crop == "grass" ~ "livestock.pasture.grass", 
          crop == "swheat" ~ "spring.wheat",
          TRUE ~ crop
        )
      ) %>%
      pivot_wider(
        names_from = crop,
        values_from = pct.change.harvest.yield,
        names_glue = "pct.change.harvest.yield.{crop}"
      ) %>%
      FlagOutliers_IQR(source.table.list.name = agriculture.clm.ls) %>% 
      as_tibble()
    
    # agriculture.clm.clean.tb %>% 
    #   select(names(.)[!grepl("pct.change.harvest.yield|2015", names(.))]) %>% 
    #   apply(., 2, TableWithNA) #display unique values for each variable except the indicator (for checking)
    
  
