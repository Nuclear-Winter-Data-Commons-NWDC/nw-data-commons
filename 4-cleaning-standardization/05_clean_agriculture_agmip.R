 #2.4a AGRICULTURE AGMIP (Multi-Model Aggregates, Jonas) ----
    
    #Clean & Reshape FAOSTAT crop indicators 
      fao.crop.indicators.clean.tb <- 
        fao.crop.indicators.tb %>%
        ReplaceNames(., names(.), tolower(names(.))) %>%
        ReplaceNames(., "item", "crop") %>%
        select(country.iso3, crop, year, value) %>%
        mutate(crop = tolower(crop)) %>%
        mutate(
          crop = case_when(
            crop == "maize (corn)" ~ "corn",
            crop == "soya beans" ~ "soya.beans",
            TRUE ~ crop
          )
        ) %>%
        group_by(country.iso3, crop) %>%
        summarise(mean.yield = mean(value, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(
          names_from = crop,
          values_from = mean.yield,
          names_glue = "mean.yield.{crop}"
        )

      # fao.crop.indicators.clean.tb %>% #one-off code to produce pairwise correlation coefficients for yields of crops
      #   select(
      #     mean.yield.corn,
      #     mean.yield.rice,
      #     mean.yield.wheat,
      #     mean.yield.soya.beans
      #   ) %>%
      #   cor(use = "pairwise.complete.obs")


    #Import Agriculture.AGMIP Data
      ImportSourceData_GoogleSheets("4a.agriculture.agmip")

    #Clean & Reshape Agriculture.AGMIP Data
      CleanReshape_AgricultureAGMIP <- function(source_table_list, source_table_names) {
        
        print("Working on cleaning & reshaping:")
        print(source_table_names)

        # Extract components
        split_parts <- strsplit(source_table_names, "_")[[1]]
        cesm.model.configuration <- tolower(split_parts[1])      # "mills" or "bardeen"
        scenario <- split_parts[2]
        crop <- tolower(split_parts[3])              # normalize crop name

        result <- 
          source_table_list %>% 
          ReplaceNames(., names(.), tolower(names(.))) %>%
          select(-country_name, -`...1`) %>%
          ReplaceNames(., "country_iso3", "country.iso3") %>%
          mutate(across(where(is.list), ~ suppressWarnings(as.character(unlist(.))))) %>%
          reshape2::melt(., id = "country.iso3") %>%
          mutate(
            crop = crop,
            cesm.model.configuration = cesm.model.configuration,
            soot.injection.scenario = 5,
            years.elapsed = str_extract(variable, "(?<=_)[^_]*$") %>% as.numeric(),
            pct.change.harvest.yield = value %>% as.numeric() %>% suppressWarnings()
          ) %>%
          left_join(countries.tb, by = "country.iso3") %>%
          left_join(fao.crop.indicators.clean.tb, by = "country.iso3") %>%
          select(
            country.name, country.iso3, country.hemisphere, country.region, country.sub.region, country.intermediate.region,
            country.nuclear.weapons, country.nato.member.2024, country.population.2018, country.land.area.sq.km,
            mean.yield.corn, mean.yield.rice, mean.yield.wheat, mean.yield.soya.beans,
            soot.injection.scenario, years.elapsed,
            cesm.model.configuration, crop, pct.change.harvest.yield
          ) %>% 
          filter(!is.na(pct.change.harvest.yield)) %>%
          as_tibble()

        return(result)
      }

    # Assemble full clean table
    agriculture.agmip.clean.tb <- 
      Map(
        CleanReshape_AgricultureAGMIP,
        agriculture.agmip.ls,
        names(agriculture.agmip.ls)
      ) %>%
      do.call(rbind, .) %>%
      mutate(
        crop = case_when(
          crop == "maize" ~ "corn",
          crop == "soy" ~ "soya.beans",
          TRUE ~ crop
        ),
        cesm.model.configuration = case_when(
          cesm.model.configuration == "bardeen" ~ "toon"
        )
      ) %>%
      pivot_wider(
        names_from = crop,
        values_from = pct.change.harvest.yield,
        names_glue = "pct.change.harvest.yield.{crop}",
        #values_fn = dplyr::first  # avoid list-columns; keeps values as-is
      )

    precipitation.clean.tb %>% as.data.frame %>% .[sample(1:nrow(.), 10),]
    # agriculture.agmip.clean.tb %>% 
    #  select(-names(.)[length(names(.))]) %>% 
    #  apply(., 2, TableWithNA) #display unique values for each variable except the indicator (for checking)
    
 