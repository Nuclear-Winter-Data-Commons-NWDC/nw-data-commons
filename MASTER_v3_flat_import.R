#00000000000000000000000000000000000000000000000000000000000#
#0000       2024-11 NW Data Commons                     0000#
#00000000000000000000000000000000000000000000000000000000000#

# 0-SETUP --------------------------------------------------------------------------------
	
  # INITIAL SETUP
    rm(list=ls()) #Remove lists
    gc()
    options(java.parameters = "- Xmx8g") #helps r not to fail when importing large xlsx files with xlsx package
    
  # SECTION & CODE CLOCKING
    
    sections.all.starttime <- Sys.time()
    section0.starttime <- sections.all.starttime
  
  # ESTABLISH BASE DIRECTORIES
    
    # Set Working Directory and R Project Directory

      wd <- "/home/wnf/code/nw-data-commons"

    #Set Source Tables Directory (raw data, configs, etc.)
      source.tables.dir <- paste0(wd, "/1-source-data")

      if(dir.exists(source.tables.dir)){ 
        print("source.tables.dir exists.")
      }else{
        print("source.tables.dir DOES NOT EXIST.")
      }
      print(source.tables.dir)
  
  # LOAD LIBRARIES/PACKAGES
    library(wnf.utils)
    LoadCommonPackages()
    library(googledrive)
      drive_auth(email = "william@fluxrme.com")
    library(purrr)
    library(ncdf4)
    library(janitor)
    library(lubridate)
    library(openxlsx)
  
  # SECTION CLOCKING
    section0.duration <- Sys.time() - section0.starttime
    section0.duration
    
# 1-IMPORT --------------------------------------------------------------------------------
  
  # IMPORT CONFIG TABLES ---- 
    
    gs4_auth(email = "william@fluxrme.com")
    
    sheet.id = "https://docs.google.com/spreadsheets/d/1M9o6hIX9R8f44-UGea09Z27yhNhK340efd6Udgwrnl8/"
        
    configs.ss <-
      as_sheets_id(sheet.id)
      
    sheet.names.v <- sheet_names(configs.ss)
    
    all.configs.ls <-
      lapply(
        sheet.names.v, 
        function(x){
          read_sheet(configs.ss, sheet = x)
        }
      )
    
    names(all.configs.ls) <- sheet.names.v
    
    #Assign each table to its own tibble object
      ListToTibbleObjects(all.configs.ls) #Converts list elements to separate tibble objects names with their respective sheet names with ".tb" appended
    
    
  # IMPORT SOURCE DATA (defining function) ----
  
    source.data.folder.id <- "1JS013_BF_b_cwfC-kirYzF-Y0yYxf31d"

    import.files.tb <- 
      drive_ls(path = as_id(source.data.folder.id)) %>%
      mutate(mimeType = map_chr(drive_resource, "mimeType")) %>%
      filter(mimeType == "application/vnd.google-apps.spreadsheet") 
      
    ImportSourceData_GoogleSheets <- function(name_of_file_to_be_imported){
        
      file.id <- 
        import.files.tb %>% 
        filter(name == name_of_file_to_be_imported) %>% 
        dplyr::select(id) %>%
        unlist %>% as.vector() %>%
        as_sheets_id(.)
      
      sheet.names <- sheet_names(file.id)
      
      configs <- source.table.configs.tb %>% filter(file.name == name_of_file_to_be_imported)
      
      import.tables.ls <- 
        lapply(
          sheet.names, 
          function(x){
            read_sheet(file.id, sheet = x)
          }
        )
      
      names(import.tables.ls) <- sheet.names #assign sheet names as list element names
      
      list.name <- configs$object.name %>% paste0(., ".ls", collapse = "")
      assign(list.name, import.tables.ls, envir = .GlobalEnv) #create a list of the imported tables in the global environment
      print(list.name)
      
    }
    
# 2-CLEANING & RESHAPING --------------------------------------------------------------------------------

  #1. TEMPERATURE ----
    
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
        melt(id = "country.id") %>%
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
          date = start.date %m+% months(months.elapsed - 1)  # months.elapsed starts at 1
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
        #surface.temp.min = surface.temp.min - 273.15,
        #surface.temp.max = surface.temp.max - 273.15
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
        mutate(
          surface.temp.weighted.by.land.area = surface.temp * country.land.area.sq.km,
          surface.temp.weighted.by.population = surface.temp * country.population.2018
        ) %>%
        dplyr::select( #select & order final variables
          country.name, country.iso3,	country.hemisphere,	
          country.region,	country.sub.region,	country.intermediate.region, 
          country.nuclear.weapons, country.nato.member.2024, 
          country.population.2018, country.land.area.sq.km,
          soot.injection.scenario, 
          years.elapsed, months.elapsed, date, month, season.n.hemisphere, season.s.hemisphere,
          surface.temp, surface.temp.weighted.by.land.area, surface.temp.weighted.by.population
        ) %>%
      as_tibble()

    temperature.clean.tb

  #2. PRECIPITATION ----
    
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
        melt(id = "country.id") %>%
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
      mutate( #converting units from m/s to mm/month
        precip.rate.convective = precip.rate.convective * 1000 * 86400 * 30.4375,
        #precip.rate.stable = precip.rate.stable * 1000 * 86400 * 30.4375,
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
        mutate(
          precip.rate.convective.weighted.by.land.area = precip.rate.convective * country.land.area.sq.km,
          precip.rate.convective.weighted.by.population = precip.rate.convective * country.population.2018
        ) %>%
        dplyr::select( #select & order final variables
          country.name, country.iso3,	country.hemisphere,	
          country.region,	country.sub.region,	country.intermediate.region, 
          country.nuclear.weapons, country.nato.member.2024, 
          country.population.2018, country.land.area.sq.km,
          soot.injection.scenario, 
          years.elapsed, months.elapsed, date, month, season.n.hemisphere, season.s.hemisphere,
          precip.rate.convective
        ) %>%
      as_tibble()

    #precipitation.clean.tb

  #3. UV ----
    
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
        melt(id = "country.id") %>%
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
      as_tibble()
    
    #uv.clean.tb %>%
    #  select(-value) %>%
    #  apply(., 2, TableWithNA)
    
  #4a. AGRICULTURE CLM (Community Land Model) ----
    
    ImportSourceData_GoogleSheets("4a.agriculture.clm")
    
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
        .[2]
      
      result <- 
        source_table_list %>% 
        ReplaceNames(., names(.),tolower(names(.))) %>% #lower-case all table names
        ReplaceNames(., c("nation-id", "nation-name"), c("country.id","country.name")) %>%  #standardize geographic variable names
        select(-id, -country.name) %>%
        melt(., id = "country.id") %>% #reshape to long
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
        select( #select & order final variables
          country.name, country.iso3,	country.hemisphere,	
          country.region,	country.sub.region,	country.intermediate.region, 
          country.nuclear.weapons, country.nato.member.2024, 
          country.population.2018, country.land.area.sq.km,
          soot.injection.scenario, 
          years.elapsed,
          crop, 
          pct.change.harvest.yield
        ) %>% 
        as_tibble #ensure final result is a tibble
      
      print(source_table_names)
      
      return(result)

    }
    
    agriculture.clm.clean.tb <- #create final cleaned & compiled data table
      Map(
        CleanReshape_AgricultureCLM,
        agriculture.clm.ls,
        names(agriculture.clm.ls)
      ) %>%
      do.call(rbind, .) %>%
      as_tibble()
    
    #agriculture.clm.clean.tb %>% 
    #  select(-pct.change.harvest.yield) %>% 
    #  apply(., 2, TableWithNA) #display unique values for each variable except the indicator (for checking)
    
  #4b. AGRICULTURE AGMIP (Multi-Model Aggregates, Jonas) ----
    
    ImportSourceData_GoogleSheets("4b.agriculture.agmip")
    
    CleanReshape_AgricultureAGMIP <- function(source_table_list, source_table_names){
        
      scenario <- 
        source_table_names %>%
        strsplit(., "_") %>% 
        unlist %>%
        .[2]
      
      crop <- 
        source_table_names %>%
        strsplit(., "_") %>% 
        unlist %>%
        .[3]
      
      result <- 
        source_table_list %>% 
        ReplaceNames(., names(.),tolower(names(.))) %>% #lower-case all table names
        select(-country_name, -`...1`) %>%
        ReplaceNames(., "country_iso3", "country.iso3") %>%  #standardize geographic variable names
        mutate(across(where(is.list), ~ suppressWarnings(as.character(unlist(.))))) %>% #convert all list variables into character
        melt(., id = "country.iso3") %>% #reshape to long
        mutate( #add/rename variables
          soot.injection.scenario = 5,
          crop = crop,
          years.elapsed = variable %>% str_extract(., "(?<=_)[^_]*$") %>% as.numeric,
          pct.change.harvest.yield = value %>% as.numeric %>% suppressWarnings()
        ) %>%
        left_join( #add country metadata from configs table
          ., 
          countries.tb,
          by = "country.iso3"
        ) %>%
        select( #select & order final variables
          country.name, country.iso3,	country.hemisphere,	
          country.region,	country.sub.region,	country.intermediate.region, 
          country.nuclear.weapons, country.nato.member.2024, 
          country.population.2018, country.land.area.sq.km,
          soot.injection.scenario,
          years.elapsed, 
          crop, pct.change.harvest.yield
        ) %>% 
        as_tibble #ensure final result is a tibble
      
      print(source_table_names)
      
      return(result)
      
    }
    
    agriculture.agmip.clean.tb <- #create final cleaned & compiled data table
      Map(
        CleanReshape_AgricultureAGMIP,
        agriculture.agmip.ls,
        names(agriculture.agmip.ls)
      ) %>%
      do.call(rbind, .) %>%
      as_tibble()
    
    #agriculture.agmip.clean.tb %>% 
    #  select(-names(.)[length(names(.))]) %>% 
    #  apply(., 2, TableWithNA) #display unique values for each variable except the indicator (for checking)
    
  #5. FISH CATCH ----
    
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
        melt(., id = "eez.num") %>% #reshape to long
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
        dcast(
          ., 
          soot.injection.scenario + eez.num + years.elapsed ~ indicator,
          value.var = "value"
        ) %>%
        as_tibble #ensure final result is a tibble
      
      print(source_table_names)
      
      return(result)
      
    }
    
    iqr.multiplier <- 100  # You can adjust this value

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
      mutate(
        # Flag outliers in mean.pct.catch.change
        mean.pct.catch.change.outlier.flag = {
          q1 <- quantile(mean.pct.catch.change, 0.25, na.rm = TRUE)
          q3 <- quantile(mean.pct.catch.change, 0.75, na.rm = TRUE)
          iqr <- q3 - q1
          lower <- q1 - iqr.multiplier * iqr
          upper <- q3 + iqr.multiplier * iqr
          print(upper)
          print(lower)
          ifelse(
            mean.pct.catch.change < lower | mean.pct.catch.change > upper,
            "outlier",
            ""
          )
        }
      ) %>%
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
        mean.pct.catch.change.outlier.flag
      )

    #fish.catch.clean.tb %>%
    #  select(
    #    eez, eez.num, 
    #    soot.injection.scenario, 
    #    years.elapsed
    #  ) %>%
    #  apply(., 2, TableWithNA)
    
  #6. SEA ICE ----
    
    ImportSourceData_GoogleSheets("6.sea.ice")
    
    CleanReshape_SeaIce <- function(source_table){
    
      scenario <- names(source_table)[1] %>% str_extract(., "(?<=NW-).*")
      
      result <-
        source_table %>%
        .[-1,] %>%
        ReplaceNames(., names(source_table)[1], "port") %>%
        melt(
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
          port, country, container.traffic, lattitude, longitude,
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
      as_tibble()
    
    #sea.ice.clean.tb %>%
    #  select(-sea.ice.thickness.meters) %>%
    #  apply(., 2, TableWithNA)
  
  #CONSOLIDATE TABLES INTRO LIST

    clean_object_names <- 
      source.table.configs.tb$object.name %>%
      sapply(., function(x){paste(x, ".clean.tb", sep="")}) %>%
      as.vector
    
    clean_table_names <- 
      source.table.configs.tb$object.name %>%
      as.vector()
    
    clean.tables.ls <- 
      lapply(
        clean_object_names, 
        function(x) {
          if (exists(x)) get(x) else NULL
        }
      ) %>%
      purrr::compact() # Remove NULL entries for non-existent tibbles
    
    names(clean.tables.ls) <- clean_table_names[clean_object_names %in% ls()]

  #FINAL CLEANING

    # Helper: Drop rows where all indicators of concern are NA
    filter_by_indicators_of_concern <- function(tb, table.name.raw) {
      
      num.rows.initial <- nrow(tb)

      indicators.str <- source.table.configs.tb$indicators.of.concern[
        source.table.configs.tb$object.name == table.name.raw
      ]

      indicators <- indicators.str %>%
        strsplit(",\\s*") %>%
        unlist()

      indicators <- indicators[indicators %in% names(tb)]

      if (length(indicators) == 0) return(tb)

      tb %<>%
        filter(if_any(all_of(indicators), ~ !is.na(.)))

      num.rows.final <- nrow(tb)

      percent.removed <- round(100 * (num.rows.initial - num.rows.final) / num.rows.initial, 1)

      cat(
        "Filtering for rows in '", table.name.raw, "' without data for indicator(s) of concern.\n",
        "Initial number of rows: ", num.rows.initial, "\n",
        "Final number of rows: ", num.rows.final, "\n",
        "Removed ", num.rows.initial - num.rows.final, " rows (", percent.removed, "%)\n\n",
        sep = ""
      )

      return(tb)
    }

    # Apply to each table
    clean.tables.ls <- mapply(
      filter_by_indicators_of_concern,
      clean.tables.ls,
      names(clean.tables.ls),
      SIMPLIFY = FALSE
    )

  #CONSOLIDATE CLIMATE INDICATORS INTO SINGLE TABLE

    # # Get list of table names to include
    # climate.tables <- c("temperature", "precipitation", "uv")

    # # Extract indicators of concern for those tables
    # indicators.of.concern <- 
    #   source.table.configs.tb %>%
    #   filter(object.name %in% climate.tables) %>%
    #   pull(indicators.of.concern) %>%
    #   strsplit(split = ",\\s*") %>%
    #   unlist() %>%
    #   trimws() %>%
    #   unique()

    # # Function to filter columns per table
    # filter_table <- function(tb, table_name) {
    #   relevant.indicators <- 
    #     source.table.configs.tb %>%
    #     filter(object.name == table_name) %>%
    #     pull(indicators.of.concern) %>%
    #     strsplit(split = ",\\s*") %>%
    #     unlist() %>%
    #     trimws()
      
    #   cols.to.keep <- c("country.iso3", "months.elapsed", "years.elapsed", "soot.injection.scenario",relevant.indicators)
    #   tb %>% select(any_of(cols.to.keep))
    # }

    # filtered.tables.ls <- clean.tables.ls[climate.tables] %>%
    #   imap(~filter_table(.x, .y))

    # country.monthly.combined.tb <-
    #   reduce(
    #     filtered.tables.ls,
    #     full_join,
    #     by = c("country.iso3", "months.elapsed", "years.elapsed")
    #   )

    # climate.indicators.tb <- 
    #   country.monthly.combined.tb %>%
    #   left_join(
    #     countries.tb %>% 
    #       select(
    #         country.iso3,
    #         country.name,
    #         country.hemisphere,
    #         country.region,
    #         country.sub.region,
    #         country.intermediate.region,
    #         country.nuclear.weapons,
    #         country.nato.member.2024,
    #         country.population.2018,
    #         country.land.area.sq.km,
    #       ),
    #     by = "country.iso3"
    #   ) %>%
    #   select(-country.iso3) %>%
    #   filter(years.elapsed <= 12)

    #clean.tables.ls[["climate.indicators"]] <- climate.indicators.tb

# 4-EXPORT --------------------------------------------------------------------------------
  
  # DEFINE & CREATE OUTPUT DIRECTORY
    
    output.base.name <- 
      Sys.time() %>% 
      gsub(":",".",.) 
      
    output.dir <-
      paste(
        wd,
        "/2-outputs/",
        output.base.name,
        "/",
        sep = ""
      )
    
    if(output.dir %>% dir.exists %>% not){
      dir.create(output.dir, recursive = TRUE)
    }
    
  # WRITE CSV FILES INTO OUTPUT DIRECTORY
    
    ExportCsvs <- 
      function(table, table_name){
        file.name <- paste(table_name,"_",output.base.name,".csv",sep="")
        write.csv(table, file.name, row.names = FALSE, na = "")
      }
    
    setwd(output.dir)
    Map(
      ExportCsvs,
      clean.tables.ls,
      names(clean.tables.ls)
    )
    
  # WRITE TABLES INTO SINGLE EXCEL FILE IN OUTPUT DIRECTORY
    
    output.file.path <- 
      paste(output.dir, "Reformatted Data_Analysis_",Sys.Date(),".xlsx", sep = "") %>%
      file.path(.)
      
    wb <- createWorkbook()
    
    for (i in seq_along(clean.tables.ls)) {
      sheet_name <- names(clean.tables.ls)[i] %>% gsub("\\.tb","",.)  # Get the name of the list element (if named)
      if (is.null(sheet_name) || sheet_name == "") {
        sheet_name <- paste0("Sheet", i)  # Assign default sheet names if missing
      }
      
      addWorksheet(wb, sheet_name)  # Create a new sheet
      writeData(wb, sheet = sheet_name, clean.tables.ls[[i]])  # Write data
    }
    
    saveWorkbook(wb, output.file.path, overwrite = TRUE)
    
    cat("Excel file successfully saved at:", output.file.path, "\n") # Print confirmation

  # CODE CLOCKING
    code.duration <- Sys.time() - sections.all.starttime
    code.duration
