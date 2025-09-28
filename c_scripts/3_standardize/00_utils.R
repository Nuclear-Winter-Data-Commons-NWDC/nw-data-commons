# 0-SETUP --------------------------------------------------------------------------------
	
  # INITIAL SETUP
    #rm(list=ls()) #Remove lists
    #gc()
    options(java.parameters = "- Xmx8g") #helps r not to fail when importing large xlsx files with xlsx package

    wd <- "/home/wnf/code/nw-data-commons"
    
  # SECTION & CODE CLOCKING
    
    sections.all.starttime <- Sys.time()
    section0.starttime <- sections.all.starttime
  
  # LOAD LIBRARIES/PACKAGES
    #library(wnf.utils)
    #LoadCommonPackages()
    
    library(googledrive)
    drive_auth(email = "william@fluxrme.com") 
    
    library(googlesheets4) 
    library(tidyverse) 
    library(reshape2) 
    library(data.table)
    library(lubridate)
    library(magrittr)
    library(readxl)
    library(stringr)
    library(tictoc)
    library(dplyr)
    library(ggplot2)
    library(janitor)
    library(lubridate)
    library(ncdf4)
    library(openxlsx)  
    library(purrr)
    library(rnaturalearth)
    library(rnaturalearthdata)
    library(sf)
    library(stringr)
    library(countrycode)
    library(patchwork)
    library(viridis)
    library(grid)
    library(graticule)
    library(units)
    
  # GENERAL HELPER FUNCTIONS
    TableWithNA <- function(vector){
      table(vector, useNA = "always")
    }
    
    ListToTibbleObjects <- function(list){
      for(i in 1:length(list)){
        
        object.name.i <- paste(names(list)[i], ".tb", sep = "")
        
        assign(
          object.name.i,
          list[[i]],
          pos = 1
        )
        
        print(paste(i, ": ", object.name.i, sep = ""))
      }
    }
    
    ReplaceNames <- function(tb,current.names, new.names) {
      
      #Data Checks
      if(!is.data.frame(tb)){
        stop("Input not a data frame. Input must be of class 'data.frame'.")
      }
      
      #New Names Checks
      if(!exists("new.names")){
        new.names <- readline(prompt = "No new names defined. Enter a vector of new names to replace current names: ")
      }
      
      if(!is.character(new.names)){
        new.names <- as.character(new.names)
        warning("'new.names' input not of class 'character.' Coercing to character vector.")
      }
      
      if(!is.character(new.names)){
        new.names <- as.character(new.names)
        warning("'new.names' input not of class 'character.' Coercing to character vector.")
      }
      
      #Current Names Checks
      if(!exists("current.names")){
        
        if(length(names(tb)) == length(new.names)){
          print("No current names to replace specified. All current names will be replaced.")
          current.names <- names(tb)
        }
        
        if(length(names(tb)) != length(new.names)){
          stop(
            paste(
              "No current names to replace specified. Current tb has ",
              length(names(tb)),
              " columns. New names is of length ",
              length(new.names),
              ".",
              sep = ""
            )
          )
        }
        
      } #End of if statement for when current.names not defined by user
      
      if(any(!current.names %in% names(tb))){
        warning(
          paste(
            "One or more current.names were not found in input data frame: '",
            current.names[!current.names %in% names(tb)],
            "'. ",
            sep = ""
          )
        )
      }
      
      #Actual Function: name replacement
      names(tb)[names(tb) %in% current.names] <- new.names
      return(tb)
    }

    IndexMatchToVectorFromTibble <- function(
      vector,
      lookup.tb,
      match.varname,
      replacement.vals.varname,
      mult.replacements.per.cell = c(FALSE,TRUE),
      mult.replacements.separator.char = NULL,
      print.matches = c(TRUE,FALSE)
    ){
      if(mult.replacements.per.cell){
        lookup.tb <-
          SplitColReshape.ToLong(
            df = lookup.tb,
            id.varname = replacement.vals.varname,
            split.varname = match.varname,
            split.char = ","
          ) #strsplit(match.col, mult.replacements.separator.char) %>% unlist %>% as.vector
      }
      
      match.col <- lookup.tb %>% dplyr::select(all_of(match.varname)) %>% dplyr::pull()
      replacement.col <- lookup.tb %>% dplyr::select(all_of(replacement.vals.varname)) %>% dplyr::pull()
      matched.vals.ls <- list()
      unmatched.vals.ls <- list()
      
      for(i in 1:length(vector)){
        if(is.na(vector[i])){next()} #Skips NAs
        if(!any(match.col == vector[i])){
          unmatched.vals.ls[[i]] <- vector[i]
          warning(
            paste("No match for '", vector[i], "' found in column '", match.varname, "'.", sep = "")
          )
        }else{
          matched.vals.ls <- vector[i]
          vector[i] <- replacement.col %>% unlist %>% .[match.col == vector[i]]
        }
      }
      
      if(!missing(print.matches) && print.matches){
        matched.vals.ls %>% unlist %>% as.vector %>% RemoveNA %>% paste(., collapse = ", ") %>%
          paste0("Values replaced: ",.) %>% print
        unmatched.vals.ls %>% unlist %>% as.vector %>% RemoveNA %>% paste(., collapse = ", ") %>%
          paste0("Values not replaced: ",.) %>% print
      }
      return(vector)
    }

  # GENERAL FUNCTIONS FOR ALL TABLES & ONE-OFF PROCEDURES ----

    

    #General function for flagging outliers using IQR method
    FlagOutliers_IQR <- function(
      tb, 
      config.tb = source.table.configs.tb, 
      default.multiplier = 10, 
      source.table.list.name = NULL
    ) {
      print("Looking for matching indicators in the following columns:")
      print(colnames(tb))
      print("Against config table rows:")
      print(config.tb$indicators.of.concern)

      # If source.table.list.name is provided, extract the object name
      object.name <- NULL
      if (!is.null(source.table.list.name)) {
        object.name <- deparse(substitute(source.table.list.name)) %>%
          gsub("\\.ls$", "", .)
        message(paste("Detected object name:", object.name))
      }

      # Try to match config row using object.name
      matched.table <- config.tb
      if (!is.null(object.name)) {
        matched.table <- matched.table %>%
          filter(object.name == !!object.name)
      }

      # Further filter by matching indicator variables
      matched.table <- matched.table %>%
        filter(sapply(indicators.of.concern, function(indicators) {
          vars <- strsplit(indicators, ",\\s*")[[1]]
          all(vars %in% colnames(tb))
        }))

      if (nrow(matched.table) == 0) {
        warning("No matching table found in config for the current tibble.")
        return(tb)
      }

      # Get the multiplier
      iqr.multiplier <- matched.table$outlier.iqr.multiplier %>% first()
      if (is.null(iqr.multiplier) || is.na(iqr.multiplier)) {
        iqr.multiplier <- default.multiplier
        warning("IQR multiplier not found in config; using default.")
      }

      # Indicators to evaluate
      indicators <- matched.table$indicators.of.concern %>% 
        strsplit(",\\s*") %>% unlist()

      for (colname in indicators) {
        if (!colname %in% colnames(tb)) next

        q1 <- quantile(tb[[colname]], 0.25, na.rm = TRUE)
        q3 <- quantile(tb[[colname]], 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower <- q1 - iqr.multiplier * iqr
        upper <- q3 + iqr.multiplier * iqr
        message(paste("Outlier bounds for", colname, ": [", round(lower, 2), ",", round(upper, 2), "]"))

        flag.col <- paste0(colname, ".outlier.flag")

        tb[[flag.col]] <- case_when(
          is.na(tb[[colname]]) ~ "",
          tb[[colname]] < lower | tb[[colname]] > upper ~ "outlier",
          TRUE ~ ""
        )
      }

      return(tb)
    }

    #One-Off Code for fao.crop.indicators table to add iso3 codes & reshape to wider
    # fao.crop.indicators.long.tb <- fao.crop.indicators.long.tb
    # fao.crop.indicators.long.tb$`Area Code (M49)` %<>% as.numeric
    # fao.crop.indicators.long.tb %<>%
    #   mutate(country.iso3 = countrycode(`Area Code (M49)`, origin = "un", destination = "iso3c"))

    # #Filter only the crops you care about (clean names for output)
    # crop_map <- c(
    #   "Maize (corn)" = "corn",
    #   "Rice" = "rice",
    #   "Wheat" = "wheat",
    #   "Sugar cane" = "sugar.cane",
    #   "Soya beans" = "soy"
    # )

    # #Reshape from wide (years as columns) to long format
    # fao.crop.indicators.wide.tb <- fao.crop.indicators.long.tb %>%
    #   filter(Item %in% names(crop_map)) %>%
    #   mutate(
    #     crop = crop_map[Item],                       # rename crop
    #     element = tolower(gsub(" ", ".", Element))   # standardize element names
    #   ) %>%
    #   pivot_longer(cols = c(`2015`), names_to = "year", values_to = "value") %>%
    #   mutate(
    #     variable = paste(crop, element, year, sep = ".")
    #   ) %>%
    #   select(country.iso3, variable, value) %>%
    #   filter(!is.na(country.iso3)) %>%
    #   pivot_wider(names_from = variable, values_from = value)

    # setwd("/home/wnf/code/nw-data-commons/2-outputs")
    # fao.output.filename <- paste0(
    #   "fao.crop.indicators.wide.",
    #   Sys.time() %>% gsub(":",".",.) %>% substr(., 1, nchar(.)-7),
    #   ".csv",
    #   sep=""
    # )
    # write.csv(fao.crop.indicators.wide.tb, fao.output.filename)
