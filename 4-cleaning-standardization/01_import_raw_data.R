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
    
