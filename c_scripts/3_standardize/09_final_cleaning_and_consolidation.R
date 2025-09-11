  #2.7 CONSOLIDATE TABLES INTO LIST

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

  #2.8 FINAL CLEANING

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


