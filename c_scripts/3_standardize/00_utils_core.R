# CORE UTILITY FUNCTIONS -------------------------------------------------------
# Data manipulation and transformation utilities used across the pipeline

# GENERAL HELPER FUNCTIONS ------------------------------------------------------

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

ReplaceNames <- function(tb, current.names, new.names) {

  # Data Checks
  if(!is.data.frame(tb)){
    stop("Input not a data frame. Input must be of class 'data.frame'.")
  }

  # New Names Checks
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

  # Current Names Checks
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

  } # End of if statement for when current.names not defined by user

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

  # Actual Function: name replacement
  names(tb)[names(tb) %in% current.names] <- new.names
  return(tb)
}

IndexMatchToVectorFromTibble <- function(
  vector,
  lookup.tb,
  match.varname,
  replacement.vals.varname,
  mult.replacements.per.cell = c(FALSE, TRUE),
  mult.replacements.separator.char = NULL,
  print.matches = c(TRUE, FALSE)
){
  if(mult.replacements.per.cell){
    lookup.tb <-
      SplitColReshape.ToLong(
        df = lookup.tb,
        id.varname = replacement.vals.varname,
        split.varname = match.varname,
        split.char = ","
      ) # strsplit(match.col, mult.replacements.separator.char) %>% unlist %>% as.vector
  }

  match.col <- lookup.tb %>% dplyr::select(all_of(match.varname)) %>% dplyr::pull()
  replacement.col <- lookup.tb %>% dplyr::select(all_of(replacement.vals.varname)) %>% dplyr::pull()
  matched.vals.ls <- list()
  unmatched.vals.ls <- list()

  for(i in 1:length(vector)){
    if(is.na(vector[i])){next()} # Skips NAs
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
      paste0("Values replaced: ", .) %>% print
    unmatched.vals.ls %>% unlist %>% as.vector %>% RemoveNA %>% paste(., collapse = ", ") %>%
      paste0("Values not replaced: ", .) %>% print
  }
  return(vector)
}
