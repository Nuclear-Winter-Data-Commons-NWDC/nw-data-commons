# VALIDATION UTILITIES ----------------------------------------------------------
# Outlier detection and data quality checks

library(dplyr)

# General function for flagging outliers using IQR method
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

  # Check if we found a match
  if (nrow(matched.table) == 0) {
    warning("No matching table found in config for the current tibble.")
    return(tb)
  }

  # Check if indicators.of.concern is blank/NA/empty BEFORE filtering
  indicators.str <- matched.table$indicators.of.concern %>% first()
  if (is.null(indicators.str) || is.na(indicators.str) || trimws(indicators.str) == "") {
    object.name.str <- if (!is.null(object.name)) paste0(" for '", object.name, "'") else ""
    warning(paste0(
      "Outlier flagging skipped", object.name.str,
      ": 'indicators.of.concern' is blank in config table. ",
      "No outlier flag variables will be created."
    ))
    return(tb)
  }

  # Further filter by matching indicator variables
  matched.table <- matched.table %>%
    filter(sapply(indicators.of.concern, function(indicators) {
      vars <- strsplit(indicators, ",\\s*")[[1]]
      all(vars %in% colnames(tb))
    }))

  if (nrow(matched.table) == 0) {
    warning("No matching table found in config for the current tibble (indicator columns don't match).")
    return(tb)
  }

  # Get the multiplier
  iqr.multiplier <- matched.table$outlier.iqr.multiplier %>% first()
  if (is.null(iqr.multiplier) || is.na(iqr.multiplier)) {
    iqr.multiplier <- default.multiplier
    warning("IQR multiplier not found in config; using default.")
  }

  # Indicators to evaluate
  indicators <- indicators.str %>%
    strsplit(",\\s*") %>% unlist()

  for (colname in indicators) {
    if (!colname %in% colnames(tb)) next

    flag.col <- paste0(colname, ".outlier.flag")

    # Check if soot.injection.scenario exists for per-scenario IQR calculation
    if ("soot.injection.scenario" %in% colnames(tb)) {
      message(paste("Calculating per-scenario outlier bounds for", colname))

      # Calculate outliers per scenario
      tb <- tb %>%
        group_by(soot.injection.scenario) %>%
        mutate(
          q1 = quantile(.data[[colname]], 0.25, na.rm = TRUE),
          q3 = quantile(.data[[colname]], 0.75, na.rm = TRUE),
          iqr = q3 - q1,
          lower = q1 - !!iqr.multiplier * iqr,
          upper = q3 + !!iqr.multiplier * iqr,
          !!flag.col := case_when(
            is.na(.data[[colname]]) ~ NA_character_,
            .data[[colname]] < lower | .data[[colname]] > upper ~ "outlier",
            TRUE ~ NA_character_
          )
        ) %>%
        ungroup() %>%
        select(-q1, -q3, -iqr, -lower, -upper)

    } else {
      # Fallback to global IQR calculation
      q1 <- quantile(tb[[colname]], 0.25, na.rm = TRUE)
      q3 <- quantile(tb[[colname]], 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - iqr.multiplier * iqr
      upper <- q3 + iqr.multiplier * iqr
      message(paste("Outlier bounds for", colname, ": [", round(lower, 2), ",", round(upper, 2), "]"))

      tb[[flag.col]] <- case_when(
        is.na(tb[[colname]]) ~ NA_character_,
        tb[[colname]] < lower | tb[[colname]] > upper ~ "outlier",
        TRUE ~ NA_character_
      )
    }

    # Check if the flag column would be entirely blank (no outliers found)
    outlier_count <- sum(tb[[flag.col]] == "outlier", na.rm = TRUE)
    if (outlier_count == 0) {
      warning(paste("No outliers found for", colname, "; flag variable", flag.col, "not added."))
      tb <- tb %>% select(-all_of(flag.col))
      next
    }

    # Report outliers found
    message(paste("Found", outlier_count, "outlier(s) for", colname))
  }

  return(tb)
}
