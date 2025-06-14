# 5-VISUALIZATION --------------------------------------------------------------------------------

  #ONE-OFF GEOSPATIAL COUNTRY-LEVEL HEATMAP

    #Parameters 
    selected_years <- c(5)
    selected_scenarios <- c(150)
    color_low <- "#d73027"      # Color for lowest yield change (e.g., red)
    color_high <- "#bce0b3"     # Color for highest yield change (e.g., green)
    crop_variable <- "pct.change.harvest.yield"  # Variable to visualize
    
    plot_data <- agriculture.clm.clean.tb
    crop <- "corn"

  # Clean and filter data
  plot_data <- agriculture.clm.clean.tb %>%
    filter(
      years.elapsed %in% selected_years,
      soot.injection.scenario %in% selected_scenarios,
      crop == crop,
      !is.na(country.iso3),
      !is.na(!!sym(crop_variable))
    ) %>%
    select(
      country.iso3, years.elapsed, soot.injection.scenario,
      !!sym(crop_variable)
    ) %>%
    distinct()

  # Load base world map
  world <- ne_countries(scale = "medium", returnclass = "sf")

  # Join + reduce plot_df to minimal set
  plot_df <- world %>%
    left_join(plot_data, by = c("iso_a3" = "country.iso3")) %>%
    select(iso_a3, name, !!sym(crop_variable), geometry)

  # Ensure variable is numeric (just in case)
  plot_df[[crop_variable]] <- as.numeric(plot_df[[crop_variable]])

  # Check: sample joined values
  print(head(plot_df, 10))

  # Title
  title_text <- sprintf(
    "%% Change in Crop Yields by Country, %sTg Scenario, Year%s %s",
    paste(unique(plot_data$soot.injection.scenario), collapse = ", "),
    if (length(selected_years) > 1) "s" else "",
    paste(unique(plot_data$years.elapsed), collapse = ", ")
  )

  # Plot
  ggplot(plot_df) +
    geom_sf(aes(fill = pct.change.harvest.yield), color = "gray70", size = 0.1) +
    scale_fill_gradient2(
      low = color_low,
      high = color_high,
      mid = "#ffe367",
      midpoint = 0,
      na.value = "lightgray",
      name = "% Change"
    ) +
    labs(title = title_text) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      plot.title = element_text(size = 16, face = "bold")
    )