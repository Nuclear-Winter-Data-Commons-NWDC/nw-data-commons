##############################################################################
####                     2024-11 NW Data Commons                          ####
##############################################################################

# 0-SETUP ----------------------------------------------------------------------

  # INITIAL SETUP
  rm(list=ls()) #Remove lists
  gc()
  options(java.parameters = "- Xmx8g") #helps R not to fail when importing large xlsx files

  # ESTABLISH BASE DIRECTORIES
  wd <- "/home/wnf/code/nw-data-commons"
  setwd(wd) # Set working directory

  source.tables.dir <- file.path(wd, "1-source-data_nc-files") # Use file.path for platform independence

  # Check if the source directory exists
  if (!dir.exists(source.tables.dir)){
    stop(paste("Error: source.tables.dir does not exist at:", source.tables.dir))
  } else {
    print(paste("Source data directory found at:", source.tables.dir))
  }

  # LOAD LIBRARIES/PACKAGES
  library(wnf.utils)
  LoadCommonPackages() # Assuming this loads a set of commonly used packages
  library(googledrive)
  try(drive_auth(email = "william@fluxrme.com"), silent = TRUE) # Authenticate Google Drive, wrap in try in case it fails in some environments
  library(purrr)
  library(ncdf4)
  library(janitor)
  library(lubridate)
  library(openxlsx)
  library(dplyr)  # Load dplyr for data manipulation
  library(tidyr)  # Load tidyr for reshaping data
  library(sf)     # Load sf for geospatial operations (you'll need this later)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(viridis)
  library(raster)
  library(shiny)

# 1-CONFIGURATION ----------------------------------------------------------------------

  # Functions to inspect .nc files to help with updating configs
  inspect_nc_file <- function(file_path) {
    # Load ncdf4 in case it isn't already
    if (!requireNamespace("ncdf4", quietly = TRUE)) {
      stop("Package 'ncdf4' is required but not installed.")
    }
    
    nc <- ncdf4::nc_open(file_path)
    on.exit(ncdf4::nc_close(nc))  # Ensure we close the file on function exit
    
    cat("============================================================\n")
    cat("INSPECTING NETCDF FILE:\n")
    cat("  ", basename(file_path), "\n")
    cat("============================================================\n\n")
    
    # 1. DIMENSIONS
    cat("DIMENSIONS:\n")
    dim_names <- names(nc$dim)
    if (length(dim_names) == 0) {
      cat("  [No dimensions found]\n\n")
    } else {
      for (dn in dim_names) {
        dim_info <- nc$dim[[dn]]
        cat("  -", dn, ": length =", dim_info$len, "\n")
      }
      cat("\n")
    }
    
    # 2. VARIABLES
    cat("VARIABLES:\n")
    var_names <- names(nc$var)
    if (length(var_names) == 0) {
      cat("  [No variables found]\n")
    } else {
      for (vn in var_names) {
        var_info <- nc$var[[vn]]
        # Each variable's 'dim' is a list of dimension objects (each with name, length, etc.)
        dim_str <- paste(sapply(var_info$dim, function(d) d$name), collapse = ", ")
        cat("  -", vn, "(dimensions:", dim_str, ")\n")
      }
    }
    
    cat("\n------------------------------------------------------------\n\n")
  }

  inspect_nc_directory <- function(dir_path) {
    files <- list.files(dir_path, pattern = "\\.nc$", full.names = TRUE)
    if (length(files) == 0) {
      cat("No .nc files found in:", dir_path, "\n")
      return(invisible(NULL))
    }
    
    for (f in files) {
      inspect_nc_file(f)
    }
  }

  # Define the structure of your model outputs. Each entry now points to a directory.
  configs.ls <- list(
    #"temp_precip" = list(
    #  directory = file.path(source.tables.dir, "1-2-temp-precip"),
    #  file_pattern = ".*\\.nc$", # Matches any file ending with .nc
    #  variables = c("TS", "TSMN", "TSMX", "PRECC", "PRECL"),
    #  time_var = "time",
    #  lat_var = "lat",
    #  lon_var = "lon",
    #  time_bnds_var = "time_bnds"  # <-- Add this if your .nc files have time boundaries
    #),
    "uv" = list(
      directory = file.path(source.tables.dir, "3-uv"),
      file_pattern = ".*\\.nc$",
      variables = c("TUV_UVA", "TUV_UVB", "TUV_UVC", "TUV_UVINDEX", "TUV_UVINDEXMX"),
      time_var = "time",
      lat_var = "lat",
      lon_var = "lon"
      # no time_bnds_var here because UV files might not have it
    )
  )

# 2-IMPORT ----------------------------------------------------------------------

  # (1) Helper function to flatten a single variable, respecting netCDF dimension order:
  flatten_variable <- function(nc_file, var_name, config) {
    # var_name must exist
    if (! (var_name %in% names(nc_file$var))) {
      stop("Variable '", var_name, "' not found in netCDF file.")
    }
    
    # 1. Grab dimension metadata for the variable
    var_info  <- nc_file$var[[var_name]]
    dims_info <- var_info$dim    # list of dimension objects, in netCDF’s declared order
    if (length(dims_info) != 3) {
      stop("Variable '", var_name, "' does not have exactly 3 dims (lon, lat, time). Found: ", length(dims_info))
    }
    
    # 2. We expect dims in the order: (lon, lat, time).
    #    If your netCDF has them in a different order, either reorder or fail.
    #    Here we strictly check the dimension names match config$lon_var, etc., in sequence.
    dim_names <- sapply(dims_info, `[[`, "name")
    expected_names <- c(config$lon_var, config$lat_var, config$time_var)
    
    if (!all(dim_names == expected_names)) {
      stop("Variable '", var_name, "' dims are ", paste(dim_names, collapse=", "),
          ", but expected (", paste(expected_names, collapse=", "), ").")
    }
    
    # 3. For each dimension, get the .vals
    #    Stop if they’re missing or empty.
    lon_dim <- nc_file$dim[[config$lon_var]]
    lat_dim <- nc_file$dim[[config$lat_var]]
    time_dim <- nc_file$dim[[config$time_var]]
    
    if (is.null(lon_dim$vals) || is.null(lat_dim$vals) || is.null(time_dim$vals)) {
      stop("One or more dimension variables (lon, lat, time) have no '$vals' in netCDF file.")
    }
    
    lon_vals <- lon_dim$vals
    lat_vals <- lat_dim$vals
    time_vals <- time_dim$vals
    
    # 4. Read the actual variable data, expecting shape = c(length(lon), length(lat), length(time))
    var_data <- ncdf4::ncvar_get(nc_file, var_name, collapse_degen = FALSE)
    dim_var <- dim(var_data)
    expected_shape <- c(length(lon_vals), length(lat_vals), length(time_vals))
    
    if (!identical(dim_var, expected_shape)) {
      stop("Variable '", var_name, "' has shape ", paste(dim_var, collapse="x"),
          ", expected ", paste(expected_shape, collapse="x"), ".")
    }
    
    # 5. Build expand.grid(...) in the same dimension order (lon, lat, time)
    df_coords <- expand.grid(
      lon = lon_vals,
      lat = lat_vals,
      time = time_vals,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    
    # 6. Flatten var_data using as.vector(...) in column-major order
    df_coords[[var_name]] <- as.vector(var_data)
    
    return(dplyr::as_tibble(df_coords))
  }


  # (2) Generalized function to import & flatten a single .nc file
  import_single_nc <- function(file_path, config) {
    tryCatch({
      message("Processing file: ", file_path)
      nc_file <- ncdf4::nc_open(file_path)
      on.exit(ncdf4::nc_close(nc_file))
      
      # If you want to forcibly check that the file *has* dim named config$lon_var, etc.
      # you can do that here or rely on flatten_variable to throw an error.
      
      # Flatten each target variable & merge
      nc_file.df <- NULL
      for (var_name in config$variables) {
        # We call the new flatten_variable
        var_df <- flatten_variable(nc_file, var_name, config)
        
        if (is.null(nc_file.df)) {
          nc_file.df <- var_df
        } else {
          # Merge on the (lon, lat, time) columns
          dim_cols <- c("lon", "lat", "time")
          nc_file.df <- dplyr::left_join(nc_file.df, var_df, by = dim_cols)
        }
      }
      
      if (is.null(nc_file.df)) {
        warning("No data imported for file: ", basename(file_path))
        return(NULL)
      }
      
      # Store the filename as a variable
      nc_file.df %<>% mutate(file_name = basename(file_path))
      
      return(nc_file.df)
      
    }, error = function(e) {
      warning("Error importing file '", file_path, "': ", e$message)
      return(NULL)
    })
  }


  # (3) Function to import data for a single model output
  import_model_data <- function(model_name, config) {
    print(paste("Importing data for model output:", model_name))
    directory <- config$directory
    file_pattern <- config$file_pattern

    if (!dir.exists(directory)) {
      warning(paste("Directory not found for model output", model_name, "at:", directory))
      return(NULL)
    }

    all_files <- list.files(directory, pattern = file_pattern, full.names = TRUE, recursive = TRUE)
    num_files <- length(all_files)

    if (num_files == 0) {
      warning(paste("No .nc files found for model output", model_name, "in:", directory))
      return(NULL)
    }

    # Process all found .nc files with progress indicator
    model_data_list <- purrr::map2(all_files, seq_len(num_files), function(file_path, index) {
      result <- import_single_nc(file_path, config)
      percentage_complete <- round((index / num_files) * 100, 1)
      message(paste0("  ", percentage_complete, "% (", index, "/", num_files, ") files processed for ", model_name))
      return(result)
    }) %>%
      purrr::compact()

    if (length(model_data_list) > 0) {
      # Combine all files for a single model into one tibble
      combined_data <- dplyr::bind_rows(model_data_list) %>%
        dplyr::mutate(model_output = model_name)
      return(combined_data)
    } else {
      warning(paste("No data imported for model output:", model_name))
      return(NULL)
    }
  }

# 3-FLATTEN NETCDF FILES (Intermediate Product) ----------------------------------------------------------------------

  # Apply the import function to all configured model outputs
  gridded_tables.ls <- 
    purrr::map(names(configs.ls), function(model_name) {
      import_model_data(model_name, configs.ls[[model_name]])
    }) %>%
    rlang::set_names(names(configs.ls)) %>%
    purrr::compact()

# 4-CREATE SIMPLIFIED DATASETS BY GEOGRAPHIC UNIT ----------------------------------------------------------------------

  uv_data <- gridded_tables.ls[["uv"]] %>%
    mutate(lon = ifelse(lon > 180, lon - 360, lon))  # Normalize longitude
  
  # Load world country polygons
  world_map <- ne_countries(scale = "medium", returnclass = "sf")

  # Ensure CRS is WGS84
  world_map <- st_transform(world_map, crs = 4326)

  # Create a numeric index for each country (since rasters don't support character values)
  world_map$country_id <- as.numeric(as.factor(world_map$iso_a3))  # Assigns unique numeric IDs

  # Store the mapping for later reference (to convert back to ISO3)
  iso3_lookup <- world_map %>% dplyr::select(country_id, iso_a3)

  # Define raster resolution (0.1° → ~111km per grid cell)
  raster_res <- 0.1  

  # Create an empty raster covering the world
  world_raster <- raster(ext = extent(world_map), res = raster_res, crs = "+proj=longlat +datum=WGS84")

  # Rasterize world polygons using numeric country ID
  world_raster <- rasterize(world_map, world_raster, field = "country_id")

  # Convert UV data into spatial points
  uv_sp <- SpatialPointsDataFrame(
    coords = uv_data[, c("lon", "lat")], 
    data = uv_data, 
    proj4string = CRS("+proj=longlat +datum=WGS84")
  )

  # Assign each UV point a country ID from the raster
  uv_data$country_id <- extract(world_raster, uv_sp)

  # Remove points with no country ID (ocean points)
  uv_data <- uv_data %>% filter(!is.na(country_id))

  # Convert country_id back to ISO3 using lookup table
  uv_data <- uv_data %>%
    left_join(iso3_lookup, by = "country_id") %>% ############################### FIGURE OUT WHY THIS IS HANGING
    select(-country_id)  # Keep only ISO3 code

  # View the updated dataset
  print(head(uv_data))


 # Drop geometry and select relevant numeric columns dynamically
  numeric_vars <- uv_data %>%
    st_drop_geometry() %>%
    select(where(is.numeric)) %>%
    select(-c(lon, lat, file_name)) %>%  # Exclude unwanted columns
    colnames()

  # Ensure time and model_output are retained
  numeric_vars <- setdiff(numeric_vars, "time")  # Prevent duplicate summarization
  grouping_vars <- c(geo_id_col, "time")  # Group by both geography and time

  # Aggregate data by geography and time
  uv_simplified.tb <- uv_data %>%
    st_drop_geometry() %>%
    select(all_of(grouping_vars), model_output, all_of(numeric_vars)) %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarize(across(all_of(numeric_vars), mean, na.rm = TRUE), .groups = "drop")

  # View final dataset
  print(uv_simplified.tb)


###################################### NEXT STEPS:
  #UPDATE NAMES SO NOT ALL ABOUT UV



  data_subdirectory_name <- "3-uv"

  # Set the directory containing user-provided datasets and masks
  user_data_dir <- file.path(source.tables.dir, data_subdirectory_name)

  # Check for user-provided geographic masks
  user_shapefile_dirs <- list.dirs(user_data_dir, recursive = FALSE)

  # Function to detect the latest received dataset (based on directory naming convention)
  get_latest_user_shapefile <- function() {
    if (length(user_shapefile_dirs) == 0) {
      return(NULL)  # No user-provided datasets
    }
    
    # Extract the latest directory (assuming YYYY-MM-Institution naming format)
    latest_dir <- user_shapefile_dirs[which.max(file.info(user_shapefile_dirs)$mtime)]
    
    # Look for a shapefile in the latest directory
    shapefile_path <- list.files(latest_dir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shapefile_path) > 0) {
      return(shapefile_path[1])  # Return the first found shapefile
    } else {
      return(NULL)  # No shapefile found in the directory
    }
  }

  # Detect latest user-provided shapefile
  user_shapefile <- get_latest_user_shapefile()

  # Load user-provided shapefile or default to Natural Earth countries
  if (!is.null(user_shapefile)) {
    message("Using user-provided geographic mask: ", user_shapefile)
    geo_mask <- st_read(user_shapefile)
    
    # Identify the first non-geometry column to use as the region identifier
    geo_id_col <- names(geo_mask)[1]  # First column assumed to be the unique region ID

  } else {
    message("No user-provided mask found. Using default Natural Earth country boundaries.")
    geo_mask <- ne_countries(scale = "medium", returnclass = "sf")
    geo_id_col <- "iso_a3"  # Standard ISO3 code for countries
  }

  geo_mask <- st_transform(geo_mask, crs = 4326) # Ensure the shapefile is in WGS84 projection
  geo_mask <- st_make_valid(geo_mask) # Ensure valid geometries

  # Extract the UV dataset
  uv_data <- gridded_tables.ls[["uv"]] %>%
    mutate(lon = ifelse(lon > 180, lon - 360, lon))  # Normalize longitude

  # Convert UV data to spatial points
  uv_sf <- st_as_sf(uv_data, coords = c("lon", "lat"), crs = 4326)

  # Perform spatial join to assign each UV point to a geographic unit
  uv_data <- st_join(uv_sf, geo_mask, left = FALSE, largest = TRUE) # Drops unmatched points (e.g., ocean)

  # Check that the geographic identifier column exists after the join
  if (!geo_id_col %in% colnames(uv_data)) {
    stop("Error: Geographic identifier column '", geo_id_col, "' not found in UV dataset after spatial join.")
  }

 

# 5-VIZUALIZE DATA ----------------------------------------------------------------------

# Visualize Gridded Data - Single Time Step (preliminary data check)
  # Define plot configuration
  plot_configs <- list(
    color_palette = c("#035653", "#efe400", "#ef0000"),  # Blue → Yellow → Red
    data_overlay_opacity = 0.3,  # Transparency level for grid overlay
    default_grid_color = "grey80",  # Grid line color
    world_map = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")  
  )

  plot_gridded_data <- function(dataset_name, variable_name, time_index = 1, config = plot_configs) {
    library(ggplot2)
    library(dplyr)
    
    # Ensure dataset exists
    if (!dataset_name %in% names(gridded_tables.ls)) {
      stop("Dataset '", dataset_name, "' not found in gridded_tables.ls")
    }
    
    # Extract dataset and check if variable exists
    dataset <- gridded_tables.ls[[dataset_name]]
    if (!variable_name %in% colnames(dataset)) {
      stop("Variable '", variable_name, "' not found in dataset '", dataset_name, "'")
    }

    # Handle time selection
    unique_times <- unique(dataset$time)
    if (time_index < 1 || time_index > length(unique_times)) {
      stop("Invalid time index: ", time_index, ". Available range: 1 - ", length(unique_times))
    }
    selected_time <- unique_times[time_index]

    # Filter dataset for selected time and normalize longitude
    data_filtered <- dataset %>%
      filter(time == selected_time) %>%
      dplyr::select(lon, lat, all_of(variable_name)) %>%
      mutate(lon = ifelse(lon > 180, lon - 360, lon))  # Normalize longitude

    # Check if data is empty
    if (nrow(data_filtered) == 0) {
      warning("No data available for the selected time step.")
      return(NULL)
    }

    # Dynamic color scale adjustment
    min_val <- min(data_filtered[[variable_name]], na.rm = TRUE)
    max_val <- max(data_filtered[[variable_name]], na.rm = TRUE)
    mean_val <- mean(data_filtered[[variable_name]], na.rm = TRUE)

    # Compute aspect ratio based on longitude & latitude range
    lon_range <- range(data_filtered$lon, na.rm = TRUE)
    lat_range <- range(data_filtered$lat, na.rm = TRUE)
    aspect_ratio <- diff(lat_range) / diff(lon_range)  # Height / Width ratio

    # Define plot
    p <- ggplot() +
      # Base map layer
      geom_sf(data = config$world_map, fill = "white", color = "black", linewidth = 0.3) +
      
      # Overlay: Grid squares with configurable transparency
      geom_tile(
        data = data_filtered, aes(x = lon, y = lat, fill = .data[[variable_name]]), 
        alpha = config$data_overlay_opacity, 
        color = config$default_grid_color, size = 0.1
      ) +

      # Custom Heatmap Scale
      scale_fill_gradientn(
        colors = config$color_palette,
        values = scales::rescale(c(min_val, mean_val, max_val)), 
        na.value = "transparent",
        name = variable_name
      ) +
      
      # Titles and theme adjustments
      labs(title = paste(variable_name, "Heatmap (Time Index:", time_index, ")"), 
          x = "Longitude", y = "Latitude") +
      theme_minimal() +
      theme(
        legend.position = "right",
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )

    # Open a new plotting window (cross-platform support)
    if (.Platform$OS.type == "windows") {
      windows(width = 10, height = 10 * aspect_ratio)  # Adjusted for aspect ratio
    } else {
      x11(width = 10, height = 10 * aspect_ratio)  # For Linux/macOS (including WSL)
    }

    # Print the plot in the new window
    print(p)

    # Optionally save the plot with appropriate dimensions
    ggsave("heatmap_plot.png", plot = p, width = 10, height = 10 * aspect_ratio, dpi = 300)
  }

  #plot_gridded_data(dataset_name = "uv", variable_name = "TUV_UVINDEX", time_index = 150)


# Visualize Gridded Data - Timestep Slider (animated)
  animate_heatmap <- function(dataset_name, config = plot_configs) {
    
    # Ensure dataset exists
    if (!dataset_name %in% names(gridded_tables.ls)) {
      stop("Dataset '", dataset_name, "' not found in gridded_tables.ls")
    }
    
    dataset <- gridded_tables.ls[[dataset_name]]
    
    # Select only numeric variables (remove lon, lat, time, and non-numeric columns)
    numeric_vars <- dataset %>%
      dplyr::select(where(is.numeric)) %>%
      dplyr::select(-c(lon, lat, time)) %>% 
      colnames()
    
    if (length(numeric_vars) == 0) {
      stop("No numeric variables available in dataset '", dataset_name, "' for heatmap visualization.")
    }
    
    # Get unique time steps
    unique_times <- unique(dataset$time)
    
    # Compute **global** min, max, and median for each numeric variable
    global_stats <- 
      dataset %>%
      summarise(across(all_of(numeric_vars), list(
        min = ~min(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      ))) %>%
      pivot_longer(cols = everything(), names_to = "combined_name", values_to = "value") %>%
      separate(combined_name, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%  # Splits at last "_"
      pivot_wider(names_from = "stat", values_from = "value")
    
    # Shiny UI
    ui <- fluidPage(
      
      # Title
      titlePanel(paste("Animated Heatmap of", dataset_name)),
      
      # Dropdown menu for variable selection
      selectInput("variable_name", "Select Variable:", 
                  choices = numeric_vars, selected = numeric_vars[1]),
      
      # Heatmap Output
      plotOutput("heatmapPlot", height = "600px"),
      
      # Slider for Time Step (below heatmap)
      sliderInput("time_index", "Time Step:", 
                  min = 1, max = length(unique_times), value = 1, step = 1,
                  animate = animationOptions(interval = 1000, loop = TRUE))
    )
    
    # Shiny Server
    server <- function(input, output, session) {
      
      # Reactive function to get global min/max/median for the selected variable
      global_color_scale <- reactive({
        global_stats %>% filter(variable == input$variable_name) %>%
          dplyr::select(min, median, max)  # Ensure we only select the relevant columns
      })
      
      # Reactive function to generate heatmap
      plot_reactive <- reactive({
        var_name <- input$variable_name  # Get selected variable
        
        # Extract dataset and filter for the selected time
        data_filtered <- dataset %>%
          filter(time == unique_times[input$time_index]) %>%
          dplyr::select(lon, lat, all_of(var_name)) %>%
          mutate(lon = ifelse(lon > 180, lon - 360, lon))  # Normalize longitude
        
        # Check if data is empty
        if (nrow(data_filtered) == 0) {
          return(NULL)
        }
        
        # Get precomputed global color scale
        color_scale <- global_color_scale()
        
        # Ensure min, median, max are correctly retrieved
        min_val <- color_scale$min
        median_val <- color_scale$median
        max_val <- color_scale$max
        
        # Generate the plot
        ggplot() +
          # Base map layer
          geom_sf(data = config$world_map, fill = "white", color = "black", linewidth = 0.3) +
          
          # Overlay: Grid squares with configurable transparency
          geom_tile(
            data = data_filtered, aes(x = lon, y = lat, fill = .data[[var_name]]), 
            alpha = config$data_overlay_opacity, 
            color = config$default_grid_color, linewidth = 0.1
          ) +
          
          # **Static** Color Scale (consistent across time steps)
          scale_fill_gradientn(
            colors = config$color_palette,
            values = scales::rescale(c(min_val, median_val, max_val)), 
            na.value = "transparent",
            limits = c(min_val, max_val),  # Fixes the legend!
            name = var_name
          ) +
          
          # Titles and theme adjustments
          labs(title = paste(var_name, "Heatmap (Time Index:", input$time_index, ")"), 
              x = "Longitude", y = "Latitude") +
          theme_minimal() +
          theme(
            legend.position = "right",
            legend.key.height = unit(1, "cm"),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
          )
      })
      
      # Render Plot in UI (No extra windows)
      output$heatmapPlot <- renderPlot({
        plot_reactive()
      })
    }
    
    # Launch App in single window
    shinyApp(ui = ui, server = server)
  }

  animate_heatmap(dataset_name = "uv")



  ############### TRYING TO CREATE VISUALIZATION OF COUNTRY-LEVEL DATA #########################################
  #NOTE: current issue seems to be that world_map$adm0_a3 is [like] the iso3 code country abbreviation ("ZWE, VNM") whereas uv_simplified_df only has numerical country indices from 1-253


  # Load world country shapes
  world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Merge simplified UV data with country geometries
  uv_simplified_sf <- world_map %>%
    left_join(uv_simplified.tb, by = c("adm0_a3" = "country_id"))  # Adjust if country_id needs mapping

  # Generate the plot
  ggplot() +
    # Base map layer: Country boundaries
    geom_sf(data = world_map, fill = "white", color = "black", size = 0.3) +

    # Overlay: Fill countries with UV Index values
    geom_sf(data = uv_simplified_sf, aes(fill = TUV_UVINDEX), color = "grey30", size = 0.2) +

    # Custom heatmap color scale
    scale_fill_gradientn(
      colors = c("#035653", "#efe400", "#ef0000"),  # Custom color scale (Low → Medium → High)
      values = scales::rescale(c(min(uv_simplified.tb$TUV_UVINDEX, na.rm = TRUE),
                                  mean(uv_simplified.tb$TUV_UVINDEX, na.rm = TRUE),
                                  max(uv_simplified.tb$TUV_UVINDEX, na.rm = TRUE))),
      na.value = "grey90",  # Grey for missing countries
      name = "UV Index"
    ) +

    # Titles and theme settings
    labs(title = "UV Index by Country",
        subtitle = "Averaged from gridded data",
        x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.key.height = unit(1, "cm"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )






