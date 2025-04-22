#' plot_map
#'
#' @import ggplot2
#' @import dplyr
#' @import grDevices
#'
#' @description Plots a choropleth map of case incidence 
#' @details Longitude is represented on the x-axis, latitude on the y axis, 
#' and the case incidence in each spatial unit is represented by a distinct color.
#' The option to plot case counts without the population denominator is not given, 
#' as case counts on a map would be a representation of the underlying population 
#' @param data an object of class "data.frame" containing equally spaced
#' (daily, weekly, monthly) observations for one or multiple locations.
#' @param var the name of the column identifying the variable of interest.
##' @param type character that specifies the type of variable in `var`. 
#' Possible values include 'cov' (covariate, default), 'counts' (case counts), 
#' and 'inc' (case incidence). If `type='inc'`, `pop` is required.
#' @param time the name of the variable that identifies the temporal dimension
#' of the data.frame. The values must be in date format ("yyyy-mm-dd")
#' representing the day of observation for daily data, the first day of the 
#' week for weekly, or the first day of the month for monthly observations.
#' @param pop the name of the variable that identifies the population.
#' @param pt the scale of the person-time (default 100,000) for incidence rates.
#' @param area the name of the variable that identifies the different locations
#' (e.g., areal units).
#' @param map the name of the sf object corresponding to the spatial unit 
#' specified in 'area'.
#' @param map_area the name of the variable that identifies the different locations
#' (e.g., areal units) in the map object. If not specified, it assumes the same
#' name as 'area'.
#' @param by_year logical, if TRUE a map for each year is produced.  
#' @param aggregate_time_fun character indicating the function to be performed in 
#' the temporal aggregation ("mean","median"), default is "mean".
#' @param log logical, default FALSE. Log-transform the variable of interest.
#' @param title title of the plot. 
#' Default is "'var'" or "'var_label'" if var_label is specified
#' @param var_label character with a custom name for the case or covariate variable
#' @param palette GHR, RColorBrewer or viridisLite palette. See all available 
#' options by running `GHR_palettes()`, `RColorBrewer::display.brewer.all()` 
#' and visiting `https://search.r-project.org/CRAN/refmans/viridisLite/html/viridis.html`
#' @param centering numerical or "median", defaults to NULL. If set, 
#' it centers the palette on that value. 
#' @param bin a vector of percentiles to bin the variable of interest, e.g.
#' `seq(0, 1, 0.25)` for quartiles. Defaults to NULL (no binning).
#' @return a map representing the incidence rates.
#' @export
#'
#'
#' @examples
#' # Load data
#' library("sf")
#' data("dengue_MS")
#' data("map_MS")
#' 
#' # Temporal average of a covariate
#' plot_map(data = dengue_MS, 
#'          var = "tmin",    
#'          type = "cov",
#'          time = "date",       
#'          area = "micro_code",  
#'          map = map_MS,         
#'          map_area = "code",   
#'          aggregate_time_fun = "mean",         
#'          palette ="Reds",
#'          by_year = FALSE,
#'          var_label= "Min Temp.")
#' 
#' # Categorical covariate
#' plot_map(data = dengue_MS, 
#'          var = "biome_name",        
#'          time = "date",      
#'          area = "micro_code", 
#'          map = map_MS,       
#'          map_area = "code",  
#'          palette ="viridis",
#'          by_year = FALSE,
#'          var_label= "Biome")
#' 
#' # Case counts by year
#' dengue_MS %>% 
#'   plot_map(var = "dengue_cases",         # Variable to be plotted 
#'            type = "counts",
#'            time = "date",        # Variable defining the date "dd-mm-yyyy"
#'            area = "micro_code",  # Variable defining area in the dataframe
#'            pop = "population",
#'            map = map_MS,         # the sf object for the map 
#'            map_area = "code",
#'            palette = "Reds")
#' 
#' # Case incidence (for 1,000 persons) by year
#' plot_map(dengue_MS,
#'          var = "dengue_cases",         # Variable to be plotted 
#'          type = "inc",
#'          time = "date",        # Variable defining the date "dd-mm-yyyy"
#'          area = "micro_code",  # Variable defining area in the dataframe
#'          pop = "population",
#'          pt = 1000,
#'          map = map_MS,         # the sf object for the map 
#'          map_area = "code",   
#'          palette = "viridis")

plot_map <- function(data,
                     var = NULL,
                     type = "cov",
                     time = NULL,
                     pop = NULL,
                     pt = 100000,
                     area = NULL,
                     map = NULL,
                     map_area = NULL,
                     by_year = TRUE, 
                     aggregate_time_fun = "mean",
                     log = FALSE,
                     title = NULL,
                     var_label = NULL,
                     palette = "IDExtremes",
                     centering = NULL,
                     bin = NULL){
  
  # Check data exists and is a data.frame
  if (missing(data)) {
    stop("Error: Missing required argument 'data'")
  } else if (!is.data.frame(data)) {
    stop("'data' should be a data.frame")
  }
  
  # Check if 'var' exists in data
  if (missing(var)) {
    stop("Error: Missing required argument 'var'")
  } else if (!is.null(var) && is.null(data[[var]])) {
    stop("No column of the data matches the 'var' argument")
  }
  
  # Check time exists, is in the data.frame and is in date format
  if (missing(time)) {
    stop("Error: Missing required argument 'time'")
  } else if (is.null(data[[time]])) {
    stop("'time' not found in the data")
  } else if(any(is.na(ymd_strict(data[[time]])))){
    stop("'Date' should be in 'yyyy-mm-dd' format")
  }
  
  # Check that 'type' is valid
  if (!type %in% c("cov","counts", "inc")) {
    stop("type must be either 'cov', 'counts' or 'inc'")
  }
  
  # Check if 'area' exists in data
  if (!is.null(area) && is.null(data[[area]])) {
    stop("No column of the data matches the 'area' argument")
  }
  
  # Check that 'aggregate_time_fun is one of the following functions
  # (sum , mean , median) if specified.
  if (!aggregate_time_fun %in% c(
    "sum",
    "mean",
    "median"
  )) {
    stop("aggregate_time_fun can be 'sum', 'mean', 'median'")
  }
  
  # Check for missing values in 'time' if specified
  if (!is.null(time)) {
    check_na(time, data)
  }
  
  # Check for missing values in 'area' if specified
  if (!is.null(area)) {
    check_na(area, data)
  }
  
  # Check data exists and is a data.frame
  if (missing(map)) {
    stop("Missing required argument 'map'")
  } 
  if (is.null(map_area)) {map_area <- area}
  
  # Check bins 
  if(!is.null(bin)){
    if(!all(c(0,1) %in% bin)){
      warning("Please include 0 and 1 in the bins to include all data.")
    }
  }
  
  # Connect data to sf object
  
  # Check for mismatches in keys before merging
  # Ensure the keys are character vectors and trim whitespace
  map_keys <- trimws(as.character(unique(map[[map_area]])))
  data_keys <- trimws(as.character(unique(data[[area]])))
  
  # Check for mismatches after normalization
  missing_in_data <- setdiff(map_keys, data_keys)
  missing_in_map <- setdiff(data_keys, map_keys)
  
  if (length(missing_in_data) > 0 || length(missing_in_map) > 0) {
    warning("Some values do not match between shapefile and data:")
    if (length(missing_in_data) > 0) {
      warning("Values in 'map' not found in 'data': ", paste(missing_in_data, collapse = ", "))
    }
    if (length(missing_in_map) > 0) {
      warning("Values in 'data' not found in 'map': ", paste(missing_in_map, collapse = ", "))
    }
  }
  
  # Check for missing values in 'time' if specified
  if (!is.null(time)) {
    check_na(time, data)
  }
  
  # Check for missing values in 'area' if specified
  if (!is.null(area)) {
    check_na(area, data)
  }
  
  
  ### OPTION 1. If the variable to be plotted is a covariate ----
  if (type=="cov" && !is.null(var)) {
    
    # Check if var is a valid column
    if (is.null(data[[var]])) {
      stop(" 'var' not found in the data.")
    }
    
    # Check that var is numeric or a factor
    if (!class(data[[var]]) %in% c("numeric", "factor", "character")) {
      stop("'var' should be numeric or a factor.")
    }
    
    
    # Check for missing values in 'var' if specified
    if (!is.null(var)) {
      check_na(var, data)
    }
    
    ##### Factor ----
    if (class(data[[var]]) %in% c("factor", "character")) {
      
      data[[var]] <- as.factor(data[[var]])
      
      # Unique in the entire dataset
      if(!isTRUE(by_year)){
        
        # Check uniques 
        data_unique <- data 
        data_unique$area <- data[[area]]
        data_unique$var <- data[[var]]
        data_unique <- data_unique |> 
          dplyr::group_by(area)  %>%
          dplyr::summarise(n_unique = n_distinct(var), .groups="keep")  |> 
          dplyr::ungroup()
        
        if(any(data_unique$n_unique > 1)){
          stop(paste0("plot_map does not accept time-varying categorical covariates. ",
                      "You may want to try setting by_year=TRUE"))
        }else{
          
          # Get uniques
          data_unique <- data 
          data_unique$area <- data[[area]]
          data_unique$var <- data[[var]]
          data_unique <- data_unique |> 
            dplyr::group_by(area)  %>%
            dplyr::filter(!duplicated(var)) |> 
            dplyr::ungroup()
          
          # Add geometries with yearly expansion
          map_data <- merge(map, data_unique, 
                            by.x = map_area, by.y = area,
                            all.x = TRUE)
        }
          
      }else if(isTRUE(by_year)){
        
        # Check uniques per year
        data_unique <- data 
        data_unique$area <- data[[area]]
        data_unique$year <- as.numeric(format(data[[time]], "%Y"))
        data_unique$var <- data[[var]]
        data_unique <- data_unique |> 
          dplyr::group_by(area, .data$year)  %>%
          dplyr::summarise(n_unique = n_distinct(var), .groups="keep")  |> 
          dplyr::ungroup()
        
        if(any(data_unique$n_unique > 1)){
          stop(paste0("plot_map does not accept time-varying categorical covariates."))
          
        }else{
          
          # Get uniques per year
          data_unique <- data 
          data_unique$area <- data[[area]]
          data_unique$year <- as.numeric(format(data[[time]], "%Y"))
          data_unique$var <- data[[var]]
          data_unique <- data_unique |> 
            dplyr::group_by(area, .data$year)  %>%
            dplyr::filter(!duplicated(var)) |> 
            dplyr::ungroup()
          
          # Add geometries with yearly expansion
          map_expanded <- map |>
            dplyr::rowwise() |>
            dplyr::mutate(year = list(unique(data_unique$year))) |> 
            tidyr::unnest(.data$year)  
          map_data <- merge(
            map_expanded, data_unique,
            by.x = c(map_area, "year"), by.y = c("area", "year"), 
            all.x = TRUE)
          
        }
      }
      
      # Create plot title 
      if (is.null(title)){
        if (is.null (var_label)){
          title<- var} else {
            title<- var_label
          }
      } else title
      
      # Create legend label 
      if (is.null (var_label)){
        legend<- var} else {
          legend<- var_label
        }
      
      # palette
      level_names <- unique(map_data$var)
      level_pals <- GHR_palette(palette)(n = length(level_names))
      names(level_pals) <- level_names
      
      # Plot
      p <- ggplot2::ggplot(map_data) +
        ggplot2::geom_sf(ggplot2::aes(fill = var)) + 
        scale_fill_manual(values = level_pals) +
        ggplot2::ggtitle(title) +
        ggplot2::labs(fill = legend) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "right")+ 
        ggplot2::theme(
          axis.title = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::labs(fill = legend) 
      
      if(isTRUE(by_year)){
        p <- p + ggplot2::facet_wrap(~.data$year)
      }
      
      
      return(p)
    }

    
    ### Numeric ----
    
    # If by_year = TRUE, add a 'year' column to the data
    if (by_year == TRUE) {
      data_agg <- data |>
        aggregate_cov(var = var,
                      time = time,
                      area = area,
                      aggregate_time = "year",
                      aggregate_time_fun = aggregate_time_fun)
    } else {
      data_agg <- data |>
        dplyr::group_by(!!rlang::sym(area)) |>
        dplyr::summarise(var = match.fun(aggregate_time_fun)(!!rlang::sym(var), na.rm = TRUE),
                         .groups = "drop")
    }
    
    # Log-transform if required
    if (isTRUE(log)) {
      data_agg$var <- log(data_agg$var) 
    }
    
    # Expand the map if by_year = TRUE
    if (by_year == TRUE) {
      # Extract the unique years from the aggregated data
      all_years <- unique (data_agg$time)
      
      # Create all combinations of map rows and the years we have in data
      # NOTE: This effectively replicates each row of 'map' for every possible year
      map_expanded <- map |>
        dplyr::rowwise() |>
        dplyr::mutate(year = list(all_years)) |>   # Attach the list of years to each row
        tidyr::unnest(.data$year)  
      
      # Merge expanded map with aggregated data
      map_data <- merge(
        map_expanded,
        data_agg,
        by.x = c(map_area, "year"), by.y = c("area", "time"), 
        all.x = TRUE
      )
      
    } else {
      
      # If not by_year, just merge by area
      map_data <- merge(
        map,
        data_agg,
        by.x = map_area,
        by.y = area,
        all.x = TRUE
      )
    }  
    
    
    if (!is.null(bin)) {
      map_data <- map_data |>
        dplyr::mutate(var = cut(var, stats::quantile(var, probs = bin, na.rm = TRUE)), 
                      var = as.factor(var))
    } else {
      # Retrieve limits of the variables for the palette definition
      limit <- map_data |>
        dplyr::summarise(
          min = min(var, na.rm = TRUE),
          med = stats::median(var, na.rm = TRUE),
          max = max(var, na.rm = TRUE)
        ) |>
        unlist()
      
      if (!is.null(centering) &&  centering == "median") {
        centering <- limit[["med"]]
      }
    }
    
    # Create plot title 
    if (is.null(title)){
      if (is.null (var_label)){
        title<- var} else {
          title<- var_label
        }
    } else title
    
    # Create legend label 
    if (is.null (var_label)){
      legend<- var} else {
        legend<- var_label
      }
    
    # Create the plot
    p <- ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = var)) + 
      ggplot2::ggtitle(title) +
      ggplot2::labs(fill = legend) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "right")+ 
      ggplot2::theme(
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::labs(fill = legend) 
    
    # Customize color gradient 
    ## if variable is binned by quartiles, only 4 colors are needed 
    if (!is.null(bin)) {
      my_palette <- GHR_palette(palette)(n = length(bin)-1)
      
      p <- p +
        ggplot2:: scale_fill_manual(values = my_palette) +
        ggplot2::labs(fill = paste0(toupper(substring(var, 1, 1)), substring(var, 2)))
      
    } else {  
      # ggplot2::scale_fill_gradientn allows for smooth interpolation across multiple
      # colors in a defined sequence between normalized values between 0 and 1. 
      # here we extract 3 colors from the selected palette that will be assigned to the
      # low, middle, high normalized values
      my_palette <- GHR_palette(palette)(n = 3)
      # normalized the selected midpoint on a 0-1 scale 
      rescaled_center <- (centering - limit[["min"]])/(limit[["max"]] - limit[["min"]])
      p <- p +
        ggplot2::scale_fill_gradientn(
          colors = my_palette,
          values = c(0, rescaled_center, 1)  # Set explicit positions for each color
        ) 
    }
    
    if (by_year == TRUE) {
      p <-p + facet_wrap(~ .data$year)
    }
    
    return(p)
    
  }
  
  
  # OPTION 2. If the variable to be plotted is counts or incidence ----
  if (type %in% c("counts","inc") && !is.null(var)) {
    
    # Check that var is numeric
    if (!is.numeric(data[[var]])) {
      stop("'var' should be numeric")
    }
    
    # Check for missing values 
    if (!is.null(var)) {
      check_na(var, data)
    }
    
    # Check for missing values in 'pop' for inc
    if(type=="inc"){       
      if (!is.null(pop)) {
        check_na(pop, data)
      }
    
    # Check type and fill pop to NA if not provided
    }else if (type=="counts") {    
      pop <- "pop"
      data$pop <- rep(NA, length(data[[var]]))
    }
    
    # If by_year = TRUE, add a 'year' column to the data
    if (isTRUE(by_year)) {
      
      data_agg <- data |>
        aggregate_cases(
          cases = var,
          pop = pop,
          time = time,
          area = area,
          pt = pt,
          aggregate_time = "year")
      
    } else {
      # Aggregate cases and pop to year within each area
      data_agg <- data |>
        dplyr::select( time = {{ time }},
                       area = {{area}},
                       cases = {{ var }},
                       pop = {{ pop }}) |>
        dplyr::group_by(area, time) |>
        dplyr::summarise(cases = sum(.data$cases, na.rm = TRUE),
                         pop = mean(pop, na.rm = TRUE),
                         .groups = "drop")
      
      # If not by_year, just merge by area
      # Aggregate the cases and pop across all years within each area
      data_agg <- data_agg |>
        dplyr::group_by(area) |>
        dplyr::summarise(cases = sum(.data$cases, na.rm = TRUE),
                         pop = sum(pop, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::mutate (inc = (.data$cases/pop)*pt)
    }
    
    # Customize the data depending on count or incidence
    if (type == "inc") {
      data_agg$inc <- (data_agg$cases / data_agg$pop) * pt
    } else if (type == "counts") {
      data_agg$inc <- data_agg$cases
    }
    
    # Log transform if required
    if (type == "inc" && log == TRUE) {
      data_agg$inc <- log(data_agg$cases + 1) - log(data_agg$pop / pt)
    } else if (type == "counts" && log == TRUE) {
      data_agg$inc <- log(data_agg$cases+1)
    }
    
    # Expand the map if by_year = TRUE
    if (isTRUE(by_year)) {
      # Extract the unique years from the aggregated data
      all_years <- unique (data_agg$time)
      
      # Create all combinations of map rows and the years we have in data
      # NOTE: This effectively replicates each row of 'map' for every possible year
      map_expanded <- map |>
        dplyr::rowwise() |>
        dplyr::mutate(year = list(all_years)) |>   # Attach the list of years to each row
        tidyr::unnest(.data$year)  
      
      # Merge expanded map with aggregated data
      map_data <- merge(
        map_expanded,
        data_agg,
        by.x = c(map_area, "year"), by.y = c("area", "time"), 
        all.x = TRUE
      )
      
    } else {
      
      # Merge aggregated data with map
      map_data <- merge(map, data_agg, by.x = map_area, by.y = "area", all.x = TRUE)
    }  
    
    
    # If discrete color scale
    if (!is.null(bin)) {

      map_data$inc <- cut(map_data$inc, 
                          stats::quantile(map_data$inc, probs = bin, na.rm = TRUE))
      map_data$inc <- as.factor(map_data$inc)

    } else {
      # Retrieve limits of the variables for the palette definition
      limit <- map_data |>
        dplyr::summarise(
          min = min(.data$inc, na.rm = TRUE),
          med = stats::median(.data$inc, na.rm = TRUE),
          max = max(.data$inc, na.rm = TRUE)
        ) |>
        unlist()
      
      if (!is.null(centering) && centering == "median") {
        centering <- limit[["med"]]
      }
    }
    
    # Customize title 
    if (is.null(title)){
      if (is.null (var_label)){
        if(type == "inc"){
          title<- "Case incidence"
        } else if (type=="counts"){
          title<- "Case counts"
        }
      }
      else{
        title<- var_label
      }
    } else title 
    
    
    # Customize legend label  
    if (is.null(title)){
      if (is.null (var_label)){
        if(type == "inc"){
          title<- "Case incidence"
        } else if (type=="counts"){
          title<- "Case counts"
        }
      } else{
        title<- var_label
      }
    } else title 
    
    # Customize legend label 
    if (is.null (var_label)){
      if(type == "inc"){
        legend <- "Incidence"
      } else if (type=="counts"){
        legend <- "Counts"
      }
    } else{
      legend<- var_label
    }
    
    
    # Create the plot
    p <- ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data$inc)) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "right")+ 
      ggplot2::ggtitle(title) +
      ggplot2::labs(fill = legend) +
      ggplot2::theme(
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5))
    
    # Customize color gradient 
    ## if variable is binned by quartiles, only 4 colors are needed 
    if (!is.null(bin)) {
      my_palette <- GHR_palette(palette)(n = length(bin)-1)
      
      p <- p +
        ggplot2:: scale_fill_manual(values = my_palette) +
        ggplot2::labs(fill = paste0(toupper(substring(var, 1, 1)), substring(var, 2)))
      
    } else if(!is.null(centering)) {  
      
      my_palette <- GHR_palette(palette)(n = 3)
      # normalized the selected midpoint on a 0-1 scale 
      rescaled_center <- (centering - limit[["min"]])/(limit[["max"]] - limit[["min"]])
      p <- p +
        ggplot2::scale_fill_gradientn(
          colors = my_palette,
          values = c(0, rescaled_center, 1)  # Set explicit positions for each color
        ) 
    }else if(is.null(centering)){
      
      my_palette <- GHR_palette(palette)(n = 10)
      p <- p + ggplot2::scale_fill_gradientn(colors = my_palette)
      
    }
    
    if(type=="inc"){
      p <- p +  ggplot2::labs(
        subtitle = paste("Incidence Rate per",
                         format(pt, big.mark = ",",scientific = FALSE),
                         "person-year"))
    }

    if (by_year == TRUE) {
      p <-p + facet_wrap(~ .data$year)
    }
    
    return(p)
    
  }
}



