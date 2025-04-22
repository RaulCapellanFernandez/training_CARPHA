#' plot_timeseries
#'
#' @description Plots time-series of covariates OR cases (case counts or case incidence) over time. 
#' @details case counts are represented as histograms; covariates and case incidence as line-plots, 
#' with values on the y axis, and time is represented on the x axis. 
#' @param data an object of class "data.frame" containing equally spaced
#' (daily, weekly, monthly) covariates or cases for one or multiple locations.
#' @param var the name of the column identifying the variable of interest.
#' @param type character that specifies the type of variable in `var`. 
#' Possible values include 'cov' (covariate, default), 'counts' (case counts), 
#' and 'inc' (case incidence). If `type='inc'`, `pop` is required.
#' @param time the name of the variable that identifies the temporal dimension
#' of the data.frame. The values must be in date format ("yyyy-mm-dd")
#' representing the day of observation for daily data, the first day of the 
#' week for weekly, or the first day of the month for monthly observations.
#' @param pop character identifying the variable name for population. only needed 
#' if `type='inc'`.
#' @param pt numerical only used for `type='inc'`. It represents the scale of the
#' person-time (default 100,000) for incidence rates.
#' @param area the name of variable that identifies the different locations
#' (e.g., areal units). It is used when multiple time-series plots are required
#' @param aggregate_space the name of the variable over which to perform 
#' spatial aggregation.
#' @param aggregate_time specifies the temporal scale over which to perform
#' temporal aggregation. Options are: "week", "month", "year".
#' @param aggregate_space_fun character indicating the function to be performed
#' in the aggregation over space, default is "mean". Options are "mean", "median", "sum". 
#' @param aggregate_time_fun character indicating the function to be performed
#' in the aggregation over time, default is "mean". Options are "mean", "median", "sum". 
#' @param panel if TRUE a separate time series for each space unit is plotted.
#' When type = "counts" the default is to plot the histograms in separate panels. 
#' @param highlight value of the area to be highlighted. Only available for
#' type = "incidence".
#' @param log logical, default FALSE. Log-transform the variable of interest.
#' @param title title of the plot. 
#' Default is "'var'" or "'var_label'" if var_label is specified.
#' @param var_label character with a custom name for the case or covariate variable.
#' @param legend character with a custom name for the legend.
#' @param ylab label for the y-axis.
#' @param xlab label for the x-axis.
#' @param free_y_scale logical, default FALSE. Allows different scales in the y_axis
#' @param palette GHR, RColorBrewer or viridisLite palette name. See all available 
#' options by running `GHR_palettes()`, `RColorBrewer::display.brewer.all()` 
#' and visiting `https://search.r-project.org/CRAN/refmans/viridisLite/html/viridis.html`.
#' For a single color (e.g. single time series or panels), one of the R colour in
#' `colors()` or a single hex codes is also possible.
#' @return a time series graph for passed covariates, incident rates or incident cases
#' @export
#'
#' @examples
#' # Load data
#' data("dengue_MS")
#' 
#' # Plotting a covariate in a single graph
#' plot_timeseries(dengue_MS,
#'                 var = "tmin",
#'                 type = "cov",
#'                 time = "date",          
#'                 area = "micro_code",   
#'                 title = "Yearly Minimun Temperature") 
#'                 
#' # Plotting a covariate with space aggregation and different panels
#' plot_timeseries(dengue_MS,
#'                 var = "tmin",
#'                 type = "cov",
#'                 time = "date",
#'                 area = "micro_code",
#'                 aggregate_space = "meso_code",
#'                 aggregate_space_fun = "mean",
#'                 panel = TRUE,           
#'                 var_label= "Minimum Temperature",
#'                 palette = "violetred")
#'                 
#' # Plotting a covariate, highlight a single area 
#' plot_timeseries(dengue_MS,
#'                 var = "dengue_cases", # Variable defining the cases 
#'                 type = "inc",
#'                 pop = "population",
#'                 time = "date",          # Variable defining the date "dd-mm-yyyy"
#'                 area = "micro_code",    # Variable defining the spatial unit
#'                 title= "Monthly Incidence",
#'                 highlight = "50001")
#' 
#' # Plot disease counts (log scale) with temporal and spatial aggregation             
#' plot_timeseries(dengue_MS,
#'                 var = "dengue_cases", 
#'                 type = "counts",
#'                 time = "date",       
#'                 aggregate_space = "meso_code",
#'                 aggregate_space_fun = "mean",
#'                 aggregate_time = "year",
#'                 aggregate_time_fun = "mean",   
#'                 area = "micro_code",   
#'                 title= "Yearly Cases",
#'                 log = TRUE) 
#'                
#' # Plot incidence for 1,000 people with a Brewer palette
#' plot_timeseries(dengue_MS,
#'                 var = "dengue_cases", # Variable defining the cases 
#'                 type = "inc",
#'                 pop = "population",
#'                 time = "date",          # Variable defining the date "dd-mm-yyyy"
#'                 area = "micro_code",    # Variable defining the spatial unit
#'                 pt = 1000,
#'                 palette = "Set1")                

plot_timeseries<- function(data,
                           var = NULL,
                           type = "cov",
                           time = NULL,
                           pop = NULL,
                           pt = 100000,
                           area = NULL,
                           aggregate_space = NULL,
                           aggregate_time = NULL,
                           aggregate_space_fun = "mean",
                           aggregate_time_fun = "mean",
                           panel = FALSE,
                           highlight = NULL,
                           log = FALSE, 
                           title = NULL,
                           var_label = NULL,
                           legend = NULL, 
                           ylab = NULL,
                           xlab = NULL,
                           free_y_scale = FALSE,
                           palette = "Qualitative") {
  
  args <- match.call()
  
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
  
  # Check that 'aggregate_space' is valid if specified
  if (!is.null(aggregate_space) && is.null(data[[aggregate_space]])) {
    stop("No column of the data match the 'aggregate_space' argument")
  }
  
  # Check that if 'aggregate_space' is valid area is also specified
  if (!is.null(aggregate_space) && is.null(area)) {
    stop("No 'area' argument provided")
  }
  
  # Check that 'aggregate_space_fun is one of the following functions
  # (sum , mean , median ) if specified.
  if (!is.null(aggregate_space) && !aggregate_space_fun %in% c(
    "sum",
    "mean",
    "median"
  )) {
    stop("aggregate_space_fun can be 'sum', 'mean' 'median'")
  }
  
  # Check that 'aggregate_time' is valid if specified
  if (!is.null(aggregate_time) && !(aggregate_time %in% c(
    "week",
    "month",
    "year"
  ))) {
    stop("'aggregate_time' can be 'week','month',year'")
  }
  
  # Check that 'aggregate_time_fun is one of the following functions
  # (sum , mean , median) if specified.
  if (!is.null(aggregate_time) && !aggregate_time_fun %in% c(
    "sum",
    "mean",
    "median"
  )) {
    stop("aggregate_time_fun can be 'sum', 'mean', 'median'")
  }
  
  # Check that only one between panel and highlight is specified
  if (panel == TRUE && !is.null(highlight)) {
    stop("'panel' and 'highlight' cannot be specified together")
  }
  
  # Create legend label 
  if (is.null (legend)){
    legend<- "Area"} else {
      legend<- legend
    }
  
  # Check for missing values in 'time' if specified
  if (!is.null(time)) {
    check_na(time, data)
  }
  
  # Check for missing values in 'area' if specified
  if (!is.null(area)) {
    check_na(area, data)
  }
  
  
  # Option 1. If the variable to be plotted is a covariate ----
  if (type=="cov" && !is.null(var)) {
    
    # Check that var is numeric
    if (is.numeric(data[[var]]) == FALSE) {
      stop("'var' should be numeric")
    }
    
    # Check for missing values in 'var' if specified
    if (!is.null(var)) {
      check_na(var, data)
    }
    
    # Option 1.A singe area, no aggregation ----
    # Aggregation is not required and data are observations from a single area
    # In this option data should be a single time series -->
    # Check for duplicated dates and return error if present multiple dates
    # otherwise plot the single time series
    
    if (is.null(area) && is.null(aggregate_space) && is.null(aggregate_time)) {
      if ((length(unique(data[[time]])) != nrow(data))) {
        stop("Duplicated time units detected, if data contains multiple spatial
         locations option 'area' or 'aggregate_space' need to be specified")
      } else {
        data <- data |>
          dplyr::select(
            var = {{ var }},
            time = {{ time }}
          ) |>
          dplyr::mutate(time = as.Date(time))
      }
      
      if(panel == TRUE){
        warning("Multiple panels not possible for a single time series")
      }
    }
    
    # Option 1.B multiple areas, no aggregation ----
    # 'area' is specified and 'aggregation' is not required (Multiple time series)
    # In option B the data.frame contains multiple time series ( one for each
    # spatial unit) that is defined by the variable 'area' -->
    # Check that no duplicates time unit exist within the same area and provide
    # error if present, otherwise plot time series stratified by area
    
    else if (!is.null(area) && is.null(aggregate_space)) {
      if (((length(unique(data[[time]])) * (length(unique(data[[area]]))))
           != nrow(data))) {
        stop(paste(
          "Duplicated time units detected within the same 'area'.",
          "Check 'aggregate_space' option"
        ))
      } else {
        if (length(unique(data[[area]])) > 15 && is.null(highlight)) {
          warning(paste(
            "More than 15 time series detected. Try 'highlight' or",
            "'aggregate_space'?"
          ))
        }
        data <- data |>
          dplyr::select(
            var = {{ var }},
            time = {{ time }},
            area = {{ area }}
          ) |>
          dplyr::mutate(
            time = as.Date(time), area = as.character(area)
          )
      }
    }
    
    # Option 1.C multiple areas, aggregation ----
    # 'aggregation over space and/or time' is required.
    # In option C the dataset contains one or multiple time series that
    # are derived aggregating original time series.
    else if (!is.null(aggregate_space) || !is.null(aggregate_time)) {
      data <- aggregate_cov(data,
                            var = var,
                            time = time,
                            area = area,
                            aggregate_time = aggregate_time,
                            aggregate_space = aggregate_space, 
                            aggregate_space_fun = aggregate_space_fun,
                            aggregate_time_fun = aggregate_time_fun)
    }
    
    # Log-transform if required
    if (isTRUE(log)) {
      data$var <- log(data$var) 
    }
    
    # Create plotting variables
    if (!is.null(aggregate_time)) {
      if (aggregate_time == "week") {
        data$time <- as.Date(paste(data$time, "1", sep = "-"), format = "%Y-%W-%u")
      } else if (aggregate_time == "month") {
        data$time <- as.Date(paste(data$time, "01", sep = "-"), format = "%Y-%m-%d")
      } else if (aggregate_time == "year") {
        data$time <- as.Date(paste(data$time, "01", "01", sep = "-"), format = "%Y-%m-%d")
      }
    }
    
    # Create plot title 
    if (is.null(title)){
      if (is.null (var_label)){
        title<- var} else {
          title<- var_label
        }
    } else title
    
    # Define the common ggplot variables and layout
    out <- ggplot2::ggplot(data, ggplot2::aes(x = time,
                                              y= var,
                                              color = area)) +
      ggplot2::geom_line() +
      ggplot2::ggtitle(title) +
      ggplot2::ylab(ylab) + ggplot2::xlab(xlab) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust=0.5))
    
    
    # Customize color for plots with multiple panels if palette has been specified
    if (!is.null(palette)){
      my_palette <- GHR_palette(palette)(2) # of 2 colors, the 1st one is white
      color <- my_palette[2]  # the second color should be the dominant one 
    }
    
    # Customize plot for many areas
    # Adjust legend columns dynamically
    legend_cols <- ifelse(length(unique(data[["area"]])) > 15, 2, 1)
    out <- out +
      ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_cols)) # Adjust legend layout
    
    # Customize plot for single time series
    if (length(unique(data$area)) < 2) {  # <2 means 0 (no specified) or 1 (single)
      out <- out + ggplot2::geom_line(color = color) +
        ggplot2::theme(legend.position = "none")
    }
    
    # Customize time x axis labels for long time series 
    nyears <- length(unique(format(as.Date(data$time), "%Y"))) # number of years
    
    if (nyears > 2){
      if (panel == TRUE){
        # Adjust breaks dynamically: fewer labels for larger datasets
        break_interval <- ifelse(nyears > 50, "5 years",
                                 ifelse(nyears > 10, "2 years",
                                        "1 year"))
        out<- out +
          ggplot2::scale_x_date(date_breaks = break_interval, date_labels = "%Y")
        
      }else{
        
        # Adjust breaks dynamically: fewer labels for larger datasets
        break_interval <- ifelse(nyears > 100, "5 years",
                                 ifelse(nyears > 50, "2 years",
                                        "1 year"))
        out<- out +
          ggplot2::scale_x_date(date_breaks = break_interval, date_labels = "%Y")
      }
    }
    
    # Customize plot if panel == TRUE
    if (panel == TRUE && (!is.null(area) || !is.null(aggregate_space))) {
      out$layers[[1]] <- NULL # substitute geom_line
      out <- out +
        ggplot2::geom_line(color = color) +
        ggplot2::facet_wrap(~ area) +
        ggplot2::guides(color = "none")
    } else {
      out <- out
    }
    
    # Customize plot if highlight is specified
    if (!is.null(highlight) && (!is.null(area) || !is.null(aggregate_space))) {
      colors <- c(rep("grey68", 2000))
      out <- out + ggplot2::scale_color_manual(values = colors) +
        ggplot2::geom_line(
          data = out$data[out$data$area == highlight, ],
          colour = "red"
        ) + ggplot2::theme(legend.position = "none")
      
    } else { out <- out}
    
    
    # Customize colors if Multiple time series are detected and Panel = FALSE
    # and no highlights has been specified
    if (panel == FALSE && is.null(highlight)){
      
      if (length(unique(data[["time"]])) != nrow(data)) {
        narea <- length(unique(out$data$area))
        my_palette <- GHR_palette(palette)(narea)
        out <- out + ggplot2::scale_color_manual(values = my_palette,
                                                 name = legend)
      }
    }
    
    # Apply free y-axis scaling if specified
    if (free_y_scale == TRUE && !is.null(area)) {
      out <- out + ggplot2::facet_wrap(~ area, scales = "free_y")
    }
    
    return(out)
  }
  
  
  # Option 2. If the variable to be plotted is counts or inc ----
  if (type %in% c("counts","inc") && !is.null(var)) {
    
    # Check that cases is numeric
    if (!is.numeric(data[[var]])) {
      stop("'var' should be numeric")
    }
    
    # Check for missing values in 'pop' if specified
    if(type=="inc"){      
      if (!is.null(pop)) {
        check_na(pop, data)
      }
    }else if (type=="counts") {    # Check type and fill pop to NA if not provided
      pop <- "pop"
      data$pop <- rep(NA, length(data[[var]]))
      panel <- TRUE
      highlights <- FALSE
    }
    
    if (type == "inc" && is.null(pop)) {
      stop("'pop' required if type = 'inc'")
    }
    
    # Option 2.A single area, no aggregation ----
    # 'area' is not specified and 'aggregation' is not required
    # In this option data should be a single time series -->
    # Check for duplicated dates and return error if present multiple dates
    # otherwise plot the single time series
    if (is.null(area) && is.null(aggregate_space) && is.null(aggregate_time)) {
      if ((length(unique(data[[time]])) != nrow(data))) {
        stop("Duplicated time units detected, if data contains multiple spatial
         locations option 'area' or 'aggregate_space' need to be specified")
      } else {
        data <- data |>
          dplyr::select(
            time = {{ time }},
            cases = {{ var }},
            pop = {{ pop }}
          ) |>
          dplyr::mutate(time = as.Date(time), inc = (.data$cases / pop) * pt)
      }
    }
    
    # Option 2.B multiple areas, no aggregation ----
    # 'area' is specified and 'aggregation' is not required (Multiple time series)
    # In option B the dataset contains multiple time series ( one for each
    # spatial unit) that is defined by the variable 'area' -->
    # Check that no duplicates time unit exist within the same area and provide
    # error if present, otherwise plot time series stratified by area
    else if (!is.null(area) && is.null(aggregate_space)) {
      if (((length(unique(data[[time]])) * (length(unique(data[[area]]))))
           != nrow(data))) {
        stop("Duplicated time units detected within the same 'area',
              check 'aggregate_space' option")
      } else {
        if (length(unique(data[[area]])) > 15) {
          warning(paste(
            "More than 15 spatial units detected,",
            "evaluate 'highlight' or 'aggregate_space' option"
          ))
        }
        data <- data |>
          dplyr::select(
            time = {{ time }},
            cases = {{ var }},
            pop = {{ pop }},
            area = {{ area }}
          ) |>
          dplyr::mutate(
            time = as.Date(time),
            area = as.character(area),
            inc = ((.data$cases / pop) * pt)
          )
      }
    }
    
    # Option 2.C multiple areas, aggregation ----
    # 'aggregation over space' or time is required (Aggregating multiple time series
    # over pre_specifed macro spatial units).
    # In option C the dataset contains one or multiple time series that
    # are derived aggregating original time series over the spatial unit targeted
    # by the aggregate_space argument.
    else if (!is.null(aggregate_space) || !is.null(aggregate_time)) {
      data <- aggregate_cases(data,
                              cases = var,
                              pop = pop,
                              time = time,
                              area = area,
                              aggregate_time = aggregate_time,
                              aggregate_space = aggregate_space,
                              pt = pt)
    }
    
    
    # Create plotting variables
    
    if (!is.null(aggregate_time)) {
      if (aggregate_time == "week") {
        data$time <- as.Date(paste(data$time, "1", sep = "-"), format = "%Y-%W-%u")
      } else if (aggregate_time == "month") {
        data$time <- as.Date(paste(data$time, "01", sep = "-"), format = "%Y-%m-%d")
      } else if (aggregate_time == "year") {
        data$time <- as.Date(paste(data$time, "01", "01", sep = "-"), format = "%Y-%m-%d")
      }
    }
    
    # Log transform if required
    
    if (type == "inc" && log == TRUE) {
      data$inc <- log(data$cases + 1) - log(data$pop / pt)
    }
    
    if (type == "counts" && log == TRUE) {
      data$cases <- log(data$cases+1)
    }
    
    
    ### PLOTTING INCIDENCE ----
    if (type == "inc") {
      
      if (is.null(title)){
        if (is.null (var_label)){
          title<- "Case incidence"} else {
            title<- var_label
          }
      } else title
      
      # when plotting incidence, add a subtitle with the person-time used 
      
      if(is.null(aggregate_time)){
        time_interval <- get_time_interval(data = data,
                                           time = "time",
                                           group = "area")
      } else {
        time_interval <- aggregate_time
      }
      
      out <- ggplot2::ggplot(data, ggplot2::aes(x = time,
                                                y= .data$inc,
                                                color = area)) +
        ggplot2::geom_line() +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(title) +
        ggplot2::labs(subtitle = paste(format(pt, big.mark = ",", scientific = FALSE), "person -", time_interval)) +
        ggplot2::ylab(ylab) + ggplot2::xlab(xlab) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5),
          axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust=0.5)
        )
      
      # Customize color for plots with multiple panels if palette has been specified
      
      if (!is.null(palette)){
        my_palette <- GHR_palette(palette)(2) # of 2 colors, the 1st one is white
        color <- my_palette[2]  # the second color should be the dominant one 
      }
      
      # Customize plot for single time series and panel==FALSE
      if (length(unique(data$area)) == 1) {
        
        out <- out + ggplot2::geom_line(color = color) +
          ggplot2::theme(legend.position = "none")
        
      }else  if(length(unique(data$area)) >= 1 & !isTRUE(panel)) {
        
        # Customize plot for many areas - Adjust legend columns dynamically
        legend_cols <- ifelse(length(unique(data[["area"]])) > 15, 2, 1)
        out <- out +
          ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_cols)) # Adjust legend layout
      }
      
      # Customize time x axis labels for long time series 
      nyears <- length(unique(format(as.Date(data$time), "%Y"))) # number of years
      
      if (nyears > 2){
        if (panel == TRUE){
          # Adjust breaks dynamically: fewer labels for larger datasets
          break_interval <- ifelse(nyears > 50, "5 years",
                                   ifelse(nyears > 10, "2 years",
                                          "1 year"))
          out<- out +
            ggplot2::scale_x_date(date_breaks = break_interval, date_labels = "%Y")
          
        }else{
          
          # Adjust breaks dynamically: fewer labels for larger datasets
          break_interval <- ifelse(nyears > 100, "5 years",
                                   ifelse(nyears > 50, "2 years",
                                          "1 year"))
          out<- out +
            ggplot2::scale_x_date(date_breaks = break_interval, date_labels = "%Y")
        }
      }
      
      # Customize plot if panel == TRUE
      if (isTRUE(panel) && (!is.null(area) || !is.null(aggregate_space) || (free_y_scale ==TRUE)))  {
        out$layers[[1]] <- NULL # substitute geom_line
        out <- out +
          ggplot2::geom_line(color = color) +
          ggplot2::facet_wrap(~ area) +
          ggplot2::guides(color = "none")
      } else {
        out <- out
      }
      
      
      # Customize plot if highlight is specified
      
      if (!is.null(highlight) && (!is.null(area) || !is.null(aggregate_space))) {
        colors <- c(rep("grey68", 2000))
        out <- out + ggplot2::scale_color_manual(values = colors) +
          ggplot2::geom_line(
            data = out$data[out$data$area == highlight, ],
            colour = "red"
          ) + ggplot2::theme(legend.position = "none")
        
      } else { out <- out}
      
      # Customize colors if Multiple time series are detected and Panel = FALSE
      # and no highlights has been specified
      
      if (panel == FALSE && is.null(highlight)){
        
        if (length(unique(data[["time"]])) != nrow(data)) {
          narea <- length(unique(out$data$area))
          my_palette <- GHR_palette(palette)(narea)
          
          out <- out + ggplot2::scale_color_manual(values = my_palette,
                                                   name = legend)
        }
      }
      
    }
    
    
    ### PLOTTING COUNTS ----
    
    else if (type == "counts") {
      
      if (is.null(title)){
        if (is.null (var_label)){
          title<- "Case counts"} else {
            title<- var_label
          }
      } else title
      
      
      
      out <- ggplot2::ggplot(data, ggplot2::aes(x = time,
                                                y= .data$cases,
                                                color = area)) +
        ggplot2::geom_bar(
          stat = "identity", linewidth = 0,
          position = "identity"
        ) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(title) +
        ggplot2::ylab(ylab) + ggplot2::xlab(xlab) +
        ggplot2::labs(fill = legend) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5),
          axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust=0.5)
        )
      
      # Customize color for plots with multiple panels if palette has been specified
      
      if (!is.null(palette)){
        my_palette <- GHR_palette(palette)(2) # of 2 colors, the 1st one is white
        color <- my_palette[2]  # the second color should be the dominant one 
      }
      
      # Customize plot for many areas
      # Adjust legend columns dynamically
      legend_cols <- ifelse(length(unique(data[["area"]])) > 15, 2, 1)
      out <- out +
        ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_cols)) # Adjust legend layout
      
      # Customize if single time series
      if (length(unique(data[["time"]])) == nrow(data)) {
        out <- out + ggplot2::geom_bar( fill = color, color = color, stat = "identity") +
          ggplot2::theme(legend.position = "none")
      }
      
      # Customize time x axis labels for long time series 
      nyears <- length(unique(format(as.Date(data$time), "%Y"))) # number of years
      
      if (nyears > 2){
        if (panel == TRUE){
          # Adjust breaks dynamically: fewer labels for larger datasets
          break_interval <- ifelse(nyears > 50, "5 years",
                                   ifelse(nyears > 10, "2 years",
                                          "1 year"))
          out<- out +
            ggplot2::scale_x_date(date_breaks = break_interval, date_labels = "%Y")
          
        }else{
          
          # Adjust breaks dynamically: fewer labels for larger datasets
          break_interval <- ifelse(nyears > 100, "5 years",
                                   ifelse(nyears > 50, "2 years",
                                          "1 year"))
          out<- out +
            ggplot2::scale_x_date(date_breaks = break_interval, date_labels = "%Y")
        }
      }
      
      # Customize if multiple time series as a panel
      if (!is.null(area) || !is.null(aggregate_space)) {
        out <- out +
          ggplot2::facet_wrap(~ area) +
          ggplot2::geom_bar( fill = color, color = color, stat = "identity") +
          ggplot2::guides(fill = "none") +
          ggplot2::guides(colour = "none")
      } else {
        out <- out
      }
      if (!is.null(highlight)) {
        warning(" Highlight not available for counts")
      }
      
      # Adjust color palette
      narea <- length(unique(out$data$area))
      my_palette <- GHR_palette(palette)(narea)
      
      out <- out +
        ggplot2::scale_fill_manual(values = my_palette) +
        ggplot2::scale_color_manual(values = my_palette, guide = "none")
    }
    
    # Apply free y-axis scaling if specified
    if (free_y_scale == TRUE && (!is.null(area)) && panel == TRUE) {
      out <- out + ggplot2::facet_wrap(~ area, scales = "free_y")
    }
    
    # return the final plot
    return(out)
  }
  
}