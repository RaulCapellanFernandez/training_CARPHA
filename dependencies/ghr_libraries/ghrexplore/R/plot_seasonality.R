#' plot_seasonality
#'
#' @description Plots yearly time series of covariates or cases (case counts or 
#' case incidence) by month.
#' @details Months are represented on the x-axis, covariates or cases on the y axis, 
#' and each year is represented by a distinct color.
#' @param data an object of class "data.frame" containing equally spaced
#' (daily, weekly, monthly) observations for one or multiple locations.
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
#' @param pt the scale of the person-time (default 100,000) for incidence rates.
#' @param area the name of the variable that identifies the different locations
#' (e.g., areal units). It is used when multiple time-series plots are required
#' @param aggregate_space the name of the variable over which to perform 
#' spatial aggregation.
#' @param aggregate_time specifies the temporal scale over which to perform
#' temporal aggregation. Options are: "week", "month".
#' @param aggregate_space_fun character indicating a function to be performed
#' in the aggregation over space, default is "mean". Only for covariates.
#' For cases the only function available is the mean
#' @param aggregate_time_fun character indicating a function to be performed
#' in the aggregation over space, default is "mean". Only for covariates.
#' For cases the only function available is the mean.
#' @param log logical, default FALSE. Log-transform the variable of interest.
#' @param title title of the plot. 
#' Default is "'var'" or "'var_label'" if var_label is specified.
#' @param var_label character with a custom name for the case or covariate variable.
#' @param ylab Label for the y-axis.
#' @param xlab Label for the x-axis.
#' @param free_y_scale if TRUE, the y-axis scale is free in each panel.
#' @return A seasonal plot for a given variable.
#' @param palette GHR, RColorBrewer or viridisLite palette. See all available 
#' options by running `GHR_palettes()`, `RColorBrewer::display.brewer.all()` 
#' and visiting `https://search.r-project.org/CRAN/refmans/viridisLite/html/viridis.html`
#' @export
#'
#' @examples
#' # Load data
#' data("dengue_MS")
#' 
#' # Seasonality plot of a covariate with space aggregation
#' plot_seasonality(dengue_MS,
#'                  var = "tmax",
#'                  var_label = "Max temp.",
#'                  type = "cov",
#'                  time = "date",
#'                  area = "micro_code",
#'                  aggregate_space = "region_code") 
#' 
#' # Plot case counts (log scale) with space aggregation
#'  plot_seasonality(dengue_MS,
#'                   var = "dengue_cases",
#'                   type = "counts",
#'                   time = "date",  
#'                   area = "micro_code",
#'                   aggregate_space = "meso_code",
#'                   log = TRUE,
#'                   var_label = "Monthly Dengue Cases", 
#'                   xlab = "Month", 
#'                   ylab = "Number of cases",
#'                   free_y_scale = TRUE)
#'                   
#' # Seasonality plot of incidence
#' plot_seasonality(dengue_MS,
#'                  var = "dengue_cases",
#'                  type = "inc",
#'                  time = "date",    
#'                  pop = "population",
#'                  area = "micro_code",
#'                  pt = 1000, 
#'                  title = "Monthly Dengue Incidence",
#'                  palette = "Set1")        
#'                  
plot_seasonality <- function(data,
                             var = NULL,
                             type = "cov",
                             time = NULL,
                             pop = NULL,
                             pt = 100000,
                             area = NULL,
                             aggregate_space = NULL,
                             aggregate_time = "month",
                             aggregate_space_fun = "mean",
                             aggregate_time_fun = "mean",
                             log = FALSE,
                             title = NULL,
                             var_label = NULL,
                             ylab = NULL,
                             xlab = NULL,
                             free_y_scale = FALSE,
                             palette = "Qualitative"){
  
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
    
    # Aggregate the data
    data <- aggregate_cov(data,
                          var = var,
                          time = time,
                          area = area,
                          aggregate_time = aggregate_time,
                          aggregate_space = aggregate_space,
                          aggregate_time_fun = aggregate_time_fun,
                          aggregate_space_fun = aggregate_space_fun)
    
    # Log-transform if required
    if (isTRUE(log)) {
      data$var <- log(data$var) 
    }
    
    
    # Retrieve the variables for the seasonal plot
    data <- data |> dplyr::mutate(
      year = as.integer(substr(time, 1, 4)),
      time = as.integer(substr(time, 6, 7))) |>
      dplyr::filter(time > 0)
    
    
    # Now the ggplot has been declared for each possibility (week,month)
    # Customize the plot
    out <- ggplot2::ggplot(data, ggplot2::aes(
      x = as.factor(time),
      y = var,
      color = as.factor(.data$year),
      group = .data$year))
    
    # Customize color
    nyears <- length(unique(data$year))
    my_palette <- GHR_palette(palette)(nyears)
    
    # Create plot title 
    if (is.null(title)){
      if (is.null (var_label)){
        title<- var} else {
          title<- var_label
        }
    } else title
    
    # Adjust legend columns dynamically
    legend_cols <- ifelse(nyears > 15, 2, 1)
    out <- out + ggplot2::scale_color_manual(values = my_palette,
                                             name = "Year")
    
    out <- out +
      ggplot2::geom_line(alpha=0.7) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(
          angle = 90,
          hjust = 1, vjust=0.5)) +
      ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_cols)) +   # Adjust legend layout
      ggplot2::ylab(ylab) +
      ggplot2::xlab(xlab)
    
    # Split the plot into multiple panels for each area
    if (length(unique(data[["area"]])) != 1) {
      out <- out + ggplot2::facet_wrap(~area)
    } else {
      out <- out
    }
    
    # X-axis label (Months or Weeks)
    if (aggregate_time == "month"){
      out <- out +
        ggplot2::scale_x_discrete(labels = function(x) month.abb[as.numeric(x)])
    } else if (aggregate_time == "week") {
      out <-  out + ggplot2::scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0,
                                                                          paste0("W", as.numeric(x)), ""))
    }
    
    # Apply free y-axis scaling if specified
    if (free_y_scale == TRUE && !is.null(area)) {
      out <- out + ggplot2::facet_wrap(~ area, scales = "free_y")
    }
    
    # return the final plot
    return(out)
    
  }
  
  # Option 2. If the variable to be plotted is cases ----
  if (type %in% c("counts","inc") && !is.null(var)) 
  {
    # Check that cases is numeric
    if (!is.numeric(data[[var]])) {
      stop("'var' should be numeric")
    }
    
    # Check for missing values in 'pop' if specified
    if (!is.null(pop) & type=="inc") {
      check_na(pop, data)
    }
    
    if (type == "counts") {
      pop <- "pop"
      data$pop <- rep(NA, length(data[[var]]))
    }
    
    if (type == "inc" && is.null(pop)) {
      stop("'pop' required if type = 'inc'")
    }
    
    #Aggregate the dataset
    data <- aggregate_cases(data,
                            cases = var,
                            pop = pop,
                            time = time,
                            area = area,
                            aggregate_time = aggregate_time,
                            aggregate_space = aggregate_space,
                            pt = pt)
    
    # Customize the data depending on count or incidence
    if (type == "inc") {
      data$inc <- (data$cases / data$pop) * pt
    } else if (type == "counts") {
      data$inc <- data$cases
    }
    
    # Log transform if required
    if (type == "inc" && log == TRUE) {
      data$inc <- log(data$cases + 1) - log(data$pop / pt)
    }
    if (type == "counts" && log == TRUE) {
      data$inc <- log(data$cases+1)
    }
    
    # Customize dates for plot
    data <- data |> dplyr::mutate(
      year = as.integer(substr(time, 1, 4)),
      time = as.integer(substr(time, 6, 7))) |>
      dplyr::filter(time > 0)
    
    # Customize title 
    if (type == "inc") {
      if (is.null(title)){
        if (is.null (var_label)){
          title<- "Case incidence"} else {
            title<- var_label
          }
      } else title 
    }   else if (type == "counts") {
      if (is.null(title)){
        if (is.null (var_label)){
          title<- "Case counts"} else {
            title<- var_label
          }
      } else title
    }
    
    # Define ggplot variables and common layout
    out <- ggplot2::ggplot(data, ggplot2::aes(
      x = as.factor(time),
      y = .data$inc,
      color = as.factor(.data$year),
      group = .data$year))
    
    # Customize color
    nyears <- length(unique(data$year))
    my_palette <- GHR_palette(palette)(nyears)
    
    # Adjust legend columns dynamically
    legend_cols <- ifelse(nyears > 15, 2, 1)
    
    out <- out + ggplot2::scale_color_manual(values = my_palette,
                                             name = "Year")
    
    # Define common layout
    out <- out +
      ggplot2::geom_line(alpha=0.7) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(title) +
      ggplot2::ylab(ylab) +
      ggplot2::xlab(xlab) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(
          angle = 90,
          hjust = 1, vjust=0.5), 
        legend.spacing.x = ggplot2::unit(0.2, 'cm')  # Reduce spacing between legend items
        )+
      ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_cols)) # Adjust legend layout
    
    # Split the plot into multiple panels for each area
    if (length(unique(data[["area"]])) != 1 && free_y_scale == TRUE) {
      out <- out + ggplot2::facet_wrap(~ area, scales = "free_y") 
    } else if (length(unique(data[["area"]])) != 1) {
      out <- out + ggplot2::facet_wrap(~ area)
    } else {
      out <- out 
    }
    
    # X-axis label (Months or Weeks)
    if (aggregate_time == "month"){
      out <- out +
        ggplot2::scale_x_discrete(labels = function(x) month.abb[as.numeric(x)])
    } else if (aggregate_time == "week") {
      out <-  out + 
        ggplot2::scale_x_discrete(labels = function(x)
          ifelse(as.numeric(x) %% 4 == 0, paste0("W", as.numeric(x)), ""))
    }
    
    
    # Person-time subtitle: when plotting incidence 
    if (type == "inc") {
      if(is.null(aggregate_time)){
        time_interval <- get_time_interval(data = data,
                                           time = "time",
                                           group = "area")
      } else {
        time_interval <- aggregate_time
      } 
      out <- out +
        ggplot2::labs(subtitle = paste(format(pt, big.mark = ",", scientific = FALSE), "person -", time_interval)) 
    }
    
    if (type == "counts") {
      out <- out
    }
    
    # return the final plot
    return(out)
  }
}
 