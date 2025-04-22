#' plot_multiple
#'
#' @title plot_multiple
#' @description outputs a list with multiple plots, each representing one variable 
#' @details variable names, types, labels and palette can be customized for 
#' each plot, the rest of parameters will be the same for all variables
#' (options depend on the chosen plot type).
#' @param plot_function indicates which of the ghr plot types to use
#' Options are: 'plot_timeseries', 'plot_heatmap', 'plot_seasonality',
#' and 'plot_map'.
#' @param ... Additional arguments to pass to the plotting function .
#' @export
#'
#' @examples
#' # Load data
#' library("sf")
#' data("dengue_MS")
#' data("map_MS")
#' 
# Multiple time series plots
#' plots <- plot_multiple(
#'   plot_function = plot_timeseries,
#'   data = dengue_MS,
#'   var = c("dengue_cases", "dengue_cases", "tmax", "tmin", "pdsi"),
#'   type = c("counts", "inc", "cov", "cov", "cov"),
#'   pop = "population",
#'   var_label = c("Dengue Cases", "Dengue inc", "Max Temp", "Min Temp", "PDSI"),
#'   palette = c("blue", "red", "darkgreen", "purple", "orange"),
#'   time = "date",
#'   area = "micro_code",
#'   panel = TRUE)
#' 
#' # Acess individual plots
#' print(plots[[1]])  
#' print(plots[[2]])  
#' print(plots[[3]])  
#' print(plots[[4]])  
#' print(plots[[5]])  
#' 
#' # Multiple heatmap plots
#' plots <- plot_multiple(
#'   plot_function = plot_heatmap,
#'   data = dengue_MS,
#'   var = c("dengue_cases", "dengue_cases", "tmax", "tmin", "pdsi"),
#'   type = c("counts", "inc", "cov", "cov", "cov"),
#'   pop = "population",
#'   var_label = c("Dengue Cases", "Dengue inc", "Max Temp", "Min Temp", "PDSI"),
#'   palette = c("Blues", "Reds", "BrBG", "viridis", "Purp"),
#'   time = "date",
#'   area = "micro_code")
#' 
#' 
#' # Multiple seasonality plots
#' plots <- plot_multiple(
#'   plot_function = plot_seasonality,
#'   data = dengue_MS,
#'   var = c("dengue_cases", "dengue_cases", "tmax", "tmin", "pdsi"),
#'   type = c("counts", "inc", "cov", "cov", "cov"),
#'   pop = "population",
#'   var_label = c("Dengue Cases", "Dengue inc", "Max Temp", "Min Temp", "PDSI"),
#'   palette =  c("Blues", "Reds", "BrBG", "viridis", "Purp"),
#'   time = "date",
#'   area = "micro_code")
#' 
#' # Multiple map plots
#' plots <- plot_multiple(
#'   plot_function = plot_map, 
#'   data = dengue_MS,
#'   var = c("dengue_cases", "dengue_cases", "tmax", "tmin", "pdsi"),
#'   type = c("inc", "counts", "cov", "cov", "cov"),
#'   pop = "population",
#'   var_label = c("Dengue Cases", "Dengue Counts", "Max Temp", "Min Temp", "PDSI"),
#'   palette = c("Reds", "Blues", "viridis", "cividis", "RdYlBu"),
#'   map = map_MS,         # the sf object for the map 
#'   map_area = "code",    # Variable defining the area in the sf object
#'   time = "date",
#'   area = "micro_code")

plot_multiple <- function(plot_function, ...) {
  
  # Define allowed plot functions
  valid_plot_functions <- c(
    "plot_timeseries", "plot_heatmap", "plot_seasonality","plot_map"
  )
  
  # Ensure plot_function is either a function or a valid function name
  if (!(is.function(plot_function) || as.character(substitute(plot_function)) %in% valid_plot_functions)) {
    stop(paste("Invalid plot function:", as.character(substitute(plot_function)), 
               "\nAllowed options:", paste(valid_plot_functions, collapse = ", ")))
  }
  
  
  # Determine which multiple plot function to apply
  if (identical(plot_function, plot_map)) {
    return(plot_multiple_map(plot_function = plot_function, ...))
  } else if (identical(plot_function, plot_timeseries) || 
             identical(plot_function, plot_heatmap) ||
             identical(plot_function, plot_seasonality)) {
    return(plot_multiple_common(plot_function = plot_function, ...))
  } else {
    stop("Unsupported plot function. Currently supported: 
         'plot_timeseries', 'plot_heatmap', 'plot_seasonality','plot_map'.")
  }
}



