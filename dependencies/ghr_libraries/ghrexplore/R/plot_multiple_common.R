
#' @title plot_multiple_common
#' @description outputs multiple plots, each representing one variable 
#' @details variable names and palette can be customized for each plot, otherwise
#' parameters will be the same for each variable plotted (options depend on the 
#' plot type chosen)
#' @param plot_function indicates which of the ghr plot types to use
#' Options are: 'plot_timeseries', 'plot_heatmap', 'plot_seasonality', 'plot
#' @param data an object of class "data.frame" containing covariates or cases 
#' @param var String specifying the name of the variable of interest.
#' @param type character that specifies the type of variable in `var`. 
#' Possible values include 'cov' (covariate, default), 'counts' (case counts), 
#' and 'inc' (case incidence). If `type='inc'`, `pop` is required.
#' @param pop only needed if "cases" is passed as argument. the name of the
#' variable that identifies the population.
#' Necessary if type = "incidence".
#' @param pt only needed if "cases" is passed as argument. the scale of the
#' person-time (default 100,000) for incidence rates.
#' Necessary if type = "incidence".
#' @param var_label character vector with custom names for the case or 
#' covariate variables. 
#' If cases are to be plotted, the label for the case variable must be the first
#'  in the vector. This vector must be the same length as the number of 
#'  covariates and cases specified  in the 'cases' and 'var' arguments
#' @param palette when character vector with length of the number of variables 
#' to be plotted, custom palettes for each plot; when single character, same 
#' palette is applied to all plots. 
#'  Options are: "GreenPurp","Purp","Green","BlueRed","Blue","Red",
#' "WaterSand","Water","Sand","Qualitative", "Sunset","Earth","BlYlRd","RdGnBl"
#' @param ... Additional arguments passed to `plot_function`.
#' @return a list of plots, each list element named after the variable plotted
#' @export
#'
#' @examples
#' # plots <- plot_multiple(
#' # plot_function = plot_timeseries,
#' # data = dengue_MS,
#' # var = c("tmax", "tmin", "pdsi"),
#' # cases = "dengue_cases",
#' # pop = "population",
#' # type = "incidence",
#' # var_label = c("Dengue Cases", "Max Temp", "Min Temp", "PDSI"),
#' # palettes = c("Blue", "Red", "Earth", "Sunset"),
#' # time = "date",
#' # area = "micro_code")
#' # # Access individual plots
#' # print(plots[["dengue_cases"]])  # Dengue cases plot
#' # print(plots[["tmax"]])          # Max Temp plot
#' # print(plots[["tmin"]])          # Min Temp plot
#' # print(plots[["pdsi"]])      # PDSI plot

plot_multiple_common <- function(plot_function, 
                                 data, 
                                 var, 
                                 type = "cov",
                                 var_label = NULL, 
                                 pop = NULL,
                                 pt = 100000,
                                 palette = "IDExtremes",  # Now a vector, not a list
                                 ...) {
  
  # Check if var is provided and is a non-empty vector
  if (is.null(var) || length(var) == 0) {
    stop("'var' must be a non-empty vector of variable names.")
  }
  
  # Ensure var contains only valid column names
  if (!all(var %in% names(data))) {
    stop("One or more elements in 'var' are not found in 'data'.")
  }
  
  # Ensure type contains only valid strings
  if (!all(type %in% c("cov", "counts", "inc"))) {
    stop("One or more elements in 'var' are not found in 'data'.")
  }
  
  # Check that type and var_label length matches var's
  expected_length <- length(var) 
  if (length(type) != expected_length) {
    stop("'type' must have the same length as 'var'")
  }
  if (!is.null(var_label) && length(var_label) != expected_length) {
    stop("'var_label' must have the same length as 'var'")
  }
  
  # Check if palette is either length 1 or the expected length
  if (!is.null(palette)) {
    if (!is.character(palette)) {
      stop("'palette' must be a character vector.")
    }
    if (!(length(palette) == 1 || length(palette) == expected_length)) {
      stop(paste("'palette' must be either length 1 (same for all plots) or match the number of variables (var + cases).",
                 "Expected length:", expected_length, "but got", length(palette)))
    }
  }
  
  # Initialize an empty list to store plots
  all_plots <- list()
  
  # Loop over each covariate and generate a plot
  for (i in seq_along(var)) {
    v <- var[i]
    t <- type[i]
    
    # var_label 
    var_label_new <- if (!is.null(var_label)) var_label[i] else v
    
    # Adjust index for palettes (use same if single, else per variable)
    palette_index <- if (length(palette) == 1) 1 else i 
    var_palette <-  palette[[palette_index]] 
    
    var_plot <- plot_function(
      data = data,
      var = v,
      type = t,
      pop = pop,
      pt = pt,
      var_label = var_label_new,
      palette = var_palette,  
      ...
    )
    
    # Store the plot 
    all_plots[[paste0("var", i)]] <- var_plot  
  }
  
  return(all_plots)
}





