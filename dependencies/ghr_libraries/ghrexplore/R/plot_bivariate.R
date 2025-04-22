#' @title plot_bivariate
#' @description plots a bivariate plot of 2 variables. It will be a scatterplot
#' if both variables are numeric and boxplots if one of them is categorical.
#' @param data data.frame containing equally spaced (daily, weekly, monthly) 
#' observations for one or multiple locations.
#' @param var 2 character vector of covariates. One of them can be a factor.
#' @param area Character, the name of the variable that identifies the different 
#' areal units. If specified, results are grouped by this variable. Defaults to 
#' NULL (no grouping).
#' @param panel if TRUE the outputs a separate plot for each grouping.
#' @param free_x_scale if TRUE and panel=TRUE, the x-axis scale is free in each panel
#' @param free_y_scale if TRUE and panel=TRUE, the y-axis scale is free in each panel
#' @param title title of the plot. 
#' Default is "'var 1' vs. 'var 2'" or "'var_label'" if var_label is specified
#' @param var_label a 2 character vector with a custom name for the variables
#' @param legend a character vector with a custom name for the legend
#' @param palette GHR, RColorBrewer or viridisLite palette. See all available 
#' options by running `GHR_palettes()`, `RColorBrewer::display.brewer.all()` 
#' and visiting `https://search.r-project.org/CRAN/refmans/viridisLite/html/viridis.html`.
#' For a single color (e.g. single plot or panels), one of the R colour in
#' `colors()` or a single hex codes is also possible.
#' @return bivariate plot of 2 covariates or cases and a covariate
#' @export
#' 
#' 
#' @examples
#' # Load data
#' data("dengue_MS")
#' 
#' # Scatter (two numeric variables) - No grouping
#' plot_bivariate(dengue_MS, var = c("pop_density", "tmin"), palette = "#d04a2d")
#' 
#' # Scatter (two numeric variables) - Grouping in the same graph
#' plot_bivariate(dengue_MS, 
#'                var = c("pop_density", "tmin"),
#'                var_label = c("Pop. density", "Min temp."),
#'                area = "micro_code")
#' 
#' # Scatter  (two numeric variables) - Grouping in panels
#' plot_bivariate(dengue_MS,
#'                var = c("pop_density", "tmin"),
#'                var_label = c("Pop. density", "Min temp."),
#'                area = "micro_code", panel = TRUE, 
#'                free_x_scale = TRUE, palette = "#d04a2d")
#' 
#' # Boxplots (one numeric, one categorical) - No grouping
#' plot_bivariate(dengue_MS, 
#'                var = c("pop_density", "biome_name"), 
#'                var_label = c("Pop. density", "Min temp."),
#'                palette = "royalblue")
#' 
#' # Boxplots (one numeric, one categorical)  - Grouping in the same graph
#' plot_bivariate(dengue_MS, 
#'                var = c("biome_name", "tmin"), 
#'                area = "meso_code",
#'                palette = "viridis")

plot_bivariate <- function(data = NULL,
                           var = NULL,
                           area = NULL,
                           panel = FALSE,
                           free_x_scale = FALSE,
                           free_y_scale = FALSE,
                           title = NULL,
                           var_label = NULL, 
                           legend = NULL, 
                           palette = "IDExtremes") {
  
  # Check minimum required arguments are missing
  args <- match.call()
  required_args <- c("data", "var")
  missing_args <- setdiff(required_args, names(args))
  
  if (length(missing_args) > 0) {
    stop(paste(
      "Missing required argument(s):",
      paste(missing_args, collapse = ", ")
    ))
  }  
  
  # Check data is a data frame
  if (!is.data.frame(data)) {
    stop("'data' should be a data.frame")
  }
  
  # Check if 'var' is a vector of exactly 2 elements
  if (is.null(var) || length(var) != 2  ) {
    stop("'var' is required and must be a vector with exactly 2 elements.")
  }
  
  # Check var exists in data
  if (!all(var %in% names(data))) {
    stop("One or more elements in 'var' are not found in 'data'.")
  }
  
  # Check if 'var_label' is a vector of exactly 2 elements
  if (!is.null(var_label) && length(var_label) != 2) {
    stop("'var_label' must be a vector with exactly 2 elements corresponding to the variables in the 'var' vector.")
  }
  
  # Check that 'area' is valid if specified
  if (!is.null(area) && is.null(data[[area]])) {
    stop("No column of the data matches the 'area' argument")
  }
  
  # Check for missing values in 'var' variables
  for (v in var) {
    check_na(v, data)
  }
  
  # Define variables. Only one can be categorical
  var1 <- var[1]
  var2 <- var[2]
  if(all(c(class(data[[var1]]), class(data[[var2]])) %in% c("character", "factor"))){
    stop("Only one categorical variable is allowed.")
  }
  
  ## 1.A: no area specified ----
  if (is.null(area)) {
    
    data <- data  |> 
      dplyr::select(
        x = {{ var1 }},
        y = {{ var2 }}
      ) 
    my_palette <- GHR_palette(palette)(2)
    color <- my_palette[2]  # the second color should be the dominant one 

  } else {
    
    ## 1.B: specified area ----
    data <- data |>
      dplyr::select(
        x = {{ var1 }},
        y = {{ var2 }},
        area = {{ area }}
      ) |>
      dplyr::mutate(
        area = as.character(area)
      )
    
    narea <- length(unique(data$area))
    my_palette <- GHR_palette(palette)(narea)
  }
  
  # Custom variable names
  if (!is.null(var_label)) {
    var1 <- var_label[1]
    var2 <- var_label[2]
  }
  
  # Create plot title
  if (is.null(title)) {
    title <- paste(var1, "vs.", var2)
  }
  
  # Create legend label
  if (is.null(legend)) {
    legend <- "Area"
  }
  
  out <- ggplot2::ggplot(data) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(var1) +
    ggplot2::ylab(var2)
  
  
  # Boxplot
  if(any(c(class(data[["x"]]), class(data[["y"]])) %in% c("character", "factor"))){
    
    if(is.null(area)){
      out <- out +
        ggplot2::geom_boxplot(ggplot2::aes(x = .data$x, y = .data$y, colour = area),
                              col = color)
    }else{
      out <- out +
        ggplot2::geom_boxplot(ggplot2::aes(x = .data$x, y = .data$y, colour = area)) +
        ggplot2::scale_color_manual(values = my_palette, name = legend) 
    }
    
    
  }else{ # Numerical
    
    if(is.null(area)){
      out <- out +
        ggplot2::geom_point(ggplot2::aes(x = .data$x, y = .data$y, colour = area),
                            size = 2, alpha = 0.7, col = color)
    }else{
      out <- out +
        ggplot2::geom_point(ggplot2::aes(x = .data$x, y = .data$y, colour = area),
                            size = 2, alpha = 0.7) +
        ggplot2::scale_color_manual(values = my_palette, name = legend) 
    }
    
  }
  
  # Adjust facet scales if panel == TRUE
  if (panel == TRUE && !is.null(area)) {
    
    # Determine scales
    facet_scales <- "fixed"
    if (free_x_scale && free_y_scale) {
      facet_scales <- "free"
    } else if (free_x_scale) {
      facet_scales <- "free_x"
    } else if (free_y_scale) {
      facet_scales <- "free_y"
    }
    
    out <- out +
      ggplot2::facet_wrap(~ area, scales = facet_scales) +
      ggplot2::guides(color = "none") 
  }
  
  return(out)
}

  
  