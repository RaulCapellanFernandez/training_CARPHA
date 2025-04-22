# Function to plot correlation matrix between covariates

#' plot_correlation
#' @param data an object of class "data.frame" containing spatio(-temporal)
#' covariates.
#' @param var a character vector containing variables for which the correlation
#' matrix will be calculated
#' @param var_label a character vector containing custom names for the variables
#' @param method a method between "pearson" or "spearman" or "kendall, default "pearson"
#' @param title title of the plot. 
#' @param palette GHR, RColorBrewer or viridisLite palette. See all available 
#' options by running `GHR_palettes()`, `RColorBrewer::display.brewer.all()` 
#' and visiting `https://search.r-project.org/CRAN/refmans/viridisLite/html/viridis.html`
#' @param print boolean. If TRUE, print the correlation matrix.
#' @return a correlation matrix and heatmap
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' # Load data
#' data("dengue_MS")
#' 
#' # Pearson correlation heatmap
#' plot_correlation(dengue_MS, 
#'                  method = "pearson",
#'                  var = c("dengue_cases","pop_density", 
#'                          "tmax", "tmin", "pdsi", "urban",
#'                          "water_network", "water_shortage"),  
#'                  var_label = c("dengue cases","pop. density", 
#'                                "max temp", "min temp", "drought index", "urbanization",
#'                                "water network", "water shortage"),
#'                  title = "Correlation matrix",
#'                  palette = "magma") 
#' 
#' # Print spearman correlation matrix
#' plot_correlation(dengue_MS,
#'                  method = "spearman",
#'                  var = c("dengue_cases","pop_density", 
#'                          "tmax", "tmin", "pdsi", "urban",
#'                          "water_network", "water_shortage"),  
#'                  var_label = c("dengue cases","pop. density", 
#'                                "max temp", "min temp", "drought index", "urbanization",
#'                                "water network", "water shortage"),
#'                  print = TRUE) 
#' 

plot_correlation <- function(data = NULL,
                             var = NULL,
                             method = "pearson",
                             title = NULL,
                             palette = "IDExtremes", 
                             var_label = NULL, 
                             print = FALSE) {
  
  args <- match.call() # Capture all arguments passed to the function
  
  # Check if required arguments are missing
  required_args <- c("data", "var")
  missing_args <- setdiff(required_args, names(args)) # Identify missing arguments
  
  if (length(missing_args) > 0) {
    stop(paste("Missing required argument(s):", paste(missing_args,
                                                      collapse = ", "
    )))
  }
  
  # Check dataset type
  if (!is.data.frame(data)) {
    stop("'data' should be a 'data.frame'")
  }
  
  # Ensure specified variables are numeric
  if (!all(sapply(data[, var], is.numeric))) {
    stop("'variables should be numeric")
  }
  
  # Use 'var' as labels if 'var_label' is not provided
  if (is.null(var_label) ) {
    var_label <- var 
  }
  
  
  correlation_matrix <- as.data.frame(stats::cor(data[, var],
                                                 method = method,
                                                 use = "complete.obs")
  )
  
  
  # Create a data frame 'cx' from correlation matrix and manipulate it further
  cx<-correlation_matrix
  
  
  # Capitalize the first letter of the correlation_matrix rownames
  rownames(correlation_matrix) <- sapply(rownames(correlation_matrix), function(x) {
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  })
  
  colnames(correlation_matrix) <- sapply(colnames(correlation_matrix), function(x) {
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  })
  
  # Add a column witht the rownames in data frame 'cx'
  cx$param = row.names(cx)
  
  # Create a data frame with only the covariates and their names 
  facs = unique(cx$param)
  
  facs = data.frame(param=facs,
                    pname = var_label)
  
  # Create a vector to order the variables by the 'var_label' parameter
  fac_ord = var_label
  
  cx <- cx |>
    dplyr::left_join(facs, by = "param") |>  # Add user-specified 'var_label' to the correlation data frame
    dplyr::select(-.data$param) |>  # Remove param column 
    # Turn data into long format 
    tidyr::pivot_longer(-.data$pname, names_to = "variable", values_to = "value") |>  
    dplyr::left_join(facs, by = c("variable" = "param")) |>  # Add user-specified 'names' again
    dplyr::rename(x = .data$pname.x, y = .data$pname.y) |>  # Rename repeat columns from join 
    dplyr::mutate(
      x = factor(.data$x, levels = fac_ord, ordered = TRUE),  # Order factors
      y = factor(.data$y, levels = rev(fac_ord), ordered = TRUE)  # Reverse order for y
    ) |>
    dplyr::mutate(value = ifelse(.data$x == .data$y, NA, .data$value)) # Set diagonal to NA
  
  # Remove correlation values between the same covariates
  #cx = cx[ -which(cx$x == cx$y), ]
  
  # Select color palette
  my_palette <- GHR_palette(palette)(n = 50)
  
  # Create plot title: Empty string if it doesn't exist 
  if (is.null(title)){
    title<- ""
  }
  
  # Create the heatmap using 'ggplot2' with specified color palette and labels
  heatmap = cx |>
    ggplot() + 
    geom_raster(aes(.data$x, .data$y, fill = .data$value)) +
    theme_bw() +
    scale_x_discrete(position="top") +
    ggplot2::scale_fill_gradientn(
      colors = my_palette, limits = c(-1, 1),
      name = paste0(
        toupper(substring(method, 1, 1)),
        substring(method, 2)
      ), 
      na.value = "grey"  # NA values (diagonal) are displayed as grey
    ) +
    theme(plot.title = ggplot2::element_text(hjust = 0.5),
          axis.title = element_blank(),
          axis.text.x = element_text(angle=90),
          axis.text.y = element_text(angle=0),
          panel.grid = element_blank()  # Ensures no grid lines
    ) +
    ggplot2::ggtitle(title)
  
  # IF only the correlation matrix is needed ----
  if(isTRUE(print)){
    print(correlation_matrix)
  }
  
  # Return the heatmap
  return(heatmap)
  
}
