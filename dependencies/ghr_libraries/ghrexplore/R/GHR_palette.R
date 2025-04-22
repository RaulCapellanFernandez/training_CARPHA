#'GHR_palette()
#'Generate GHR Color Palettes
#'
#' Generates friendly color palettes with color ranges useful in
#' GHRexplore plotting including custom, ColorBrewer and viridis palettes.
#' See all available options by running `GHR_palettes()`,
#' `RColorBrewer::display.brewer.all()` and visiting `https://search.r-project.org/CRAN/refmans/viridisLite/html/viridis.html`.
#' 
#'@param palette Name of the GHR, RcolorBrewer or viridisLite palette.
#'
#'@return
#'GHR_palette() returns the function that generates the color palette and the
#'attribute 'na_color'.\cr
#'GHR_colors() returns a vector of the colors.\cr
#'GHR_palettes() returns a plot with the custom GHR palettes.
#'
#'@export

GHR_palette <- function(palette = "IDExtremes") {
  
  # Project palettes
  if (palette == "IDExtremes") {
    colorbar <- grDevices::colorRampPalette(c("#168c81", "white", "#ab9fde"))
    attr(colorbar, 'na_color') <- 'grey80'}
  else if (palette == "Purp") {
    colorbar <- grDevices::colorRampPalette(c("white", "#ab9fde"))
    attr(colorbar, 'na_color') <- 'grey80'}
  else if (palette == "Green"){
    colorbar <- grDevices::colorRampPalette(c( "white", "#168c81"))
    attr(colorbar, 'na_color') <- 'grey80'}
  else if (palette=="Qualitative") {
    colorbar<- grDevices::colorRampPalette(c( "#168c81", "#328ca1", "#4d8bbf",
                                              "#688bde", "#838afc","#9e8bfc",
                                              "#b98afb", "#d58afa", "#f08af8",
                                              "#e78fcb", "#e3949e", "#de9971",
                                              "#d99e44", "#d4a318", "#c9a53d",
                                              "#baa661", "#acab86", "#9faeaa",
                                              "#92b2cf", "#85b6f3"))
    attr(colorbar, 'na_color') <- 'grey80'} 
  else if (palette=="Colorblind") {
    colorbar<- grDevices::colorRampPalette(c( "#000000", "#E69F00", "#56B4E9",
                                              "#009E73", "#F0E442","#0072B2",
                                              "#D55E00", "#CC79A7", "#999999"))
    attr(colorbar, 'na_color') <- 'grey80'}
  else if (palette == "BlYlRd") {
    colorbar <- grDevices::colorRampPalette(c("#328ca1", "#F6EDBD", "#e3949e"))
    attr(colorbar, 'na_color') <- 'grey80'}
  
  # RColorBrewer and viridis palettes
  else{
    
    # Get the maximum number of colours for each palette (hence suppress warnings)
    colorbar <- tryCatch({
      suppressWarnings(
        grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, name=palette)))},
      error = function(e) {NULL}
    )
    
    # Viridis returns warning if it doesnt exist
    if(is.null(colorbar)){
      
      tryCatch({colorbar <- 
        grDevices::colorRampPalette(viridisLite::viridis(50, option=palette))}, 
        error = function(e) {NULL},
        warning = function(w) {NULL})      
    }
    
    # Single R color names
    if(is.null(colorbar) & palette %in% colors()){
      colorbar <- grDevices::colorRampPalette(palette)
    }
    
    # Single hex colors
    if(is.null(colorbar) & all(class(try(col2rgb(palette), silent = TRUE))!="try-error")){
      colorbar <- grDevices::colorRampPalette(palette)
    }
    
    if(is.null(colorbar)){
      stop("The selected palette is invalid. Please select a GHR, RColorBrewer or viridisLite palette.")
    }
  }
  
  return(colorbar)
}

#' GHR_colors
#' Generates a color palette based on IDE_palette
#'
#'@rdname GHR_palette
#'@export
#'@param n numerical, number of colours
#'@param palette string, name of the palette

GHR_colors <- function(n,
                       palette = "IDExtremes") {
  colors <- GHR_palette(palette)(n)
  
  return(colors)
}


#' GHR_palettes
#' Creates a visualization of all custom GHR palettes.
#'
#' @rdname GHR_palette
#' @returns a plot with all available palettes
#' @export
#'
#' @examples GHR_palettes()
GHR_palettes <- function(){
  
  # Helper to draw ramps
  draw_ramp <- function(palette){
    
    colors <-  GHR_colors(8, palette)
    df <- data.frame(color = colors, labels = seq_along(colors)) 
    
    ggplot2::ggplot(data = df) +
      ggplot2::geom_rect(ggplot2::aes(xmin = labels - 0.5, xmax = labels + 0.5,
                                      ymin = 0.5, ymax = 1.5, fill = I(colors)), 
                         color = "white") +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::ggtitle(palette) +
      ggplot2::theme_void() +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  
  # GHR palettes
  ghr_palettes <- cowplot::plot_grid(
    draw_ramp("IDExtremes"), draw_ramp("Purp"), draw_ramp("Green"),
    draw_ramp("Qualitative"), draw_ramp("Colorblind"), draw_ramp("BlYlRd"),
    ncol = 3
  )
  
  print(ghr_palettes)
}

