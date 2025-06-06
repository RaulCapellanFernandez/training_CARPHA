% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_bivariate.R
\name{plot_bivariate}
\alias{plot_bivariate}
\title{plot_bivariate}
\usage{
plot_bivariate(
  data = NULL,
  var = NULL,
  area = NULL,
  panel = FALSE,
  free_x_scale = FALSE,
  free_y_scale = FALSE,
  title = NULL,
  var_label = NULL,
  legend = NULL,
  palette = "IDExtremes"
)
}
\arguments{
\item{data}{data.frame containing equally spaced (daily, weekly, monthly)
observations for one or multiple locations.}

\item{var}{2 character vector of covariates. One of them can be a factor.}

\item{area}{Character, the name of the variable that identifies the different
areal units. If specified, results are grouped by this variable. Defaults to
NULL (no grouping).}

\item{panel}{if TRUE the outputs a separate plot for each grouping.}

\item{free_x_scale}{if TRUE and panel=TRUE, the x-axis scale is free in each panel}

\item{free_y_scale}{if TRUE and panel=TRUE, the y-axis scale is free in each panel}

\item{title}{title of the plot.
Default is "'var 1' vs. 'var 2'" or "'var_label'" if var_label is specified}

\item{var_label}{a 2 character vector with a custom name for the variables}

\item{legend}{a character vector with a custom name for the legend}

\item{palette}{GHR, RColorBrewer or viridisLite palette. See all available
options by running \code{GHR_palettes()}, \code{RColorBrewer::display.brewer.all()}
and visiting \verb{https://search.r-project.org/CRAN/refmans/viridisLite/html/viridis.html}.
For a single color (e.g. single plot or panels), one of the R colour in
\code{colors()} or a single hex codes is also possible.}
}
\value{
bivariate plot of 2 covariates or cases and a covariate
}
\description{
plots a bivariate plot of 2 variables. It will be a scatterplot
if both variables are numeric and boxplots if one of them is categorical.
}
\examples{
# Load data
data("dengue_MS")

# Scatter (two numeric variables) - No grouping
plot_bivariate(dengue_MS, var = c("pop_density", "tmin"), palette = "#d04a2d")

# Scatter (two numeric variables) - Grouping in the same graph
plot_bivariate(dengue_MS, 
               var = c("pop_density", "tmin"),
               var_label = c("Pop. density", "Min temp."),
               area = "micro_code")

# Scatter  (two numeric variables) - Grouping in panels
plot_bivariate(dengue_MS,
               var = c("pop_density", "tmin"),
               var_label = c("Pop. density", "Min temp."),
               area = "micro_code", panel = TRUE, 
               free_x_scale = TRUE, palette = "#d04a2d")

# Boxplots (one numeric, one categorical) - No grouping
plot_bivariate(dengue_MS, 
               var = c("pop_density", "biome_name"), 
               var_label = c("Pop. density", "Min temp."),
               palette = "royalblue")

# Boxplots (one numeric, one categorical)  - Grouping in the same graph
plot_bivariate(dengue_MS, 
               var = c("biome_name", "tmin"), 
               area = "meso_code",
               palette = "viridis")
}
