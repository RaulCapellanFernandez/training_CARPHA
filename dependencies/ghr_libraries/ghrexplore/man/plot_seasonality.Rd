% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_seasonality.R
\name{plot_seasonality}
\alias{plot_seasonality}
\title{plot_seasonality}
\usage{
plot_seasonality(
  data,
  var = NULL,
  type = "cov",
  time = NULL,
  pop = NULL,
  pt = 1e+05,
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
  palette = "Qualitative"
)
}
\arguments{
\item{data}{an object of class "data.frame" containing equally spaced
(daily, weekly, monthly) observations for one or multiple locations.}

\item{var}{the name of the column identifying the variable of interest.}

\item{type}{character that specifies the type of variable in \code{var}.
Possible values include 'cov' (covariate, default), 'counts' (case counts),
and 'inc' (case incidence). If \code{type='inc'}, \code{pop} is required.}

\item{time}{the name of the variable that identifies the temporal dimension
of the data.frame. The values must be in date format ("yyyy-mm-dd")
representing the day of observation for daily data, the first day of the
week for weekly, or the first day of the month for monthly observations.}

\item{pop}{character identifying the variable name for population. only needed
if \code{type='inc'}.}

\item{pt}{the scale of the person-time (default 100,000) for incidence rates.}

\item{area}{the name of the variable that identifies the different locations
(e.g., areal units). It is used when multiple time-series plots are required}

\item{aggregate_space}{the name of the variable over which to perform
spatial aggregation.}

\item{aggregate_time}{specifies the temporal scale over which to perform
temporal aggregation. Options are: "week", "month".}

\item{aggregate_space_fun}{character indicating a function to be performed
in the aggregation over space, default is "mean". Only for covariates.
For cases the only function available is the mean}

\item{aggregate_time_fun}{character indicating a function to be performed
in the aggregation over space, default is "mean". Only for covariates.
For cases the only function available is the mean.}

\item{log}{logical, default FALSE. Log-transform the variable of interest.}

\item{title}{title of the plot.
Default is "'var'" or "'var_label'" if var_label is specified.}

\item{var_label}{character with a custom name for the case or covariate variable.}

\item{ylab}{Label for the y-axis.}

\item{xlab}{Label for the x-axis.}

\item{free_y_scale}{if TRUE, the y-axis scale is free in each panel.}

\item{palette}{GHR, RColorBrewer or viridisLite palette. See all available
options by running \code{GHR_palettes()}, \code{RColorBrewer::display.brewer.all()}
and visiting \verb{https://search.r-project.org/CRAN/refmans/viridisLite/html/viridis.html}}
}
\value{
A seasonal plot for a given variable.
}
\description{
Plots yearly time series of covariates or cases (case counts or
case incidence) by month.
}
\details{
Months are represented on the x-axis, covariates or cases on the y axis,
and each year is represented by a distinct color.
}
\examples{
# Load data
data("dengue_MS")

# Seasonality plot of a covariate with space aggregation
plot_seasonality(dengue_MS,
                 var = "tmax",
                 var_label = "Max temp.",
                 type = "cov",
                 time = "date",
                 area = "micro_code",
                 aggregate_space = "region_code") 

# Plot case counts (log scale) with space aggregation
 plot_seasonality(dengue_MS,
                  var = "dengue_cases",
                  type = "counts",
                  time = "date",  
                  area = "micro_code",
                  aggregate_space = "meso_code",
                  log = TRUE,
                  var_label = "Monthly Dengue Cases", 
                  xlab = "Month", 
                  ylab = "Number of cases",
                  free_y_scale = TRUE)
                  
# Seasonality plot of incidence
plot_seasonality(dengue_MS,
                 var = "dengue_cases",
                 type = "inc",
                 time = "date",    
                 pop = "population",
                 area = "micro_code",
                 pt = 1000, 
                 title = "Monthly Dengue Incidence",
                 palette = "Set1")        
                 
}
