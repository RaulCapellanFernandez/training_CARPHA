% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_timeseries.R
\name{plot_timeseries}
\alias{plot_timeseries}
\title{plot_timeseries}
\usage{
plot_timeseries(
  data,
  var = NULL,
  type = "cov",
  time = NULL,
  pop = NULL,
  pt = 1e+05,
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
  palette = "Qualitative"
)
}
\arguments{
\item{data}{an object of class "data.frame" containing equally spaced
(daily, weekly, monthly) covariates or cases for one or multiple locations.}

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

\item{pt}{numerical only used for \code{type='inc'}. It represents the scale of the
person-time (default 100,000) for incidence rates.}

\item{area}{the name of variable that identifies the different locations
(e.g., areal units). It is used when multiple time-series plots are required}

\item{aggregate_space}{the name of the variable over which to perform
spatial aggregation.}

\item{aggregate_time}{specifies the temporal scale over which to perform
temporal aggregation. Options are: "week", "month", "year".}

\item{aggregate_space_fun}{character indicating the function to be performed
in the aggregation over space, default is "mean". Options are "mean", "median", "sum".}

\item{aggregate_time_fun}{character indicating the function to be performed
in the aggregation over time, default is "mean". Options are "mean", "median", "sum".}

\item{panel}{if TRUE a separate time series for each space unit is plotted.
When type = "counts" the default is to plot the histograms in separate panels.}

\item{highlight}{value of the area to be highlighted. Only available for
type = "incidence".}

\item{log}{logical, default FALSE. Log-transform the variable of interest.}

\item{title}{title of the plot.
Default is "'var'" or "'var_label'" if var_label is specified.}

\item{var_label}{character with a custom name for the case or covariate variable.}

\item{legend}{character with a custom name for the legend.}

\item{ylab}{label for the y-axis.}

\item{xlab}{label for the x-axis.}

\item{free_y_scale}{logical, default FALSE. Allows different scales in the y_axis}

\item{palette}{GHR, RColorBrewer or viridisLite palette name. See all available
options by running \code{GHR_palettes()}, \code{RColorBrewer::display.brewer.all()}
and visiting \verb{https://search.r-project.org/CRAN/refmans/viridisLite/html/viridis.html}.
For a single color (e.g. single time series or panels), one of the R colour in
\code{colors()} or a single hex codes is also possible.}
}
\value{
a time series graph for passed covariates, incident rates or incident cases
}
\description{
Plots time-series of covariates OR cases (case counts or case incidence) over time.
}
\details{
case counts are represented as histograms; covariates and case incidence as line-plots,
with values on the y axis, and time is represented on the x axis.
}
\examples{
# Load data
data("dengue_MS")

# Plotting a covariate in a single graph
plot_timeseries(dengue_MS,
                var = "tmin",
                type = "cov",
                time = "date",          
                area = "micro_code",   
                title = "Yearly Minimun Temperature") 
                
# Plotting a covariate with space aggregation and different panels
plot_timeseries(dengue_MS,
                var = "tmin",
                type = "cov",
                time = "date",
                area = "micro_code",
                aggregate_space = "meso_code",
                aggregate_space_fun = "mean",
                panel = TRUE,           
                var_label= "Minimum Temperature",
                palette = "violetred")
                
# Plotting a covariate, highlight a single area 
plot_timeseries(dengue_MS,
                var = "dengue_cases", # Variable defining the cases 
                type = "inc",
                pop = "population",
                time = "date",          # Variable defining the date "dd-mm-yyyy"
                area = "micro_code",    # Variable defining the spatial unit
                title= "Monthly Incidence",
                highlight = "50001")

# Plot disease counts (log scale) with temporal and spatial aggregation             
plot_timeseries(dengue_MS,
                var = "dengue_cases", 
                type = "counts",
                time = "date",       
                aggregate_space = "meso_code",
                aggregate_space_fun = "mean",
                aggregate_time = "year",
                aggregate_time_fun = "mean",   
                area = "micro_code",   
                title= "Yearly Cases",
                log = TRUE) 
               
# Plot incidence for 1,000 people with a Brewer palette
plot_timeseries(dengue_MS,
                var = "dengue_cases", # Variable defining the cases 
                type = "inc",
                pop = "population",
                time = "date",          # Variable defining the date "dd-mm-yyyy"
                area = "micro_code",    # Variable defining the spatial unit
                pt = 1000,
                palette = "Set1")                
}
