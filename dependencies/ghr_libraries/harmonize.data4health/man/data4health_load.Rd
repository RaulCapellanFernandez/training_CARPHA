% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data4health_load.R
\name{data4health_load}
\alias{data4health_load}
\title{Load the health data}
\usage{
data4health_load(input_file)
}
\arguments{
\item{input_file}{Complete path to the input file.}
}
\value{
Load files into the R environment
}
\description{
Load the health data
}
\examples{
\dontrun{
data <- data4health_load("path/to/your/file.csv")
head(data)  # View first few rows
}
}
