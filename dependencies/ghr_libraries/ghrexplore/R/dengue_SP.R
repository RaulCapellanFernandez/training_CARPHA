#' Dengue cases from the "Sao Paulo" state of Brazil
#'
#' The current data frame reports the weekly number of notified dengue cases in
#' the municipality of Sao Paolo together with climatic covariates.

#'
#' @format ##
#' A data frame with 678 rows and 8 columns:
#' \describe{
#'   \item{date}{First day of the week, in date format ("%d-%m-%Y")}
#'   \item{geocode}{Unique ID code for Sao Paulo microregion}
#'   \item{dengue_cases}{Number of notified dengue cases}
#'   \item{year}{Year 2000 - 2022}
#'   \item{temp_med}{Weekly average daily mean temperature}
#'   \item{precip_tot}{Weekly cumulative precipitation}
#'   \item{enso}{El Niño-Southern Oscillation Index}
#'   \item{pop}{Number of inhabitants}
#' }
"dengue_SP"