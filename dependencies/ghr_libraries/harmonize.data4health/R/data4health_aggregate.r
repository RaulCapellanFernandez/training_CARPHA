#' Aggregate the dataframe to count frequency
#'
#' @param data The dataframe to be filtered. It can be a dataframe from the R
#' environment, or a filepath (accepted formats are csv, xls, xlsx, dbf,
#' and dbc).
#' @param space_col The name of the column which should be used for the
#' spatial aggregation.
#' @param time_col The name of the column which should be used for the
#' temporal aggregation
#' @param ... Any other columns that should be aggregated.
#' @return An aggregated dataframe.
#' @examples
#' data <- data.frame(
#'   space = c("A", "B", "B", "B"),
#'   day = c("2023-01-15", "2023-05-20", "2023-01-27", "2023-02-02"),
#'   month = c("2023-01-01", "2023-05-01", "2023-01-01", "2023-02-02"),
#'   value1 = c(10, 15, 20, 25),
#'   value2 = c(5, 8, 12, 17),
#'   gender = c("Male", "Female", "Female", "Female")
#' )
#' data4health_aggregate(data, time_col = "month", space_col = "space", "gender")
#' @importFrom readxl read_excel
#' @importFrom foreign read.dbf
#' @importFrom read.dbc read.dbc
#' @importFrom utils read.csv
#' @export
#'

data4health_aggregate <- function(data, space_col, time_col, ...) {

  # get passed columns and conditions to filter
  params <- list(...)

  # load data, either as dataframe or from file
  if (is.data.frame(data)) {

    # initialise a new dataframe
    data <- data

  } else if (is.character(data) && file.exists(data)) {

    # recognize file extension
    file_ext <- tolower(tools::file_ext(data))

    #load file
    if (file_ext == "csv") {
      data <- read.csv(data)
    } else if (file_ext %in% c("xlsx", "xls")) {
      data <- readxl::read_excel(data)
    } else if (file_ext == "dbf") {
      data <- foreign::read.dbf(data)
    } else if (file_ext == "dbc") {
      data <- read.dbc::read.dbc(data)
    }
  }

  # get full list of columns to be agreggated
  cols <- c(time_col, space_col, unlist(params))

  # check if columns are in dataframe
  if (!all(cols %in% colnames(data))) {
    stop(paste0("Your passed column(s) '", setdiff(cols, colnames(data)),
                "' is not present in the dataframe."))
  }

  # aggregate
  agg_data <- as.data.frame(do.call(table,
                                    c(lapply(cols, function(col) data[[col]]),
                                      list(dnn = cols))),
                            responseName = "freq")

  return(agg_data)
}

### END
