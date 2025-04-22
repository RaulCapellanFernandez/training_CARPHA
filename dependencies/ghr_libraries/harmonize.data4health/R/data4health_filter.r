#' Load the cleaning scripts
#'
#' @param data The dataframe to be filtered. It can be a dataframe from the R
#' environment, or a filepath (accepted formats are csv, xls, xlsx, dbf,
#' and dbc).
#' @param ... Any columns that you want to be filtered. Each columns should 
#' be passed with a list including the filter condition and the filter values.
#' Filter conditions depend on the column type:
#' - numeric = "over", "under", "between",
#' - Date = "after", "before", "between",
#' - character = "include", "exclude"
#' @return A filtered dataframe.
#' @examples
#' data <- data.frame(
#'   ID = c(1, 2, 3, 4, 5),
#'   Name = c("Alice", "Bob", "Charlie", "David", "Emily"),
#'   Age = c(25, 30, 22, 28, 35),
#'   Date = as.Date(c("2023-01-15", "2023-02-20", "2023-03-05", "2023-04-10", "2023-05-18")),
#'   City = c("New York", "London", "Paris", "Tokyo", "Sydney")
#' )
#' filtered_data <- data4health_filter(
#'   data,
#'   Age = list(over = 25),
#'   Date = list(between = c("2023-02-10", "2023-04-15")),
#'   City = list(exclude = c("London", "Tokyo"))
#' )
#' @importFrom readxl read_excel
#' @importFrom foreign read.dbf
#' @importFrom read.dbc read.dbc
#' @importFrom utils read.csv
#' @export
#'

data4health_filter <- function(data, ...) {

  # get passed columns and conditions to filter
  params <- list(...)

  # Check if any columns were passed to filter
  if (length(params) == 0) {
    stop("Provide at least one column to filter by.")
  }

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
    # data <- data4health_load(data) eventually change this
  } else {
    stop("Passed argument 'data' is not a dataframe and not a file to be loaded.")
  }

  # check if all passed columns exist in the data
  if (!all(names(params) %in% colnames(data))) {
    stop(paste0("Your passed column(s) '",
                setdiff(names(params), colnames(data)),
                "' is not present in the dataframe."))
  }

  # Loop through the filter columns
  for (col in names(params)) {

    # get column class
    col_class <- class(data[[col]])

    # get filter criterium
    filter_crit <- names(params[[col]])

    # accepted criteria for each column class
    criteria <- list(numeric = c("over", "under", "between"),
                     Date = c("after", "before", "between"),
                     character = c("include", "exclude"))

    # check that the criteria are correct for the column class
    if (!filter_crit %in% criteria[[col_class]]) {
      stop(paste0("Your passed filter criterium '", filter_crit,
                  "' is not valid. Since column '", col,
                  "' is ", col_class, " accepted criteria are: ",
                  paste(criteria[[col_class]], collapse = ", "), "."))
    }

    # filter columns
    filter_value <- params[[col]][[filter_crit]]
    if (filter_crit %in% c("after", "over")) {
      data <- data[data[[col]] >= filter_value, ]
    } else if (filter_crit %in% c("before", "under")) {
      data <- data[data[[col]] <= filter_value, ]
    } else if (filter_crit %in% c("between")) {
      data <- data[data[[col]] >= min(filter_value) &
                     data[[col]] <= max(filter_value) , ]
    } else if (filter_crit %in% c("include")) {
      data <- data[data[[col]] %in% filter_value, ]
    } else if (filter_crit %in% c("exclude")) {
      data <- data[!data[[col]] %in% filter_value, ]
    } else (
      stop(paste0("Something is wrong with your filter_crit ", filter_crit))
    )
  }
  return(data)
}

### END
