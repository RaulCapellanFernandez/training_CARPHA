
# Helper Functions for Internal Checks ----

#' get_time_interval
#'
#' This function determines the time interval (e.g., daily, weekly, monthly, yearly)
#' of a dataset based on the differences between the first two time points.
#'
#' @param data an object of class 'data.frame' containing equally spaced
#'             time series data (daily, weekly, monthly data) for one or multiple spatial units or groups.
#' @param time A string representing the name of the column in `data` that contains time information.
#'             The column should be in "yyyy-mm-dd" format.
#' @param group (Optional) A string representing the names of the columns in `data`
#'             that contains grouping information. Required if `data` contains multiple spatial units.
#'             
#' @importFrom rlang := !! sym
#' 
#' @return A character string indicating the time interval of the dataset. Possible values:
#'         "day", "week", "month", "year", or "unknown".

get_time_interval <- function(data = NULL, time = NULL, group = NULL) {
  
  # Check if 'data' and 'time' are provided
  if (is.null(data) || is.null(time)) {
    stop("'data' and 'time' must be provided.")
  }
  
  # Input validation for 'group'
  if (!is.null(group) && !all(names(group) %in% names(data))) {
    stop("Grouping variable names in 'group' should match column names in 'data'.") 
  }
  
  # Check if there are no duplicated dates
  if (!is.null(group)) {
    if (nrow(unique(data[, c(group, time)])) != nrow(data[, c(group, time)])) {
      stop(paste(
        "Duplicated time units detected. To analyze data with multiple grouping variables, please specify them using 'group' argument." 
      ))
    }
  } else { 
    if ((length(unique(data[[time]])) != nrow(data))) { 
      stop(paste(
        "Duplicated time units detected. If the data contains multiple spatial units then the 'group' argument is required"
      ))
    }
  }  
  
  # Sort data by grouping variables and time
  
  if(!is.null(group)) {
    data <- data |>
      dplyr::arrange(dplyr::across(all_of(c(group, time)))) 
    date_vector <- as.Date(data[[time]])
  } else {
    data <- data |>
      dplyr::arrange(dplyr::across(all_of(c(time)))) 
    if(is.vector(data)) {
      date_vector <- as.Date(data)
    } else {
      date_vector <- as.Date(data[[time]])
    }
  }
  
  # Check for failed date conversion
  if (any(is.na(date_vector))) {
    stop("NA detected. Ensure 'time' column is in 'yyyy-mm-dd' format.")
  }
  
  # Calculate difference between the first two dates
  date_diffs <- diff(date_vector[1:2])
  # Determine the time interval based on the difference
  if (date_diffs == 1) {
    t <- "day"
  } else if (date_diffs == 7) {
    t <- "week"
  } else if (date_diffs >= 28 && date_diffs <= 31) {
    t <- "month"
  } else if (date_diffs >= 350 && date_diffs <= 370) {
    t <- "year"
  } else {
    t <- "unknown"
  }
  # Return the identified time interval as a string
  return(t)
}

#' check_consecutive
#'
#' @description This function verifies whether time values in a dataset are consecutive,
#' based on the detected time interval (e.g., daily, weekly, monthly, yearly).
#' If a `group` column is specified, the check is performed within each group.
#'
#' @param data A dataframe containing the dataset. Must include the time variable
#'             and optionally an group variable.
#' @param time A string representing the name of the column in `data` that contains time values.
#'             The column should be in "yyyy-mm-dd" format.
#' @param group (Optional) A string representing the names of the columns in `data`
#'             that contain grouping information. If provided, the check will
#'             be performed separately for group.
#'
#' @return A logical value: `TRUE` if all time values (within each group if `group` is specified)
#'         are consecutive, and `FALSE` otherwise.


check_consecutive <- function(data = NULL, time = NULL, group = NULL) { # this doesn't currently work! 
  
  # Check if 'data' and 'time' are provided
  if (is.null(data) || is.null(time)) {
    stop("'data' and 'time' must be provided.")
  }
  
  # Check if there are no duplicated dates
  if (!is.null(group)) {
    if (nrow(unique(data[, c(group, time)])) != nrow(data[, c(group, time)])) {
      stop(paste(
        "Duplicated time units detected. To analyze data with multiple grouping variables, please specify them using 'group' argument." 
      ))
    }
  } else { 
    if ((length(unique(data[[time]])) != nrow(data))) { 
      stop(paste(
        "Duplicated time units detected. If the data contains multiple spatial units then the 'group' argument is required"
      ))
    }
  }  
  
  # Subset relevant columns (group and time)
  # Ensure `time` is in Date format
  data <- data |> 
    dplyr::mutate(!!time := as.Date(.data[[time]])) |>
    dplyr::select(all_of(c(group, time)))
  
  # Determine the time interval (e.g., day, week, month, year)
  interval <- get_time_interval(data, time = time, group = group)
  
  # Check consecutiveness within each 'group'
  if (!is.null(group)) {
    is_consecutive <- data |>
      dplyr::arrange(dplyr::across(all_of(c(group, time)))) |>
      dplyr::group_by(dplyr::across(all_of(group))) |>
      dplyr::mutate(date_diffs = as.numeric(difftime(.data[[time]], 
                                                     dplyr::lag(.data[[time]]), 
                                                     units = "days"))) |>
      dplyr::filter(!is.na(.data$date_diffs)) |>
      dplyr::summarize(all_consecutive = dplyr::case_when(
        interval == "day"   ~ all(.data$date_diffs == 1),
        interval == "week"  ~ all(.data$date_diffs == 7),
        interval == "month" ~ all(.data$date_diffs >= 28 & .data$date_diffs <= 31),
        interval == "year"  ~ all(.data$date_diffs >= 350 & .data$date_diffs <= 370),
        TRUE ~ FALSE
      ), .groups = "drop") |>
      dplyr::pull(.data$all_consecutive)
    
    # Combine results across all groups
    is_consecutive <- all(is_consecutive)
  } else {
    # Check consecutiveness when 'group' is not specified
    is_consecutive <- data |>
      dplyr::arrange(.data[[time]]) |>
      dplyr::mutate(date_diffs = as.numeric(difftime(.data[[time]], 
                                                     dplyr::lag(.data[[time]]), 
                                                     units = "days"))) |>
      dplyr::filter(!is.na(.data$date_diffs)) |>
      dplyr::summarize(all_consecutive = dplyr::case_when(
        interval == "day"   ~ all(.data$date_diffs == 1),
        interval == "week"  ~ all(.data$date_diffs == 7),
        interval == "month" ~ all(.data$date_diffs >= 28 & .data$date_diffs <= 31),
        interval == "year"  ~ all(.data$date_diffs >= 350 & .data$date_diffs <= 370),
        TRUE ~ FALSE
      )) |>
      dplyr::pull(.data$all_consecutive)
  }
  
  return(is_consecutive)
}


#' check_na
#' @description This function checks for NAs in a specified column and 
#' returns a warning if there are any
#' @param data A dataframe containing the dataset. 
#' @param col_name A string representing the name of the column in `data` 
#' @return A warning specifying the columns with missing values
#'         
# Function to check for NAs in a specified column and return a warning if there are any
check_na <- function(col_name, data) {
  if (!is.null(col_name) && col_name %in% names(data)) {
    if (any(is.na(data[[col_name]]))) {
      warning(paste("Missing values found in the '", col_name, "' column"))
    }
  }
}


# Helper Functions for aggregation ----

#' aggregate_cov
#'
#' @param data an object of class "data.frame" containing equally spaced
#' (daily, weekly, monthly ) incident cases for one or multiple locations.
#' @param var  the name of variable that identify the covariate
#' @param time the name of the variable that identify the temporal dimension
#' of the data.frame. The values must be in date format ("dd-mm-yyyy")
#' representing the day of observation if the data.frame has a
#' daily observations, the first day of the week if the data.frame has weekly
#' observations, or the first day of the month if the data.frame has a monthly
#' observations.
#' @param area the name of the variable identifying the finest spatial unit
#' if the data contains observations form multiple areas
#' @param aggregate_space the name of the variable over which perform the
#' spatial aggregation.
#' @param aggregate_time one of the three option among "week", "month", "year".
#' Specify the temporal scale over which perform the aggregation.
#' @param aggregate_space_fun character indicating the function to be performed
#' in the aggregation over space, default is "mean"
#' @param aggregate_time_fun character indicating the function to be performed
#' in the aggregation over time, default is "mean"

#' @return an aggregated dataset
#' @export
#'

aggregate_cov <- function(data = NULL,
                          var = NULL,
                          time = NULL,
                          area = NULL,
                          aggregate_space = NULL,
                          aggregate_time = NULL,
                          aggregate_space_fun = "mean",
                          aggregate_time_fun = "mean") {
  
  # Check data is a data.frame
  
  if (!is.data.frame(data)) stop("'data' should be a data.frame")
  
  # Check necessary columsn and their type
  
  if (any(c(var, time) %in% colnames(data) == FALSE)) {
    stop("'time' and/or 'var' not found in the data")
  }
  
  if (!is.numeric(data[[var]])) stop("'var' should be numeric")
  
  # Validate aggregate_space and aggregate_time
  if (!is.null(aggregate_space) && !aggregate_space %in% colnames(data)) {
    stop("No column of the data matches the 'aggregate_space' argument")
  }
  
  if (!is.null(aggregate_time) && !aggregate_time %in% c("week", "month", "year")) {
    stop("'aggregate_time' can be 'week','month', or 'year'")
  }
  
  if (!is.null(aggregate_space) && !aggregate_space_fun %in% c(
    "sum", "mean", "median")) {
    stop("aggregate_space_fun can be 'sum', 'mean' 'median'")
  }
  
  # Check that 'aggregate_time_fun is one of the following functions
  # (sum , mean , median) if specified.
  
  if (!is.null(aggregate_time) && !aggregate_time_fun %in% c(
    "sum", "mean","median" )) {
    stop("aggregate_time_fun can be 'sum', 'mean', 'median'")
  }
  
  # Check that 'time' is in the correct date format and if correct
  # transform it to a Date
  
  if (any(is.na(as.Date(data[[time]], format = "%Y-%m-%d")))) {
    stop("'Date' should be in 'yyyy-mm-dd' format")
  } else {data[[time]] <- as.Date(data[[time]], format = "%Y-%m-%d")}
  
  
  t <- get_time_interval(data = data,
                         time = time,
                         group = area)
  
  # Check time aggregation 
  
  scale_rank <- c("day" = 1, "week" = 2, "month" = 3, "year" = 4)
  if (!is.null(aggregate_time)) {
    # If data scale is recognized
    if (!t %in% names(scale_rank)) {
      stop(sprintf("Unrecognized current data scale '%s'.", t))
    }
    
    # If user tries to go from e.g. monthly (3) -> weekly (2), throw error
    if (scale_rank[aggregate_time] < scale_rank[t]) {
      stop(sprintf(
        "Cannot aggregate from %s data to %s data (finer resolution). 
         Please choose a coarser time scale.",
        t, aggregate_time
      ))
    }
  }
  
  # If the data are weekly and user asks monthly aggregation, shift date +3 days

  if (t == "week" && aggregate_time == "month") {
    data[[time]] <- data[[time]] + 3
  }
  
  # If area is missing and aggregate_space is missing, create a dummy area
  
  if (is.null(area) && is.null(aggregate_space)) {
    data$area <- 1
    area <- "area"
  }
  
  # 1. Aggregate_space is specified
  
  if(!is.null(aggregate_space)) {
    
    data <- data |>
      dplyr::select(
        time = {{ time }},
        area = {{ aggregate_space }},
        var = {{ var }}) |>
      dplyr::mutate(time = as.Date(time), area = as.character(area)) |>
      dplyr::group_by(time, area) |>
      dplyr::summarise(var = match.fun(aggregate_space_fun)(var),
                       .groups = "drop")
    
  } else { data <- data |>
    dplyr::select(
      time = {{ time }},
      area = {{ area }},
      var = {{ var }}) |>
    dplyr::mutate(time = as.Date(time), area = as.character(area))
  }
  
  # 2. Aggregate_time is specified
  
  #  Week
  
  if (!is.null(aggregate_time) && aggregate_time == "week") {
    data$time <- format(as.Date(data$time), "%Y-%W")
  }
  
  # Month
  else if (!is.null(aggregate_time) && aggregate_time == "month") {
    data$time <- format(as.Date(data$time), "%Y-%m")
  }
  
  # Year
  else if (!is.null(aggregate_time) && aggregate_time == "year") {
    data$time <- format(as.Date(data$time), "%Y")
  }
  
  # Aggregate over time
  
  data <- data |>
    dplyr::group_by(area,time) |>
    dplyr::summarise(var = match.fun(aggregate_time_fun)(var),
                     .groups = "drop")
  
  return(data)
}

#' aggregate_cases
#'
#' @param data an object of class "data.frame" containing equally spaced
#' (daily, weekly, monthly ) incident cases for one or multiple locations.
#' @param cases  the name of variable that identify cases
#' @param time the name of the variable that identify the temporal dimension
#' of the data.frame. The values must be in date format ("dd-mm-yyyy")
#' representing the day of observation if the data.frame has a
#' daily observations, the first day of the week if the data.frame has weekly
#' observations, or the first day of the month if the data.frame has a monthly
#' observations.
#' @param area the name of the variable identifying the finest spatial unit
#' if the data contains observations form multiple areas
#' @param pop the name of the variable that identify the population.
#' Necessary if type = "incidence".
#' @param pt the scale of the person-time (default 100000) for incidence rates.
#' @param aggregate_space the name of the variable over which perform the
#' spatial aggregation.
#' @param aggregate_time one of the three option among "week", "month", "year".
#' Specify the temporal scale over which perform the aggregation.
#'
#' @return an aggregated dataset
#' @export
#'

aggregate_cases <- function(data = NULL,
                            cases = NULL,
                            pop = NULL,
                            time = NULL,
                            area = NULL,
                            pt = 100000,
                            aggregate_space = NULL,
                            aggregate_time = NULL) {
  
  # Check data is a data.frame
  
  if (!is.data.frame(data)) stop("'data' should be a data.frame")
  
  # Check necessary columsn and their type
  
  if (any(c(cases, time) %in% colnames(data) == FALSE)) {
    stop("'time' and/or 'cases' not found in the data")
  }
  
  if (!is.numeric(data[[cases]])) stop("'cases' should be numeric")
  
  # Validate aggregate_space and aggregate_time
  if (!is.null(aggregate_space) && !aggregate_space %in% colnames(data)) {
    stop("No column of the data matches the 'aggregate_space' argument")
  }
  
  if (!is.null(aggregate_time) && !aggregate_time %in% c("week", "month", "year")) {
    stop("'aggregate_time' can be 'week','month', or 'year'")
  }
  
  
  # Check that 'time' is in the correct date format and if correct
  # transform it to a Date
  
  if (any(is.na(as.Date(data[[time]], format = "%Y-%m-%d")))) {
    stop("'Date' should be in 'yyyy-mm-dd' format")
  } else (data[[time]] <- as.Date(data[[time]], format = "%Y-%m-%d"))
  
  
  t <- get_time_interval(data = data,
                         time = time,
                         group = area)
  
  # Check time aggregation 
  
  scale_rank <- c("day" = 1, "week" = 2, "month" = 3, "year" = 4)
  if (!is.null(aggregate_time)) {
    # If data scale is recognized
    if (!t %in% names(scale_rank)) {
      stop(sprintf("Unrecognized current data scale '%s'.", t))
    }
    
    # If user tries to go from e.g. monthly (3) -> weekly (2), throw error
    if (scale_rank[aggregate_time] < scale_rank[t]) {
      stop(sprintf(
        "Cannot aggregate from %s data to %s data (finer resolution). 
         Please choose a coarser time scale.",
        t, aggregate_time
      ))
    }
  }
  
  # If the data are weekly and user asks monthly aggregation, shift date +3 days
  if (t == "week" && aggregate_time == "month") {
    data[[time]] <- data[[time]] + 3
  }
  
  # Add a column for area with same value for all observations if area is missing 
  # and area was not provided by the user 
  
  if (is.null(area) && is.null(aggregate_space)) {
    data$area <- 1
    area <- "area"
  }
  
  # 1. Aggregate_space is specified
  
  if(!is.null(aggregate_space)) {
    
    data <- data |>
      dplyr::select(
        time = {{ time }},
        area = {{ aggregate_space }},
        cases = {{ cases }},
        pop = {{ pop }}) |>
      dplyr::mutate(time = as.Date(time), area = as.character(area)) |>
      dplyr::group_by(time, area) |>
      dplyr::summarise(cases = sum(cases), pop = sum(pop), .groups = "drop")
    
  } else { data <- data |>
    dplyr::select(
      time = {{ time }},
      area = {{ area }},
      cases = {{ cases }},
      pop = {{pop}}) |>
    dplyr::mutate(time = as.Date(time), area = as.character(area))
  }
  
  # 2. Aggregate_time is specified
  
  #  Week
  
  if (!is.null(aggregate_time) && aggregate_time == "week") {
    data$time <- format(as.Date(data$time), "%Y-%W")
  }
  
  # Month
  else if (!is.null(aggregate_time) && aggregate_time == "month") {
    data$time <- format(as.Date(data$time), "%Y-%m")
  }
  else if (!is.null(aggregate_time) && aggregate_time == "year") {
    data$time <- format(as.Date(data$time), "%Y")}
  
  # Aggregate over Time
  
  data <- data |>
    dplyr::group_by(area,time) |>
    dplyr::summarise(cases = sum(cases), pop = mean(pop)) |>
    dplyr::ungroup()
  
  
  # Compute Incidence
  
  data$inc <- (data$cases/data$pop) * pt
  
  if (is.null(pop)) {
    data <- data |> dplyr::select(area,time,cases)
  }
  
  return(data)
}


#' ymd_strict
#' 
#' Checks parsing of a date accoring to yyyy-mm-dd
#'
#' @param x a string to be parsed
#' @return an aggregated dataset
ymd_strict <- function(x) {
  parsed <- as.Date(x, format = "%Y-%m-%d")
  ifelse(!is.na(parsed) & format(parsed, "%Y-%m-%d") == x, parsed, as.Date(NA))
}


