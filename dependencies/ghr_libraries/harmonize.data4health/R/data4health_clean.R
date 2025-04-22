#' Clean the health data
#'
#' @param country The country of origin of your health data.
#' @param disease The disease that you want to .
#' @param data Health data.
#' @param cols_to_include Columns to be included
#' @param cols_to_remove Columns to be removed
#' @param missing_threshold Columns with more  missing values
#' than this will be excluded
#' @param rename A named list of current columns names and how to rename them.
#' @param date_to_week Which column should be transformed to week?
#' @param date_to_month Which column should be transformed to week?
#' @param age_threshold Rows with the age above this number will be removed.
#' @param save Should the output be saved locally?
#' @param output_file required if save = TRUE. Complete path wher the output of function
#' should be saved.
#' @return Performs basic cleaning on your files
#' @examples
#' data("peru_dengue_data")
#' data4health_clean(country = "peru", disease = "dengue",
#'                        data = peru_dengue_data,
#' output_file = "/shared/earth/GHR/daniela/harmonize_datapaper/clean_data_output/peru_dengue.csv",
#'                        cols_to_remove = c("diresa", "ubigeo", "localcod"),
#'                        rename = c(edade = "age", fecha_nacimiento = "birthdate"),
#'                        missing_threshold = 20,
#'                        age_threshold = 120)
#' @importFrom utils file.edit
#' @importFrom utils write.csv
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %>%
#' @importFrom stringr  str_to_title
#' @export


data4health_clean <- function(country = NULL, disease = NULL,
                       data,
                       cols_to_remove = NULL,
                       cols_to_include = NULL,
                       missing_threshold = 100,
                       rename = NULL,
                       date_to_week = NULL,
                       date_to_month = NULL,
                       age_threshold = 120,
                       output_file,
                       save = FALSE) {
  
  # check that if save is true, an output file name was provided
  if (save == TRUE & missing(output_file)) {
    stop("With 'save = TRUE', an output file needs to be provided.")
  }

  # check that only cols_to_remove or cols_to_include was provided
  if (all(!is.null(cols_to_remove), !is.null(cols_to_include))) {
    stop("Error: You must provide either 'cols_to_remove' OR 'cols_to', but not both.")
  }

  # include columns
  if (!is.null(cols_to_include)) {
    data <- data[, names(data) %in% cols_to_include]
    message(paste0("Only the following columns were kept from your dataset: ",
                   paste(cols_to_include, collapse = ", ")))
  }

  # remove columns
  if (!is.null(cols_to_remove)) {
    data <- data[, !names(data) %in% cols_to_remove]
    message(paste0("The following columns were removed from your dataset: ",
                   paste(cols_to_remove, collapse = ", ")))
  }

  # remove columns with missing values
  total_missing <- colSums(is.na(data)) +
    colSums(data == "", na.rm = TRUE) +
    colSums(data == "\\N", na.rm = TRUE)

  total_missing_df <- data.frame(column = names(total_missing),
                                 missing = unname(total_missing),
                                 missing_pct = unname(total_missing) / nrow(data) *100)

  missing_threshold <- missing_threshold
  remove_missing <- total_missing_df$column[total_missing_df$missing_pct > missing_threshold]

  if (length(remove_missing > 0)) {
      data <- data[, setdiff(names(data), remove_missing)]
      message(paste0("The following columns were removed from your dataset ",
                  "because it contains more missing data than the ",
                  "established trheshold of ", missing_threshold, " : ",
                      paste(remove_missing, collapse = ", ")))
  }

  # rename columns
  if (!is.null(rename)) {
    for (col in names(rename)){
      colnames(data)[colnames(data) == col] <- rename[[col]]
    }
  }

  # transform date to week
  if (!is.null(date_to_week)) {
    if (date_to_week %in% colnames(data)) {
      date_column <- date_to_week
    } else if (date_to_week %in% names(rename)) {
      date_column <- rename[[date_to_week]]
    } else {
      stop(paste0("Could not find column '", date_to_week, "' in dataset."))
    }

    data[[date_column]] <- as.Date(data[[date_column]])

    data[[paste0(date_column, "_week")]] <- data[[date_column]] - as.integer(format(data[[date_column]], "%w"))
  }

  # transform date to month
  if (!is.null(date_to_month)) {
    if (date_to_month %in% colnames(data)) {
      date_column <- date_to_month
    } else if (date_to_month %in% names(rename)) {
      date_column <- rename[[date_to_month]]
    } else {
      stop(paste0("Could not find column '", date_to_month, "' in dataset."))
    }
    
    data[[date_column]] <- as.Date(data[[date_column]])
    
    data[[paste0(date_column, "_month")]] <- as.Date(format(data[[date_column]], "%Y-%m-01"))
  }


  if (country == "peru"){
    if (disease == "dengue") {

        # age
        data$age <- ifelse(data$tipo_edad == "A", data$edad,
                            ifelse(data$tipo_edad == "M", data$edad / 12,
                                    ifelse(data$tipo_edad == "D", data$edad / 365, NA)))
        age_threshold <- age_threshold
        remove_age <- nrow(data[data$age > age_threshold, ])
        if (remove_age > 0) {
            data <- data[data$age <= age_threshold, ]
            message(paste0(remove_age, " entries were removed because the age was ",
                        "above the set threshold of ", age_threshold, "."))
        }

        # sex
        data <- data %>%
        dplyr::mutate(sex = case_when(
            sexo == "F" ~ "female",
            sexo == "M" ~ "male",
            TRUE ~ "other"
        ))

        # dengue type
        data <- data %>%
            dplyr::mutate(type = case_when(
            diagnostic == "A97.0" ~ "dengue without alarm signs",
            diagnostic == "A97.1" ~ "dengue with alarm signs",
            diagnostic == "A97.2" ~ "serious dengue",
            TRUE ~ "other"
        ))

        # time
        data$week <- as.numeric(data$semana) #data$week <- data$semana
        data$year <- data$ano
        data$epiweek_year <- paste0(sprintf("%02d", data$week), "-", data$year)

        # spatially
        data$adm1 <- stringr::str_to_title(data$departamento)
        data$adm2 <- stringr::str_to_title(data$provincia)
        data$adm3 <- stringr::str_to_title(data$distrito)

        # removing duplicated columns
        col_pairs <- list(departamento = "adm1", provincia = "adm2",
                            distrito = "adm3", edad = "age", tipo_edad = "age",
                            sexo = "sex", diagnostic = "type", enfermedad = "type",
                            semana = "week", ano = "year")
        remove_duplicate <- c()

        for (col in col_pairs) {
            if (col %in% colnames(data)) {
            remove_duplicate <- append(remove_duplicate,
                                        names(col_pairs[col_pairs == col]))
            }
        }

        if (length(remove_duplicate > 0)) {
            data <- data[, setdiff(names(data), remove_duplicate)]
            message(paste0("The following columns were removed from your dataset because they have been replaced: ",
                    paste(unique(remove_duplicate), collapse = ", ")))
        }        
    }
  } else if (country == "colombia") {
    if (disease == "dengue") {
        
    }
  } else if (country == "brazil") {
    if (disease == "dengue") {
        
    }
  }


  # save dataframe
  if (save == TRUE) {
    write.csv(data, output_file, row.names = FALSE)
    if (file.exists(output_file)) {
      message(paste0("File was saved succesfully to path: ", output_file))
    }
  }

  return(data)
}

### END
