#' Load the health data
#'
#' @param input_file Complete path to the input file.
#' @return Load files into the R environment
#' @examples
#' \dontrun{
#' data <- data4health_load("path/to/your/file.csv")
#' head(data)  # View first few rows
#' }
#' @importFrom  utils read.csv
#' @importFrom  readxl read_excel
#' @export

data4health_load <- function(input_file) {

  data <- data.frame()
  count <- 0

  for (file in input_file) {
    # check if file exist
    if (!file.exists(file)) {
      stop(paste0("The file '", file, "' does not exist."))
    }

    # check the file has an extension
    ext <- tools::file_ext(file)
    if (ext == "") {
      stop("The provided file has no recognizable extension.")
    }

    # load file
    if (ext == "csv") {
      new <- read.csv(file)
    } else if (ext == "rds") {
      new <- readRDS(file)
    } else  if (ext %in% c("xlsx", "xls")) {
      new <- suppressWarnings(read_excel(file))
    } else {
      stop("Extension ", ext, "is not yet supported.")
    }

    if (count == 0) {
      data <- new
      columns <- colnames(data)
    } else {
      if (setequal(colnames(new), columns)) {
        data <- rbind(data, new)
      } else {
        # find 'different' columns
        extra_in_columns <- setdiff(columns, colnames(new))
        extra_in_new <- setdiff(colnames(new), columns)

        stop(paste0("Column names of '", file, "' do not match.\n",
                    "Expected: ", paste(extra_in_columns, collapse = ", "),
                    "\n Given: ", paste(extra_in_new, collapse = "; "), "."))
      }
    }

    count <- count + 1
  }



  return(data)
      # lines <- readLines(input_file)
      # lines_fixed <- gsub("\\\\,", " ", lines)
      # temp_file <- tempfile()
      # writeLines(lines_fixed, temp_file)
      # data <- read.csv(temp_file, stringsAsFactors = FALSE)
}
