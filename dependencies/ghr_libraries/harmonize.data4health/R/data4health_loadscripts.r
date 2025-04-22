#' Load the cleaning scripts
#'
#' @param country The country of origin of your health data.
#' @param disease The disease that you want to .
#' @return Opens a script in your editor.
#' @examples
#' data4health_loadscript(country = "colombia", disease = "dengue")
#' data4health_loadscript(country = "brazil", disease = "malaria")
#' @importFrom utils file.edit
#' @export

data4health_loadscript <- function(country, disease) {

  path <- system.file("scripts", package = "harmonize.data4health")

  if (missing(country) && !missing(disease)) {
    countries <- unique(sub("_.*", "", list.files(path = path, pattern = disease)))
    stop(paste0("Country is missing. It must be one of the following: ",
                paste0(countries, collapse = ", ")))
  }
  if (!missing(country) && missing(disease)) {
    diseases <- unique(sub(".*_(.*?)\\..*", "\\1", list.files(path = path, pattern = country)))
    stop(paste0("Disease is missing. It must be one of the following: ",
                paste0(diseases, collapse = ", ")))

  }
  if (missing(country) && missing(disease)) {
    diseases <- unique(sub(".*_(.*?)\\..*", "\\1", list.files(path = path)))
    countries <- unique(sub("_.*", "", list.files(path = path)))
    stop(paste0("Disease and country are missing. 
                Country must be one of the following: ",
                paste0(countries, collapse = ", "),
                ".\n \nDisease must be one of the following: ",
                paste0(diseases, collapse = ", ")))

  }
  script_path <- paste0(path, "/", country, "_", disease, ".qmd")

  cat("Message")
  message(paste0("interactive is ", interactive()))
  print(paste0("interactive is ", interactive()))
  message(paste0("normalized path ", normalizePath(script_path, mustWork = FALSE)))
  print(paste0("normalized path ", normalizePath(script_path, mustWork = FALSE)))

  if (file.exists(script_path)) {
    message(script_path)
    print(script_path)
    file.edit(script_path)
    message("file opened")
  } else {
    print("Path does not exist.")
  }

}

### END
