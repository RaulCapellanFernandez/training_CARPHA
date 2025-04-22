#' Load the cleaning scripts
#'
#' @return Open a webbrowser with the data4health user interface
#' @examples
#' #data4health_ui()
#' @importFrom shiny runApp
#' @export




data4health_ui <- function() {
    appDir <- system.file("shiny", package = "harmonize.data4health")
    shiny::runApp(appDir)
}
