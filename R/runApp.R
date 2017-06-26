#' Run app
#' @export
runQualityMonitoring <- function() {
  wd <- getwd()
  shiny::runApp("inst/shiny", launch.browser = TRUE)
  on.exit(setwd(wd))
}
