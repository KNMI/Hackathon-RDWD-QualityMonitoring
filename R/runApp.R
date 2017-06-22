#' Run app
#' @export
runQualityMonitoring <- function() {
  wd <- getwd()
  runApp("inst/shiny", launch.browser = TRUE)
  on.exit(setwd(wd))
}
