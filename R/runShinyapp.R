#' @title run Shiny app for soil moisture sensor calibration
#' @description \code{runShinyapp} does start shiny application for soil moisture sensor calibration. 
#' @return shiny gui
#' @examples 
#' SWCalibrateR::runShinyapp()
#' @seealso 
#'  \code{\link[shiny]{runApp}}
#' @rdname runShinyapp
#' @export 
#' @importFrom shiny runApp
runShinyapp <- function() {
  appDir <- system.file("shiny", package = "SWCalibrateR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `SWCalibrateR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}