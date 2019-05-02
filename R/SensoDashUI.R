#' @title Launch SensoDash application
#'
#' @description A User Interface for the SensoDash package. The UI is a Shiny application for consumer data segmentation.
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'  SensoDashUI()
#'
#' }
SensoDashUI <- function() {
  shiny::runApp(system.file("app/", package = "SensoDash"), launch.browser = T)
}
