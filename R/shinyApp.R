
#' Presize shiny app
#'
#' Besides the programmatic approach to using presize, we also supply a shiny app.
#' @usage
#' launch_presize_app()
#' @export
#' @import shiny ggplot2 shinydashboard

launch_presize_app <- function(){
  shiny::runApp(system.file("shinyApp", package = "presize"))
}


