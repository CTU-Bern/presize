#' Presize shiny app
#'
#' Besides the programmatic approach to using presize, we also supply a shiny app,
#' enabling point-and-click interaction with the program. The app will open in a
#' new window. Select the appropriate method from the menu on the left and enter
#' the relevant parameters indicated in the panel on the right. The output is then
#' displayed lower down the page.
#'
#' The main disadvantage to the app is that it only allows a single scenario at
#' a time.
#'
#' The app is also available at \href{https://shiny.ctu.unibe.ch/presize/}{https://shiny.ctu.unibe.ch/presize/}.
#'
#' \if{html}{\figure{app.png}{options: width="100\%"}}
#'
#' @usage
#' launch_presize_app()
#' @export
#' @example
#' # launch the app
#' launch_presize_app()

launch_presize_app <- function(){
  shiny::runApp(system.file("shinyApp", package = "presize"))
}


