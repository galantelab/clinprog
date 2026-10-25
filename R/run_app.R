#' Launch the clinprog Interactive Suite
#'
#' @title Run the clinprog Shiny Application
#'
#' @description
#' Launches the local Shiny application for interactive survival and regression analyses.
#'
#' @details
#' Initiates the application UI and server bindings via \code{\link[shiny]{shinyApp}}.
#'
#' @return A Shiny application object (\code{\link[shiny]{shinyApp}}) that opens
#' automatically in the default web browser or RStudio Viewer.
#'
#' @seealso \code{\link{app_ui}}, \code{\link{app_server}}
#'
#' @examples
#' \dontrun{
#' # Launch the interactive application:
#' clinprog::run_app()
#' }
#'
#' @export
run_app <- function() {
  # Allow larger uploads (expression and clinical tables can easily exceed the
  # default Shiny limit of 30 MB). Set to 500 MB to accommodate typical inputs.
  options(shiny.maxRequestSize = 500 * 1024^2)

  # Check multiple possible locations for quarto resources
  if (dir.exists("inst/quarto")) {
    shiny::addResourcePath("quarto", "inst/quarto")
  } else {
    # Try to find the correct package path
    quarto_path <- system.file("quarto", package = "clinprog")
    if (quarto_path != "" && dir.exists(quarto_path)) {
      shiny::addResourcePath("quarto", quarto_path)
    }
  }

  shiny::shinyApp(ui = app_ui, server = app_server)
}
