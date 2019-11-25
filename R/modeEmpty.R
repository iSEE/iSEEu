#' App pre-configured to launch with no visible panel
#'
#' This mode launches an app that does not display any panel,
#' irrespective of which panels are available.
#'
#' This mode presents the advantage to launch an interface in a minimal amount of time,
#' as it does not need to render any panel when the interface is launched.
#' Users can then use the `"Organize panels"` widget to select panels to display in the interface.
#'
#' @param ... Arguments passed to [iSEE()].
#'
#' @return A Shiny app object is returned.
#'
#' @export
#' @importFrom S4Vectors DataFrame
#' @importFrom iSEE iSEE
#'
#' @examples
#' example("SingleCellExperiment")
#'
#' app <- modeEmpty(sce)
#' if (interactive()) {
#'   shiny::runApp(app, port=1234)
#' }
modeEmpty <- function(...){
  # Do not show any panel
  initialPanels <- S4Vectors::DataFrame(
    Name=character(0),
    Width=integer(0),
    Height=integer(0)
  )
  # Preconfigure an app
  app <- iSEE::iSEE(initialPanels=initialPanels, ...)

  return(app)
}
