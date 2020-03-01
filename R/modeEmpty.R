#' App pre-configured to launch with no visible panel
#'
#' This mode launches an app that does not display any panel.
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
#' @importFrom iSEE iSEE
#' @importFrom shiny runApp
#'
#' @examples
#' example("SingleCellExperiment")
#' rownames(sce) <- paste0("G", 1:200)
#' colnames(sce) <- paste0("C", 1:100)
#'
#' app <- modeEmpty(sce)
#' if (interactive()) {
#'   shiny::runApp(app, port=1234)
#' }
#'
modeEmpty <- function(...) {
  # Preconfigure an app
  app <- iSEE::iSEE(initial = list(), ...)

  return(app)
}
