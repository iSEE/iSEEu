
#' App pre-configured to compare multiple reduced dimension plots
#'
#' This mode launches a Shiny App preconfigured with multiple linked reduced
#' dimension plots for interactive data exploration of the
#' \code{\link{SingleCellExperiment}} object.
#'
#' @param se An object that coercible to \linkS4class{SingleCellExperiment}
#' @param includeNames Character vector with the names of reduced dimensions
#'   to display as individual panels. The default uses all available in
#'   \code{reducedDimNames(se)}.
#' @param colorBy Character scalar controling coloring of cells. Must match either
#'   to one of \code{colnames(colData(se))} or \code{rownames(se)}.
#' @param ... Additional arguments passed to \code{\link{iSEE}}.
#' @param plot_width The grid width of linked plots (numeric vector of length
#'   either 1 or equal to \code{length(includeNames)}
#'
#' @return A Shiny app object is returned.
#'
#' @export
#' @importFrom methods is
#' @importFrom SingleCellExperiment reducedDimNames colData
#' @importFrom S4Vectors DataFrame
#'
#' @examples
#' library(scRNAseq)
#'
#' # Example data ----
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#' class(sce)
#'
#' library(scater)
#' sce <- logNormCounts(sce, exprs_values="tophat_counts")
#' sce <- runPCA(sce, ncomponents = 30)
#' sce <- runTSNE(sce)
#' sce <- runUMAP(sce)
#' reducedDimNames(sce)
#'
#' # launch the app ----
#' app <- modeReducedDim(sce, colorBy = "Primary.Type")
#' if (interactive()) {
#'   shiny::runApp(app, port=1234)
#' }
#' app <- modeReducedDim(sce, colorBy = "Scnn1a")
#' if (interactive()) {
#'   shiny::runApp(app, port=1234)
#' }
modeReducedDim <- function(
  se,
  includeNames = reducedDimNames(se),
  colorBy = NULL,
  ...,
  plot_width=4) {
  # This mode is only for SingleCellExperiments
  stopifnot(exprs = {
    is(se, "SingleCellExperiment")
    is.character(includeNames)
    length(includeNames) > 0L
    all(includeNames %in% reducedDimNames(se))
    is.numeric(plot_width)
    length(plot_width) == 1L || length(plot_width) == length(includeNames)
    is.null(colorBy) || (is.character(colorBy) && length(colorBy) == 1L &&
                           (colorBy %in% colnames(colData(se)) ||
                              colorBy %in% rownames(se)))
  })

  # define reduced dimension panels
  n <- length(includeNames)
  redDimPlotArgs <- new('DataFrame', nrows = n,
                        rownames = sprintf('redDimPlot%i', seq_len(n)))
  redDimPlotArgs[['Type']] <- seq_along(includeNames)
  redDimPlotArgs[['XAxis']] <- rep(1L, length(includeNames))
  redDimPlotArgs[['YAxis']] <- rep(2L, length(includeNames))

  if (!is.null(colorBy)) {
    if (colorBy %in% colnames(colData(se))) {
      redDimPlotArgs[['ColorBy']] <- rep("Column data", n)
      redDimPlotArgs[['ColorByColData']] <- rep(colorBy, n)
    } else {
      redDimPlotArgs[['ColorBy']] <- rep("Feature name", n)
      redDimPlotArgs[['ColorByFeatName']] <- rep(match(colorBy, rownames(se)), n)
    }
  }

  # Show only the active
  initialPanels <- DataFrame(
    Name = c(sprintf("Reduced dimension plot %i", seq_len(n))),
    Width = plot_width
  )

  # Preconfigure an app
  app <- iSEE(
    se = se,
    redDimArgs = redDimPlotArgs, colDataArgs = NULL, featAssayArgs = NULL,
    rowStatArgs = NULL, rowDataArgs = NULL, sampAssayArgs = NULL,
    colStatArgs = NULL, customDataArgs = NULL, customStatArgs = NULL,
    heatMapArgs = NULL,
    redDimMax = length(reducedDimNames(se)), colDataMax = 0, featAssayMax = 0,
    rowStatMax = 0, rowDataMax = 0, sampAssayMax = 0, colStatMax = 0,
    customDataMax = 0, customStatMax = 0, heatMapMax = 0,
    initialPanels = initialPanels,
    ...
  )

  return(app)
}

