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
#'   to one of \code{colnames(colData(se))} or \code{rownames(se)}. If coloring
#'   by a colData column, a column data plot is opened in addition to the
#'   reduced dimension panels. If coloring by a feature, a row statistics table
#'   is openend in addition to the reduced dimension panels, from which the
#'   latter are receiving the color.
#' @param ... Additional arguments passed to \code{\link{iSEE}}.
#' @param plot_width The grid width of linked plots (numeric vector of length
#'   either 1 or equal to \code{length(includeNames)}). The total width of
#'   the window is 12, so \code{plot_width = 4} for example will show three
#'   panels per row. If \code{plot_width = NULL} (the default), a value will be
#'   estimated depending on the number of reduced dimension panels.
#'
#' @return A Shiny app object is returned.
#'
#' @export
#' @importFrom methods is new
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
#' # ... coloring by a column data variable
#' app <- modeReducedDim(sce, colorBy = "Primary.Type")
#' if (interactive()) {
#'   shiny::runApp(app, port=1234)
#' }
#' # ... coloring by a feature
#' app <- modeReducedDim(sce, colorBy = "Scnn1a")
#' if (interactive()) {
#'   shiny::runApp(app, port=1234)
#' }
modeReducedDim <- function(
  se,
  includeNames = reducedDimNames(se),
  colorBy = NULL,
  ...,
  plot_width = NULL) {
  # This mode is only for SingleCellExperiments
  stopifnot(exprs = {
    is(se, "SingleCellExperiment")
    is.character(includeNames)
    length(includeNames) > 0L
    all(includeNames %in% reducedDimNames(se))
    is.null(plot_width) || (is.numeric(plot_width) &&
                              (length(plot_width) == 1L ||
                                 length(plot_width) == length(includeNames)))
    is.null(colorBy) || (is.character(colorBy) && length(colorBy) == 1L &&
                           (colorBy %in% colnames(colData(se)) ||
                              colorBy %in% rownames(se)))
  })

  maxOtherPanels <- 5L # maximum number of other (non-redDim) panels

  # determine plot_width
  n <- length(includeNames)
  if (is.null(plot_width)) {
    nr <- floor(sqrt(n))
    nc <- ceiling(n / nr)
    plot_width <- 12 / max(nc, 3)
  }

  # initial panels to show
  initialPanels <- DataFrame(
    Name = c(sprintf("Reduced dimension plot %i", seq_len(n))),
    Width = plot_width
  )

  # define reduced dimension panels
  redDimPlotArgs <- new('DataFrame', nrows = n,
                        rownames = sprintf('redDimPlot%i', seq_len(n)))
  redDimPlotArgs[['Type']] <- match(includeNames, reducedDimNames(se))
  redDimPlotArgs[['XAxis']] <- rep(1L, n)
  redDimPlotArgs[['YAxis']] <- rep(2L, n)

  rowStatTableArgs <- colDataPlotArgs <- NULL

  # add color
  if (!is.null(colorBy)) {
    if (colorBy %in% colnames(colData(se))) {
      # coloring of reduced dimension panels
      redDimPlotArgs[['ColorBy']] <- rep("Column data", n)
      redDimPlotArgs[['ColorByColData']] <- rep(colorBy, n) # TODO: receive color from "Column data plot 1"

      # additional column data plot
      colDataPlotArgs <- new('DataFrame', nrows = maxOtherPanels,
                             rownames = sprintf('colDataPlot%i', seq_len(maxOtherPanels)))
      colDataPlotArgs[['YAxis']] <- rep(colorBy, maxOtherPanels)
      colDataPlotArgs[['XAxis']] <- rep("None", maxOtherPanels)
      colDataPlotArgs[['ColorBy']] <- rep("Column data", maxOtherPanels)
      colDataPlotArgs[['ColorByColData']] <- rep(colorBy, maxOtherPanels)
      initialPanels <- rbind(initialPanels,
                             DataFrame(Name = "Column data plot 1",
                                       Width = plot_width[1]))

    } else {
      # coloring of reduced dimension panels
      redDimPlotArgs[['ColorBy']] <- rep("Feature name", n)
      redDimPlotArgs[['ColorByRowTable']] <- rep("Row statistics table 1", n)

      # additional row statistic table
      rowStatTableArgs <- new('DataFrame', nrows = maxOtherPanels,
                              rownames = sprintf('rowStatTable%i', seq_len(maxOtherPanels)))
      rowStatTableArgs[['Selected']] <- rep(match(colorBy, rownames(se)), maxOtherPanels)
      rowStatTableArgs[['Search']] <- rep(colorBy, maxOtherPanels)
      initialPanels <- rbind(initialPanels,
                             DataFrame(Name = "Row statistics table 1",
                                       Width = plot_width[1]))
    }
  }

  # Preconfigure an app
  app <- iSEE(
    se = se,
    redDimArgs = redDimPlotArgs, colDataArgs = colDataPlotArgs, featAssayArgs = NULL,
    rowStatArgs = rowStatTableArgs, rowDataArgs = NULL, sampAssayArgs = NULL,
    colStatArgs = NULL, customDataArgs = NULL, customStatArgs = NULL,
    heatMapArgs = NULL,
    redDimMax = length(reducedDimNames(se)), colDataMax = maxOtherPanels,
    featAssayMax = maxOtherPanels, rowStatMax = maxOtherPanels,
    rowDataMax = maxOtherPanels, sampAssayMax = maxOtherPanels,
    colStatMax = maxOtherPanels, customDataMax = maxOtherPanels,
    customStatMax = maxOtherPanels, heatMapMax = maxOtherPanels,
    initialPanels = initialPanels,
    ...
  )

  return(app)
}

