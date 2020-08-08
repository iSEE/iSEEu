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
#' @param colorBy Character scalar controlling coloring of cells. Must match either
#'   to one of \code{colnames(colData(se))} or \code{rownames(se)}. If coloring
#'   by a colData column, a column data plot is opened in addition to the
#'   reduced dimension panels. If coloring by a feature, a row statistics table
#'   is openend in addition to the reduced dimension panels, from which the
#'   latter are receiving the color.
#' @param ... Additional arguments passed to \code{\link{iSEE}}.
#' @param plotWidth The grid width of linked plots (numeric vector of length
#'   either 1 or equal to \code{length(includeNames)}). The total width of
#'   the window is 12, so \code{plotWidth = 4} for example will show three
#'   panels per row. If \code{plotWidth = NULL} (the default), a value will be
#'   estimated depending on the number of reduced dimension panels.
#'
#' @return A Shiny app object is returned.
#'
#' @export
#' @importFrom methods is
#' @importFrom SingleCellExperiment reducedDimNames colData
#' @importFrom iSEE ReducedDimensionPlot ColumnDataPlot RowDataTable iSEE
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
#'     shiny::runApp(app, port=1234)
#' }
#' # ... coloring by a feature
#' app <- modeReducedDim(sce, colorBy = "Scnn1a")
#' if (interactive()) {
#'     shiny::runApp(app, port=1234)
#' }
modeReducedDim <- function(
    se,
    includeNames = reducedDimNames(se),
    colorBy = NULL,
    ...,
    plotWidth = NULL) {
    # This mode is only for SingleCellExperiments
    stopifnot(exprs = {
        is(se, "SingleCellExperiment")
        is.character(includeNames)
        length(includeNames) > 0L
        all(includeNames %in% reducedDimNames(se))
        is.null(plotWidth) || 
            (is.numeric(plotWidth) && (length(plotWidth) == 1L || length(plotWidth) == length(includeNames)))
        is.null(colorBy) || 
            (is.character(colorBy) && length(colorBy) == 1L && 
                (colorBy %in% colnames(colData(se)) || colorBy %in% rownames(se)))
    })
    
    # determine plotWidth
    n <- length(includeNames)
    if (is.null(plotWidth)) {
        nr <- floor(sqrt(n))
        nc <- ceiling(n / nr)
        plotWidth <- 12 / max(nc, 3)
    }
    
    initial <- lapply(seq_len(n), function(i) {
        iSEE::ReducedDimensionPlot(
            Type = includeNames[i],
            PanelWidth = as.integer(plotWidth)
        )
    })
    
    # add color
    if (!is.null(colorBy)) {
        if (colorBy %in% colnames(colData(se))) {
            # coloring of reduced dimension panels
            initial <- lapply(initial, function(rdp) {
                rdp[["ColorBy"]] <- "Column data"
                rdp[["ColorByColumnData"]] <- colorBy
                rdp
            })
            
            cdp <- list(
                iSEE::ColumnDataPlot(
                    YAxis = colorBy,
                    XAxis = "None",
                    ColorBy = "Column data",
                    ColorByColumnData = colorBy,
                    PanelWidth = as.integer(plotWidth[1])
                )
            )
            
            initial <- c(initial, cdp)
            
        } else {
            # coloring of reduced dimension panels
            initial <- lapply(initial, function(rdp) {
                rdp[["ColorBy"]] <- "Feature name"
                rdp[["ColorByFeatureSource"]] <- "RowDataTable1"
                rdp
            })
            
            rdt <- list(
                iSEE::RowDataTable(
                    Selected = colorBy,
                    Search = colorBy,
                    PanelWidth = as.integer(plotWidth[1])
                )
            )
            
            initial <- c(initial, rdt)
        }
    }
    
    # Preconfigure an app
    app <- iSEE::iSEE(
        se = se,
        initial = initial,
        ...
    )
    
    return(app)
}

