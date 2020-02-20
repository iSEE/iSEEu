#' App pre-configured to link multiple feature assay plots
#'
#' This mode launches a Shiny App preconfigured with multiple chain-linked
#' feature expression plots for interactive data exploration of the
#' [SingleCellExperiment][SingleCellExperiment::SingleCellExperiment()] or
#' [SummarizedExperiment][SummarizedExperiment::SummarizedExperiment()]
#' object.
#'
#' @param se An object that coercible to
#' [SingleCellExperiment-class][SingleCellExperiment::SingleCellExperiment()]
#' @param features `data.frame` with columns named `x` and `y`
#' that define the features on the axes of the linked plots.
#' Plots are serially linked from the first row to the last.
#' @param plotAssay The assay (one of assayNames(se)) to use for the plots
#'   (character vector of length either 1 or equal to `nrow(features)`).
#' @param ... Additional arguments passed to [iSEE()].
#' @param plotWidth The grid width of linked plots (numeric vector of
#' length either 1 or equal to `nrow(features)`
#'
#' @return A Shiny app object is returned.
#'
#' @export
#' @importFrom iSEE iSEE ReducedDimensionPlot ColumnDataPlot ColumnDataTable ComplexHeatmapPlot
#'   FeatureAssayPlot RowDataPlot RowDataTable SampleAssayPlot
#' @importFrom shiny runApp
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
#'
#' # Select top variable genes ----
#'
#' plot_count <- 6
#' rv <- rowVars(assay(sce, "tophat_counts"))
#' top_var <- head(order(rv, decreasing=TRUE), plot_count*2)
#' top_var_genes <- rownames(sce)[top_var]
#'
#' plot_features <- data.frame(
#'     x=head(top_var_genes, plot_count),
#'     y=tail(top_var_genes, plot_count),
#'     stringsAsFactors=FALSE
#'  )
#'
#' # launch the app itself ----
#'
#' app <- modeGating(sce, features = plot_features)
#' if (interactive()) {
#'   shiny::runApp(app, port=1234)
#' }
#'
modeGating <- function(se, features, plotAssay = NA_character_,
                       ..., plotWidth = 4) {
  # This mode is meaningless with fewer than two FeatureAssayPlots
  stopifnot(nrow(features) > 1)
  stopifnot(all(c("x", "y") %in% colnames(features)))
  stopifnot(length(plotWidth) %in% c(1, nrow(features)))
  if (length(plotWidth) == 1) {
    plotWidth <- rep(plotWidth, nrow(features))
  }
  stopifnot(length(plotAssay) %in% c(1, nrow(features)))
  if (length(plotAssay) == 1) {
    plotAssay <- rep(plotAssay, nrow(features))
  }

  arguments <- list(...)
  if (!("extra" %in% names(arguments))) {
    arguments$extra <- list(iSEE::ReducedDimensionPlot(), iSEE::ColumnDataPlot(),
                            iSEE::ColumnDataTable(), iSEE::ComplexHeatmapPlot(),
                            iSEE::FeatureAssayPlot(), iSEE::RowDataPlot(),
                            iSEE::RowDataTable(), iSEE::SampleAssayPlot())
  }
  if ("initial" %in% names(arguments)) {
    warning("Replacing specified 'initial' argument. Use iSEE::iSEE() directly ",
            "for more precise control of the starting configuration.")
    arguments$initial <- NULL
  }
  arguments$se <- se

  initial <- lapply(seq_len(nrow(features)), function(i) {
    iSEE::FeatureAssayPlot(
      Assay = plotAssay[i],
      XAxis = "Feature name",
      XAxisFeatureName = features[i, "x"],
      YAxisFeatureName = features[i, "y"],
      ColumnSelectionSource = ifelse(i == 1, "---",
                                     paste0("FeatureAssayPlot", i - 1)),
      SelectionEffect = ifelse(i != nrow(features), "Restrict",
                               "Color"),
      PanelWidth = as.integer(plotWidth[i])
    )
  })
  arguments$initial <- initial

  app <- do.call(iSEE::iSEE, arguments)

  return(app)
}

