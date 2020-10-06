#' The AggregatedDotPlot class
#'
#' Implements an aggregated dot plot where each feature/group combination is represented by a dot.
#' The color of the dot scales with the mean assay value across all samples for a given group,
#' while the size of the dot scales with the number of non-zero values across all samples.
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
#' # launch the app itself ----
#' if (interactive()) {
#'     iSEE(sce, initial=list(
#'         AggregatedDotPlot(ColumnData=c("Primary.Type"))
#'     ))
#' }
#' 
#' @name AggregatedDotPlot
#' @aliases
#' AggregatedDotPlot-class
#' .panelColor,AggregatedDotPlot-method
#' .fullName,AggregatedDotPlot-method
#' .generateOutput,AggregatedDotPlot-method
NULL

#' @export
setClass("AggregatedDotPlot", contains="ComplexHeatmapPlot")

#' @export
AggregatedDotPlot <- function(...) {
    new("AggregatedDotPlot", ...)
}

#' @export
setMethod(".panelColor", "AggregatedDotPlot", function(x) "#703737FF")

#' @export
setMethod(".fullName", "AggregatedDotPlot", function(x) "Aggregated dot plot")

#' @export
#' @importFrom SummarizedExperiment assay rowData colData
#' @importFrom ggplot2 ggplot geom_text aes theme_void
setMethod(".generateOutput", "AggregatedDotPlot", function(x, se, all_memory, all_contents) {
    # print(str(x))
    plot_env <- new.env()
    plot_env$se <- se
    plot_env$colormap <- metadata(se)$colormap

    all_cmds <- list()
    all_cmds$select <- .processMultiSelections(x, all_memory, all_contents, plot_env)
    all_cmds$assay <- iSEE:::.process_heatmap_assay_values(x, se, plot_env)

    # Computing the various statistics.
    computation <- c(
        sprintf(".group_by <- SummarizedExperiment::colData(se)[,%s];", 
            paste(deparse(x[["ColumnData"]]), collapse="")),
        "averages <- scuttle::sumCountsAcrossCells(plot.data, .group_by, average=TRUE);",
        "n.detected <- scuttle::numDetectedAcrossCells(plot.data, .group_by, average=TRUE);"
    )

    .textEval(computation, plot_env)
    all_cmds$command <- computation

    # Organizing in the plot.data.
    prep.cmds <- c(
       ".levels <- do.call(paste, c(as.list(SummarizedExperiment::colData(averages)), sep=', '))",
        "plot.data <- data.frame(
    Feature=rep(rownames(averages), ncol(averages)), 
    Group=rep(.levels, each=nrow(averages)),
    Average=as.numeric(SummarizedExperiment::assay(averages)), 
    Detected=as.numeric(SummarizedExperiment::assay(n.detected))
)")
    .textEval(prep.cmds, plot_env)
    all_cmds$prep <- prep.cmds

    .low_color <- "grey"
    .high_color <- "red"

    plot.cmds <- c(
        'dplot <- ggplot(plot.data)',
        'geom_point(aes_string(x = "Group", y = "Feature", size = "Detected", col = "Average"))',
        'scale_size(limits = c(0, max(plot.data$Detected)))',
        sprintf(
            'scale_color_gradient(limits = c(0, max(plot.data$Average)), low = %s, high = %s)',
            deparse(.low_color), deparse(.high_color)
        ),
        'theme(panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(size = 0.5, colour = "grey80"),
    panel.grid.minor = element_line(size = 0.25, colour = "grey80"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))'
    )

    plot.cmds <- paste0(plot.cmds, collapse=" +\n    ")
    plot_out <- .textEval(plot.cmds, plot_env)
    all_cmds$plot <- plot.cmds 

    list(commands=all_cmds, contents=plot_env$plot.data, plot=plot_out, varname="dplot")
})

#' @export
#' @importFrom shiny renderPlot tagList
setMethod(".renderOutput", "AggregatedDotPlot", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.

    # nocov start
    output[[plot_name]] <- renderPlot({
        p.out <- .retrieveOutput(plot_name, se, pObjects, rObjects)
        print(p.out$plot)
    })
    # nocov end
})

#' @export
#' @importFrom grDevices pdf dev.off
setMethod(".exportOutput", "AggregatedDotPlot", function(x, se, all_memory, all_contents) {
    contents <- .generateOutput(x, se, all_memory=all_memory, all_contents=all_contents)
    newpath <- paste0(.getEncodedName(x), ".pdf")

    # These are reasonably satisfactory heuristics:
    # Width = Pixels -> Inches, Height = Bootstrap -> Inches.
    pdf(newpath, width=x[[iSEE:::.organizationHeight]]/75, height=x[[iSEE:::.organizationWidth]]*2)
    print(contents$plot)
    dev.off()

    newpath
})


