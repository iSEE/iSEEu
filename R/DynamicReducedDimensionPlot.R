#' Dynamic reduced dimension plot
#'
#' A dimensionality reduction plot that dynamically recomputes the coordinates for the samples,
#' based on the selected subset of samples (and possibly features) in transmitting panels.
#' All samples in active and saved multiple selections are used here.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualization:
#' \itemize{
#' \item \code{Type}, a string specifying the type of dimensionality reduction method to use.
#' This can be \code{"PCA"} (default), \code{"TSNE"} or \code{"UMAP"},
#' which uses the relevant functions from the \pkg{scater} package.
#' \item \code{NGenes}, an integer scalar specifying the number of highly variable genes to use in the dimensionality reduction.
#' Only used if an explicit selection of features is not made in the app.
#' Defaults to 1000.
#' \item \code{Assay}, string indicating the assay to use for the calculations.
#' Defaults to the first named assay in the SummarizedExperiment.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{ColumnDotPlot},
#' \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{DynamicReducedDimensionPlot(...)} creates an instance of a DynamicReducedDimensionPlot class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{DynamicReducedDimensionPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"DynamicReducedDimensionPlot"} entry containing \code{valid.assay.names}.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method.
#' \item \code{\link{.refineParameters}(x, se)} returns \code{x} after setting \code{"Assay"} to the first valid value.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method for further refinements to \code{x}.
#' If valid assay names are not available, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.fullName}(x)} will return \code{"Dynamic reduced dimension plot"}.
##' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all new slots described above, as well as in the parent classes via the \linkS4class{ColumnDotPlot} method.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.generateDotPlotData}(x, envir)} will create a data.frame of newly computed coordinates in \code{envir}.
#' The method will return the commands required to do so as well as a list of labels.
#' }
#'
#' For handling multiple selections:
#' \itemize{
#' \item \code{\link{.multiSelectionInvalidated}(x)} will always return \code{TRUE},
#' as any change in the upstream selection of points will alter the coordinates and invalidate any brush/lasso on \code{x}.
#' }
#'
#' @author Aaron Lun
#' @examples
#' library(scRNAseq)
#' library(scater)
#'
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#' sce <- logNormCounts(sce, exprs_values="tophat_counts")
#' sce <- runPCA(sce, ncomponents=4)
#' sce <- runTSNE(sce)
#'
#' drdp <- DynamicReducedDimensionPlot(PanelId=1L, Assay="logcounts",
#'     ColumnSelectionSource="ReducedDimensionPlot1")
#'
#' if (interactive()) {
#'     iSEE(sce, initial=list(ReducedDimensionPlot(PanelId=1L), drdp))
#' }
#'
#' @name DynamicReducedDimensionPlot-class
#' @aliases DynamicReducedDimensionPlot DynamicReducedDimensionPlot-class
#' initialize,DynamicReducedDimensionPlot-method
#' .fullName,DynamicReducedDimensionPlot-method
#' .panelColor,DynamicReducedDimensionPlot-method
#' .defineDataInterface,DynamicReducedDimensionPlot-method
#' .generateDotPlotData,DynamicReducedDimensionPlot-method
#' .createObservers,DynamicReducedDimensionPlot-method
#' .cacheCommonInfo,DynamicReducedDimensionPlot-method
#' .refineParameters,DynamicReducedDimensionPlot-method
#' .multiSelectionInvalidated,DynamicReducedDimensionPlot-method
NULL

#' @export
setClass("DynamicReducedDimensionPlot", contains="ColumnDotPlot",
    slots=c(NGenes="integer", Type="character", Assay="character"))

#' @importFrom S4Vectors setValidity2 isSingleString
setValidity2("DynamicReducedDimensionPlot", function(object) {
    msg <- character(0)

    if (length(n <- object[["NGenes"]])!=1L || n < 1L) {
        msg <- c(msg, "'NGenes' must be a positive integer scalar")
    }

    if (!isSingleString(val <- object[["Type"]]) || !val %in% c("PCA", "TSNE", "UMAP")) {
        msg <- c(msg, "'Type' must be one of 'TSNE', 'PCA' or 'UMAP'")
    }

    if (length(object[["Assay"]])!=1) {
        msg <- c(msg, "'Assay' must be a single string")
    }

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod("initialize", "DynamicReducedDimensionPlot", function(.Object, Type="PCA", NGenes=1000L, Assay=NA_character_, ...)
    callNextMethod(.Object, Type=Type, NGenes=NGenes, Assay=Assay, ...))

#' @export
#' @importFrom methods new
DynamicReducedDimensionPlot <- function(...) {
    new("DynamicReducedDimensionPlot", ...)
}

#' @export
setMethod(".fullName", "DynamicReducedDimensionPlot", function(x) "Dynamic reduced dimension plot")

#' @export
setMethod(".panelColor", "DynamicReducedDimensionPlot", function(x) "#0F0F0F")

#' @export
setMethod(".defineDataInterface", "DynamicReducedDimensionPlot", function(x, se, select_info) {
    plot_name <- .getEncodedName(x)

    list(
        selectInput(paste0(plot_name, "_Type"), label="Type:",
            choices=c("PCA", "TSNE", "UMAP"), selected=x[["Type"]]),
        numericInput(paste0(plot_name, "_NGenes"), label="Number of HVGs:",
            min=1, value=x[["NGenes"]]),
        selectInput(paste0(plot_name, "_Assay"),
            label="Assay",
            choices=.getCachedCommonInfo(se, "DynamicReducedDimensionPlot")$valid.assay.names,
            selected=x[["Assay"]])
    )
})

#' @export
#' @importFrom SummarizedExperiment assayNames
setMethod(".cacheCommonInfo", "DynamicReducedDimensionPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "DynamicReducedDimensionPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    named_assays <- assayNames(se)
    named_assays <- named_assays[named_assays!=""]
    .setCachedCommonInfo(se, "DynamicReducedDimensionPlot", valid.assay.names=named_assays)
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "DynamicReducedDimensionPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    valid.choices <- .getCachedCommonInfo(se, "DynamicReducedDimensionPlot")$valid.assay.names
    if (length(valid.choices)==0L) {
        warning(sprintf("no valid 'Assay' detected for '%s'", class(x)[1]))
        return(NULL)
    }

    if (is.na(x[["Assay"]])) {
        x[["Assay"]] <- valid.choices[1]
    }

    x
})

#' @export
setMethod(".createObservers", "DynamicReducedDimensionPlot",
    function(x, se, input, session, pObjects, rObjects)
{
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
        fields=c("Type", "NGenes"),
        input=input, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".generateDotPlotData", "DynamicReducedDimensionPlot", function(x, envir) {
    commands <- character(0)

    if (!exists("col_selected", envir=envir, inherits=FALSE) || sum(lengths(envir$col_selected)) <= 2L) {
        commands <- c(commands,
            "plot.data <- data.frame(X=numeric(0), Y=numeric(0));")
    } else {
        commands <- c(commands,
            ".chosen <- unique(unlist(col_selected));",
            "set.seed(100000)", # to avoid problems with randomization.
            sprintf(".coords <- scater::calculate%s(assay(se, %s)[,.chosen], ntop=%i, ncomponents=2);",
                x[["Type"]], deparse(x[["Assay"]]), x[["NGenes"]]),
            "plot.data <- data.frame(.coords, row.names=.chosen);",
            "colnames(plot.data) <- c('X', 'Y');"
        )
    }

    commands <- c(commands,
        "plot.data <- plot.data[colnames(se),,drop=FALSE];",
        "rownames(plot.data) <- colnames(se);")

    eval(parse(text=commands), envir=envir)

    list(data_cmds=commands, plot_title=sprintf("Dynamic %s plot", x[["Type"]]),
        x_lab=paste0(x[["Type"]], "1"), y_lab=paste0(x[["Type"]], "2"))
})

#' @export
setMethod(".multiSelectionInvalidated", "DynamicReducedDimensionPlot", function(x) TRUE)
