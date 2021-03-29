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
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing the steps of a panel-specific tour.
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
#' .definePanelTour,DynamicReducedDimensionPlot-method
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

    .addSpecificTour(class(x), "Type", function(plot_name) {
        data.frame(
            element=paste0("#", plot_name, "_Type + .selectize-control"),
            intro="We can control the type of dimensionality reduction to compute on the selected samples. Currently there are only the usual choices of PCA, t-SNE or UMAP."
        )
    })

    .addSpecificTour(class(x), "NGenes", function(plot_name) {
        data.frame(
            element=paste0("#", plot_name, "_NGenes"),
            intro = "We perform the calculations on only the most highly variable features in the dataset, to speed up the calculation and to reduce noise at the cost of potentially discarding some biological signal. Here, we can control the exact number of genes to vary the speed-signal-noise trade-off.")
    })

    .addSpecificTour(class(x), "Assay", function(plot_name) {
        data.frame(
            element=paste0("#", .getEncodedName(x), "_Assay + .selectize-control"),
            intro = "Here, we can change the assay values to use for the dimensionality reduction. We suggest using log-transformed normalized values (or something equivalent) where the differences between values have some meaning. For log-values, the differences can be nominally interpreted as log-fold changes.")
    })

    list(
        .selectInput.iSEE(x, "Type", label="Type:",
            choices=c("PCA", "TSNE", "UMAP"), selected=x[["Type"]]),
        .numericInput.iSEE(x, "NGenes", label="Number of HVGs:",
            min=1, value=x[["NGenes"]]),
        .selectInput.iSEE(x, "Assay", label="Assay:",
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
    named_assays <- named_assays[nzchar(named_assays)]
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
        fields=c("Type", "NGenes", "Assay"),
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

#' @export
setMethod(".definePanelTour", "DynamicReducedDimensionPlot", function(x) {
    rbind(
        c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">Dynamic reduced dimension plot</font> panel performs dimensionality reduction on the samples selected in another panel. Each point here corresponds to a sample in our <code>SummarizedExperiment</code>.", .getPanelColor(x))),
        c(paste0("#", .getEncodedName(x), "_DataBoxOpen"), "The <i>Data parameters</i> box shows the available parameters that can be tweaked in this plot.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        callNextMethod()
    )
})
