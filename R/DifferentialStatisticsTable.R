#' Differential statistics table
#'
#' A table that dynamically computes differential statistics based on a selected subset of samples.
#' Comparisons are made between the active selection in the transmitting panel
#' and (i) all non-selected points, if no saved selections are available;
#' or (ii) each subset of points in each saved selection.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualization:
#' \itemize{
#' \item \code{LogFC}, a numeric scalar indicating the log-fold change threshold to test against.
#' Defaults to zero.
#' \item \code{TestMethod}, string indicating the test to use (based on the \code{findMarkers} function from \pkg{scran}).
#' This can be \code{"t"} (default), \code{"wilcox"} or \code{"binom"}.
#' \item \code{Assay}, string indicating the assay to use for testing.
#' Defaults to the first named assay in the SummarizedExperiment.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{RowTable},
#' \linkS4class{Table} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{DifferentialStatisticsTable(...)} creates an instance of a DifferentialStatisticsTable class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{DifferentialStatisticsTable} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"DifferentialStatisticsTable"} entry containing \code{valid.assay.names}.
#' This will also call the equivalent \linkS4class{RowTable} method.
#' \item \code{\link{.refineParameters}(x, se)} returns \code{x} after setting \code{"Assay"} to the first valid value.
#' This will also call the equivalent \linkS4class{RowTable} method for further refinements to \code{x}.
#' If valid assay names are not available, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.fullName}(x)} will return \code{"Differential statistics table"}.
#' \item \code{\link{.hideInterface}(x)} will return \code{TRUE} for UI elements related to multiple row selections,
#' otherwise calling the method for \linkS4class{RowTable}.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all new slots described above, as well as in the parent classes via the \linkS4class{RowTable} method.
#' }
#'
#' For creating the table:
#' \itemize{
#' \item \code{\link{.generateTable}(x, envir)} will create a data.frame of newly computed statistics in \code{envir}.
#' The method will return the commands required to do so.
#' }
#'
#' @examples
#' library(scRNAseq)
#' library(scater)
#'
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#' sce <- logNormCounts(sce, exprs_values="tophat_counts")
#' sce <- runPCA(sce, ncomponents=4)
#' sce <- runTSNE(sce)
#'
#' dst <- DifferentialStatisticsTable(PanelId=1L, PanelWidth=8L,
#'     ColumnSelectionSource="ReducedDimensionPlot1")
#'
#' rdp <- ReducedDimensionPlot(PanelId=1L,
#'     ColorByFeatureSource="DifferentialStatisticsTable1")
#'
#' if (interactive()) {
#'     iSEE(sce, initial=list(rdp, dst))
#' }
#'
#' @name DifferentialStatisticsTable-class
#' @aliases DifferentialStatisticsTable DifferentialStatisticsTable-class
#' initialize,DifferentialStatisticsTable-method
#' .fullName,DifferentialStatisticsTable-method
#' .panelColor,DifferentialStatisticsTable-method
#' .defineDataInterface,DifferentialStatisticsTable-method
#' .hideInterface,DifferentialStatisticsTable-method
#' .generateTable,DifferentialStatisticsTable-method
#' .createObservers,DifferentialStatisticsTable-method
#' .cacheCommonInfo,DifferentialStatisticsTable-method
#' .refineParameters,DifferentialStatisticsTable-method
#' .multiSelectionInvalidated,DifferentialStatisticsTable-method
#' .hideInterface,DifferentialStatisticsTable-method
NULL

#' @export
setClass("DifferentialStatisticsTable", contains="RowTable",
    slots=c(LogFC="numeric", TestMethod="character", Assay="character"))

#' @importFrom S4Vectors setValidity2
setValidity2("DifferentialStatisticsTable", function(object) {
    msg <- character(0)

    if (length(val <- object[["LogFC"]])!=1L || val < 0) {
        msg <- c(msg, "'NGenes' must be a non-negative number")
    }

    if (!isSingleString(val <- object[["TestMethod"]]) || !val %in% c("t", "wilcox", "binom")) {
        msg <- c(msg, "'TestMethod' must be in 't', 'wilcox' or 'binom'")
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
#' @importFrom methods new
DifferentialStatisticsTable <- function(...) {
    new("DifferentialStatisticsTable", ...)
}

#' @export
setMethod("initialize", "DifferentialStatisticsTable", function(.Object, LogFC=0, TestMethod="t", Assay=NA_character_, ...)
    callNextMethod(.Object, LogFC=LogFC, ColumnSelectionType="Union", TestMethod=TestMethod, Assay=Assay, ...))

#' @export
#' @importFrom shiny numericInput selectInput
setMethod(".defineDataInterface", "DifferentialStatisticsTable", function(x, se, select_info) {
    plot_name <- .getEncodedName(x)
    list(
        numericInput(paste0(plot_name, "_LogFC"),
            label="Log-FC threshold",
            min=0,
            value=x[["LogFC"]]),
        selectInput(paste0(plot_name, "_TestMethod"),
            label="Test method",
            choices=c(`t-test`="t", `Wilcoxon rank sum`="wilcox", `Binomial test`="binom"),
            selected=x[["TestMethod"]]),
        selectInput(paste0(plot_name, "_Assay"),
            label="Assay",
            choices=.getCachedCommonInfo(se, "DifferentialStatisticsTable")$valid.assay.names,
            selected=x[["Assay"]])
    )
})

#' @export
#' @importFrom SummarizedExperiment assayNames
setMethod(".cacheCommonInfo", "DifferentialStatisticsTable", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "DifferentialStatisticsTable"))) {
        return(se)
    }

    se <- callNextMethod()

    named_assays <- assayNames(se)
    named_assays <- named_assays[nzchar(named_assays)]
    .setCachedCommonInfo(se, "DifferentialStatisticsTable", valid.assay.names=named_assays)
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "DifferentialStatisticsTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    valid.choices <- .getCachedCommonInfo(se, "DifferentialStatisticsTable")$valid.assay.names
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
setMethod(".createObservers", "DifferentialStatisticsTable",
    function(x, se, input, session, pObjects, rObjects)
{
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createUnprotectedParameterObservers(plot_name,
        fields=c("LogFC", "TestMethod", "Assay"),
        input=input, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".generateTable", "DifferentialStatisticsTable", function(x, envir) {
    empty <- "tab <- data.frame(Top=integer(0), p.value=numeric(0), FDR=numeric(0));"

    if (!exists("col_selected", envir, inherits=FALSE) || !"active" %in% names(envir$col_selected)) {
        commands <- empty
        eval(parse(text=commands), envir=envir)
    } else {
        if (length(envir$col_selected)<2L) {
            spawn.cmds <- ".grouping <- ifelse(colnames(se) %in% col_selected$active, 'active', 'other')";
            subsettor <- ""
        } else {
            spawn.cmds <- c(".chosen <- unlist(col_selected);",
                ".grouping <- rep(names(col_selected), lengths(col_selected));")
            subsettor <- "[,.chosen,drop=FALSE]"
        }

        eval(parse(text=spawn.cmds), envir)

        # Check that there actually are two groups, otherwise this bit fails hard.
        if (length(unique(envir$.grouping)) < 2L) {
            commands <- empty
            eval(parse(text=commands), envir=envir)
        } else {
            stat.cmds <- c(
                sprintf(".de.stats <- scran::findMarkers(assay(se, %s)%s, .grouping,
    direction='up', lfc=%s, test.type=%s)",
                    deparse(x[["Assay"]]), subsettor, x[["LogFC"]], deparse(x[["TestMethod"]])),
                "tab <- as.data.frame(.de.stats[['active']]);"
            )
            eval(parse(text=stat.cmds), envir=envir)
            commands <- c(spawn.cmds, stat.cmds)
        }
    }

    commands 
})

#' @export
setMethod(".hideInterface", "DifferentialStatisticsTable", function(x, field) {
    if (field %in% c("RowSelectionSource", "RowSelectionType", "RowSelectionSaved", "RowSelectionDynamicSource")) {
        TRUE
    } else if (field %in% "ColumnSelectionSource") {
        FALSE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".fullName", "DifferentialStatisticsTable", function(x) "Differential statistics table")

#' @export
setMethod(".panelColor", "DifferentialStatisticsTable", function(x) "#B73CE4")
