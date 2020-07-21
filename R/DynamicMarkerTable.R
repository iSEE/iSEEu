#' Dynamic marker table
#'
#' A table that dynamically identifies marker genes for a selected subset of samples.
#' Comparisons are made between the active selection in the transmitting panel
#' and (i) all non-selected points, if no saved selections are available;
#' or (ii) each subset of points in each saved selection.
#'
#' @section Slot overview:
#' The following slots control the test procedure:
#' \itemize{
#' \item \code{LogFC}, a numeric scalar indicating the log-fold change threshold to test against.
#' Defaults to zero.
#' \item \code{TestMethod}, string indicating the test to use (based on the \code{findMarkers} function from \pkg{scran}).
#' This can be \code{"t"} (default), \code{"wilcox"} or \code{"binom"}.
#' \item \code{Assay}, string indicating the assay to use for testing.
#' Defaults to the first named assay in the SummarizedExperiment.
#' }
#' 
#' The following slots control the rendered table:
#' \itemize{
#' \item \code{ExtraFields}, a character vector containing names of \code{\link{rowData}} columns to be included in the table.
#' Set to the output of \code{\link{getTableExtraFields}}.
#' This cannot be changed once the application starts.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{RowTable},
#' \linkS4class{Table} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{DynamicMarkerTable(...)} creates an instance of a DynamicMarkerTable class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{DynamicMarkerTable} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"DynamicMarkerTable"} entry 
#' containing \code{valid.assay.names} and \code{valid.rowdata.names}.
#' This will also call the equivalent \linkS4class{RowTable} method.
#' \item \code{\link{.refineParameters}(x, se)} returns \code{x} after setting \code{"Assay"} to the first valid value.
#' This will also call the equivalent \linkS4class{RowTable} method for further refinements to \code{x}.
#' If valid assay names are not available, \code{NULL} is returned instead.
#' Any \code{"ExtraFields"} are intersected with the valid \code{rowData} names.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.fullName}(x)} will return \code{"Dynamic marker table"}.
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
#' dst <- DynamicMarkerTable(PanelId=1L, PanelWidth=8L,
#'     ColumnSelectionSource="ReducedDimensionPlot1")
#'
#' rdp <- ReducedDimensionPlot(PanelId=1L,
#'     ColorByFeatureSource="DynamicMarkerTable1")
#'
#' if (interactive()) {
#'     iSEE(sce, initial=list(rdp, dst))
#' }
#'
#' @name DynamicMarkerTable-class
#' @aliases DynamicMarkerTable DynamicMarkerTable-class
#' DifferentialStatisticsTable
#' initialize,DynamicMarkerTable-method
#' .fullName,DynamicMarkerTable-method
#' .panelColor,DynamicMarkerTable-method
#' .defineDataInterface,DynamicMarkerTable-method
#' .hideInterface,DynamicMarkerTable-method
#' .generateTable,DynamicMarkerTable-method
#' .createObservers,DynamicMarkerTable-method
#' .cacheCommonInfo,DynamicMarkerTable-method
#' .refineParameters,DynamicMarkerTable-method
#' .multiSelectionInvalidated,DynamicMarkerTable-method
#' .hideInterface,DynamicMarkerTable-method
NULL

#' @export
setClass("DynamicMarkerTable", contains="RowTable",
    slots=c(LogFC="numeric", TestMethod="character", Assay="character", ExtraFields="character"))

#' @importFrom S4Vectors setValidity2
setValidity2("DynamicMarkerTable", function(object) {
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
DynamicMarkerTable <- function(...) {
    new("DynamicMarkerTable", ...)
}

#' @export
DifferentialStatisticsTable <- function(...) {
    .Deprecated(new="DynamicMarkerTable")
    new("DynamicMarkerTable", ...)
}

#' @export
setMethod("initialize", "DynamicMarkerTable", 
    function(.Object, LogFC=0, TestMethod="t", ...)
{
    args <- list(LogFC=LogFC, TestMethod=TestMethod, ...)

    args <- .emptyDefault(args, field="Assay", 
        default=iSEEOptions$get("assay")[1])

    args <- .emptyDefault(args, field="ColumnSelectionDynamicSource", 
        default=iSEEOptions$get("selection.dynamic.multiple"))

    # Clean out any user supplied value and force the class to use the globals.
    args$ExtraFields <- NA_character_

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom shiny numericInput selectInput
setMethod(".defineDataInterface", "DynamicMarkerTable", function(x, se, select_info) {
    plot_name <- .getEncodedName(x)
    cached <- .getCachedCommonInfo(se, "DynamicMarkerTable")

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
            choices=cached$valid.assay.names,
            selected=x[["Assay"]])
    )
})

#' @export
#' @importFrom SummarizedExperiment assayNames rowData
setMethod(".cacheCommonInfo", "DynamicMarkerTable", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "DynamicMarkerTable"))) {
        return(se)
    }

    se <- callNextMethod()

    named_assays <- assayNames(se)
    named_assays <- named_assays[nzchar(named_assays)]

    rdata <- rowData(se)
    valid_rd <- .findAtomicFields(rdata)

    # We determine the valid fields from the first encountered instance of the
    # class, which assumes that 'ExtraFields' is a class-wide constants. (We
    # actually ensure that this is the case by forcibly setting them in
    # .refineParameters later.)
    extras <- x[["ExtraFields"]]
    if (.needs_filling(extras)) {
        extras <- getTableExtraFields() 
    }

    .setCachedCommonInfo(se, "DynamicMarkerTable", 
        valid.assay.names=named_assays, 
        valid.rowdata.names=intersect(extras, valid_rd))
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "DynamicMarkerTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    cached <- .getCachedCommonInfo(se, "DynamicMarkerTable")

    valid.choices <- cached$valid.assay.names
    if (length(valid.choices)==0L) {
        warning(sprintf("no valid 'Assay' detected for '%s'", class(x)[1]))
        return(NULL)
    }
    if (is.na(x[["Assay"]])) {
        x[["Assay"]] <- valid.choices[1]
    }

    # Forcing everyone to use the same globals.
    x[["ExtraFields"]] <- cached$valid.rowdata.names

    # It can't be anything else, really.
    x[["ColumnSelectionType"]] <- "Union"

    x
})

#' @export
setMethod(".createObservers", "DynamicMarkerTable",
    function(x, se, input, session, pObjects, rObjects)
{
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createUnprotectedParameterObservers(plot_name,
        fields=c("LogFC", "TestMethod", "Assay"),
        input=input, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".generateTable", "DynamicMarkerTable", function(x, envir) {
    extras <- x[["ExtraFields"]] 

    if (!exists("col_selected", envir, inherits=FALSE) || !"active" %in% names(envir$col_selected)) {
        commands <- .spawn_empty_cmds(extras)
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
            commands <- .spawn_empty_cmds(extras)
            eval(parse(text=commands), envir=envir)
        } else {
            if (length(extras)) {
                row.anno <- paste0(",\n    row.data=", .define_extra_data(extras))
            } else {
                row.anno <- ""
            }

            stat.cmds <- c(
                sprintf(".de.stats <- scran::findMarkers(assay(se, %s)%s, .grouping,
    direction='up', lfc=%s, test.type=%s%s)",
                    deparse(x[["Assay"]]), subsettor, x[["LogFC"]], deparse(x[["TestMethod"]]), row.anno),
                "tab <- as.data.frame(.de.stats[['active']]);"
            )
            eval(parse(text=stat.cmds), envir=envir)
            commands <- c(spawn.cmds, stat.cmds)
        }
    }

    commands 
})

.define_extra_data <- function(extras, subset="") {
    sprintf("rowData(se)[%s,%s,drop=FALSE]", subset, paste(deparse(extras), collapse=""))
}

.spawn_empty_cmds <- function(extras) {
    empty <- "tab <- data.frame(Top=integer(0), p.value=numeric(0), FDR=numeric(0));"
    if (length(extras)) {
        empty <- c(empty, sprintf("tab <- cbind(as.data.frame(%s), tab);", .define_extra_data(extras, subset="0")))
    }
    empty
}

#' @export
setMethod(".hideInterface", "DynamicMarkerTable", function(x, field) {
    if (field %in% c("RowSelectionSource", "RowSelectionType", "RowSelectionSaved", "RowSelectionDynamicSource")) {
        TRUE
    } else if (field %in% "ColumnSelectionSource") {
        FALSE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".fullName", "DynamicMarkerTable", function(x) "Dynamic marker table")

#' @export
setMethod(".panelColor", "DynamicMarkerTable", function(x) "#B73CE4")
