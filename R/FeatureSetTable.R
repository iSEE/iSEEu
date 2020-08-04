#' Feature set table
#'
#' A table where each row is itself a feature set and can be clicked to transmit a multiple feature selection to another panel.
#'
#' @section Slot overview:
#' The following slots control the feature sets in use:
#' \itemize{
#' \item \code{Collection}, string specifying the type of feature set collection to show.
#' Defaults to the first set.
#' \item \code{CreateCollections}, a named character vector where each entry is named after a feature set collection.
#' Each entry should be a string containing R commands to define a data.frame named \code{tab}, where each row is a feature set and the row names are the names of those sets.
#' \item \code{RetrieveSet}, a named character vector where each entry is named after a feature set collection.
#' Each entry should be a string containing R commands to define a character vector named \code{selected} containing the identity of all rows of the SummarizedExperiment in the set of interest.
#' (These commands can assume that a \code{.set_id} variable is present containing the name of the chosen feature set,
#' as well as the \code{se} variable containing the input SummarizedExperiment object.)
#' }
#'
#' The \code{CreateCollections} and \code{RetrieveSet} parameters cannot be changed inside the \code{iSEE} application.
#' In fact, they cannot even be set in the constructor as they are global to all FeatureSetTable instances.
#' Rather, they are always set to the fields of the same name in the output of \code{\link{getFeatureSetCommands}}.
#' If these fields are also \code{NULL}, we fall back to the output of \code{\link{createGeneSetCommands}} with default parameters.
#'
#' The following slots control the selections:
#' \itemize{
#' \item \code{Selected}, a string containing the name of the currently selected gene set.
#' Defaults to \code{""}, i.e., no selection.
#' \item \code{Search}, a string containing the regular expression for the global search.
#' Defaults to \code{""}, i.e., no search.
#' \item \code{SearchColumns}, a character vector where each entry contains the search string for each column.
#' Defaults to an empty character vector, i.e., no search.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @section Constructor:
#' \code{FeatureSetTable(...)} creates an instance of a FeatureSetTable class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{FeatureSetTable} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"FeatureSetTable"} entry containing \code{available.sets}, a named list of DataFrames containing information about the individual gene sets for each collection.
#' This will also call the equivalent \linkS4class{Panel} method.
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{Collection} with the first valid collection.
#' It also replaces \code{NA} values for \code{Selected} with the first valid set in the chosen collection.
#' This will also call the equivalent \linkS4class{Panel} method.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.fullName}(x)} will return \code{"Gene set table"}.
#' \item \code{\link{.hideInterface}(x)} will return \code{TRUE} for UI elements related to multiple selections,
#' otherwise calling the method for \linkS4class{Panel}.
#' \item \code{\link{.defineOutput}(x)} will return a HTML element containing a \code{\link{datatable}} widget.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all new slots described above, as well as in the parent classes via the \linkS4class{Panel} method.
#' }
#'
#' For creating the table:
#' \itemize{
#' \item \code{\link{.generateOutput}(x, envir)} will create a data.frame of gene set descriptions in \code{envir}, based on the contents of \code{x[["CreateCollections"]]}.
#' It will also return the commands required to do so and the name of the variable corresponding to said data.frame.
#' \item \code{\link{.renderOutput}(x, se, ..., output, pObjects, rObjects)}
#' will add a \code{\link{datatable}} widget to the output,
#' which is used to render the aforementioned data.frame.
#' }
#'
#' For controlling the multiple selections:
#' \itemize{
#' \item \code{\link{.multiSelectionDimension}(x)} returns \code{"row"}.
#' \item \code{\link{.multiSelectionCommands}(x, index)} returns a string specifying the commands to be used to extract the identities of the genes in the currently selected set, based on the contents of \code{x[["RetrieveSet"]]}.
#' \code{index} is ignored.
#' \item \code{\link{.multiSelectionActive}(x)} returns the name of the currently selected gene set,
#' unless no selection is made, in which case \code{NULL} is returned.
#' \item \code{\link{.multiSelectionClear}(x)} returns \code{x} but with the \code{Selected} slot replaced by an empty string.
#' \item \code{\link{.multiSelectionAvailable}(x, contents)} returns \code{contents$available},
#' which is set to the number of features in \code{se}.
#' }
#'
#' @author Aaron Lun
#' @examples
#' library(scRNAseq)
#' sce <- LunSpikeInData(location=FALSE)
#'
#' library(scater)
#' sce <- logNormCounts(sce)
#'
#' library(scran)
#' rowData(sce) <- cbind(rowData(sce), modelGeneVarWithSpikes(sce, "ERCC"))
#'
#' cmds <- createGeneSetCommands(collections="GO",
#'     organism="org.Mm.eg.db", identifier="ENSEMBL")
#' gst <- FeatureSetTable(PanelId=1L, RetrieveSet=cmds$RetrieveSet,
#'     CreateCollections=cmds$CreateCollections)
#'
#' rdp <- RowDataPlot(RowSelectionSource="FeatureSetTable1",
#'     SelectionEffect="Color",
#'     XAxis="Row data", XAxisRowData="mean", YAxis="total")
#'
#' rdt <- RowDataTable(RowSelectionSource="FeatureSetTable1")
#'
#' if (interactive()) {
#'     iSEE(sce, initial=list(gst, rdp, rdt))
#' }
#'
#' @name FeatureSetTable-class
#' @aliases FeatureSetTable FeatureSetTable-class
#' initialize,FeatureSetTable-method
#' .fullName,FeatureSetTable-method
#' .panelColor,FeatureSetTable-method
#' .cacheCommonInfo,FeatureSetTable-method
#' .refineParameters,FeatureSetTable-method
#' .defineDataInterface,FeatureSetTable-method
#' .hideInterface,FeatureSetTable-method
#' .defineOutput,FeatureSetTable-method
#' .generateOutput,FeatureSetTable-method
#' .createObservers,FeatureSetTable-method
#' .renderOutput,FeatureSetTable-method
#' .multiSelectionDimension,FeatureSetTable-method
#' .multiSelectionActive,FeatureSetTable-method
#' .multiSelectionCommands,FeatureSetTable-method
#' .multiSelectionAvailable,FeatureSetTable-method
#' .multiSelectionClear,FeatureSetTable-method
NULL

#' @export
setClass("FeatureSetTable", contains="Panel",
    slots=c(
        Collection="character",
        CreateCollections="character",
        RetrieveSet="character",
        Selected="character",
        Search="character",
        SearchColumns="character"
    )
)

#' @importFrom S4Vectors isSingleString
setValidity2("FeatureSetTable", function(object) {
    msg <- character(0)

    if (length(object[["Collection"]])!=1) {
        msg <- c(msg, "'Collection' should be a single string")
    }

    cre.cmds <- object[["CreateCollections"]]
    ret.cmds <- object[["RetrieveSet"]]
    if (!.needs_filling(cre.cmds) && !.needs_filling(ret.cmds)) {
        nms <- names(cre.cmds)
        if (is.null(nms) || anyDuplicated(nms)) {
            msg <- c(msg, "names of 'CreateCollections' must be non-NULL and unique")
        }
        if (!identical(nms, names(ret.cmds))) {
            msg <- c(msg, "names of 'CreateCollections' and 'RetrieveSet' must be identical")
        }
    }

    if (!isSingleString(object[["Selected"]])) {
        msg <- c(msg, "'Selected' should be a single string")
    }

    if (!isSingleString(object[["Search"]])) {
        msg <- c(msg, "'Search' should be a single string")
    }

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod("initialize", "FeatureSetTable", 
    function(.Object, Collection=NA_character_, Selected="", Search="", SearchColumns=character(0), ...) 
{
    args <- list(..., Collection=Collection, Selected=Selected, Search=Search, SearchColumns=SearchColumns)

    args$CreateCollections <- NA_character_
    args$RetrieveSet <- NA_character_

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
FeatureSetTable <- function(...) {
    new("FeatureSetTable", ...)
}

#' @export
setMethod(".cacheCommonInfo", "FeatureSetTable", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "FeatureSetTable"))) {
        return(se)
    }

    se <- callNextMethod()

    # NOTE: these fields are assumed to be globals, so it's okay to use their
    # values when caching the common values. We also store the commands that we
    # ended up choosing, with the plan to force all FeatureSetTables to use the
    # commands of the first encountered FeatureSetTable in .refineParameters.
    cre.cmds <- x[["CreateCollections"]]
    ret.cmds <- x[["RetrieveSet"]]
    if (.needs_filling(cre.cmds) || .needs_filling(ret.cmds)) {
        stuff <- getFeatureSetCommands()
        if (is.null(stuff)) {
            stuff <- createGeneSetCommands()
        }
        cre.cmds <- stuff$CreateCollections
        ret.cmds <- stuff$RetrieveSet
    }

    created <- lapply(cre.cmds, function(code) {
        env <- new.env()
        eval(parse(text=code), envir=env)
        env$tab
    })

    .setCachedCommonInfo(se, "FeatureSetTable", 
        available.sets=created,
        create.collections.cmds=cre.cmds,
        retrieve.set.cmds=ret.cmds)
})

#' @export
setMethod(".refineParameters", "FeatureSetTable", function(x, se) {
    x[["CreateCollections"]] <- .getCachedCommonInfo(se, "FeatureSetTable")$create.collections.cmds
    x[["RetrieveSet"]] <- .getCachedCommonInfo(se, "FeatureSetTable")$retrieve.set.cmds
    validObject(x)

    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    all.sets <- .getCachedCommonInfo(se, "FeatureSetTable")$available.sets
    if (length(all.sets)==0) {
        warning(sprintf("no feature sets specified for '%s'", class(x)[1]))
        return(NULL)
    }

    if (is.na(coll <- x[["Collection"]]) || !(coll %in% names(all.sets))) {
        x[["Collection"]] <- names(all.sets)[1]
    }

    chosen <- x[["Selected"]]
    setnames <- rownames(all.sets[[x[["Collection"]]]])
    if (is.na(chosen) || (chosen!="" && !chosen %in% setnames)) {
        x[["Selected"]] <- setnames[1]
    }

    x
})

#' @export
setMethod(".fullName", "FeatureSetTable", function(x) "Feature set table")

#' @export
setMethod(".panelColor", "FeatureSetTable", function(x) "#BB00FF")

#' @export
#' @importFrom DT dataTableOutput
setMethod(".defineOutput", "FeatureSetTable", function(x) {
    panel_name <- .getEncodedName(x)
    tagList(
        dataTableOutput(panel_name),
        hr()
    )
})

#' @export
#' @importFrom shiny selectInput
setMethod(".defineDataInterface", "FeatureSetTable", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    all.sets <- .getCachedCommonInfo(se, "FeatureSetTable")$available.sets
    list(
        selectInput(paste0(panel_name, "_Collection"),
            label="Collection:",
            choices=names(all.sets),
            selected=x[["Collection"]]
        ),
        callNextMethod()
    )
})

#' @export
setMethod(".hideInterface", "FeatureSetTable", function(x, field) {
    if (field %in% "SelectionBoxOpen") {
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".generateOutput", "FeatureSetTable", function(x, se, ..., all_memory, all_contents) {
    all.sets <- .getCachedCommonInfo(se, "FeatureSetTable")$available.sets
    current <- x[["Collection"]]

    list(
        commands=list(x[["CreateCollections"]][current]),
        contents=list(table=all.sets[[current]], available=nrow(se)),
        varname="tab"
    )
})

#' @export
#' @importFrom shiny observeEvent
setMethod(".createObservers", "FeatureSetTable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    panel_name <- .getEncodedName(x)

    .createProtectedParameterObservers(panel_name,
        fields="Collection",
        input=input, pObjects=pObjects, rObjects=rObjects)

    # Observer for the DataTable row selection. Note that this needs the
    # ignoreNULL=FALSE in order to acknowledge 'unselection'; however, it
    # _also_ needs ignoreInit=TRUE to avoid wiping out any initial value of
    # 'Selected' due to an empty input at app start.
    select_field <- paste0(panel_name, "_rows_selected")
    observeEvent(input[[select_field]], {
        chosen <- input[[select_field]]

        if (length(chosen)==0L) {
            chosen <- ""
        } else {
            chosen <- rownames(pObjects$contents[[panel_name]]$table)[chosen]
        }

        previous <- pObjects$memory[[panel_name]][["Selected"]]
        if (chosen==previous) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][["Selected"]] <- chosen
        .requestActiveSelectionUpdate(panel_name, session=session, pObjects=pObjects,
            rObjects=rObjects, update_output=FALSE)

    }, ignoreNULL=FALSE, ignoreInit=TRUE)

    # Observer for the search field:
    search_field <- paste0(panel_name, "_search")
    observeEvent(input[[search_field]], {
        search <- input[[search_field]]
        if (identical(search, pObjects$memory[[panel_name]][["Search"]])) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][["Search"]] <- search
    })

    # Observer for the column search fields:
    colsearch_field <- paste0(panel_name, "_search_columns")
    observeEvent(input[[colsearch_field]], {
        search <- input[[colsearch_field]]
        if (identical(search, pObjects$memory[[panel_name]][["SearchColumns"]])) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][["SearchColumns"]] <- search
    })
})

#' @export
#' @importFrom DT renderDataTable datatable selectRows dataTableProxy
setMethod(".renderOutput", "FeatureSetTable", function(x, se, ..., output, pObjects, rObjects) {
    callNextMethod()

    panel_name <- .getEncodedName(x)
    output[[panel_name]] <- renderDataTable({
        .trackUpdate(panel_name, rObjects)
        param_choices <- pObjects$memory[[panel_name]]

        # See comments in ?iSEE:::.create_table_output.
        force(rObjects$rerendered)

        t.out <- .retrieveOutput(panel_name, se, pObjects, rObjects)
        full_tab <- t.out$contents$table

        chosen <- param_choices[["Selected"]]
        search <- param_choices[["Search"]]
        search_col <- param_choices[["SearchColumns"]]
        search_col <- lapply(search_col, FUN=function(x) { list(search=x) })

        # If the existing row in memory doesn't exist in the current table, we
        # don't initialize it with any selection.
        idx <- which(rownames(full_tab)==chosen)[1]
        if (!is.na(idx)) {
            selection <- list(mode="single", selected=idx)
        } else {
            selection <- "single"
        }

        # Clearing the current row selection in 'input', otherwise some madness
        # happens with the observer seeming to respond to the datatable()
        # re-rendering but applying the old value of 'input[[*_rows_selected]]'
        # to the new 'full_tab' - not good.
        selectRows(dataTableProxy(panel_name, deferUntilFlush=FALSE), NULL)

        datatable(
            full_tab, filter="top", rownames=TRUE,
            options=list(
                search=list(search=search, smart=FALSE, regex=TRUE, caseInsensitive=FALSE),
                searchCols=c(list(NULL), search_col), # row names are the first column!
                scrollX=TRUE),
            selection=selection
        )
    })
})

#' @export
setMethod(".multiSelectionDimension", "FeatureSetTable", function(x) "row")

#' @export
setMethod(".multiSelectionCommands", "FeatureSetTable", function(x, index) {
    c(
        sprintf(".set_id <- %s;", deparse(x[["Selected"]])),
        x[["RetrieveSet"]][x[["Collection"]]]
    )
})

#' @export
setMethod(".multiSelectionActive", "FeatureSetTable", function(x) {
    if (nzchar(x[["Selected"]])) {
        x[["Selected"]]
    } else {
        NULL
    }
})

#' @export
setMethod(".multiSelectionClear", "FeatureSetTable", function(x) {
    x[["Selected"]] <- ""
    x
})

#' @export
setMethod(".multiSelectionAvailable", "FeatureSetTable", function(x, contents) {
    contents$available
})
