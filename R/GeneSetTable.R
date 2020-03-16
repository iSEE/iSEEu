#' Gene set table
#'
#' A table where each row is a gene set and can be clicked to transmit a multiple feature selection to another panel.
#' This usually requires some set-up with \code{\link{.setIdentifierType}} and related functions, see Examples.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualization:
#' \itemize{
#' \item \code{Type}, string specifying the type of gene set collection to show.
#' Defaults to \code{"GO"}.
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
#' \code{GeneSetTable(...)} creates an instance of a GeneSetTable class, 
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{DifferentialStatisticsTable} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#' 
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.fullName}(x)} will return \code{"Gene set table"}.
#' \item \code{\link{.hideInterface}(x)} will return \code{TRUE} for UI elements related to multiple selections,
#' otherwise calling the method for \linkS4class{Panel}.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all new slots described above, as well as in the parent classes via the \linkS4class{Panel} method.
#' }
#'
#' For creating the table:
#' \itemize{
#' \item \code{\link{.generateOutput}(x, envir)} will create a data.frame of gene set descriptions in \code{envir},
#' based on the \code{mode="show"} output of \code{\link{.geneSetCommands}}.
#' It will also return the commands required to do so.
#' \item \code{\link{.renderOutput}(x, se, ..., output, pObjects, rObjects)} 
#' will add a \code{\link{datatable}} widget to the output, 
#' which is used to render the aforementioned data.frame.
#' }
#'
#' For controlling the multiple selections:
#' \itemize{
#' \item \code{\link{.multiSelectionDimension}(x)} returns \code{"row"}.
#' \item \code{\link{.multiSelectionCommands}(x, index)} returns a string specifying the commands to be used to extract the identities of the genes in the currently selected set, based on the \code{mode="extract"} output of \code{\link{.geneSetCommands}}.
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
#' sce <- LunSpikeInData()
#'
#' library(scater)
#' sce <- logNormCounts(sce)
#'
#' library(scran)
#' rowData(sce) <- cbind(rowData(sce), modelGeneVarWithSpikes(sce, "ERCC"))
#'
#' # This defaults to 'org.Hs.eg.db' with 'ENTREZID'.
#' .setOrganism("org.Mm.eg.db")
#' .setIdentifierType("ENSEMBL")
#' gst <- GeneSetTable(PanelId=1L)
#'
#' rdp <- RowDataPlot(RowSelectionSource="GeneSetTable1",
#'     SelectionEffect="Color",
#'     XAxis="Row data", XAxisRowData="mean", YAxis="total")
#'
#' rdt <- RowDataTable(RowSelectionSource="GeneSetTable1")
#'
#' if (interactive()) {
#'     iSEE(sce, initial=list(gst, rdp, rdt))
#' }
#'
#' @name GeneSetTable-class
#' @aliases GeneSetTable GeneSetTable-class
#' .fullName,GeneSetTable-method
#' .panelColor,GeneSetTable-method
#' .defineDataInterface,GeneSetTable-method
#' .defineOutput,GeneSetTable-method
#' .generateOutput,GeneSetTable-method
#' .createObservers,GeneSetTable-method
#' .renderOutput,GeneSetTable-method
#' .multiSelectionDimension,GeneSetTable-method 
#' .multiSelectionActive,GeneSetTable-method 
#' .multiSelectionCommands,GeneSetTable-method 
#' .multiSelectionAvailable,GeneSetTable-method 
#' .multiSelectionClear,GeneSetTable-method 
NULL

#' @export
setClass("GeneSetTable", contains="Panel",
    slots=c(
        Type="character",
        Selected="character",
        Search="character",
        SearchColumns="character"
    )
)

#' @importFrom S4Vectors isSingleString
setValidity2("GeneSetTable", function(object) {
    msg <- character(0)

    if (!isSingleString(type <- object[["Type"]])) {
        msg <- c(msg, sprintf("'Organism' should be a single string", orgdb))
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
setMethod("initialize", "GeneSetTable", function(.Object, Type="GO", Selected="", Search="", SearchColumns=character(0), ...)
    callNextMethod(.Object, Type=Type, Selected=Selected, Search=Search, SearchColumns=SearchColumns, ...))

#' @export
GeneSetTable <- function(...) {
    new("GeneSetTable", ...)
}

#' @export
setMethod(".fullName", "GeneSetTable", function(x) "Gene set table")

#' @export
setMethod(".panelColor", "GeneSetTable", function(x) "#BB00FF")

#' @export
#' @importFrom DT dataTableOutput
setMethod(".defineOutput", "GeneSetTable", function(x, ...) {
    panel_name <- .getEncodedName(x)
    dataTableOutput(panel_name)
})

#' @export
#' @importFrom shiny selectInput 
setMethod(".defineDataInterface", "GeneSetTable", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    list(
        selectInput(paste0(panel_name, "_Type"),
            label="Gene set type:",
            choices=.list_available_gene_sets(),
            selected=x[["Type"]]
        )
    )
})

#' @export
setMethod(".hideInterface", "GeneSetTable", function(x, field) {
    if (field %in% "SelectBoxOpen") {
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".generateOutput", "GeneSetTable", function(x, se, ..., all_memory, all_contents) {
    envir <- new.env()
    commands <- .getGeneSetCommands(x[["Type"]], mode="show")
    eval(parse(text=commands), envir=envir)
    list(
        commands=list(commands),
        contents=list(table=envir$tab, available=nrow(se))
    )
})

#' @export
#' @importFrom shiny observeEvent
setMethod(".createObservers", "GeneSetTable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    panel_name <- .getEncodedName(x)

    .createProtectedParameterObservers(panel_name,
        fields="Type",
        input=input, pObjects=pObjects, rObjects=rObjects)

    # Observer for the DataTable row selection:
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
    }, ignoreNULL=FALSE)

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
setMethod(".renderOutput", "GeneSetTable", function(x, se, ..., output, pObjects, rObjects) {
    callNextMethod()

    panel_name <- .getEncodedName(x)
    output[[panel_name]] <- renderDataTable({
        .trackUpdate(panel_name, rObjects)
        param_choices <- pObjects$memory[[panel_name]]

        t.out <- .retrieveOutput(panel_name, se, pObjects, rObjects)
        full_tab <- t.out$contents$table
        pObjects$varname[[panel_name]] <- "tab"

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
setMethod(".multiSelectionDimension", "GeneSetTable", function(x) "row")

#' @export
setMethod(".multiSelectionCommands", "GeneSetTable", function(x, index) {
    sprintf(.getGeneSetCommands(x[["Type"]], mode="extract"), deparse(x[["Selected"]]))
})

#' @export
setMethod(".multiSelectionActive", "GeneSetTable", function(x) {
    if (x[["Selected"]]!="") {
        x[["Selected"]]
    } else {
        NULL
    }
})

#' @export
setMethod(".multiSelectionClear", "GeneSetTable", function(x) {
    x[["Selected"]] <- ""
    x
})

#' @export
setMethod(".multiSelectionAvailable", "GeneSetTable", function(x, contents) {
    contents$available
})
