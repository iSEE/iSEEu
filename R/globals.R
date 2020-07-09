.globals <- (function() {
    current <- list()
    list(
        get=function(name) current[[name]],
        set=function(name, value) {
            current[[name]] <<- value
            invisible(NULL)
        }
    )
})()

#' Global feature set commands
#'
#' Get or set the commands to define the global collection of feature sets.
#'
#' @param value A list of two character vectors named \code{"CreateCollections"} and \code{"RetrieveSet"}.
#' Both vectors should be of the same length and have the same names.
#' Vectors should contain R commands to create collections and retrieve sets;
#' see \code{?\linkS4class{FeatureSetTable}} and the output of \code{\link{.createGeneSetCommands}} for details.
#'
#' @return 
#' \code{getFeatureSetCommands} returns the current global feature set commands.
#'
#' \code{setFeatureSetCommands} will set the current global feature set commands and return \code{NULL} invisibly.
#'
#' @details
#' By setting these values, all subsequent constructions of \linkS4class{FeatureSetTable} will have the same set of commands in their \code{"CreateCollections"} and \code{"RetrieveSet"} slots.
#' This allows users to easily customize all \linkS4class{FeatureSetTable} parameters at once.
#' Note that it only applies during the construction of the \linkS4class{FeatureSetTable} and has no effect on the \code{iSEE} application once it starts.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{createGeneSetCommands}}, for one method of generating \code{value}.
#'
#' @examples
#' old <- getFeatureSetCommands()
#'
#' new.cmds <- createGeneSetCommands(organism="org.Mm.eg.db", 
#'     identifier="SYMBOL")
#' setFeatureSetCommands(new.cmds)
#'
#' getFeatureSetCommands()
#'
#' setFeatureSetCommands(old) 
#' @export
#' @rdname global-FeatureSetCommands
getFeatureSetCommands <- function() {
    .globals$get("FeatureSetCommands")
}

#' @export
#' @rdname global-FeatureSetCommands
setFeatureSetCommands <- function(value) {
    .globals$set("FeatureSetCommands", value)
}

#' Global extra table fields
#'
#' Get or set the names of the extra \code{\link{rowData}} or \code{\link{colData}} fields to include in a table. 
#'
#' @param value A character vector containing the names of extra fields to include.
#'
#' @return
#' \code{getTableExtraFields} returns the current global extra table fields.
#'
#' \code{setTableExtraFields} will set the current global extra table fields and return \code{NULL} invisibly.
#'
#' @details
#' By setting these values, all subsequent constructions of \linkS4class{DynamicMarkerTable} will have the same set of extra fields specified in their \code{"ExtraFields"} slot.
#' This allows users to easily customize all \linkS4class{DynamicMarkerTable} parameters at once.
#' Note that it only applies during the construction of the \linkS4class{DynamicMarkerTable} and has no effect on the \code{iSEE} application once it starts.
#'
#' @author Aaron Lun
#'
#' @examples
#' old <- getTableExtraFields()
#'
#' setTableExtraFields(LETTERS)
#' getTableExtraFields()
#'
#' setTableExtraFields(old)
#' @export
#' @rdname global-TableExtraFields
getTableExtraFields <- function() {
    .globals$get("TableExtraFields")
}

#' @export
#' @rdname global-TableExtraFields
setTableExtraFields <- function(value) {
    .globals$set("TableExtraFields", value)
}

#' Global DE fields
#'
#' Get or set the names of \code{\link{rowData}} columns related to a differential expression analysis.
#'
#' @param value A character vector containing the names of acceptable columns for each statistic.
#'
#' @return
#' \code{getPValueFields} returns the acceptable column names for p-values.
#'
#' \code{getLogFCFields} returns the acceptable column names for log-fold changes.
#' 
#' \code{getAveAbFields} returns the acceptable column names for the average abundances.
#'
#' The corresponding setters set the global column names for each statistic and return \code{NULL} invisibly.
#' 
#' @author Aaron Lun
#'
#' @seealso
#' \linkS4class{VolcanoPlot} and \linkS4class{MAPlot}, which are affected by these globals.
#'
#' @examples
#' old <- getPValueFields()
#'
#' setPValueFields(LETTERS)
#' getPValueFields()
#'
#' setPValueFields(old)
#' @export
#' @rdname globals-PValueFields
getPValueFields <- function() {
    .acceptable_template("PValueFields", c("PValue", "p.value", "pval"))
}

#' @export
#' @rdname globals-PValueFields
getLogFCFields <- function() {
    .acceptable_template("LogFCFields", c("logFC", "LogFC"))
}

#' @export
#' @rdname globals-PValueFields
getAveAbFields <- function() {
    .acceptable_template("AveAbFields", c("AveExpr", "logCPM"))
}

.acceptable_template <- function(field, defaults) {
    global <- .globals$get(field)
    if (is.null(global)) {
        defaults
    } else {
        global
    }
}

#' @export
#' @rdname globals-PValueFields
setPValueFields <- function(value) {
    .globals$set(PValueFields=value)
    invisible(NULL)
}

#' @export
#' @rdname globals-PValueFields
setLogFCFields <- function(value) {
    .globals$set(LogFCFields=value)
    invisible(NULL)
}

#' @export
#' @rdname globals-PValueFields
setAveAbFields <- function(value) {
    .globals$set(AveAbFields=value)
    invisible(NULL)
}
