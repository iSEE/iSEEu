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
#' Set the commands to define the global collection of feature sets.
#' This is deprecated in favor of \code{\link{registerFeatureSetCommands}}.
#'
#' @param value A list of two character vectors named \code{"collections"} and \code{"sets"}.
#' Both vectors should be of the same length and have the same names.
#' Vectors should contain R commands to create collections and retrieve sets;
#' see \code{?\linkS4class{FeatureSetTable}} and the output of \code{\link{createGeneSetCommands}} for details.
#'
#' @return
#' \code{setFeatureSetCommands} will set the current global feature set commands and return \code{NULL} invisibly.
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
setFeatureSetCommands <- function(value) {
    .globals$set("FeatureSetCommands", value)
}

#' Global extra table fields
#'
#' Get or set the names of the extra fields to include in a table.
#'
#' @param value A character vector containing the names of extra fields to include.
#'
#' @return
#' \code{getTableExtraFields} returns the current global extra table fields.
#'
#' \code{setTableExtraFields} will set the current global extra table fields and return \code{NULL} invisibly.
#'
#' @details
#' These utilities allow users to easily set the feature set commands for all \linkS4class{DynamicMarkerTable}s at once.
#' Any global settings only take effect (i) during setup of the \code{\link{iSEE}} application
#' and (ii) if the first \linkS4class{DynamicMarkerTable} does not have an existing values in the \code{"TableExtraFields"} slots.
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
    out <- .globals$get("TableExtraFields")
    if (is.null(out)) {
        out <- character()
    }
    out
}

#' @export
#' @rdname global-TableExtraFields
setTableExtraFields <- function(value) {
    .globals$set("TableExtraFields", value)
}

#' Global DE prefixes
#'
#' Get or set patterns for acceptable names of \code{\link{rowData}} columns related to a differential expression analysis.
#' These functions are deprecated; use their counterparts in \code{?"\link{registerPValuePatterns}"} instead.
#'
#' @param value A character vector containing the acceptable prefixes for each statistic.
#'
#' @return
#' \code{getPValuePattern} returns the patterns for acceptable column names for p-values.
#'
#' \code{getLogFCPattern} returns the patterns for acceptable column names for log-fold changes.
#'
#' \code{getAveAbPattern} returns the patterns for acceptable column names for the average abundances.
#'
#' The corresponding setters set the global parts for each statistic and return \code{NULL} invisibly.
#'
#' @details
#' These utilities allow users to easily get and set the patterns of acceptable fields in all
#' \linkS4class{VolcanoPlot}s, \linkS4class{MAPlot}s and \linkS4class{LogFCLogFCPlot}s at once.
#' Any global settings only take effect (i) during setup of the \code{\link{iSEE}} application
#' and (ii) if the first panel of each class does not have existing values in the
#' \code{"PValueFields"}, \code{"LogFCFields"} or \code{"AveAbFields"} slots (which take precedence if present).
#'
#' Each of these global settings are treated as \emph{patterns} for partial matching.
#' For the \code{"PValue"} pattern, columns with the names \code{"PValue.X"} and \code{"X.PValue"} will be considered acceptable matches.
#' All partial matching must be exact - regular expressions are not supported.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \linkS4class{VolcanoPlot}, \linkS4class{MAPlot} and \linkS4class{LogFCLogFCPlot},
#' which are affected by these globals.
#'
#' @examples
#' old <- getPValuePattern()
#'
#' setPValuePattern(LETTERS)
#' getPValuePattern()
#'
#' setPValuePattern(old)
#' @export
#' @rdname globals-PValuePattern
getPValuePattern <- function() {
    .acceptable_template("PValuePattern", c("PValue", "p.value", "pval"))
}

#' @export
#' @rdname globals-PValuePattern
getLogFCPattern <- function() {
    .acceptable_template("LogFCPattern", c("logFC", "LogFC"))
}

#' @export
#' @rdname globals-PValuePattern
getAveAbPattern <- function() {
    .acceptable_template("AveAbPattern", c("AveExpr", "logCPM"))
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
#' @rdname globals-PValuePattern
setPValuePattern <- function(value) {
    .Deprecated(new="registerPValuePatterns")
    .globals$set("PValuePattern", value)
    invisible(NULL)
}

#' @export
#' @rdname globals-PValuePattern
setLogFCPattern <- function(value) {
    .Deprecated(new="registerLogFCPatterns")
    .globals$set("LogFCPattern", value)
    invisible(NULL)
}

#' @export
#' @rdname globals-PValuePattern
setAveAbPattern <- function(value) {
    .Deprecated(new="registerAveAbPatterns")
    .globals$set("AveAbPattern", value)
    invisible(NULL)
}
