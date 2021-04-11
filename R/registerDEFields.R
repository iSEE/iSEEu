#' Register DE-related fields
#'
#' Register the names of fields containing various DE statistics, to populate the user interface of DE-related \linkS4class{Panel}s.
#' 
#' @param se A \linkS4class{SummarizedExperiment} to be visualized with various DE-related Panels.
#' This is expected to have a number of DE-related fields in its \code{\link{rowData}}.
#' @param patterns A character vector containing partial names,
#' to match against the \code{colnames} of the \code{\link{rowData}} to identify relevant fields containing DE statistics.
#' @param fields A character vector containing the names of the relevant fields containing the DE statistics.
#'
#' @return
#' All \code{register} functions will return \code{se}, modified to contain the supplied \code{patterns} or \code{fields}.
#' These will be used as suggestions by DE-related Panels to identify the relevant fields.
#'
#' All \code{get} functions will return a character vector containing the value set by the corresponding \code{register} function;
#' or \code{NULL}, if nothing was set.
#'
#' @author Aaron Lun
#'
#' @details
#' DE-related Panels need to find relevant \code{\link{rowData}} fields containing p-values, log-fold changes, etc. to set appropriate defaults in the user interface.
#' These functions allow a user to tune the definition of what those Panels consider to be relevant,
#' which is occasionally necessary if the DE statistics are stored in a \code{\link{rowData}} field with an usual column name.
#' The idea is to \code{register} the relevant fields in \code{se}, which can then be supplied to \code{\link{iSEE}} with the affected Panels - see Examples.
#' 
#' The registered \code{fields} should be the names of appropriate columns in \code{\link{rowData}} containing continuous variables.
#' Columns containing categorical or non-atomic variables will generally be ignored.
#' For each DE statistic, if any \code{fields} are registered in \code{se}, they will be used directly and \code{patterns} will be ignored.
#' 
#' The registered \code{patterns} are used for partial name matching to the names of appropriate columns of \code{\link{rowData}}.
#' All partial matching must be exact - regular expressions are not supported.
#' Matches can occur anywhere in the name.
#' For example, with \code{"PValue"}, columns with the names \code{"PValue.X"} and \code{"X.PValue"} will be considered acceptable matches.
#'
#' If no \code{patterns} are supplied, the Panels will use the following defaults:
#' \itemize{
#' \item \code{"PValue"}, \code{"pval"} and \code{"p.value"} for the p-values.
#' \item \code{"AveAb"} and \code{"AveExpr"} for the average abundances.
#' \item \code{"LogFC"} and \code{"logFC"} for the log-fold changes.
#' }
#' 
#' @examples
#' # Making up some results with unusual names.
#' se <- SummarizedExperiment(matrix(rnorm(10000), 1000, 10))
#' rownames(se) <- paste0("GENE_", seq_len(nrow(se)))
#' rowData(se)$pvalue <- runif(nrow(se))
#' rowData(se)$lfc <- rnorm(nrow(se))
#' rowData(se)$average <- rnorm(nrow(se))
#'
#' se <- registerPValueFields(se, "pvalue")
#' getPValueFields(se)
#' se <- registerAveAbFields(se, "average")
#' getAveAbFields(se)
#' se <- registerLogFCFields(se, "lfc")
#' getLogFCFields(se)
#'
#' if (interactive()) {
#'     iSEE(se, initial=list(MAPlot()))
#' }
#' 
#' @name registerDEFields
NULL

#' @export
#' @rdname registerDEFields
registerPValueFields <- function(se, fields) .register_de_stuff(se, fields, "PValue", "Fields")

#' @export
#' @rdname registerDEFields
registerAveAbFields <- function(se, fields) .register_de_stuff(se, fields, "AveAb", "Fields")

#' @export
#' @rdname registerDEFields
registerLogFCFields <- function(se, fields) .register_de_stuff(se, fields, "LogFC", "Fields")

#' @export
#' @rdname registerDEFields
registerPValuePatterns <- function(se, patterns) .register_de_stuff(se, patterns, "PValue", "Patterns")

#' @export
#' @rdname registerDEFields
registerAveAbPatterns <- function(se, patterns) .register_de_stuff(se, patterns, "AveAb", "Patterns")

#' @export
#' @rdname registerDEFields
registerLogFCPatterns <- function(se, patterns) .register_de_stuff(se, patterns, "LogFC", "Patterns")

.register_de_stuff <- function(se, value, prefix, type) {
    value <- list(value)
    names(value) <- paste0("iSEEu_", prefix, "_", type)
    do.call(registerAppOptions, c(list(se=se), value))
}

#' @export
#' @rdname registerDEFields
getPValueFields <- function(se) .get_de_stuff(se, "PValue", "Fields")

#' @export
#' @rdname registerDEFields
getAveAbFields <- function(se) .get_de_stuff(se, "AveAb", "Fields")

#' @export
#' @rdname registerDEFields
getLogFCFields <- function(se) .get_de_stuff(se, "LogFC", "Fields")

#' @export
#' @rdname registerDEFields
getPValuePatterns <- function(se) .get_de_stuff(se, "PValue", "Patterns")

#' @export
#' @rdname registerDEFields
getAveAbPatterns <- function(se) .get_de_stuff(se, "AveAb", "Patterns")

#' @export
#' @rdname registerDEFields
getLogFCPatterns <- function(se) .get_de_stuff(se, "LogFC", "Patterns")

.get_de_stuff <- function(se, prefix, type) {
    opt <- paste0("iSEEu_", prefix, "_", type)
    getAppOption(opt, se)
}

# Keeping the old getPValuePattern() and relatives for back-compatibility.
.matchPValueFields <- function(se, available) .match_fields(getPValueFields(se), c(getPValuePattern(), getPValuePatterns(se)), available)

.matchAveAbFields <- function(se, available) .match_fields(getAveAbFields(se), c(getAveAbPattern(), getAveAbPatterns(se)), available)

.matchLogFCFields <- function(se, available) .match_fields(getLogFCFields(se), c(getLogFCPattern(), getLogFCPatterns(se)), available)

.match_fields <- function(fields, patterns, available) {
    if (!is.null(fields)) {
        intersect(fields, available)
    } else {
        okay <- logical(length(available))
        for (x in patterns) {
            okay <- okay | grepl(x, available, fixed=TRUE)
        }
        unique(available[okay])
    }
}
