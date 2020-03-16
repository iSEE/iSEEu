#' Acceptable fields for DE panels
#'
#' Set or get the acceptable fields to use for all \linkS4class{Panel} instances related to differential expression,
#' including \linkS4class{VolcanoPlot} and \linkS4class{MAPlot}.
#'
#' @param value Character vector of acceptable fields (usually in the \code{\link{rowData}}) for a given statistic.
#'
#' @return
#' \code{.getAcceptablePValueFields} will return a character vector of acceptable names for p-value fields.
#'
#' \code{.getAcceptableLogFCFields} will return a character vector of acceptable names for log-FC fields.
#'
#' \code{.getAcceptableAveAbFields} will return a character vector of acceptable names for average abundance fields.
#'
#' The setter functions will define the set of acceptable fields and return \code{NULL} invisibly.
#' 
#' @author Aaron Lun
#'
#' @examples
#' old <- .getAcceptablePValueFields()
#' old
#'
#' .setAcceptableFields(PValue="YAY")
#' .getAcceptablePValueFields()
#'
#' # Restoring.
#' .setAcceptableFields(PValue=old)
#'
#' @name utils-de
NULL

#' @export
#' @rdname utils-de
.getAcceptablePValueFields <- function() {
    .acceptable_template("PValue", c("PValue", "p.value", "pval"))
}

#' @export
#' @rdname utils-de
.getAcceptableLogFCFields <- function() {
    .acceptable_template("LogFC", c("logFC", "LogFC"))
}

#' @export
#' @rdname utils-de
.getAcceptableAveAbFields <- function() {
    .acceptable_template("AveAb", c("AveExpr", "logCPM"))
}

.acceptable_template <- function(field, defaults) {
    global <- getOption(paste0("iSEEu_de_acceptable_", tolower(field)), NULL)
    if (is.null(global)) {
        defaults
    } else {
        global
    }
}

#' @export
#' @rdname utils-de
.setAcceptablePValueFields <- function(value) {
    options(iSEEu_de_acceptable_pvalue=value)
    invisible(NULL)
}

#' @export
#' @rdname utils-de
.setAcceptableLogFCFields <- function(value) {
    options(iSEEu_de_acceptable_logfc=value)
    invisible(NULL)
}

#' @export
#' @rdname utils-de
.setAcceptableAveAbFields <- function(value) {
    options(iSEEu_de_acceptable_aveab=value)
    invisible(NULL)
}

#' @importFrom ggplot2 scale_color_manual
.de_color_scale <- "scale_color_manual(values=c(down='dodgerblue', none='grey', up='salmon'), name='Outcome',
    labels=setNames(sprintf('%s (%s)', c('Down', 'None', 'Up'), tabulate(.de_status, 3)), c('down', 'none', 'up'))) +"

.define_de_priority <- function(envir) {
    cmds <- c(
        ".priority <- factor(plot.data$IsSig, c('none', 'down', 'up'), ordered=TRUE);",
        ".rescaled <- c(none=1, down=2, up=2);"
    )
    eval(parse(text=cmds), envir)
    list(commands=cmds, rescaled=TRUE)
}

#' @importFrom stats p.adjust
.define_de_status <- function(x, lfc, pval) {
    c(
        sprintf(
            ".de_status <- p.adjust(%s, method=%s) <= %s & abs(%s) >= %s;",
            pval, deparse(x[["PValueCorrection"]]), deparse(x[["PValueThreshold"]]), 
            lfc, deparse(x[["LogFCThreshold"]])
        ),
        sprintf(".de_status <- .de_status * sign(%s) + 2L;", lfc),
        "plot.data$IsSig <- c('down', 'none', 'up')[.de_status];"
    )
}
