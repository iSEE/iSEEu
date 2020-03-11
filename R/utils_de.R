#' Acceptable fields for DE panels
#'
#' Set or get the acceptable fields to use for all \linkS4class{Panel} instances related to differential expression,
#' including \linkS4class{VolcanoPlot} and \linkS4class{MAPlot}.
#'
#' @param PValue,LogFC,AveAb Character vector of acceptable fields (usually in the \code{\link{rowData}}) for a given statistic.
#'
#' @return
#' \code{.acceptablePValueFields} will return a character vector of acceptable names for p-value fields.
#'
#' \code{.acceptableLogFCFields} will return a character vector of acceptable names for log-FC fields.
#'
#' \code{.acceptableAveAbFields} will return a character vector of acceptable names for average abundance fields.
#'
#' \code{.setAcceptableFields} will define the set of acceptable fields.
#' 
#' @author Aaron Lun
#'
#' @examples
#' old <- .acceptablePValueFields()
#' old
#'
#' .setAcceptableFields(PValue="YAY")
#' .acceptablePValueFields()
#'
#' # Restoring.
#' .setAcceptableFields(PValue=old)
#'
#' @name acceptable-de
NULL

#' @export
#' @rdname acceptable-de
.acceptablePValueFields <- function() {
    .acceptable_template("PValue", c("PValue", "p.value", "pval"))
}

#' @export
#' @rdname acceptable-de
.acceptableLogFCFields <- function() {
    .acceptable_template("LogFC", c("logFC", "LogFC"))
}

#' @export
#' @rdname acceptable-de
.acceptableAveAbFields <- function() {
    .acceptable_template("AveAb", c("AveExpr", "logCPM"))
}

.acceptable_template <- function(field, defaults) {
    global <- getOption("iSEEu_de_acceptable", NULL)[[field]]
    if (is.null(global)) {
        defaults
    } else {
        global
    }
}

#' @export
#' @rdname acceptable-de
.setAcceptableFields <- function(PValue=NULL, LogFC=NULL, AveAb=NULL) {
    options(iSEEu_de_acceptable=list(PValue=PValue, LogFC=LogFC, AveAb=AveAb))
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
