#' Acceptable fields for DE panels
#'
#' Set or get the acceptable fields to use for all \linkS4class{Panel} instances related to differential expression,
#' including \linkS4class{VolcanoPlot} and \linkS4class{MAPlot}.
#' These functions are now deprecated.
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
#' .setAcceptablePValueFields("YAY")
#' .getAcceptablePValueFields()
#'
#' # Restoring.
#' .setAcceptablePValueFields(old)
#'
#' @name utils-de
NULL

#' @export
#' @rdname utils-de
.getAcceptablePValueFields <- function() {
    .Deprecated(new="getPValueFields")
    getPValueFields()
}

#' @export
#' @rdname utils-de
.getAcceptableLogFCFields <- function() {
    .Deprecated(new="getLogFCFields")
    getLogFCFields()
}

#' @export
#' @rdname utils-de
.getAcceptableAveAbFields <- function() {
    .Deprecated(new="getAveAbFields")
    getAveAbFields()
}

#' @export
#' @rdname utils-de
.setAcceptablePValueFields <- function(value) {
    .Deprecated(new="setPValueFields")
    setPValueFields(value)
    invisible(NULL)
}

#' @export
#' @rdname utils-de
.setAcceptableLogFCFields <- function(value) {
    .Deprecated(new="setLogFCFields")
    setLogFCFields(value)
    invisible(NULL)
}

#' @export
#' @rdname utils-de
.setAcceptableAveAbFields <- function(value) {
    .Deprecated(new="setAveAbFields")
    setAveAbFields(value)
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
.define_de_status <- function(x, lfc, pval, varname=".de_status") {
    c(
        sprintf(
            "%s <- p.adjust(%s, method=%s) <= %s & abs(%s) >= %s;",
            varname, 
            pval, deparse(x[["PValueCorrection"]]), deparse(x[["PValueThreshold"]]), 
            lfc, deparse(x[["LogFCThreshold"]])
        ),
        sprintf("%s <- %s * sign(%s) + 2L;", varname, varname, lfc)
    )
}

.define_de_validity <- function(object) {
    msg <- character(0)

    p <- object[["PValueThreshold"]]
    if (length(p)!=1 || p <= 0 || p > 1) {
        msg <- c(msg, "'PValueThreshold' must be a numeric scalar in (0, 1]")
    }

    lfc <- object[["LogFCThreshold"]]
    if (length(lfc)!=1 || lfc < 0) {
        msg <- c(msg, "'LogFCThreshold' must be a non-negative numeric scalar")
    }

    corr <- object[["PValueCorrection"]]
    if (length(corr)!=1 || !corr %in% p.adjust.methods) {
        msg <- c(msg, "'PValueCorrection' must be in 'p.adjust.methods'")
    }

    if (any(is.na(object[["PValueFields"]]))) {
        msg <- c(msg, "'PValueFields' should contain non-NA strings")
    }

    if (any(is.na(object[["LogFCFields"]]))) {
        msg <- c(msg, "'LogFCFields' should contain non-NA strings")
    }

    msg
}

.update_de_field_choices <- function(x, choices, all.names, msg) {
    fields <- intersect(all.names, x[[choices]])
    if (length(fields)==0) {
        warning(sprintf("no valid %s fields for '%s'", msg, class(x)[1]))
        return(NULL)
    }
    x[[choices]] <- fields
    x
}

.update_chosen_de_field <- function(x, field, choices) {
    if (!x[[field]] %in% x[[choices]]) {
        x[[field]] <- x[[choices]][1]
    }
    x
}
