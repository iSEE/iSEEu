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
        ".rescaled <- c(none=1, down=2, up=2);",
        ".priority <- factor(plot.data$IsSig, names(.rescaled), ordered=TRUE);"
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

.define_de_validity <- function(object, allow.na.fields=FALSE) {
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

    msg
}


#' Update global field choices
#'
#' Update the global-responsive acceptable choices for a particular field of a \linkS4class{Panel} class.
#'
#' @param x A \linkS4class{Panel} object.
#' @param field String specifying the name of the class-constant slot holding the acceptable values.
#' @param choices Character vector containing the current acceptable values, usually defined in a manner that responds to globals.
#'
#' @details
#' We use globals to set class-wide parameters in an efficient and reliable manner (well, relative to manual synchronization).
#' Our task is to (i) respond to globals within an R session while (ii) ensuring that the memory is enough to reproduce an app.
#' This means that any globals must act \emph{through} the memory, otherwise they are lost in a new session where the globals are not set.
#' 
#' The solution is to:
#' \enumerate{
#' \item Set the contents of the \code{field} to \code{NA_character_} in the constructor for \code{x}.
#' \item Define the \dQuote{current valid} choices in \code{\link{.refineParameters}} for \code{x}.
#' \item Replace any \code{NA_character_} value with the current valid choices in \code{\link{.refineParameters}}.
#' Non-\code{NA} values are \emph{NOT} replaced. 
#' }
#' 
#' In standard use, the \code{field} slot will ultimately be set to the current valid choices.
#' This is achieved for all instances of a class and will only change at \code{\link{iSEE}} runtime by modifying the global parameters.
#' However, should we serialize the memory and reload it in a new R session, the same application state can be restored.
#' Here, the effect of the original globals is captured in the non-\code{NA} \code{field} slot, even if the globals of the new session are different.
#'
#' This requires some discipline to not use the cached information about the global state in any other methods.
#' It also doesn't protect against people modifying \code{x} via \code{[[<-} after construction.
#' Such people are, in general, assumed to know what they are doing.
#'
#' @author Aaron Lun
#'
#' @return \code{x}, possibly after setting the \code{field} slot to \code{choices}.
#'
#' @rdname INTERNAL_update_global_field_choices
.update_global_field_choices <- function(x, field, choices) {
    if (identical(NA_character_, x[[field]])) {
        x[[field]] <- choices
    }
    x
}

.update_chosen_de_field <- function(x, field, choices) {
    if (!x[[field]] %in% x[[choices]]) {
        x[[field]] <- x[[choices]][1]
    }
    x
}
