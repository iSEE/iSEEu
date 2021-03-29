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
    .Deprecated(new="getPValuePattern")
    getPValuePattern()
}

#' @export
#' @rdname utils-de
.getAcceptableLogFCFields <- function() {
    .Deprecated(new="getLogFCPattern")
    getLogFCPattern()
}

#' @export
#' @rdname utils-de
.getAcceptableAveAbFields <- function() {
    .Deprecated(new="getAveAbPattern")
    getAveAbPattern()
}

#' @export
#' @rdname utils-de
.setAcceptablePValueFields <- function(value) {
    .Deprecated(new="setPValuePattern")
    setPValuePattern(value)
    invisible(NULL)
}

#' @export
#' @rdname utils-de
.setAcceptableLogFCFields <- function(value) {
    .Deprecated(new="setLogFCPattern")
    setLogFCPattern(value)
    invisible(NULL)
}

#' @export
#' @rdname utils-de
.setAcceptableAveAbFields <- function(value) {
    .Deprecated(new="setAveAbPattern")
    setAveAbPattern(value)
    invisible(NULL)
}

#' @importFrom ggplot2 scale_color_manual
.de_color_scale <- "local({
    .de_colors <- c(down='dodgerblue', none='grey', up='salmon')
    .de_tab <- table(factor(plot.data$IsSig, names(.de_colors)))
    .labels <- sprintf('%s (%s)', names(.de_tab), .de_tab)
    names(.labels) <- names(.de_tab)
    scale_color_manual(values=.de_colors, name='Outcome', labels=.labels)
}) +"

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

.define_de_validity <- function(object, patterns) {
    msg <- character(0)

    p <- object[["PValueThreshold"]]
    msg <- .validNumberError(msg, object, "PValueThreshold", lower=0, upper=1)
    msg <- .validNumberError(msg, object, "LogFCThreshold", lower=0, upper=Inf)
    msg <- .allowableChoiceError(msg, object, "PValueCorrection", p.adjust.methods)

    msg
}

.needs_filling <- function(value) identical(value, NA_character_)

.match_acceptable_fields <- function(globals, available) {
    provided <- globals
    okay <- logical(length(available))
    for (x in provided) {
        okay <- okay | grepl(x, available, fixed=TRUE)
    }
    unique(available[okay])
}

.define_gene_sig_ui <- function(x) {
    list(
        .numericInput.iSEE(x, "PValueThreshold", 
            label="P-value threshold:",
            value=x[["PValueThreshold"]], min=0, max=1, step=0.005),
        .numericInput.iSEE(x, "LogFCThreshold", 
            label="Log-FC threshold:",
            value=x[["LogFCThreshold"]], min=0, max=NA, step=0.5),
        .selectInput.iSEE(x, "PValueCorrection", 
            label="Correction method:",
            selected=x[["PValueCorrection"]], 
            choices=p.adjust.methods)
    )
}

.define_gene_sig_tours <- function(x) {
    .addSpecificTour(class(x), "PValueThreshold", function(plot_name) {
        data.frame(
            element=paste0("#", plot_name, "_", "PValueThreshold"),
            intro="Features with <em>adjusted</em> p-values lower than the specified threshold are considered significant, and are colored accordingly on the plot.
Note that this is combined with the log-fold change threshold if the latter is non-zero."
        )
    })

    .addSpecificTour(class(x), "LogFCThreshold", function(plot_name) {
        data.frame(
            element=paste0("#", plot_name, "_", "LogFCThreshold"),
            intro="Features are only considered significant if they have low adjusted p-values <em>and</em> absolute log-fold changes greater than the threshold specified here.
Note that this is not a particularly formal filter and may not control the relevant error rate."
        )
    })

    .addSpecificTour(class(x), "PValueCorrection", function(plot_name) {
        data.frame(
            element=paste0("#", plot_name, "_", "PValueCorrection + .selectize-control"),
            intro="Here we can choose the multiple testing correction method to use on the p-values.
By and large, the Benjamini-Hochberg method is the best choice for genome-scale results."            
        )
    })
}
