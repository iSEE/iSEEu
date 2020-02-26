#' @export
setClass("VolcanoPlot", contains="RowDataPlot", 
    slots=c(PValueThreshold="numeric", LogFCThreshold="numeric"))

setMethod(".fullName", "VolcanoPlot", function(x) "Volcano plot")

#' @export
setMethod("initialize", "VolcanoPlot", function(.Object, ...) {
    args <- list(...)
    if (is.null(args$PValueThreshold)) args$PValueThreshold <- 0.01
    if (is.null(args$LogFCThreshold)) args$LogFCThreshold <- 0
    do.call(callNextMethod, c(list(.Object), args))
})

setValidity2("VolcanoPlot", function(object) {
    msg <- character(0)

    p <- object[["PValueThreshold"]]
    if (length(p)!=1 || p <= 0 || p > 1) {
        msg <- c(msg, "p-value threshold must be in (0, 1]")
    }

    lfc <- object[["LogFCThreshold"]]
    if (length(lfc)!=1 || lfc < 0) {
        msg <- c(msg, "log-fold change threshold must be non-negative")
    }

    if (length(msg)) msg else TRUE
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "VolcanoPlot", function(x, se) {
    x <- callNextMethod() # Do this first to trigger warnings from base classes.
    if (is.null(x)) {
        return(NULL)
    }

    x[["XAxis"]] <- "Row data"

    # TODO: probably want to expose this in a bag of 'developer utilities'.
    covariates <- iSEE:::.get_common_info(se, "RowDotPlot")$continuous.rowData.names

    p.fields <- acceptablePValueFields()
    p.fields <- intersect(p.fields, covariates)
    if (length(p.fields)==0) {
        warning(sprintf("no valid p-value fields for '%s'", class(x)[1]))
        return(NULL)
    }

    lfc.fields <- acceptableLogFCFields()
    lfc.fields <- intersect(lfc.fields, covariates)
    if (length(lfc.fields)==0L) {
        warning(sprintf("no valid log-FC fields for '%s'", class(x)[1]))
        return(NULL)
    }

    # TODO: either expose or copy this over.
    x <- iSEE:::.replace_na_with_first(x, "YAxis", p.fields)
    x <- iSEE:::.replace_na_with_first(x, "XAxisRowData", lfc.fields)

    x
})

#' @export
#' @importFrom shiny numericInput
setMethod(".defineDataInterface", "VolcanoPlot", function(x, se, select_info) {
    plot_name <- .getEncodedName(x)
    input_FUN <- function(field) paste0(plot_name, "_", field)

    c(callNextMethod(),
        list(
            numericInput(input_FUN("PValueThreshold"), label="P-value threshold:", 
                value=x[["PValueThreshold"]], min=0, max=1, step=0.005),
            numericInput(input_FUN("LogFCValueThreshold"), label="Log-FC threshold:", 
                value=x[["LogFCThreshold"]], min=0, max=NA, step=0.5)
        )
    )
})

#' @export
setMethod(".hideInterface", "VolcanoPlot", function(x, field) {
    if (field == "XAxis") TRUE
    callNextMethod()       
})

#' @export
setMethod(".generateDotPlotData", "VolcanoPlot", function(x, envir) {
    output <- callNextMethod()

    extra_cmds <- c(
        sprintf(
            ".de_status <- (rowData(se)[[%s]] <= %s) * sign(rowData(se)[[%s]]);",
            deparse(x[["YAxis"]]), deparse(x[["PValueThreshold"]]), deparse(x[["XAxisRowData"]])
        ),
        "plot.data$IsSig <- c('down', 'none', 'up')[.de_status + 2L];",
        "plot.data$Y <- -log10(plot.data$Y)"
    )

    eval(parse(text=extra_cmds), envir)
    print(head(envir$plot.data))
    output$commands <- c(output$commands, extra_cmds)
    output$labels$Y <- sprintf("-Log10[%s]", output$labels$Y)

    output
})

#' @export
setMethod(".prioritizeDotPlotData", "VolcanoPlot", function(x, envir) {
    cmds <- c(
        ".priority <- factor(plot.data$IsSig, c('none', 'down', 'sig'), ordered=TRUE);",
        ".rescaled <- c(none=1, down=2, up=2);"        
    )
    eval(parse(text=cmds), envir)
    list(commands=cmds, rescaled=TRUE)
})

#' @importFrom ggplot2 geom_point scale_color_manual aes
setMethod(".generateDotPlot", "VolcanoPlot", function(x, labels, envir) {
    output <- callNextMethod()

    # Need to add a color scale for the significant points.
    if (x[["ColorBy"]]=="None") {
        # TODO: Need to fix this, have .generateDotPlot add a 'gg' so we 
        # can just evaluate commands.
        output$plot <- output$plot + 
            geom_point(aes(x=X, y=Y, color=IsSig), data=envir$plot.data) +
            scale_color_manual(values=c(down='dodgerblue', none='grey', up='salmon'))
    }

    output
})
