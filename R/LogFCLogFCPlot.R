#' The LogFCLogFCPlot class
#'
#' The LogFCLogFCPlot is a \linkS4class{RowDataPlot} subclass that is dedicated to creating a scatter plot of two log-fold changes.
#' Each axis contains the log-fold change for a differential expression analysis and each point represents a feature.
#' Users are expected to load relevant statistics into the \code{\link{rowData}} of a \linkS4class{SummarizedExperiment}.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualization:
#' \itemize{
#' \item \code{XPValueField}, a string specifying the field of \code{\link{rowData}} containing the p-values for the x-axis comparison.
#' \item \code{YPValueField}, a string specifying the field of \code{\link{rowData}} containing the p-values for the y-axis comparison.
#' \item \code{PValueThreshold}, a numeric scalar in (0, 1] specifying the threshold to use on the (adjusted) p-value.
#' Defaults to 0.05.
#' \item \code{LogFCThreshold}, a non-negative numeric scalar specifying the threshold to use on the log-fold change.
#' Defaults to 0.
#' \item \code{PValueCorrection}, a string specifying the multiple testing correction to apply.
#' Defaults to \code{"BH"}, but can take any value from \code{\link{p.adjust.methods}}.
#' }
#'
#' The following slots control the choice of columns in the user interface:
#' \itemize{
#' \item \code{PValueFields}, a character vector specifying the names of all columns containing p-values.
#' Set to all columns with names starting with any of the strings in \code{\link{getPValueFields}}.
#' This cannot be changed after the application has started and will be constant for all LogFCLogFCPlot instances.
#' \item \code{LogFCFields}, a character vector specifying the names of all columns containing log-fold changes.
#' Set to all columns with names starting with any of the strings in \code{\link{getLogFCFields}}.
#' This cannot be changed after the application has started and will be constant for all LogFCLogFCPlot instances.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{RowDataPlot},
#' \linkS4class{RowDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{LogFCLogFCPlot(...)} creates an instance of a LogFCLogFCPlot class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{RowDataPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x, se)} returns \code{se} after being loaded with class-specific constants.
#' This includes \code{"valid.p.fields"} and \code{"valid.lfc.fields"}, character vectors containing the names of valid \code{\link{rowData}} columns for the p-values and log-fold changes, respectively.
#' \item \code{\link{.refineParameters}(x, se)} returns \code{x} after setting \code{XAxis="Row data"}
#' as well as \code{"PValueFields"} and \code{"LogFCFields"} to their corresponding cached vectors.
#' This will also call the equivalent \linkS4class{RowDataPlot} method for further refinements to \code{x}.
#' If valid p-value and log-fold change fields are not available, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.allowableXAxisChoices}(x, se)} returns a character vector specifying the acceptable log-fold change-related variables in \code{\link{rowData}(se)} that can be used as choices for the x-axis.
#' \item \code{\link{.allowableYAxisChoices}(x, se)} returns a character vector specifying the acceptable log-fold change-related variables in \code{\link{rowData}(se)} that can be used as choices for the y-axis.
#' \item \code{\link{.hideInterface}(x, field)} will return \code{TRUE} for \code{field="XAxis"},
#' otherwise it will call the \linkS4class{RowDataPlot} method.
#' \item \code{\link{.fullName}(x)} will return \code{"LogFC-logFC plot"}.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all new slots described above, as well as in the parent classes via the \linkS4class{RowDataPlot} method.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.generateDotPlotData}(x, envir)} will create a data.frame of row metadata variables in \code{envir}.
#' This contains the two sets of log-fold changes on both axes,
#' plus an extra field specifying whether or not the feature was considered to be significantly up or down.
#' The method will return the commands required to do so as well as a list of labels.
#' \item \code{\link{.prioritizeDotPlotData}(x, envir)} will create variables in \code{envir} marking the priority of points.
#' Significant features receive higher priority (i.e., are plotted over their non-significant counterparts) and are less aggressively downsampled when \code{Downsample=TRUE}.
#' The method will return the commands required to do this as well as a logical scalar indicating that rescaling of downsampling resolution is performed.
#' \item \code{\link{.colorByNoneDotPlotField}(x)} will return a string specifying the field of the data.frame (generated by \code{\link{.generateDotPlotData}}) containing the significance information.
#' This is to be used for coloring when \code{ColorBy="None"}.
#' \item \code{\link{.colorByNoneDotPlotScale}(x)} will return a string containing a \pkg{ggplot2} command to add a default color scale when \code{ColorBy="None"}.
#' \item \code{\link{.generateDotPlot}(x, labels, envir)} returns a list containing \code{plot} and \code{commands}, using the inital \linkS4class{ColumnDataPlot} \link{ggplot} and adding horizontal lines demarcating the log-fold change threshold.
#' }
#'
#' @docType methods
#' @aliases LogFCLogFCPlot LogFCLogFCPlot-class
#' initialize,LogFCLogFCPlot-method
#' .cacheCommonInfo,LogFCLogFCPlot-method
#' .refineParameters,LogFCLogFCPlot-method
#' .defineDataInterface,LogFCLogFCPlot-method
#' .createObservers,LogFCLogFCPlot-method
#' .hideInterface,LogFCLogFCPlot-method
#' .fullName,LogFCLogFCPlot-method
#' .panelColor,LogFCLogFCPlot-method
#' .generateDotPlotData,LogFCLogFCPlot-method
#' .allowableXAxisChoices,LogFCLogFCPlot-method
#' .allowableYAxisChoices,LogFCLogFCPlot-method
#' .prioritizeDotPlotData,LogFCLogFCPlot-method
#' .colorByNoneDotPlotField,LogFCLogFCPlot-method
#' .colorByNoneDotPlotScale,LogFCLogFCPlot-method
#' .generateDotPlot,LogFCLogFCPlot-method
#'
#' @examples
#' # Making up some results:
#' se <- SummarizedExperiment(matrix(rnorm(10000), 1000, 10))
#' rownames(se) <- paste0("GENE_", seq_len(nrow(se)))
#' rowData(se)$PValue1 <- runif(nrow(se))
#' rowData(se)$LogFC1 <- rnorm(nrow(se))
#' rowData(se)$PValue2 <- runif(nrow(se))
#' rowData(se)$LogFC2 <- rnorm(nrow(se))
#' 
#' if (interactive()) {
#'     iSEE(se, initial=list(LogFCLogFCPlot(XAxisRowData="LogFC1", YAxis="LogFC2",
#'         XPValueField="PValue1", YPValueField="PValue2")))
#' }
#'
#' @author Aaron Lun
#'
#' @seealso
#' \link{RowDataPlot}, for the base class.
#'
#' @name LogFCLogFCPlot-class
NULL

#' @export
setClass("LogFCLogFCPlot", contains="RowDataPlot",
    slots=c(YPValueField="character", XPValueField="character",
        PValueThreshold="numeric", LogFCThreshold="numeric", PValueCorrection="character",
        PValueFields="character", LogFCFields="character"))

#' @export
setMethod(".fullName", "LogFCLogFCPlot", function(x) "LogFC-logFC plot")

#' @export
setMethod(".panelColor", "LogFCLogFCPlot", function(x) "#770055")

#' @export
setMethod("initialize", "LogFCLogFCPlot", function(.Object, 
    YPValueField=NA_character_, XPValueField=NA_character_,
    PValueThreshold=0.05, LogFCThreshold=0, PValueCorrection="BH", ...)
{
    args <- list(YPValueField=YPValueField, XPValueField=XPValueField,
        PValueThreshold=PValueThreshold, LogFCThreshold=LogFCThreshold, 
        PValueCorrection=PValueCorrection, ...)

    args$PValueFields <- NA_character_
    args$LogFCFields <- NA_character_

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
LogFCLogFCPlot <- function(...) {
    new("LogFCLogFCPlot", ...)
}

#' @importFrom stats p.adjust.methods
setValidity2("LogFCLogFCPlot", function(object) {
    msg <- character(0)

    field <- object[["YPValueField"]]
    if (length(field)!=1) {
        msg <- c(msg, "'YPValueField' must be a single string")
    }

    field <- object[["XPValueField"]]
    if (length(field)!=1) {
        msg <- c(msg, "'XPValueField' must be a single string")
    }

    msg <- c(msg, .define_de_validity(object, allow.na.fields=TRUE))

    if (length(msg)) msg else TRUE
})

#' @export
setMethod(".cacheCommonInfo", "LogFCLogFCPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "LogFCLogFCPlot"))) {
        return(se)
    }

    se <- callNextMethod()
    all.cont <- .getCachedCommonInfo(se, "RowDotPlot")$continuous.rowData.names

    # We determine the valid fields from the first encountered instance of the
    # class, which assumes that 'PValueFields' and 'LogFCFields' are class-wide
    # constants. (We actually ensure that this is the case by forcibly setting
    # them in .refineParameters later.)
    acceptable.p <- x[["PValueFields"]]
    if (.needs_filling(acceptable.p)) {
        acceptable.p <- getPValueFields()
    }
    p.okay <- lapply(acceptable.p, grepl, x=all.cont)
    p.okay <- Reduce(`|`, p.okay)

    acceptable.lfc <- x[["LogFCFields"]]
    if (.needs_filling(acceptable.lfc)) {
        acceptable.lfc <- getLogFCFields()
    }
    lfc.okay <- lapply(acceptable.lfc, grepl, x=all.cont)
    lfc.okay <- Reduce(`|`, lfc.okay)

    .setCachedCommonInfo(se, "LogFCLogFCPlot",
        valid.p.fields=all.cont[p.okay],
        valid.lfc.fields=all.cont[lfc.okay])
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "LogFCLogFCPlot", function(x, se) {
    x[["PValueFields"]] <- .getCachedCommonInfo(se, "LogFCLogFCPlot")$valid.p.fields
    x[["LogFCFields"]] <- .getCachedCommonInfo(se, "LogFCLogFCPlot")$valid.lfc.fields

    x <- callNextMethod() # Trigger warnings from base classes.
    if (is.null(x)) {
        return(NULL)
    }

    if (length(x[["PValueFields"]])==0L) {
        warning("no valid p-value fields for '", class(x)[1], "'")
        return(NULL)
    }

    x <- .update_chosen_de_field(x, "XPValueField", "PValueFields")
    x <- .update_chosen_de_field(x, "YPValueField", "PValueFields")

    x[["XAxis"]] <- "Row data"
    x
})

#' @export
setMethod(".allowableXAxisChoices", "LogFCLogFCPlot", function(x, se) x[["LogFCFields"]])

#' @export
setMethod(".allowableYAxisChoices", "LogFCLogFCPlot", function(x, se) x[["LogFCFields"]])

#' @export
#' @importFrom shiny numericInput selectInput hr
#' @importFrom stats p.adjust.methods
setMethod(".defineDataInterface", "LogFCLogFCPlot", function(x, se, select_info) {
    plot_name <- .getEncodedName(x)
    input_FUN <- function(field) paste0(plot_name, "_", field)

    c(callNextMethod(),
        list(
            hr(),
            selectInput(input_FUN("YPValueField"),
                label="P-value field (Y-axis):",
                selected=x[["YPValueField"]],
                choices=x[["PValueFields"]]),
            selectInput(input_FUN("XPValueField"),
                label="P-value field (X-axis):",
                selected=x[["XPValueField"]],
                choices=x[["PValueFields"]]),
            hr(),
            numericInput(input_FUN("PValueThreshold"), label="P-value threshold:",
                value=x[["PValueThreshold"]], min=0, max=1, step=0.005),
            numericInput(input_FUN("LogFCThreshold"), label="Log-FC threshold:",
                value=x[["LogFCThreshold"]], min=0, max=NA, step=0.5),
            selectInput(input_FUN("PValueCorrection"), label="Correction method:",
                selected=x[["PValueCorrection"]], choices=p.adjust.methods)
        )
    )
})

#' @export
setMethod(".hideInterface", "LogFCLogFCPlot", function(x, field) {
    if (field == "XAxis") TRUE else callNextMethod()
})

#' @export
setMethod(".createObservers", "LogFCLogFCPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createUnprotectedParameterObservers(plot_name,
        fields=c("XPValueField", "YPValueField", 
             "PValueThreshold", "LogFCThreshold", "PValueCorrection"),
        input=input, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".generateDotPlotData", "LogFCLogFCPlot", function(x, envir) {
    output <- callNextMethod()

    x.pvals <- sprintf("rowData(se)[[%s]]", deparse(x[["XPValueField"]]))
    y.pvals <- sprintf("rowData(se)[[%s]]", deparse(x[["YPValueField"]]))
    extra_cmds <- c(
        .define_de_status(x, "plot.data$X", x.pvals, varname=".de_status_x"),
        .define_de_status(x, "plot.data$Y", y.pvals, varname=".de_status_y"),
        "plot.data$IsSig <- c('none', 'x-only', 'y-only', 'both')[1 + (.de_status_x!=2) + 2 * (.de_status_y!=2)];",
        ".freq_status <- tabulate(1 + (.de_status_x - 1) + 3 * (.de_status_y - 1), nbins=9);"
    )

    eval(parse(text=extra_cmds), envir)
    output$commands <- c(output$commands, extra_cmds)

    output
})

#' @export
setMethod(".prioritizeDotPlotData", "LogFCLogFCPlot", function(x, envir) { 
    cmds <- c(
        ".rescaled <- c(none=1, `x-only`=2, `y-only`=2, both=3);",
        ".priority <- factor(plot.data$IsSig, names(.rescaled), ordered=TRUE);"
    )
    eval(parse(text=cmds), envir)
    list(commands=cmds, rescaled=TRUE)
})

#' @export
setMethod(".colorByNoneDotPlotField", "LogFCLogFCPlot", function(x) "IsSig")

#' @export
setMethod(".colorByNoneDotPlotScale", "LogFCLogFCPlot", function(x) 
    "scale_color_manual(values=c(none='grey', `x-only`='#f65058', `y-only`='#28334a', both='#fbcd22'), name='Outcome',
    labels=setNames(
        c(
            sprintf('None (%s)', .freq_status[5]),
            paste(sprintf('X-axis %s (%s)', c('down', 'up'), .freq_status[c(4,6)]), collapse='\n'),
            paste(sprintf('Y-axis %s (%s)', c('down', 'up'), .freq_status[c(2,8)]), collapse='\n'),
            paste(sprintf('X-axis %s, Y-axis %s (%s)', rep(c('down', 'up'), each=2), rep(c('down', 'up'), 2), .freq_status[c(1,7,3,9)]), collapse='\n')
        ), c('none', 'x-only', 'y-only', 'both'))) +")

#' @export
#' @importFrom ggplot2 geom_hline
setMethod(".generateDotPlot", "LogFCLogFCPlot", function(x, labels, envir) {
    output <- callNextMethod()

    # Adding the lines.
    extras <- "dot.plot <- dot.plot +"
    lfc <- x[["LogFCThreshold"]]
    if (lfc > 0) {
        # No idea why I need ggplot2:: here, but it just can't find it otherwise.
        extras <- c(extras, 
              sprintf("ggplot2::geom_hline(yintercept=c(-1, 1)*%s, color=\"darkgreen\", linetype=\"dashed\") +", lfc),
              sprintf("ggplot2::geom_vline(xintercept=c(-1, 1)*%s, color=\"darkgreen\", linetype=\"dashed\")", lfc))
    }

    if (length(extras) > 1) {
        extras <- paste(extras, collapse="\n    ")
        output$commands <- c(output$commands, list(ma=extras))
        output$plot <- eval(parse(text=extras), envir=envir)
    }

    output
})
