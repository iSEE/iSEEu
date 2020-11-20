#' The MAPlot class
#'
#' The MAPlot is a \linkS4class{RowDataPlot} subclass that is dedicated to creating a MA plot.
#' It retrieves the log-fold change and average abundance and creates a row-based plot where each point represents a feature.
#' Users are expected to load relevant statistics into the \code{\link{rowData}} of a \linkS4class{SummarizedExperiment}.
#'
#' @section Slot overview:
#' The following slots control the thresholds used in the visualization:
#' \itemize{
#' \item \code{PValueField}, a string specifying the field of \code{\link{rowData}} containing the p-values.
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
#' \item \code{PValuePattern}, a character vector specifying the patterns of all potential columns containing p-values, see \code{\link{getPValuePattern}}.
#' \item \code{LogFCPattern}, a character vector specifying the patterns of all potential columns containing log-fold changes, see \code{\link{getLogFCPattern}}.
#' \item \code{AveAbPattern}, a character vector specifying the patterns of all potential columns containing average abundances, see \code{\link{getAveAbPattern}}.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{RowDataPlot},
#' \linkS4class{RowDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{MAPlot(...)} creates an instance of a MAPlot class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' Initial values for \code{PValuePattern}, \code{AveAbPattern} and \code{LogFCPattern} are set to the outputs of \code{\link{getPValuePattern}}, \code{\link{getAveAbPattern}} and \code{\link{getLogFCPattern}}, respectively.
#' These parameters are considered to be global constants and cannot be changed inside the running \code{iSEE} application.
#' Similarly, it is not possible for multiple MAPlots in the same application to have different values for these slots;
#' within the app, all values are set to those of the first encountered MAPlot to ensure consistency.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{RowDataPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x, se)} returns \code{se} after being loaded with class-specific constants.
#' This includes \code{"valid.p.fields"}, \code{"valid.ab.fields"} and \code{"valid.lfc.fields"}, which are character vectors containing the names of valid \code{\link{rowData}} columns for the p-values, average abundances and log-fold changes, respectively.
#' \item \code{\link{.refineParameters}(x, se)} returns \code{x} after setting \code{XAxis="Row data"} and the various \code{*Pattern} fields to their cached values.
#' This will also call the equivalent \linkS4class{RowDataPlot} method for further refinements to \code{x}.
#' If valid p-value, abundance and log-fold change fields are not available, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.allowableXAxisChoices}(x, se)} returns a character vector specifying the acceptable average abundance-related variables in \code{\link{rowData}(se)} that can be used as choices for the x-axis.
#' \item \code{\link{.allowableYAxisChoices}(x, se)} returns a character vector specifying the acceptable log-fold change-related variables in \code{\link{rowData}(se)} that can be used as choices for the y-axis.
#' \item \code{\link{.hideInterface}(x, field)} will return \code{TRUE} for \code{field="XAxis"},
#' otherwise it will call the \linkS4class{RowDataPlot} method.
#' \item \code{\link{.fullName}(x)} will return \code{"MA plot"}.
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
#' This should contain average abundances on the x-axis and log-fold changes on the y-axis,
#' in addition to an extra field specifying whether or not the feature was considered to be significantly up or down.
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
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing the steps of a panel-specific tour.
#' }
#'
#' @docType methods
#' @aliases MAPlot MAPlot-class
#' initialize,MAPlot-method
#' .cacheCommonInfo,MAPlot-method
#' .refineParameters,MAPlot-method
#' .defineDataInterface,MAPlot-method
#' .createObservers,MAPlot-method
#' .hideInterface,MAPlot-method
#' .fullName,MAPlot-method
#' .panelColor,MAPlot-method
#' .generateDotPlotData,MAPlot-method
#' .allowableXAxisChoices,MAPlot-method
#' .allowableYAxisChoices,MAPlot-method
#' .prioritizeDotPlotData,MAPlot-method
#' .colorByNoneDotPlotField,MAPlot-method
#' .colorByNoneDotPlotScale,MAPlot-method
#' .generateDotPlot,MAPlot-method
#' .definePanelTour,MAPlot-method
#'
#' @examples
#' # Making up some results:
#' se <- SummarizedExperiment(matrix(rnorm(10000), 1000, 10))
#' rownames(se) <- paste0("GENE_", seq_len(nrow(se)))
#' rowData(se)$PValue <- runif(nrow(se))
#' rowData(se)$LogFC <- rnorm(nrow(se))
#' rowData(se)$AveExpr <- rnorm(nrow(se))
#'
#' if (interactive()) {
#'     iSEE(se, initial=list(MAPlot()))
#' }
#'
#' @author Aaron Lun
#'
#' @seealso
#' \link{RowDataPlot}, for the base class.
#'
#' @name MAPlot-class
NULL

#' @export
setClass("MAPlot", contains="RowDataPlot",
    slots=c(PValueField="character", PValueThreshold="numeric", LogFCThreshold="numeric", PValueCorrection="character",
        PValuePattern="character", LogFCPattern="character", AveAbPattern="character"))

#' @export
setMethod(".fullName", "MAPlot", function(x) "MA plot")

#' @export
setMethod(".panelColor", "MAPlot", function(x) "#666600")

#' @export
setMethod("initialize", "MAPlot", function(.Object, PValueField=NA_character_,
    PValueThreshold=0.05, LogFCThreshold=0, PValueCorrection="BH", ...)
{
    args <- list(PValueField=PValueField, PValueThreshold=PValueThreshold,
        LogFCThreshold=LogFCThreshold, PValueCorrection=PValueCorrection, ...)

    args$PValuePattern <- getPValuePattern()
    args$AveAbPattern <- getAveAbPattern()
    args$LogFCPattern <- getLogFCPattern()

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods new
MAPlot <- function(...) {
    new("MAPlot", ...)
}

#' @importFrom stats p.adjust.methods
setValidity2("MAPlot", function(object) {
    msg <- character(0)

    field <- object[["PValueField"]]
    if (length(field)!=1) {
        msg <- c(msg, "'PValueField' must be a single string")
    }

    msg <- c(msg, .define_de_validity(object, patterns=c("PValuePattern", "LogFCPattern", "AveAbPattern")))

    if (length(msg)) msg else TRUE
})

#' @export
setMethod(".cacheCommonInfo", "MAPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "MAPlot"))) {
        return(se)
    }

    se <- callNextMethod()
    all.cont <- .getCachedCommonInfo(se, "RowDotPlot")$continuous.rowData.names

    # NOTE: these fields are assumed to be globals, so it's okay to use their
    # values when caching the common values.  The plan is to use
    # .refineParameters to force all MAPlots to use the fields defined by
    # the patterns of the first encountered MAPlot.
    p.okay <- .match_acceptable_fields(x[["PValuePattern"]], all.cont)
    lfc.okay <- .match_acceptable_fields(x[["LogFCPattern"]], all.cont)
    ab.okay <- .match_acceptable_fields(x[["AveAbPattern"]], all.cont)

    .setCachedCommonInfo(se, "MAPlot",
        valid.lfc.fields=unique(lfc.okay),
        valid.p.fields=unique(p.okay),
        valid.ab.fields=unique(ab.okay))
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "MAPlot", function(x, se) {
    x <- callNextMethod() # Do this first to trigger warnings from base classes.
    if (is.null(x)) {
        return(NULL)
    }

    p.fields <- .getCachedCommonInfo(se, "MAPlot")$valid.p.fields
    if (length(p.fields)==0L) {
        warning("no valid p-value fields for '", class(x)[1], "'")
        return(NULL)
    }

    x <- .replaceMissingWithFirst(x, "PValueField", p.fields)

    x[["XAxis"]] <- "Row data"
    x
})

#' @export
setMethod(".allowableXAxisChoices", "MAPlot", function(x, se) .getCachedCommonInfo(se, "MAPlot")$valid.ab.fields)

#' @export
setMethod(".allowableYAxisChoices", "MAPlot", function(x, se) .getCachedCommonInfo(se, "MAPlot")$valid.lfc.fields)

#' @export
#' @importFrom shiny numericInput selectInput hr
#' @importFrom stats p.adjust.methods
setMethod(".defineDataInterface", "MAPlot", function(x, se, select_info) {
    plot_name <- .getEncodedName(x)
    input_FUN <- function(field) paste0(plot_name, "_", field)

    c(callNextMethod(),
        list(
            selectInput(input_FUN("PValueField"),
                label="P-value field:",
                selected=x[["PValueField"]],
                choices=.getCachedCommonInfo(se, "MAPlot")$valid.p.fields),
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
setMethod(".hideInterface", "MAPlot", function(x, field) {
    if (field == "XAxis") TRUE else callNextMethod()
})

#' @export
setMethod(".createObservers", "MAPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createUnprotectedParameterObservers(plot_name,
        fields=c("PValueField", "PValueThreshold", "LogFCThreshold", "PValueCorrection"),
        input=input, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".generateDotPlotData", "MAPlot", function(x, envir) {
    output <- callNextMethod()

    pval.field <- sprintf("rowData(se)[[%s]]", deparse(x[["PValueField"]]))
    extra_cmds <- .define_de_status(x, lfc="plot.data$Y", pval=pval.field)
    extra_cmds <- c(extra_cmds, "plot.data$IsSig <- c('down', 'none', 'up')[.de_status];")

    eval(parse(text=extra_cmds), envir)
    output$commands <- c(output$commands, extra_cmds)

    output
})

#' @export
setMethod(".prioritizeDotPlotData", "MAPlot", function(x, envir) .define_de_priority(envir))

#' @export
setMethod(".colorByNoneDotPlotField", "MAPlot", function(x) "IsSig")

#' @export
setMethod(".colorByNoneDotPlotScale", "MAPlot", function(x) .de_color_scale)

#' @export
#' @importFrom ggplot2 geom_hline
setMethod(".generateDotPlot", "MAPlot", function(x, labels, envir) {
    output <- callNextMethod()

    # Adding the horizontal lines.
    extras <- "dot.plot <- dot.plot +"
    lfc <- x[["LogFCThreshold"]]
    if (lfc > 0) {
        # No idea why I need ggplot2:: here, but it just can't find it otherwise.
        extras <- c(extras, sprintf("ggplot2::geom_hline(yintercept=c(-1, 1)*%s, color=\"darkgreen\", linetype=\"dashed\")", lfc))
    }

    if (length(extras) > 1) {
        extras <- paste(extras, collapse="\n    ")
        output$commands <- c(output$commands, list(ma=extras))
        output$plot <- eval(parse(text=extras), envir=envir)
    }

    output
})

#' @export
setMethod(".definePanelTour", "MAPlot", function(x) {
    prev <- callNextMethod()
    skip <- grep("VisualBoxOpen$", prev$element)
    prev <- prev[-seq_len(skip-1),]

    rbind(
        c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">MA plot</font> panel shows the log-fold change from a differential comparison against the average abundance. Each point here corresponds to a feature in our <code>SummarizedExperiment</code>, and the number of significantly different features in the comparisons is shown in the legend.", .getPanelColor(x))),
        c(paste0("#", .getEncodedName(x), "_DataBoxOpen"), "The <i>Data parameters</i> box shows the available parameters that can be tweaked in this plot.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        c(paste0("#", .getEncodedName(x), "_YAxis + .selectize-control"), "We can control the columns containing the log-fold changes, based on the available fields in the <code>rowData</code> of the <code>SummarizedExperiment</code>."),
        c(paste0("#", .getEncodedName(x), "_XAxisRowData + .selectize-control"), "Similarly, we can control the columns containing the average abundance of each feature, again based on the <code>rowData</code> fields. This is generally expected to be some sort of metric on the log-scale, e.g., an average log-CPM."),
        c(paste0("#", .getEncodedName(x), "_PValueThreshold"), "A variety of thresholds can also be tuned to define significant differences; the most relevant of these is the threshold on the false discovery rate."),
        prev
    )
})
