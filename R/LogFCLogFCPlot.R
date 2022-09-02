#' The LogFCLogFCPlot class
#'
#' The LogFCLogFCPlot is a \linkS4class{RowDataPlot} subclass that is dedicated to creating a scatter plot of two log-fold changes.
#' Each axis contains the log-fold change for a differential expression analysis and each point represents a feature.
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
#' In addition, this class inherits all slots from its parent \linkS4class{RowDataPlot},
#' \linkS4class{RowDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{LogFCLogFCPlot(...)} creates an instance of a LogFCLogFCPlot class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' Users are expected to load relevant statistics into the \code{\link{rowData}} of a \linkS4class{SummarizedExperiment}.
#' There should be two columns for the p-values from each comparison - and another two for the corresponding log-fold changes - for each gene/row, see Examples.
#' The expected column names (and how to tune them) are listed at \code{?"\link{registerPValueFields}"}.
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
#' as well as \code{"PValuePattern"} and \code{"LogFCPattern"} to their corresponding cached values.
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
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing the steps of a panel-specific tour.
#' \item \code{\link{.getDotPlotColorHelp}(x, color_choices)} returns a function that generates an \pkg{rintrojs} tour for the color choice UI.
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
#' .definePanelTour,LogFCLogFCPlot-method
#' .getDotPlotColorHelp,LogFCLogFCPlot-method
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
        PValueThreshold="numeric", LogFCThreshold="numeric", PValueCorrection="character"))

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

    msg <- c(msg, .define_de_validity(object, patterns=c("PValuePattern", "LogFCPattern")))

    if (length(msg)) msg else TRUE
})

#' @export
setMethod(".cacheCommonInfo", "LogFCLogFCPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "LogFCLogFCPlot"))) {
        return(se)
    }

    se <- callNextMethod()
    all.cont <- .getCachedCommonInfo(se, "RowDotPlot")$continuous.rowData.names

    p.okay <- .matchPValueFields(se, all.cont)
    lfc.okay <- .matchLogFCFields(se, all.cont)

    .setCachedCommonInfo(se, "LogFCLogFCPlot", valid.p.fields=p.okay, valid.lfc.fields=lfc.okay)
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "LogFCLogFCPlot", function(x, se) {
    x <- callNextMethod() # Trigger warnings from base classes.
    if (is.null(x)) {
        return(NULL)
    }

    p.fields <- .getCachedCommonInfo(se, "LogFCLogFCPlot")$valid.p.fields
    if (length(p.fields)==0L) {
        warning("no valid p-value fields for '", class(x)[1], "'")
        return(NULL)
    }

    x <- .replaceMissingWithFirst(x, "XPValueField", p.fields)
    x <- .replaceMissingWithFirst(x, "YPValueField", p.fields)

    x[["XAxis"]] <- "Row data"
    x
})

#' @export
setMethod(".allowableXAxisChoices", "LogFCLogFCPlot", function(x, se) .getCachedCommonInfo(se, "LogFCLogFCPlot")$valid.lfc.fields)

#' @export
setMethod(".allowableYAxisChoices", "LogFCLogFCPlot", function(x, se) .getCachedCommonInfo(se, "LogFCLogFCPlot")$valid.lfc.fields)

#' @export
#' @importFrom shiny numericInput selectInput hr
#' @importFrom stats p.adjust.methods
setMethod(".defineDataInterface", "LogFCLogFCPlot", function(x, se, select_info) {
    plot_name <- .getEncodedName(x)
    input_FUN <- function(field) paste0(plot_name, "_", field)
    p.fields <- .getCachedCommonInfo(se, "LogFCLogFCPlot")$valid.p.fields

    .addSpecificTour(class(x), "YAxis", function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", "YAxis + .selectize-control"),
                    intro="Here, we select the <code>rowData</code> field containing the log-fold changes to show on the y-axis.
This is presumably generated from some comparison between conditions, e.g., for differential gene expression."
                ),
                c(
                    element=paste0("#", plot_name, "_", "XAxisRowData + .selectize-control"),
                    intro="Similarly, we can select the <code>rowData</code> field containing the log-fold changes to show on the x-axis.
This should have been generated from another comparison between conditions, typically different to that used for the y-axis.
If the comparisons are not completely independent, one should be careful when interpreting technical correlations between the log-fold changes."
                )
            )
        )
    })

    .addSpecificTour(class(x), "YPValueField", function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", "YPValueField + .selectize-control"),
                    intro="Here, we select the <code>rowData</code> field containing the p-values for the y-axis comparison.
This is used to identify significant features for this comparison, after adjusting for multiple testing and applying log-fold change thresholds.
Points will then be colored based on whether they are significant in this comparison (and/or in the x-axis comparison).
<br/><br/>
Note that these p-values should be on the raw scale, i.e., not log-transformed, and they should not already be corrected for multiple testing."
                ),
                c(
                    element=paste0("#", plot_name, "_", "XPValueField + .selectize-control"),
                    intro="Here, we select the <code>rowData</code> field containing the p-values for the x-axis comparison.
This is used to identify significant features for this comparison, which is combined with the corresponding results for the y-axis to determine the coloring for each point.
<br/><br/>
Again, p-values should be on the raw scale, i.e., not log-transformed, and not corrected for multiple testing."
                )
            )
        )
    })

    .define_gene_sig_tours(x)

    c(callNextMethod(),
        list(
            hr(),
            .selectInput.iSEE(x, "YPValueField",
                label="P-value field (Y-axis):",
                selected=x[["YPValueField"]],
                choices=p.fields),
            selectInput(input_FUN("XPValueField"),
                label="P-value field (X-axis):",
                selected=x[["XPValueField"]],
                choices=p.fields),
            hr()
        ),
        .define_gene_sig_ui(x)
    )
})

#' @export
#' @importFrom shiny tagList
setMethod(".getDotPlotColorHelp", "LogFCLogFCPlot", function(x, color_choices) {
    FUN <- callNextMethod()

    function(plot_name) {
        df <- FUN(plot_name)
        df[1,2] <- "Here, we choose whether to color points by per-row attributes.
When <em>None</em> is selected, the plot defaults to a constant color for all non-significant features
and a variety of other colors for each combination of significant/non-significant features in each direction across the two comparisons.
The number of features in each category is also shown in the legend.
<br/><br/>
Alternatively, try out some of the different choices here, and note how further options become available when each choice is selected."
        df
    }
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
        "",
        .define_de_status(x, "plot.data$X", x.pvals, varname=".de_status_x"),
        "plot.data$IsSigX <- .de_status_x;\n",
        .define_de_status(x, "plot.data$Y", y.pvals, varname=".de_status_y"),
        "plot.data$IsSigY <- .de_status_y;\n",
        "plot.data$IsSig <- c('none', 'x-only', 'y-only', 'both')[1 + (.de_status_x!=2) + 2 * (.de_status_y!=2)];"
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
setMethod(".colorByNoneDotPlotScale", "LogFCLogFCPlot", function(x) {
    var <- if (x[["Downsample"]]) "plot.data.pre" else "plot.data"
    paste0(
"local({
    .freq_status <- tabulate(1 + (", var, "$IsSigX - 1) + 3 * (", var, "$IsSigY - 1), nbins=9);
    .de_labels <- c(
        none=sprintf('none (%s)', .freq_status[5]),
        `x-only`=paste(sprintf('x %s 0 (%s)', c('<', '>'), .freq_status[c(4,6)]), collapse='\\n'),
        `y-only`=paste(sprintf('y %s 0 (%s)', c('<', '>'), .freq_status[c(2,8)]), collapse='\\n'),
        both=paste(sprintf('x %s 0, y %s 0 (%s)', rep(c('<', '>'), each=2), rep(c('<', '>'), 2), 
            .freq_status[c(1,7,3,9)]), collapse='\\n')
    );
    scale_color_manual(values=c(none='grey', `x-only`='#fc766a', `y-only`='#2da8d8', both='#2a2b2d'), 
        name='Outcome', labels=.de_labels)
}) +")
})

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

#' @export
setMethod(".definePanelTour", "LogFCLogFCPlot", function(x) {
    prev <- callNextMethod()
    skip <- grep("VisualBoxOpen$", prev$element)
    prev <- prev[-seq_len(skip-1),]

    rbind(
        c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">LogFC-logFC plot</font> panel shows the log-fold change from one differential comparison against the log-fold change from another differential comparison. Each point here corresponds to a feature in our <code>SummarizedExperiment</code>, and the number of significantly different features in either or both comparisons is shown in the legend.", .getPanelColor(x))),
        c(paste0("#", .getEncodedName(x), "_DataBoxOpen"), "The <i>Data parameters</i> box shows the available parameters that can be tweaked in this plot.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        c(paste0("#", .getEncodedName(x), "_YAxis + .selectize-control"), "We can control the columns containing the log-fold changes, based on the available fields in the <code>rowData</code> of the <code>SummarizedExperiment</code>."),
        c(paste0("#", .getEncodedName(x), "_YPValueField + .selectize-control"), "Similarly, we can control the columns containing the p-values corresponding to each of the log-fold changes, again based on the <code>rowData</code> fields."),
        c(paste0("#", .getEncodedName(x), "_PValueThreshold"), "A variety of thresholds can also be tuned to define significant differences; the most relevant of these is the threshold on the false discovery rate."),
        prev
    )
})
