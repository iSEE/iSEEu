#' The ReducedDimensionHexPlot class
#'
#' The ReducedDimensionHexPlot is a [ReducedDimensionPlot-class] subclass that is dedicated to creating a reduced dimension plot summarising data points in hexagonal bins.
#'
#' @section Slot overview:
#' The following slots control the parameters used in the visualization:
#' \itemize{
#' \item `BinResolution`, a numeric positive scalar specifying the number of hexagonal bins in both vertical and horizontal directions.
#' Defaults to 100.
#' }
#'
#' In addition, this class inherits all slots from its parent [ReducedDimensionPlot-class],
#' [ColumnDotPlot-class], [DotPlot-class] and [Panel-class] classes.
#'
#' @section Constructor:
#' `ReducedDimensionHexPlot(...)` creates an instance of a ReducedDimensionHexPlot class,
#' where any slot and its value can be passed to `...` as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, `x` is an instance of a [ReducedDimensionHexPlot-class] class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a `"ReducedDimensionHexPlot"` entry containing
#' `valid.colData.names`, a character vector of names of columns that are valid (i.e., contain atomic values);
#' `discrete.colData.names`, a character vector of names for columns with discrete atomic values;
#' and `continuous.colData.names`, a character vector of names of columns with continuous atomic values.
#' This will also call the equivalent [ColumnDotPlot-class] method.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.fullName}(x)} will return `"Hexagonal reduced dimension plot"`.
#' \item \code{\link{.hideInterface}(x, field)} will return `TRUE` for `field="Downsample"` as downsampling is not applicable to this panel that summarizes all data points in each hexagonal bin;
#' otherwise this function will call the [ReducedDimensionPlot-class] method.
#' \item \code{\link{.defineVisualShapeInterface}(x)} will return `NULL` for this panel, as the shape aesthetic is not applicable to this panel that does not display individual data points.
#' \item \code{\link{.defineVisualSizeInterface}(x)} overrides the equivalent method inherited from all parents classes and will return instead an HTML tag definition that contains a user input controlling the number of hexagonal bins in both vertical and horizontal directions.
#' \item \code{\link{.defineVisualOtherInterface}(x)} will return `NULL`, as there are no additional visual parameters for this panel.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all new slots described above, as well as in the parent classes via the [ReducedDimensionPlot-class] method.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.generateDotPlot}(x, envir)} will return a list with `plot`, a [ggplot2::ggplot()] object; and `commands`, a character vector of commands to produce that object when evaluated inside `envir`.
#' }
#'
#' @docType methods
#' @aliases ReducedDimensionHexPlot ReducedDimensionHexPlot-class
#' initialize,ReducedDimensionHexPlot-method
#' .fullName,ReducedDimensionHexPlot-method
#' .panelColor,ReducedDimensionHexPlot-method
#' .cacheCommonInfo,ReducedDimensionHexPlot-method
#' .createObservers,ReducedDimensionHexPlot-method
#' .hideInterface,ReducedDimensionHexPlot-method
#' .defineVisualShapeInterface,ReducedDimensionHexPlot-method
#' .defineVisualSizeInterface,ReducedDimensionHexPlot-method
#' .defineVisualOtherInterface,ReducedDimensionHexPlot-method
#' .generateDotPlot,ReducedDimensionHexPlot-method
#'
#' @examples
#' library(scRNAseq)
#'
#' # Example data ----
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#' class(sce)
#'
#' library(scater)
#' sce <- logNormCounts(sce, exprs_values="tophat_counts")
#'
#' sce <- runPCA(sce, ncomponents=4)
#' sce <- runTSNE(sce)
#' rowData(sce)$ave_count <- rowMeans(assay(sce, "tophat_counts"))
#' rowData(sce)$n_cells <- rowSums(assay(sce, "tophat_counts") > 0)
#'
#' # launch the app itself ----
#'
#' if (interactive()) {
#'     iSEE(sce, initial=list(
#'         ReducedDimensionHexPlot(BinResolution=50),
#'         ReducedDimensionPlot()
#'     ))
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso
#' \link{ReducedDimensionPlot}, for the base class.
#' @name ReducedDimensionHexPlot-class
NULL

# Definition ----

collated <- character(0)

.plotBinResolution <- "BinResolution"
collated[.plotBinResolution] <- "numeric"

#' @export
#' @importClassesFrom iSEE ReducedDimensionPlot
setClass("ReducedDimensionHexPlot", contains="ReducedDimensionPlot", slots=collated)

#' @export
#' @importFrom methods new
ReducedDimensionHexPlot <- function(...) {
    new("ReducedDimensionHexPlot", ...)
}

#' @export
#' @importMethodsFrom iSEE .fullName
setMethod(".fullName", "ReducedDimensionHexPlot", function(x) "Hexagonal reduced dimension plot")

#' @export
#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "ReducedDimensionHexPlot", function(x) "#991717")

# Initialization ----

#' @export
#' @importFrom methods callNextMethod
#' @importFrom iSEE .emptyDefault
setMethod("initialize", "ReducedDimensionHexPlot", function(.Object, ...) {
    args <- list(...)

    args <- .emptyDefault(args, .plotBinResolution, 100)
    args[["Downsample"]] <- FALSE

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom SummarizedExperiment colData
#' @importFrom methods callNextMethod
#' @importFrom iSEE .findAtomicFields .whichGroupable .whichNumeric
setMethod(".cacheCommonInfo", "ReducedDimensionHexPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "ReducedDimensionHexPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    df <- colData(se)
    displayable <- .findAtomicFields(df)

    subdf <- df[,displayable,drop=FALSE]
    discrete <- .whichGroupable(subdf)
    continuous <- .whichNumeric(subdf)

    .setCachedCommonInfo(se, "ReducedDimensionHexPlot",
        valid.colData.names=displayable,
        discrete.colData.names=displayable[discrete],
        continuous.colData.names=displayable[continuous])
})

# Interface ----

#' @export
setMethod(".hideInterface", "ReducedDimensionHexPlot", function(x, field) {
    hidden_fields <- c("Downsample")
    if (field %in% hidden_fields)
        TRUE
    else callNextMethod()
})

#' @export
setMethod(".defineVisualShapeInterface", "ReducedDimensionHexPlot", function(x) {
    NULL
})

#' @export
#' @importFrom shiny tagList
setMethod(".defineVisualSizeInterface", "ReducedDimensionHexPlot", function(x) {
    plot_name <- .getEncodedName(x)
    tagList(
        numericInput(
            paste0(plot_name, "_", .plotBinResolution), label="Bin resolution:",
            min=1, value=x[[.plotBinResolution]], step = 1)
    )
})

#' @export
setMethod(".defineVisualOtherInterface", "ReducedDimensionHexPlot", function(x) {
    NULL
})

# Observers ----

#' @export
#' @importFrom methods callNextMethod
setMethod(".createObservers", "ReducedDimensionHexPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
        fields=c(.plotBinResolution),
        input=input, pObjects=pObjects, rObjects=rObjects)

    invisible(NULL)
})

# Plotting ----

#' @export
#' @importMethodsFrom iSEE .generateDotPlot
#' @importFrom iSEE .addFacets .buildAes .buildLabs
#' @importFrom ggplot2 geom_hex
setMethod(".generateDotPlot", "ReducedDimensionHexPlot", function(x, labels, envir) {
    plot_data <- envir$plot.data
    is_subsetted <- exists("plot.data.all", envir=envir, inherits=FALSE)
    is_downsampled <- exists("plot.data.pre", envir=envir, inherits=FALSE)
    plot_type <- envir$plot.type

    args <- list(plot_data,
        param_choices=x,
        x_lab=labels$X,
        y_lab=labels$Y,
        color_lab=labels$ColorBy,
        shape_lab=labels$ShapeBy,
        size_lab=labels$SizeBy,
        title=labels$title,
        is_subsetted=is_subsetted,
        is_downsampled=is_downsampled)

    plot_cmds <- do.call(.reduced_dimension_hex_plot, args)

    # Adding a faceting command, if applicable.
    facet_cmd <- .addFacets(x)
    if (length(facet_cmd)) {
        N <- length(plot_cmds)
        plot_cmds[[N]] <- paste(plot_cmds[[N]], "+")
        plot_cmds <- c(plot_cmds, facet_cmd)
    }

    # Adding self-brushing boxes, if they exist.
    plot_cmds <- .addMultiSelectionPlotCommands(x,
        flip=(plot_type == "violin_horizontal"),
        envir=envir, commands=plot_cmds)

    list(plot=.textEval(plot_cmds, envir), commands=plot_cmds)
})

.reduced_dimension_hex_plot <- function(plot_data, param_choices,
    x_lab, y_lab, color_lab, shape_lab, size_lab, title,
    by_row=FALSE, is_subsetted=FALSE, is_downsampled=FALSE)
{
    plot_cmds <- list()
    plot_cmds[["ggplot"]] <- "ggplot() +"

    color_set <- !is.null(plot_data$ColorBy)
    shape_set <- param_choices[["ShapeBy"]] != "None"
    size_set <- param_choices[["SizeBy"]] != "None"
    color_discrete <- is.factor(plot_data$ColorBy)

    if (color_set && !color_discrete) {
        new_aes <- .buildAes(color=FALSE, shape=shape_set, size=size_set, alt=c(z="ColorBy"))
    } else if (color_set && param_choices[["ColorBy"]] == "Sample name") {
        new_aes <- .buildAes(color=FALSE, shape=shape_set, size=size_set)
    } else {
        new_aes <- .buildAes(color=color_set, shape=shape_set, size=size_set)
    }

    plot_cmds[["hex"]] <- .create_hex(param_choices,
        new_aes, color_set, size_set, color_discrete, is_subsetted)

    # Adding axes labels.
    if (is.null(color_lab) || color_discrete) {
        color_lab <- "Count"
    }
    plot_cmds[["labs"]] <- .buildLabs(x=x_lab, y=y_lab, fill=color_lab, shape=shape_lab, size=size_lab, title=title)

    # Adding further aesthetic elements.
    plot_cmds[["theme_base"]] <- "theme_bw() +"
    plot_cmds[["theme_custom"]] <- sprintf(
        "theme(legend.position='%s', legend.box='vertical', legend.text=element_text(size=%s), legend.title=element_text(size=%s),
        axis.text=element_text(size=%s), axis.title=element_text(size=%s), title=element_text(size=%s))",
        tolower(param_choices[["LegendPosition"]]),
        param_choices[["FontSize"]]*9,
        param_choices[["FontSize"]]*11,
        param_choices[["FontSize"]]*10,
        param_choices[["FontSize"]]*12,
        param_choices[["FontSize"]]*12)

    return(unlist(plot_cmds))
}

.create_hex <- function(param_choices, aes, color, size, color_discrete, is_subsetted) {
    plot_cmds <- list()

    fallback <- sprintf(
        "geom_hex(%s, bins = %i, alpha=%s, plot.data) +",
        aes,
        as.integer(param_choices[[.plotBinResolution]]),
        param_choices[["PointAlpha"]]
    )

    color_choice <- param_choices[["ColorBy"]]
    if (color_choice %in% c("Column data", "Feature name")) {
        if (color_discrete) {
            plot_cmds[["hex"]] <- fallback
        } else {
            plot_cmds[["hex"]] <- sprintf(
                sprintf('stat_summary_hex(%s, geom = "hex", bins = %i, fun=%s, alpha=%s, plot.data) +',
                    aes,
                    as.integer(param_choices[[.plotBinResolution]]),
                    deparse("mean"),
                    param_choices[["PointAlpha"]])
            )
        }
    } else if (color_choice == "Sample name") {
        plot_cmds[["hex"]] <- c(fallback, sprintf(
            "geom_point(%s, data=subset(plot.data, ColorBy == 'TRUE'), color=%s, alpha=1, size=5*%s) +",
            aes, deparse(param_choices[["ColorBySampleNameColor"]]), param_choices[["PointSize"]]))
    }
    else { # color by nothing -> count of samples per bin
        plot_cmds[["hex"]] <- fallback
    }

    # TODO: export functionality from iSEE (copy start)
    # Defining boundaries if zoomed.
    bounds <- param_choices[["ZoomData"]]
    if (length(bounds)) {
        plot_cmds[["coord"]] <- sprintf(
            "coord_cartesian(xlim=c(%s, %s), ylim=c(%s, %s), expand=FALSE) +", # FALSE, to get a literal zoom.
            deparse(bounds["xmin"]), deparse(bounds["xmax"]),
            deparse(bounds["ymin"]),  deparse(bounds["ymax"])
        )
    } else {
        full_data <- ifelse(is_subsetted, "plot.data.all", "plot.data")
        plot_cmds[["coord"]] <- sprintf("coord_cartesian(xlim=range(%s$X, na.rm=TRUE),
    ylim=range(%s$Y, na.rm=TRUE), expand=TRUE) +", full_data, full_data)
    }
    # TODO: export functionality from iSEE (copy end)

    return(unlist(plot_cmds))
}
