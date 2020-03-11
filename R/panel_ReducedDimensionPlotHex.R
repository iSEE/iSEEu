# Definition ----

collated <- character(0)

.plotBinResolution <- "BinResolution"
collated[.plotBinResolution] <- "numeric"

#' @export
#' @importClassesFrom iSEE ReducedDimensionPlot
setClass("ReducedDimensionHexPlot", contains="ReducedDimensionPlot", slots=collated)

#' @export
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
setMethod("initialize", "ReducedDimensionHexPlot", function(.Object, ...) {
    args <- list(...)

    args <- iSEE:::.empty_default(args, .plotBinResolution, 100)
    args[["Downsample"]] <- FALSE

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom SummarizedExperiment colData
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "ReducedDimensionHexPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "ReducedDimensionHexPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    df <- colData(se)
    displayable <- iSEE:::.find_atomic_fields(df)

    subdf <- df[,displayable,drop=FALSE]
    discrete <- iSEE:::.which_groupable(subdf)
    continuous <- iSEE:::.which_numeric(subdf)

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
    facet_cmd <- iSEE:::.add_facets(x)
    if (length(facet_cmd)) {
        N <- length(plot_cmds)
        plot_cmds[[N]] <- paste(plot_cmds[[N]], "+")
        plot_cmds <- c(plot_cmds, facet_cmd)
    }

    # Adding self-brushing boxes, if they exist.
    plot_cmds <- .addMultiSelectionPlotCommands(x,
        flip=(plot_type == "violin_horizontal"),
        envir=envir, commands=plot_cmds)

    list(plot=iSEE:::.text_eval(plot_cmds, envir), commands=plot_cmds)
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
        new_aes <- iSEE:::.build_aes(color=FALSE, shape=shape_set, size=size_set, alt=c(z="ColorBy"))
    } else if (color_set && param_choices[["ColorBy"]] == "Sample name") {
        new_aes <- iSEE:::.build_aes(color=FALSE, shape=shape_set, size=size_set)
    } else {
        new_aes <- iSEE:::.build_aes(color=color_set, shape=shape_set, size=size_set)
    }

    plot_cmds[["hex"]] <- .create_hex(param_choices,
        new_aes, color_set, size_set, color_discrete, is_subsetted)

    # Adding axes labels.
    if (is.null(color_lab) || color_discrete) {
        color_lab <- "Count"
    }
    plot_cmds[["labs"]] <- iSEE:::.build_labs(x=x_lab, y=y_lab, fill=color_lab, shape=shape_lab, size=size_lab, title=title)

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
