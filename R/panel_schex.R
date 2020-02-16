#' @export
setClass("schexPlot", contains="ReducedDimPlot")

#' @export
schexPlot <- function(...) {
    new("schexPlot", ...)
}

#' @export
setMethod(".fullName", "schexPlot", function(x) "schex plot")

#' @export
setMethod(".panelColor", "schexPlot", function(x) "#991717")

setMethod(".generateDotPlotData", "schexPlot", function(x, envir){
    data_cmds <- list()

    data_cmds[["make_hexbin"]] <- sprintf("se.schex <- schex::make_hexbin(se, 80, dimension_reduction = %s)",
        deparse(x[[iSEE:::.redDimType]]))

    gene_selected_y <- x[[iSEE:::.colorByFeatName]]
    plot_title <- gene_selected_y

    data_cmds <- unlist(data_cmds)
    iSEE:::.text_eval(data_cmds, envir)

    list(commands = data_cmds, labels = list(title = plot_title))
})

setMethod(".generateDotPlot", "schexPlot", function(x, labels, envir) {
    plot_cmds <- list()

    # TODO: iSEE:::.colorBySampNameTitle should not be an option
    # TODO: action='mean' does not support discrete factor or character values
    if (identical(x[[iSEE:::.colorByField]], iSEE:::.colorByFeatNameTitle)) {
        plot_hexbin_cmd <- sprintf("schex::plot_hexbin_feature(se.schex, feature=%s, action='mean', type=%s) +",
            deparse(x[[iSEE:::.colorByFeatName]]),
            deparse(x[[iSEE:::.colorByFeatNameAssay]])
            )
    } else if (identical(x[[iSEE:::.colorByField]], iSEE:::.colorByColDataTitle)) {
        plot_hexbin_cmd <- sprintf("schex::plot_hexbin_meta(se.schex, col=%s, action='mean') +",
            deparse(x[[iSEE:::.colorByColData]]))
    } else { # iSEE:::.colorByNothingTitle
        plot_hexbin_cmd <- sprintf("plot_hexbin_density(se.schex, title = NULL, xlab = NULL, ylab = NULL) +")
    }

    plot_cmds[["ggplot"]] <- plot_hexbin_cmd

    # Adding hexbins to the plot.
    plot_cmds[["theme_base"]] <- "theme_bw() +"
    plot_cmds[["theme_legend"]] <- "theme(legend.position = 'bottom')"

    gg_plot <- eval(parse(text=plot_cmds), envir)

    list(plot=gg_plot, commands=plot_cmds)
})

setMethod(".generateOutput", "schexPlot", function(x, se, all_memory, all_contents) {
    # Initialize an environment storing information for generating ggplot commands
    plot_env <- new.env()
    plot_env$se <- se
    plot_env$colormap <- metadata(se)$colormap

    all_cmds <- list()
    all_labels <- list()

    # Doing this first so that .generateDotPlotData can respond to the selection.
    all_cmds$select <- .processMultiSelections(x, all_memory, all_contents, plot_env)

    xy_out <- .generateDotPlotData(x, plot_env)
    all_cmds$xy <- xy_out$commands
    all_labels <- c(all_labels, xy_out$labels)

    # Collect the plot coordinates BEFORE downsampling (which alters the environment value)
    panel_data <- plot_env$plot.data

    plot_out <- .generateDotPlot(x, all_labels, plot_env)
    all_cmds$plot <- plot_out$commands

    list(commands=all_cmds, contents=panel_data, plot=plot_out$plot)
})
