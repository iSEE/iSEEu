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
