#' @export
setClass("schexPlot", contains="ReducedDimPlot")

#' @export
schexPlot <- function(...) {
    new("schexPlot", ...)
}
