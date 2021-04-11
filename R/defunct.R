#' Defunct functions
#'
#' Pretty much as it says here. These functions are all defunct and should not be used.
#' 
#' @param ... Ignored.
#'
#' @return 
#' All functions error out with a defunct message pointing towards its replacement (if available).
#'
#' @author Aaron Lun
#'
#' @name defunct
NULL

#' @export
#' @rdname defunct
.getAcceptablePValueFields <- function(...) {
    .Defunct("getPValueFields")
}

#' @export
#' @rdname defunct
.getAcceptableAveAbFields <- function(...) {
    .Defunct("getAveAbFields")
}

#' @export
#' @rdname defunct
.getAcceptableLogFCFields <- function(...) {
    .Defunct("getLogFCFields")
}

#' @export
#' @rdname defunct
.setAcceptablePValueFields <- function(...) {
    .Defunct("registerPValueFields")
}

#' @export
#' @rdname defunct
.setAcceptableAveAbFields <- function(...) {
    .Defunct("registerAveAbFields")
}

#' @export
#' @rdname defunct
.setAcceptableLogFCFields <- function(...) {
    .Defunct("registerLogFCFields")
}
