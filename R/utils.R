msg <- function(x, startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("iSEE.quiet"))) {
      rlang::inform(x, class = "packageStartupMessage")
    }
  } else {
    rlang::inform(x)
  }
}
