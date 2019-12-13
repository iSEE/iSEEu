#' New iSEE panel class addin.
#'
#' Runs the addin miniUI to create a new iSEE panel class.
#'
#' @return `NULL`, invisibly.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_new_panel_addin
new_panel_addin = function() {
  sys.source(system.file(package = 'iSEEu', 'scripts', 'new_panel.R'))
}

#' List available parent classes for new iSEE panel classes
#'
#' Collects the list of classes - both virtual and concrete - defined in either [iSEE::iSEE-pkg] or `iSEEu`, and that extend the [iSEE::Panel-class].
#'
#' @return A character vector
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_collect_parent_classes
#' @importFrom methods extends getClasses
collect_parent_classes <- function() {
	x <- unique(c(getClasses("package:iSEE"), getClasses("package:iSEEu")))
	is_panel <- function(Class) {
		extends(Class, "Panel")
	}
	keep <- vapply(x, is_panel, FUN.VALUE = logical(1))
	x[keep]
}

#' Create a new panel file
#'
#' Opens a template R script in the editor, to define a new iSEE panel class .
#'
#' @param encoded Name of the new panel class.
#' @param decoded Extended name of the new panel class (for display).
#' @param parent Name of the parent panel class
#'
#' @export
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso \linkS4class{Panel}
#'
#' @examples
#' new_panel_file("NewRedDimPlot", "New reduced dimension plot", "RedDimPlot")
new_panel_file <- function(encoded, decoded, parent="Panel") {
  template_file <- system.file(package = "iSEEu", "templates", "NewPanel.R")
  template_content <- scan(template_file, "character", sep = "\n", quiet = TRUE, blank.lines.skip = FALSE)
  template_content <- paste0(template_content, collapse = "\n")
  template_content <- gsub("__ENCODED__", encoded, template_content, fixed = TRUE)
  template_content <- gsub("__DECODED__", decoded, template_content, fixed = TRUE)
  template_content <- gsub("__PARENT__", parent, template_content, fixed = TRUE)
  rstudioapi::documentNew(template_content, type = "r")
}
