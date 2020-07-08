.globals <- (function() {
    current <- list()
    list(
        get=function(name) current[[name]],
        set=function(name, value) {
            current[[name]] <<- value
            invisible(NULL)
        }
    )
})()

#' Global feature set commands
#'
#' Get or set the commands to define the global collection of feature sets.
#'
#' @param value A list of two character vectors named \code{"CreateCollections"} and \code{"RetrieveSet"}.
#' Both vectors should be of the same length and have the same names.
#' Vectors should contain R commands to create collections and retrieve sets;
#' see \code{?\linkS4class{FeatureSetTable}} and the output of \code{\link{.createGeneSetCommands}} for details.
#'
#' @return 
#' \code{getFeatureSetCommands} returns the current global feature set commands.
#'
#' \code{setFeatureSetCommands} will set the current global feature set commands and return \code{NULL} invisibly.
#'
#' @details
#' By setting these values, all subsequent constructions of \linkS4class{FeatureSetTable} will have the same set of commands in their \code{"CreateCollections"} and \code{"RetrieveSet"} slots.
#' This allows users to easily customize all \linkS4class{FeatureSetTable} parameters at once.
#' Note that it only applies during the construction of the \linkS4class{FeatureSetTable} and has no effect on the \code{iSEE} application once it starts.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{createGeneSetCommands}}, for one method of generating \code{value}.
#'
#' @examples
#' old <- getFeatureSetCommands()
#'
#' new.cmds <- createGeneSetCommands(organism="org.Mm.eg.db", 
#'     identifier="SYMBOL")
#' setFeatureSetCommands(new.cmds)
#'
#' getFeatureSetCommands()
#'
#' setFeatureSetCommands(old) 
#' @export
#' @rdname global-FeatureSetCommands
getFeatureSetCommands <- function() {
    .globals$get("FeatureSetTable_commands")
}

#' @export
#' @rdname global-FeatureSetCommands
setFeatureSetCommands <- function(value) {
    .globals$set("FeatureSetTable_commands", value)
}
