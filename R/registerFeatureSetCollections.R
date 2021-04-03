#' Register feature set collections
#'
#' Register feature set collations and their annotations for display in \linkS4class{FeatureSetTable}s.
#'
#' @param se The \linkS4class{SummarizedExperiment} object to be used in \code{\link{iSEE}}.
#' @param collections A named list containing one or more \linkS4class{CharacterList} objects.
#' Each entry represents a collection of feature sets (see Details) and should be named.
#' @param commands A named list containing two character vectors of commands to use to generate collections and sets.
#'
#' @return 
#' For \code{registerFeatureSetCollections} and \code{registerFeatureSetCommands},
#' a modified \code{se} is returned that contains the feature set collections or commands, respectively.
#' This can be used with \linkS4class{FeatureSetTable}s in \code{\link{iSEE}} calls.
#'
#' For \code{getFeatureSetCollections}, the list of CharacterLists is returned.
#' Alternatively \code{NULL}, if no such list was stored by \code{registerFeatureSetCollections}.
#'
#' For \code{getFeatureSetCommands}, the list of of commands is returned containing \code{collections} and \code{sets}.
#' Alternatively \code{NULL}, if no such list was stored by \code{registerFeatureSetCommands}.
#' 
#' @author Aaron Lun
#' 
#' @details
#' Arbitrary feature sets are challenging as there is no obvious place to store them.
#' \code{registerFeatureSetCollections} and friends will insert these sets into the \code{\link{metadata}} of the SummarizedExperiment object,
#' allowing the corresponding getter functions to quickly extract them later within the \code{iSEE} app.
#'
#' \code{collections} should be a named list containing \linkS4class{CharacterList} objects.
#' Each \linkS4class{CharacterList} represents a collection where each entry is a feature set, i.e., a character vector corresponding to some of the row names of \code{se}. 
#' The \code{\link{mcols}} can contain additional per-set fields (e.g., descriptions, enrichment statistics) that will be shown in the \linkS4class{FeatureSetTable}.
#'
#' \code{commands} should be a list containing:
#' \itemize{
#' \item \code{collections}, a named character vector where each entry is named after a feature set collection.
#' Each entry should be a string containing R commands to define a data.frame named \code{tab}, where each row is a feature set and the row names are the names of those sets.
#' \item \code{sets}, a character vector where each entry is named after a feature set collection in the same order as \code{commands$collections}.
#' Each entry should be a string containing R commands to define a character vector named \code{selected} containing the identity of all rows of the SummarizedExperiment in the set of interest.
#' (These commands can assume that a \code{.set_id} variable is present containing the name of the chosen feature set,
#' as well as the \code{se} variable containing the input SummarizedExperiment object.)
#' }
#'
#' If neither \code{collections} nor \code{commands} are provided, any previously registered content in \code{se} is removed.
#' 
#' @examples
#' library(scRNAseq)
#' sce <- LunSpikeInData(location=FALSE)
#'
#' # Make up some random collections.
#' random <- CharacterList(
#'    Aaron = sample(rownames(sce), 10),
#'    Kevin = sample(rownames(sce), 20),
#'    Charlotte = sample(rownames(sce), 30),
#'    Fed = sample(rownames(sce), 40)
#' )
#' mcols(random)$p.value <- runif(4)
#'
#' # Storing the collections inside our SummarizedExperiment.
#' sce <- registerFeatureSetCollections(sce, list(random=random))
#' getFeatureSetCollection(sce)
#'
#' if (interactive()) {
#'     iSEE(sce, initial=list(FeatureSetTable()))
#' }
#' 
#' @export
#' @rdname registerFeatureSetCollections
registerFeatureSetCollections <- function(se, collections) {
    if (!missing(collections)) {
        .validate_collections(collections)
        registerAppOptions(se, iSEEu_FeatureSetTable_collections=collections)
    } else {
        existing <- getAllAppOptions(se)
        existing <- existing[names(existing)!="iSEEu_FeatureSetTable_collections"]
        registerAppOptions(se, existing)
    }
}

#' @export
#' @rdname registerFeatureSetCollections
registerFeatureSetCommands <- function(se, commands) {
    if (!missing(commands)) {
        .validate_commands(commands)
        registerAppOptions(se, iSEEu_FeatureSetTable_commands=commands)
    } else {
        existing <- getAllAppOptions(se)
        existing <- existing[names(existing)!="iSEEu_FeatureSetTable_commands"]
        registerAppOptions(se, existing)
    }
}

#' @importClassesFrom IRanges CharacterList
.validate_collections <- function(collections) {
    if (length(collections)) {
        nms <- names(collections)
        if (is.null(nms) || anyDuplicated(nms) || any(nms=="")) {
            stop("'names(collections)' must be unique and non-empty")
        }

        for (x in collections) {
            if (!is(x, "CharacterList") || is.null(names(x))) {
                stop("each entry of '...' should be a named CharacterList")
            }
        }
    }
}

.validate_commands <- function(commands) {
    nms <- names(commands$collections)
    if (is.null(nms) || anyDuplicated(nms)) {
        stop("'names(commands$collections) must be non-NULL and unique")
    }
    if (!identical(nms, names(commands$sets))) {
        stop("'names(commands$collections)' and 'names(commands$sets)' must be the same")
    }
}

#' @export
#' @rdname registerFeatureSetCollections
getFeatureSetCollections <- function(se) {
    getAppOption("iSEEu_FeatureSetTable_collections", se)
}

#' @export
#' @rdname registerFeatureSetCollections
getFeatureSetCommands <- function(se) {
    if (missing(se)) {
        .globals$get("FeatureSetCommands")
    } else {
        getAppOption("iSEEu_FeatureSetTable_commands", se)
    }
}
