#' Register feature set collections
#'
#' Register feature set collations and their annotations for display in \linkS4class{FeatureSetTable}s.
#'
#' @param se The \linkS4class{SummarizedExperiment} object to be used in \code{\link{iSEE}}.
#' @param collections A named list containing one or more \linkS4class{CharacterList} objects.
#' Each entry represents a collection of feature sets (see Details) and should be named.
#' @param collection String containing the name of the collection to retrieve.
#' @param commands A named list containing two character vectors of commands to use to generate collections and sets.
#'
#' @return 
#' For \code{registerCollections}, \code{se} is returned with embedded feature set collections.
#' This can be used with \linkS4class{FeatureSetTable}s in \code{\link{iSEE}} calls.
#'
#' For \code{retrieveCollection}, the CharacterList corresponding to \code{collection} is returned.
#'
#' @author Aaron Lun
#' 
#' @details
#' Arbitrary feature sets are challenging as there is no obvious place to store them.
#' \code{registerCollections} will insert them into the \code{\link{metadata}} of the SummarizedExperiment object,
#' allowing \code{retrieveCollection} to quickly extract them later within the \code{iSEE} app.
#'
#' If \code{collections} is supplied, it should be a named list containing \linkS4class{CharacterList} objects.
#' Each \linkS4class{CharacterList} represents a collection where each entry is a feature set, i.e., a character vector corresponding to some of the row names of \code{se}. 
#' The \code{\link{mcols}} can contain additional per-set fields (e.g., descriptions, enrichment statistics) that will be shown in the \linkS4class{FeatureSetTable}.
#'
#' If \code{collections} is not supplied, the function looks at whether \code{commands} is provided.
#' If so, it should contain R commands instructing \code{\link{iSEE}} to create the collections and sets on the fly.
#' The entries of the list should be named:
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
#' # Storing the collections inside my object.
#' sce <- registerCollections(sce, list(random=random))
#' retrieveCollection(sce, "random")
#'
#' if (interactive()) {
#'     iSEE(sce, initial=list(FeatureSetTable()))
#' }
#' 
#' @export
#' @importClassesFrom IRanges CharacterList
#' @importFrom S4Vectors metadata metadata<- mcols
registerCollections <- function(se, collections, commands) {
    value <- list()

    # TODO: fix the handling of deeply nested lists.
    meta <- metadata(se)[["iSEEu"]]
    if (is.null(meta)) { 
        meta <- list() 
    }

    if (!missing(collections)) {
        .validate_collections(collections)
        meta$collections <- collections
    } else if (!missing(commands)) {
        .validate_commands(commands)
        meta$commands <- commands
    } else {
        meta <- NULL
    }

    meta$FeatureSetTable <- meta
    metadata(se)[["iSEEu"]] <- meta

    se
}

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
#' @rdname registerCollections
#' @importFrom S4Vectors metadata 
retrieveCollection <- function(se, collection=NULL) {
    all.collections <- metadata(se)[["iSEEu"]]$FeatureSetTable$collections
    if (is.null(collection)) {
        all.collections
    } else {
        all.collections[[collection]]
    }
}
