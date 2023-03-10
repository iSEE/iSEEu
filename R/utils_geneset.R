#' Gene set utilities
#'
#' Utility functions to control the behavior of the \linkS4class{GeneSetTable}.
#'
#' @param value For \code{.setIdentifierType} and \code{.setOrganism},
#' a string containing the type of identifier or organism package to use.
#'
#' For \code{.setGeneSetCommands}, a named list containing two character vectors, see Details.
#' @param collection String specifying the gene set collection.
#' @param mode String specifying the mode of operation for the returned commands.
#'
#' @return
#' \code{.getIdentifierType} will return the identifier type to use, defaulting to \code{"ENTREZID"}.
#'
#' \code{.getOrganism} will return the organism package to use, defaulting \code{"org.Hs.eg.db"}.
#'
#' \code{.getGeneSetCommands} will return:
#' \itemize{
#' \item If \code{mode="show"}, a string containing R commands that create \code{tab}, 
#' a data.frame of all gene sets for a given \code{collection}.
#' \item If \code{mode="extract"}, a format string containing R commands that (after formatting) create \code{selected},
#' a character vector of gene identities for the selected gene set.
#' This format string should accept one string argument corresponding to the deparsed name of the gene set.
#' }
#'
#' Each of the setter functions will set the corresponding option and return \code{NULL}, invisibly.
#'
#' @details
#' By default, \code{.getGeneSetCommands} will extract GO and KEGG terms.
#' The organism and identifier type relates to the manner in which this default extraction is performed.
#'
#' Users can add their own gene set collections by supplying a named list to \code{.setGeneSetCommands}.
#' Each element of the list should be a named character vector of length two,
#' with names \code{"show"} and \code{"extract"} - see the return value for what these are.
#' The names of the list should be unique and will be used in the \linkS4class{GeneSetTable} interface.
#'
#' Alternatively, any element of the list may be \code{NULL}, in which case it is excluded from the interface.
#' This is useful for setting, e.g., \code{GO=NULL} to ignore the in-built GO terms.
#'
#' @author Aaron Lun
#' @examples
#' .setIdentifierType("ENSEMBLID")
#' .getIdentifierType()
#'
#' .setOrganism("org.Mm.eg.db")
#' .getOrganism()
#'
#' .getGeneSetCommands("GO", "show")
#' .getGeneSetCommands("GO", "extract")
#'
#' .setGeneSetCommands(
#'     list(AaronRandomCollection=
#'         c(
#'             show='tab <- some_function_to_list_my_gene_sets()',
#'             extract='selected <- some_function_to_get_one_gene_set(%s)'
#'         )
#'     )
#' )
#' 
#' .getGeneSetCommands("AaronRandomCollection", "show")
#' .getGeneSetCommands("AaronRandomCollection", "extract")
#' 
#' @seealso
#' \linkS4class{GeneSetTable}, where these functions have their effect.
#' @name utils-geneset
NULL

#' @export
#' @rdname utils-geneset
.getIdentifierType <- function() {
    .Deprecated(new="getFeatureSetCommands")
    global <- getOption("iSEEu_gene_set_id", NULL)
    if (is.null(global)) {
        "ENTREZID"
    } else {
        global
    }
}

#' @export
#' @rdname utils-geneset
.setIdentifierType <- function(value) {
    .Deprecated(new="setFeatureSetCommands")
    options(iSEEu_gene_set_id=value)
    invisible(NULL)
}

#' @export
#' @rdname utils-geneset
.getOrganism <- function() {
    .Deprecated(new="getFeatureSetCommands")
    global <- getOption("iSEEu_gene_set_species", NULL)
    if (is.null(global)) {
        "org.Hs.eg.db"
    } else {
        global
    }
}

#' @export
#' @rdname utils-geneset
.setOrganism <- function(value) {
    .Deprecated(new="setFeatureSetCommands")
    options(iSEEu_gene_set_species=value)
    invisible(NULL)
}

#' @export
#' @rdname utils-geneset
.getGeneSetCommands <- function(collection, mode) {
    .Deprecated(new="getFeatureSetCommands")
    global <- getOption("iSEEu_gene_set_commands", NULL)[[collection]]
    if (!is.null(global)) {
        return(global[[mode]])
    }

    if (collection=="GO") {
        nm <- "GO"
    } else {
        nm <- "PATH"
    }

    if (mode=="show") {
        term_cmd <- sprintf(".all_terms <- AnnotationDbi::keys(%s::%s, keytype='%s');", .getOrganism(), .getOrganism(), nm)
        if (collection=="GO") {
            paste(
                term_cmd,
                "tab <- AnnotationDbi::select(GO.db::GO.db, keys=.all_terms, columns='TERM');",
                "rownames(tab) <- tab$GOID;",
                "tab$GOID <- NULL;", 
                sep="\n")
        } else {
            paste(
                term_cmd,
                "tab <- read.delim('http://rest.kegg.jp/list/pathway', header=FALSE);",
                "colnames(tab) <- c('ID', 'Description');",
                "rownames(tab) <- sub('map', '', tab$ID);",
                "tab <- tab[intersect(rownames(tab), .all_terms),];",
                sep="\n"
            )
        }
    } else {
        dp <- deparse(.getIdentifierType())
        paste(
            sprintf(".genes_in_set <- tryCatch(AnnotationDbi::select(%s::%s, keys=%%s, keytype='%s',", 
                .getOrganism(), .getOrganism(), nm),
            sprintf("   column=%s)[,%s], error=function(e) character(0));", dp, dp),
            "selected <- intersect(rownames(se), .genes_in_set)",
            sep="\n"
        )
    }
}

#' @export
#' @rdname utils-geneset
.setGeneSetCommands <- function(value) {
    .Deprecated(new="setFeatureSetCommands")
    options(iSEEu_gene_set_commands=value)
    invisible(NULL)
}

.list_available_gene_sets <- function() {
    global <- getOption("iSEEu_gene_set_commands", NULL)
    available <- names(global)[vapply(global, is.null, TRUE)]

    for (default in c("GO", "KEGG")) {
        if (!default %in% names(global)) {
            available <- c(available, default)
        }
    }

    available
}
