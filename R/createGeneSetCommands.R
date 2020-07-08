#' Create gene set commands
#'
#' Create the commands required to populate \linkS4class{FeatureSetTable}s with commonly used gene sets.
#'
#' @param collections Character vectors specifying the gene set collections of interest.
#' @param organism String containing the \pkg{org.*.eg.db} package to use to extract mappings of gene sets to gene IDs.
#' @param identifier String specifying the identifier to use to extract IDs for the organism package.
#'
#' @return
#' A list of character vectors describing how to create collections and retrieve gene sets.
#' These can be used as arguments for the \code{\link{FeatureSetTable}} constructor.
#'
#' @details
#' GO terms are extracted using the \code{"GOALL"} mode,
#' which extracts both direct and indirect children of each term.
#' A description for each GO term is extracted using the \pkg{GO.db} package.
#'
#' Mappings of genes to KEGG pathway are extracted from the organism package using the \code{"PATH"} term.
#' Unfortunately, this is not particularly up-to-date due to the ridiculous licensing around KEGG terms.
#' Descriptions for each pathway are extracted from \url{http://rest.kegg.jp/list/pathway}.
#' 
#' @author Aaron Lun
#'
#' @examples
#' out <- createFeatureSetCommands()
#' cat(out$CreateCollections['GO'], "\n")
#' cat(out$RetrieveSet['GO'], "\n")
#' 
#' @seealso
#' \linkS4class{FeatureSetTable}, where the commands are intended for use.
#' @export
createGeneSetCommands <- function(collections=c("GO", "KEGG"), organism="org.Hs.eg.db", identifier="ENTREZID") {
    init <- retrieve <- list()

    retrieve.format <- paste(
        sprintf(".genes_in_set <- tryCatch(AnnotationDbi::select(%s::%s, keys=.set_id, keytype='%%s',", organism, organism),
        sprintf("   column=%s)[,%s], error=function(e) character(0));", deparse(identifier), deparse(identifier)),
        "selected <- intersect(rownames(se), .genes_in_set)",
        sep="\n"
    )

    init.format <- sprintf(".all_terms <- AnnotationDbi::keys(%s::%s, keytype='%%s');", organism, organism)

    if ("GO" %in% collections) {
        init[["GO"]] <- paste(
            sprintf(init.format, "GOALL"),
            "tab <- AnnotationDbi::select(GO.db::GO.db, keys=.all_terms, columns='TERM');",
            "rownames(tab) <- tab$GOID;",
            "tab$GOID <- NULL;",
            sep="\n")

        retrieve[["GO"]] <- sprintf(retrieve.format, "GOALL")
    }

    if ("KEGG" %in% collections) {
         init[["KEGG"]] <- paste( 
            sprintf(init.format, "PATH"),
            "tab <- read.delim('http://rest.kegg.jp/list/pathway', header=FALSE);",
            "colnames(tab) <- c('ID', 'Description');",
            "rownames(tab) <- sub('path:map', '', tab$ID);",
            "tab <- tab[intersect(rownames(tab), .all_terms),];",
            sep="\n")

         retrieve[["KEGG"]] <- sprintf(retrieve.format, "PATH")
    }

    list(CreateCollections=unlist(init), RetrieveSet=unlist(retrieve))
}

