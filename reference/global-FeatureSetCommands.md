# Global feature set commands

Set the commands to define the global collection of feature sets. This
is deprecated in favor of
[`registerFeatureSetCommands`](https://isee.github.io/iSEEu/reference/registerFeatureSetCollections.md).

## Usage

``` r
setFeatureSetCommands(value)
```

## Arguments

- value:

  A list of two character vectors named `"collections"` and `"sets"`.
  Both vectors should be of the same length and have the same names.
  Vectors should contain R commands to create collections and retrieve
  sets; see
  `?`[`FeatureSetTable`](https://isee.github.io/iSEEu/reference/FeatureSetTable-class.md)
  and the output of
  [`createGeneSetCommands`](https://isee.github.io/iSEEu/reference/createGeneSetCommands.md)
  for details.

## Value

`setFeatureSetCommands` will set the current global feature set commands
and return `NULL` invisibly.

## See also

[`createGeneSetCommands`](https://isee.github.io/iSEEu/reference/createGeneSetCommands.md),
for one method of generating `value`.

## Author

Aaron Lun

## Examples

``` r
old <- getFeatureSetCommands()

new.cmds <- createGeneSetCommands(organism="org.Mm.eg.db",
    identifier="SYMBOL")
setFeatureSetCommands(new.cmds)

getFeatureSetCommands()
#> $collections
#>                                                                                                                                                                                                                                                                        GO 
#>                                                            ".all_terms <- AnnotationDbi::keys(org.Mm.eg.db::org.Mm.eg.db, keytype='GOALL');\ntab <- AnnotationDbi::select(GO.db::GO.db, keys=.all_terms, columns='TERM');\nrownames(tab) <- tab$GOID;\ntab$GOID <- NULL;" 
#>                                                                                                                                                                                                                                                                      KEGG 
#> ".all_terms <- AnnotationDbi::keys(org.Mm.eg.db::org.Mm.eg.db, keytype='PATH');\ntab <- KEGGREST::keggList('pathway');\ntab <- data.frame(ID=names(tab), Description=tab);\nrownames(tab) <- sub('map', '', tab$ID);\ntab <- tab[intersect(rownames(tab), .all_terms),];" 
#> 
#> $sets
#>                                                                                                                                                                                                                                     GO 
#> ".genes_in_set <- tryCatch(AnnotationDbi::select(org.Mm.eg.db::org.Mm.eg.db, keys=.set_id, keytype='GOALL',\n   column=\"SYMBOL\")[,\"SYMBOL\"], error=function(e) character(0));\nselected <- intersect(rownames(se), .genes_in_set)" 
#>                                                                                                                                                                                                                                   KEGG 
#>  ".genes_in_set <- tryCatch(AnnotationDbi::select(org.Mm.eg.db::org.Mm.eg.db, keys=.set_id, keytype='PATH',\n   column=\"SYMBOL\")[,\"SYMBOL\"], error=function(e) character(0));\nselected <- intersect(rownames(se), .genes_in_set)" 
#> 

setFeatureSetCommands(old)
```
