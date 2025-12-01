# Gene set utilities

Utility functions to control the behavior of the
[GeneSetTable](https://isee.github.io/iSEEu/reference/GeneSetTable-class.md).

## Usage

``` r
.getIdentifierType()

.setIdentifierType(value)

.getOrganism()

.setOrganism(value)

.getGeneSetCommands(collection, mode)

.setGeneSetCommands(value)
```

## Arguments

- value:

  For `.setIdentifierType` and `.setOrganism`, a string containing the
  type of identifier or organism package to use.

  For `.setGeneSetCommands`, a named list containing two character
  vectors, see Details.

- collection:

  String specifying the gene set collection.

- mode:

  String specifying the mode of operation for the returned commands.

## Value

`.getIdentifierType` will return the identifier type to use, defaulting
to `"ENTREZID"`.

`.getOrganism` will return the organism package to use, defaulting
`"org.Hs.eg.db"`.

`.getGeneSetCommands` will return:

- If `mode="show"`, a string containing R commands that create `tab`, a
  data.frame of all gene sets for a given `collection`.

- If `mode="extract"`, a format string containing R commands that (after
  formatting) create `selected`, a character vector of gene identities
  for the selected gene set. This format string should accept one string
  argument corresponding to the deparsed name of the gene set.

Each of the setter functions will set the corresponding option and
return `NULL`, invisibly.

## Details

By default, `.getGeneSetCommands` will extract GO and KEGG terms. The
organism and identifier type relates to the manner in which this default
extraction is performed.

Users can add their own gene set collections by supplying a named list
to `.setGeneSetCommands`. Each element of the list should be a named
character vector of length two, with names `"show"` and `"extract"` -
see the return value for what these are. The names of the list should be
unique and will be used in the
[GeneSetTable](https://isee.github.io/iSEEu/reference/GeneSetTable-class.md)
interface.

Alternatively, any element of the list may be `NULL`, in which case it
is excluded from the interface. This is useful for setting, e.g.,
`GO=NULL` to ignore the in-built GO terms.

## See also

[GeneSetTable](https://isee.github.io/iSEEu/reference/GeneSetTable-class.md),
where these functions have their effect.

## Author

Aaron Lun

## Examples

``` r
.setIdentifierType("ENSEMBLID")
#> Warning: '.setIdentifierType' is deprecated.
#> Use 'setFeatureSetCommands' instead.
#> See help("Deprecated")
.getIdentifierType()
#> Warning: '.getIdentifierType' is deprecated.
#> Use 'getFeatureSetCommands' instead.
#> See help("Deprecated")
#> [1] "ENSEMBLID"

.setOrganism("org.Mm.eg.db")
#> Warning: '.setOrganism' is deprecated.
#> Use 'setFeatureSetCommands' instead.
#> See help("Deprecated")
.getOrganism()
#> Warning: '.getOrganism' is deprecated.
#> Use 'getFeatureSetCommands' instead.
#> See help("Deprecated")
#> [1] "org.Mm.eg.db"

.getGeneSetCommands("GO", "show")
#> Warning: '.getGeneSetCommands' is deprecated.
#> Use 'getFeatureSetCommands' instead.
#> See help("Deprecated")
#> Warning: '.getOrganism' is deprecated.
#> Use 'getFeatureSetCommands' instead.
#> See help("Deprecated")
#> Warning: '.getOrganism' is deprecated.
#> Use 'getFeatureSetCommands' instead.
#> See help("Deprecated")
#> [1] ".all_terms <- AnnotationDbi::keys(org.Mm.eg.db::org.Mm.eg.db, keytype='GO');\ntab <- AnnotationDbi::select(GO.db::GO.db, keys=.all_terms, columns='TERM');\nrownames(tab) <- tab$GOID;\ntab$GOID <- NULL;"
.getGeneSetCommands("GO", "extract")
#> Warning: '.getGeneSetCommands' is deprecated.
#> Use 'getFeatureSetCommands' instead.
#> See help("Deprecated")
#> Warning: '.getIdentifierType' is deprecated.
#> Use 'getFeatureSetCommands' instead.
#> See help("Deprecated")
#> Warning: '.getOrganism' is deprecated.
#> Use 'getFeatureSetCommands' instead.
#> See help("Deprecated")
#> Warning: '.getOrganism' is deprecated.
#> Use 'getFeatureSetCommands' instead.
#> See help("Deprecated")
#> [1] ".genes_in_set <- tryCatch(AnnotationDbi::select(org.Mm.eg.db::org.Mm.eg.db, keys=%s, keytype='GO',\n   column=\"ENSEMBLID\")[,\"ENSEMBLID\"], error=function(e) character(0));\nselected <- intersect(rownames(se), .genes_in_set)"

.setGeneSetCommands(
    list(AaronRandomCollection=
        c(
            show='tab <- some_function_to_list_my_gene_sets()',
            extract='selected <- some_function_to_get_one_gene_set(%s)'
        )
    )
)
#> Warning: '.setGeneSetCommands' is deprecated.
#> Use 'setFeatureSetCommands' instead.
#> See help("Deprecated")

.getGeneSetCommands("AaronRandomCollection", "show")
#> Warning: '.getGeneSetCommands' is deprecated.
#> Use 'getFeatureSetCommands' instead.
#> See help("Deprecated")
#> [1] "tab <- some_function_to_list_my_gene_sets()"
.getGeneSetCommands("AaronRandomCollection", "extract")
#> Warning: '.getGeneSetCommands' is deprecated.
#> Use 'getFeatureSetCommands' instead.
#> See help("Deprecated")
#> [1] "selected <- some_function_to_get_one_gene_set(%s)"
```
