# Create gene set commands

Create the commands required to populate
[FeatureSetTable](https://isee.github.io/iSEEu/reference/FeatureSetTable-class.md)s
with commonly used gene sets.

## Usage

``` r
createGeneSetCommands(
  collections = c("GO", "KEGG"),
  organism = "org.Hs.eg.db",
  identifier = "ENTREZID"
)
```

## Arguments

- collections:

  Character vectors specifying the gene set collections of interest.

- organism:

  String containing the org.\*.eg.db package to use to extract mappings
  of gene sets to gene IDs.

- identifier:

  String specifying the identifier to use to extract IDs for the
  organism package.

## Value

A list of two character vectors describing how to create collections and
retrieve gene sets. This follows the expectations for `commands` in
[`registerFeatureSetCommands`](https://isee.github.io/iSEEu/reference/registerFeatureSetCollections.md).

## Details

GO terms are extracted using the `"GOALL"` mode, which extracts both
direct and indirect children of each term. A description for each GO
term is extracted using the GO.db package.

Mappings of genes to KEGG pathway are extracted from the organism
package using the `"PATH"` term. Unfortunately, this is not up to date
due to the licensing around KEGG terms. Descriptions for each pathway
are extracted from <http://rest.kegg.jp/list/pathway>.

The output of this function can be used as the `commands` argument of
[`registerFeatureSetCommands`](https://isee.github.io/iSEEu/reference/registerFeatureSetCollections.md).
It is also used by default in the
[`FeatureSetTable`](https://isee.github.io/iSEEu/reference/FeatureSetTable-class.md)
constructor when no collections are registered.

## See also

[FeatureSetTable](https://isee.github.io/iSEEu/reference/FeatureSetTable-class.md),
where the commands are intended for use.

[`registerFeatureSetCommands`](https://isee.github.io/iSEEu/reference/registerFeatureSetCollections.md),
to use the commands globally.

## Author

Aaron Lun

## Examples

``` r
out <- createGeneSetCommands()
cat(out$collections['GO'], "\n")
#> .all_terms <- AnnotationDbi::keys(org.Hs.eg.db::org.Hs.eg.db, keytype='GOALL');
#> tab <- AnnotationDbi::select(GO.db::GO.db, keys=.all_terms, columns='TERM');
#> rownames(tab) <- tab$GOID;
#> tab$GOID <- NULL; 
cat(out$sets['GO'], "\n")
#> .genes_in_set <- tryCatch(AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, keys=.set_id, keytype='GOALL',
#>    column="ENTREZID")[,"ENTREZID"], error=function(e) character(0));
#> selected <- intersect(rownames(se), .genes_in_set) 
```
