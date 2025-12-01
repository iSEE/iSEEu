# Register feature set collections

Register feature set collations and their annotations for display in
[FeatureSetTable](https://isee.github.io/iSEEu/reference/FeatureSetTable-class.md)s.

## Usage

``` r
registerFeatureSetCollections(se, collections)

registerFeatureSetCommands(se, commands)

getFeatureSetCollections(se)

getFeatureSetCommands(se)
```

## Arguments

- se:

  The SummarizedExperiment object to be used in `iSEE`.

- collections:

  A named list containing one or more CharacterList objects. Each entry
  represents a collection of feature sets (see Details) and should be
  named.

- commands:

  A named list containing two character vectors of commands to use to
  generate collections and sets.

## Value

For `registerFeatureSetCollections` and `registerFeatureSetCommands`, a
modified `se` is returned that contains the feature set collections or
commands, respectively. This can be used with
[FeatureSetTable](https://isee.github.io/iSEEu/reference/FeatureSetTable-class.md)s
in `iSEE` calls.

For `getFeatureSetCollections`, the list of CharacterLists is returned.
Alternatively `NULL`, if no such list was stored by
`registerFeatureSetCollections`.

For `getFeatureSetCommands`, the list of of commands is returned
containing `collections` and `sets`. Alternatively `NULL`, if no such
list was stored by `registerFeatureSetCommands`.

## Details

Arbitrary feature sets are challenging as there is no obvious place to
store them. `registerFeatureSetCollections` and friends will insert
these sets into the `metadata` of the SummarizedExperiment object,
allowing the corresponding getter functions to quickly extract them
later within the `iSEE` app.

`collections` should be a named list containing CharacterList objects.
Each CharacterList represents a collection where each entry is a feature
set, i.e., a character vector corresponding to some of the row names of
`se`. The `mcols` can contain additional per-set fields (e.g.,
descriptions, enrichment statistics) that will be shown in the
[FeatureSetTable](https://isee.github.io/iSEEu/reference/FeatureSetTable-class.md).

`commands` should be a list containing:

- `collections`, a named character vector where each entry is named
  after a feature set collection. Each entry should be a string
  containing R commands to define a data.frame named `tab`, where each
  row is a feature set and the row names are the names of those sets.

- `sets`, a character vector where each entry is named after a feature
  set collection in the same order as `commands$collections`. Each entry
  should be a string containing R commands to define a character vector
  named `selected` containing the identity of all rows of the
  SummarizedExperiment in the set of interest. (These commands can
  assume that a `.set_id` variable is present containing the name of the
  chosen feature set, as well as the `se` variable containing the input
  SummarizedExperiment object.)

If neither `collections` nor `commands` are provided, any previously
registered content in `se` is removed.

## Author

Aaron Lun

## Examples

``` r
library(scRNAseq)
sce <- LunSpikeInData(location=FALSE)

# Make up some random collections.
random <- CharacterList(
   Aaron = sample(rownames(sce), 10),
   Kevin = sample(rownames(sce), 20),
   Charlotte = sample(rownames(sce), 30),
   Fed = sample(rownames(sce), 40)
)
mcols(random)$p.value <- runif(4)

# Storing the collections inside our SummarizedExperiment.
sce <- registerFeatureSetCollections(sce, list(random=random))
getFeatureSetCollections(sce)
#> $random
#> CharacterList of length 4
#> [["Aaron"]] ENSMUSG00000084856 ENSMUSG00000100813 ... ENSMUSG00000082190
#> [["Kevin"]] ENSMUSG00000102346 ENSMUSG00000092856 ... ENSMUSG00000074062
#> [["Charlotte"]] ENSMUSG00000104218 ENSMUSG00000015519 ... ENSMUSG00000081302
#> [["Fed"]] ENSMUSG00000095874 ENSMUSG00000054206 ... ENSMUSG00000028581
#> 

if (interactive()) {
    iSEE(sce, initial=list(FeatureSetTable()))
}
```
