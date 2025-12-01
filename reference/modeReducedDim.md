# App pre-configured to compare multiple reduced dimension plots

This mode launches a Shiny App preconfigured with multiple linked
reduced dimension plots for interactive data exploration of the
[`SingleCellExperiment`](https://rdrr.io/pkg/SingleCellExperiment/man/SingleCellExperiment.html)
object.

## Usage

``` r
modeReducedDim(
  se,
  includeNames = reducedDimNames(se),
  colorBy = NULL,
  ...,
  plotWidth = NULL
)
```

## Arguments

- se:

  An object that coercible to
  [SingleCellExperiment](https://rdrr.io/pkg/SingleCellExperiment/man/SingleCellExperiment.html)

- includeNames:

  Character vector with the names of reduced dimensions to display as
  individual panels. The default uses all available in
  `reducedDimNames(se)`.

- colorBy:

  Character scalar controlling coloring of cells. Must match either to
  one of `colnames(colData(se))` or `rownames(se)`. If coloring by a
  colData column, a column data plot is opened in addition to the
  reduced dimension panels. If coloring by a feature, a row statistics
  table is openend in addition to the reduced dimension panels, from
  which the latter are receiving the color.

- ...:

  Additional arguments passed to `iSEE`.

- plotWidth:

  The grid width of linked plots (numeric vector of length either 1 or
  equal to `length(includeNames)`). The total width of the window is 12,
  so `plotWidth = 4` for example will show three panels per row. If
  `plotWidth = NULL` (the default), a value will be estimated depending
  on the number of reduced dimension panels.

## Value

A Shiny app object is returned.

## Examples

``` r
library(scRNAseq)

# Example data ----
sce <- ReprocessedAllenData(assays="tophat_counts")
class(sce)
#> [1] "SingleCellExperiment"
#> attr(,"package")
#> [1] "SingleCellExperiment"

library(scater)
sce <- logNormCounts(sce, exprs_values="tophat_counts")
sce <- runPCA(sce, ncomponents = 30)
sce <- runTSNE(sce)
sce <- runUMAP(sce)
reducedDimNames(sce)
#> [1] "PCA"  "TSNE" "UMAP"

# launch the app ----
# ... coloring by a column data variable
app <- modeReducedDim(sce, colorBy = "Primary.Type")
if (interactive()) {
    shiny::runApp(app, port=1234)
}
# ... coloring by a feature
app <- modeReducedDim(sce, colorBy = "Scnn1a")
if (interactive()) {
    shiny::runApp(app, port=1234)
}
```
