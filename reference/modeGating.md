# App pre-configured to link multiple feature assay plots

This mode launches a Shiny App preconfigured with multiple chain-linked
feature expression plots for interactive data exploration of the
[SingleCellExperiment](https://rdrr.io/pkg/SingleCellExperiment/man/SingleCellExperiment.html)
or `SummarizedExperiment` object.

## Usage

``` r
modeGating(se, features, plotAssay = NA_character_, ..., plotWidth = 4)
```

## Arguments

- se:

  An object that coercible to
  [SingleCellExperiment-class](https://rdrr.io/pkg/SingleCellExperiment/man/SingleCellExperiment.html)

- features:

  `data.frame` with columns named `x` and `y` that define the features
  on the axes of the linked plots. Plots are serially linked from the
  first row to the last.

- plotAssay:

  The assay (one of assayNames(se)) to use for the plots (character
  vector of length either 1 or equal to `nrow(features)`).

- ...:

  Additional arguments passed to `iSEE()`.

- plotWidth:

  The grid width of linked plots (numeric vector of length either 1 or
  equal to `nrow(features)`

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

# Select top variable genes ----

plot_count <- 6
rv <- rowVars(assay(sce, "tophat_counts"))
top_var <- head(order(rv, decreasing=TRUE), plot_count*2)
top_var_genes <- rownames(sce)[top_var]

plot_features <- data.frame(
    x=head(top_var_genes, plot_count),
    y=tail(top_var_genes, plot_count),
    stringsAsFactors=FALSE
 )

# launch the app itself ----

app <- modeGating(sce, features = plot_features)
if (interactive()) {
  shiny::runApp(app, port=1234)
}
```
