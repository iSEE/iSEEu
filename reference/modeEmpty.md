# App pre-configured to launch with no visible panel

This mode launches an app that does not display any panel.

## Usage

``` r
modeEmpty(...)
```

## Arguments

- ...:

  Arguments passed to `iSEE()`.

## Value

A Shiny app object is returned.

## Details

This mode presents the advantage to launch an interface in a minimal
amount of time, as it does not need to render any panel when the
interface is launched. Users can then use the `"Organize panels"` widget
to select panels to display in the interface.

## Examples

``` r
example("SingleCellExperiment")
#> 
#> SnglCE> ncells <- 100
#> 
#> SnglCE> u <- matrix(rpois(20000, 5), ncol=ncells)
#> 
#> SnglCE> v <- log2(u + 1)
#> 
#> SnglCE> pca <- matrix(runif(ncells*5), ncells)
#> 
#> SnglCE> tsne <- matrix(rnorm(ncells*2), ncells)
#> 
#> SnglCE> sce <- SingleCellExperiment(assays=list(counts=u, logcounts=v),
#> SnglCE+     reducedDims=SimpleList(PCA=pca, tSNE=tsne))
#> 
#> SnglCE> sce
#> class: SingleCellExperiment 
#> dim: 200 100 
#> metadata(0):
#> assays(2): counts logcounts
#> rownames: NULL
#> rowData names(0):
#> colnames: NULL
#> colData names(0):
#> reducedDimNames(2): PCA tSNE
#> mainExpName: NULL
#> altExpNames(0):
#> 
#> SnglCE> ## coercion from SummarizedExperiment
#> SnglCE> se <- SummarizedExperiment(assays=list(counts=u, logcounts=v))
#> 
#> SnglCE> as(se, "SingleCellExperiment")
#> class: SingleCellExperiment 
#> dim: 200 100 
#> metadata(0):
#> assays(2): counts logcounts
#> rownames: NULL
#> rowData names(0):
#> colnames: NULL
#> colData names(0):
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):
#> 
#> SnglCE> ## coercion from RangedSummarizedExperiment
#> SnglCE> rse <- as(se, "RangedSummarizedExperiment")
#> 
#> SnglCE> as(rse, "SingleCellExperiment")
#> class: SingleCellExperiment 
#> dim: 200 100 
#> metadata(0):
#> assays(2): counts logcounts
#> rownames: NULL
#> rowData names(0):
#> colnames: NULL
#> colData names(0):
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):
#> 
#> SnglCE> # coercion to a RangedSummarizedExperiment
#> SnglCE> as(sce, "RangedSummarizedExperiment")
#> class: RangedSummarizedExperiment 
#> dim: 200 100 
#> metadata(0):
#> assays(2): counts logcounts
#> rownames: NULL
#> rowData names(0):
#> colnames: NULL
#> colData names(0):
#> 
#> SnglCE> # coercion to a SummarizedExperiment is slightly buggy right now
#> SnglCE> # and requires a little workaround:
#> SnglCE> as(as(sce, "RangedSummarizedExperiment"), "SummarizedExperiment")
#> class: SummarizedExperiment 
#> dim: 200 100 
#> metadata(0):
#> assays(2): counts logcounts
#> rownames: NULL
#> rowData names(0):
#> colnames: NULL
#> colData names(0):
rownames(sce) <- paste0("G", 1:200)
colnames(sce) <- paste0("C", 1:100)

app <- modeEmpty(sce)
if (interactive()) {
  shiny::runApp(app, port=1234)
}
```
