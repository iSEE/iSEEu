# Dynamic reduced dimension plot

A dimensionality reduction plot that dynamically recomputes the
coordinates for the samples, based on the selected subset of samples
(and possibly features) in transmitting panels. All samples in active
and saved multiple selections are used here.

## Slot overview

The following slots control the thresholds used in the visualization:

- `Type`, a string specifying the type of dimensionality reduction
  method to use. This can be `"PCA"` (default), `"TSNE"` or `"UMAP"`,
  which uses the relevant functions from the scater package.

- `NGenes`, an integer scalar specifying the number of highly variable
  genes to use in the dimensionality reduction. Only used if an explicit
  selection of features is not made in the app. Defaults to 1000.

- `Assay`, string indicating the assay to use for the calculations.
  Defaults to the first named assay in the SummarizedExperiment.

In addition, this class inherits all slots from its parent
ColumnDotPlot, DotPlot and Panel classes.

## Constructor

`DynamicReducedDimensionPlot(...)` creates an instance of a
DynamicReducedDimensionPlot class, where any slot and its value can be
passed to `...` as a named argument.

## Supported methods

In the following code snippets, `x` is an instance of a
DynamicReducedDimensionPlot class. Refer to the documentation for each
method for more details on the remaining arguments.

For setting up data values:

- `.cacheCommonInfo(x)` adds a `"DynamicReducedDimensionPlot"` entry
  containing `valid.assay.names`. This will also call the equivalent
  ColumnDotPlot method.

- `.refineParameters(x, se)` returns `x` after setting `"Assay"` to the
  first valid value. This will also call the equivalent ColumnDotPlot
  method for further refinements to `x`. If valid assay names are not
  available, `NULL` is returned instead.

For defining the interface:

- `.defineDataInterface(x, se, select_info)` returns a list of interface
  elements for manipulating all slots described above.

- `.panelColor(x)` will return the specified default color for this
  panel class.

- `.fullName(x)` will return `"Dynamic reduced dimension plot"`.

For monitoring reactive expressions:

- `.createObservers(x, se, input, session, pObjects, rObjects)` sets up
  observers for all new slots described above, as well as in the parent
  classes via the ColumnDotPlot method.

For creating the plot:

- `.generateDotPlotData(x, envir)` will create a data.frame of newly
  computed coordinates in `envir`. The method will return the commands
  required to do so as well as a list of labels.

For handling multiple selections:

- `.multiSelectionInvalidated(x)` will always return `TRUE`, as any
  change in the upstream selection of points will alter the coordinates
  and invalidate any brush/lasso on `x`.

For documentation:

- `.definePanelTour(x)` returns an data.frame containing the steps of a
  panel-specific tour.

## Author

Aaron Lun

## Examples

``` r
library(scRNAseq)
library(scater)

sce <- ReprocessedAllenData(assays="tophat_counts")
sce <- logNormCounts(sce, exprs_values="tophat_counts")
sce <- runPCA(sce, ncomponents=4)
sce <- runTSNE(sce)

drdp <- DynamicReducedDimensionPlot(PanelId=1L, Assay="logcounts",
    ColumnSelectionSource="ReducedDimensionPlot1")

if (interactive()) {
    iSEE(sce, initial=list(ReducedDimensionPlot(PanelId=1L), drdp))
}
```
