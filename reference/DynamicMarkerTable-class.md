# Dynamic marker table

A table that dynamically identifies marker genes for a selected subset
of samples. Comparisons are made between the active selection in the
transmitting panel and (i) all non-selected points, if no saved
selections are available; or (ii) each subset of points in each saved
selection.

## Slot overview

The following slots control the test procedure:

- `LogFC`, a numeric scalar indicating the log-fold change threshold to
  test against. Defaults to zero.

- `TestMethod`, string indicating the test to use (based on the
  `findMarkers` function from scran). This can be `"t"` (default),
  `"wilcox"` or `"binom"`.

- `Assay`, string indicating the assay to use for testing. Defaults to
  the first named assay in the SummarizedExperiment.

The following slots control the rendered table:

- `ExtraFields`, a character vector containing names of `rowData`
  columns to be included in the table. Set to the output of
  [`getTableExtraFields`](https://isee.github.io/iSEEu/reference/global-TableExtraFields.md).
  This cannot be changed once the application starts.

In addition, this class inherits all slots from its parent RowTable,
Table and Panel classes.

## Constructor

`DynamicMarkerTable(...)` creates an instance of a DynamicMarkerTable
class, where any slot and its value can be passed to `...` as a named
argument.

## Supported methods

In the following code snippets, `x` is an instance of a
DynamicMarkerTable class. Refer to the documentation for each method for
more details on the remaining arguments.

For setting up data values:

- `.cacheCommonInfo(x)` adds a `"DynamicMarkerTable"` entry containing
  `valid.assay.names` and `valid.rowdata.names`. This will also call the
  equivalent RowTable method.

- `.refineParameters(x, se)` returns `x` after setting `"Assay"` to the
  first valid value. This will also call the equivalent RowTable method
  for further refinements to `x`. If valid assay names are not
  available, `NULL` is returned instead. Any `"ExtraFields"` are
  intersected with the valid `rowData` names.

For defining the interface:

- `.defineDataInterface(x, se, select_info)` returns a list of interface
  elements for manipulating all slots described above.

- `.panelColor(x)` will return the specified default color for this
  panel class.

- `.fullName(x)` will return `"Dynamic marker table"`.

- `.hideInterface(x)` will return `TRUE` for UI elements related to
  multiple row selections, otherwise calling the method for RowTable.

For monitoring reactive expressions:

- `.createObservers(x, se, input, session, pObjects, rObjects)` sets up
  observers for all new slots described above, as well as in the parent
  classes via the RowTable method.

For creating the table:

- `.generateTable(x, envir)` will create a data.frame of newly computed
  statistics in `envir`. The method will return the commands required to
  do so.

For documentation:

- `.definePanelTour(x)` returns an data.frame containing the steps of a
  panel-specific tour.

## Examples

``` r
library(scRNAseq)
library(scater)

sce <- ReprocessedAllenData(assays="tophat_counts")
sce <- logNormCounts(sce, exprs_values="tophat_counts")
sce <- runPCA(sce, ncomponents=4)
sce <- runTSNE(sce)

dst <- DynamicMarkerTable(PanelId=1L, PanelWidth=8L,
    ColumnSelectionSource="ReducedDimensionPlot1")

rdp <- ReducedDimensionPlot(PanelId=1L,
    ColorByFeatureSource="DynamicMarkerTable1")

if (interactive()) {
    iSEE(sce, initial=list(rdp, dst))
}
```
