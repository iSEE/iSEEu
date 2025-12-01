# Gene set table

A table where each row is a gene set and can be clicked to transmit a
multiple feature selection to another panel. This has been deprecated in
favor of the simpler
[FeatureSetTable](https://isee.github.io/iSEEu/reference/FeatureSetTable-class.md).

## Slot overview

The following slots control the type of gene sets to show:

- `Type`, string specifying the type of gene set collection to show.
  Defaults to `"GO"`.

The following slots control the table selections:

- `Selected`, a string containing the name of the currently selected
  gene set. Defaults to `""`, i.e., no selection.

- `Search`, a string containing the regular expression for the global
  search. Defaults to `""`, i.e., no search.

- `SearchColumns`, a character vector where each entry contains the
  search string for each column. Defaults to an empty character vector,
  i.e., no search.

In addition, this class inherits all slots from its parent Panel class.

## Constructor

`GeneSetTable(...)` creates an instance of a GeneSetTable class, where
any slot and its value can be passed to `...` as a named argument.

## Supported methods

In the following code snippets, `x` is an instance of a GeneSetTable
class. Refer to the documentation for each method for more details on
the remaining arguments.

For defining the interface:

- `.defineDataInterface(x, se, select_info)` returns a list of interface
  elements for manipulating all slots described above.

- `.panelColor(x)` will return the specified default color for this
  panel class.

- `.fullName(x)` will return `"Gene set table"`.

- `.hideInterface(x)` will return `TRUE` for UI elements related to
  multiple selections, otherwise calling the method for Panel.

- `.defineOutput(x)` will return a HTML element containing a `datatable`
  widget.

For monitoring reactive expressions:

- `.createObservers(x, se, input, session, pObjects, rObjects)` sets up
  observers for all new slots described above, as well as in the parent
  classes via the Panel method.

For creating the table:

- `.generateOutput(x, envir)` will create a data.frame of gene set
  descriptions in `envir`, based on the `mode="show"` output of
  [`.getGeneSetCommands`](https://isee.github.io/iSEEu/reference/utils-geneset.md).
  It will also return the commands required to do so and the name of the
  variable corresponding to said data.frame.

- `.renderOutput(x, se, ..., output, pObjects, rObjects)` will add a
  `datatable` widget to the output, which is used to render the
  aforementioned data.frame.

For controlling the multiple selections:

- `.multiSelectionDimension(x)` returns `"row"`.

- `.multiSelectionCommands(x, index)` returns a string specifying the
  commands to be used to extract the identities of the genes in the
  currently selected set, based on the `mode="extract"` output of
  [`.getGeneSetCommands`](https://isee.github.io/iSEEu/reference/utils-geneset.md).
  `index` is ignored.

- `.multiSelectionActive(x)` returns the name of the currently selected
  gene set, unless no selection is made, in which case `NULL` is
  returned.

- `.multiSelectionClear(x)` returns `x` but with the `Selected` slot
  replaced by an empty string.

- `.multiSelectionAvailable(x, contents)` returns `contents$available`,
  which is set to the number of features in `se`.

## Author

Aaron Lun

## Examples

``` r
library(scRNAseq)
sce <- LunSpikeInData(location=FALSE)

library(scater)
sce <- logNormCounts(sce)

library(scran)
rowData(sce) <- cbind(rowData(sce), modelGeneVarWithSpikes(sce, "ERCC"))

# This defaults to 'org.Hs.eg.db' with 'ENTREZID'.
.setOrganism("org.Mm.eg.db")
#> Warning: '.setOrganism' is deprecated.
#> Use 'setFeatureSetCommands' instead.
#> See help("Deprecated")
.setIdentifierType("ENSEMBL")
#> Warning: '.setIdentifierType' is deprecated.
#> Use 'setFeatureSetCommands' instead.
#> See help("Deprecated")
gst <- GeneSetTable(PanelId=1L)
#> Warning: 'GeneSetTable' is deprecated.
#> Use 'FeatureSetTable' instead.
#> See help("Deprecated")

rdp <- RowDataPlot(RowSelectionSource="GeneSetTable1",
    ColorBy="Row selection",
    XAxis="Row data", XAxisRowData="mean", YAxis="total")

rdt <- RowDataTable(RowSelectionSource="GeneSetTable1")

if (interactive()) {
    iSEE(sce, initial=list(gst, rdp, rdt))
}
```
