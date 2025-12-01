# The AggregatedDotPlot class

Implements an aggregated dot plot where each feature/group combination
is represented by a dot. The color of the dot scales with the mean assay
value across all samples for a given group, while the size of the dot
scales with the proportion of non-zero values across samples in that
group.

## Slot overview

The following slots control the choice of features:

- `CustomRows`, a logical scalar indicating whether custom rows in
  `CustomRowsText` should be used. If `TRUE`, the feature identities are
  extracted from the `CustomRowsText` slot; otherwise they are defined
  from a transmitted row selection. Defaults to `TRUE`.

- `CustomRowsText`, a string containing the names of the features of
  interest, typically corresponding to the row names of the
  SummarizedExperiment. Names should be new-line separated within this
  string. Defaults to the name of the first row in the
  SummarizedExperiment.

The following slots control the specification of groups:

- `ColumnDataLabel`, a string specifying the name of the `colData` field
  to use to group cells. The chosen field should correspond to a
  categorical factor. Defaults to the first categorical field.

- `ColumnDataFacet`, a string specifying the name of the `colData` field
  to use for faceting. The chosen field should correspond to a
  categorical factor. Defaults to `"---"`, i.e., no faceting.

The following slots control the choice of assay values:

- `Assay`, a string specifying the name of the assay containing
  continuous values, to use for calculating the mean and the proportion
  of non-zero values. Defaults to the first valid assay name.

The following slots control the visualization parameters:

- `VisualBoxOpen`, a logical scalar indicating whether the visual
  parameter box should be open on initialization. Defaults to `FALSE`.

- `VisualChoices`, a character vector specifying the visualization
  options to show. Defaults to `"Color"` but can also include
  `"Transform"` and `"Legend"`.

The following slots control the transformation of the mean values:

- `MeanNonZeroes`, a logical scalar indicating whether the mean should
  only be computed over non-zero values. Defaults to `FALSE`.

- `Center`, a logical scalar indicating whether the means for each
  feature should be centered across all groups. Defaults to `FALSE`.

- `Scale`, a logical scalar indicating whether the standard deviation
  for each feature across all groups should be scaled to unity. Defaults
  to `FALSE`.

The following slots control the color:

- `UseCustomColormap`, a logical scalar indicating whether to use a
  custom color scale. Defaults to `FALSE`, in which case the
  application-wide color scale defined by `ExperimentColorMap` is used.

- `CustomColorLow`, a string specifying the low color (i.e., at an
  average of zero) for a custom scale. Defaults to `"grey"`.

- `CustomColorHigh`, a string specifying the high color for a custom
  scale. Defaults to `"red"`.

- `CenteredColormap`, a string specifying the divergent colormap to use
  when `Center` is `TRUE`. Defaults to `"blue < grey < orange"`; other
  choices are `"purple < black < yellow"`, `"blue < grey < red"` and
  `"green < grey < red"`.

In addition, this class inherits all slots from its parent Panel class.

## Constructor

`AggregatedDotPlot(...)` creates an instance of a AggregatedDotPlot
class, where any slot and its value can be passed to `...` as a named
argument.

## Supported methods

In the following code snippets, `x` is an instance of an
AggregatedDotPlot class. Refer to the documentation for each method for
more details on the remaining arguments.

For setting up data values:

- `.cacheCommonInfo(x)` adds a `"AggregatedDotPlot"` entry containing
  `continuous.assay.names` and `discrete.colData.names`.

- `.refineParameters(x, se)` returns `x` after setting `"Assay"`,
  `"ColumnDataLabel"` and `"ColumnDataFacet"` to valid values. If
  continuous assays or discrete `colData` variables are not available,
  `NULL` is returned instead.

For defining the interface:

- `.defineInterface(x, se, select_info)` creates an interface to modify
  the various parameters in the slots, mostly by calling the parent
  method and adding another visualization parameter box.

- `.defineDataInterface(x, se, select_info)` creates an interface to
  modify the data-related parameters, i.e., those that affect the
  position of the points.

- `.defineOutput(x)` defines the output HTML element.

- `.panelColor(x)` will return the specified default color for this
  panel class.

- `.fullName(x)` will return `"Aggregated dot plot"`.

- `.hideInterface(x)` will return `TRUE` for UI elements related to
  multiple row selections.

For monitoring reactive expressions:

- `.createObservers(x, se, input, session, pObjects, rObjects)` will
  create all relevant observers for the UI elements.

For generating output:

- `.generateOutput(x, se, all_memory, all_contents)` will return the
  aggregated dot plot as a
  [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object,
  along with the commands used for its creation.

- `.renderOutput(x, se, output, pObjects, rObjects)` will render the
  aggregated dot plot onto the interface.

- `.exportOutput(x, se, all_memory, all_contents)` will save the
  aggregated dot plot to a PDF file named after `x`, returning the path
  to the new file.

For providing documentation:

- `.definePanelTour(x)` will return a data.frame to be used in rintrojs
  as a panel-specific tour.

## See also

Panel, for the immediate parent class.

ComplexHeatmapPlot, for another panel with multi-row visualization
capability.

## Author

Aaron Lun

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
#> Loading required package: scuttle
#> Loading required package: ggplot2
sce <- logNormCounts(sce, exprs_values="tophat_counts")

# launch the app itself ----
if (interactive()) {
    iSEE(sce, initial=list(
        AggregatedDotPlot(ColumnDataLabel="Primary.Type")
    ))
}
```
