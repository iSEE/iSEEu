# Register DE-related fields

Register the names of fields containing various DE statistics, to
populate the user interface of DE-related Panels.

## Usage

``` r
registerPValueFields(se, fields)

registerAveAbFields(se, fields)

registerLogFCFields(se, fields)

registerPValuePatterns(se, patterns)

registerAveAbPatterns(se, patterns)

registerLogFCPatterns(se, patterns)

getPValueFields(se)

getAveAbFields(se)

getLogFCFields(se)

getPValuePatterns(se, defaults = c("PValue", "p.value", "pval"))

getAveAbPatterns(se, defaults = c("AveExpr", "logCPM"))

getLogFCPatterns(se, defaults = c("logFC", "LogFC"))
```

## Arguments

- se:

  A SummarizedExperiment to be visualized with various DE-related
  Panels. This is expected to have a number of DE-related fields in its
  `rowData`.

- fields:

  A character vector containing the names of the relevant fields
  containing the DE statistics. Alternatively `NULL` to remove any
  existing setting.

- patterns:

  A character vector containing partial names, to match against the
  `colnames` of the `rowData` to identify relevant fields containing DE
  statistics. Alternatively `NULL` to remove any existing setting.

- defaults:

  Character vector specifying the default patterns to provide when no
  patterns were registered in `se`.

## Value

All `register` functions will return `se`, modified to contain the
supplied `patterns` or `fields`. These will be used as suggestions by
DE-related Panels to identify the relevant fields.

All `get` functions will return a character vector containing the value
set by the corresponding `register` function; or `NULL`, if nothing was
set.

## Details

DE-related Panels need to find relevant `rowData` fields containing
p-values, log-fold changes, etc. to set appropriate defaults in the user
interface. These functions allow a user to tune the definition of what
those Panels consider to be relevant, which is occasionally necessary if
the DE statistics are stored in a `rowData` field with an unusual column
name. The idea is to `register` the relevant fields in `se`, which can
then be supplied to `iSEE` with the affected Panels - see Examples.

The registered `fields` should be the names of appropriate columns in
`rowData` containing continuous variables. Columns containing
categorical or non-atomic variables will generally be ignored. For each
DE statistic, if any `fields` are registered in `se`, they will be used
directly and `patterns` will be ignored.

The registered `patterns` are used for partial name matching to the
names of appropriate columns of `rowData`. All partial matching must be
exact - regular expressions are not supported. Matches can occur
anywhere in the name. For example, with `"PValue"`, columns with the
names `"PValue.X"` and `"X.PValue"` will be considered acceptable
matches. If no `patterns` are supplied, the Panels will use the values
in `defaults`.

## Author

Aaron Lun

## Examples

``` r
# Making up some results with unusual names.
se <- SummarizedExperiment(matrix(rnorm(10000), 1000, 10))
rownames(se) <- paste0("GENE_", seq_len(nrow(se)))
rowData(se)$pvalue <- runif(nrow(se))
rowData(se)$lfc <- rnorm(nrow(se))
rowData(se)$average <- rnorm(nrow(se))

se <- registerPValueFields(se, "pvalue")
getPValueFields(se)
#> [1] "pvalue"
se <- registerAveAbFields(se, "average")
getAveAbFields(se)
#> [1] "average"
se <- registerLogFCFields(se, "lfc")
getLogFCFields(se)
#> [1] "lfc"

if (interactive()) {
    iSEE(se, initial=list(MAPlot()))
}
```
