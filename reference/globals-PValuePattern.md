# Global DE prefixes

Get or set patterns for acceptable names of `rowData` columns related to
a differential expression analysis. These functions are deprecated; use
their counterparts in
`?"`[`registerPValuePatterns`](https://isee.github.io/iSEEu/reference/registerDEFields.md)`"`
instead.

## Usage

``` r
getPValuePattern()

getLogFCPattern()

getAveAbPattern()

setPValuePattern(value)

setLogFCPattern(value)

setAveAbPattern(value)
```

## Arguments

- value:

  A character vector containing the acceptable prefixes for each
  statistic.

## Value

`getPValuePattern` returns the patterns for acceptable column names for
p-values.

`getLogFCPattern` returns the patterns for acceptable column names for
log-fold changes.

`getAveAbPattern` returns the patterns for acceptable column names for
the average abundances.

The corresponding setters set the global parts for each statistic and
return `NULL` invisibly.

## Details

These utilities allow users to easily get and set the patterns of
acceptable fields in all
[VolcanoPlot](https://isee.github.io/iSEEu/reference/VolcanoPlot-class.md)s,
[MAPlot](https://isee.github.io/iSEEu/reference/MAPlot-class.md)s and
[LogFCLogFCPlot](https://isee.github.io/iSEEu/reference/LogFCLogFCPlot-class.md)s
at once. Any global settings only take effect (i) during setup of the
`iSEE` application and (ii) if the first panel of each class does not
have existing values in the `"PValueFields"`, `"LogFCFields"` or
`"AveAbFields"` slots (which take precedence if present).

Each of these global settings are treated as *patterns* for partial
matching. For the `"PValue"` pattern, columns with the names
`"PValue.X"` and `"X.PValue"` will be considered acceptable matches. All
partial matching must be exact - regular expressions are not supported.

## See also

[VolcanoPlot](https://isee.github.io/iSEEu/reference/VolcanoPlot-class.md),
[MAPlot](https://isee.github.io/iSEEu/reference/MAPlot-class.md) and
[LogFCLogFCPlot](https://isee.github.io/iSEEu/reference/LogFCLogFCPlot-class.md),
which are affected by these globals.

## Author

Aaron Lun

## Examples

``` r
old <- getPValuePattern()

setPValuePattern(LETTERS)
#> Warning: 'setPValuePattern' is deprecated.
#> Use 'registerPValuePatterns' instead.
#> See help("Deprecated")
getPValuePattern()
#>  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
#> [20] "T" "U" "V" "W" "X" "Y" "Z"

setPValuePattern(old)
#> Warning: 'setPValuePattern' is deprecated.
#> Use 'registerPValuePatterns' instead.
#> See help("Deprecated")
```
