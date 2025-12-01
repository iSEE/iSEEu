# Global extra table fields

Get or set the names of the extra fields to include in a table.

## Usage

``` r
getTableExtraFields()

setTableExtraFields(value)
```

## Arguments

- value:

  A character vector containing the names of extra fields to include.

## Value

`getTableExtraFields` returns the current global extra table fields.

`setTableExtraFields` will set the current global extra table fields and
return `NULL` invisibly.

## Details

These utilities allow users to easily set the feature set commands for
all
[DynamicMarkerTable](https://isee.github.io/iSEEu/reference/DynamicMarkerTable-class.md)s
at once. Any global settings only take effect (i) during setup of the
`iSEE` application and (ii) if the first
[DynamicMarkerTable](https://isee.github.io/iSEEu/reference/DynamicMarkerTable-class.md)
does not have an existing values in the `"TableExtraFields"` slots.

## Author

Aaron Lun

## Examples

``` r
old <- getTableExtraFields()

setTableExtraFields(LETTERS)
getTableExtraFields()
#>  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
#> [20] "T" "U" "V" "W" "X" "Y" "Z"

setTableExtraFields(old)
```
