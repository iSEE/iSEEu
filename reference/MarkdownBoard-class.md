# The MarkdownBoard class

The MarkdownBoard class renders user-supplied Markdown into HTML to
display inside the app. This is useful for displaying information
alongside other panels, or for users to jot down their own notes.

## Slot overview

The following slots are relevant to the rendered content:

- `Content`, a string containing Markdown-formatted text. This will be
  rendered to HTML for display inside the app.

In addition, this class inherits all slots from its parent Panel class.

## Constructor

`MarkdownBoard(...)` creates an instance of a MarkdownBoard class, where
any slot and its value can be passed to `...` as a named argument.

## Supported methods

In the following code snippets, `x` is an instance of a RowDataPlot
class. Refer to the documentation for each method for more details on
the remaining arguments.

For defining the interface:

- `.defineDataInterface(x, se, select_info)` returns a list of interface
  elements for editing the `Content`.

- `.panelColor(x)` will return the specified default color for this
  panel class.

- `.hideInterface(x, field)` will return `TRUE` for all
  selection-related parameters.

- `.fullName(x)` will return `"Volcano plot"`.

For monitoring reactive expressions:

- `.createObservers(x, se, input, session, pObjects, rObjects)` sets up
  observers for all new slots described above, as well as in the parent
  classes via the RowDataPlot method.

For rendering the display:

- `.defineOutput(x)` will return a UI element to display the HTML.

- `.renderOutput(x, se, ..., output, pObjects, rObjects)` will add
  reactive expressions to render the HTML.

- `.generateOutput(x, se, all_memory, all_contents)` will render the
  Markdown to HTML via the rmarkdown package, returning a string
  containing the rendered content in the `text` element of the output
  list. The Markdown-formatted content is converted into an R comment
  for code tracking purposes.

- `.exportOutput(x, se, all_memory, all_contents)` will create a HTML
  containing the rendered Markdown, and return a string containing the
  path to that HTML.

For documentation:

- `.definePanelTour(x)` returns an data.frame containing the steps of a
  panel-specific tour. Not that there's a great deal to say here.

## See also

Panel, for the base class.

## Author

Aaron Lun

## Examples

``` r
if (interactive()) {
    iSEE(SummarizedExperiment(), initial=list(MarkdownBoard()))
}
```
