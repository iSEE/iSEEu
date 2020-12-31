#' The MarkdownBoard class
#'
#' The MarkdownBoard class renders user-supplied Markdown into HTML to display inside the app.
#' This is useful for displaying information alongside other panels, or for users to jot down their own notes.
#'
#' @section Slot overview:
#' The following slots are relevant to the rendered content:
#' \itemize{
#' \item \code{Content}, a string containing Markdown-formatted text.
#' This will be rendered to HTML for display inside the app.
#' }
#' 
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @section Constructor:
#' \code{MarkdownBoard(...)} creates an instance of a MarkdownBoard class,
#' where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{RowDataPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for editing the \code{Content}.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' \item \code{\link{.hideInterface}(x, field)} will return \code{TRUE} for all selection-related parameters.
#' \item \code{\link{.fullName}(x)} will return \code{"Volcano plot"}.
#' }
#' 
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all new slots described above,
#' as well as in the parent classes via the \linkS4class{RowDataPlot} method.
#' }
#'
#' For rendering the display:
#' \itemize{
#' \item \code{\link{.defineOutput}(x)} will return a UI element to display the HTML.
#' \item \code{\link{.renderOutput}(x, se, ..., output, pObjects, rObjects)} will add reactive expressions to render the HTML.
#' \item \code{\link{.generateOutput}(x, se, all_memory, all_contents)} will render the Markdown to HTML via the \pkg{rmarkdown} package,
#' returning a string containing the rendered content in the \code{text} element of the output list.
#' The Markdown-formatted content is converted into an R comment for code tracking purposes.
#' \item \code{\link{.exportOutput}(x, se, all_memory, all_contents)} will create a HTML containing the rendered Markdown, and return a string containing the path to that HTML.
#' }
#'
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing the steps of a panel-specific tour.
#' Not that there's a great deal to say here.
#' }
#'
#' @author Aaron Lun
#'
#' @examples
#' if (interactive()) {
#'     iSEE(SummarizedExperiment(), initial=list(MarkdownBoard()))
#' }
#'
#' @docType methods
#' @aliases MarkdownBoard MarkdownBoard-class
#' initialize,MarkdownBoard-method
#' .defineDataInterface,MarkdownBoard-method
#' .createObservers,MarkdownBoard-method
#' .hideInterface,MarkdownBoard-method
#' .fullName,MarkdownBoard-method
#' .panelColor,MarkdownBoard-method
#' .generateOutput,MarkdownBoard-method
#' .renderOutput,MarkdownBoard-method
#' .defineOutput,MarkdownBoard-method
#' .exportOutput,MarkdownBoard-method
#' .definePanelTour,MarkdownBoard-method
#'
#' @seealso
#' \linkS4class{Panel}, for the base class.
#' @name MarkdownBoard-class
NULL

#' @export
setClass("MarkdownBoard", contains='Panel', slots=c(Content="character"))

#' @export
setMethod("initialize", "MarkdownBoard", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, "Content", "# Placeholder\n\nFill me with text!")
    do.call(callNextMethod, c(list(.Object), args))
})

setValidity2("MarkdownBoard", function(object) {
    msg <- character(0)

    msg <- .validStringError(msg, object, "Content")

    if (length(msg)){
        return(msg)
    }
    TRUE
})

#' @export
MarkdownBoard <- function(...) {
    new("MarkdownBoard", ...)
}

#' @export
setMethod(".fullName", "MarkdownBoard", function(x) "Markdown board")

#' @export
setMethod(".panelColor", "MarkdownBoard", function(x) "black")

#' @export
setMethod(".hideInterface", "MarkdownBoard", function(x, field) {
    if (field %in% c("RowSelectionRestrict", "ColumnSelectionRestrict",
            "RowSelectionSource", "ColumnSelectionSource",
            "RowSelectionDynamicSource", "ColumnSelectionDynamicSource",
            "SelectionBoxOpen"))
    {
        return(TRUE)
    }
    callNextMethod()
})

.launch_modal <- "iSEEu_launch_modal"
.save_contents <- "iSEEu_save_contents"

#' @export
#' @importFrom shinyAce aceEditor
setMethod(".defineDataInterface", "MarkdownBoard", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    list(
        aceEditor(paste0(panel_name, "_Content"),
            mode="markdown",
            theme="xcode",
            autoComplete="disabled",
            value=slot(x, "Content"),
            debounce=1000,
            height="500px"
        )
    )
})

#' @export
#' @importFrom shiny modalDialog actionButton showModal observeEvent removeModal
setMethod(".createObservers", "MarkdownBoard", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    panel_name <- .getEncodedName(x)
    .createUnprotectedParameterObservers(panel_name, "Content", input, pObjects, rObjects)
})

#' @export
#' @importFrom shiny uiOutput hr
setMethod(".defineOutput", "MarkdownBoard", function(x) {
    tagList(
        uiOutput(.getEncodedName(x)),
        hr()
    )
})

#' @export
#' @importFrom shiny renderUI HTML
setMethod(".renderOutput", "MarkdownBoard", function(x, se, ..., output, pObjects, rObjects) {
    panel_name <- .getEncodedName(x)
    output[[panel_name]] <- renderUI({
        out <- .retrieveOutput(panel_name, se, pObjects, rObjects)
        HTML(out$text)
    })
})

#' @export
setMethod(".generateOutput", "MarkdownBoard", function(x, se, all_memory, all_contents) {
    current <- slot(x, "Content")

    tmpout <- tempfile(fileext=".html")
    on.exit(unlink(tmpout))
    out <- .run_pandoc(current, tmpout)

    if (is(out, "try-error")) {
        # nocov start
        showNotification(as.character(out), type="error")
        out <- "Error: failed to render to HTML"
        # nocov end
    } else {
        out <- paste(readLines(tmpout), collapse="\n")
    }

    by.line <- strsplit(current, "\n")[[1]]
    commented <- paste(paste("#>", by.line), collapse="\n")

    list(
        contents=NULL,
        varname=NULL,
        commands=commented,
        text=out
    )
})

.run_pandoc <- function(md, out, ...) {
    tmpin <- tempfile(fileext=".md")
    write(md, file=tmpin)
    on.exit(unlink(tmpin))
    try(rmarkdown::pandoc_convert(tmpin, output=out, ...), silent=TRUE)
}

#' @export
setMethod(".exportOutput", "MarkdownBoard", function(x, se, all_memory, all_contents) {
    tmpout <- paste0(.getEncodedName(x), ".html")
    out <- .run_pandoc(slot(x, "Content"), file.path(getwd(), tmpout), options="-s")

    if (is(out, "try-error")) {
        # nocov start
        showNotification(as.character(out), type="error")
        write(c("Error: failed to render to HTML", as.character(out)), file=tmpout)
        # nocov end
    }

    tmpout
})

#' @export
setMethod(".definePanelTour", "MarkdownBoard", function(x) {
    out <- rbind(
        c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">MarkdownBoard</font> panel will render arbitrary Markdown to HTML to be displayed on-screen. This allows us to put down extra information about the surrounding plots, and for users to jot down notes that can be saved in the code tracker.", .getPanelColor(x))),
        c(paste0("#", .getEncodedName(x), "_DataBoxOpen"), "The <i>Data parameters</i> box shows the available parameters that can be tweaked in this panel.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        c(paste0("#", .getEncodedName(x), "_Content"), "The most important (and only) parameter is the Markdown text itself. Users can fill this in with whatever Markdown they like; the app will render this 1 second later.")
    )
    data.frame(element=out[,1], intro=out[,2])
})
