#' The AggregatedDotPlot class
#'
#' Implements an aggregated dot plot where each feature/group combination is represented by a dot.
#' The color of the dot scales with the mean assay value across all samples for a given group,
#' while the size of the dot scales with the number of non-zero values across all samples.
#'
#' @examples
#' library(scRNAseq)
#'
#' # Example data ----
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#' class(sce)
#'
#' library(scater)
#' sce <- logNormCounts(sce, exprs_values="tophat_counts")
#' 
#' # launch the app itself ----
#' if (interactive()) {
#'     iSEE(sce, initial=list(
#'         AggregatedDotPlot(ColumnData=c("Primary.Type"))
#'     ))
#' }
#' 
#' @name AggregatedDotPlot
#' @aliases
#' AggregatedDotPlot-class
#' .cacheCommonInfo,AggregatedDotPlot-method
#' .refineParameters,AggregatedDotplot-method
#' .generateOutput,AggregatedDotPlot-method
#' .renderOutput,AggregatedDotPlot-method
#' .exportOutput,AggregatedDotPlot-method
#' .defineDataInterface,AggregatedDotPlot-method
#' .defineInterface,AggregatedDotPlot-method 
#' .hideInterface,AggregatedDotPlot-method
#' .panelColor,AggregatedDotPlot-method
#' .fullName,AggregatedDotPlot-method
#' .generateOutput,AggregatedDotPlot-method
NULL

.ADPAssay <- "Assay"
.ADPCustomFeatNames <- "CustomRows"
.ADPFeatNameText <- "CustomRowsText"
.ADPClusterFeatures <- "ClusterRows"
.ADPColData <- "ColumnData"

.visualParamChoice <- "VisualChoices"
.visualParamBoxOpen <- "VisualBoxOpen"
.ADPColorUpper <- "Color"

.ADPExpressors <- "MeanNonZeroes"
.ADPCenter <- "Center"
.ADPScale <- "Scale"

collated <- character(0)

collated[.ADPAssay] <- "character"
collated[.ADPCustomFeatNames] <- "logical"
collated[.ADPFeatNameText] <- "character"
collated[.ADPColData] <- "character"

collated[.visualParamChoice] <- "character"
collated[.visualParamBoxOpen] <- "logical"
collated[.ADPColorUpper] <- "character"

collated[.ADPExpressors] <- "logical"
collated[.ADPCenter] <- "logical"
collated[.ADPScale] <- "logical"

#' @export
setClass("AggregatedDotPlot", contains="Panel", slots=collated)

#' @export
AggregatedDotPlot <- function(...) {
    new("AggregatedDotPlot", ...)
}

.visualParamChoiceColorTitle <- "Color"
.visualParamChoiceTransformTitle <- "Transform"
.visualParamChoiceLegendTitle <- "Legend"

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "AggregatedDotPlot", function(.Object, ...) {
    args <- list(...)

    args <- .emptyDefault(args, .ADPAssay, NA_character_)
    args <- .emptyDefault(args, .ADPCustomFeatNames, TRUE)
    args <- .emptyDefault(args, .ADPFeatNameText, NA_character_)

    vals <- args[[.ADPFeatNameText]]
    if (length(vals)!=1L) {
        args[[.ADPFeatNameText]] <- paste(vals, collapse="\n")
    }

    args <- .emptyDefault(args, .ADPColData, character(0))

    args <- .emptyDefault(args, .visualParamChoice, .visualParamChoiceColorTitle)
    args <- .emptyDefault(args, .visualParamBoxOpen, FALSE)

    args <- .emptyDefault(args, .ADPColorUpper, "red")
    args <- .emptyDefault(args, .ADPExpressors, FALSE)
    args <- .emptyDefault(args, .ADPCenter, FALSE)
    args <- .emptyDefault(args, .ADPScale, FALSE)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
setValidity2("AggregatedDotPlot", function(object) {
    msg <- character(0)

    msg <- .singleStringError(msg, object, 
        c(.ADPAssay, .ADPFeatNameText))

    msg <- .validStringError(msg, object, .ADPColorUpper)

    msg <- .multipleChoiceError(msg, object, .visualParamChoice,
        c(
            .visualParamChoiceColorTitle, 
            .visualParamChoiceTransformTitle, 
            .visualParamChoiceLegendTitle
        )
    )

    msg <- .validLogicalError(msg, object, 
        c(
            .ADPCustomFeatNames, 
            .visualParamBoxOpen, 
            .ADPExpressors,
            .ADPCenter, 
            .ADPScale
        )
    )

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom methods callNextMethod
#' @importFrom SummarizedExperiment assayNames colData
setMethod(".cacheCommonInfo", "AggregatedDotPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "AggregatedDotPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    named_assays <- assayNames(se)
    named_assays <- named_assays[named_assays!=""]
    assays_continuous <- vapply(named_assays, .isAssayNumeric, logical(1), se=se)

    df <- colData(se)
    coldata_displayable <- .findAtomicFields(df)
    subdf <- df[,coldata_displayable,drop=FALSE]
    coldata_discrete <- .whichGroupable(subdf)

    .setCachedCommonInfo(se, "AggregatedDotPlot",
        continuous.assay.names=named_assays[assays_continuous],
        discrete.colData.names=coldata_displayable[coldata_discrete])
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "AggregatedDotPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    if (nrow(se)==0L) {
        warning(sprintf("no rows available for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    all_assays <- .getCachedCommonInfo(se, "AggregatedDotPlot")$continuous.assay.names
    if (length(all_assays)==0L) {
        warning(sprintf("no valid 'assays' for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    all_assays <- c(intersect(iSEEOptions$get("assay"), all_assays), all_assays)
    x <- iSEE:::.replace_na_with_first(x, .ADPAssay, all_assays)

    if (is.na(x[[.ADPFeatNameText]])) {
        x[[.ADPFeatNameText]] <- rownames(se)[1]
    }

    all_coldata <- .getCachedCommonInfo(se, "AggregatedDotPlot")$discrete.colData.names
    x[[.ADPColData]] <- intersect(x[[.ADPColData]], all_coldata)

    x
})

#' @export
setMethod(".panelColor", "AggregatedDotPlot", function(x) "#703737FF")

#' @export
setMethod(".fullName", "AggregatedDotPlot", function(x) "Aggregated dot plot")

#' @export
#' @importFrom SummarizedExperiment assay rowData colData
#' @importFrom ggplot2 ggplot geom_text aes theme_void
setMethod(".generateOutput", "AggregatedDotPlot", function(x, se, all_memory, all_contents) {
    # print(str(x))
    plot_env <- new.env()
    plot_env$se <- se
    plot_env$colormap <- metadata(se)$colormap

    all_cmds <- list()
    all_cmds$select <- .processMultiSelections(x, all_memory, all_contents, plot_env)
    all_cmds$assay <- .extractAssaySubmatrix(x, se, plot_env,
        use_custom_row_slot=.ADPCustomFeatNames,
        custom_row_text_slot=.ADPFeatNameText)

    # Computing the various statistics.
    coldata.names <- x[[.ADPColData]]
    if (length(coldata.names)) {
        cmd <- sprintf(".group_by <- SummarizedExperiment::colData(se)[,%s,drop=FALSE];", 
            paste(deparse(coldata.names), collapse=""))
    } else {
        cmd <- ".group_by <- rep('all', ncol(se));"
    }

    computation <- c(cmd,
        ".averages.se <- scuttle::sumCountsAcrossCells(plot.data, .group_by, average=TRUE, store.number=NULL);",
        ".averages <- SummarizedExperiment::assay(.averages.se);",
        ".n.detected.se <- scuttle::numDetectedAcrossCells(plot.data, .group_by, average=TRUE);",
        ".n.detected <- SummarizedExperiment::assay(.n.detected.se);"
    )

    if (x[[.ADPExpressors]] ){
        computation <- c(computation,
            ".averages <- .averages / .n.detected;",
            ".averages[is.na(.averages)] <- 0;"
        )
    }

    if (x[[.ADPCenter]]) {
        computation <- c(computation,
            sprintf(".averages <- t(scale(t(.averages), center=TRUE, scale=%s));", deparse(x[[.ADPScale]]))
        )
    }

    .textEval(computation, plot_env)
    all_cmds$command <- computation

    # Organizing in the plot.data.
    prep.cmds <- c(
       ".levels <- do.call(paste, c(as.list(SummarizedExperiment::colData(.averages.se)), sep=', '))",
        "plot.data <- data.frame(
    Feature=rep(rownames(.averages), ncol(.averages)), 
    Group=rep(.levels, each=nrow(.averages)),
    Average=as.numeric(.averages), 
    Detected=as.numeric(.n.detected)
)")
    .textEval(prep.cmds, plot_env)
    all_cmds$prep <- prep.cmds

    if (!x[[.ADPCenter]]) {
        .low_color <- "grey80"
        .high_color <- x[[.ADPColorUpper]]
        col.cmd <- sprintf(
            'scale_color_gradient(limits = c(0, max(plot.data$Average)), low = %s, high = %s)',
            deparse(.low_color), deparse(.high_color)
        )
    } else {
        col.cmd <- "scale_color_gradient2()"
    }

    plot.cmds <- c(
        'dplot <- ggplot(plot.data)',
        'geom_point(aes_string(x = "Group", y = "Feature", size = "Detected", col = "Average"))',
        'scale_size(limits = c(0, max(plot.data$Detected)))',
        col.cmd,
        'theme(panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(size = 0.5, colour = "grey80"),
    panel.grid.minor = element_line(size = 0.25, colour = "grey80"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.title.y = element_blank())',
        sprintf('xlab(%s)', deparse(paste0(coldata.names, collapse=", ")))
    )

    plot.cmds <- paste0(plot.cmds, collapse=" +\n    ")
    plot_out <- .textEval(plot.cmds, plot_env)
    all_cmds$plot <- plot.cmds 

    list(commands=all_cmds, contents=plot_env$plot.data, plot=plot_out, varname="dplot")
})

#' @export
#' @importFrom shiny renderPlot tagList
setMethod(".renderOutput", "AggregatedDotPlot", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.

    # nocov start
    output[[plot_name]] <- renderPlot({
        p.out <- .retrieveOutput(plot_name, se, pObjects, rObjects)
        print(p.out$plot)
    })
    # nocov end

    callNextMethod()
})

#' @export
#' @importFrom grDevices pdf dev.off
setMethod(".exportOutput", "AggregatedDotPlot", function(x, se, all_memory, all_contents) {
    contents <- .generateOutput(x, se, all_memory=all_memory, all_contents=all_contents)
    newpath <- paste0(.getEncodedName(x), ".pdf")

    # These are reasonably satisfactory heuristics:
    # Width = Pixels -> Inches, Height = Bootstrap -> Inches.
    pdf(newpath, width=x[[iSEE:::.organizationHeight]]/75, height=x[[iSEE:::.organizationWidth]]*2)
    print(contents$plot)
    dev.off()

    newpath
})

.dimnamesModalOpen <- "INTERNAL_modal_open"

#' @export
#' @importFrom shiny selectInput radioButtons checkboxInput actionButton
#' @importFrom methods callNextMethod
setMethod(".defineDataInterface", "AggregatedDotPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    all_assays <- .getCachedCommonInfo(se, "AggregatedDotPlot")$continuous.assay.names
    all_coldata <- .getCachedCommonInfo(se, "AggregatedDotPlot")$discrete.colData.names

    list(
        selectInput(.input_FUN(.ADPAssay), label="Assay choice",
            choices=all_assays, selected=x[[.ADPAssay]]),
        checkboxInput(.input_FUN(.ADPCustomFeatNames), label="Use custom rows",
            value=x[[.ADPCustomFeatNames]]),
        iSEE:::.conditional_on_check_solo(
            .input_FUN(.ADPCustomFeatNames),
            on_select=TRUE,
            actionButton(.input_FUN(.dimnamesModalOpen), label="Edit feature names")),
        selectizeInput(.input_FUN(.ADPColData), label="Column annotations:",
            selected=x[[.ADPColData]], choices=all_coldata, multiple=TRUE,
            options=list(plugins=list('remove_button', 'drag_drop')))
    )
})

#' @export
#' @importFrom colourpicker colourInput
setMethod(".defineInterface", "AggregatedDotPlot", function(x, se, select_info) {
    out <- callNextMethod()
    plot_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }
    pchoice_field <- .input_FUN(.visualParamChoice)
    center_field <- .input_FUN(.ADPCenter)
    
    c(
        out[1],
        list(
            collapseBox(
                id=.input_FUN(.visualParamBoxOpen),
                title="Visual parameters",
                open=x[[.visualParamBoxOpen]],
                checkboxGroupInput(
                    inputId=pchoice_field, 
                    label=NULL, 
                    inline=TRUE,
                    selected=x[[.visualParamChoice]],
                    choices=c(
                         .visualParamChoiceColorTitle, 
                         .visualParamChoiceTransformTitle, 
                         .visualParamChoiceLegendTitle)
                ),
                iSEE:::.conditional_on_check_group(
                    pchoice_field, 
                    .visualParamChoiceColorTitle,
                    hr(),
                    iSEE:::.conditional_on_check_solo(
                        center_field, 
                        on_select=FALSE,
                        colourInput(.input_FUN(.ADPColorUpper),
                            label="Upper color",
                            value=x[[.ADPColorUpper]])
                    ),
                    iSEE:::.conditional_on_check_solo(
                        center_field, 
                        on_select=FALSE
                        # TODO: add some color choices here.
                    )
                ),
                iSEE:::.conditional_on_check_group(
                    pchoice_field, 
                    .visualParamChoiceTransformTitle,
                    hr(),
                    checkboxInput(.input_FUN(.ADPExpressors),
                        label="Compute average expression over non-zero samples",
                        value=x[[.ADPExpressors]]),
                    checkboxInput(center_field,
                        label="Center averages (log-FC from average)",
                        value=x[[.ADPCenter]]),
                    iSEE:::.conditional_on_check_solo(
                        center_field, 
                        on_select=TRUE,
                        checkboxInput(.input_FUN(.ADPScale),
                            label="Scale averages (Z-scores)",
                            value=x[[.ADPScale]])
                    )
                )
            )
        ),
        out[-1] 
    )
})

#' @importFrom shiny plotOutput
#' @export
setMethod(".defineOutput", "AggregatedDotPlot", function(x) {
    plot_name <- .getEncodedName(x)
    plotOutput(plot_name, height=paste0(x[[iSEE:::.organizationHeight]], "px"))
})

#' @export
setMethod(".createObservers", "AggregatedDotPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    # Not much point distinguishing between protected and unprotected here,
    # as there aren't any selections transmitted from this panel anyway.
    .createProtectedParameterObservers(plot_name,
        fields=c(.ADPCustomFeatNames),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(.ADPColData, .ADPAssay, .ADPColorUpper, 
            .ADPExpressors, .ADPCenter, .ADPScale),
        input=input, pObjects=pObjects, rObjects=rObjects, ignoreNULL = FALSE)

    .createMultiSelectionEffectObserver(plot_name,
        by_field=iSEE:::.selectColSource, 
        type_field=iSEE:::.selectColType, 
        saved_field=iSEE:::.selectColSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .createCustomDimnamesModalObservers(plot_name, .ADPFeatNameText, .dimnamesModalOpen, se, 
        input=input, session=session, pObjects=pObjects, rObjects=rObjects, source_type="row")

    invisible(NULL)
})

#' @export
setMethod(".hideInterface", "AggregatedDotPlot", function(x, field) {
    if (field %in% c(iSEE:::.multiSelectHistory)) {
        TRUE
    } else {
        callNextMethod()
    }
})
