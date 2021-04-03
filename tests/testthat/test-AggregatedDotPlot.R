# library(testthat); library(iSEEu); source("setup-mimic_live_app.R"); source("test-AggregatedDotPlot.R")

context("AggregatedDotPlot")

se <- SummarizedExperiment(
    assays = list(logcounts=matrix(0, 10, 4)),
    colData=DataFrame(
        CellGroup = factor(rep(c(1, 2), each = 2))
    ))
dimnames(se) <- list(1:nrow(se), letters[seq_len(ncol(se))])

test_that("DynamicMarkerTable constructor works correctly", {
    out <- AggregatedDotPlot()
    expect_error(AggregatedDotPlot(CustomColorHigh=1), "got class \"numeric\"")
})

test_that("initialize processes multiple custom row names", {
    out <- AggregatedDotPlot(CustomRowsText = c("H13", "H1f0", "H1fnt", "H1foo", "H1fx", "H2-Aa"))
    expect_identical(out[["CustomRowsText"]], "H13\nH1f0\nH1fnt\nH1foo\nH1fx\nH2-Aa")
})

test_that(".cacheCommonInfo works", {
    out <- AggregatedDotPlot()
    se2 <- .cacheCommonInfo(out, se)
    se2 <- .cacheCommonInfo(out, se2) # return se
  
    panel_cache <- metadata(se2)[["iSEE"]][["cached"]][["AggregatedDotPlot"]]
    expect_identical(panel_cache$continuous.assay.names, "logcounts")
    expect_identical(panel_cache$discrete.colData.names, "CellGroup")
})

test_that(".refineParameters works", {

    FUN <- selectMethod(".refineParameters", signature="AggregatedDotPlot")
    expect_null(FUN(NULL, se))

    out <- AggregatedDotPlot()
    se2 <- .cacheCommonInfo(out, se)

    expect_warning(.refineParameters(out, se[c(), ]), "no rows available")
    expect_null(.refineParameters(out, se[c(), ]))

    se_discrete <- se
    storage.mode(assay(se_discrete)) <- "character"
    se_discrete <- .cacheCommonInfo(out, se_discrete)
    expect_warning(.refineParameters(out, se_discrete), "no valid 'assays'")
    expect_null(.refineParameters(out, se_discrete))

    se_colData <- se
    se_colData$CellGroup <- seq_len(ncol(se_colData))
    se_colData <- .cacheCommonInfo(out, se_colData)
    expect_warning(.refineParameters(out, se_colData), "no discrete 'colData'")
    expect_null(.refineParameters(out, se_colData))

    out2 <- .refineParameters(out, se2)
    expect_identical(out2[["Assay"]], "logcounts")
    expect_identical(out2[["ColumnDataLabel"]], "CellGroup")
    expect_identical(out2[["CustomRowsText"]], "1")

})

test_that(".definePanelTour works", {

    x <- AggregatedDotPlot()
    tour <- .definePanelTour(x)

    expect_s3_class(tour, "data.frame")
    expect_identical(colnames(tour), c("element", "intro"))

})

test_that(".defineInterface works correctly", {

    x <- AggregatedDotPlot()
    se <- .cacheCommonInfo(x, se)
    x <- .refineParameters(x, se)

    expect_error(.defineDataInterface(x, se, list()), NA)

    out <- .defineInterface(x, se, list())
    expect_type(out, "list")
    expect_length(out, 3)

    expect_true(.hideInterface(x, "SelectionHistory"))
    expect_false(.hideInterface(x, "random"))

})

test_that("miscellaneous bits work correctly", {

    x <- AggregatedDotPlot()
    expect_identical(length(.panelColor(x)), 1L)
    expect_identical(length(.fullName(x)), 1L)

})

test_that(".generateOutput works correctly", {

    x <- AggregatedDotPlot()
    se <- .cacheCommonInfo(x, se)
    x <- .refineParameters(x, se)
    se <- iSEE:::.set_colormap(se, ExperimentColorMap())

    collated <- .generateOutput(x, se, all_memory=list(), all_contents=list())
    expect_s3_class(collated$plot, "ggplot")

    # Testing what happens with faceting.
    y <- x
    y[["ColumnDataFacet"]] <- "CellGroup"
    collated <- .generateOutput(y, se, all_memory=list(), all_contents=list())
    expect_s3_class(collated$plot, "ggplot")

    # Testing alternative color schemes.
    y <- x
    y[["Center"]] <- TRUE
    collated <- .generateOutput(y, se, all_memory=list(), all_contents=list())
    expect_s3_class(collated$plot, "ggplot")

    y <- x
    y[["UseCustomColormap"]] <- TRUE
    collated <- .generateOutput(y, se, all_memory=list(), all_contents=list())
    expect_s3_class(collated$plot, "ggplot")

    y <- x
    y[["MeanNonZeroes"]] <- TRUE
    collated <- .generateOutput(y, se, all_memory=list(), all_contents=list())
    expect_s3_class(collated$plot, "ggplot")
    expect_true(any(unlist(collated$commands) == ".averages[is.na(.averages)] <- 0;"))

})

test_that("renderOutput returns NULL", {

  out <- AggregatedDotPlot()
  expect_null(.renderOutput(out, se, output = list(), pObjects = list(), rObjects = list()))

})

test_that(".exportOutput handles AggregatedDotPlot", {

    AggregatedDotPlot1 <- AggregatedDotPlot(PanelId=1L)
    se2 <- .cacheCommonInfo(AggregatedDotPlot1, se)
    AggregatedDotPlot1 <- .refineParameters(AggregatedDotPlot1, se2)
    memory <- list(AggregatedDotPlot1=AggregatedDotPlot1)

    pObjects <- mimic_live_app(se, memory) # read from iSEE... iSEE.utils package?
    se2 <- iSEE:::.set_colormap(se2, ExperimentColorMap())

    out <- .exportOutput(memory$AggregatedDotPlot1, se2, memory, pObjects$contents)
    expect_identical(out, "AggregatedDotPlot1.pdf")

})

test_that(".defineOutput returns a tag list", {

    x <- AggregatedDotPlot()
    out <- .defineOutput(x)
    expect_type(out, "list")
    expect_length(out, 3)

})

test_that(".createObservers returns NULL", {

    x <- AggregatedDotPlot()
    expect_null(.createObservers(x, se, list(), NULL, list(), list()))

})
