# library(testthat); library(iSEEu); source("test-DynamicReducedDimensionPlot.R")

se <- SummarizedExperiment(list(logcounts=matrix(0, 10, 10)),
    rowData=DataFrame(PValue=runif(10), LogFC=rnorm(10), AveExpr=rnorm(10)))
dimnames(se) <- list(1:nrow(se), letters[seq_len(ncol(se))])

test_that("DynamicReducedDimensionPlot constructor works correctly", {
    out <- DynamicReducedDimensionPlot()
    expect_error(DynamicReducedDimensionPlot(NGenes=-1L), "positive")
    expect_error(DynamicReducedDimensionPlot(Type="a"), "must be")
})

test_that("DynamicReducedDimensionPlot interface definition works correctly", {
    x <- DynamicReducedDimensionPlot()
    expect_error(.defineDataInterface(x, se, list()), NA)

    expect_false(.hideInterface(x, "ColumnSelectionSource"))
    expect_true(.hideInterface(x, "RowSelectionSource"))

    expect_identical(.fullName(x), "Dynamic reduced dimension plot")
    expect_is(.panelColor(x), "character")
})

test_that("DynamicReducedDimensionPlot caching and refinement works correctly", {
    x <- DynamicReducedDimensionPlot()
    expect_identical(x[["Assay"]], NA_character_)

    se2 <- se
    assayNames(se2) <- "" 
    se2 <- .cacheCommonInfo(x, se2)
    expect_warning(x2 <- .refineParameters(x, se2), "no valid")
    expect_identical(x2, NULL)

    se2 <- se
    se2 <- .cacheCommonInfo(x, se2)
    expect_identical(.getCachedCommonInfo(se2, "DynamicReducedDimensionPlot")$valid.assay.names, "logcounts")

    x <- .refineParameters(x, se2)
    expect_identical(x[["Assay"]], "logcounts")
})

test_that("DynamicReducedDimensionPlot generates the plotting data correctly", {
    env <- new.env()
    env$col_selected <- list()
    env$se <- se

    x <- DynamicReducedDimensionPlot()
    se <- .cacheCommonInfo(x, se)
    x <- .refineParameters(x, se)

    out <- .generateDotPlotData(x, env)
    expect_identical(nrow(env$plot.data), ncol(se))
    expect_true(all(is.na(env$plot.data$X)))

    env$col_selected <- list(active=letters[1:2], saved1=letters[3:4], saved2=letters[2:3])
    out <- .generateDotPlotData(x, env)
    expect_identical(nrow(env$plot.data), ncol(se))
    expect_false(all(is.na(env$plot.data$X)))
})

test_that("DynamicReducedDimensionPlot generates a tour correctly", {
    expect_s3_class(.definePanelTour(DynamicReducedDimensionPlot()), "data.frame")
})
