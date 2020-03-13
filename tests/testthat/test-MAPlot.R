# Tests the various bits and pieces of the MAPlot.
# library(testthat); library(iSEEu); source("test-MAPlot.R")

se <- SummarizedExperiment(list(counts=matrix(0, 10, 4)),
    rowData=DataFrame(PValue=runif(10), LogFC=rnorm(10), AveExpr=rnorm(10)))
dimnames(se) <- list(1:nrow(se), letters[seq_len(ncol(se))])

test_that("MAPlot constructor works", {
    expect_s4_class(MAPlot(), "MAPlot")

    expect_error(MAPlot(PValueField=character(0)), "must be a single string")

    expect_error(MAPlot(LogFCThreshold=numeric(0)), "must be a non-negative numeric scalar")

    expect_error(MAPlot(PValueThreshold=numeric(0)), "must be a numeric scalar")

    expect_error(MAPlot(PValueCorrection='stuff'), "must be in")
})

test_that("MAPlot common caching works correctly", {
    x <- MAPlot()
    se2 <- .cacheCommonInfo(x, se)

    cached <- .getCachedCommonInfo(se2, "MAPlot")
    expect_identical(cached$pval.rowData.names, "PValue")
    expect_identical(cached$ave.rowData.names, "AveExpr")
    expect_identical(cached$lfc.rowData.names, "LogFC")

    expect_true(!is.null(.getCachedCommonInfo(se2, "RowDotPlot")))

    # Should be a no-op.
    expect_identical(se2, .cacheCommonInfo(x, se2))
})

test_that("MAPlot refinement works correctly", {
    x <- MAPlot()
    expect_identical(x[["XAxis"]], "None")
    expect_identical(x[["PValueField"]], NA_character_)
    expect_identical(y[["XAxisRowData"]], NA_character_)
    expect_identical(y[["YAxis"]], NA_character_)

    se2 <- .cacheCommonInfo(x, se)
    y <- .refineParameters(x, se2)
    expect_identical(y[["XAxis"]], "Row data")
    expect_identical(y[["PValueField"]], "PValue")
    expect_identical(y[["XAxisRowData"]], "AveExpr")
    expect_identical(y[["YAxis"]], "LogFC")

    se3 <- se
    rowData(se3)$PValue <- NULL
    se3 <- .cacheCommonInfo(x, se3)
    expect_warning(out <- .refineParameters(x, se3), "no valid")
    expect_identical(out, NULL)

    # Works for the other two fields.
    se3 <- se
    rowData(se3)$LogFC <- NULL
    se3 <- .cacheCommonInfo(x, se3)
    expect_warning(out <- .refineParameters(x, se3), "no valid")
    expect_identical(out, NULL)

    se3 <- se
    rowData(se3)$AveExpr <- NULL
    se3 <- .cacheCommonInfo(x, se3)
    expect_warning(out <- .refineParameters(x, se3), "no valid")
    expect_identical(out, NULL)
})

test_that("MAPlot interface generation works correctly", {
    x <- MAPlot()
    expect_error(.defineDataInterface(x, se, list()), NA)

    expect_true(.hideInterface(x, "XAxis")) 
    expect_false(.hideInterface(x, "random")) 

    expect_identical(.fullName(x), "MA plot")
    expect_is(.panelColor(x), "character")
})

test_that("MAPlot dot plot generation works correctly", {
    x <- MAPlot()
    se2 <- .cacheCommonInfo(x, se)
    y <- .refineParameters(x, se2)
    envir <- new.env()
    envir$se <- se2

    out <- .generateDotPlotData(y, envir)
    expect_identical(envir$plot.data$Y, rowData(se2)$LogFC)
    expect_identical(envir$plot.data$X, rowData(se2)$AveExpr)
    expect_identical(names(out), c("commands", "labels"))

    priority <- .prioritizeDotPlotData(y, envir)
    expect_identical(names(priority), c("commands", "rescaled"))

    expect_identical(.colorByNoneDotPlotField(y), "IsSig")
    expect_match(.colorByNoneDotPlotScale(y), "scale_color_manual")

    library(ggplot2)
    envir$plot.type <- "scatter"
    .generateDotPlot(y, out$labels, envir)
    expect_s3_class(envir$dot.plot, "ggplot")
})


