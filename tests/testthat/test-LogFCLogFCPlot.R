# Tests the various bits and pieces of the LogFCLogFCPlot.
# library(testthat); library(iSEEu); source("test-LogFCLogFCPlot.R")

se <- SummarizedExperiment(list(counts=matrix(0, 10, 4)),
    rowData=DataFrame(PValue.1=runif(10), PValue.2=runif(10), 
                      LogFC.1=rnorm(10), LogFC.2=rnorm(10)))
dimnames(se) <- list(1:nrow(se), letters[seq_len(ncol(se))])

test_that("LogFCLogFCPlot constructor works", {
    expect_s4_class(LogFCLogFCPlot(), "LogFCLogFCPlot")

    expect_error(LogFCLogFCPlot(XPValueField=character(0)), "must be a single string")
    expect_error(LogFCLogFCPlot(YPValueField=character(0)), "must be a single string")

    expect_error(LogFCLogFCPlot(LogFCThreshold=numeric(0)), "should be a numeric scalar")

    expect_error(LogFCLogFCPlot(PValueThreshold=numeric(0)), "should be a numeric scalar")

    expect_error(LogFCLogFCPlot(PValueCorrection='stuff'), "should be one of")
})

test_that("LogFCLogFCPlot refinement works correctly", {
    x <- LogFCLogFCPlot()
    expect_identical(x[["XAxis"]], "None")
    expect_identical(x[["XPValueField"]], NA_character_)
    expect_identical(x[["YPValueField"]], NA_character_)
    expect_identical(x[["XAxisRowData"]], NA_character_)
    expect_identical(x[["YAxis"]], NA_character_)

    se2 <- .cacheCommonInfo(x, se)
    y <- .refineParameters(x, se2)
    expect_identical(y[["XAxis"]], "Row data")
    expect_identical(y[["XPValueField"]], "PValue.1")
    expect_identical(y[["YPValueField"]], "PValue.1")
    expect_identical(y[["XAxisRowData"]], "LogFC.1")
    expect_identical(y[["YAxis"]], "LogFC.1")

    se3 <- se
    rowData(se3)$PValue.1 <- rowData(se3)$PValue.2 <- NULL
    se3 <- .cacheCommonInfo(x, se3)
    expect_warning(out <- .refineParameters(x, se3), "no valid")
    expect_identical(out, NULL)

    se3 <- se
    rowData(se3)$LogFC.1 <- rowData(se3)$LogFC.2 <- NULL
    se3 <- .cacheCommonInfo(x, se3)
    expect_warning(out <- .refineParameters(x, se3), "no valid")
    expect_identical(out, NULL)
})

test_that("LogFCLogFCPlot interface generation works correctly", {
    x <- LogFCLogFCPlot()
    expect_error(.defineDataInterface(x, se, list()), NA)

    expect_true(.hideInterface(x, "XAxis")) 
    expect_false(.hideInterface(x, "random")) 

    expect_identical(.fullName(x), "LogFC-logFC plot")
    expect_is(.panelColor(x), "character")
})

test_that("LogFCLogFCPlot dot plot generation works correctly", {
    x <- LogFCLogFCPlot(XAxisRowData="LogFC.1", YAxis="LogFC.2")
    se2 <- .cacheCommonInfo(x, se)
    y <- .refineParameters(x, se2)
    envir <- new.env()
    envir$se <- se2

    out <- .generateDotPlotData(y, envir)
    expect_identical(envir$plot.data$Y, rowData(se2)$LogFC.2)
    expect_identical(envir$plot.data$X, rowData(se2)$LogFC.1)
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

test_that("LogFCLogFCPlot generates a tour correctly", {
    expect_s3_class(.definePanelTour(LogFCLogFCPlot()), "data.frame")
})
