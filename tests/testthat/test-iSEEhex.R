# library(testthat)

context("iSEEhex")

test_that("iSEEhex is attached", {
    expect_true("package:iSEEhex" %in% search())
})

test_that("ReducedDimensionHexPlot can be created", {
    out <- ReducedDimensionHexPlot()
    expect_s4_class(out, "ReducedDimensionHexPlot")
})
