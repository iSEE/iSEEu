# library(testthat)

context("iSEEhex")

test_that("iSEEhex is attached", {
    expect_true(iSEEu:::is_attached("iSEEhex"))
})

test_that("ReducedDimensionHexPlot can be created", {
    out <- ReducedDimensionHexPlot()
    expect_s4_class(out, "ReducedDimensionHexPlot")
})
