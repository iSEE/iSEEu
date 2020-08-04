# library(iSEEu); library(testthat); source("setup-sce.R"); source("test-ReducedDimensionHexPlot.R")

context("ReducedDimensionHexPlot")

test_that("ReducedDimensionHexPlot can be created", {
    out <- ReducedDimensionHexPlot()
    expect_is(out, "ReducedDimensionHexPlot")
    expect_identical(out[["Downsample"]], FALSE)
})

test_that(".fullName works", {
  x <- ReducedDimensionHexPlot()

  out <- .fullName(x)
  expect_identical(out, "Hexagonal reduced dimension plot")
})

test_that(".panelColor works", {
  x <- ReducedDimensionHexPlot()

  out <- .panelColor(x)
  expect_identical(out, "#991717")
})

test_that(".cacheCommonInfo works", {
  x <- ReducedDimensionHexPlot()

  out <- .cacheCommonInfo(x, sce)
  expect_is(out, "SingleCellExperiment")
  info <- .getCachedCommonInfo(out, "ReducedDimensionHexPlot")
  expect_true(all(info$valid.colData.names %in% colnames(colData(sce))))
  expect_true(all(info$discrete.colData.names %in% colnames(colData(sce))))
  expect_true(all(info$continuous.colData.names %in% colnames(colData(sce))))

  out2 <- .cacheCommonInfo(x, out)
  expect_identical(out2, out)
})

test_that(".hideInterface works", {
  x <- ReducedDimensionHexPlot()

  expect_identical(.hideInterface(x, "Downsample"), TRUE)
  expect_identical(.hideInterface(x, "BinResolution"), FALSE)
})

test_that(".defineVisualShapeInterface works", {
  x <- ReducedDimensionHexPlot()

  out <- .defineVisualShapeInterface(x)
  expect_null(out)
})

test_that(".defineVisualSizeInterface works", {
  x <- ReducedDimensionHexPlot()

  out <- .defineVisualSizeInterface(x)
  expect_is(out, "shiny.tag.list")
})

test_that(".defineVisualOtherInterface works", {
  x <- ReducedDimensionHexPlot()

  out <- .defineVisualOtherInterface(x)
  expect_null(out)
})

test_that(".createObservers works", {
  x <- ReducedDimensionHexPlot(PanelId=1L)

  input <- new.env()
  session <- new.env()
  pObjects <- new.env()
  rObjects <- new.env()

  out <- .createObservers(x, sce, input, session, pObjects, rObjects)
  expect_null(out)
  expect_named(rObjects, c("ReducedDimensionHexPlot1_INTERNAL_dimnames",
    "ReducedDimensionHexPlot1_INTERNAL_saved_choices",
    "ReducedDimensionHexPlot1",
    "ReducedDimensionHexPlot1_INTERNAL_single_select",
    "ReducedDimensionHexPlot1_INTERNAL_relinked_select",
    "ReducedDimensionHexPlot1_INTERNAL_multi_select"))
})

test_that(".generateOutput works", {
    x <- ReducedDimensionHexPlot(PanelId=1L)
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    all_memory <- list()
    all_contents <- list()

    out <- .generateOutput(x, sce, all_memory = all_memory, all_contents = all_contents)
    expect_named(out, c("commands", "contents", "plot", "varname"))
    expect_type(unlist(out$commands), "character")
    expect_is(out$contents, "data.frame")
    expect_is(out$plot, "ggplot")
})

test_that("faceting works", {
    x <- ReducedDimensionHexPlot(PanelId=1L, FacetByRow = "driver_1_s", FacetByColumn = "driver_1_s")
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    all_memory <- list()
    all_contents <- list()

    out <- .generateOutput(x, sce, all_memory = all_memory, all_contents = all_contents)
    expect_named(out, c("commands", "contents", "plot", "varname"))
    expect_type(unlist(out$commands), "character")
    expect_is(out$contents, "data.frame")
    expect_is(out$plot, "ggplot")

    expect_match(out$commands$facets[["FacetRow"]], "FacetRow")
    expect_match(out$commands$facets[["FacetColumn"]], "FacetColumn")
})

test_that("coloring by feature name works", {
    x <- ReducedDimensionHexPlot(PanelId=1L, ColorBy = "Feature name", ColorByFeatureName = "Cux2")
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    all_memory <- list()
    all_contents <- list()

    out <- .generateOutput(x, sce, all_memory = all_memory, all_contents = all_contents)
    expect_named(out, c("commands", "contents", "plot", "varname"))
    expect_type(unlist(out$commands), "character")
    expect_is(out$contents, "data.frame")
    expect_is(out$plot, "ggplot")
})

test_that("coloring by sample name works", {
    x <- ReducedDimensionHexPlot(PanelId=1L, ColorBy = "Sample name", ColorBySampleName = "SRR2139778")
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    all_memory <- list()
    all_contents <- list()

    out <- .generateOutput(x, sce, all_memory = all_memory, all_contents = all_contents)
    expect_named(out, c("commands", "contents", "plot", "varname"))
    expect_type(unlist(out$commands), "character")
    expect_is(out$contents, "data.frame")
    expect_is(out$plot, "ggplot")
})

test_that("coloring by discrete covariate works", {
    x <- ReducedDimensionHexPlot(PanelId=1L, ColorBy = "Column data", ColorByColumnData = "driver_1_s")
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    all_memory <- list()
    all_contents <- list()

    out <- .generateOutput(x, sce, all_memory = all_memory, all_contents = all_contents)
    expect_named(out, c("commands", "contents", "plot", "varname"))
    expect_type(unlist(out$commands), "character")
    expect_is(out$contents, "data.frame")
    expect_is(out$plot, "ggplot")
})

test_that("zoom works", {
    x <- ReducedDimensionHexPlot(PanelId=1L, ZoomData = c(xmin = -12, xmax = 10, ymin = -12, ymax = 0))
    sce <- .cacheCommonInfo(x, sce)
    x <- .refineParameters(x, sce)

    all_memory <- list()
    all_contents <- list()

    out <- .generateOutput(x, sce, all_memory = all_memory, all_contents = all_contents)
    expect_named(out, c("commands", "contents", "plot", "varname"))
    expect_type(unlist(out$commands), "character")
    expect_is(out$contents, "data.frame")
    expect_is(out$plot, "ggplot")
})

test_that("ReducedDimensionHexPlot generates a tour correctly", {
    tour <- .definePanelTour(ReducedDimensionHexPlot())

    expect_s3_class(tour, "data.frame")

    expect_true(any(grepl("check the .*Size.* box", tour$intro)))

    expect_true(any(grepl("average value across all points", tour$intro)))

    expect_true(any(grepl("bin resolution", tour$intro)))
})
