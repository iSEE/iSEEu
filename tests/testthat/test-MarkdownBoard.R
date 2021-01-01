# This tests the MarkdownBoard.
# library(testthat); library(iSEEu); source("test-MarkdownBoard.R")

test_that("MarkdownBoard constructor works correctly", {
    X <- MarkdownBoard()
    expect_match(X[["Content"]], "Placeholder")

    X <- MarkdownBoard(Content="WHEE")
    expect_match(X[["Content"]], "WHEE")

    expect_error(MarkdownBoard(Content=LETTERS), "string")
})

test_that("MarkdownBoard basic methods work correctly", {
    X <- MarkdownBoard()
    expect_match(.fullName(X), "Markdown board")
    expect_true(isSingleString(.panelColor(X)))
})

test_that("MarkdownBoard interface is generated correctly", {
    X <- MarkdownBoard(PanelId=1L)
    expect_error(ui <- .defineInterface(X, SummarizedExperiment(), list()), NA)
    expect_true(any(grepl("MarkdownBoard1_Content", as.character(ui))))
    expect_false(.hideInterface(X, "SelectionParamOpen"))

    out.ui <- .defineOutput(X)
    expect_true(any(grepl("MarkdownBoard1", as.character(out.ui))))
})

test_that("MarkdownBoard output generation works correctly", {
    X <- MarkdownBoard(PanelId=1L)
    expect_warning(out <- .generateOutput(X), NA)
    expect_match(out$text, "Placeholder")

    old <- setwd(tempdir())
    expect_warning(out <- .exportOutput(X), NA)
    expect_true(any(grepl("Placeholder", readLines(out))))
    setwd(old)
})

test_that("MarkdownBoard reactives work correctly", {
    X <- MarkdownBoard(PanelId=2L)
    expect_null(.createObservers(X, SummarizedExperiment(), list(), NULL, list(), list()))
    expect_error(.renderOutput(X, SummarizedExperiment(), output=list(), pObjects=list(), rObjects=list()), NA)
})

test_that("MarkdownBoard tour generation works correctly", {
    X <- MarkdownBoard(PanelId=1L)
    tour <- .definePanelTour(X)
    expect_identical(colnames(tour), c("element", "intro"))
})

