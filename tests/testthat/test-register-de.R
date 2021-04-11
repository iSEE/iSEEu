# This tests the various DE field registration functions.
# library(testthat); library(iSEEu); source("test-register-de.R")

se <- SummarizedExperiment(matrix(rnorm(10000), 1000, 10))

test_that("all getters and setters work as expected", {
    se <- registerPValueFields(se, "pvalue")
    expect_identical(getPValueFields(se), "pvalue")

    se <- registerAveAbFields(se, "average")
    expect_identical(getAveAbFields(se), "average")
    expect_identical(getPValueFields(se), "pvalue") # doesn't remove existing entries.

    se <- registerLogFCFields(se, "lfc")
    expect_identical(getLogFCFields(se), "lfc")

    # Same for patterns.
    se <- registerPValuePatterns(se, "PVALUE")
    expect_identical(getPValuePatterns(se), "PVALUE")

    se <- registerAveAbPatterns(se, "AVERAGE")
    expect_identical(getAveAbPatterns(se), "AVERAGE")
    expect_identical(getPValuePatterns(se), "PVALUE") # doesn't remove existing entries.
    expect_identical(getPValueFields(se), "pvalue") # doesn't intefere with the fields.

    se <- registerLogFCPatterns(se, "LFC")
    expect_identical(getLogFCPatterns(se), "LFC")

    # Setting to NULL works.
    se <- registerLogFCFields(se, NULL)
    expect_null(getLogFCFields(se))
})

test_that("default getters work as expected", {
    expect_true("PValue" %in% getPValuePatterns(se))
    expect_true("LogFC" %in% getLogFCPatterns(se))
    expect_true("AveExpr" %in% getAveAbPatterns(se))
})

test_that("matcher works as expected", {
    copy <- se

    copy <- registerPValueFields(copy, "pvalue")
    expect_identical(iSEEu:::.matchPValueFields(copy, "pvalue"), "pvalue")

    copy <- registerAveAbFields(copy, "average")
    expect_identical(iSEEu:::.matchAveAbFields(copy, "average"), "average")

    copy <- registerLogFCFields(copy, "lfc")
    expect_identical(iSEEu:::.matchLogFCFields(copy, "lfc"), "lfc")

    # Works the same for patterns.
    copy <- se
    
    copy <- registerPValuePatterns(copy, "pv")
    expect_identical(iSEEu:::.matchPValueFields(copy, "pvalue"), "pvalue")

    copy <- registerAveAbPatterns(copy, c("av", "dummy"))
    expect_identical(iSEEu:::.matchAveAbFields(copy, "average"), "average")

    copy <- registerLogFCPatterns(copy, c("dummy", "lf"))
    expect_identical(iSEEu:::.matchLogFCFields(copy, c("lfc1", "lfc2")), c("lfc1", "lfc2"))
})
