# library(testthat); library(iSEEu); source("test-AggregatedDotPlot.R")

se <- SummarizedExperiment(list(logcounts=matrix(0, 10, 4)),
    rowData=DataFrame(PValue=runif(10), LogFC=rnorm(10), AveExpr=rnorm(10)))
dimnames(se) <- list(1:nrow(se), letters[seq_len(ncol(se))])

test_that("DynamicMarkerTable constructor works correctly", {
    out <- AggregatedDotPlot()
    expect_error(AggregatedDotPlot(Color=1), "got class \"numeric\"")
})

test_that("initialize processes multiple custom row names", {
    out <- AggregatedDotPlot(CustomRowsText = c("H13", "H1f0", "H1fnt", "H1foo", "H1fx", "H2-Aa"))
    expect_identical(out[["CustomRowsText"]], "H13\nH1f0\nH1fnt\nH1foo\nH1fx\nH2-Aa")
})
