# library(testthat); library(iSEEu); source("test-GeneSetTable.R")

se <- SummarizedExperiment(list(logcounts=matrix(0, 10, 4)),
    rowData=DataFrame(PValue=runif(10), LogFC=rnorm(10), AveExpr=rnorm(10)))
dimnames(se) <- list(1:nrow(se), letters[seq_len(ncol(se))])

test_that("GeneSetTable constructor works as expected", {
    out <- GeneSetTable()
    expect_error(GeneSetTable(Type=character(0)), "single string")
    expect_error(GeneSetTable(Selected=character(0)), "single string")
    expect_error(GeneSetTable(Search=character(0)), "single string")
})

test_that("GeneSetTable interface elements work as expected", {
    out <- GeneSetTable()
    expect_match(.fullName(out), "table")
    expect_is(.fullName(out), "character")
    expect_error(.defineDataInterface(out, se, list()), NA)
    expect_true(.hideInterface(out, "SelectBoxOpen"))
})

test_that("GeneSetTable generates sensible output", {
    out <- GeneSetTable()
    spawn <- .generateOutput(out, se, list(), list())
    expect_is(spawn$commands[[1]], "character")
    expect_identical(spawn$contents$available, nrow(se))

    pObjects <- rObjects <- new.env()
    .renderOutput(out, se, output=list(), pObjects=pObjects, rObjects=rObjects)
})

test_that("GeneSetTable implements multiple selection methods correctly", {
    out <- GeneSetTable()
    expect_identical(.multiSelectionDimension(out), "row")
    expect_identical(.multiSelectionActive(out), NULL)

    out <- GeneSetTable(Selected="BLAH")
    expect_match(.multiSelectionCommands(out, NULL), "BLAH")
    expect_identical(.multiSelectionActive(out), "BLAH")

    expect_identical(.multiSelectionClear(out)[["Selected"]], "")
    expect_identical(.multiSelectionAvailable(out, list(available=10)), 10)
})

test_that("GeneSetTable utilities work as expected", {
    oldi <- .getIdentifierType()
    olds <- .getOrganism()

    .setIdentifierType("BLAH")
    expect_identical(.getIdentifierType(), "BLAH")

    .setOrganism("WHEE")
    expect_identical(.getOrganism(), "WHEE")

    stuff <- list(AaronRandomCollection=
        c(
            show='tab <- some_function_to_list_my_gene_sets()',
            extract='selected <- some_function_to_get_one_gene_set(%s)'
        )
    )
    .setGeneSetCommands(stuff)
    expect_match(.getGeneSetCommands("AaronRandomCollection", "show"), "tab")
    expect_match(.getGeneSetCommands("AaronRandomCollection", "extract"), "selected")

    # This continues to work.
    expect_match(.getGeneSetCommands("KEGG", "show"), "tab")
    expect_match(.getGeneSetCommands("KEGG", "extract"), "selected")

    # Resetting.
    .setIdentifierType(oldi)
    .setOrganism(olds)
})
