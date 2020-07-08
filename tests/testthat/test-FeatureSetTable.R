# library(testthat); library(iSEEu); source("test-FeatureSetTable.R")

se <- SummarizedExperiment(list(logcounts=matrix(0, 10, 4)),
    rowData=DataFrame(PValue=runif(10), LogFC=rnorm(10), AveExpr=rnorm(10)))
dimnames(se) <- list(1:nrow(se), letters[seq_len(ncol(se))])

test_that("FeatureSetTable constructor works as expected", {
    out <- FeatureSetTable()
    expect_error(FeatureSetTable(Type=character(0)), "single string")
    expect_error(FeatureSetTable(Selected=character(0)), "single string")
    expect_error(FeatureSetTable(Search=character(0)), "single string")
})

test_that("FeatureSetTable interface elements work as expected", {
    out <- FeatureSetTable()
    expect_match(.fullName(out), "table")
    expect_is(.fullName(out), "character")
    expect_error(.defineDataInterface(out, se, list()), NA)
    expect_true(.hideInterface(out, "SelectBoxOpen"))
})

test_that("FeatureSetTable generates sensible output", {
    out <- FeatureSetTable()
    spawn <- .generateOutput(out, se, list(), list())
    expect_is(spawn$commands[[1]], "character")
    expect_identical(spawn$contents$available, nrow(se))

    pObjects <- rObjects <- new.env()
    .renderOutput(out, se, output=list(), pObjects=pObjects, rObjects=rObjects)
})

test_that("FeatureSetTable implements multiple selection methods correctly", {
    out <- FeatureSetTable()
    expect_identical(.multiSelectionDimension(out), "row")
    expect_identical(.multiSelectionActive(out), NULL)

    out <- FeatureSetTable(Selected="BLAH")
    expect_match(.multiSelectionCommands(out, NULL), "BLAH")
    expect_identical(.multiSelectionActive(out), "BLAH")

    expect_identical(.multiSelectionClear(out)[["Selected"]], "")
    expect_identical(.multiSelectionAvailable(out, list(available=10)), 10)
})

test_that("createGeneSetCommands works as expected", {
    cmds <- createGeneSetCommands()
    
    # GO creation works.
    env <- new.env()
    eval(parse(text=cmds$CreateCollections[1]), envir=env)
    expect_true(nrow(env$tab) > 0)

    # GO retrieval works.
    env$se <- se
    env$.set_id <- rownames(env$tab)[1]
    eval(parse(text=cmds$RetrieveSet[1]), envir=env)
    expect_type(env$selected, "character")

    # KEGG creation works.
    env <- new.env()
    eval(parse(text=cmds$CreateCollections[2]), envir=env)
    expect_true(nrow(env$tab) > 0)

    env$se <- se
    env$.set_id <- rownames(env$tab)[1]
    eval(parse(text=cmds$RetrieveSet[2]), envir=env)
    expect_type(env$selected, "character")
})
