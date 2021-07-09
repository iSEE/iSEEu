# library(testthat); library(iSEEu); source("test-FeatureSetTable.R")

se <- SummarizedExperiment(list(logcounts=matrix(0, 100, 4)),
    rowData=DataFrame(PValue=runif(100), LogFC=rnorm(100), AveExpr=rnorm(100)))
dimnames(se) <- list(1:nrow(se), letters[seq_len(ncol(se))])

#############################################
# Testing the defaults.

test_that("createGeneSetCommands works as expected", {
    cmds <- createGeneSetCommands()
    
    # GO creation works.
    env <- new.env()
    eval(parse(text=cmds$collections[1]), envir=env)
    expect_true(nrow(env$tab) > 0)

    # GO retrieval works.
    env$se <- se
    env$.set_id <- rownames(env$tab)[1]
    eval(parse(text=cmds$sets[1]), envir=env)
    expect_type(env$selected, "character")

    # KEGG creation works.
    env <- new.env()
    eval(parse(text=cmds$collections[2]), envir=env)
    expect_true(nrow(env$tab) > 0)

    env$se <- se
    env$.set_id <- rownames(env$tab)[1]
    eval(parse(text=cmds$sets[2]), envir=env)
    expect_type(env$selected, "character")
})

#############################################

random <- CharacterList(
   Aaron = sample(rownames(se), 10),
   Kevin = sample(rownames(se), 20),
   Charlotte = sample(rownames(se), 30),
   Fed = sample(rownames(se), 40)
)
mcols(random)$p.value <- runif(4)

test_that("registerCollections works correctly for collections", {
    se <- registerFeatureSetCollections(se, list(random=random))
    expect_identical(getFeatureSetCollections(se)$random, random)

    se <- registerFeatureSetCollections(se, list(random=random, other=rev(random)))
    expect_identical(getFeatureSetCollections(se)$other, rev(random))

    expect_error(registerFeatureSetCollections(se, list(random)), "unique")
    expect_error(registerFeatureSetCollections(se, list(A=random, A=random)), "unique")
    expect_error(registerFeatureSetCollections(se, list(A=random, random)), "unique")
    expect_error(registerFeatureSetCollections(se, list(random=unname(random))), "named")
})

test_that("registerCollections works correctly for commands", {
    cmds <- createGeneSetCommands()
    se <- registerFeatureSetCommands(se, commands=cmds)

    expect_identical(getFeatureSetCommands(se)$collections, cmds$collections)
    expect_identical(getFeatureSetCommands(se)$sets, cmds$sets)

    expect_error(registerFeatureSetCommands(se, commands=list(collections=unname(cmds$collections), sets=character(0)), "unique"))
    expect_error(registerFeatureSetCommands(se, commands=list(collections=cmds$collections, sets=character(0)), "same"))
})

#############################################

test_that("FeatureSetTable constructor works as expected", {
    out <- FeatureSetTable()
    expect_error(FeatureSetTable(Collection=character(0)), "single string")
    expect_error(FeatureSetTable(Selected=character(0)), "single string")
    expect_error(FeatureSetTable(Search=character(0)), "single string")
})

test_that("FeatureSetTable interface elements work as expected", {
    out <- FeatureSetTable()
    expect_match(.fullName(out), "table")
    expect_is(.fullName(out), "character")
    expect_error(.defineDataInterface(out, se, list()), NA)
    expect_true(.hideInterface(out, "SelectionBoxOpen"))
})

test_that("FeatureSetTable responds to registration of collections", {
    se2 <- registerFeatureSetCollections(se, list(random=random))

    out <- FeatureSetTable()
    se2 <- .cacheCommonInfo(out, se2)
    out <- .refineParameters(out, se2)

    expect_identical(out[["Collection"]], "random")
    expect_identical(.getCachedCommonInfo(se2, "FeatureSetTable")$available.sets, list(random=as.data.frame(mcols(random))))
    expect_match(.getCachedCommonInfo(se2, "FeatureSetTable")$create.collections.cmds[[1]], "getFeatureSetCollections")
})

test_that("FeatureSetTable generates sensible output", {
    out <- FeatureSetTable()
    se2 <- registerFeatureSetCollections(se, list(random=random))
    se2 <- .cacheCommonInfo(out, se2)
    out <- .refineParameters(out, se2)

    spawn <- .generateOutput(out, se2, list(), list())
    expect_is(spawn$commands[[1]], "character")
    expect_identical(spawn$contents$available, nrow(se))

    pObjects <- rObjects <- new.env()
    .renderOutput(out, se, output=list(), pObjects=pObjects, rObjects=rObjects)
})

test_that("FeatureSetTable implements multiple selection methods correctly", {
    out <- FeatureSetTable()
    se2 <- registerFeatureSetCollections(se, list(random=random))
    se2 <- .cacheCommonInfo(out, se2)
    out <- .refineParameters(out, se2)

    expect_identical(.multiSelectionDimension(out), "row")
    expect_identical(.multiSelectionActive(out), NULL)

    out <- FeatureSetTable(Collection="random", Selected="Aaron")
    expect_true(any(grepl("Aaron", .multiSelectionCommands(out, NULL))))
    expect_true(any(grepl("random", .multiSelectionCommands(out, NULL))))
    expect_identical(.multiSelectionActive(out), "Aaron")

    expect_identical(.multiSelectionClear(out)[["Selected"]], "")
    expect_identical(.multiSelectionAvailable(out, list(available=10)), 10)
})

test_that("FeatureSetTable responds to registration of commands", {
    cmds <- createGeneSetCommands(identifier="SYMBOL")
    se <- registerFeatureSetCommands(se, commands=cmds)
     
    out <- FeatureSetTable()
    se2 <- .cacheCommonInfo(out, se)
    out <- .refineParameters(out, se2)

    expect_identical(out[["Collection"]], "GO")
    expect_identical(names(.getCachedCommonInfo(se2, "FeatureSetTable")$available.sets), names(cmds$collections))
    expect_match(.getCachedCommonInfo(se2, "FeatureSetTable")$create.collections.cmds[[1]], "GO")

    out <- FeatureSetTable(Collection="GO", Selected="GO:00001")
    select.cmds <- .multiSelectionCommands(out, NULL)
    expect_match(select.cmds[2], "org.Hs.eg.db")
    expect_match(select.cmds[2], "SYMBOL")
})

test_that("FeatureSetCommands constructor falls back to the defaults", {
    out <- FeatureSetTable()
    se2 <- .cacheCommonInfo(out, se)
    out <- .refineParameters(out, se2)
    expect_identical(out[["Collection"]], "GO")

    # Overriding the globals (deprecated!).
    old <- getFeatureSetCommands()
    setFeatureSetCommands(list(RetrieveSet=c(A="1"), CreateCollections=c(A="1")))

    out <- FeatureSetTable()
    se2 <- .cacheCommonInfo(out, se)
    out <- .refineParameters(out, se2)
    expect_identical(names(.getCachedCommonInfo(se2, "FeatureSetTable")$available.sets), "A")

    out <- FeatureSetTable(Collection="A", Selected="XXX")
    select.cmds <- .multiSelectionCommands(out, NULL)
    expect_identical(unname(select.cmds[2]), "1")

    setFeatureSetCommands(old)
})

test_that("FeatureSetTable generates a tour correctly", {
    expect_s3_class(.definePanelTour(FeatureSetTable()), "data.frame")
})


