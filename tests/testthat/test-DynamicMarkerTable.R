# library(testthat); library(iSEEu); source("test-DynamicMarkerTable.R")

se <- SummarizedExperiment(list(logcounts=matrix(0, 10, 4)),
    rowData=DataFrame(PValue=runif(10), LogFC=rnorm(10), AveExpr=rnorm(10)))
dimnames(se) <- list(1:nrow(se), letters[seq_len(ncol(se))])

test_that("DynamicMarkerTable constructor works correctly", {
    out <- DynamicMarkerTable()
    expect_error(DynamicMarkerTable(LogFC=-1), "non-negative")
    expect_error(DynamicMarkerTable(TestMethod="a"), "must be")
})

test_that("DynamicMarkerTable interface definition works correctly", {
    x <- DynamicMarkerTable()
    expect_error(.defineDataInterface(x, se, list()), NA)

    expect_false(.hideInterface(x, "ColumnSelectionSource"))
    expect_true(.hideInterface(x, "RowSelectionSource"))

    expect_identical(.fullName(x), "Dynamic marker table")
    expect_is(.panelColor(x), "character")
})

test_that("DynamicMarkerTable caching and refinement works correctly", {
    x <- DynamicMarkerTable()
    expect_identical(x[["Assay"]], "logcounts")

    expect_warning(x2 <- .refineParameters(x, se), "no valid")
    expect_identical(x2, NULL)

    se <- .cacheCommonInfo(x, se)
    expect_identical(.getCachedCommonInfo(se, "DynamicMarkerTable")$valid.assay.names, "logcounts")

    x <- .refineParameters(x, se)
    expect_identical(x[["Assay"]], "logcounts")
})

test_that("DynamicMarkerTable generates the table correctly", {
    env <- new.env()
    env$col_selected <- list()
    env$se <- se

    x <- DynamicMarkerTable()
    se <- .cacheCommonInfo(x, se)
    x <- .refineParameters(x, se)

    out <- .generateTable(x, env)
    expect_identical(nrow(env$tab), 0L)        

    env$col_selected <- list(saved1=letters[1:2])
    out <- .generateTable(x, env)
    expect_identical(nrow(env$tab), 0L)        

    env$col_selected <- list(active=letters[1:2])
    out <- .generateTable(x, env)
    expect_identical(nrow(env$tab), nrow(se))
    past <- ncol(env$tab)

    env$col_selected <- list(active=letters[1:2], saved1=letters[3:4], saved2=letters[2:3])
    out <- .generateTable(x, env)
    expect_identical(nrow(env$tab), nrow(se))
    expect_identical(ncol(env$tab), past+1L)
})

test_that("DynamicMarkerTable responds correctly to extra fields", {
    env <- new.env()
    env$col_selected <- list()
    rowData(se)$Symbol <- sprintf("GENE_%i", seq_len(nrow(se)))
    env$se <- se

    # Trying to set it globally won't work.
    x <- DynamicMarkerTable(ExtraFields="Symbol")
    expect_identical(x[["ExtraFields"]], NA_character_)

    x[["ExtraFields"]] <- "Symbol2"
    se2 <- .cacheCommonInfo(x, se)
    x <- .refineParameters(x, se2)
    expect_identical(x[["ExtraFields"]], character(0))

    out <- .generateTable(x, env)
    expect_false("Symbol" %in% colnames(env$tab))
    env$col_selected <- list(active=letters[1:2])
    out <- .generateTable(x, env)
    expect_false("Symbol" %in% colnames(env$tab))

    # Trying again, this time with the correct symbols.
    x[["ExtraFields"]] <- "Symbol"
    se2 <- .cacheCommonInfo(x, se)
    x <- .refineParameters(x, se2)
    expect_identical(x[["ExtraFields"]], "Symbol")

    env$col_selected <- list()
    out <- .generateTable(x, env)
    expect_true("Symbol" %in% colnames(env$tab))
    env$col_selected <- list(active=letters[1:2])
    out <- .generateTable(x, env)
    expect_true("Symbol" %in% colnames(env$tab))
})

test_that("DynamicMarkerTable responds correctly to global extra fields", {
    old <- getTableExtraFields()
    setTableExtraFields(LETTERS)

    rowData(se)$A <- 1
    x <- DynamicMarkerTable()
    se2 <- .cacheCommonInfo(x, se)
    x <- .refineParameters(x, se2)
    expect_identical(x[["ExtraFields"]], "A")

    setTableExtraFields(old)
})

test_that("DynamicMarkerTable generates a tour correctly", {
    expect_s3_class(.definePanelTour(DynamicMarkerTable()), "data.frame")
})
