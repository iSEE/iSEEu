context("modeReducedDim")

test_that("modeReducedDim works", {
  set.seed(1)
  ncells <- 100
  ngenes <- 200
  u <- matrix(rpois(ngenes * ncells, 5), ncol = ncells,
              dimnames = list(paste0("g",seq_len(ngenes)),
                              paste0("c", seq_len(ncells))))
  v <- log2(u + 1)

  pca <- matrix(runif(ncells*5), ncells)
  tsne <- matrix(rnorm(ncells*2), ncells)

  sce <- SingleCellExperiment::SingleCellExperiment(
      assays = list(counts = u, logcounts = v),
      colData = S4Vectors::DataFrame(type = letters[(seq_len(ncells) %% 5) + 1],
                                     row.names = colnames(u)),
      reducedDims = S4Vectors::SimpleList(PCA = pca, tSNE = tsne)
      )

  # valid arguments
  expect_error(modeReducedDim("wrong type"))
  expect_error(modeReducedDim(sce, "unknown"))
  expect_error(modeReducedDim(sce, colorBy = 1L))
  expect_error(modeReducedDim(sce, colorBy = "unknown"))
  expect_error(modeReducedDim(sce, plot_width = "wrong type"))
  expect_error(modeReducedDim(sce, plot_width = 1:10))

  # returns valid shiny app
  app1 <- modeReducedDim(sce)
  app2 <- modeReducedDim(sce, includeNames = reducedDimNames(sce))
  app3 <- modeReducedDim(sce, colorBy = "type")
  app4 <- modeReducedDim(sce, colorBy = "g17")
  expect_is(app1, "shiny.appobj")
  expect_equal(app1, app2)

  # remark: use shinyTest instead
  envapp3 <- environment(get("server", envir = environment(app3$serverFuncSource)))
  envapp4 <- environment(get("server", envir = environment(app4$serverFuncSource)))
  expect_identical(nrow(get("initialPanels", envir = envapp3)), 2L)
  expect_identical(nrow(get("initialPanels", envir = envapp4)), 3L)
})
