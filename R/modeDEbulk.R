#' Start iSEE in a configuration that is ready to explore DE analysis results
#'
#' @param se An object that coercible to 
#' [SingleCellExperiment-class][SingleCellExperiment::SingleCellExperiment()]
#' @param ... Additional arguments passed to [iSEE()].
#' @param plot_width The grid width of linked plots (numeric vector of
#' length either 1 or equal to `nrow(features)`
#'
#' @return A Shiny app object is returned.
#'
#' @export
#'
#' @examples
#' # TODO: specify all the prelim steps!!!
#' se_airway <- readRDS("se_airway_de.rds")
#' sce_airway <- as(se_airway, "SingleCellExperiment")
#' sce_airway <- scater::runPCA(sce_airway, exprs_values = "vst_counts")
#' # I could safely filter out rows for non-expressed features
#' sce_airway <- sce_airway[rowSums(counts(sce_airway)) > 0,]
#' 
#' # use symbols as row names? 
#' 
#' app <- modeDEbulk(sce_airway)
#' if (interactive()) {
#'   shiny::runApp(app, port=1234)
#' }
modeDEbulk <- function(se, plot_width = 4, ...) {
  
  # TODOs:
  
  # check that some columns are in there?
  
  # handle some nesting-nasty cases?
  
  # use symbols as row names? 
  
  # plot_width to be handled automatically?
  
  
  # Reduced dimension panel settings -------------------------------------------
  reddim <- redDimPlotDefaults(se, 5)
  reddim$PointSize <- 4
  reddim$ColorBy <- "Column data"
  # should be actually checked if this exists?
  reddim$ColorByColData <- "condition"
  
  # Feature assay plot settings ------------------------------------------------
  featassay <- featAssayPlotDefaults(se, 5)
  featassay[["Assay"]] <- c(2L, 2L, 2L, 2L, 2L) # normalized counts
  featassay[["XAxis"]] <- "Column data"
  featassay[["XAxisColData"]] <- "SampleName"
  featassay[["PointSize"]] <- 4
  featassay[['YAxisRowTable']] <- c("Row statistics table 1", "Row statistics table 2", "---", "---", "---")
  featassay[['ColorBy']] <- c("Column data", "None", "None", "None", "None")
  featassay[['ColorByColData']] <- c("dex", "SampleName", "SampleName", "SampleName", "SampleName")
  
  # Row statistics table settings ----------------------------------------------
  rowstat <- rowStatTableDefaults(se, 5)
  rowstat[['SelectByPlot']] <- c("---", "Row data plot 2", "---", "---", "---")
  rowstat[['Selected']] <- c(3L, 2L, 1L, 1L, 1L)
  
  # Row data plot settings -----------------------------------------------------
  # ma plot and also volcano close by
  rowdata <- rowDataPlotDefaults(se, 5)
  rowdata[['YAxis']] <- c("DESeq2_dex_trt_vs_untrt:log2FoldChange", "DESeq2_dex_trt_vs_untrt:log10_pvalue", 
                          "baseMean", "baseMean", "baseMean")
  rowdata[['XAxis']] <- c("Row data", "Row data", "None", "None", "None")
  rowdata[['XAxisRowData']] <- c("DESeq2_dex_trt_vs_untrt:log10_baseMean", "DESeq2_dex_trt_vs_untrt:log2FoldChange", 
                                 "baseVar", "baseVar", "baseVar")
  
  # Initial panel settings -----------------------------------------------------
  initialpanels_de <- DataFrame(
    Name = c("Reduced dimension plot 1", 
           "Feature assay plot 1", 
           "Row statistics table 1", 
           "Row data plot 1", 
           "Row data plot 2", 
           "Row statistics table 2",
           "Feature assay plot 2"),
    Width = c(4, 4, 4, 3, 3, 3, 3)
  ) 

  app <- iSEE(
    se,
    redDimArgs = reddim,
    rowDataArgs = rowdata,
    rowStatArgs = rowstat,
    featAssayArgs = featassay,
    initialPanels = initialpanels_de,
    ...
  )
  return(app)
} 

