% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DynamicReducedDimensionPlot.R
\name{DynamicReducedDimensionPlot-class}
\alias{DynamicReducedDimensionPlot-class}
\alias{DynamicReducedDimensionPlot}
\alias{initialize,DynamicReducedDimensionPlot-method}
\alias{.fullName,DynamicReducedDimensionPlot-method}
\alias{.panelColor,DynamicReducedDimensionPlot-method}
\alias{.defineDataInterface,DynamicReducedDimensionPlot-method}
\alias{.generateDotPlotData,DynamicReducedDimensionPlot-method}
\alias{.createObservers,DynamicReducedDimensionPlot-method}
\alias{.cacheCommonInfo,DynamicReducedDimensionPlot-method}
\alias{.refineParameters,DynamicReducedDimensionPlot-method}
\alias{.multiSelectionInvalidated,DynamicReducedDimensionPlot-method}
\alias{.definePanelTour,DynamicReducedDimensionPlot-method}
\title{Dynamic reduced dimension plot}
\description{
A dimensionality reduction plot that dynamically recomputes the coordinates for the samples,
based on the selected subset of samples (and possibly features) in transmitting panels.
All samples in active and saved multiple selections are used here.
}
\section{Slot overview}{

The following slots control the thresholds used in the visualization:
\itemize{
\item \code{Type}, a string specifying the type of dimensionality reduction method to use.
This can be \code{"PCA"} (default), \code{"TSNE"} or \code{"UMAP"},
which uses the relevant functions from the \pkg{scater} package.
\item \code{NGenes}, an integer scalar specifying the number of highly variable genes to use in the dimensionality reduction.
Only used if an explicit selection of features is not made in the app.
Defaults to 1000.
\item \code{Assay}, string indicating the assay to use for the calculations.
Defaults to the first named assay in the SummarizedExperiment.
}

In addition, this class inherits all slots from its parent \linkS4class{ColumnDotPlot},
\linkS4class{DotPlot} and \linkS4class{Panel} classes.
}

\section{Constructor}{

\code{DynamicReducedDimensionPlot(...)} creates an instance of a DynamicReducedDimensionPlot class,
where any slot and its value can be passed to \code{...} as a named argument.
}

\section{Supported methods}{

In the following code snippets, \code{x} is an instance of a \linkS4class{DynamicReducedDimensionPlot} class.
Refer to the documentation for each method for more details on the remaining arguments.

For setting up data values:
\itemize{
\item \code{\link{.cacheCommonInfo}(x)} adds a \code{"DynamicReducedDimensionPlot"} entry containing \code{valid.assay.names}.
This will also call the equivalent \linkS4class{ColumnDotPlot} method.
\item \code{\link{.refineParameters}(x, se)} returns \code{x} after setting \code{"Assay"} to the first valid value.
This will also call the equivalent \linkS4class{ColumnDotPlot} method for further refinements to \code{x}.
If valid assay names are not available, \code{NULL} is returned instead.
}

For defining the interface:
\itemize{
\item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
\item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
\item \code{\link{.fullName}(x)} will return \code{"Dynamic reduced dimension plot"}.
}

For monitoring reactive expressions:
\itemize{
\item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all new slots described above, as well as in the parent classes via the \linkS4class{ColumnDotPlot} method.
}

For creating the plot:
\itemize{
\item \code{\link{.generateDotPlotData}(x, envir)} will create a data.frame of newly computed coordinates in \code{envir}.
The method will return the commands required to do so as well as a list of labels.
}

For handling multiple selections:
\itemize{
\item \code{\link{.multiSelectionInvalidated}(x)} will always return \code{TRUE},
as any change in the upstream selection of points will alter the coordinates and invalidate any brush/lasso on \code{x}.
}

For documentation:
\itemize{
\item \code{\link{.definePanelTour}(x)} returns an data.frame containing the steps of a panel-specific tour.
}
}

\examples{
library(scRNAseq)
library(scater)

sce <- ReprocessedAllenData(assays="tophat_counts")
sce <- logNormCounts(sce, exprs_values="tophat_counts")
sce <- runPCA(sce, ncomponents=4)
sce <- runTSNE(sce)

drdp <- DynamicReducedDimensionPlot(PanelId=1L, Assay="logcounts",
    ColumnSelectionSource="ReducedDimensionPlot1")

if (interactive()) {
    iSEE(sce, initial=list(ReducedDimensionPlot(PanelId=1L), drdp))
}

}
\author{
Aaron Lun
}
