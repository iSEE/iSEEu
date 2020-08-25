# iSEEu 1.1.7

* Improved safety and correctness of the calculation of the number of DEGs.

# iSEEu 1.1.6

* Version bump to trigger reinstallation with new iSEE class definitions.

# iSEEu 1.1.5

* Added panel-specific tours for all panel classes via the `.definePanelTour()` generic.

# iSEEu 1.1.4

* Generalized the DE-related globals to work as patterns rather directly specifying the acceptable names.
* Align `DynamicMarkerTable`'s treatment of `getTableExtraFields()` with the globals strategy.

# iSEEu 1.1.3

* Overhauled handling of global parameters for greater consistency.
* Added the `LogFCLogFCPlot` to plot two DE comparisons against each other.
* Switched to KEGGREST to get the names of pathways.

# iSEEu 1.1.2

* Replaced `GeneSetTable` with the more general `FeatureSetTable`.
  Improved handling of arbitrary feature sets.
* Renamed `DifferentialStatisticsTable` to the more appropriate `DynamicMarkerTable`.
  Support inclusion of extra fields from the `rowData`.
* Global parameters now only affect construction of `MAPlot`s and `VolcanoPlot`s.

# iSEEu 1.1.1

* Improved documentation of the `ReducedDimensionHexPlot` methods.

# iSEEu 0.99.8

* Trigger new build on the single package builder.

# iSEEu 0.99.7

* Replace `!=""` by `nzchar()`.
* Use 4-space indent.
* Add info on how to contribute to iSEEu, including coding style.

# iSEEu 0.99.6

* Trigger new build on the single package builder.

# iSEEu 0.99.5

* Add more suggested packages.
* Add screenshot images to the vignette.
* More realistic examples in the vignette.
* Fix `GeneSetTable` to acknowledge initial value of `"Selected"`.

# iSEEu 0.99.4

* Fix to GitHub action `R-CMD-check`.

# iSEEu 0.99.3

* Set up GitHub action `R-CMD-check`.
* Remove Travis CI `covr` code coverage and `pkgdown` site deployment.
* Fix man page warning.

# iSEEu 0.99.2

* Create observer for `"Assay"` in `DiffStatTable`.
* Update NEWS.

# iSEEu 0.99.1

* Trigger new build on the single package builder.

# iSEEu 0.99.0

* Added panel `DifferentialStatisticsTable`.
* Added panel `DynamicReducedDimensionPlot`.
* Added panel `GeneSetTable`.
* Added panel `MAPlot`.
* Added panel `ReducedDimensionHexPlot`.
* Added panel `VolcanoPlot`.
* Added mode `modeEmpty`.
* Added mode `modeGating`.
* Added mode `modeReducedDim`.

# iSEEu 0.1.0

* `iSEEu` is officially born!
