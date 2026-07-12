# ximage 0.1.0

* New function `xtext()` draws matrix values as labels at cell centres, in
 the same raster-order layout used by `ximage()` (thanks to a draft in
 issue #22). Supports vapour and gdalraster reader-output lists.

* New vignette "Array orientation: raster order and the R matrix" explains
 the raster-order convention, the `byrow = TRUE` and `as.vector(t(m))`
 round trip, multi-band `aperm()` recipes, and the cell-edge extent
 convention (#14).

* Now support raw output of fastpng. 

* Now support {gdalraster} output of `read_ds(, as_list = TRUE)` or raw numeric, thanks to Chris Toney. 

* `ximage()` now handles `breaks` argument to go with `col` (for numeric input). 

* `image()` now works with list output from gdalnara (same as gdal_raster_image but with nativeRaster). 

* Added suport for `xcontour()` for the output output of `gdal_raster_data()` in {vapour}. 

* `ximage()` can now plot sf::gdal_read objects. 

* Fixed default extent for an array/matrix, it was transposed (!). 

* ximage now does what imfun in whatarelief did, i.e. with the output of gdal_raster_data, gdal_raster_dsn, gdal_raster_image do the obvious plot. 

* Add 'mesh_plot' mode to `ximage()`, not fully implemented. 

* Increase the default colour classes, 12 -> 96. 

* Support 'raster' class as per as.raster. 

* Added xcontour and xrect functions. 
