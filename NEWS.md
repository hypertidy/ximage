# ximage 0.0

* Now support {gdalraster} output of `read_ds(, as_list = TRUE)`. 

* `ximage()` now handles `breaks` argument to go with `col` (for numeric input). 

* `image()` now works with list output from gdalnara (same as gdal_raster_image but with nativeRaster). 

* Added suport for `xcontour()` for the output output of `gdal_raster_data()` in {vapour}. 

* `ximage()` can now plot sf::gdal_read objects. 

* Fixed default extent for an array/matrix, it was transposed (!). 

* ximage now dows what imfun in whatarelief did, i.e. with the output of gdal_raster_data, gdal_raster_dsn, gdal_raster_image do the obvious plot. 

* Add 'mesh_plot' mode to `ximage()`, not fully implemented. 

* Increase the default colour classes, 12 -> 96. 

* Support 'raster' class as per as.raster. 

* Added xcontour and xrect functions. 
