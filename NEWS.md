# ximage dev

* Arrays with more than two dimensions are drawn one 2D slice at a time.
 The spatial dimensions are found by name (`names(dim())` as
 `zaro::zaro_read()` gives, `names(dimnames())` as `zaro::zaro_array()`
 gives, or the `gis` attribute of `gdalraster::mdim_read()`), or set with
 the new `xy` argument, and the new `slice` argument picks the slice
 (default the first). A message reports the slice drawn and how to get the
 others (silence with `options(ximage.verbose = FALSE)`). Selecting 3 or 4
 indices along one dimension draws an RGB(A) composite, e.g.
 `slice = list(band = c(4, 3, 2))`. Only the slice is read, so lazy arrays
 fetch only its chunks. Previously these inputs errored, or were silently
 drawn wrong: 3 or 4 time steps as false-colour RGB(A), 2 as grey/alpha,
 transposed, or with 4D slices side by side.

* Unnamed 3D arrays with more than 4 entries in the third dimension, and
 GDAL reader output with more than 4 bands, now draw the first slice with a
 message instead of an error (or silently the first band). Two bands from a
 reader list are two slices, not grey/alpha. Unnamed 3D arrays with 2 to 4
 entries are unchanged (grey/alpha, RGB, RGBA).

* New `coords` argument in `ximage()`, `xcontour()` and `xtext()` places
 data by coordinates: cell centres or edges as vectors, or curvilinear
 centres or corners as matrices (`extent = list(x, y)` is the same).
 Coordinates also come from `gdalraster::mdim_read()` metadata and ndr
 coordinates and affine indexes. Data is oriented north up from them: rows
 are reversed when y increases (most CF, NetCDF and Zarr data), columns
 when x decreases. Irregular, curvilinear and rotated or sheared grids are
 drawn cell by cell as quads, contours are traced in index space and
 placed through the coordinates.

* Scale and offset of `gdalraster::mdim_read()` output are applied, CF
 time coordinates are decoded for the standard calendars to label slices,
 and values of magnitude 1e30 or more are reported as a likely fill value.

* New `as_ximage()` is the shared input decoder behind `ximage()`,
 `xcontour()`, `xtext()` and `xpanels()` (#23); it returns the matrix to
 draw in raster order with its extent (or cell corners), slice and labels.
 It is an S3 generic so other packages can add methods. ndr `DataArray`
 and `Variable` objects are drawn through `ndr::isel()`, so lazy data stays
 lazy. Every verb now accepts every input form: `xcontour()` and `xtext()`
 gain support for gdalraster `gis` lists and arrays.

* `ximage()` now also returns 'zlim' (the data range colours were mapped
 to, for drawing other slices with the same colours), and 'dims', 'slice'
 and 'labels' for arrays.

* New `xslices()` lists the slices of an array (index and coordinate label
 per non-spatial dimension, metadata only), its rows work as `slice`. New
 `xpanels()` draws several slices with one colour scale.

* ximage now depends on R >= 3.5.0.

* Removed unused internal colour helpers left behind by the move to palr.

* Removed internal definition `to_hex()` in favour of imported `palr::image_hex()`. 

* ximage now imports palr. 


# ximage 0.1.0

* New vignette "Why ximage" frames the package motivation: raster order
 versus `image()`, `rasterImage()` as the honest primitive, GDAL flat
 reads and the dimension guess, and nativeRaster as the fast destination
 for colour imagery.

* Bare atomic vectors (numeric, integer, raw, character) with no dim and no
 known attributes now get a guessed dimension by integer-division detection
 of the length, choosing the most nearly square factorization (landscape
 preferred) and assuming raster scanline order, i.e. flat output from
 GDAL readers such as `vapour::gdal_raster_data()` or
 `gdalraster::read_ds()`, built as `matrix(x, ncol = NC, byrow = TRUE)`
 (column-major R data needs `dim(x) <- c(NR, NC)` instead). A message
 reports the guess and the candidate shapes from 1xN through Nx1, in GDAL
 dimension order (ncol x nrow). Raw vectors also consider 3-plane (RGB)
 and 4-plane (RGBA) scanline pixel-interleaved interpretations; numeric
 data never gets a plane interpretation by default. Vectors longer than `getOption("ximage.guess_max", 2^24)`
 error unless `force = TRUE` or the dimension is set explicitly.

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
