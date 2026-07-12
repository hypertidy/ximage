# Changelog

## ximage 0.1.0

- New function
  [`xtext()`](https://hypertidy.github.io/ximage/reference/xtext.md)
  draws matrix values as labels at cell centres, in the same
  raster-order layout used by
  [`ximage()`](https://hypertidy.github.io/ximage/reference/ximage.md)
  (thanks to a draft in issue
  [\#22](https://github.com/hypertidy/ximage/issues/22)). Supports
  vapour and gdalraster reader-output lists.

- New vignette “Array orientation: raster order and the R matrix”
  explains the raster-order convention, the `byrow = TRUE` and
  `as.vector(t(m))` round trip, multi-band
  [`aperm()`](https://rdrr.io/r/base/aperm.html) recipes, and the
  cell-edge extent convention
  ([\#14](https://github.com/hypertidy/ximage/issues/14)).

- Now support raw output of fastpng.

- Now support {gdalraster} output of `read_ds(, as_list = TRUE)` or raw
  numeric, thanks to Chris Toney.

- [`ximage()`](https://hypertidy.github.io/ximage/reference/ximage.md)
  now handles `breaks` argument to go with `col` (for numeric input).

- [`image()`](https://rdrr.io/r/graphics/image.html) now works with list
  output from gdalnara (same as gdal_raster_image but with
  nativeRaster).

- Added suport for
  [`xcontour()`](https://hypertidy.github.io/ximage/reference/xcontour.md)
  for the output output of `gdal_raster_data()` in {vapour}.

- [`ximage()`](https://hypertidy.github.io/ximage/reference/ximage.md)
  can now plot sf::gdal_read objects.

- Fixed default extent for an array/matrix, it was transposed (!).

- ximage now does what imfun in whatarelief did, i.e. with the output of
  gdal_raster_data, gdal_raster_dsn, gdal_raster_image do the obvious
  plot.

- Add ‘mesh_plot’ mode to
  [`ximage()`](https://hypertidy.github.io/ximage/reference/ximage.md),
  not fully implemented.

- Increase the default colour classes, 12 -\> 96.

- Support ‘raster’ class as per as.raster.

- Added xcontour and xrect functions.
