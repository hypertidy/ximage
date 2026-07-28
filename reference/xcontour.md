# Contour matrix data and draw

Like contour() but to work with
[`ximage()`](https://hypertidy.github.io/ximage/reference/ximage.md)

## Usage

``` r
xcontour(x, extent = NULL, ..., add = FALSE)
```

## Arguments

- x:

  something we can contour

- extent:

  optional, numeric xmin,xmax,ymin,ymax

- ...:

  Arguments passed on to
  [`ximage`](https://hypertidy.github.io/ximage/reference/ximage.md)

  `zlim`

  :   optional, absolute range of data to map colours to (maintains
      comparable colours across plots); values outside display as
      'na.col'; single-band numeric data only

  `xlab`

  :   x axis label, empty by default

  `ylab`

  :   y axis label, empty by default

  `col`

  :   colours to map single-band data to

  `breaks`

  :   a set of finite numeric breakpoints for the colours, one more
      break than colour (if not, colours are interpolated to fit)

  `alpha`

  :   optional constant opacity in `[0, 1]` (or vector/matrix, recycled)
      applied on top of any existing alpha channel; not supported for
      nativeRaster input

  `na.col`

  :   colour for missing values, default "transparent"

  `force`

  :   proceed with a guessed dimension for very long bare vectors (see
      Details), default `FALSE`

- add:

  add to plot, or start afresh

## Value

nothing, called for its side effect of creating or adding to a plot

## Details

Input may be a matrix or a list from `gdal_raster_data()` in the vapour
package or from `read_ds()` in the gdalraster package.

## Examples

``` r
#EPSG:27200 see https://github.com/mdsumner/volcano
ex <- c(2667400, 2668010, 6478700, 6479570)
v <- volcano[nrow(volcano):1, ncol(volcano):1]
ximage(v, extent = ex, asp = 1)

xcontour(v, add = TRUE, extent = ex, col = "white")
xrect(ex, add = TRUE, border = "hotpink", lwd = 5)
```
