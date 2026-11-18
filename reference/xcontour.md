# Contour matrix data and draw

Like contour() but to work with
[`ximage()`](https://hypertidy.github.io/ximage/reference/ximage.md)

## Usage

``` r
xcontour(
  x,
  extent = NULL,
  ...,
  add = FALSE,
  slice = NULL,
  xy = NULL,
  coords = NULL
)
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

- slice:

  index of the slice to draw for arrays with more than two dimensions: a
  named list or vector, e.g. `list(time = 3, depth = 1)`, an unnamed
  value for the first non-spatial dimension, or one row of
  [`xslices()`](https://hypertidy.github.io/ximage/reference/xslices.md)
  output. Default is the first slice

- xy:

  the two spatial dimensions, x then y, as names or positions

- coords:

  optional coordinates of the spatial dimensions, `list(x = , y = )` of
  centres, edges, or curvilinear matrices (see Details)

## Value

nothing, called for its side effect of creating or adding to a plot

## Details

Input may be anything
[`ximage()`](https://hypertidy.github.io/ximage/reference/ximage.md)
accepts that holds data values: a matrix, a list from
`gdal_raster_data()` in the vapour package or from `read_ds()` in the
gdalraster package, or an array (one slice is drawn, see
[`as_ximage()`](https://hypertidy.github.io/ximage/reference/as_ximage.md)).

With irregular or curvilinear `coords` the contours are traced in the
index space of the matrix and placed by interpolating the cell centre
coordinates. Curvilinear contours are drawn with
[`graphics::lines()`](https://rdrr.io/r/graphics/lines.html) without
labels ('levels' and 'nlevels' are used, other arguments go to
[`lines()`](https://rdrr.io/r/graphics/lines.html)).

## Examples

``` r
#EPSG:27200 see https://github.com/mdsumner/volcano
ex <- c(2667400, 2668010, 6478700, 6479570)
v <- volcano[nrow(volcano):1, ncol(volcano):1]
ximage(v, extent = ex, asp = 1)

xcontour(v, add = TRUE, extent = ex, col = "white")
xrect(ex, add = TRUE, border = "hotpink", lwd = 5)


## curvilinear
lon <- outer(1:87, 1:61, function(i, j) j + i / 10)
lat <- outer(1:87, 1:61, function(i, j) -i + j / 20)
ximage(volcano, coords = list(x = lon, y = lat), asp = 1)
xcontour(volcano, coords = list(x = lon, y = lat), add = TRUE)
```
