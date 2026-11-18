# Label the cells of a matrix in place

Draw the values of a matrix as text labels, each at the centre of its
cell in the same layout used by
[`ximage()`](https://hypertidy.github.io/ximage/reference/ximage.md).
The first cell of the matrix is the top-left label, and each row of the
matrix is a line of labels reading left to right down the page ("raster
order").

## Usage

``` r
xtext(
  x,
  extent = NULL,
  add = FALSE,
  cex = NULL,
  fit = 0.9,
  ...,
  slice = NULL,
  xy = NULL,
  coords = NULL
)
```

## Arguments

- x:

  a matrix of values to draw as labels, or a reader-output list

- extent:

  optional, numeric xmin,xmax,ymin,ymax (defaults to the 0,ncol 0,nrow
  index space of the matrix)

- add:

  add to plot, or start afresh

- cex:

  text size, computed to fit the cells if not supplied

- fit:

  fraction of the cell the largest label should occupy when 'cex' is
  computed, default 0.9

- ...:

  arguments passed to [`text()`](https://rdrr.io/r/graphics/text.html),
  for example 'col'

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

invisibly, a list with 'x', 'y' (label positions), 'labels', and 'cex'
as used

## Details

Text size is chosen automatically to fit the largest label within a
cell, scaled by 'fit' (use 'cex' to override). Labels that are NA are
not drawn.

Input may be a matrix (values are used as labels via
[`format()`](https://rdrr.io/r/base/format.html)), a list in the style
of vapour or gdalraster reader output (a row-major vector with
'dimension'/'extent' attributes, or a 'gis' attribute), or an array (one
slice is labelled, see
[`as_ximage()`](https://hypertidy.github.io/ximage/reference/as_ximage.md)).
With irregular or curvilinear `coords` each label sits at the centre of
its cell's corners.

## Examples

``` r
m <- matrix(1:12, 3, byrow = TRUE)
ximage(m)
xtext(m, add = TRUE)


## the label positions are cell centres in the extent
ximage(volcano, extent = c(0, 1, 0, 1))
xtext(volcano[seq(1, 87, by = 8), seq(1, 61, by = 6)],
      extent = c(0, 1, 0, 1), add = TRUE, col = "white")
```
