#' Contour matrix data and draw
#'
#' Like contour() but to work with [ximage()]
#'
#' Input may be a matrix or a list from `gdal_raster_data()` in the vapour package or from `read_ds()`
#' in the gdalraster package.
#'
#' @param x something we can contour
#' @inheritParams ximage
#' @inheritDotParams ximage
#' @return nothing, called for its side effect of creating or adding to a plot
#' @export
#' @importFrom graphics contour
#' @examples
#' #EPSG:27200 see https://github.com/mdsumner/volcano
#' ex <- c(2667400, 2668010, 6478700, 6479570)
#' v <- volcano[nrow(volcano):1, ncol(volcano):1]
#' ximage(v, extent = ex, asp = 1)
#'
#' xcontour(v, add = TRUE, extent = ex, col = "white")
#' xrect(ex, add = TRUE, border = "hotpink", lwd = 5)
xcontour <- function(x, extent = NULL, ..., add = FALSE) {
  UseMethod("xcontour")
}
#' @export
xcontour.default <- function(x, extent = NULL, ..., add = FALSE) {
  if (is.numeric(x) && is.null(dim(x)) && "gis" %in% names(attributes(x))) {
    ## vector output from gdalraster, first band
    gis <- attr(x, "gis")
    if (is.null(extent)) extent <- gis$bbox[c(1, 3, 2, 4)]
    x <- matrix(x[seq_len(prod(gis$dim[1:2]))], gis$dim[2L], byrow = TRUE)
  }
  if (is.null(dim(x)) || length(dim(x)) < 2L) {
    stop("'x' must be a matrix")
  }
  ## default extent is the index space of the input, before reorientation
  if (is.null(extent)) extent <- c(0, ncol(x), 0, nrow(x))
  x <- t(x[nrow(x):1, ])
  xre <- diff(extent[1:2])/nrow(x)
  yre <- diff(extent[3:4])/ncol(x)
  xx <- seq(extent[1] + xre/2, extent[2] - xre/2, length.out = nrow(x) )
  yy <- seq(extent[3] + yre/2, extent[4] - yre/2, length.out = ncol(x) )
  graphics::contour(xx, yy, x, add = add, ...)
}
#' @export
xcontour.list <- function(x, extent = NULL, ..., add = FALSE) {

  if (all(c("geotransform", "cols", "rows", "driver") %in% names(x))) {
    ## smells like sf
    stop("no xcontour for sf")
    ximage_sf_data(x, extent = extent,  add = add, ...)
    return(invisible(x))
  }
  ## validate that we have extent, dimension as attributes
  attrs <- attributes(x)
  if ("gis" %in% names(attrs)) {
    ## gdalraster output
    attrs <- attrs[["gis"]]
    attrs$dimension <- attrs$dim
    attrs$extent <- attrs$bbox[c(1, 3, 2, 4)]
  }
  if (!is.null(attrs$extent) && is.null(extent)) extent <- attrs$extent
  dimension <- attrs$dimension
  ## a dimension attribute (vapour, gdalraster gis) means the elements hold
  ## GDAL row-major data; with no dimension attribute a matrix element is
  ## taken to be an already-oriented R matrix (same rule as ximage.list)
  row_major <- !is.null(dimension)
  if (is.null(dimension) &&
      (is.null(dim(x[[1L]])) || length(dim(x[[1L]])) < 2L)) {
    stop("no dimension known")
  }
  if (is.character(x[[1L]])) {
    stop("cannot contour colour data")
  }

  m <- if (row_major) {
    matrix(as.vector(x[[1L]]), dimension[2L], byrow = TRUE)
  } else {
    x[[1L]]
  }
  xcontour(m, extent = extent, add = add, ...)

  ##if (coastline) graphics::lines(coastline(extent, projection = projection, dimension = c(512, 512)))

  ## return the materialized data
  invisible(x)
}
