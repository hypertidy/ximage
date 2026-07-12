#' Plot an image (no matter what)
#'
#' ximage combines the best of image() and rasterImage().
#'
#' [ximage()] is designed like a graphics function with the following
#' limitations removed, `image()` 1:4, `rasterImage()` 5:7.
#'
#' \enumerate{
#'    \item Allow arrays with RGB/A.
#'    \item Allow matrix with character (named colours, or hex) or raw (Byte) values
#'    \item Allow list output from vapour or gdalraster, a list with numeric values, hex character, or nativeRaster
#'    \item Plot in 0,ncol 0,nrow by default
#'    \item Override default with extent (xmin, xmax, ymin, ymax)
#'
#'    \item Allow general numeric values.
#'    \item Start a plot from scratch without setting up a plot to paint to.
#'    \item Plot by default in 0,ncol,0,nrow if unspecified.
#' }
#'
#' Data orientation is "raster order", the first cell is the top-left of the
#' displayed image, following scan lines down the page (see the package
#' vignette on orientation).
#'
#' Colour mapping via 'col', 'breaks', and 'zlim' applies to single-band
#' numeric data only. Multi-band (grey/alpha, RGB, RGBA) data is scaled
#' automatically: values within 0,1 are used as-is, within 0,255 are divided
#' by 255, and anything else is rescaled by the finite range of the colour
#' bands. Missing values (NA, NaN) display as 'na.col' in all cases.
#'
#' @param x matrix, array, raw or character matrix, native raster
#'   (nativeRaster, or raster), or list as output by GDAL reader functions
#' @param extent optional, numeric xmin,xmax,ymin,ymax
#' @param zlim optional, absolute range of data to map colours to (maintains
#'   comparable colours across plots); values outside display as 'na.col';
#'   single-band numeric data only
#' @param add add to plot, or start afresh
#' @param ... passed to plot when `add = FALSE`
#' @param xlab x axis label, empty by default
#' @param ylab y axis label, empty by default
#' @param col colours to map single-band data to
#' @param breaks a set of finite numeric breakpoints for the colours, one more
#'   break than colour (if not, colours are interpolated to fit)
#' @param alpha optional constant opacity in `[0, 1]` (or vector/matrix,
#'   recycled) applied on top of any existing alpha channel; not supported for
#'   nativeRaster input
#' @param na.col colour for missing values, default "transparent"
#'
#' @return invisibly, a list with 'x' (the colour data as plotted) and
#'   'extent' (xmin, xmax, ymin, ymax used, the 0,ncol 0,nrow index space of
#'   the input if not supplied)
#' @export
#' @importFrom grDevices hcl.colors rgb col2rgb colorRampPalette
#' @importFrom graphics rasterImage
#' @examples
#' ximage(volcano)
#' ximage(as.raster(matrix(0:1, 49, 56)))
#' v <- volcano
#' v[v > 180] <- NA
#' ximage(v, na.col = "hotpink")
ximage <- function(x, extent = NULL, zlim = NULL, add = FALSE, ...,
                   xlab = NULL, ylab = NULL,
                   col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                   alpha = NULL, na.col = "transparent") {
  UseMethod("ximage")
}


#' @export
#' @importFrom stats na.omit
ximage.list <- function(x, extent = NULL, zlim = NULL, add = FALSE, ...,
                        xlab = NULL, ylab = NULL,
                        col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                        alpha = NULL, na.col = "transparent") {

  if (all(c("geotransform", "cols", "rows", "driver") %in% names(x))) {
    ## smells like sf
    out <- ximage_sf_data(x, extent = extent, zlim = zlim, add = add, ...,
                          xlab = xlab, ylab = ylab, col = col, breaks = breaks,
                          alpha = alpha, na.col = na.col)
    return(invisible(out))
  }

  ## validate that we have extent, dimension as attributes
  attrs <- attributes(x)
  if ("gis" %in% names(attrs)) {
    ## gdalraster output
    attrs <- attrs[["gis"]]
    attrs$dimension <- attrs$dim
    attrs$projection <- attrs$srs
    attrs$extent <- attrs$bbox[c(1, 3, 2, 4)]
  }
  if (!is.null(attrs$extent) && is.null(extent)) extent <- attrs$extent
  dimension <- attrs$dimension
  ## a dimension attribute (vapour, gdalraster gis) means the elements hold
  ## GDAL row-major data, whether stored as vectors or as (ncol, nrow)
  ## matrices (e.g. from asplit()); with no dimension attribute a matrix
  ## element is taken to be an already-oriented R matrix
  row_major <- !is.null(dimension)
  if (is.null(dimension)) {
    dm1 <- dim(x[[1L]])
    if (!is.null(dm1) && length(dm1) >= 2L) {
      dimension <- dm1[2:1]
    } else {
      stop("no dimension known")
    }
  }

  if (is.character(x[[1L]])) {
    first <- stats::na.omit(x[[1L]])
    if (length(first) && (grepl("^#", first[1L]) ||
                          first[1L] %in% grDevices::colours())) {
      ## we have image (colour) data
    } else {
      ## can't read data in ximage
      stop("can't read data in this package")
    }
  }

  if (inherits(x[[1L]], "nativeRaster")) {
    out <- ximage(x[[1L]], extent = extent, zlim = zlim, add = add, ...,
                  xlab = xlab, ylab = ylab, col = col, breaks = breaks,
                  alpha = alpha, na.col = na.col)
    return(invisible(out))
  }

  if (length(x) %in% c(3, 4)) {
    if (row_major) {
      arr <- aperm(array(unlist(lapply(x, as.vector), use.names = FALSE),
                         c(dimension[1:2], length(x))),
                   c(2, 1, 3))
    } else {
      arr <- array(unlist(x, use.names = FALSE), c(dim(x[[1L]])[1:2], length(x)))
    }
    out <- ximage(arr,
                  extent = extent, zlim = zlim, add = add, ...,
                  xlab = xlab, ylab = ylab, col = col, breaks = breaks,
                  alpha = alpha, na.col = na.col)
  } else {
    m <- if (row_major) {
      matrix(as.vector(x[[1L]]), dimension[2L], byrow = TRUE)
    } else {
      x[[1L]]
    }
    out <- ximage(m,
                  extent = extent, zlim = zlim, add = add, ...,
                  xlab = xlab, ylab = ylab, col = col, breaks = breaks,
                  alpha = alpha, na.col = na.col)
  }
  invisible(out)
}

#' @export
ximage.raw <- function(x, extent = NULL, zlim = NULL, add = FALSE, ...,
                       xlab = NULL, ylab = NULL,
                       col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                       alpha = NULL, na.col = "transparent") {
  x <- .unpack_whd(x)
  ximage.default(x, extent = extent, zlim = zlim, add = add, ...,
                 xlab = xlab, ylab = ylab, col = col, breaks = breaks,
                 alpha = alpha, na.col = na.col)
}

#' @export
ximage.numeric <- function(x, extent = NULL, zlim = NULL, add = FALSE, ...,
                           xlab = NULL, ylab = NULL,
                           col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                           alpha = NULL, na.col = "transparent") {
  x <- .unpack_whd(x)
  ximage.default(x, extent = extent, zlim = zlim, add = add, ...,
                 xlab = xlab, ylab = ylab, col = col, breaks = breaks,
                 alpha = alpha, na.col = na.col)
}

#' @export
ximage.integer <- function(x, extent = NULL, zlim = NULL, add = FALSE, ...,
                           xlab = NULL, ylab = NULL,
                           col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                           alpha = NULL, na.col = "transparent") {
  x <- .unpack_whd(x)
  ximage.default(x, extent = extent, zlim = zlim, add = add, ...,
                 xlab = xlab, ylab = ylab, col = col, breaks = breaks,
                 alpha = alpha, na.col = na.col)
}

## unpack fastpng-style width/height/depth attributed vectors to array,
## dropping a depth-1 dimension (grey stays a matrix)
.unpack_whd <- function(x) {
  attrs <- attributes(x)
  if (all(c("width", "height", "depth") %in% names(attrs))) {
    x <- aperm(array(x, c(attrs$depth, attrs$width, attrs$height)), c(3, 2, 1))
    if (attrs$depth == 1) x <- x[, , 1L, drop = TRUE]
  }
  x
}

#' @export
ximage.default <- function(x, extent = NULL, zlim = NULL, add = FALSE, ...,
                           xlab = NULL, ylab = NULL,
                           col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                           alpha = NULL, na.col = "transparent") {

  if (is.list(x)) {
    out <- ximage.list(x, extent = extent, zlim = zlim, add = add, ...,
                       xlab = xlab, ylab = ylab, col = col, breaks = breaks,
                       alpha = alpha, na.col = na.col)
    return(invisible(out))
  }

  if (is.numeric(x) && "gis" %in% names(attributes(x))) {
    ## vector output from gdalraster
    gis <- attr(x, "gis")
    x_list <- asplit(array(x, dim = gis$dim), MARGIN = 3)
    attr(x_list, "gis") <- gis
    out <- ximage.list(x_list, extent = extent, zlim = zlim, add = add, ...,
                       xlab = xlab, ylab = ylab, col = col, breaks = breaks,
                       alpha = alpha, na.col = na.col)
    return(invisible(out))
  }

  stopifnot(is.array(x) || is.matrix(x))

  ## single choke point: everything becomes a hex colour matrix
  x <- to_hex(x, col = col, breaks = breaks, zlim = zlim,
              alpha = alpha, na.col = na.col)

  if (is.null(extent)) {
    extent <- c(0, dim(x)[2L], 0, dim(x)[1L])
  }
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""

  if (is.list(extent) && length(extent) == 2) {
    stop("curvilinear 'extent' (list of x, y arrays) is not supported, see the quadmesh package")
  }
  if (!add) plot(extent[1:2], extent[3:4], type = "n", ..., xaxs = "i", yaxs = "i",
                 xlab = xlab, ylab = ylab)

  graphics::rasterImage(x, extent[1L], extent[3L], extent[2L], extent[4L],
                        interpolate = FALSE)
  invisible(list(x = x, extent = extent))
}

#' @export
ximage.nativeRaster <- function(x, extent = NULL, zlim = NULL, add = FALSE, ...,
                                xlab = NULL, ylab = NULL,
                                col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                                alpha = NULL, na.col = "transparent") {
  if (!is.null(alpha)) warning("'alpha' is not supported for nativeRaster input, ignored")
  if (is.null(extent)) {
    extent <- c(0, dim(x)[2L], 0, dim(x)[1L])
  }
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""

  if (is.list(extent) && length(extent) == 2) {
    stop("curvilinear 'extent' (list of x, y arrays) is not supported, see the quadmesh package")
  }
  if (!add) plot(extent[1:2], extent[3:4], type = "n", ..., xaxs = "i", yaxs = "i",
                 xlab = xlab, ylab = ylab)
  graphics::rasterImage(x, extent[1L], extent[3L], extent[2L], extent[4L],
                        interpolate = FALSE)
  invisible(list(x = x, extent = extent))
}

#' @export
ximage.raster <- function(x, extent = NULL, zlim = NULL, add = FALSE, ...,
                          xlab = NULL, ylab = NULL,
                          col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                          alpha = NULL, na.col = "transparent") {
  if (is.null(extent)) {
    extent <- c(0, dim(x)[2L], 0, dim(x)[1L])
  }
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""
  x <- to_hex(matrix(as.character(x), dim(x)[1L], dim(x)[2L]),
              alpha = alpha, na.col = na.col)
  ximage.default(x, extent = extent, zlim = zlim, add = add, ...,
                 xlab = xlab, ylab = ylab, col = col, breaks = breaks,
                 alpha = NULL, na.col = na.col)
}


.gt_dim_to_extent <- function(x, dim) {
  xx <- c(x[1], x[1] + dim[1] * x[2])
  yy <- c(x[4] + dim[2] * x[6], x[4])
  c(xx, yy)
}

ximage_sf_data <- function(x, extent = NULL, ...) {
  d <- attr(x, "data")
  dm <- dim(d)
  do_extent <- TRUE
  if (!is.null(extent)) {
    do_extent <- FALSE
  }
  if (is.null(d)) stop("no data in sf read object")

  if (is.null(dm) || length(dm) < 2) {
    d <- matrix(d)
    if (!is.null(extent)) warning("extent ignored for 1D array")
    extent <- NULL
  } else if (length(dm) > 2) {
    d <- matrix(d[seq_len(prod(dm[1:2]))], dm[1L], dm[2L])
  }
  if (do_extent) {
    ## sf gdal_read doesn't update the geotransform offsets so do that here
    gt <- x$geotransform
    if (x$cols[1] > 1) {
      gt[1] <- gt[1] + gt[2] * (x$cols[1] - 1)
    }
    if (x$rows[1] > 1) {
      gt[4] <- gt[4] + gt[6] * (x$rows[1] - 1)
    }
    extent <- .gt_dim_to_extent(gt, dim(d)[2:1])
  }
  ximage(t(d), extent = extent, ...)
}
