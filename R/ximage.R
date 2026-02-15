
.rescale <- function(x) {
  rg <- range(x, na.rm = TRUE)
  (x - rg[1L])/diff(rg)
}

#' A new image()

#' Plot an image (no matter what)
#'
#' ximage combines the best of image() and rasterImage().
#'
#' [ximage()] is designed like graphics function with the following limitations removed,
#' `image()` 1:4, `rasterImage()` 5:7.
#'
#'
#' \enumerate{
#'    \item Allow arrays with RGB/A.
#'    \item Allow matrix with character (named colours, or hex) or raw (Byte) values
#'    \item Allow list output from vapour, a list with numeric values, hex character, or nativeRaster
#'    \item Plot in 0,ncol 0,nrow by default
#'    \item Override default with extent (xmin, xmax, ymin, ymax)
#'
#'    \item Allow general numeric values.
#'    \item Start a plot from scratch without setting up a plot to paint to.
#'    \item Plot by default in 0,ncol,0,nrow if unspecified.
#' }
#'
#'  ximage uses the GIS raster default used by rasterImage. WIP: There is a similar function 'image0?'
#'  that provides the same features but assumes that orientation is like image...
#'
#' Colours by 'col' are only mapped for numeric data, this may change (to remap RGB or raw imagery via greyscale conversion)
#'
#' @param x matrix, array, or native raster (nativeRaster, or raster)
#' @param extent optional, numeric xmin,xmax,ymin,ymax
#' @param zlim optional, range of data to set colour map (to maintain absolute colours across multiple plots)
#' @param add add to plot, or start afresh
#' @param ... passed to plot when `add = FALSE`
#' @param xlab x axis label, empty by default
#' @param ylab y axis label, empty by default
#' @param breaks a set of finite numeric breakpoints for the colours (optional, passed to underlying color mapping functions)
#' @param col optional colours to map matrix/array data to
#'
#' @return a list with components: 'x' (the input object or processed raster data) and 'extent' (a numeric vector of length 4 giving xmin, xmax, ymin, ymax; defaults to c(0, ncol, 0, nrow) if not supplied). Returned invisibly.
#' @export
#' @importFrom grDevices hcl.colors rgb
#' @importFrom graphics rasterImage
#' @examples
#' ximage(volcano)
#' ximage(as.raster(matrix(0:1, 49, 56)))
ximage <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {
  UseMethod("ximage")
}


#' @export
#' @importFrom stats na.omit
ximage.list <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {


  if (all(c("geotransform", "cols", "rows", "driver") %in% names(x))) {
    ## smells like sf
    ximage_sf_data(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col)
    return(invisible(x))
  }

   ## here validate that we have extent, dimension as attributes, otherwise just see if it's a matrix
  attrs <- attributes(x)
  if ("gis" %in% names(attrs)) {
    ## gdalraster output
    attrs <- attrs[["gis"]]
    attrs$dimension <- attrs$dim
    attrs$projection <- attrs$srs
    attrs$extent <- attrs$bbox[c(1, 3, 2, 4)]
  }
  if (!is.null(attrs$extent) && is.null(extent)) extent <- attrs$extent
  dimension <- NULL
  if (!is.null(attrs$dimension)) {
    dimension <- attrs$dimension

  }
  projection <- NULL

  if (is.null(dimension)) {
    if (!is.null(dim(x[[1]]))) {
      dimension <- dim(x[[1]])
    } else {
    stop("no dimension known")
    }
  }
 if (!is.null(attrs$projection)) projection <- attrs$projection
  if (is.character(x[[1]])) {
    if (grepl("^#", stats::na.omit(x[[1]])[1])) {
      ## we have image data
    } else {
      ## can't read data in ximage
      stop("can't read data in this package")
    }
  }

    if (inherits(x[[1]], "nativeRaster")) {
    ximage(x[[1L]], extent = extent,  zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col)
      return(invisible(x))
  }

  if (length(x) %in% c(3, 4)) {
    ximage(aperm(array(unlist(x), c(dimension[1:2], 3)), c(2, 1, length(x))),
                 extent = extent,  zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col, breaks = breaks)
  } else {
    ximage(matrix(x[[1]], dimension[2L], byrow = TRUE),
                   extent = extent,  zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col, breaks = breaks)
  }

  ## return the materialized data
  invisible(x)
}
#' @export
ximage.raw <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {
  if (all(c("width", "height", "depth") %in% names(attributes(x)))) {
    attrs <- attributes(x)
    x <- aperm(array(x, c(attrs$depth, attrs$width, attrs$height)), c(3, 2, 1))
    if (attrs$depth == 1) x<- x[,,1L, drop = TRUE]
  }
  ximage.default(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col, breaks = breaks)
}

#' @export
ximage.numeric <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {
  if (all(c("width", "height", "depth") %in% names(attributes(x)))) {
    attrs <- attributes(x)
    x <- aperm(array(x, c(attrs$depth, attrs$width, attrs$height)), c(3, 2, 1))
  }
  ximage.default(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col, breaks = breaks)
}

#' @export
ximage.integer <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {
  if (all(c("width", "height", "depth") %in% names(attributes(x)))) {
    attrs <- attributes(x)
    x <- aperm(array(x, c(attrs$depth, attrs$width, attrs$height)), c(3, 2, 1))
  }
  ximage.default(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col, breaks = breaks)
}

#' @importFrom grDevices colorRampPalette
#' @export
ximage.default <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {

  if (is.list(x)) {
    ximage.list(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col)
    return(invisible(x))
  }

  if (is.numeric(x) && "gis" %in% names(attributes(x))) {
    ## vector output from gdalraster
    gis <- attr(x, "gis")
    x_list <- asplit(array(x, dim = gis$dim), MARGIN=3)
    attr(x_list, "gis") <- gis
    ximage.list(x_list, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col)
    return(invisible(x_list))
  }



  stopifnot(inherits(x, "array"))

   if (is.raw(x)) {
      ## convert
      x <- array(as.integer(x), dim(x))
    }
  ## rescale to
  ## assume RGB if dim(x)[3] is 3, or 4
  ## allow 0,1 or arbitrary numeric
  ## allow character hex
  if (is.numeric(x)) {
    rg <- range(x, na.rm = TRUE)

    ## we're not expecting zlim to be used if it's RGB/A
    if (!is.null(zlim)) {

      x[x < zlim[1L]] <- NA
      x[x > zlim[2L]] <- NA
      if (is.finite(zlim[1])) rg[1] <- zlim[1]
      if (is.finite(zlim[2])) rg[2] <- zlim[2]

    }

    ## politely ignore numeric arrays with 3 or 4 slices
    dmx <- dim(x)
    tt <- length(dmx) %in% c(3, 4) && is.numeric(x) ##&& all(x >= 0, na.rm = TRUE)

    if (!tt) {
      if (is.null(col)) col <-  colorRampPalette(grDevices::hcl.colors(12, "YlOrRd",
                                                                              rev = TRUE))


      if (length(breaks) > 0 && !(length(breaks)-1) == length(col)) {

        col <- colorRampPalette(col)(length(breaks)-1)
      }
      x <- matrix(palr::image_pal(x, col, breaks = breaks), dim(x)[1L], dim(x)[2L])
    } else {
      ## here would should nara
      x <- (x - rg[1L])/diff(rg)


    }

  } else {

    ## else character
    x <- matrix(x, dim(x)[1L])
  }
  if (is.null(extent)) {
    extent <- c(0, dim(x)[2L], 0, dim(x)[1L])
  }
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""

  if (is.list(extent) && length(extent) == 2) {
    stop("meshplot not yet supported")
  }
  if (!add) plot(extent[1:2], extent[3:4], type = "n", ..., xaxs = "i", yaxs = "i", xlab = xlab, ylab = ylab)

  graphics::rasterImage(x, extent[1], extent[3], extent[2], extent[4], interpolate = FALSE)
  invisible(list(x = x, extent = extent))
}

#' @export
#' @return a list with components: 'x' (the input object) and 'extent' (a numeric vector of length 4 giving xmin, xmax, ymin, ymax; defaults to c(0, ncol, 0, nrow) if not supplied). Returned invisibly.
ximage.nativeRaster <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {
    if (is.null(extent)) {
    extent <- c(0, dim(x)[2L], 0, dim(x)[1L])
    }
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""

  if (is.list(extent) && length(extent) == 2) {
    stop("meshplot not yet supported")
  }
  if (!add) plot(extent[1:2], extent[3:4], type = "n", ..., xaxs = "i", yaxs = "i", xlab = xlab, ylab = ylab)
  graphics::rasterImage(x, extent[1], extent[3], extent[2], extent[4], interpolate = FALSE)
  invisible(list(x = x, extent = extent))
}

#' @export
#' @return a list with components: 'x' (the input object) and 'extent' (a numeric vector of length 4 giving xmin, xmax, ymin, ymax; defaults to c(0, ncol, 0, nrow) if not supplied). Returned invisibly.
ximage.raster <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {
  ximage.nativeRaster(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col, breaks = breaks)
}



.gt_dim_to_extent <- function (x, dim)
{
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

  if (is.null(dm) || length(dm)  < 2) {
    d <- matrix(d)
    if (!is.null(extent)) warning("extent ignored for 1D array")
    extent <- NULL
  } else if (length(dm) > 2) {
    d <- matrix(d[1:prod(dm[1:2])], dm[1], dm[2])
  }
  if (do_extent) {

    ## sf gdal_read (@  4901a41ec56d2ad1524bab553c9195a6bd417987) doesn't update the geotransform ofsets so we do that here
    gt <- x$geotransform
    if (x$cols[1] > 1) {
      gt[1] <- gt[1] + gt[2] * (x$cols[1] - 1)
    }
    if (x$rows[1] > 1) {
      gt[4] <- gt[4] + gt[6] * (x$rows[1] - 1)
    }


      extent <- .gt_dim_to_extent(gt, dm[1:2])
  }
  ximage(t(d), extent = extent, ...)
}
