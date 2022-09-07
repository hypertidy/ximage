t0 <- function(x) {
  d <- seq_len(length(dim(x)))
  d[1:2] <- d[2:1]
  aperm(x, d)
}

flip_r <- function(x) {
  dm <- dim(x)
  if (length(dm) == 3L) {
    x[dm[1L]:1L,,]
  } else {
    x[dm[1L]:1L, ]
  }
}
flip_c <- function(x) {
  dm <- dim(x)
  if (length(dm) == 3L) {
    x[,dm[2L]:1L,]
  } else {
    x[,dm[2L]:1L]
  }
}

.rescale <- function(x) {
  rg <- range(x, na.rm = TRUE)
  (x - rg[1L])/diff(rg)
}
.make_hex_matrix <- function(x, cols = NULL) {
   alpha <- 1
  if (length(dim(x)) > 2) {
    if (dim(x)[3] == 4L) {
      alpha <- x[,,4L]
    }
    out <- matrix(rgb(x[,,1L], x[,,2L], x[,,3L], alpha), dim(x)[1L])
  } else {

    out <- matrix(cols[x * (length(cols) - 1) + 1], dim(x)[1L])
  }
  out
}

# TODO
# dispatch matrix vs array using S3 class
# autodetect maxcolorvalue as 1 or 255, or allow override
# collapse RGB as intensity and allow colour map override
# allow greyscale mode ?
# allow raw mode

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
#' @param ylab y axis lable, empty by default
#' @param col optional colours to map matrix/array data to
#' @return a list with 'x' and 'extent' invisibly (extent is the 0,ncol 0,nrow space of the array if not supplied)
#' @export
#' @importFrom grDevices hcl.colors rgb
#' @importFrom graphics rasterImage
#' @examples
#' ximage(volcano)
#' ximage(as.raster(matrix(0:1, 49, 56)))
ximage <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(12, "YlOrRd", rev = TRUE)) {
  UseMethod("ximage")
}



#' @export
ximage.default <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(12, "YlOrRd", rev = TRUE)) {
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

    x <- (x - rg[1L])/diff(rg)

    x <- .make_hex_matrix(x, cols = col )
  } else {

    ## else character
    x <- matrix(x, dim(x)[1L])
  }
  if (is.null(extent)) {
    extent <- c(0, dim(x)[1L], 0, dim(x)[2L])
  }
  ## if !add
#  par(xaxs = "i", yaxs = "i")
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""

  if (!add) plot(extent[1:2], extent[3:4], type = "n", ..., xaxs = "i", yaxs = "i", xlab = xlab, ylab = ylab)
  graphics::rasterImage(x, extent[1], extent[3], extent[2], extent[4], interpolate = FALSE)
  invisible(list(x = x, extent = extent))
}

#' @export
ximage.nativeRaster <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(12, "YlOrRd", rev = TRUE)) {
    if (is.null(extent)) {
    extent <- c(0, dim(x)[1L], 0, dim(x)[2L])
    }
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""

  if (!add) plot(extent[1:2], extent[3:4], type = "n", ..., xaxs = "i", yaxs = "i", xlab = xlab, ylab = ylab)
  graphics::rasterImage(x, extent[1], extent[3], extent[2], extent[4], interpolate = FALSE)
}

#' @export
ximage.raster <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(12, "YlOrRd", rev = TRUE)) {
  ximage.nativeRaster(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col)
}


