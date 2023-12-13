#' A new contour
#'
#' To work with [ximage()]
#'
#' Input may be a matrix or a list from gdal_raster_data() in the vapour package.
#'
#' @param x something we can contour
#' @inheritParams ximage
#' @inheritDotParams ximage
#' @return nothing, called for its side effect of creating or adding to a plot
#' @export
#' @importFrom graphics contour
#' @examples
#' ex <-  c(2667394, 2668004, 6478902, 6479772)
#' v <- volcano[nrow(volcano):1, ncol(volcano):1]
#' ximage(v, extent = ex, asp = 1)
#' #im <- whatarelief::imagery(extent = ex, projection = "+proj=nzmg +datum=WGS84")
#' #ximage(im, add = TRUE, extent = ex)
#' xcontour(v, add = TRUE, extent = ex, col = "white")
xcontour <- function(x, extent = NULL, ..., add = FALSE) {
  UseMethod("xcontour")
}
#' @export
xcontour.default <- function(x, extent = NULL, ..., add = FALSE) {
  x <- t(x[nrow(x):1, ])
  if (is.null(extent)) extent <- c(0, ncol(x), 0, nrow(x))
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
    ximage_sf_data(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col)
    return(invisible(x))
  }
   ## here validate that we have extent, dimension as attributes, otherwise just see if it's a matrix
  attrs <- attributes(x)
  if (!is.null(attrs$extent) && is.null(extent)) extent <- attrs$extent
  dimension <- NULL
  if (!is.null(attrs$dimension)) {
    dimension <- attrs$dimension

  }
  projection <- NULL

  if (is.null(dimension)) {
    if (is.null(dim(x[[1]]))) {
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
      stop("can't read data in the this package")
#      x[[1]] <- as.vector(t(elevation(source = x[[1]], extent = attr(x, "extent"), dimension = attr(x, "dimension"), projection = attr(x, "projection"))))
    }
  }


    xcontour(matrix(x[[1]], dimension[2L], byrow = TRUE),
                   extent = extent,  add = add, ...)

  ##if (coastline) graphics::lines(coastline(extent, projection = projection, dimension = c(512, 512)))

  ## return the materialized data
  invisible(x)
}
