#' A new contour
#'
#' To work with [ximage()]
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
  x <- t(x[nrow(x):1, ])
  if (is.null(extent)) extent <- c(0, ncol(x), 0, nrow(x))
  xre <- diff(extent[1:2])/nrow(x)
  yre <- diff(extent[3:4])/ncol(x)
  xx <- seq(extent[1] + xre/2, extent[2] - xre/2, length.out = nrow(x) )
  yy <- seq(extent[3] + yre/2, extent[4] - yre/2, length.out = ncol(x) )
  graphics::contour(xx, yy, x, add = add, ...)
}
