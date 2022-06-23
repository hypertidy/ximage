## rotate image to rasterImage
r0 <- function(x) t(x[,ncol(x):1])
## rotate rasterImage to image
r1 <- function(x) t(x)[, ncol(x):1]


.make_hex_matrix <- function(x) {
   alpha <- 1
  if (length(dim(x)) > 2) {
    if (dim(x)[3] == 4L) {
      alpha <- x[,,4L]
    }
    out <- matrix(rgb(x[,,1L], x[,,2L], x[,,3L], alpha), dim(x)[1L])
  } else {
    out <- matrix(hcl.colors(12, "YlOrRd", rev = TRUE)[scales::rescale(x, c(1, 12))], dim(x)[1L])
  }
  out
}

#' Plot an image (no matter what)
#'
#' ximage combines the best of image() and rasterImage().
#'
#' @param x
#' @param extent
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ximage <- function(x, extent = NULL, ...) {
  UseMethod("ximage")
}

#' Title
#'
#' @param x
#' @param extent
#' @param zlim
#' @param add
#' @param ...
#'
#'@export
ximage.default <- function(x, extent = NULL, zlim = NULL, add = FALSE, ...) {
  stopifnot(inherits(x, "array"))
  ## rescale to
  ## assume RGB if dim(x)[3] is 3, or 4
  ## allow 0,1 or arbitrary numeric
  ## allow character hex
  if (is.numeric(x)) {
    rg <- range(x, na.rm = TRUE)
    x <- (x - rg[1L])/diff(rg)
    x <- .make_hex_matrix(x)
  } else {
    if (is.raw(x)) {
      ## convert
    }
    ## else character
    x <- matrix(x, dim(x)[1L])
  }
  if (is.null(extent)) {
    extent <- c(0, dim(x)[1L], 0, dim(x)[2L])
  }
  ## if !add
#  par(xaxs = "i", yaxs = "i")
  plot(extent[1:2], extent[3:4], type = "n", ..., xaxs = "i", yaxs = "i")
  rasterImage(r0(x), extent[1], extent[3], extent[2], extent[4], interpolate = FALSE)
}
