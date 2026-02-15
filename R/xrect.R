#' xrect
#'
#' Draw rectangles from four columns xmin,xmax,ymin,ymax
#'
#' Calls [rect()], but will instantiate a plot if `add = FALSE`.
#'
#' @param x four columns worth of rectangles
#' @param add instantiate a plot or add to existing (default is `add = FALSE`)
#' @param ... arguments passed to [rect()]
#' @param asp aspect ratio, defaults to 1
#'
#' @return nothing, called for side effect of creating or adding to a plot
#' @export
#' @importFrom graphics rect
#' @examples
#' xrect(runif(100))
#' xrect(sort(runif(100)))
#' xrect(runif(100), col = hcl.colors(25, alpha = seq(.2, .8, length.out = 25)))
#'
#' ex <- c(0.2, 0.8, .2, .6)
#' xrect(ex, add = TRUE, lwd = 5, lty = 2)
xrect <- function(x, add = FALSE, ..., asp = 1L) {
  ## assuming cbind(xmin, xmax, ymin, ymax)
  x <- matrix(unlist(x, use.names = FALSE), ncol = 4L)
  xlim <- range(x[,1:2], na.rm = TRUE)
  ylim <- range(x[,3:4], na.rm = TRUE)
  if (!add) {
    plot(NA, xlim = xlim, ylim = ylim, asp = asp, xlab = "", ylab = "")
  }
  graphics::rect(x[,1L], x[,3L], x[,2L], x[, 4L], ...)
 invisible(NULL)
}
