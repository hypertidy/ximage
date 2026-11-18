#' Label the cells of a matrix in place
#'
#' Draw the values of a matrix as text labels, each at the centre of its cell
#' in the same layout used by [ximage()]. The first cell of the matrix is the
#' top-left label, and each row of the matrix is a line of labels reading
#' left to right down the page ("raster order").
#'
#' Text size is chosen automatically to fit the largest label within a cell,
#' scaled by 'fit' (use 'cex' to override). Labels that are NA are not drawn.
#'
#' Input may be a matrix (values are used as labels via [format()]), a
#' list in the style of vapour or gdalraster reader output (a row-major
#' vector with 'dimension'/'extent' attributes, or a 'gis' attribute), or an
#' array (one slice is labelled, see [as_ximage()]). With irregular or
#' curvilinear `coords` each label sits at the centre of its cell's corners.
#'
#' @param x a matrix of values to draw as labels, or a reader-output list
#' @param extent optional, numeric xmin,xmax,ymin,ymax (defaults to the
#'   0,ncol 0,nrow index space of the matrix)
#' @param add add to plot, or start afresh
#' @param cex text size, computed to fit the cells if not supplied
#' @param fit fraction of the cell the largest label should occupy when
#'   'cex' is computed, default 0.9
#' @param ... arguments passed to [text()], for example 'col'
#' @inheritParams as_ximage
#'
#' @return invisibly, a list with 'x', 'y' (label positions), 'labels', and
#'   'cex' as used
#' @export
#' @importFrom graphics text strwidth strheight
#' @examples
#' m <- matrix(1:12, 3, byrow = TRUE)
#' ximage(m)
#' xtext(m, add = TRUE)
#'
#' ## the label positions are cell centres in the extent
#' ximage(volcano, extent = c(0, 1, 0, 1))
#' xtext(volcano[seq(1, 87, by = 8), seq(1, 61, by = 6)],
#'       extent = c(0, 1, 0, 1), add = TRUE, col = "white")
xtext <- function(x, extent = NULL, add = FALSE, cex = NULL, fit = 0.9, ...,
                  slice = NULL, xy = NULL, coords = NULL) {
  UseMethod("xtext")
}

#' @export
xtext.default <- function(x, extent = NULL, add = FALSE, cex = NULL,
                          fit = 0.9, ..., slice = NULL, xy = NULL, coords = NULL) {
  .stop_if_bare(x)
  d <- as_ximage(x, extent = extent, slice = slice, xy = xy, coords = coords,
                 bands = FALSE,
                 verbose = getOption("ximage.verbose", TRUE) && !add)
  x <- d$data
  if (inherits(x, "nativeRaster")) stop("cannot label a nativeRaster", call. = FALSE)
  dm <- dim(x)
  if (is.null(dm) || length(dm) != 2L) {
    stop("'x' must be a matrix", call. = FALSE)
  }
  extent <- d$extent
  if (!add) {
    plot(extent[1:2], extent[3:4], type = "n", xlab = "", ylab = "",
         xaxs = "i", yaxs = "i")
  }
  if (is.null(d$corners)) {
    xres <- diff(extent[1:2]) / dm[2L]
    yres <- diff(extent[3:4]) / dm[1L]
    ## cell centres in raster order: row 1 of the matrix is the top row
    xs <- extent[1L] + (seq_len(dm[2L]) - 0.5) * xres
    ys <- extent[4L] - (seq_len(dm[1L]) - 0.5) * yres
    xx <- rep(xs, dm[1L])
    yy <- rep(ys, each = dm[2L])
  } else {
    cc <- .cell_centres(d)
    xx <- as.vector(t(cc$x))
    yy <- as.vector(t(cc$y))
    cx <- d$corners$x
    cy <- d$corners$y
    side <- function(a, b) stats::median(abs(a - b), na.rm = TRUE)
    nr <- nrow(cx)
    nc <- ncol(cx)
    xres <- side(cx[, -1L], cx[, -nc])
    yres <- side(cy[-1L, ], cy[-nr, ])
  }
  labs <- as.vector(t(format(x, trim = TRUE)))
  labs[as.vector(t(is.na(x)))] <- NA_character_

  if (is.null(cex)) {
    w <- suppressWarnings(
      max(graphics::strwidth(labs, units = "user", cex = 1), na.rm = TRUE))
    h <- suppressWarnings(
      max(graphics::strheight(labs, units = "user", cex = 1), na.rm = TRUE))
    cex <- if (is.finite(w) && is.finite(h) && w > 0 && h > 0 &&
               is.finite(xres) && is.finite(yres)) {
      fit * min(xres / w, yres / h)
    } else {
      1
    }
  }
  graphics::text(xx, yy, labels = labs, cex = cex, ...)
  invisible(list(x = xx, y = yy, labels = labs, cex = cex))
}
