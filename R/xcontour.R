#' Contour matrix data and draw
#'
#' Like contour() but to work with [ximage()]
#'
#' Input may be anything [ximage()] accepts that holds data values: a matrix,
#' a list from `gdal_raster_data()` in the vapour package or from `read_ds()`
#' in the gdalraster package, or an array (one slice is drawn, see
#' [as_ximage()]).
#'
#' With irregular or curvilinear `coords` the contours are traced in the
#' index space of the matrix and placed by interpolating the cell centre
#' coordinates. Curvilinear contours are drawn with [graphics::lines()]
#' without labels ('levels' and 'nlevels' are used, other arguments go to
#' `lines()`).
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
#'
#' ## curvilinear
#' lon <- outer(1:87, 1:61, function(i, j) j + i / 10)
#' lat <- outer(1:87, 1:61, function(i, j) -i + j / 20)
#' ximage(volcano, coords = list(x = lon, y = lat), asp = 1)
#' xcontour(volcano, coords = list(x = lon, y = lat), add = TRUE)
xcontour <- function(x, extent = NULL, ..., add = FALSE, slice = NULL, xy = NULL,
                     coords = NULL) {
  UseMethod("xcontour")
}
#' @export
xcontour.default <- function(x, extent = NULL, ..., add = FALSE, slice = NULL,
                             xy = NULL, coords = NULL) {
  .stop_if_bare(x)
  d <- as_ximage(x, extent = extent, slice = slice, xy = xy, coords = coords,
                 bands = FALSE,
                 verbose = getOption("ximage.verbose", TRUE) && !add)
  m <- .value_matrix(d$data, "contour")
  if (is.null(d$corners)) {
    extent <- d$extent
    z <- t(m[nrow(m):1, , drop = FALSE])
    xre <- diff(extent[1:2]) / nrow(z)
    yre <- diff(extent[3:4]) / ncol(z)
    xx <- seq(extent[1] + xre / 2, extent[2] - xre / 2, length.out = nrow(z))
    yy <- seq(extent[3] + yre / 2, extent[4] - yre / 2, length.out = ncol(z))
    graphics::contour(xx, yy, z, add = add, ...)
  } else if (.is_rectilinear(d$corners)) {
    cc <- .cell_centres(d)
    graphics::contour(cc$x[1L, ], rev(cc$y[, 1L]), t(m[nrow(m):1, , drop = FALSE]),
                      add = add, ...)
  } else {
    .contour_curvilinear(m, d$corners, d$centres, add = add, ...)
  }
  invisible(NULL)
}

## bare vectors are guessed by ximage() but not by the adornments
.stop_if_bare <- function(x) {
  if (is.atomic(x) && is.null(dim(x)) && is.null(attr(x, "gis", exact = TRUE)) &&
      !all(c("width", "height", "depth") %in% names(attributes(x)))) {
    stop("'x' must be a matrix", call. = FALSE)
  }
  invisible(x)
}

## numeric single-band matrix from decoded data
.value_matrix <- function(m, what = "contour") {
  if (inherits(m, "nativeRaster") || is.character(m)) {
    stop(sprintf("cannot %s colour data", what), call. = FALSE)
  }
  if (length(dim(m)) != 2L) stop("'x' must be a matrix", call. = FALSE)
  if (is.raw(m)) m <- matrix(as.integer(m), nrow(m))
  m
}

## corners ((ny + 1) x (nx + 1)) of a grid with straight rows and columns
.is_rectilinear <- function(corners) {
  x <- corners$x
  y <- corners$y
  all(x == rep(x[1L, ], each = nrow(x))) && all(y == y[, 1L])
}

## cell centres (ny x nx), as given or from the corners of decoded data
.cell_centres <- function(d) {
  if (!is.null(d$centres)) return(d$centres)
  corners <- d$corners
  f <- function(m) {
    r <- nrow(m)
    k <- ncol(m)
    (m[-1L, -1L] + m[-r, -1L] + m[-1L, -k] + m[-r, -k]) / 4
  }
  list(x = f(corners$x), y = f(corners$y))
}

## bilinear interpolation of matrix M at fractional (row, col)
.bilinear <- function(M, r, u) {
  nr <- nrow(M)
  nc <- ncol(M)
  r0 <- pmin(pmax(floor(r), 1), nr - 1)
  u0 <- pmin(pmax(floor(u), 1), nc - 1)
  fr <- r - r0
  fu <- u - u0
  M[cbind(r0, u0)] * (1 - fr) * (1 - fu) + M[cbind(r0, u0 + 1)] * (1 - fr) * fu +
    M[cbind(r0 + 1, u0)] * fr * (1 - fu) + M[cbind(r0 + 1, u0 + 1)] * fr * fu
}

.contour_curvilinear <- function(m, corners, centres = NULL, add = FALSE, ...) {
  args <- list(...)
  ny <- nrow(m)
  nx <- ncol(m)
  if (nx < 2L || ny < 2L) stop("need at least 2 x 2 cells to contour", call. = FALSE)
  lev <- args$levels
  if (is.null(lev)) {
    nlev <- if (is.null(args$nlevels)) 10 else args$nlevels
    lev <- pretty(range(m, finite = TRUE), nlev)
  }
  ## contourLines() wants z[x, y] with y increasing: reverse the rows
  cl <- grDevices::contourLines(seq_len(nx), seq_len(ny),
                                t(m[ny:1, , drop = FALSE]), levels = lev)
  cc <- .cell_centres(list(corners = corners, centres = centres))
  plot_args <- c("asp", "main", "sub", "xlim", "ylim", "axes", "frame.plot", "xlab", "ylab")
  drop <- c("levels", "nlevels", "labels", "labcex", "drawlabels", "method",
            "vfont", plot_args)
  if (!add) {
    pa <- args[intersect(names(args), plot_args)]
    if (is.null(pa$xlab)) pa$xlab <- ""
    if (is.null(pa$ylab)) pa$ylab <- ""
    do.call(graphics::plot, c(list(x = range(corners$x, finite = TRUE),
                                   y = range(corners$y, finite = TRUE),
                                   type = "n", xaxs = "i", yaxs = "i"), pa))
  }
  la <- args[setdiff(names(args), drop)]
  for (l in cl) {
    r <- ny + 1 - l$y
    do.call(graphics::lines, c(list(x = .bilinear(cc$x, r, l$x),
                                    y = .bilinear(cc$y, r, l$x)), la))
  }
  invisible(cl)
}
