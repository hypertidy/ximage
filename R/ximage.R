#' Plot an image
#'
#' ximage combines the best of `graphics::image()` and `graphics::rasterImage()`.
#'
#' [ximage()] is a combination those graphics function with the the best features in one.
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
#'    \item Draw one slice of arrays with more dimensions, such as Zarr and
#'    NetCDF reads from zaro, gdalraster `mdim_read()` or ndr.
#'    \item Place the image by coordinates, including irregular and
#'    curvilinear grids.
#' }
#'
#' Data orientation is "raster order", the first cell is the top-left of the
#' displayed image, following scan lines down the page (see the package
#' vignette on orientation).
#'
#' Arrays with more than two dimensions are drawn one slice at a time,
#' the spatial dimensions are found by name (or set with `xy`), the slice
#' with `slice`, and a message says which slice was drawn and how to get the
#' others. Coordinates from the input's metadata or from `coords` orient and
#' place the image, irregular or curvilinear coordinates are drawn cell by
#' cell. See [as_ximage()] for the details.
#'
#' Bare atomic vectors (no dim, no 'gis' attribute from gdalraster, no
#' width/height/depth attributes) are accepted and a dimension is guessed
#' by integer-division detection of the vector length. The most nearly
#' square factorization is chosen (preferring landscape, ncol >= nrow) and
#' the data is assumed to be in raster scanline order, i.e. flat output
#' from GDAL readers such as `vapour::gdal_raster_data()` or
#' `gdalraster::read_ds()`, built as `matrix(x, ncol = NC, byrow = TRUE)`;
#' column-major R data needs `dim(x) <- c(NR, NC)` instead. A message
#' reports the guess and the candidate factorizations from 1xN through
#' Nx1, in GDAL dimension order (ncol x nrow, xsize x ysize). Raw
#' vectors also consider 3-plane (RGB) and 4-plane (RGBA) scanline
#' pixel-interleaved interpretations; numeric data never gets a plane
#' interpretation (whole numbers in 0..255 are too common as ordinary
#' data to imply an image, a much narrower opt-in detection may come
#' later). For very long vectors (more than
#' `getOption("ximage.guess_max", 2^24)` elements) the guess stops with
#' an error unless `force = TRUE`, or set the dimension explicitly.
#'
#' Colour mapping via 'col', 'breaks', and 'zlim' applies to single-band
#' numeric data only. Multi-band (grey/alpha, RGB, RGBA) data is scaled
#' automatically: values within 0,1 are used as-is, within 0,255 are divided
#' by 255, and anything else is rescaled by the finite range of the colour
#' bands. Missing values (NA, NaN) display as 'na.col' in all cases.
#'
#' @param x matrix, array, raw or character matrix, native raster
#'   (nativeRaster, or raster), or list as output by GDAL reader functions,
#'   array from zaro, gdalraster `mdim_read()` or ndr, or atomic vector
#'   (guessing will apply)
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
#' @param force proceed with a guessed dimension for very long bare vectors
#'   (see Details), default `FALSE`
#' @inheritParams as_ximage
#'
#' @return invisibly, a list with 'x' (the colour data as plotted), 'extent'
#'   (xmin, xmax, ymin, ymax used, the 0,ncol 0,nrow index space of the input
#'   if not supplied), 'zlim' (the data range the colours were mapped to, use
#'   it to draw other slices with the same colours), and for arrays 'dims',
#'   'slice' and 'labels' (see [as_ximage()])
#' @export
#' @importFrom grDevices hcl.colors rgb col2rgb colorRampPalette
#' @importFrom graphics rasterImage
#' @examples
#' ximage(volcano)
#' ximage(as.raster(matrix(0:1, 49, 56)))
#' v <- volcano
#' v[v > 180] <- NA
#' ximage(v, na.col = "hotpink")
#'
#' ## bare vectors in GDAL scanline order get a guessed dimension, with a
#' ## message listing candidate shapes (ncol x nrow); here the guess 87x61
#' ## is the mirror of the true 61x87, so pick from the candidates
#' ximage(as.vector(t(volcano)))
#'
#' ## we don't get the right shape because our image is vertically tall
#' ## set manually
#' ximage(matrix(as.vector(t(volcano)), ncol = 61, byrow = TRUE))
#'
#' ## see that with the transpose form (landscape orientation) our guess is corrrect
#' ## but is that what was intended? (impossible to determine completely automatically)
#' ximage(as.vector(volcano))
#'
#' ## arrays: (lon, lat, time) as zaro::zaro_array() gives, latitude south
#' ## to north, the first slice is drawn north up with a message
#' a <- array(c(t(volcano[87:1, ]), t(volcano[87:1, ]) * 0.8),
#'            c(lon = 61, lat = 87, time = 2))
#' lonlat <- list(x = seq(140, 150, length.out = 61), y = seq(-45, -30, length.out = 87))
#' o <- ximage(a, coords = lonlat)
#' ximage(a, coords = lonlat, slice = list(time = 2), zlim = o$zlim)
#'
#' ## curvilinear coordinates, drawn cell by cell
#' lon <- outer(1:87, 1:61, function(i, j) j + i / 10)
#' lat <- outer(1:87, 1:61, function(i, j) -i + j / 20)
#' ximage(volcano, coords = list(x = lon, y = lat), asp = 1)
ximage <- function(x, extent = NULL, zlim = NULL, add = FALSE, ...,
                   xlab = NULL, ylab = NULL,
                   col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                   alpha = NULL, na.col = "transparent", force = FALSE,
                   slice = NULL, xy = NULL, coords = NULL) {
  UseMethod("ximage")
}

#' @export
ximage.default <- function(x, extent = NULL, zlim = NULL, add = FALSE, ...,
                           xlab = NULL, ylab = NULL,
                           col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                           alpha = NULL, na.col = "transparent", force = FALSE,
                           slice = NULL, xy = NULL, coords = NULL) {
  d <- as_ximage(x, extent = extent, slice = slice, xy = xy, coords = coords,
                 force = force)
  out <- .draw_image(d, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab,
                     col = col, breaks = breaks, alpha = alpha, na.col = na.col)
  invisible(out)
}

## draw decoded data (from as_ximage()), placed by extent or by corners
.draw_image <- function(d, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,
                        col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                        alpha = NULL, na.col = "transparent") {
  x <- d$data
  if (inherits(x, "nativeRaster")) {
    if (!is.null(alpha)) warning("'alpha' is not supported for nativeRaster input, ignored")
    hex <- x
    zl <- NULL
  } else {
    zl <- .zlim_of(x, zlim, breaks)
    ## single choke point: everything becomes a hex colour matrix
    hex <- to_hex(x, col = col, breaks = breaks, zlim = zlim,
                  alpha = alpha, na.col = na.col)
  }
  extent <- d$extent
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""
  if (!add) plot(extent[1:2], extent[3:4], type = "n", ..., xaxs = "i", yaxs = "i",
                 xlab = xlab, ylab = ylab)
  if (is.null(d$corners)) {
    graphics::rasterImage(hex, extent[1L], extent[3L], extent[2L], extent[4L],
                          interpolate = FALSE)
  } else {
    .draw_quads(d$corners, hex)
  }
  list(x = hex, extent = extent, zlim = zl, dims = d$dims, slice = d$slice,
       labels = d$labels)
}

## the data range colours are mapped to, single band data only
.zlim_of <- function(x, zlim = NULL, breaks = NULL) {
  if (!is.null(zlim)) return(range(zlim))
  if (!is.null(breaks)) return(range(breaks))
  dm <- dim(x)
  single <- length(dm) == 2L || (length(dm) == 3L && dm[3L] == 1L)
  if (!single || is.character(x)) return(NULL)
  v <- if (is.raw(x)) as.integer(x) else as.numeric(x)
  v <- v[is.finite(v)]
  if (!length(v)) NULL else range(v)
}

## curvilinear or irregular cells: each cell is a quad from the corner
## matrices ((nrow + 1) x (ncol + 1)), filled with its colour
.draw_quads <- function(corners, hex) {
  ny <- nrow(hex)
  nx <- ncol(hex)
  i <- rep(seq_len(nx), each = ny)
  j <- rep(seq_len(ny), nx)
  n1 <- ny + 1L
  idx <- cbind((i - 1L) * n1 + j, i * n1 + j, i * n1 + j + 1L, (i - 1L) * n1 + j + 1L)
  fill <- as.vector(hex)
  ## outlines in the fill colour close the hairline seams that antialiased
  ## devices leave between adjacent polygons
  .draw_faces(as.vector(corners$x), as.vector(corners$y), idx, fill = fill,
              border = fill, drop_transparent = TRUE)
}

## draw faces (rows of 'idx', vertex indices) as polygons in one call,
## dropping faces with a non-finite vertex
.draw_faces <- function(vx, vy, idx, fill = NA, border = NA, drop_transparent = FALSE) {
  if (!nrow(idx)) return(invisible(0L))
  k <- ncol(idx)
  fx <- matrix(vx[idx], ncol = k)
  fy <- matrix(vy[idx], ncol = k)
  ok <- rowSums(!is.finite(fx) | !is.finite(fy)) == 0L
  n <- nrow(idx)
  fill <- rep_len(fill, n)
  border <- rep_len(border, n)
  if (drop_transparent) {
    clear <- is.na(fill) | fill == "transparent" |
      (nchar(fill) == 9L & substr(fill, 8L, 9L) == "00")
    ok <- ok & !clear
  }
  if (!any(ok)) return(invisible(0L))
  px <- t(cbind(fx[ok, , drop = FALSE], NA))
  py <- t(cbind(fy[ok, , drop = FALSE], NA))
  graphics::polygon(as.vector(px), as.vector(py), col = fill[ok], border = border[ok])
  invisible(sum(ok))
}
