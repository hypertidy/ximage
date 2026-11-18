#' Decode input to an image in raster order
#'
#' `as_ximage()` is the shared input decoder behind [ximage()], [xcontour()]
#' and [xtext()]. It turns any supported input into a matrix (or
#' a colour array) in raster order, with its placement, and for arrays with
#' more than two dimensions it chooses the slice to draw.
#'
#' @section Arrays and slices:
#'
#' An array with more than two dimensions (or two named dimensions) is drawn
#' one 2D slice at a time. The two spatial dimensions are found in this order:
#' \enumerate{
#'   \item the `xy` argument, dimension names or positions, x first
#'   \item dimension names, from `names(dim(x))` (as `zaro::zaro_read()`
#'   gives) or `names(dimnames(x))` (as `zaro::zaro_array()` gives), or the
#'   `gis` attribute of `gdalraster::mdim_read()` output, matched against
#'   common names ("x", "lon", "longitude", "nav_lon", "xi_rho", ...)
#'   \item position: the first two dimensions, as rows (y) then columns (x)
#'   for an R array, or as x then y for GDAL reader output
#' }
#'
#' Every other dimension takes the index given by `slice`, or 1. A message
#' reports the slice drawn and how to ask for the others. Set
#' `options(ximage.verbose = FALSE)` to silence it. Selecting 3 or 4 indices
#' along one dimension draws them as an RGB(A) composite, e.g.
#' `slice = list(band = c(4, 3, 2))`. A dimension named like a band ("band",
#' "channel", "rgb", ...) with 3 or 4 entries is drawn as a composite by
#' default, and so is the third dimension of an unnamed 3D array with 2 to 4
#' entries (grey-alpha, RGB, RGBA, unchanged from earlier versions).
#'
#' Only the selected slice is read, by `[`, so lazy arrays (such as
#' `zaro::zaro_array()` output) fetch just the chunks that slice needs.
#'
#' @section Coordinates and orientation:
#'
#' The drawn slice is put in raster order: first row at the top. When the y
#' coordinates are known and increase with the row index (most CF, NetCDF and
#' Zarr data, latitude south to north), the rows are reversed so that north
#' is up, and decreasing x coordinates reverse the columns.
#'
#' Coordinates come from the input's metadata (the `gis` attribute of
#' gdalraster multidimensional reads, ndr coordinates and affine indexes) or
#' from the `coords` argument, `list(x = , y = )`:
#' \itemize{
#'   \item a vector with one value per cell gives cell centres
#'   \item a vector with one more value than cells gives cell edges (as CF
#'   bounds do)
#'   \item matrices give curvilinear coordinates, cell centres (one per
#'   cell) or corners (one more in each direction), in the same layout as the
#'   data
#' }
#' Regularly spaced coordinates give an extent and the image is drawn with
#' [graphics::rasterImage()]. Irregular spacing, curvilinear coordinates and
#' rotated or sheared affine transforms are drawn cell by cell as quads.
#' Corners are found from centres by averaging the four
#' neighbours and extrapolating at the edges (longitudes that wrap need to be
#' made continuous first).
#'
#' Physical values: scale and offset in gdalraster multidimensional output
#' are applied. Values of magnitude 1e30 or more are reported as a likely
#' fill value, set them to `NA` (`zaro::zaro_array(cf = TRUE)` does this).
#'
#' @param x input data, anything [ximage()] accepts
#' @param extent optional, numeric xmin,xmax,ymin,ymax, overrides any
#'   placement from the input or from `coords`
#' @param ... arguments for methods
#' @param slice index of the slice to draw for arrays with more than two
#'   dimensions: a named list or vector, e.g. `list(time = 3, depth = 1)`, or an
#'   unnamed value for the first non-spatial dimension. Default is the first
#'   slice
#' @param xy the two spatial dimensions, x then y, as names or positions
#' @param coords optional coordinates of the spatial dimensions,
#'   `list(x = , y = )` of centres, edges, or curvilinear matrices (see
#'   Details)
#' @param force proceed with a guessed dimension for very long bare vectors
#'   (see [ximage()])
#' @param bands allow a band-like dimension to be drawn as a colour composite,
#'   default `TRUE` ([xcontour()] and [xtext()] use `FALSE`)
#' @param verbose report the slice drawn and possible fill values, default
#'   `getOption("ximage.verbose", TRUE)`
#'
#' @return a list of class "ximage_data" with
#' \describe{
#'   \item{data}{the matrix (or nrow x ncol x bands array, nativeRaster, or
#'   colour matrix) in raster order}
#'   \item{extent}{xmin, xmax, ymin, ymax}
#'   \item{corners}{`NULL`, or `list(x, y)` of (nrow + 1) x (ncol + 1) cell
#'   corner matrices for data that can't be placed by an extent}
#'   \item{centres}{`NULL`, or `list(x, y)` of nrow x ncol cell centre
#'   matrices when they were given as coordinates (with corners)}
#'   \item{dims}{named dimension sizes of the input array}
#'   \item{xy}{names of the x and y dimensions}
#'   \item{slice}{named list of the indices drawn for the other dimensions}
#'   \item{labels}{named character, the coordinate value of each slice index
#'   where known}
#' }
#' @export
#' @examples
#' ## an array (lat, lon, time) with time last, lat south to north
#' a <- array(rep(volcano[87:1, ], 3) * rep(1:3, each = length(volcano)),
#'            c(lat = 87, lon = 61, time = 3))
#' d <- as_ximage(a, coords = list(x = 100 + 1:61, y = -60 + 1:87))
#' d$extent
#' ximage(a, slice = list(time = 2), coords = list(x = 100 + 1:61, y = -60 + 1:87))
as_ximage <- function(x, extent = NULL, ..., slice = NULL, xy = NULL,
                      coords = NULL, force = FALSE, bands = TRUE,
                      verbose = getOption("ximage.verbose", TRUE)) {
  UseMethod("as_ximage")
}

#' @export
as_ximage.ximage_data <- function(x, extent = NULL, ..., slice = NULL, xy = NULL,
                                  coords = NULL, force = FALSE, bands = TRUE,
                                  verbose = getOption("ximage.verbose", TRUE)) {
  if (!is.null(extent)) x$extent <- extent
  x
}

#' @export
as_ximage.default <- function(x, extent = NULL, ..., slice = NULL, xy = NULL,
                              coords = NULL, force = FALSE, bands = TRUE,
                              verbose = getOption("ximage.verbose", TRUE)) {
  if (is.list(extent) && length(extent) == 2L) {
    ## curvilinear 'extent' (list of x, y arrays) is coordinates
    if (is.null(coords)) coords <- extent
    extent <- NULL
  }
  src <- .source_of(x, force = force)
  if (src$kind == "image") {
    out <- .image_data(src$data, extent = if (is.null(extent)) src$extent else extent)
  } else {
    out <- .resolve_nd(src$nd, slice = slice, xy = xy, coords = coords,
                       extent = extent, bands = bands, verbose = verbose)
  }
  out
}

#' @export
print.ximage_data <- function(x, ...) {
  dm <- dim(x$data)
  cat("<ximage_data>", paste(dm, collapse = " x "),
      if (inherits(x$data, "nativeRaster")) "nativeRaster" else typeof(x$data), "\n")
  if (!is.null(x$dims) && length(x$dims) > 2L) {
    cat("  dims:   ", paste(sprintf("%s: %d", names(x$dims), x$dims), collapse = ", "), "\n")
  }
  if (!is.null(x$xy)) cat("  xy:     ", x$xy[["x"]], "(x) by", x$xy[["y"]], "(y)\n")
  if (length(x$slice)) {
    cat("  slice:  ", .format_slice(x$slice, x$labels), "\n")
  }
  if (is.null(x$corners)) {
    cat("  extent: ", paste(format(x$extent), collapse = ", "), "\n")
  } else {
    cat("  corners:", paste(dim(x$corners$x), collapse = " x "),
        "(curvilinear), range", paste(format(x$extent), collapse = ", "), "\n")
  }
  invisible(x)
}

## -------------------------------------------------------------------------
## building the result
## -------------------------------------------------------------------------

.ximage_data <- function(data, extent, corners = NULL, dims = NULL, xy = NULL,
                         slice = NULL, labels = NULL) {
  structure(list(data = data, extent = extent, corners = corners, dims = dims,
                 xy = xy, slice = slice, labels = labels),
            class = "ximage_data")
}

## data that is already an image in raster order (nativeRaster, colours)
.image_data <- function(data, extent = NULL) {
  if (is.null(extent)) extent <- c(0, dim(data)[2L], 0, dim(data)[1L])
  .ximage_data(data, extent)
}

## -------------------------------------------------------------------------
## sources: what kind of input is this
## -------------------------------------------------------------------------

## returns list(kind = "image", data, extent) for inputs already in display
## form, or list(kind = "nd", nd) for (possibly lazy) arrays to slice
.source_of <- function(x, force = FALSE) {
  if (inherits(x, "nativeRaster")) {
    return(list(kind = "image", data = x, extent = NULL))
  }
  if (inherits(x, "raster")) {
    return(list(kind = "image",
                data = matrix(as.character(x), dim(x)[1L], dim(x)[2L]),
                extent = NULL))
  }
  if (inherits(x, "ndr::Dataset")) {
    stop("a Dataset holds several variables, draw one of them, e.g. ds$name or ds[[\"name\"]]",
         call. = FALSE)
  }
  if (inherits(x, c("ndr::DataArray", "ndr::Variable"))) {
    return(list(kind = "nd", nd = .nd_ndr(x)))
  }
  if (is.list(x)) {
    return(.source_list(x))
  }
  if (!is.atomic(x) || is.null(x)) {
    stop(sprintf("cannot draw an object of class '%s'", class(x)[1L]), call. = FALSE)
  }
  gis <- attr(x, "gis", exact = TRUE)
  if (!is.null(gis)) {
    return(list(kind = "nd", nd = .nd_gis(x, gis)))
  }
  x <- .unpack_whd(x)
  if (is.null(dim(x))) {
    x <- .ximage_guess(x, force = force)
  }
  if (length(dim(x)) < 2L) {
    stop("'x' must have at least two dimensions", call. = FALSE)
  }
  list(kind = "nd", nd = .nd_array(x))
}

## a plain (possibly lazy) R array, R orientation: rows are y
.nd_array <- function(x) {
  d <- dim(x)
  nm <- names(d)
  if (is.null(nm)) nm <- names(dimnames(x))
  named <- !is.null(nm) && all(nzchar(nm)) && !anyDuplicated(nm)
  if (!named) nm <- paste0("dim", seq_along(d))
  .nd(d, nm, named = named, layout = "yx",
      extract = function(idx) .extract_array(x, idx), data = x)
}

## extract by '[' with empty arguments for whole dimensions, drop = FALSE,
## so lazy arrays plan only the chunks they need
.extract_array <- function(x, idx) {
  args <- lapply(idx, function(i) if (is.null(i)) quote(expr = ) else i)
  out <- do.call(`[`, c(list(x), args, list(drop = FALSE)))
  dim(out) <- unname(dim(out))
  dimnames(out) <- NULL
  out
}

## gdalraster read_ds() (classic) and mdim_read() (multidim) vectors
.nd_gis <- function(x, gis) {
  d <- as.integer(gis$dim)
  bb <- gis$bbox
  ext <- if (length(bb) == 4L && all(is.finite(bb))) bb[c(1L, 3L, 2L, 4L)] else NULL
  vals <- function() {
    v <- x
    attributes(v) <- NULL
    dim(v) <- d
    v
  }
  if (identical(gis$type, "multidim")) {
    nm <- gis$dim_names
    named <- !is.null(nm) && length(nm) == length(d) && all(nzchar(nm)) &&
      !anyDuplicated(nm)
    if (!named) nm <- paste0("dim", seq_along(d))
    crd <- gis$coords
    if (!is.null(crd)) crd <- unname(crd)[seq_along(d)]
    units <- gis$dim_units
    sc <- if (is.null(gis$scale)) NA_real_ else gis$scale
    of <- if (is.null(gis$offset)) NA_real_ else gis$offset
    return(.nd(d, nm, named = named, layout = "xy",
               extract = function(idx) .extract_array(vals(), idx),
               coords = crd, units = units, calendar = gis$calendar,
               extent = ext, scale = sc, offset = of))
  }
  ## classic raster: (xsize, ysize, nbands)
  if (length(d) == 2L) d <- c(d, 1L)
  .nd(d, c("x", "y", "band"), named = TRUE, layout = "xy",
      extract = function(idx) .extract_array(vals(), idx), extent = ext)
}

## lists: vapour gdal_raster_*(), gdalraster read_ds(as_list = TRUE), sf
## gdal_read(), and lists of R matrices
.source_list <- function(x) {
  if (all(c("geotransform", "cols", "rows", "driver") %in% names(x))) {
    ## smells like sf
    return(.source_sf(x))
  }
  if (!length(x)) stop("no dimension known", call. = FALSE)
  attrs <- attributes(x)
  if ("gis" %in% names(attrs)) {
    ## gdalraster output
    attrs <- attrs[["gis"]]
    attrs$dimension <- attrs$dim
    attrs$extent <- attrs$bbox[c(1, 3, 2, 4)]
  }
  extent <- attrs$extent
  dimension <- attrs$dimension
  ## a dimension attribute (vapour, gdalraster gis) means the elements hold
  ## GDAL row-major data, whether stored as vectors or as (ncol, nrow)
  ## matrices (e.g. from asplit()); with no dimension attribute a matrix
  ## element is taken to be an already-oriented R matrix
  row_major <- !is.null(dimension)
  if (is.null(dimension)) {
    dm1 <- dim(x[[1L]])
    if (is.null(dm1) || length(dm1) < 2L) stop("no dimension known", call. = FALSE)
  }
  first <- x[[1L]]
  if (inherits(first, "nativeRaster")) {
    return(list(kind = "image", data = first, extent = extent))
  }
  if (is.character(first)) {
    f1 <- stats::na.omit(first)
    if (!(length(f1) && (grepl("^#", f1[1L]) || f1[1L] %in% grDevices::colours()))) {
      stop("can't read data in this package", call. = FALSE)
    }
    ## colour data, one band
    m <- if (row_major) matrix(as.vector(first), dimension[2L], byrow = TRUE) else first
    return(list(kind = "image", data = m, extent = extent))
  }
  nb <- length(x)
  if (row_major) {
    d <- c(as.integer(dimension[1:2]), nb)
    a <- array(unlist(lapply(x, as.vector), use.names = FALSE), d)
    nd <- .nd(d, c("x", "y", "band"), named = TRUE, layout = "xy",
              extract = function(idx) .extract_array(a, idx), extent = extent)
  } else {
    d <- c(dim(first)[1:2], nb)
    a <- array(unlist(x, use.names = FALSE), d)
    nd <- .nd(d, c("y", "x", "band"), named = TRUE, layout = "yx",
              extract = function(idx) .extract_array(a, idx), extent = extent)
  }
  list(kind = "nd", nd = nd)
}

.gt_dim_to_extent <- function(x, dim) {
  xx <- c(x[1], x[1] + dim[1] * x[2])
  yy <- c(x[4] + dim[2] * x[6], x[4])
  c(xx, yy)
}

.source_sf <- function(x) {
  d <- attr(x, "data")
  if (is.null(d)) stop("no data in sf read object", call. = FALSE)
  dm <- dim(d)
  extent <- NULL
  if (is.null(dm) || length(dm) < 2) {
    d <- matrix(d)
  } else {
    if (length(dm) > 2) d <- matrix(d[seq_len(prod(dm[1:2]))], dm[1L], dm[2L])
    ## sf gdal_read doesn't update the geotransform offsets so do that here
    gt <- x$geotransform
    if (x$cols[1] > 1) gt[1] <- gt[1] + gt[2] * (x$cols[1] - 1)
    if (x$rows[1] > 1) gt[4] <- gt[4] + gt[6] * (x$rows[1] - 1)
    extent <- .gt_dim_to_extent(gt, dim(d))
  }
  m <- t(d)
  list(kind = "nd", nd = .nd(dim(m), c("y", "x"), named = TRUE, layout = "yx",
                             extract = function(idx) .extract_array(m, idx),
                             extent = extent, data = m))
}

## ndr DataArray or Variable: slice with ndr::isel() so lazy data stays lazy
.nd_ndr <- function(x) {
  if (!requireNamespace("ndr", quietly = TRUE)) {
    stop("the ndr package is needed to draw an ndr object", call. = FALSE)
  }
  shp <- ndr::shape(x)
  nm <- names(shp)
  d <- as.integer(shp)
  crd <- vector("list", length(d))
  transform <- NULL
  if (inherits(x, "ndr::DataArray")) {
    for (idx in attr(x, "coords")) {
      if (inherits(idx, "ndr::AffineIndex")) {
        transform <- list(gt = as.numeric(ndr::as_geotransform(idx)),
                          dims = attr(idx, "dims"))
      } else if (inherits(idx, c("ndr::ImplicitCoord", "ndr::ExplicitCoord"))) {
        k <- match(ndr::coord_dim(idx), nm)
        if (!is.na(k)) crd[k] <- list(ndr::coord_values(idx))
      }
    }
  }
  extract <- function(idx) {
    sel <- list()
    for (k in seq_along(idx)) if (!is.null(idx[[k]])) sel[[nm[k]]] <- idx[[k]]
    y <- if (length(sel)) do.call(ndr::isel, c(list(x), sel)) else x
    a <- as.array(y)
    ## isel() drops dimensions selected to a scalar, the rest keep their order
    dim(a) <- vapply(seq_along(idx), function(k)
      if (is.null(idx[[k]])) d[k] else length(idx[[k]]), integer(1L))
    a
  }
  .nd(d, nm, named = TRUE, layout = "yx", extract = extract, coords = crd,
      transform = transform)
}

## the description of an n-dimensional source
.nd <- function(dim, names, named, layout, extract, coords = NULL, units = NULL,
                calendar = NA, extent = NULL, transform = NULL, scale = NA_real_,
                offset = NA_real_, data = NULL) {
  dim <- as.integer(dim)
  names(dim) <- names
  if (is.null(coords)) coords <- vector("list", length(dim))
  list(dim = dim, named = named, layout = layout, extract = extract,
       coords = coords, units = units, calendar = calendar, extent = extent,
       transform = transform, scale = scale, offset = offset, data = data)
}

## -------------------------------------------------------------------------
## resolving: choose x, y and the slice, extract, orient, place
## -------------------------------------------------------------------------

.x_names <- c("x", "lon", "long", "longitude", "nav_lon", "rlon", "xc",
              "xi_rho", "xi_u", "xi_v", "xi_psi", "easting",
              "projection_x_coordinate", "grid_xt", "nlon", "ni", "west_east")
.y_names <- c("y", "lat", "latitude", "nav_lat", "rlat", "yc",
              "eta_rho", "eta_u", "eta_v", "eta_psi", "northing",
              "projection_y_coordinate", "grid_yt", "nlat", "nj", "south_north")
.band_names <- c("band", "bands", "channel", "channels", "rgb", "rgba")

.find_xy <- function(nd, xy = NULL, verbose = TRUE) {
  nm <- names(nd$dim)
  n <- length(nm)
  if (n < 2L) stop("'x' must have at least two dimensions", call. = FALSE)
  if (!is.null(xy)) {
    if (length(xy) != 2L) stop("'xy' must give two dimensions, x then y", call. = FALSE)
    i <- if (is.character(xy)) match(xy, nm) else as.integer(xy)
    if (anyNA(i) || any(i < 1L | i > n) || i[1L] == i[2L]) {
      stop(sprintf("'xy' must name two different dimensions of: %s",
                   paste(nm, collapse = ", ")), call. = FALSE)
    }
    return(c(x = i[1L], y = i[2L]))
  }
  if (nd$named) {
    lnm <- tolower(nm)
    ix <- which(lnm %in% .x_names)
    iy <- which(lnm %in% .y_names)
    if (length(ix) == 1L && length(iy) == 1L) return(c(x = ix, y = iy))
  }
  out <- if (nd$layout == "xy") c(x = 1L, y = 2L) else c(x = 2L, y = 1L)
  if (nd$named && n > 2L && verbose) {
    message(sprintf(paste0(
      "dimension names not recognised as x and y, drawing '%s' (x) by '%s' (y)\n",
      "  set 'xy' to choose, e.g. xy = c(\"%s\", \"%s\")"),
      nm[out[["x"]]], nm[out[["y"]]], nm[out[["x"]]], nm[out[["y"]]]))
  }
  out
}

## the index for each non-spatial dimension: list named by dimension
.parse_slice <- function(slice, nd, other) {
  nm <- names(nd$dim)
  onm <- nm[other]
  out <- rep(list(1L), length(other))
  names(out) <- onm
  if (is.null(slice) || !length(other)) {
    if (!is.null(slice) && length(slice) && !all(slice == 1L)) {
      stop("'slice' given, but there are no dimensions to slice", call. = FALSE)
    }
    return(out)
  }
  if (is.data.frame(slice)) {
    if (nrow(slice) != 1L) stop("'slice' as a data frame must have one row", call. = FALSE)
    slice <- as.list(slice)[intersect(names(slice), onm)]
  }
  if (is.list(slice) || !is.null(names(slice))) {
    slice <- as.list(slice)
    snm <- names(slice)
    if (is.null(snm) || !all(nzchar(snm))) {
      stop("a 'slice' list must be named by dimension", call. = FALSE)
    }
    bad <- setdiff(snm, onm)
    if (length(bad)) {
      stop(sprintf("'slice' names %s, the dimensions to slice are: %s",
                   paste(bad, collapse = ", "), paste(onm, collapse = ", ")),
           call. = FALSE)
    }
    out[snm] <- slice
  } else if (length(other) == 1L || length(slice) == 1L) {
    out[[1L]] <- slice
  } else if (length(slice) == length(other)) {
    out[] <- as.list(slice)
  } else {
    stop(sprintf(paste0("an unnamed 'slice' needs one index per dimension to slice (%s), ",
                        "or name them, e.g. slice = list(%s = 1)"),
                 paste(onm, collapse = ", "), onm[1L]), call. = FALSE)
  }
  for (k in names(out)) {
    i <- out[[k]]
    n <- nd$dim[[k]]
    if (!is.numeric(i) || !length(i) || anyNA(i) || any(i != round(i)) ||
        any(i < 1 | i > n)) {
      stop(sprintf("'slice' for '%s' must be whole numbers in 1..%d", k, n), call. = FALSE)
    }
    out[[k]] <- as.integer(i)
  }
  out
}

.resolve_nd <- function(nd, slice = NULL, xy = NULL, coords = NULL, extent = NULL,
                        bands = TRUE, verbose = TRUE) {
  verbose <- isTRUE(verbose)
  dm <- nd$dim
  nm <- names(dm)
  sp <- .find_xy(nd, xy, verbose = verbose)
  ix <- sp[["x"]]
  iy <- sp[["y"]]
  other <- setdiff(seq_along(dm), c(ix, iy))
  sel <- .parse_slice(slice, nd, other)

  ## composite: one dimension with 2 to 4 indices drawn as colour bands
  ib <- integer(0)
  multi <- names(sel)[lengths(sel) > 1L]
  if (length(multi) > 1L) {
    stop(sprintf("only one dimension can have several indices in 'slice' (a colour composite), got: %s",
                 paste(multi, collapse = ", ")), call. = FALSE)
  }
  if (length(multi)) {
    if (!bands) stop("'slice' must give a single index for each dimension here", call. = FALSE)
    if (!length(sel[[multi]]) %in% c(3L, 4L)) {
      stop("a colour composite takes 3 (RGB) or 4 (RGBA) indices", call. = FALSE)
    }
    ib <- match(multi, nm)
  } else if (bands && is.null(slice)) {
    if (nd$named) {
      cand <- other[tolower(nm[other]) %in% .band_names & dm[other] %in% c(3L, 4L)]
    } else {
      cand <- other[length(dm) == 3L & other == 3L & dm[other] %in% 2:4]
    }
    if (length(cand) == 1L) {
      ib <- cand
      sel[[nm[ib]]] <- seq_len(dm[ib])
    }
  }

  ## extract only the selected slice
  idx <- vector("list", length(dm))
  for (k in other) idx[[k]] <- sel[[nm[k]]]
  if (!length(other) && ix == 2L && iy == 1L && !is.null(nd$data)) {
    a <- nd$data  ## a plain matrix, as is
  } else {
    a <- nd$extract(idx)
    keep <- c(iy, ix, ib)
    ord <- sort(keep)
    a <- array(a, dim(a)[ord])
    perm <- match(keep, ord)
    if (!identical(perm, seq_along(perm))) a <- aperm(a, perm)
  }

  ## physical values (gdalraster multidim leaves these to the caller)
  if (is.numeric(a)) {
    if (is.finite(nd$scale) && nd$scale != 1) a <- a * nd$scale
    if (is.finite(nd$offset) && nd$offset != 0) a <- a + nd$offset
  }

  ## coordinates of x and y: user, then the source's transform or coords
  ny <- dm[[iy]]
  nx <- dm[[ix]]
  cx <- nd$coords[[ix]]
  cy <- nd$coords[[iy]]
  if (!is.numeric(cx)) cx <- NULL
  if (!is.numeric(cy)) cy <- NULL
  corners <- NULL
  centres <- NULL
  if (is.null(coords) && !is.null(nd$transform)) {
    tr <- .transform_coords(nd$transform, nm[ix], nm[iy], nx, ny, ix < iy)
    if (is.list(tr) && !is.null(tr$corners)) {
      corners <- tr$corners
    } else if (!is.null(tr)) {
      cx <- tr$x
      cy <- tr$y
    }
  }
  if (!is.null(coords)) {
    uc <- .user_coords(coords, nx, ny, x_first = ix < iy)
    if (!is.null(uc$corners)) {
      corners <- uc$corners
      centres <- uc$centres
    } else {
      if (!is.null(uc$x)) cx <- uc$x
      if (!is.null(uc$y)) cy <- uc$y
    }
  }

  if (!is.null(corners)) {
    if (!is.null(extent)) warning("'extent' is ignored with curvilinear coordinates")
    rg <- function(v) range(v, finite = TRUE)
    out_extent <- c(rg(corners$x), rg(corners$y))
  } else {
    ex <- .axis_edges(cx, nx, "x")
    ey <- .axis_edges(cy, ny, "y")
    ## fill in a missing step from the other axis for single row/column data
    if (is.null(ex$edges) && !is.null(cx) && !is.null(ey$step)) ex <- .axis_edges(cx, nx, "x", ey$step)
    if (is.null(ey$edges) && !is.null(cy) && !is.null(ex$step)) ey <- .axis_edges(cy, ny, "y", ex$step)
    xe <- ex$edges
    ye <- ey$edges
    ## centres as given (else midway between edges), for labels and contours
    xc <- if (length(cx) == nx) as.numeric(cx)
    yc <- if (length(cy) == ny) as.numeric(cy)
    ## orientation: x increasing to the right, y decreasing down the rows
    if (!is.null(xe) && xe[1L] > xe[length(xe)]) {
      a <- .flip(a, cols = TRUE)
      xe <- rev(xe)
      xc <- rev(xc)
    }
    if (!is.null(ye) && ye[1L] < ye[length(ye)]) {
      a <- .flip(a, rows = TRUE)
      ye <- rev(ye)
      yc <- rev(yc)
    }
    if (!is.null(extent)) {
      out_extent <- extent
    } else if (is.null(xe) && is.null(ye)) {
      out_extent <- if (is.null(nd$extent)) c(0, nx, 0, ny) else nd$extent
    } else {
      if (is.null(xe)) xe <- if (is.null(nd$extent)) 0:nx else seq(nd$extent[1L], nd$extent[2L], length.out = nx + 1L)
      if (is.null(ye)) ye <- if (is.null(nd$extent)) ny:0 else seq(nd$extent[4L], nd$extent[3L], length.out = ny + 1L)
      if (isTRUE(ex$regular %in% c(TRUE, NA)) && isTRUE(ey$regular %in% c(TRUE, NA))) {
        out_extent <- c(xe[1L], xe[nx + 1L], ye[ny + 1L], ye[1L])
      } else {
        ## irregular spacing: rectilinear cells drawn as quads
        corners <- list(x = matrix(xe, ny + 1L, nx + 1L, byrow = TRUE),
                        y = matrix(ye, ny + 1L, nx + 1L))
        if (is.null(xc)) xc <- (xe[-1L] + xe[-(nx + 1L)]) / 2
        if (is.null(yc)) yc <- (ye[-1L] + ye[-(ny + 1L)]) / 2
        centres <- list(x = matrix(xc, ny, nx, byrow = TRUE), y = matrix(yc, ny, nx))
        out_extent <- c(range(xe), range(ye))
      }
    }
  }

  dims <- dm
  labels <- .slice_labels(nd, sel)
  out <- .ximage_data(a, out_extent, corners = corners, dims = dims,
                      xy = c(x = nm[ix], y = nm[iy]), slice = sel, labels = labels)
  out$centres <- centres
  if (verbose) .report(out, nd, sel, slice_given = !is.null(slice), ib = ib)
  out
}

## flip rows and/or columns of a matrix or nrow x ncol x bands array
.flip <- function(a, rows = FALSE, cols = FALSE) {
  d <- dim(a)
  i <- if (rows) d[1L]:1L else seq_len(d[1L])
  j <- if (cols) d[2L]:1L else seq_len(d[2L])
  if (length(d) == 2L) a[i, j, drop = FALSE] else a[i, j, , drop = FALSE]
}

## 1D coordinates to cell edges, in index order; centres (n values) or
## edges (n + 1 values). list(edges, regular, step), edges NULL if unknown
.axis_edges <- function(v, n, axis = "x", step = NULL) {
  if (is.null(v)) return(list(edges = NULL, regular = NA, step = NULL))
  v <- as.numeric(v)
  if (!length(v) %in% c(n, n + 1L)) {
    stop(sprintf("%s coordinates must have %d (centres) or %d (edges) values, got %d",
                 axis, n, n + 1L, length(v)), call. = FALSE)
  }
  if (anyNA(v) || !all(is.finite(v))) {
    stop(sprintf("%s coordinates must be finite", axis), call. = FALSE)
  }
  if (length(v) == 1L) {
    ## a single centre: needs a step from elsewhere
    if (is.null(step)) return(list(edges = NULL, regular = NA, step = NULL))
    s <- abs(step)
    return(list(edges = v + c(-s, s) / 2, regular = TRUE, step = s))
  }
  dv <- diff(v)
  if (!(all(dv > 0) || all(dv < 0))) {
    stop(sprintf("%s coordinates must be strictly increasing or decreasing, for curvilinear coordinates give matrices", axis),
         call. = FALSE)
  }
  s <- mean(dv)
  regular <- all(abs(dv - s) <= 1e-5 * abs(s))
  if (length(v) == n + 1L) {
    edges <- v
  } else if (regular) {
    edges <- v[1L] - s / 2 + s * (0:n)
  } else {
    edges <- c(v[1L] - dv[1L] / 2, (v[-1L] + v[-n]) / 2, v[n] + dv[n - 1L] / 2)
  }
  list(edges = edges, regular = regular, step = abs(s))
}

## user 'coords': list(x, y) of 1D vectors or 2D matrices
.user_coords <- function(coords, nx, ny, x_first = FALSE) {
  if (!is.list(coords) || length(coords) != 2L) {
    stop("'coords' must be a list of two, x and y", call. = FALSE)
  }
  if (!is.null(names(coords)) && all(c("x", "y") %in% names(coords))) {
    coords <- coords[c("x", "y")]
  }
  cx <- coords[[1L]]
  cy <- coords[[2L]]
  is2 <- function(v) !is.null(dim(v)) && length(dim(v)) == 2L &&
    min(dim(v)) > 1L
  if (is2(cx) || is2(cy)) {
    if (!(is2(cx) && is2(cy))) stop("curvilinear 'coords' need x and y matrices", call. = FALSE)
    mx <- .orient_2d(cx, nx, ny, x_first, "x")
    my <- .orient_2d(cy, nx, ny, x_first, "y")
    if (!identical(dim(mx), dim(my))) {
      stop("x and y coordinate matrices must have the same shape", call. = FALSE)
    }
    if (all(dim(mx) == c(ny, nx))) {
      return(list(corners = list(x = .corners_from_centres(mx),
                                 y = .corners_from_centres(my)),
                  centres = list(x = mx, y = my)))
    }
    return(list(corners = list(x = mx, y = my)))
  }
  list(x = if (is.null(cx)) NULL else as.vector(cx),
       y = if (is.null(cy)) NULL else as.vector(cy))
}

## a 2D coordinate matrix in the layout of the data's storage to
## (rows = y, cols = x), centres (ny, nx) or corners (ny + 1, nx + 1)
.orient_2d <- function(m, nx, ny, x_first, axis) {
  m <- matrix(as.numeric(m), nrow(m), ncol(m))
  ok_yx <- function(d) all(d == c(ny, nx)) || all(d == c(ny, nx) + 1L)
  ok_xy <- function(d) all(d == c(nx, ny)) || all(d == c(nx, ny) + 1L)
  d <- dim(m)
  if (x_first && ok_xy(d)) return(t(m))
  if (ok_yx(d)) return(m)
  if (ok_xy(d)) return(t(m))
  stop(sprintf("%s coordinate matrix is %d x %d, expected %d x %d (centres) or %d x %d (corners)",
               axis, d[1L], d[2L], ny, nx, ny + 1L, nx + 1L), call. = FALSE)
}

## corners from cell centres: average of the four neighbours after linear
## extrapolation by one cell on every side
.corners_from_centres <- function(m) {
  nr <- nrow(m)
  nc <- ncol(m)
  if (nr < 2L || nc < 2L) {
    stop("curvilinear coordinates need at least 2 x 2 cells", call. = FALSE)
  }
  p <- rbind(2 * m[1L, ] - m[2L, ], m, 2 * m[nr, ] - m[nr - 1L, ])
  p <- cbind(2 * p[, 1L] - p[, 2L], p, 2 * p[, nc] - p[, nc - 1L])
  r <- nr + 2L
  k <- nc + 2L
  (p[-1L, -1L] + p[-r, -1L] + p[-1L, -k] + p[-r, -k]) / 4
}

## affine transforms (ndr AffineIndex): 1D edges when rectilinear, else
## corners. 'x_first' says the x dimension comes before y in storage
.transform_coords <- function(tr, xname, yname, nx, ny, x_first) {
  if (!all(c(xname, yname) %in% tr$dims)) return(NULL)
  gt <- tr$gt
  if (gt[3L] == 0 && gt[5L] == 0) {
    return(list(x = gt[1L] + gt[2L] * (0:nx), y = gt[4L] + gt[6L] * (0:ny)))
  }
  i <- rep(0:nx, each = ny + 1L)
  j <- rep(0:ny, nx + 1L)
  list(corners = list(x = matrix(gt[1L] + i * gt[2L] + j * gt[3L], ny + 1L),
                      y = matrix(gt[4L] + i * gt[5L] + j * gt[6L], ny + 1L)))
}

## -------------------------------------------------------------------------
## labels and messages
## -------------------------------------------------------------------------

.slice_labels <- function(nd, sel) {
  if (!length(sel)) return(NULL)
  nm <- names(nd$dim)
  out <- character(0)
  for (k in names(sel)) {
    i <- match(k, nm)
    v <- nd$coords[[i]]
    if (is.null(v) || length(sel[[k]]) != 1L) next
    u <- if (length(nd$units) >= i) nd$units[[i]] else ""
    out[[k]] <- .coord_label(v[sel[[k]]], u, nd$calendar)
  }
  if (!length(out)) NULL else out
}

.coord_label <- function(v, units = "", calendar = NA) {
  if (inherits(v, c("Date", "POSIXt"))) return(format(v))
  if (is.numeric(v) && is.character(units) && length(units) == 1L && nzchar(units)) {
    tt <- .cf_time(v, units, calendar)
    if (!is.null(tt)) return(tt)
    return(paste(format(signif(v, 6)), units))
  }
  if (is.numeric(v)) return(format(signif(v, 6)))
  format(v)
}

## decode CF "<unit> since <origin>" for the standard calendars
.cf_time <- function(v, units, calendar = NA) {
  m <- regmatches(units, regexec("^\\s*([A-Za-z]+)\\s+since\\s+(.+?)\\s*$", units))[[1L]]
  if (length(m) != 3L) return(NULL)
  cal <- if (is.null(calendar) || is.na(calendar)) "standard" else tolower(calendar)
  if (!cal %in% c("standard", "gregorian", "proleptic_gregorian")) return(NULL)
  mult <- switch(sub("s$", "", tolower(m[2L])),
                 second = 1, sec = 1, minute = 60, min = 60, hour = 3600,
                 hr = 3600, h = 3600, day = 86400, d = 86400, NULL)
  if (is.null(mult)) return(NULL)
  origin <- sub("(Z|\\s*UTC|\\s*[+-]00:?00)$", "", m[3L])
  t0 <- as.POSIXct(origin, tz = "UTC", optional = TRUE,
                   tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS",
                                  "%Y-%m-%d %H:%M", "%Y-%m-%d"))
  if (is.na(t0)) return(NULL)
  tt <- t0 + v * mult
  if (all(as.numeric(tt) %% 86400 == 0)) format(tt, "%Y-%m-%d") else format(tt, "%Y-%m-%d %H:%M:%S")
}

.format_slice <- function(sel, labels = NULL) {
  paste(vapply(names(sel), function(k) {
    i <- sel[[k]]
    s <- if (length(i) == 1L) sprintf("%s = %d", k, i) else
      sprintf("%s = c(%s)", k, paste(i, collapse = ", "))
    if (!is.null(labels) && !is.na(labels[k])) s <- sprintf("%s [%s]", s, labels[[k]])
    s
  }, ""), collapse = ", ")
}

.report <- function(out, nd, sel, slice_given, ib) {
  dm <- nd$dim
  nm <- names(dm)
  a <- out$data
  if (is.numeric(a)) {
    big <- suppressWarnings(max(abs(a), na.rm = TRUE))
    if (is.finite(big) && big >= 1e30) {
      message(sprintf(paste0(
        "values of magnitude %s present, likely a fill value: set them to NA\n",
        "  (zaro::zaro_array(cf = TRUE) and xarray-style CF decoding do this)"),
        format(signif(big, 6))))
    }
  }
  if (slice_given) return(invisible())
  ## other slices exist along dimensions that are not drawn whole
  free <- setdiff(names(sel), nm[ib])
  free <- free[dm[free] > 1L]
  if (!length(free)) return(invisible())
  message(sprintf(paste0(
    "array (%s)\n",
    "  drawing %s (x) by %s (y)%s at %s\n",
    "  other slices: slice = list(%s)"),
    paste(sprintf("%s: %d", nm, dm), collapse = ", "),
    out$xy[["x"]], out$xy[["y"]],
    if (length(ib)) sprintf(" with %s = %s as colours", nm[ib],
                            paste(sel[[nm[ib]]], collapse = ",")) else "",
    .format_slice(sel[free], out$labels),
    paste(sprintf("%s = <1..%d>", free, dm[free]), collapse = ", ")))
  invisible()
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
