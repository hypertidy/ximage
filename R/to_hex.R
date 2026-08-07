## Internal colour conversion for ximage ------------------------------------
##
## to_hex() is the single choke point that turns any supported input into a
## character matrix of hex colours ready for graphics::rasterImage().
##
## Supported inputs:
##   - numeric/integer matrix:        palette mapping via col/breaks/zlim
##   - numeric 3D array, 1 band:      squeezed to matrix, palette mapping
##   - numeric 3D array, 2 bands:     grey + alpha
##   - numeric 3D array, 3 bands:     RGB (optional constant 'alpha')
##   - numeric 3D array, 4 bands:     RGBA ('alpha' multiplies the alpha band)
##   - raw matrix/array:              converted to integer, then as above
##   - character matrix:              colours passed through, NA -> 'na.col'
##
## Behaviour guarantees:
##   - Non-finite values (NA, NaN, Inf) always map to 'na.col' and never
##     reach rgb()/rasterImage() (fixes NaN crash, issue #7).
##   - Colour scaling is autodetected per image: data within [0, 1] is used
##     as-is, within [0, 255] is divided by 255, anything else is rescaled
##     by the finite range of the colour bands jointly (so hue relationships
##     are preserved); an alpha band is scaled independently so a constant
##     opaque alpha does not distort the stretch.
##   - Constant (zero-range) data maps to the middle of the palette rather
##     than producing NaN colours (issue #4).
##   - 'zlim' anchors the palette to an absolute range; values outside zlim
##     map to 'na.col' (base image() semantics). 'zlim' is ignored with a
##     warning for multi-band input.
##   - 'alpha' is a constant (or recycled vector/matrix) opacity multiplier
##     in [0, 1] applied on top of any existing alpha (issue #15).
##
## Not exported. Keep this file free of any external package calls.

#' @importFrom palr image_hex
to_hex <- function(x, col = NULL, breaks = NULL, zlim = NULL,
                   alpha = NULL, na.col = "transparent") {
  return(palr::image_hex(x, col = col, breaks = breaks, zlim = zlim, alpha = alpha, na.col = na.col))

}

## map a numeric matrix to hex colours through a palette
.map_palette <- function(x, col = NULL, breaks = NULL, zlim = NULL,
                         na.col = "transparent") {
  dm <- dim(x)
  vals <- as.vector(x)

  if (is.null(col)) col <- grDevices::hcl.colors(96L, "YlOrRd", rev = TRUE)
  if (is.function(col)) {
    col <- col(if (!is.null(breaks)) length(breaks) - 1L else 96L)
  }

  if (!is.null(zlim)) {
    zlim <- range(zlim)
    vals[vals < zlim[1L] | vals > zlim[2L]] <- NA_real_
  }
  ok <- is.finite(vals)
  if (!any(ok)) {
    return(matrix(na.col, dm[1L], dm[2L]))
  }

  if (is.null(breaks)) {
    rg <- if (!is.null(zlim) && all(is.finite(zlim))) zlim else range(vals[ok])
    if (!(diff(rg) > 0)) rg <- rg + c(-0.5, 0.5)  ## constant data
    breaks <- seq(rg[1L], rg[2L], length.out = length(col) + 1L)
  } else {
    breaks <- sort(breaks)
    if ((length(breaks) - 1L) != length(col)) {
      col <- grDevices::colorRampPalette(col)(length(breaks) - 1L)
    }
  }
  idx <- findInterval(vals, breaks, all.inside = TRUE)
  out <- col[idx]
  out[!ok] <- na.col
  matrix(out, dm[1L], dm[2L])
}

## autodetect the value convention of a band (or set of bands) and
## rescale to [0, 1]; non-finite values pass through untouched
.band_scale <- function(vals) {
  ok <- is.finite(vals)
  if (!any(ok)) return(rep(0, length(vals)))
  rg <- range(vals[ok])
  if (rg[1L] >= 0 && rg[2L] <= 1) {
    ## already 0,1
  } else if (rg[1L] >= 0 && rg[2L] <= 255) {
    vals <- vals / 255
  } else {
    if (!(diff(rg) > 0)) rg <- rg + c(-0.5, 0.5)
    vals <- (vals - rg[1L]) / diff(rg)
  }
  vals
}

## 3 or 4 band numeric array to hex
.rgb_hex <- function(x, alpha = NULL, na.col = "transparent") {
  dm <- dim(x)
  n <- dm[1L] * dm[2L]
  nbands <- dm[3L]

  bad <- !is.finite(x[, , 1L]) | !is.finite(x[, , 2L]) | !is.finite(x[, , 3L])
  a <- NULL
  if (nbands == 4L) {
    a <- as.vector(x[, , 4L])
    bad <- bad | !is.finite(x[, , 4L])
  }

  ## joint scaling of the colour bands preserves hue relationships
  rgbvals <- .band_scale(c(x[, , 1L], x[, , 2L], x[, , 3L]))
  rgbvals[!is.finite(rgbvals)] <- 0
  rgbvals <- pmin(pmax(rgbvals, 0), 1)
  r <- rgbvals[seq_len(n)]
  g <- rgbvals[n + seq_len(n)]
  b <- rgbvals[2L * n + seq_len(n)]

  if (is.null(a)) {
    a <- rep(1, n)
  } else {
    a <- .band_scale(a)
    a[!is.finite(a)] <- 0
  }
  a <- .combine_alpha(a, alpha, n)

  hex <- grDevices::rgb(r, g, b, a)
  hex[as.vector(bad)] <- na.col
  matrix(hex, dm[1L], dm[2L])
}

## 2 band numeric array (grey + alpha) to hex
.greya_hex <- function(x, alpha = NULL, na.col = "transparent") {
  dm <- dim(x)
  n <- dm[1L] * dm[2L]
  bad <- !is.finite(x[, , 1L]) | !is.finite(x[, , 2L])

  g <- .band_scale(as.vector(x[, , 1L]))
  g[!is.finite(g)] <- 0
  g <- pmin(pmax(g, 0), 1)

  a <- .band_scale(as.vector(x[, , 2L]))
  a[!is.finite(a)] <- 0
  a <- .combine_alpha(a, alpha, n)

  hex <- grDevices::rgb(g, g, g, a)
  hex[as.vector(bad)] <- na.col
  matrix(hex, dm[1L], dm[2L])
}

## multiply existing alpha by the user-supplied constant/matrix, clamp
.combine_alpha <- function(a, alpha, n) {
  if (!is.null(alpha)) {
    a <- a * rep(as.vector(alpha), length.out = n)
  }
  pmin(pmax(a, 0), 1)
}

## apply a constant alpha multiplier to an existing matrix of R colours,
## leaving 'na.col' cells untouched (missingness display is not modulated)
.apply_alpha <- function(hex, alpha, na.col = NULL) {
  if (is.null(alpha)) return(hex)
  dm <- dim(hex)
  keep <- if (is.null(na.col)) rep(FALSE, length(hex)) else hex == na.col
  vals <- hex
  vals[keep] <- "#00000000"  ## placeholder, restored below
  m <- grDevices::col2rgb(vals, alpha = TRUE) / 255
  a <- .combine_alpha(m[4L, ], alpha, ncol(m))
  out <- grDevices::rgb(m[1L, ], m[2L, ], m[3L, ], a)
  out[keep] <- na.col
  matrix(out, dm[1L], dm[2L])
}
