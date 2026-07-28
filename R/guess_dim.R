## Dimension guessing for bare atomic vectors (no dim, no 'gis' attribute,
## no fastpng width/height/depth attributes).
##
## The guess assumes raster scanline order, i.e. flat output from GDAL
## readers (vapour::gdal_raster_data, gdalraster::read_ds), built with
## matrix(x, ncol = NC, byrow = TRUE) so the first value is the top-left
## cell scanning across rows down the page. Dimensions are reported in
## GDAL order (ncol x nrow, xsize x ysize) to match what the reader saw;
## column-major R data needs dim(x) <- c(NR, NC) instead, noted in the
## message.
##
## For byte-like data (raw, or integer-ish values within 0..255) we also
## consider 3-plane (RGB) and 4-plane (RGBA) interpretations. Raw vectors
## are assumed pixel-interleaved (RGBRGB...), numeric/integer vectors are
## assumed band-sequential (GDAL style).

## all divisor pairs of n as a 2-column matrix (nrow, ncol), ordered from
## 1 x n through n x 1 (integer-division detection)
.dim_choices <- function(n) {
  stopifnot(length(n) == 1L, is.finite(n), n >= 1)
  d <- seq_len(floor(sqrt(n)))
  d <- d[n %% d == 0]
  lo <- cbind(nrow = d, ncol = n / d)
  hi <- cbind(nrow = rev(n / d), ncol = rev(d))
  out <- rbind(lo, hi)
  out[!duplicated(out), , drop = FALSE]
}

## squareness score, 0 is square, larger is more elongated (symmetric in
## aspect, but ties between a x b and b x a resolve to the first row seen,
## which is the landscape ncol >= nrow orientation)
.aspect_score <- function(m) {
  ## computed on the max/min ratio so mirrored pairs score identically
  ## (exact ties, resolved by which.min to the first seen, the landscape
  ## ncol >= nrow orientation)
  log(pmax(m[, 1L], m[, 2L]) / pmin(m[, 1L], m[, 2L]))
}

## does this look like Byte data? deliberately narrow: only raw counts,
## whole numbers within 0..255 are far too common as ordinary data (1:12,
## category codes, small counts) to imply an RGB image. A future opt-in
## could detect much narrower characteristics (strong dynamic range
## across 0..255 or 0..65535, image-like value distribution) but for now
## numeric data never gets a plane interpretation by default.
.byte_like <- function(x) {
  is.raw(x)
}

## choose a layout: list(planes, dim = c(nrow, ncol), score, choices)
.guess_layout <- function(x, planes = .byte_like(x)) {
  n <- length(x)
  if (n < 1) stop("cannot guess a dimension for a zero-length vector", call. = FALSE)
  ch2 <- .dim_choices(n)
  s2 <- .aspect_score(ch2)
  i2 <- which.min(s2)
  cand <- list(list(planes = 1L, dim = ch2[i2, ], score = s2[i2], choices = ch2))
  if (isTRUE(planes)) {
    for (nb in c(3L, 4L)) {
      if (n > nb && n %% nb == 0) {
        chp <- .dim_choices(n / nb)
        keep <- chp[, 1L] > 1 & chp[, 2L] > 1
        if (!any(keep)) next
        chp <- chp[keep, , drop = FALSE]
        sp <- .aspect_score(chp)
        ip <- which.min(sp)
        cand[[length(cand) + 1L]] <-
          list(planes = nb, dim = chp[ip, ], score = sp[ip], choices = chp)
      }
    }
  }
  scores <- vapply(cand, function(a) a$score, numeric(1L))
  planes <- vapply(cand, function(a) a$planes, integer(1L))
  ## on a tied squareness score raw data prefers a colour-plane
  ## interpretation, other types prefer the plain 2D one
  eps <- if (is.raw(x)) -1e-9 else 1e-9
  cand[[which.min(scores + ifelse(planes > 1L, eps, 0))]]
}

## format the candidate pair listing, truncated around the chosen pair
.format_choices <- function(ch, chosen, max_show = 12L) {
  lab <- sprintf("%dx%d", ch[, 1L], ch[, 2L])
  ichosen <- which(ch[, 1L] == chosen[1L] & ch[, 2L] == chosen[2L])[1L]
  if (!is.na(ichosen)) lab[ichosen] <- sprintf("[%s]", lab[ichosen])
  n <- length(lab)
  if (n <= max_show) return(paste(lab, collapse = ", "))
  keep <- sort(unique(pmin(pmax(c(1L, 2L, ichosen + (-2:2), n - 1L, n), 1L), n)))
  out <- character(0L)
  last <- 0L
  for (i in keep) {
    if (i > last + 1L) out <- c(out, "...")
    out <- c(out, lab[i])
    last <- i
  }
  paste(out, collapse = ", ")
}

## guess a matrix/array from a bare atomic vector, with messaging and a
## guard against very large inputs (override with force = TRUE, or set
## the dimension explicitly)
.ximage_guess <- function(x, force = FALSE) {
  n <- length(x)
  g <- .guess_layout(x)
  nr <- g$dim[[1L]]
  nc <- g$dim[[2L]]
  nb <- g$planes
  ## display in GDAL order, ncol x nrow ascending by ncol
  disp <- g$choices[rev(seq_len(nrow(g$choices))), 2:1, drop = FALSE]
  maxn <- getOption("ximage.guess_max", 2^24)
  if (n > maxn && !isTRUE(force)) {
    stop(sprintf(paste0(
      "are you sure? we're guessing dim of %d x %d (ncol x nrow%s) for a vector of length %s\n",
      "  use 'force = TRUE' to go ahead, or set the shape yourself, e.g.\n",
      "    x <- matrix(x, ncol = %d, byrow = TRUE) ## raster scanline order (the guess)\n",
      "    dim(x) <- c(%d, %d)                     ## if your data is column-major"),
      nc, nr, if (nb > 1L) sprintf(" x %d planes", nb) else "",
      format(n, big.mark = ","), nc, nr, nc),
      call. = FALSE)
  }
  if (nb == 1L) {
    prime <- nrow(g$choices) == 2L && n > 1L
    message(sprintf(paste0(
      "guessing dimension for vector of length %s\n",
      "  using ncol x nrow = %d x %d (GDAL xsize x ysize), as matrix(x, ncol = %d, byrow = TRUE)\n",
      "  candidates (ncol x nrow): %s%s"),
      format(n, big.mark = ","), nc, nr, nc,
      .format_choices(disp, c(nc, nr)),
      if (prime) sprintf("\n  note: length %s is prime, only trivial shapes available", format(n, big.mark = ",")) else ""))
    return(matrix(x, nrow = nr, ncol = nc, byrow = TRUE))
  }
  interleave <- if (is.raw(x)) "pixel-interleaved (RGBRGB...)" else "band-sequential (GDAL order)"
  message(sprintf(paste0(
    "guessing dimension for vector of length %s\n",
    "  using ncol x nrow x planes = %d x %d x %d (GDAL xsize x ysize x bands), assumed %s scanline\n",
    "  candidates for %d planes (ncol x nrow): %s\n",
    "  (set the shape yourself or pass a matrix/array to choose a different interpretation)"),
    format(n, big.mark = ","), nc, nr, nb, interleave, nb,
    .format_choices(disp, c(nc, nr))))
  if (is.raw(x)) {
    aperm(array(x, c(nb, nc, nr)), c(3L, 2L, 1L))
  } else {
    aperm(array(x, c(nc, nr, nb)), c(2L, 1L, 3L))
  }
}
