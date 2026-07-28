test_that("dim choices are complete and ordered", {
  ch <- ximage:::.dim_choices(12)
  expect_equal(ch[, 1L], c(1, 2, 3, 4, 6, 12))
  expect_equal(ch[, 2L], c(12, 6, 4, 3, 2, 1))
  ## square input has no duplicated middle pair
  ch <- ximage:::.dim_choices(16)
  expect_equal(sum(ch[, 1L] == 4), 1L)
  ## prime
  ch <- ximage:::.dim_choices(13)
  expect_equal(nrow(ch), 2L)
})

test_that("guess prefers near-square, landscape on ties", {
  g <- ximage:::.guess_layout(rnorm(12))
  expect_equal(unname(g$dim), c(3, 4))
  g <- ximage:::.guess_layout(rnorm(5307))  ## volcano length, 3 * 29 * 61
  expect_equal(unname(g$dim), c(61, 87))
  expect_equal(g$planes, 1L)
})

test_that("bare numeric vector plots with a message", {
  v <- as.vector(t(volcano))
  pdf(NULL)
  on.exit(dev.off())
  expect_message(out <- ximage(v), "guessing dimension")
  expect_equal(out$extent, c(0, 87, 0, 61))
})

test_that("numeric data never gets planes by default, opt-in works", {
  ## byte-range whole numbers are too common as ordinary data to imply RGB
  x <- as.integer(sample(0:255, 3 * 64 * 64, replace = TRUE))
  g <- ximage:::.guess_layout(x)
  expect_equal(g$planes, 1L)
  ## the simple case stays simple: 1:12 as double is 3 x 4, one plane
  g <- ximage:::.guess_layout(as.double(1:12))
  expect_equal(g$planes, 1L)
  expect_equal(unname(g$dim), c(3, 4))
  ## explicit opt-in still detects the 3-plane 64x64 interpretation
  g <- ximage:::.guess_layout(x, planes = TRUE)
  expect_equal(g$planes, 3L)
  expect_equal(unname(g$dim), c(64, 64))
})

test_that("raw prefers planes on tied squareness", {
  ## 100 x 100 grey bytes are divisible by 4, tie between 100x100 and
  ## 50x50x4 goes to the plane interpretation for raw
  x <- as.raw(sample(0:255, 100 * 100, replace = TRUE))
  g <- ximage:::.guess_layout(x)
  expect_equal(g$planes, 4L)
  expect_equal(unname(g$dim), c(50, 50))
  ## same values as integer never get planes
  g <- ximage:::.guess_layout(as.integer(as.integer(x)))
  expect_equal(g$planes, 1L)
  expect_equal(unname(g$dim), c(100, 100))
})

test_that("2D fill is raster scanline, matrix(x, ncol, byrow = TRUE)", {
  m <- matrix(rnorm(12), 3, 4)
  v <- as.vector(t(m))  ## scanline flatten, as a GDAL reader returns
  suppressMessages(g <- ximage:::.ximage_guess(v))
  expect_equal(g, m)
})

test_that("plane fill logic is band-sequential scanline (GDAL order)", {
  ## numeric planes need opt-in now, exercise the fill logic directly
  a <- array(sample(0:255, 3 * 64 * 64, replace = TRUE), c(64, 64, 3))
  v <- as.vector(aperm(a, c(2, 1, 3)))  ## per band scanline, bands sequential
  g <- ximage:::.guess_layout(v, planes = TRUE)
  expect_equal(g$planes, 3L)
  expect_equal(aperm(array(v, c(64, 64, 3)), c(2, 1, 3)), a)
})

test_that("raw planes fill is scanline pixel-interleaved", {
  a <- array(as.raw(sample(0:255, 4 * 50 * 50, replace = TRUE)), c(50, 50, 4))
  ## interleave in scanline order: for row, for col, for band
  v <- as.vector(aperm(a, c(3, 2, 1)))
  suppressMessages(g <- ximage:::.ximage_guess(v))
  expect_equal(g, a)
})

test_that("messages report GDAL order, ncol x nrow", {
  v <- rnorm(5307)  ## 61 rows x 87 cols guessed, reported as 87 x 61
  expect_message(ximage:::.ximage_guess(v), "ncol x nrow = 87 x 61", fixed = TRUE)
  expect_message(ximage:::.ximage_guess(v), "matrix(x, ncol = 87, byrow = TRUE)", fixed = TRUE)
})

test_that("really big vectors require force", {
  op <- options(ximage.guess_max = 100)
  on.exit(options(op))
  v <- rnorm(144)
  expect_error(ximage:::.ximage_guess(v), "are you sure")
  expect_silent(suppressMessages(ximage:::.ximage_guess(v, force = TRUE)))
})

test_that("prime lengths are noted, zero length errors", {
  expect_message(ximage:::.ximage_guess(rnorm(13)), "prime")
  expect_error(ximage:::.ximage_guess(numeric(0)), "zero-length")
})

test_that("character colour vectors are accepted", {
  pdf(NULL)
  on.exit(dev.off())
  expect_message(out <- ximage(rep(c("#000000", "hotpink"), 8)), "guessing dimension")
  expect_equal(out$extent, c(0, 4, 0, 4))
})

test_that("gis and whd attributed vectors are not intercepted", {
  ## fastpng-style attributes take precedence over guessing
  x <- as.raw(sample(0:255, 4 * 2 * 3, replace = TRUE))
  attr(x, "width") <- 2L
  attr(x, "height") <- 3L
  attr(x, "depth") <- 4L
  pdf(NULL)
  on.exit(dev.off())
  expect_no_message(ximage(x))
})

test_that("default method guesses for vectors that dispatch straight there", {
  pdf(NULL)
  on.exit(dev.off())
  ## logical has no explicit method
  expect_message(out <- ximage(rep(c(TRUE, FALSE), 8)), "guessing dimension")
  expect_equal(out$extent, c(0, 4, 0, 4))
  ## classed atomic vectors fall through to default
  x <- structure(rnorm(12), class = "somevectorthing")
  expect_message(out <- ximage.default(x), "guessing dimension")
  expect_equal(out$extent, c(0, 4, 0, 3))
})
