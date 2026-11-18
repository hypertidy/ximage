## the shared decoder: slices, spatial dimensions, orientation, placement

with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(code)
}
quiet <- function(code) suppressMessages(code)
undim <- function(x) {
  dim(x) <- unname(dim(x))
  x
}
## keep the first message for expect_message(), drop the rest
quiet_after <- function(code) {
  first <- TRUE
  withCallingHandlers(code, message = function(m) {
    if (first) first <<- FALSE else invokeRestart("muffleMessage")
  })
}

## a (y, x, time) test array whose cells say where they are: value = 100 * t + 10 * i + j
## for row i, column j of time step t, raster order (row 1 at the top)
yxt <- function(ny = 3, nx = 4, nt = 5) {
  a <- array(0, c(ny, nx, nt))
  for (t in seq_len(nt)) a[, , t] <- 100 * t + outer(10 * seq_len(ny), seq_len(nx), "+")
  a
}

test_that("plain matrices pass through unchanged", {
  d <- as_ximage(volcano)
  expect_s3_class(d, "ximage_data")
  expect_identical(d$data, volcano)
  expect_identical(d$extent, c(0, 61, 0, 87))
  expect_null(d$corners)
  expect_output(print(d), "ximage_data")
})

test_that("zaro_read() style (time, lat, lon) named dim slices on time", {
  a <- yxt()
  b <- aperm(a, c(3, 1, 2))
  names(dim(b)) <- c("time", "lat", "lon")
  expect_message(d <- as_ximage(b), "other slices: slice = list\\(time = <1..5>\\)")
  expect_equal(d$data, a[, , 1])
  expect_identical(d$xy, c(x = "lon", y = "lat"))
  d3 <- as_ximage(b, slice = list(time = 3))
  expect_equal(d3$data, a[, , 3])
  ## unnamed slice for the only non-spatial dimension, and a named vector
  expect_equal(as_ximage(b, slice = 3)$data, a[, , 3])
  expect_equal(as_ximage(b, slice = c(time = 3))$data, a[, , 3])
})

test_that("zaro_array() style (lon, lat, time) named dimnames is transposed", {
  a <- yxt(nt = 3)
  b <- aperm(a, c(2, 1, 3))
  dimnames(b) <- list(lon = NULL, lat = NULL, time = NULL)
  ## time = 3 is not taken for RGB, a named non-band dimension is a slice
  expect_message(d <- as_ximage(b), "drawing lon \\(x\\) by lat \\(y\\)")
  expect_equal(d$data, a[, , 1])
  expect_equal(as_ximage(b, slice = list(time = 2))$data, a[, , 2])
  with_null_device({
    res <- quiet(ximage(b))
    expect_identical(dim(res$x), c(3L, 4L))
  })
})

test_that("the slice message is only given when the slice was defaulted", {
  a <- yxt()
  names(dim(a)) <- c("y", "x", "time")
  expect_message(as_ximage(a), "time = 1")
  expect_no_message(as_ximage(a, slice = 2))
  expect_no_message(as_ximage(a, verbose = FALSE))
  withr_opt <- options(ximage.verbose = FALSE)
  on.exit(options(withr_opt))
  expect_no_message(as_ximage(a))
})

test_that("unnamed 3D arrays keep the band rule, more bands become slices", {
  expect_identical(dim(as_ximage(array(runif(24), c(2, 4, 3)))$data), c(2L, 4L, 3L))
  expect_identical(dim(as_ximage(array(runif(16), c(2, 4, 2)))$data), c(2L, 4L, 2L))
  a <- yxt(nt = 5)
  expect_message(d <- as_ximage(a), "dim3 = <1..5>")
  expect_equal(d$data, a[, , 1])
  ## bands = FALSE (xcontour, xtext) never makes a composite
  expect_equal(quiet(as_ximage(a[, , 1:3], bands = FALSE))$data, a[, , 1])
})

test_that("4D arrays slice on every non-spatial dimension", {
  a <- array(seq_len(3 * 4 * 2 * 5), c(3, 4, 2, 5))
  expect_message(d <- as_ximage(a), "dim3 = <1..2>, dim4 = <1..5>")
  expect_equal(d$data, a[, , 1, 1])
  expect_equal(as_ximage(a, slice = c(2, 4))$data, a[, , 2, 4])
  expect_equal(as_ximage(a, slice = list(dim4 = 3))$data, a[, , 1, 3])
  with_null_device(expect_no_error(quiet(ximage(a))))
})

test_that("colour composites from a band-like dimension or a slice selection", {
  a <- array(runif(3 * 4 * 5), c(y = 3, x = 4, band = 5))
  d <- as_ximage(a, slice = list(band = c(4, 3, 2)))
  expect_identical(dim(d$data), c(3L, 4L, 3L))
  expect_equal(d$data[, , 1], undim(a[, , 4]))
  ## band-named with 3 entries is a composite by default
  a3 <- array(runif(3 * 4 * 3), c(y = 3, x = 4, band = 3))
  expect_identical(dim(as_ximage(a3)$data), c(3L, 4L, 3L))
  expect_error(as_ximage(a, slice = list(band = 1:2)), "3 \\(RGB\\) or 4")
  expect_error(as_ximage(a, slice = list(band = 1:3), bands = FALSE), "single index")
})

test_that("bad slice and xy arguments error clearly", {
  a <- yxt()
  names(dim(a)) <- c("y", "x", "time")
  expect_error(as_ximage(a, slice = list(depth = 1)), "dimensions to slice are: time")
  expect_error(as_ximage(a, slice = 6), "1..5")
  expect_error(as_ximage(a, slice = 1.5), "whole numbers")
  expect_error(as_ximage(a, xy = c("x", "nope")), "two different dimensions")
  a4 <- array(1, c(y = 2, x = 2, t = 3, z = 2))
  expect_error(as_ximage(a4, slice = list(t = 1:3, z = 1:2)), "only one dimension")
  expect_error(as_ximage(a4, slice = c(1, 1, 1)), "one index per dimension")
  expect_error(as_ximage(volcano, slice = 2), "no dimensions to slice")
})

test_that("xy chooses the spatial dimensions, unrecognised names fall back with a message", {
  a <- yxt()
  names(dim(a)) <- c("a", "b", "c")
  expect_message(d <- quiet_after(as_ximage(a, verbose = TRUE)), "not recognised as x and y")
  expect_equal(d$data, undim(a[, , 1]))
  ## x = c, y = a: slices along b
  d2 <- as_ximage(a, xy = c("c", "a"), slice = list(b = 2))
  expect_equal(d2$data, undim(a[, 2, ]))
  d3 <- as_ximage(a, xy = c(3, 1), slice = 2)
  expect_equal(d3$data, undim(a[, 2, ]))
})

test_that("ascending y coordinates flip the rows, descending x the columns", {
  m <- matrix(1:12, 3, byrow = TRUE)
  d <- as_ximage(m, coords = list(x = 1:4, y = c(10, 20, 30)))
  expect_identical(d$data, m[3:1, ])
  expect_equal(d$extent, c(0.5, 4.5, 5, 35))
  d <- as_ximage(m, coords = list(x = 4:1, y = c(30, 20, 10)))
  expect_identical(d$data, m[, 4:1])
  expect_equal(d$extent, c(0.5, 4.5, 5, 35))
  ## an explicit extent places, the coordinates still orient
  d <- as_ximage(m, extent = c(0, 1, 0, 1), coords = list(x = 1:4, y = c(10, 20, 30)))
  expect_identical(d$extent, c(0, 1, 0, 1))
  expect_identical(d$data, m[3:1, ])
})

test_that("coordinates can be edges, one more than cells", {
  m <- matrix(1:12, 3, byrow = TRUE)
  d <- as_ximage(m, coords = list(x = 0:4 * 2, y = c(0, 1, 2, 3)))
  expect_equal(d$extent, c(0, 8, 0, 3))
  expect_identical(d$data, m[3:1, ])
  expect_error(as_ximage(m, coords = list(x = 1:3, y = 1:3)), "4 \\(centres\\) or 5 \\(edges\\)")
  expect_error(as_ximage(m, coords = list(x = c(1, 3, 2, 4), y = 1:3)), "increasing or decreasing")
})

test_that("only one axis known: the other is index space", {
  m <- matrix(1:12, 3, byrow = TRUE)
  d <- as_ximage(m, coords = list(x = 101:104, y = NULL))
  expect_equal(d$extent, c(100.5, 104.5, 0, 3))
})

test_that("irregular 1D coordinates give rectilinear corners", {
  m <- matrix(1:12, 3, byrow = TRUE)
  d <- as_ximage(m, coords = list(x = c(1, 2, 4, 8), y = c(3, 2, 1)))
  expect_false(is.null(d$corners))
  expect_identical(dim(d$corners$x), c(4L, 5L))
  expect_equal(d$corners$x[1, ], c(0.5, 1.5, 3, 6, 10))
  expect_equal(d$corners$y[, 1], c(3.5, 2.5, 1.5, 0.5))
  expect_equal(d$extent, c(0.5, 10, 0.5, 3.5))
  with_null_device({
    expect_no_error(ximage(m, coords = list(x = c(1, 2, 4, 8), y = c(3, 2, 1))))
    expect_no_error(xcontour(m, coords = list(x = c(1, 2, 4, 8), y = c(3, 2, 1))))
    res <- xtext(m, coords = list(x = c(1, 2, 4, 8), y = c(3, 2, 1)), cex = 1)
    ## labels sit at the given centres, the cells span the midpoints
    expect_equal(res$x[1:4], c(1, 2, 4, 8))
  })
})

test_that("curvilinear centres give corners, regular grids exactly", {
  ny <- 3
  nx <- 4
  cx <- matrix(seq_len(nx) - 0.5, ny, nx, byrow = TRUE)
  cy <- matrix(ny - seq_len(ny) + 0.5, ny, nx)
  d <- as_ximage(matrix(1:12, 3), coords = list(x = cx, y = cy))
  expect_equal(d$corners$x, matrix(0:nx, ny + 1, nx + 1, byrow = TRUE))
  expect_equal(d$corners$y, matrix(ny:0, ny + 1, nx + 1))
  expect_equal(d$extent, c(0, 4, 0, 3))
  ## corners given directly, and transposed (x-first storage) input
  d2 <- as_ximage(matrix(1:12, 3), coords = list(x = d$corners$x, y = d$corners$y))
  expect_identical(d2$corners, d$corners[c("x", "y")])
  d3 <- as_ximage(matrix(1:12, 3), coords = list(x = t(cx), y = t(cy)))
  expect_equal(d3$corners, d$corners)
  expect_error(as_ximage(matrix(1:12, 3), coords = list(x = cx, y = 1:3)), "x and y matrices")
  expect_error(as_ximage(matrix(1:12, 3), coords = list(x = cx[, 1:3], y = cy[, 1:3])),
               "expected 3 x 4")
})

test_that("curvilinear 'extent' list is coordinates, as before it was an error", {
  lon <- outer(1:87, 1:61, function(i, j) j + i / 10)
  lat <- outer(1:87, 1:61, function(i, j) -i + j / 20)
  with_null_device({
    res <- ximage(volcano, extent = list(lon, lat))
    expect_equal(res$extent, c(range(as_ximage(volcano, coords = list(lon, lat))$corners$x),
                               range(as_ximage(volcano, coords = list(lon, lat))$corners$y)))
    expect_no_error(xcontour(volcano, coords = list(x = lon, y = lat), add = TRUE,
                             levels = c(120, 160), col = "white"))
    res <- xtext(volcano[1:5, 1:5], coords = list(x = lon[1:5, 1:5], y = lat[1:5, 1:5]))
    expect_equal(res$x[1], lon[1, 1])
    expect_equal(res$y[1], lat[1, 1])
  })
})

test_that("gdalraster mdim_read() style vectors: dims, coords, scale, labels", {
  ## (lon, lat, time) fastest first, lat ascending south to north
  nx <- 4; ny <- 3; nt <- 3
  a <- yxt(ny, nx, nt)             ## raster order, row 1 north
  s <- a[ny:1, , , drop = FALSE]   ## stored south to north
  v <- as.vector(aperm(s, c(2, 1, 3)))
  attr(v, "gis") <- list(type = "multidim", dim = c(nx, ny, nt),
                         dim_names = c("lon", "lat", "time"),
                         coords = list(lon = c(140.5, 141.5, 142.5, 143.5),
                                       lat = c(-44.5, -43.5, -42.5),
                                       time = c(0, 24, 48)),
                         bbox = c(140, -45, 144, -42), srs = "", scale = 0.5, offset = 1,
                         dim_units = c("degrees_east", "degrees_north", "hours since 2000-01-01"),
                         calendar = "standard")
  expect_message(d <- as_ximage(v), "time = 1 \\[2000-01-01\\]")
  expect_equal(d$data, a[, , 1] * 0.5 + 1)
  expect_equal(d$extent, c(140, 144, -45, -42))
  expect_equal(as_ximage(v, slice = 3)$data, a[, , 3] * 0.5 + 1)
  expect_identical(as_ximage(v, slice = 2)$labels, c(time = "2000-01-02"))
  with_null_device({
    res <- quiet(ximage(v))
    expect_equal(dim(res$x), c(ny, nx))  ## not a 3 band RGB
    expect_no_error(quiet(xcontour(v)))
    expect_identical(quiet(xtext(v, cex = 1))$labels[1], format(a[1, 1, 1] * 0.5 + 1))
  })
})

test_that("classic gdalraster and vapour multi-band input slices beyond 4 bands", {
  gis <- list(type = "raster", bbox = c(0, 0, 4, 3), dim = c(4L, 3L, 5L), srs = "")
  v <- structure(as.numeric(1:60), gis = gis)
  expect_message(d <- as_ximage(v), "band = <1..5>")
  expect_equal(d$data, matrix(1:12, 3, byrow = TRUE))
  expect_equal(as_ximage(v, slice = list(band = 2))$data, matrix(13:24, 3, byrow = TRUE))
  ## two bands are two slices, not grey + alpha
  l2 <- structure(list(as.numeric(1:12), as.numeric(13:24)), extent = c(0, 4, 0, 3),
                  dimension = c(4L, 3L))
  expect_message(d2 <- as_ximage(l2), "band = <1..2>")
  expect_equal(d2$data, matrix(1:12, 3, byrow = TRUE))
})

test_that("fill values are reported", {
  m <- volcano
  m[1:5, 1:5] <- 9.96921e36
  expect_message(as_ximage(m), "likely a fill value")
  expect_no_message(as_ximage(m, verbose = FALSE))
})

test_that("CF times decode for standard calendars only", {
  expect_identical(.cf_time(c(0, 1), "days since 1970-01-01"), c("1970-01-01", "1970-01-02"))
  expect_identical(.cf_time(6, "hours since 2000-01-01 00:00:00"), "2000-01-01 06:00:00")
  expect_identical(.cf_time(0, "seconds since 2000-01-01T00:00:00Z"), "2000-01-01")
  expect_null(.cf_time(0, "days since 2000-01-01", "noleap"))
  expect_null(.cf_time(0, "metres"))
  expect_identical(.coord_label(5, "m"), "5 m")
  expect_identical(.coord_label(as.Date("2020-01-02")), "2020-01-02")
})

test_that("as_ximage() output can be drawn again", {
  d <- as_ximage(volcano)
  with_null_device({
    res <- ximage(d, extent = c(0, 1, 0, 1))
    expect_identical(res$extent, c(0, 1, 0, 1))
    expect_identical(res$x, ximage(volcano)$x)
  })
})

test_that("unsupported objects error", {
  expect_error(as_ximage(function(x) x), "cannot draw an object of class 'function'")
  expect_error(as_ximage(list()), "no dimension known")
})
