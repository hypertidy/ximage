## xslices() and xpanels()

with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(code)
}

test_that("xslices lists the non-spatial index combinations", {
  a <- array(1, c(y = 3, x = 4, time = 2, depth = 3))
  s <- xslices(a)
  expect_identical(names(s), c("time", "depth"))
  expect_identical(nrow(s), 6L)
  expect_identical(attr(s, "xy"), c("x", "y"))
  expect_identical(ncol(xslices(volcano)), 0L)
  expect_identical(ncol(xslices(as.raster(matrix(0:1, 2, 2)))), 0L)
})

test_that("xslices labels from coordinates, rows feed 'slice'", {
  v <- runif(4 * 3 * 3)
  attr(v, "gis") <- list(type = "multidim", dim = c(4, 3, 3), dim_names = c("lon", "lat", "time"),
                         coords = list(1:4, 3:1, c(0, 1, 2)),
                         dim_units = c("", "", "days since 2020-01-01"))
  s <- xslices(v)
  expect_identical(s$time_label, c("2020-01-01", "2020-01-02", "2020-01-03"))
  expect_equal(as_ximage(v, slice = s[2, ])$data, as_ximage(v, slice = list(time = 2))$data)
  expect_error(as_ximage(v, slice = s[1:2, ]), "one row")
})

test_that("xpanels draws slices with one colour scale", {
  a <- array(c(volcano, volcano + 100, volcano + 200), c(y = 87, x = 61, time = 3))
  with_null_device({
    res <- xpanels(a)
    expect_length(res$panels, 3L)
    expect_identical(res$zlim, range(a))
    expect_identical(res$panels[[1L]]$zlim, range(a))
    res <- xpanels(a, slice = list(time = c(1, 3)), zlim = c(0, 1000))
    expect_length(res$panels, 2L)
    expect_identical(res$panels[[2L]]$slice$time, 3L)
    res <- xpanels(a, slice = xslices(a)[2:3, , drop = FALSE])
    expect_identical(res$panels[[1L]]$slice$time, 2L)
    expect_message(xpanels(a, max_panels = 2), "first 2 of 3")
    expect_error(xpanels(volcano), "no dimensions")
    expect_error(xpanels(a, slice = 1:2), "named")
  })
})

test_that("xslices reads no data from lazy arrays", {
  skip_if_not_installed("altarr")
  fetched <- 0L
  fetch <- function(cc) {
    fetched <<- fetched + nrow(cc)
    lapply(seq_len(nrow(cc)), function(i) rep(0, 4 * 3))
  }
  a <- altarr::altarr(c(4L, 3L, 10L), c(4L, 3L, 1L), fetch,
                      dimnames = list(lon = NULL, lat = NULL, time = NULL))
  s <- xslices(a)
  expect_identical(nrow(s), 10L)
  expect_identical(fetched, 0L)
})
