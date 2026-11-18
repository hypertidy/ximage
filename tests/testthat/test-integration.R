## integration with the readers ximage decodes by structure, each skipped
## when the package isn't installed

with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(code)
}

test_that("lazy altarr arrays fetch only the chunks of the slice drawn", {
  skip_if_not_installed("altarr")
  fetched <- 0L
  fetch <- function(cc) {
    fetched <<- fetched + nrow(cc)
    lapply(seq_len(nrow(cc)), function(i) rep(as.double(cc[i, 3L]), 4 * 3))
  }
  a <- altarr::altarr(c(4L, 3L, 10L), c(4L, 3L, 1L), fetch,
                      dimnames = list(lon = NULL, lat = NULL, time = NULL))
  d <- as_ximage(a, slice = list(time = 6))
  expect_identical(fetched, 1L)
  expect_identical(dim(d$data), c(3L, 4L))
  expect_true(all(d$data == 5))
})

test_that("zaro arrays, lazy and read, draw the same", {
  skip_if_not_installed("zaro")
  skip_if_not_installed("altarr")
  op <- options(zaro.verbose = FALSE)
  on.exit(options(op))
  zd <- zaro::zaro_dataset(zaro::zaro(system.file("extdata", "example.zarr", package = "zaro")))
  crd <- list(x = as.vector(zaro::zaro_read(zd, "lon")), y = as.vector(zaro::zaro_read(zd, "lat")))
  lazy <- zaro::zaro_array(zd, "temp")
  eager <- zaro::zaro_read(zd, "temp")
  d1 <- as_ximage(lazy, coords = crd, slice = list(time = 3))
  d2 <- as_ximage(eager, coords = crd, slice = list(time = 3))
  expect_equal(d1$data, d2$data)
  expect_equal(d1$extent, d2$extent)
  expect_identical(d1$xy, c(x = "lon", y = "lat"))
})

test_that("ndr DataArray slices with coordinates, labels, and affine indexes", {
  skip_if_not_installed("ndr")
  v <- ndr::Variable(dims = c("time", "lat", "lon"), data = array(as.numeric(1:24), c(2, 3, 4)))
  da <- ndr::DataArray(variable = v, coords = list(
    lat = ndr::ImplicitCoord(dimension = "lat", n = 3L, offset = -10, step = 10),
    lon = ndr::ExplicitCoord(dimension = "lon", values = c(100, 110, 120, 130)),
    time = ndr::ExplicitCoord(dimension = "time", values = as.Date("2020-01-01") + 0:1)))
  d <- as_ximage(da, slice = list(time = 2))
  ## lat ascending: rows reversed so north is up
  expect_equal(d$data, array(as.numeric(1:24), c(2, 3, 4))[2, 3:1, ])
  expect_equal(d$extent, c(95, 135, -15, 15))
  expect_identical(d$labels, c(time = "2020-01-02"))
  expect_message(as_ximage(da), "time = 1 \\[2020-01-01\\]")
  expect_no_error(as_ximage(v, verbose = FALSE))
  ## a rotated affine index is drawn by corners
  rot <- ndr::DataArray(variable = ndr::Variable(dims = c("y", "x"), data = matrix(1:6, 2, 3)),
                        coords = list(ndr::AffineIndex(dims = c("x", "y"), shape = c(3L, 2L),
                                                       transform = c(0, 1, 0.5, 2, 0.25, -1))))
  dr <- as_ximage(rot)
  expect_identical(dim(dr$corners$x), c(3L, 4L))
  expect_equal(dr$corners$x[1, ], c(0, 1, 2, 3))
  expect_equal(dr$corners$x[3, 1], 1)
  with_null_device(expect_no_error(ximage(rot)))
})

test_that("gdalraster mdim_read() matches the classic north-up view", {
  skip_if_not_installed("gdalraster")
  skip_if_not("mdim_read" %in% getNamespaceExports("gdalraster"))
  skip_if_not(isTRUE(gdalraster::gdal_formats("netCDF")$multidim_raster))
  f <- system.file("extdata", "mdim_small.nc", package = "gdalraster")
  skip_if(!nzchar(f))
  v <- gdalraster::mdim_read(f, "/t2m")
  d <- as_ximage(v, verbose = FALSE)
  cl <- gdalraster::mdim_as_classic(f, "t2m", 2, 1)
  on.exit(cl$close())
  r <- gdalraster::read_ds(cl, bands = 1)
  g <- attr(v, "gis")
  expect_equal(as.vector(t(d$data)), as.vector(r) * g$scale + g$offset)
  expect_equal(d$extent, attr(r, "gis")$bbox[c(1, 3, 2, 4)])
})
