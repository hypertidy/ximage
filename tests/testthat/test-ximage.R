## integration tests for ximage() dispatch, run against a null device

with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(code)
}

test_that("ximage returns the plotted colours and extent invisibly", {
  with_null_device({
    res <- ximage(volcano)
    expect_named(res, c("x", "extent"))
    expect_identical(res$extent, c(0, ncol(volcano), 0, nrow(volcano)))
    expect_identical(dim(res$x), dim(volcano))
  })
})

test_that("numeric matrix with NA, zlim, and RGB with NaN plot end to end", {
  with_null_device({
    v <- volcano
    v[v > 180] <- NA
    expect_no_error(ximage(v, na.col = "hotpink"))
    expect_no_error(ximage(volcano, zlim = c(100, 150)))
    a3 <- array(runif(24), c(2, 4, 3))
    a3[1, 1, 2] <- NaN
    expect_no_error(ximage(a3))
    expect_no_error(ximage(a3, alpha = 0.5))
  })
})

test_that("vapour-style lists honour attributes, orientation, and band count", {
  with_null_device({
    d <- c(4L, 3L)  ## (ncol, nrow)
    vals <- as.numeric(1:12)
    l <- structure(list(vals), extent = c(0, 4, 0, 3), dimension = d,
                   projection = "EPSG:4326")
    res <- ximage(l)
    expect_identical(res$extent, c(0, 4, 0, 3))
    expect_identical(dim(res$x), c(3L, 4L))
    ## row-major convention: first vector element is the top-left cell
    ref <- ximage(matrix(vals, d[2L], byrow = TRUE))
    expect_identical(res$x[1, 1], ref$x[1, 1])

    l3 <- structure(list(vals, vals, vals), extent = c(0, 4, 0, 3),
                    dimension = d)
    expect_no_error(ximage(l3))
    ## 4 bands (a previous version hardcoded 3 in the array dims)
    l4 <- structure(list(vals, vals, vals, rep(255, 12)),
                    extent = c(0, 4, 0, 3), dimension = d)
    expect_no_error(ximage(l4))
    ## hex character band, gdal_raster_image style
    lh <- structure(list(rep(c("#FF0000FF", "#00FF00FF"), 6)),
                    extent = c(0, 4, 0, 3), dimension = d)
    expect_no_error(ximage(lh))
  })
})

test_that("a list holding a plain matrix is not scrambled", {
  with_null_device({
    res <- ximage(list(volcano))
    expect_identical(dim(res$x), dim(volcano))
    expect_identical(res$x, ximage(volcano)$x)
  })
})

test_that("gdalraster-style gis attribute inputs work", {
  with_null_device({
    gis <- list(type = "raster", bbox = c(0, 0, 4, 3), dim = c(4L, 3L, 1L),
                srs = "EPSG:4326")
    gv <- structure(as.numeric(1:12), gis = gis)
    res <- ximage(gv)
    expect_identical(res$extent, c(0, 4, 0, 3))
  })
})

test_that("fastpng-style width/height/depth attributed vectors work", {
  with_null_device({
    fr <- as.raw(rep(c(255, 0, 0, 255), 12))
    attributes(fr) <- list(width = 4L, height = 3L, depth = 4L)
    expect_no_error(ximage(fr))
    ## grey + alpha, depth 2 (issue 13)
    fga <- as.raw(rep(c(128, 255), 12))
    attributes(fga) <- list(width = 4L, height = 3L, depth = 2L)
    expect_no_error(ximage(fga))
    fg <- as.raw(0:11)
    attributes(fg) <- list(width = 4L, height = 3L, depth = 1L)
    expect_no_error(ximage(fg))
  })
})

test_that("raster class input tolerates NA colours", {
  with_null_device({
    rr <- as.raster(matrix(c("red", NA, "blue", "green"), 2))
    expect_no_error(ximage(rr))
  })
})

test_that("all methods match the generic signature", {
  gen <- names(formals(ximage))
  meths <- c("ximage.default", "ximage.list", "ximage.numeric",
             "ximage.integer", "ximage.raw", "ximage.nativeRaster",
             "ximage.raster")
  for (f in meths) {
    expect_identical(names(formals(getFromNamespace(f, "ximage"))), gen,
                     info = f)
  }
})
