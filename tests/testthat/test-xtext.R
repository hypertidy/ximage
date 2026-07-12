## tests for xtext(), run against a null device

with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(code)
}

test_that("xtext places labels at cell centres in raster order", {
  with_null_device({
    m <- matrix(1:12, 3, byrow = TRUE)
    res <- xtext(m)
    ## first label is the first matrix cell, at the top-left cell centre
    expect_identical(res$labels[1], "1")
    expect_identical(res$x[1], 0.5)
    expect_identical(res$y[1], nrow(m) - 0.5)
    ## reading order across the top row is the first matrix row
    expect_identical(res$labels[1:4], c("1", "2", "3", "4"))
    expect_true(all(res$y[1:4] == nrow(m) - 0.5))
    ## last label is the bottom-right cell
    expect_identical(res$labels[12], "12")
    expect_identical(res$x[12], ncol(m) - 0.5)
    expect_identical(res$y[12], 0.5)
  })
})

test_that("xtext respects extent and computes a finite fitting cex", {
  with_null_device({
    m <- matrix(1:12, 3, byrow = TRUE)
    ex <- c(140, 148, -44, -38)
    res <- xtext(m, extent = ex)
    xres <- diff(ex[1:2]) / ncol(m)
    yres <- diff(ex[3:4]) / nrow(m)
    expect_equal(res$x[1], ex[1] + xres / 2)
    expect_equal(res$y[1], ex[4] - yres / 2)
    expect_true(is.finite(res$cex) && res$cex > 0)
    ## explicit cex is used as given
    expect_identical(xtext(m, extent = ex, cex = 2)$cex, 2)
  })
})

test_that("xtext skips NA labels and survives all-NA input", {
  with_null_device({
    m <- matrix(c(1, NA, 3, 4), 2, byrow = TRUE)
    res <- xtext(m)
    expect_identical(res$labels[2], NA_character_)
    res <- xtext(matrix(NA_real_, 2, 2))
    expect_true(all(is.na(res$labels)))
    expect_true(is.finite(res$cex))
  })
})

test_that("xtext list method matches the matrix path", {
  with_null_device({
    vals <- as.numeric(1:12)
    d <- c(4L, 3L)
    l <- structure(list(vals), dimension = d, extent = c(0, 4, 0, 3))
    res_l <- xtext(l, cex = 1)
    res_m <- xtext(matrix(vals, d[2L], byrow = TRUE), cex = 1)
    expect_identical(res_l, res_m)
    ## gis attribute form
    gis <- list(type = "raster", bbox = c(0, 0, 4, 3), dim = c(4L, 3L, 1L),
                srs = "x", datatype = "Int16")
    res_g <- xtext(structure(list(vals), gis = gis), cex = 1)
    expect_identical(res_g, res_m)
  })
})

test_that("xtext errors on vector input and lists without dimension", {
  with_null_device({
    expect_error(xtext(1:10))
    expect_error(xtext(list(1:12)), "dimension")
  })
})
