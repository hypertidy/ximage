## unit tests for the internal to_hex() colour converter
to_hex <- ximage:::to_hex

test_that("numeric matrix maps to a hex matrix of the same shape", {
  m <- matrix(1:12, 3)
  h <- to_hex(m)
  expect_type(h, "character")
  expect_identical(dim(h), dim(m))
  expect_true(all(grepl("^#", h)))
})

test_that("constant matrices do not produce NaN colours (issue 4)", {
  h <- to_hex(matrix(5, 4, 4))
  expect_true(all(grepl("^#", h)))
  expect_length(unique(as.vector(h)), 1L)
})

test_that("NA, NaN, and all-missing input map to na.col (issue 4)", {
  expect_true(all(to_hex(matrix(NA_real_, 3, 3)) == "transparent"))
  h <- to_hex(matrix(c(1, NA, NaN, 4), 2), na.col = "hotpink")
  expect_identical(h[2, 1], "hotpink")
  expect_identical(h[1, 2], "hotpink")
  expect_true(grepl("^#", h[1, 1]))
  ## single finite value among NA
  h <- to_hex(matrix(c(1, NA, NA, NA), 2))
  expect_identical(sum(h == "transparent"), 3L)
  expect_true(grepl("^#", h[1, 1]))
})

test_that("breaks bin values, colours interpolate to fit (issue 10)", {
  h <- to_hex(matrix(c(0.5, 1.5, 5, 9), 2), col = c("black", "white"),
              breaks = c(0, 1, 2, 10))
  expect_length(unique(as.vector(h)), 3L)
  expect_identical(h[1, 2], h[2, 2])
  ## values outside user breaks clamp to the end bins
  h <- to_hex(matrix(c(-5, 50), 1), col = c("black", "gray", "white"),
              breaks = c(0, 1, 2, 10))
  expect_identical(h[1, 1], "black")
  expect_identical(h[1, 2], "white")
})

test_that("zlim anchors the palette and masks outside values", {
  cols <- c("blue", "red")
  h1 <- to_hex(matrix(c(0, 10), 1), col = cols, zlim = c(0, 100))
  h2 <- to_hex(matrix(c(0, 10, 200), 1), col = cols, zlim = c(0, 100),
               na.col = "green")
  expect_identical(h1[1, 1:2], h2[1, 1:2])
  expect_identical(h2[1, 3], "green")
})

test_that("col may be a palette function", {
  h <- to_hex(matrix(1:4, 2),
              col = grDevices::colorRampPalette(c("black", "white")))
  expect_true(all(grepl("^#", h)))
})

test_that("RGB scaling is autodetected for 0-1, 0-255, and arbitrary ranges", {
  set.seed(1)
  a3 <- array(runif(24), c(2, 4, 3))
  h1 <- to_hex(a3)
  h255 <- to_hex(a3 * 255)
  expect_identical(substr(h1, 1, 7), substr(h255, 1, 7))
  expect_true(all(grepl("^#", to_hex(a3 * 10000 - 5000))))
})

test_that("NaN in RGB bands maps to na.col without error (issue 7)", {
  set.seed(1)
  a3 <- array(runif(24), c(2, 4, 3))
  a3[1, 1, 2] <- NaN
  h <- to_hex(a3)
  expect_identical(h[1, 1], "transparent")
  expect_true(all(grepl("^#", h[-1])))
})

test_that("constant alpha applies to RGB and multiplies RGBA (issue 15)", {
  set.seed(1)
  a3 <- array(runif(24), c(2, 4, 3))
  h <- to_hex(a3, alpha = 0.5)
  expect_true(all(substr(h, 8, 9) == "80"))
  a4 <- array(runif(32), c(2, 4, 4))
  a4[, , 4] <- 1
  h <- to_hex(a4, alpha = 0.25)
  expect_true(all(substr(h, 8, 9) == "40"))
})

test_that("an opaque 0-255 alpha band does not skew the colour stretch", {
  set.seed(1)
  a3 <- array(runif(24), c(2, 4, 3))
  a4 <- array(0, c(2, 4, 4))
  a4[, , 1:3] <- a3 * 255
  a4[, , 4] <- 255
  h <- to_hex(a4)
  expect_identical(substr(h, 1, 7), substr(to_hex(a3), 1, 7))
  expect_true(all(substr(h, 8, 9) == "FF"))
})

test_that("zlim warns and is ignored for multi-band input", {
  a3 <- array(runif(24), c(2, 4, 3))
  expect_warning(to_hex(a3, zlim = c(0, 1)), "zlim")
})

test_that("2-band input is grey plus alpha (issue 13)", {
  ga <- array(c(runif(8), rep(c(0, 1), 4)), c(2, 4, 2))
  h <- to_hex(ga)
  expect_true(all(substr(h, 2, 3) == substr(h, 4, 5)))
  expect_true(all(substr(h, 8, 9) %in% c("00", "FF")))
})

test_that("raw, logical, and depth-1 array inputs work", {
  r <- as.raw(0:255)
  dim(r) <- c(16, 16)
  expect_identical(dim(to_hex(r)), c(16L, 16L))
  expect_true(all(grepl("^#", to_hex(matrix(c(TRUE, FALSE), 2, 2)))))
  expect_identical(dim(to_hex(array(1:6, c(2, 3, 1)))), c(2L, 3L))
})

test_that("character input passes through, NA -> na.col, alpha applied", {
  cm <- matrix(c("red", NA, "#00FF00", "#0000FF80"), 2)
  h <- to_hex(cm, alpha = 0.5)
  expect_identical(h[2, 1], "transparent")
  expect_identical(substr(h[1, 1], 8, 9), "80")
  expect_identical(substr(h[2, 2], 8, 9), "40")
})

test_that("unsupported inputs error clearly", {
  expect_error(to_hex(1:10))
  expect_error(to_hex(array(1, c(2, 2, 2, 2))))
  expect_error(to_hex(array(1, c(2, 2, 5))))
})
