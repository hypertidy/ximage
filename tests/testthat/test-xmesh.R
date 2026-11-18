## xmesh(): mesh decoding, values, polygon and raster drawing

with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(code)
}

## two triangles making the unit square, and the same as quads
sq_xy <- cbind(c(0, 1, 1, 0), c(0, 0, 1, 1))
sq_tri <- rbind(c(1, 2, 3), c(1, 3, 4))

test_that("bare vertices and index draw, values per vertex or per face", {
  with_null_device({
    res <- xmesh(sq_xy, index = sq_tri)
    expect_identical(res$faces[[1L]], matrix(as.integer(sq_tri), 2))
    expect_null(res$col)  ## outlines only
    res <- xmesh(sq_xy, index = sq_tri, value = c(0, 1, 2, 3))
    expect_length(res$col, 2L)
    expect_identical(res$zlim, c(0, 3))
    res <- xmesh(sq_xy, index = sq_tri, value = c(10, 20))
    expect_length(unique(res$col), 2L)
    ## data frame and list vertices, quads
    expect_no_error(xmesh(data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)),
                          index = matrix(1:4, 1), value = 1))
    expect_no_error(xmesh(list(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)),
                          index = matrix(1:4, 1), value = "hotpink"))
  })
})

test_that("vertex and face values share ximage's colour mapping", {
  with_null_device({
    res <- xmesh(sq_xy, index = sq_tri, value = c(1, 1.5), zlim = c(0, 4),
                 col = c("black", "white"))
    expect_identical(res$col, c("black", "black"))
    res <- xmesh(sq_xy, index = sq_tri, value = c(1, 3), breaks = c(0, 2, 4),
                 col = c("black", "white"))
    expect_identical(res$col, c("black", "white"))
  })
})

test_that("faces with a non-finite vertex are dropped", {
  with_null_device({
    plot(0:1, 0:1, type = "n")
    xy <- sq_xy
    xy[4, ] <- NA
    expect_identical(.draw_faces(xy[, 1], xy[, 2], sq_tri, fill = "red"), 1L)
    expect_identical(.draw_faces(sq_xy[, 1], sq_xy[, 2], sq_tri, fill = "red"), 2L)
    expect_identical(.draw_faces(sq_xy[, 1], sq_xy[, 2], sq_tri,
                                 fill = c("red", "transparent"), drop_transparent = TRUE), 1L)
  })
})

test_that("cdtr-style, laridae-style and mesh3d-style lists are recognised", {
  cd <- list(P = sq_xy, T = sq_tri, PA = cbind(z = c(1, 2, 3, 4)), depth = c(1L, 1L),
             S = rbind(c(1, 2), c(2, 3)))
  m <- .as_mesh(cd)
  expect_identical(names(m$vattr), c("z", "x", "y"))
  expect_identical(names(m$fattr), "depth")
  expect_identical(dim(m$segments), c(2L, 2L))
  lr <- list(vertices = data.frame(x = sq_xy[, 1], y = sq_xy[, 2], h = 4:1,
                                   origin = "input", id = 1:4),
             triangles = data.frame(v0 = c(1, 1), v1 = c(2, 3), v2 = c(3, 4), depth = 1L),
             segments = data.frame(v0 = 1, v1 = 2, segment = 1, count = 1))
  m <- .as_mesh(lr)
  expect_true(all(c("h", "id", "x", "y") %in% names(m$vattr)))
  expect_identical(names(m$fattr), "depth")
  m3 <- structure(list(vb = rbind(t(sq_xy), c(0, 1, 2, 3), 1), ib = matrix(1:4, 4),
                       material = list()), class = c("mesh3d", "shape3d"))
  m <- .as_mesh(m3)
  expect_identical(m$default, "z")
  expect_identical(ncol(m$faces[[1L]]), 4L)
  with_null_device({
    expect_no_error(xmesh(cd, value = "z", segments = TRUE))
    expect_no_error(xmesh(cd, value = "depth"))
    expect_no_error(xmesh(lr, value = "h", segments = "red"))
    res <- xmesh(m3)
    expect_length(res$col, 1L)
    m3$material$color <- c("red", "green", "blue", "white")
    expect_no_error(xmesh(m3))
  })
})

test_that("mesh input errors are clear", {
  expect_error(.as_mesh(sq_xy), "not a recognised mesh")
  expect_error(.as_mesh(sq_xy, index = sq_tri - 1L), "1-based")
  expect_error(.as_mesh(sq_xy, index = sq_tri[, 1:2]), "3 \\(triangles\\) or 4")
  expect_error(.as_mesh(structure(list(), class = "laridae_mesh")), "lari_tables")
  m <- .as_mesh(sq_xy, index = sq_tri)
  expect_error(.mesh_values(m, 1:3), "per vertex \\(4\\) or per face \\(2\\)")
  expect_error(.mesh_values(m, "nope"), "not an attribute")
})

test_that("raster method interpolates inside the mesh only", {
  skip_if_not_installed("cdtr")
  skip_if_not("cdt_locate" %in% getNamespaceExports("cdtr"))
  with_null_device({
    plot(c(-1, 2), c(-1, 2), type = "n", xaxs = "i", yaxs = "i")
    res <- xmesh(sq_xy, index = sq_tri, value = c(0, 1, 2, 3), method = "raster",
                 add = TRUE, dimension = c(30, 30))
    v <- quadmesh_free <- res$col
    expect_identical(dim(v), c(30L, 30L))
    ## outside the square is missing, inside within the vertex range
    expect_true(is.na(v[1, 1]) && is.na(v[30, 30]))
    expect_true(all(v[!is.na(v)] >= 0 & v[!is.na(v)] <= 3))
    expect_true(sum(!is.na(v)) > 0.05 * length(v))
    ## face values are constant per face, quads work
    res <- xmesh(sq_xy, index = matrix(1:4, 1), value = 7, method = "raster",
                 add = TRUE, dimension = c(30, 30))
    expect_identical(unique(res$col[!is.na(res$col)]), 7)
    ## per-vertex colours blend
    res <- xmesh(sq_xy, index = sq_tri, value = c("black", "black", "white", "white"),
                 method = "raster", add = TRUE, dimension = c(30, 30))
    expect_true(is.character(res$col))
  })
})

test_that("cdtr and laridae triangulations draw", {
  skip_if_not_installed("cdtr")
  tri <- cdtr::cdt_triangulate(c(0, 1, 1, 0, 0.5), c(0, 0, 1, 1, 0.5))
  with_null_device({
    res <- xmesh(tri, value = tri$P[, 1])
    expect_length(res$col, nrow(tri$T))
  })
  skip_if_not_installed("laridae")
  m <- laridae::lari_triangulate(c(0, 1, 1, 0, 0.5), c(0, 0, 1, 1, 0.5))
  with_null_device({
    res <- xmesh(m, value = "x", border = "grey")
    expect_length(res$col, nrow(m$triangles))
  })
})

test_that("quadmesh mesh3d draws by its z", {
  skip_if_not_installed("quadmesh")
  qm <- quadmesh::quadmesh(volcano)
  with_null_device({
    res <- xmesh(qm)
    expect_length(res$col, ncol(qm$ib))
    expect_identical(res$zlim, range(qm$vb[3, ]))
  })
})
