#' Draw a triangle or quad mesh
#'
#' Draw the faces of a mesh, filled by a value on its vertices or faces, with
#' the same colour mapping as [ximage()] ('col', 'breaks', 'zlim', 'alpha',
#' 'na.col'), so a mesh and an image can share a colour scale.
#'
#' A mesh is vertices (x, y), faces (triangles or quads, 1-based vertex
#' indices) and optionally values. These inputs are recognised by their
#' structure, no extra packages are needed to draw them:
#' \itemize{
#'   \item `cdtr::cdt_triangulate()` output (and RTriangle): `P`, `T`, with
#'   per-vertex attributes `PA` and per-triangle `depth`, and constraint
#'   edges `S`
#'   \item `laridae::lari_triangulate()` or `laridae::lari_tables()` output:
#'   `vertices` (x, y and attribute columns), `triangles` (v0, v1, v2, depth)
#'   and `segments` (a live `laridae_mesh` handle is drawn with
#'   `xmesh(laridae::lari_tables(m))`)
#'   \item rgl 'mesh3d' objects (from quadmesh, anglr, textures): `vb`, `it`,
#'   `ib`, with `material$color` and the z coordinate as values
#'   \item vertices as a two-column matrix, a data frame with x and y, or
#'   `list(x, y)`, with faces in `index` (3 or 4 columns)
#' }
#'
#' `value` can be a vector with one value per vertex or one per face (when
#' the counts are equal it is taken per vertex), the name of a vertex or face
#' attribute (e.g. "depth" for cdtr and laridae, and "x" and "y" for any
#' mesh), colours, or a single fill colour. With no value the mesh is drawn as outlines.
#'
#' The "polygon" method draws every face as a polygon in one call, vertex
#' values are averaged to their faces (flat shading). The "raster" method
#' locates each pixel centre of the plot in the mesh and interpolates vertex
#' values linearly across its triangle (quads are split in two), then draws
#' the result with [ximage()]: smooth shading, and the cost is in pixels
#' not faces, so it suits big meshes. Face values are constant on each face,
#' pixels outside the mesh are 'na.col'. It needs the cdtr package (>= 0.2.0,
#' for `cdtr::cdt_locate()`), and falls back to "polygon" with a message.
#'
#' Faces with a vertex that is not finite are dropped, so a projected mesh
#' can mark vertices that fail (or faces to remove, such as those crossing
#' the dateline) with `NA`.
#'
#' @param x a mesh, see Details
#' @param value values to fill by, see Details
#' @param index faces when `x` is bare vertices, a matrix of 3 (triangles) or 4
#'   (quads) columns of 1-based vertex indices
#' @param add add to plot, or start afresh
#' @param ... passed to plot when `add = FALSE`
#' @param border outline colour of the faces, default none when filled and
#'   the foreground colour when not
#' @param method "polygon" (default) or "raster", see Details
#' @param segments draw the mesh's constraint edges (cdtr `S`, laridae
#'   `segments`), `TRUE` or a colour
#' @param dimension for `method = "raster"`, the pixel grid (ncol, nrow),
#'   default is the plot region's size on the device
#' @inheritParams ximage
#' @return invisibly, a list with 'vertices', 'faces', 'col' (face colours,
#'   or the colour matrix drawn by the "raster" method) and 'zlim'
#' @export
#' @examples
#' ## a quad mesh of volcano's cells, as bare vertices and index
#' nr <- nrow(volcano); nc <- ncol(volcano)
#' xy <- cbind(rep(0:nc, each = nr + 1), rep(nr:0, nc + 1))
#' i <- rep(seq_len(nc), each = nr); j <- rep(seq_len(nr), nc)
#' quads <- cbind((i - 1) * (nr + 1) + j, i * (nr + 1) + j,
#'                i * (nr + 1) + j + 1, (i - 1) * (nr + 1) + j + 1)
#' xmesh(xy, index = quads, value = as.vector(volcano), asp = 1)
#'
#' ## a triangulation, coloured by a per-vertex value
#' if (requireNamespace("cdtr", quietly = TRUE)) {
#'   set.seed(1)
#'   pts <- cbind(runif(300), runif(300))
#'   tri <- cdtr::cdt_triangulate(pts[, 1], pts[, 2])
#'   z <- sin(tri$P[, 1] * 6) + cos(tri$P[, 2] * 6)
#'   xmesh(tri, value = z, border = "grey40", asp = 1)
#'   xmesh(tri, value = z, method = "raster", asp = 1)
#' }
xmesh <- function(x, value = NULL, index = NULL, add = FALSE, ...,
                  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                  zlim = NULL, alpha = NULL, na.col = "transparent",
                  border = NULL, method = c("polygon", "raster"),
                  segments = FALSE, dimension = NULL, xlab = NULL, ylab = NULL) {
  UseMethod("xmesh")
}

#' @export
xmesh.default <- function(x, value = NULL, index = NULL, add = FALSE, ...,
                          col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                          zlim = NULL, alpha = NULL, na.col = "transparent",
                          border = NULL, method = c("polygon", "raster"),
                          segments = FALSE, dimension = NULL,
                          xlab = NULL, ylab = NULL) {
  method <- match.arg(method)
  m <- .as_mesh(x, index)
  v <- .mesh_values(m, value)
  vx <- m$xy[, 1L]
  vy <- m$xy[, 2L]
  if (is.null(border)) border <- if (v$type == "none") graphics::par("fg") else NA
  zl <- if (is.null(v$values)) NULL else .zlim_of(matrix(v$values), zlim, breaks)

  if (!add) {
    if (is.null(xlab)) xlab <- ""
    if (is.null(ylab)) ylab <- ""
    ok <- is.finite(vx) & is.finite(vy)
    plot(range(vx[ok]), range(vy[ok]), type = "n", ..., xaxs = "i", yaxs = "i",
         xlab = xlab, ylab = ylab)
  }

  if (method == "raster" && v$type != "none" &&
      !(requireNamespace("cdtr", quietly = TRUE) &&
        "cdt_locate" %in% getNamespaceExports("cdtr"))) {
    message("method = \"raster\" needs the cdtr package (>= 0.2.0), drawing polygons instead")
    method <- "polygon"
  }

  if (method == "raster" && v$type != "none") {
    img <- .mesh_raster(m, v, dimension)
    ximage(img$data, extent = img$extent, add = TRUE, zlim = zl, col = col,
           breaks = breaks, alpha = alpha, na.col = na.col)
    out_col <- img$data
    if (!all(is.na(border))) {
      for (k in seq_along(m$faces)) {
        .draw_faces(vx, vy, m$faces[[k]], fill = NA, border = border)
      }
    }
  } else {
    fill <- .face_colours(m, v, col = col, breaks = breaks, zlim = zlim,
                          alpha = alpha, na.col = na.col)
    off <- 0L
    for (k in seq_along(m$faces)) {
      f <- m$faces[[k]]
      ii <- off + seq_len(nrow(f))
      .draw_faces(vx, vy, f, fill = if (is.null(fill)) NA else fill[ii],
                  border = border,
                  drop_transparent = all(is.na(border)))
      off <- off + nrow(f)
    }
    out_col <- fill
  }

  if (!isFALSE(segments) && !is.null(m$segments) && nrow(m$segments)) {
    sc <- if (isTRUE(segments)) graphics::par("fg") else segments
    s <- m$segments
    graphics::segments(vx[s[, 1L]], vy[s[, 1L]], vx[s[, 2L]], vy[s[, 2L]], col = sc)
  }
  invisible(list(vertices = m$xy, faces = m$faces, col = out_col, zlim = zl))
}

## -------------------------------------------------------------------------
## decoding meshes
## -------------------------------------------------------------------------

## list(xy (n x 2), faces (list of index matrices, 3 or 4 columns), vattr,
## fattr (named lists of per-vertex and per-face columns, faces in order),
## segments (k x 2 or NULL), vcol, fcol (colours or NULL), default)
.as_mesh <- function(x, index = NULL) {
  if (inherits(x, "laridae_mesh")) {
    stop("a laridae_mesh is a live handle, draw its tables: xmesh(laridae::lari_tables(m))",
         call. = FALSE)
  }
  if (inherits(x, "mesh3d") || (is.list(x) && !is.null(x$vb) &&
                                (!is.null(x$it) || !is.null(x$ib)))) {
    return(.mesh_mesh3d(x))
  }
  if (is.list(x) && all(c("P", "T") %in% names(x))) {
    return(.mesh_pt(x))
  }
  if (is.list(x) && all(c("vertices", "triangles") %in% names(x))) {
    return(.mesh_laridae(x))
  }
  if (is.null(index)) {
    stop("not a recognised mesh: give vertices in 'x' and faces in 'index', see ?xmesh",
         call. = FALSE)
  }
  xy <- .mesh_xy(x)
  f <- .mesh_index(index, nrow(xy))
  .mesh(xy, list(f))
}

.mesh <- function(xy, faces, vattr = list(), fattr = list(), segments = NULL,
                  vcol = NULL, fcol = NULL, default = NULL) {
  faces <- faces[!vapply(faces, is.null, logical(1L))]
  ## vertex coordinates are attributes too, value = "x" colours by x
  if (is.null(vattr$x)) vattr$x <- xy[, 1L]
  if (is.null(vattr$y)) vattr$y <- xy[, 2L]
  list(xy = xy, faces = faces, vattr = vattr, fattr = fattr, segments = segments,
       vcol = vcol, fcol = fcol, default = default)
}

.mesh_xy <- function(x) {
  if (is.data.frame(x) && all(c("x", "y") %in% names(x))) {
    return(cbind(as.numeric(x$x), as.numeric(x$y)))
  }
  if (is.list(x) && !is.data.frame(x) && length(x) >= 2L) {
    if (all(c("x", "y") %in% names(x))) return(cbind(as.numeric(x$x), as.numeric(x$y)))
    return(cbind(as.numeric(x[[1L]]), as.numeric(x[[2L]])))
  }
  x <- as.matrix(x)
  if (ncol(x) < 2L) stop("vertices need two columns, x and y", call. = FALSE)
  cbind(as.numeric(x[, 1L]), as.numeric(x[, 2L]))
}

.mesh_index <- function(index, n) {
  f <- as.matrix(index)
  if (!ncol(f) %in% c(3L, 4L)) {
    stop("'index' must have 3 (triangles) or 4 (quads) columns", call. = FALSE)
  }
  storage.mode(f) <- "integer"
  if (any(f < 1L, na.rm = TRUE)) {
    stop("'index' is 1-based, add 1 to 0-based vertex indices", call. = FALSE)
  }
  if (any(f > n, na.rm = TRUE)) stop("'index' refers to vertices that don't exist", call. = FALSE)
  f
}

## cdtr cdt_triangulate(), RTriangle triangulate()
.mesh_pt <- function(x) {
  xy <- .mesh_xy(x$P)
  tri <- .mesh_index(x$T[, 1:3, drop = FALSE], nrow(xy))
  vattr <- list()
  if (!is.null(x$PA) && length(x$PA)) {
    pa <- as.matrix(x$PA)
    if (nrow(pa) == nrow(xy)) {
      cn <- colnames(pa)
      if (is.null(cn)) cn <- paste0("PA", seq_len(ncol(pa)))
      for (k in seq_len(ncol(pa))) vattr[[cn[k]]] <- pa[, k]
    }
  }
  fattr <- list()
  for (nm in c("depth", "depth_distinct")) {
    if (!is.null(x[[nm]]) && length(x[[nm]]) == nrow(tri)) fattr[[nm]] <- x[[nm]]
  }
  seg <- if (!is.null(x$S) && length(x$S)) {
    s <- as.matrix(x$S)[, 1:2, drop = FALSE]
    storage.mode(s) <- "integer"
    s
  }
  .mesh(xy, list(tri), vattr = vattr, fattr = fattr, segments = seg)
}

## laridae lari_triangulate(), lari_tables()
.mesh_laridae <- function(x) {
  v <- x$vertices
  t <- x$triangles
  xy <- cbind(as.numeric(v$x), as.numeric(v$y))
  tri <- .mesh_index(cbind(t$v0, t$v1, t$v2), nrow(xy))
  num <- function(df, skip) {
    keep <- setdiff(names(df)[vapply(df, is.numeric, logical(1L))], skip)
    lapply(df[keep], as.numeric)
  }
  vattr <- num(v, c("x", "y"))
  fattr <- num(t, c("v0", "v1", "v2"))
  s <- x$segments
  seg <- if (!is.null(s) && NROW(s)) cbind(as.integer(s$v0), as.integer(s$v1))
  .mesh(xy, list(tri), vattr = vattr, fattr = fattr, segments = seg)
}

## rgl mesh3d (quadmesh, anglr, textures)
.mesh_mesh3d <- function(x) {
  vb <- x$vb
  w <- if (nrow(vb) >= 4L) vb[4L, ] else 1
  xy <- cbind(vb[1L, ] / w, vb[2L, ] / w)
  n <- nrow(xy)
  tri <- if (!is.null(x$it) && length(x$it)) .mesh_index(t(x$it), n)
  quad <- if (!is.null(x$ib) && length(x$ib)) .mesh_index(t(x$ib), n)
  vattr <- list()
  default <- NULL
  if (nrow(vb) >= 3L) {
    z <- vb[3L, ] / w
    vattr$z <- z
    if (length(unique(z[is.finite(z)])) > 1L) default <- "z"
  }
  nf <- NROW(tri) + NROW(quad)
  vcol <- fcol <- NULL
  cl <- x$material$color
  if (!is.null(cl) && length(cl) > 1L) {
    if (length(cl) == n) vcol <- cl else if (length(cl) == nf) fcol <- cl
  }
  .mesh(xy, list(tri, quad), vattr = vattr, vcol = vcol, fcol = fcol,
        default = default)
}

## resolve 'value': list(type = "vertex"/"face"/"none", values, colours)
.mesh_values <- function(m, value) {
  nv <- nrow(m$xy)
  nf <- sum(vapply(m$faces, nrow, integer(1L)))
  if (is.null(value)) {
    if (!is.null(m$vcol)) return(list(type = "vertex", values = NULL, colours = m$vcol))
    if (!is.null(m$fcol)) return(list(type = "face", values = NULL, colours = m$fcol))
    if (is.null(m$default)) return(list(type = "none", values = NULL, colours = NULL))
    value <- m$default
  }
  if (is.character(value) && length(value) == 1L) {
    if (value %in% names(m$vattr)) {
      return(list(type = "vertex", values = m$vattr[[value]], colours = NULL))
    }
    if (value %in% names(m$fattr)) {
      return(list(type = "face", values = m$fattr[[value]], colours = NULL))
    }
    if (!value %in% grDevices::colours() && !grepl("^#", value)) {
      stop(sprintf("'value' \"%s\" is not an attribute of the mesh (%s) or a colour",
                   value, paste(c(names(m$vattr), names(m$fattr)), collapse = ", ")),
           call. = FALSE)
    }
    return(list(type = "face", values = NULL, colours = rep(value, nf)))
  }
  type <- if (length(value) == nv) "vertex" else if (length(value) == nf) "face" else
    stop(sprintf("'value' must have one value per vertex (%d) or per face (%d)", nv, nf),
         call. = FALSE)
  if (is.character(value)) return(list(type = type, values = NULL, colours = value))
  if (is.factor(value)) value <- as.integer(value)
  list(type = type, values = as.numeric(value), colours = NULL)
}

## one colour per face, in face order (NULL when unfilled)
.face_colours <- function(m, v, col, breaks, zlim, alpha, na.col) {
  if (v$type == "none") return(NULL)
  per_face <- function(vals) {
    unlist(lapply(m$faces, function(f) rowMeans(matrix(vals[f], ncol = ncol(f)))),
           use.names = FALSE)
  }
  if (!is.null(v$colours)) {
    cl <- v$colours
    if (v$type == "vertex") {
      rgba <- grDevices::col2rgb(cl, alpha = TRUE) / 255
      cl <- grDevices::rgb(per_face(rgba[1L, ]), per_face(rgba[2L, ]),
                           per_face(rgba[3L, ]), per_face(rgba[4L, ]))
    }
    return(as.vector(to_hex(matrix(cl), alpha = alpha, na.col = na.col)))
  }
  fv <- if (v$type == "vertex") per_face(v$values) else v$values
  if (!any(is.finite(fv))) return(rep(na.col, length(fv)))
  as.vector(to_hex(matrix(fv), col = col, breaks = breaks,
                   zlim = if (is.null(zlim)) range(v$values, finite = TRUE) else zlim,
                   na.col = na.col, alpha = alpha))
}

## pixel size of the plot region
.plot_pixels <- function() {
  px <- grDevices::dev.size("px")
  plt <- graphics::par("plt")
  as.integer(pmax(1, round(px * c(diff(plt[1:2]), diff(plt[3:4])))))
}

## interpolate (vertex) or locate (face) values onto the plot's pixel grid
.mesh_raster <- function(m, v, dimension = NULL) {
  tris <- lapply(m$faces, function(f) {
    if (ncol(f) == 3L) f else rbind(f[, c(1L, 2L, 3L), drop = FALSE],
                                    f[, c(1L, 3L, 4L), drop = FALSE])
  })
  ## face ids follow the triangles, quads give two each
  fid <- unlist(lapply(seq_along(m$faces), function(k) {
    f <- m$faces[[k]]
    off <- sum(vapply(m$faces[seq_len(k - 1L)], nrow, integer(1L)))
    ids <- off + seq_len(nrow(f))
    if (ncol(f) == 3L) ids else c(ids, ids)
  }))
  tri <- do.call(rbind, tris)
  xy <- m$xy
  okv <- is.finite(xy[, 1L]) & is.finite(xy[, 2L])
  okt <- rowSums(matrix(!okv[tri], ncol = 3L)) == 0L
  tri <- tri[okt, , drop = FALSE]
  fid <- fid[okt]
  used <- sort(unique(as.vector(tri)))
  remap <- integer(nrow(xy))
  remap[used] <- seq_along(used)
  P <- xy[used, , drop = FALSE]
  tri[] <- remap[tri]

  usr <- graphics::par("usr")
  if (is.null(dimension)) dimension <- .plot_pixels()
  nx <- as.integer(dimension[1L])
  ny <- as.integer(dimension[2L])
  xs <- usr[1L] + (seq_len(nx) - 0.5) * (usr[2L] - usr[1L]) / nx
  ys <- usr[4L] - (seq_len(ny) - 0.5) * (usr[4L] - usr[3L]) / ny
  xq <- rep(xs, ny)
  yq <- rep(ys, each = nx)

  ## locate every pixel centre (NA outside the mesh, cdt_interpolate() would
  ## extrapolate), then weight the vertex values by barycentric coordinates
  loc <- cdtr::cdt_locate(P, tri, xq, yq, barycentric = TRUE)
  hit <- loc$triangle
  if (v$type == "vertex") {
    inside <- !is.na(hit)
    th <- tri[hit[inside], , drop = FALSE]
    bw <- as.matrix(loc[inside, c("b1", "b2", "b3")])
    blend <- function(a) {
      a <- a[used]
      out <- rep(NA_real_, length(hit))
      out[inside] <- rowSums(bw * matrix(a[th], ncol = 3L))
      out
    }
    if (!is.null(v$colours)) {
      rgba <- grDevices::col2rgb(v$colours, alpha = TRUE)
      ch <- lapply(1:4, function(k) blend(rgba[k, ]))
      vals <- rep(NA_character_, length(hit))
      vals[inside] <- grDevices::rgb(ch[[1L]][inside], ch[[2L]][inside],
                                     ch[[3L]][inside], ch[[4L]][inside],
                                     maxColorValue = 255)
    } else {
      vals <- blend(v$values)
    }
  } else {
    src <- if (!is.null(v$colours)) v$colours else v$values
    vals <- src[fid[hit]]
  }
  list(data = matrix(vals, ny, nx, byrow = TRUE), extent = usr)
}
