#' Slices of an array
#'
#' `xslices()` lists the slices of an array that [ximage()] can draw: one
#' row per combination of the non-spatial dimensions, with a column of
#' indices for each and a "_label" column with its coordinate value where
#' known. Only metadata is read. A row can be given to `slice` in [ximage()]
#' and friends.
#'
#' `xpanels()` draws several slices side by side with one colour scale (the
#' range of all the slices drawn, unless 'zlim' or 'breaks' is given), each
#' titled by its slice. Give the slices as a list with several indices along
#' one dimension, e.g. `list(time = 1:6)`, or as rows of `xslices()` output.
#' The default is the first slices along the first non-spatial dimension.
#'
#' @inheritParams as_ximage
#' @param x an array, or anything [ximage()] accepts
#' @param slice slices to draw, see Details
#' @param max_panels the most panels drawn by default
#' @param main panel titles, default from the slice and its label
#' @param mfrow panel layout, default from [grDevices::n2mfrow()]
#' @param ... passed to plot for each panel
#' @inheritParams ximage
#' @return `xslices()` a data frame (with zero columns for data with no
#'   dimensions to slice); `xpanels()` invisibly, a list of what [ximage()]
#'   returns for each panel and the 'zlim' used
#' @export
#' @examples
#' a <- array(c(volcano, volcano * 0.9, volcano * 0.8, volcano * 0.7),
#'            c(y = 87, x = 61, time = 4))
#' xslices(a)
#' xpanels(a)
#' xpanels(a, slice = list(time = c(1, 4)))
xslices <- function(x, xy = NULL) {
  src <- .source_of(x)
  if (src$kind == "image") return(data.frame(row.names = 1L))
  nd <- src$nd
  sp <- .find_xy(nd, xy, verbose = FALSE)
  other <- setdiff(seq_along(nd$dim), sp)
  if (!length(other)) return(data.frame(row.names = 1L))
  nm <- names(nd$dim)
  if (prod(as.numeric(nd$dim[other])) > 1e6) {
    stop("more than a million slices, choose them with 'slice' directly", call. = FALSE)
  }
  out <- expand.grid(lapply(nd$dim[other], seq_len), KEEP.OUT.ATTRS = FALSE)
  for (k in other) {
    v <- nd$coords[[k]]
    if (is.null(v)) next
    u <- if (length(nd$units) >= k) nd$units[[k]] else ""
    out[[paste0(nm[k], "_label")]] <- .coord_label(v, u, nd$calendar)[out[[nm[k]]]]
  }
  attr(out, "xy") <- nm[sp]
  out
}

#' @rdname xslices
#' @export
xpanels <- function(x, slice = NULL, ..., zlim = NULL, xy = NULL, coords = NULL,
                    col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL,
                    alpha = NULL, na.col = "transparent", main = NULL,
                    mfrow = NULL, max_panels = 16L) {
  rows <- .panel_rows(x, slice, xy, max_panels)
  ds <- lapply(rows, function(s) {
    as_ximage(x, slice = s, xy = xy, coords = coords, bands = FALSE, verbose = FALSE)
  })
  if (is.null(zlim) && is.null(breaks)) {
    zs <- unlist(lapply(ds, function(d) .zlim_of(d$data)))
    if (length(zs)) zlim <- range(zs)
  }
  n <- length(ds)
  if (is.null(mfrow)) mfrow <- grDevices::n2mfrow(n)
  op <- graphics::par(mfrow = mfrow, mar = c(2, 2, 2, 0.5))
  on.exit(graphics::par(op))
  if (is.null(main)) {
    main <- vapply(seq_len(n), function(i) {
      s <- ds[[i]]$slice
      .format_slice(s[names(rows[[i]])], ds[[i]]$labels)
    }, "")
  }
  main <- rep_len(main, n)
  res <- lapply(seq_len(n), function(i) {
    .draw_image(ds[[i]], zlim = zlim, ..., col = col, breaks = breaks,
                alpha = alpha, na.col = na.col, main = main[i])
  })
  invisible(list(panels = res, zlim = zlim))
}

## a list of single-slice specifications (named lists)
.panel_rows <- function(x, slice, xy, max_panels) {
  if (is.data.frame(slice)) {
    idx <- slice[setdiff(names(slice), grep("_label$", names(slice), value = TRUE))]
    return(lapply(seq_len(nrow(idx)), function(i) as.list(idx[i, , drop = FALSE])))
  }
  if (is.null(slice)) {
    s <- xslices(x, xy = xy)
    if (!ncol(s)) stop("no dimensions to make panels from", call. = FALSE)
    d1 <- names(s)[1L]
    n <- max(s[[d1]])
    if (n > max_panels) {
      message(sprintf("drawing the first %d of %d slices along '%s', choose with 'slice'",
                      max_panels, n, d1))
    }
    slice <- stats::setNames(list(seq_len(min(n, max_panels))), d1)
  }
  slice <- as.list(slice)
  if (is.null(names(slice)) || !all(nzchar(names(slice)))) {
    stop("panels need a named 'slice', e.g. list(time = 1:4)", call. = FALSE)
  }
  multi <- names(slice)[lengths(slice) > 1L]
  if (length(multi) > 1L) {
    stop("panels go along one dimension, give several indices for one only", call. = FALSE)
  }
  if (!length(multi)) return(list(slice))
  lapply(slice[[multi]], function(i) {
    s <- slice
    s[[multi]] <- i
    s
  })
}
