# nrview.R -- pre-alpha kernel for a live GDAL-backed canvas
#
# The convention: a view is (extent, dim, crs). The affine falls out.
# Background = GDAL read at that extent/dim. Geometry = one wk_transform
# then integer drawing on a nara nativeRaster. Pan/zoom = arithmetic on
# the extent, then repaint. Display = grid.raster inside eventloop, or
# ximage in a plain session.
#
# Requires: wk, vapour, nara, eventloop (x11 only), optionally farver,
#           ximage, ozmaps (demo).
#
# source() this file, then see demo_australia() at the bottom.

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---------------------------------------------------------------------
## pixel packing: hex colours -> nativeRaster packed ints
## ---------------------------------------------------------------------

hex_to_native <- function(hex) {
  if (requireNamespace("farver", quietly = TRUE) &&
      "encode_native" %in% getNamespaceExports("farver")) {
    return(farver::encode_native(hex))
  }
  m <- grDevices::col2rgb(hex, alpha = TRUE)
  ## packed ABGR, alpha in the high byte pushes past .Machine$integer.max,
  ## so pack as double and wrap
  v <- m[1L, ] + m[2L, ] * 256 + m[3L, ] * 65536 + m[4L, ] * 16777216
  big <- v >= 2147483648
  v[big] <- v[big] - 4294967296
  as.integer(v)
}

## hex pixels in raster order (top-left first, row-major) -> nativeRaster
hex_to_nr <- function(hex, dim) {
  structure(hex_to_native(hex),
            dim = c(dim[2L], dim[1L]),   # (nrow, ncol) = (height, width)
            class = "nativeRaster")
}

## ---------------------------------------------------------------------
## the view
## ---------------------------------------------------------------------

## extent is c(xmin, xmax, ymin, ymax) in crs; dim is c(ncol, nrow)
nr_view <- function(extent, dim = c(800L, 600L), crs = "EPSG:3857") {
  v <- new.env(parent = emptyenv())
  v$extent  <- as.numeric(extent)
  v$dim     <- as.integer(dim)
  v$crs     <- crs
  v$nr      <- nara::nr_new(dim[1L], dim[2L], "grey90")
  v$layers  <- list()      # painters, each function(v) called in order
  v$dirty   <- TRUE
  class(v)  <- c("nrview", class(v))
  v
}

## the one transform: world -> pixel (raster order, y flips)
world_to_px <- function(v) {
  e <- v$extent
  wk::wk_affine_compose(
    wk::wk_affine_translate(dx = -e[1L], dy = -e[4L]),
    wk::wk_affine_scale(
      scale_x =  v$dim[1L] / (e[2L] - e[1L]),
      scale_y = -v$dim[2L] / (e[4L] - e[3L])
    )
  )
}

## ---------------------------------------------------------------------
## painters (layers)
## ---------------------------------------------------------------------

## dynamic raster background: any GDAL dsn, warped to the view on demand
layer_raster <- function(v, dsn, resample = "bilinear") {
  force(dsn); force(resample)
  v$layers <- c(v$layers, list(function(v) {
    px <- vapour::gdal_raster_image(
      dsn,
      target_ext = v$extent,
      target_dim = v$dim,
      target_crs = v$crs,
      resample   = resample
    )[[1L]]
    ## nr_blit anchors the sprite by its center, so a whole-frame
    ## background goes in via nr_copy_into (same dims, no anchor)
    nara::nr_copy_into(v$nr, hex_to_nr(px, v$dim))
  }))
  v$dirty <- TRUE
  invisible(v)
}

## vector overlay: anything wk_coords() understands (wk, sf, geos, ...)
## geometry must already be in v$crs -- the affine does not reproject
layer_geom <- function(v, g, color = "hotpink", fill = NULL) {
  force(g); force(color); force(fill)
  v$layers <- c(v$layers, list(function(v) {
    xy <- wk::wk_coords(wk::wk_transform(g, world_to_px(v)))
    grp <- interaction(xy$feature_id, xy$part_id %||% 1L,
                       xy$ring_id %||% 1L, drop = TRUE)
    for (idx in split(seq_len(nrow(xy)), grp)) {
      if (!is.null(fill)) {
        nara::nr_polygon(v$nr, xy$x[idx], xy$y[idx], fill = fill,
                         color = color)
      } else {
        nara::nr_polyline(v$nr, xy$x[idx], xy$y[idx], color = color)
      }
    }
  }))
  v$dirty <- TRUE
  invisible(v)
}

## ---------------------------------------------------------------------
## repaint / navigation: pure arithmetic on the extent
## ---------------------------------------------------------------------

repaint <- function(v) {
  nara::nr_fill(v$nr, "grey90")
  for (p in v$layers) p(v)
  v$dirty <- FALSE
  invisible(v)
}

## pan by a fraction of the current view (default a tenth)
pan <- function(v, fx = 0, fy = 0) {
  e  <- v$extent
  dx <- fx * (e[2L] - e[1L])
  dy <- fy * (e[4L] - e[3L])
  v$extent <- e + c(dx, dx, dy, dy)
  v$dirty <- TRUE
  invisible(v)
}

## zoom by factor f (< 1 in, > 1 out) about world point (cx, cy)
zoom <- function(v, f, cx = NULL, cy = NULL) {
  e  <- v$extent
  cx <- cx %||% mean(e[1:2])
  cy <- cy %||% mean(e[3:4])
  v$extent <- c(cx + (e[1:2] - cx) * f, cy + (e[3:4] - cy) * f)
  v$dirty <- TRUE
  invisible(v)
}

## device npc coords (0..1, y up) -> world coords
npc_to_world <- function(v, mx, my) {
  e <- v$extent
  c(e[1L] + mx * (e[2L] - e[1L]),
    e[3L] + my * (e[4L] - e[3L]))
}

## non-interactive display: same picture in a plain session or Rmd
show_view <- function(v) {
  if (v$dirty) repaint(v)
  ximage::ximage(v$nr, extent = v$extent, asp = 1)
  invisible(v)
}

## ---------------------------------------------------------------------
## the event loop
## ---------------------------------------------------------------------
##
## keys:  arrows / hjkl  pan
##        + / =          zoom in (at mouse pointer)
##        - / _          zoom out (at mouse pointer)
##        r              reset to starting extent
##        q / ESC        quit
## mouse: drag to pan (repaint on release; cheap shifted blit while
##        dragging so remote sources stay usable)

run_view <- function(v, fps = 30) {
  home <- v$extent
  drag <- NULL   # list(mx, my, extent) at mouse_down

  repaint(v)

  handler <- function(event, mouse_x, mouse_y, frame_num,
                      fps_actual, fps_target, dev_width, dev_height,
                      ...) {

    if (!is.null(event)) {
      if (event$type == "key_press") {
        key <- event$char %||% event$str %||% ""
        step <- 0.1
        switch(
          key,
          "Left"  = ,
          "h" = pan(v, -step, 0),
          "Right" = ,
          "l" = pan(v,  step, 0),
          "Up"    = ,
          "k" = pan(v, 0,  step),
          "Down"  = ,
          "j" = pan(v, 0, -step),
          "+" = ,
          "=" = { w <- npc_to_world(v, mouse_x, mouse_y)
          zoom(v, 1 / 1.5, w[1L], w[2L]) },
          "-" = ,
          "_" = { w <- npc_to_world(v, mouse_x, mouse_y)
          zoom(v, 1.5, w[1L], w[2L]) },
          "r" = { v$extent <- home; v$dirty <- TRUE },
          "q" = stop("user quit"),
          NULL
        )
        if (identical(event$int, 27L)) stop("user quit")  # ESC
      } else if (event$type == "mouse_down") {
        drag <<- list(mx = mouse_x, my = mouse_y, extent = v$extent)
      } else if (event$type == "mouse_up" && !is.null(drag)) {
        drag <<- NULL
        v$dirty <- TRUE            # full repaint (the real GDAL read)
      }
    }

    if (!is.null(drag)) {
      ## during drag: move the extent, but do not hit GDAL --
      ## draw the stale frame at an offset (the slippy-map illusion,
      ## resolved properly on mouse_up)
      e  <- drag$extent
      dx <- (drag$mx - mouse_x) * (e[2L] - e[1L])
      dy <- (drag$my - mouse_y) * (e[4L] - e[3L])
      v$extent <- e + c(dx, dx, dy, dy)
      grid::grid.raster(
        v$nr, interpolate = FALSE,
        x = grid::unit(0.5 - (drag$mx - mouse_x), "npc"),
        y = grid::unit(0.5 - (drag$my - mouse_y), "npc")
      )
      return(invisible())
    }

    if (v$dirty) repaint(v)
    grid::grid.raster(v$nr, interpolate = FALSE)
  }

  eventloop::run_loop(
    handler,
    fps_target    = fps,
    double_buffer = TRUE,
    width         = v$dim[1L] / 100,
    height        = v$dim[2L] / 100
  )
  invisible(v)
}

## ---------------------------------------------------------------------
## demo sources
## ---------------------------------------------------------------------

## OSM as a GDAL dsn: the general form of "the background is a GDAL read"
## (tiles, COG on S3, VRT-virtualized Zarr -- all the same call)
dsn_osm <- function() {
  paste0(
    "<GDAL_WMS><Service name=\"TMS\">",
    "<ServerUrl>https://tile.openstreetmap.org/${z}/${x}/${y}.png</ServerUrl>",
    "</Service><DataWindow>",
    "<UpperLeftX>-20037508.34</UpperLeftX><UpperLeftY>20037508.34</UpperLeftY>",
    "<LowerRightX>20037508.34</LowerRightX><LowerRightY>-20037508.34</LowerRightY>",
    "<TileLevel>18</TileLevel><TileCountX>1</TileCountX><TileCountY>1</TileCountY>",
    "<YOrigin>top</YOrigin></DataWindow>",
    "<Projection>EPSG:3857</Projection>",
    "<BlockSizeX>256</BlockSizeX><BlockSizeY>256</BlockSizeY>",
    "<BandsCount>3</BandsCount>",
    "<UserAgent>nrview-prealpha</UserAgent>",
    "<Cache/></GDAL_WMS>"
  )
}

demo_australia <- function(interactive = TRUE) {
  v <- nr_view(extent = c(110, 155, -45, -8),
               dim = c(900L, 700L), crs = "EPSG:4326")
  layer_raster(v, dsn_osm())
  if (requireNamespace("ozmaps", quietly = TRUE)) {
    layer_geom(v, ozmaps::ozmap_states, color = "hotpink")
  }
  if (interactive) run_view(v) else show_view(v)
  invisible(v)
}
