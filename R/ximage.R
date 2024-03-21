t0 <- function(x) {
  d <- seq_len(length(dim(x)))
  d[1:2] <- d[2:1]
  aperm(x, d)
}

flip_r <- function(x) {
  dm <- dim(x)
  if (length(dm) == 3L) {
    x[dm[1L]:1L,,]
  } else {
    x[dm[1L]:1L, ]
  }
}
flip_c <- function(x) {
  dm <- dim(x)
  if (length(dm) == 3L) {
    x[,dm[2L]:1L,]
  } else {
    x[,dm[2L]:1L]
  }
}

.rescale <- function(x) {
  rg <- range(x, na.rm = TRUE)
  (x - rg[1L])/diff(rg)
}
.make_hex_matrix <- function(x, cols = NULL, ..., breaks) {
   alpha <- 1
  if (length(dim(x)) > 2) {
    if (dim(x)[3] == 4L) {
      alpha <- x[,,4L]
    }
    out <- matrix(rgb(x[,,1L], x[,,2L], x[,,3L], alpha), dim(x)[1L])
  } else {

    out <- matrix(cols[x * (length(cols) - 1) + 1], dim(x)[1L])
  }
  out
}

# TODO
# dispatch matrix vs array using S3 class
# autodetect maxcolorvalue as 1 or 255, or allow override
# collapse RGB as intensity and allow colour map override
# allow greyscale mode ?
# allow raw mode

#' A new image()

#' Plot an image (no matter what)
#'
#' ximage combines the best of image() and rasterImage().
#'
#' [ximage()] is designed like graphics function with the following limitations removed,
#' `image()` 1:4, `rasterImage()` 5:7.
#'
#'
#' \enumerate{
#'    \item Allow arrays with RGB/A.
#'    \item Allow matrix with character (named colours, or hex) or raw (Byte) values
#'    \item Allow list output from vapour, a list with numeric values, hex character, or nativeRaster
#'    \item Plot in 0,ncol 0,nrow by default
#'    \item Override default with extent (xmin, xmax, ymin, ymax)
#'
#'    \item Allow general numeric values.
#'    \item Start a plot from scratch without setting up a plot to paint to.
#'    \item Plot by default in 0,ncol,0,nrow if unspecified.
#' }
#'
#'  ximage uses the GIS raster default used by rasterImage. WIP: There is a similar function 'image0?'
#'  that provides the same features but assumes that orientation is like image...
#'
#' Colours by 'col' are only mapped for numeric data, this may change (to remap RGB or raw imagery via greyscale conversion)
#'
#' @param x matrix, array, or native raster (nativeRaster, or raster)
#' @param extent optional, numeric xmin,xmax,ymin,ymax
#' @param zlim optional, range of data to set colour map (to maintain absolute colours across multiple plots)
#' @param add add to plot, or start afresh
#' @param ... passed to plot when `add = FALSE`
#' @param xlab x axis label, empty by default
#' @param ylab y axis lable, empty by default
#' @param col optional colours to map matrix/array data to
#' @return a list with 'x' and 'extent' invisibly (extent is the 0,ncol 0,nrow space of the array if not supplied)
#' @export
#' @importFrom grDevices hcl.colors rgb
#' @importFrom graphics rasterImage
#' @examples
#' ximage(volcano)
#' ximage(as.raster(matrix(0:1, 49, 56)))
ximage <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {
  UseMethod("ximage")
}


#' @export
#' @importFrom stats na.omit
ximage.list <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {


  if (all(c("geotransform", "cols", "rows", "driver") %in% names(x))) {
    ## smells like sf
    ximage_sf_data(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col)
    return(invisible(x))
  }

   ## here validate that we have extent, dimension as attributes, otherwise just see if it's a matrix
  attrs <- attributes(x)
  if ("gis" %in% names(attrs)) {
    ## gdalraster output
    attrs <- attrs[["gis"]]
    attrs$dimension <- attrs$dim
    attrs$projection <- attrs$srs
    attrs$extent <- attrs$bbox[c(1, 3, 2, 4)]
  }
  if (!is.null(attrs$extent) && is.null(extent)) extent <- attrs$extent
  dimension <- NULL
  if (!is.null(attrs$dimension)) {
    dimension <- attrs$dimension

  }
  projection <- NULL

  if (is.null(dimension)) {
    if (is.null(dim(x[[1]]))) {
      dimension <- dim(x[[1]])
    } else {
    stop("no dimension known")
    }
  }
 if (!is.null(attrs$projection)) projection <- attrs$projection
  if (is.character(x[[1]])) {
    if (grepl("^#", stats::na.omit(x[[1]])[1])) {
      ## we have image data
    } else {
      ## can't read data in ximage
      stop("can't read data in the this package")
#      x[[1]] <- as.vector(t(elevation(source = x[[1]], extent = attr(x, "extent"), dimension = attr(x, "dimension"), projection = attr(x, "projection"))))
    }
  }

    if (inherits(x[[1]], "nativeRaster")) {
    ximage(x[[1L]], extent = extent,  zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col)
      return(invisible(x))
  }

  if (length(x) %in% c(3, 4)) {
    ximage(aperm(array(unlist(x), c(dimension[1:2], 3)), c(2, 1, length(x))),
                 extent = extent,  zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col, breaks = breaks)
  } else {
    ximage(matrix(x[[1]], dimension[2L], byrow = TRUE),
                   extent = extent,  zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col, breaks = breaks)
  }
  ##if (coastline) graphics::lines(coastline(extent, projection = projection, dimension = c(512, 512)))

  ## return the materialized data
  invisible(x)
}
#' @export
ximage.raw <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {
  if (all(c("width", "height", "depth") %in% names(attributes(x)))) {
    attrs <- attributes(x)
    x <- aperm(array(x, c(attrs$depth, attrs$width, attrs$height)), c(3, 2, 1))
    if (attrs$depth == 1) x<- x[,,1L, drop = TRUE]
  }
  ximage.default(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col, breaks = breaks)
}

#' @export
ximage.numeric <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {
  if (all(c("width", "height", "depth") %in% names(attributes(x)))) {
    attrs <- attributes(x)
    x <- aperm(array(x, c(attrs$depth, attrs$width, attrs$height)), c(3, 2, 1))
  }
  ximage.default(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col, breaks = breaks)
}

#' @export
ximage.integer <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {
  if (all(c("width", "height", "depth") %in% names(attributes(x)))) {
    attrs <- attributes(x)
    x <- aperm(array(x, c(attrs$depth, attrs$width, attrs$height)), c(3, 2, 1))
  }
  ximage.default(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col, breaks = breaks)
}

#' @export
ximage.default <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE), breaks = NULL) {

  if (is.list(x)) {
    ximage.list(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col)
    return(invisible(x))
  }

  if (is.numeric(x) && "gis" %in% names(attributes(x))) {
    ## vector output from gdalraster
    gis <- attr(x, "gis")
    x_list <- asplit(array(x, dim = gis$dim), MARGIN=3)
    attr(x_list, "gis") <- gis
    ximage.list(x_list, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col)
    return(invisible(x_list))
  }

  ## fastpng raw output
  # if (is.raw(x) && all(c("width", "height", "depth") %in% names(attributes(x)))) {
  #   attrs <- attributes(x)
  #   x <- aperm(array(x, c(attrs$depth, attrs$width, attrs$height)), c(3, 2, 1))
  # }

  stopifnot(inherits(x, "array"))

   if (is.raw(x)) {
      ## convert
      x <- array(as.integer(x), dim(x))
    }
  ## rescale to
  ## assume RGB if dim(x)[3] is 3, or 4
  ## allow 0,1 or arbitrary numeric
  ## allow character hex
  if (is.numeric(x)) {
    rg <- range(x, na.rm = TRUE)

    ## we're not expecting zlim to be used if it's RGB/A
    if (!is.null(zlim)) {

      x[x < zlim[1L]] <- NA
      x[x > zlim[2L]] <- NA
      if (is.finite(zlim[1])) rg[1] <- zlim[1]
      if (is.finite(zlim[2])) rg[2] <- zlim[2]

    }


    #x <- .make_hex_matrix(x, cols = col )
    ## politely ignore numeric arrays with 3 or 4 slices
    dmx <- dim(x)
    tt <- length(dmx) %in% c(3, 4) && is.numeric(x) ##&& all(x >= 0, na.rm = TRUE)

    #if (!tt && !is.null(col)) {

    if (!tt) {
      #browser()
      if (is.null(col)) col <-  colorRampPalette(grDevices::hcl.colors(12, "YlOrRd",
                                                                              rev = TRUE))


      if (length(breaks) > 0 && !(length(breaks)-1) == length(col)) {

        col <- colorRampPalette(col)(length(breaks)-1)
      }
      x <- matrix(palr::image_pal(x, col, breaks = breaks), dim(x)[1L], dim(x)[2L])
    } else {
      ## here would should nara
      x <- (x - rg[1L])/diff(rg)


    }

  } else {

    ## else character
    x <- matrix(x, dim(x)[1L])
  }
  if (is.null(extent)) {
    extent <- c(0, dim(x)[2L], 0, dim(x)[1L])
  }
  ## if !add
#  par(xaxs = "i", yaxs = "i")
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""

  if (is.list(extent) && length(extent) == 2) {
    stop("meshplot not yet supported")
    #ximage_meshplot(x, extent, add = add)
  }
  if (!add) plot(extent[1:2], extent[3:4], type = "n", ..., xaxs = "i", yaxs = "i", xlab = xlab, ylab = ylab)

  if (anyNA(x)) x[is.na(x)] <- 1
  graphics::rasterImage(x, extent[1], extent[3], extent[2], extent[4], interpolate = FALSE)
  invisible(list(x = x, extent = extent))
}

#' @export
ximage.nativeRaster <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE)) {
    if (is.null(extent)) {
    extent <- c(0, dim(x)[2L], 0, dim(x)[1L])
    }
  if (is.null(xlab)) xlab <- ""
  if (is.null(ylab)) ylab <- ""

  if (is.list(extent) && length(extent) == 2) {
    stop("meshplot not yet supported")
    #ximage_meshplot(x, extent, add = add)
  }
  if (!add) plot(extent[1:2], extent[3:4], type = "n", ..., xaxs = "i", yaxs = "i", xlab = xlab, ylab = ylab)
  graphics::rasterImage(x, extent[1], extent[3], extent[2], extent[4], interpolate = FALSE)
}

#' @export
ximage.raster <- function(x, extent = NULL, zlim = NULL, add = FALSE, ..., xlab = NULL, ylab = NULL,  col = hcl.colors(96, "YlOrRd", rev = TRUE)) {
  ximage.nativeRaster(x, extent = extent, zlim = zlim, add = add, ..., xlab = xlab, ylab = ylab, col = col)
}



.gt_dim_to_extent <- function (x, dim)
{
    xx <- c(x[1], x[1] + dim[1] * x[2])
    yy <- c(x[4] + dim[2] * x[6], x[4])
    c(xx, yy)
}
ximage_sf_data <- function(x, extent = NULL, ...) {
 #
 #  List of 19
 # $ filename            : chr "vrt:///vsicurl/https://gebco2022.s3.valeria.science/gebco_2022_complete_cog.tif?ovr=8"
 # $ driver              : chr [1:2] "VRT" "Virtual Raster"
 # $ cols                : num [1:2] 1 42
 # $ rows                : num [1:2] 1 21
 # $ bands               : int 1
 # $ crs                 :List of 2
 #  ..$ input: chr "WGS 84"
 #  ..$ wkt  : chr "GEOGCRS[\"WGS 84\",\n    ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n        MEMBER[\"World Geodetic Sys"| __truncated__
 #  ..- attr(*, "class")= chr "crs"
 # $ geotransform        : num [1:6] -180 8.57 0 90 0 ...
 # $ datatype            : chr "Int16"
 # $ sub                 : chr NA
 # $ meta                : chr "AREA_OR_POINT=Area"
 # $ band_meta           :List of 1
 #  ..$ : chr(0)
 # $ attribute_tables    :List of 1
 #  ..$ : list()
 # $ color_tables        :List of 1
 #  ..$ : NULL
 # $ ranges              : num [1, 1:4] -32768 0 32767 0
 # $ blocksizes          : int [1, 1:2] 512 512
 # $ descriptions        : chr ""
 # $ default_geotransform: int 0
 # $ proxy               : logi FALSE
 # $ colorInterp         : int 1
 # - attr(*, "data")= num [1:42, 1:21] -3001 -3203 -3158 -2999 -2897 ...
 #  ..- attr(*, "units")= chr ""
 #
 #
  d <- attr(x, "data")
  dm <- dim(d)
  do_extent <- TRUE
  if (!is.null(extent)) {
    do_extent <- FALSE
  }
  if (is.null(d)) stop("no data in sf read object")

  if (is.null(dm) || length(dm)  < 2) {
    d <- matrix(d)
    if (!is.null(extent)) warning("extent ignored for 1D array")
    extent <- NULL
  } else if (length(dm) > 2) {
    d <- matrix(d[1:prod(dim[1:2])], dm[1], dm[2])
  }
  if (do_extent) {

    ## sf gdal_read (@  4901a41ec56d2ad1524bab553c9195a6bd417987) doesn't update the geotransform ofsets so we do that here
    gt <- x$geotransform
    if (x$cols[1] > 1) {
      gt[1] <- gt[1] + gt[2] * (x$cols[1] - 1)
    }
    if (x$rows[1] > 1) {
      gt[4] <- gt[4] + gt[6] * (x$row[1] - 1)
    }


      extent <- .gt_dim_to_extent(gt, dm[1:2])
  }
  ximage(t(d), extent = extent, ...)
}
