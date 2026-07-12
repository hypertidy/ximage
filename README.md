
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ximage

<!-- badges: start -->

[![R-CMD-check](https://github.com/hypertidy/ximage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hypertidy/ximage/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of ximage is to make something like ‘image()’ and
‘rasterImage()’ but without the missing functionality and useability.

ximage supports making images from

- numeric, character, byte matrix
- numeric array
- nativeRaster
- data returned from `vapour::gdal_raster_()` or `gdalraster::read_ds()`
  functions which include numeric, character, byte vectors or
  nativeRaster types

The orientation is in “raster-order”, i.e. when you’re looking at the
picture it’s topleft to topright, then scan lines down each row to the
bottom (like western reading and `par(mfrow)` order).

This matches the way that spatial data readers read imagery, and is
equivalent to populating an R matrix with
`matrix(<>, nrow, ncol, byrow  = TRUE)` (note backwards order of
nrow,ncol). Please note that getting the data out of the matrix is not
in this order, but it is with `as.vector(t(m))`. Higher dimensional
arrays need more care, there’s no `byrow` for `array()`.

## Example

This is a basic example which shows you how to image a topographic
dataset, with arbitrary image overlay placement.

``` r
library(ximage)
ximage(topo)  ## plot in the index space of the matrix
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

``` r

## or, plot in the geographic space (we happen to know this for this matrix)
ximage(topo, extent = c(-180, 180, -90, 90), axes = F)
ximage(logo_a, extent = c(170, 180, -40, -30), add = TRUE)

axis(1); axis(2);box()
```

<img src="man/figures/README-example-2.png" alt="" width="100%" />

``` r

ximage(logo_a)  ## plot a RGB array
## plot  a native raster over the other in a different window
ximage(logo_n, extent = c(10, 20, 20, 40), add = TRUE)

ximage(topo, extent = c(40, 60, 80, 100), add = TRUE, col = hcl.colors(256))
```

<img src="man/figures/README-example-3.png" alt="" width="100%" />

We can get imagery from the internet, and plot it very quickly.

``` r
library(vapour)  
virtual_earth <-  "<GDAL_WMS><Service name=\"VirtualEarth\"><ServerUrl>http://a${server_num}.ortho.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=90</ServerUrl></Service><MaxConnections>4</MaxConnections><Cache/></GDAL_WMS>"
par(mar = rep(0, 4))
px <- dev.size("px")
px[which.min(px)] <- 0

## change lon_0 and lat_0 to anywhere you like
im <- gdal_raster_nara(virtual_earth, target_ext = c(-1, 1, -1, 1) * 3e5, target_dim = px, target_crs = "+proj=laea +lon_0=147 +lat_0=-42")

system.time(ximage(im, asp = 1))
```

<img src="man/figures/README-imagery-1.png" alt="" width="100%" />

    #>    user  system elapsed 
    #>   0.003   0.000   0.003


    ## crank up the size it's still fast
    px <- px * 4
    im <- gdal_raster_nara(virtual_earth, target_ext = c(-1, 1, -1, 1) * 3e5, target_dim = px, target_crs = "+proj=laea +lon_0=147 +lat_0=-42")

    system.time(ximage(im, asp = 1))

<img src="man/figures/README-imagery-2.png" alt="" width="100%" />

    #>    user  system elapsed 
    #>   0.040   0.024   0.063

## Code of Conduct

Please note that the ximage project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
