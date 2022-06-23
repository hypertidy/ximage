## ximage is like image() BUT THAT MIGHT CHANGE
## do we need a like-rasterImage()? does that cover native orientation and does it
## cover the same case as


src <- "/vsicurl/https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt"

info <- vapour::vapour_raster_info(src)[c("extent", "dimension")]

## say we want a tiny region, native to NASADEM
pt <- maps::world.cities |> dplyr::filter(name == "Sydney", country.etc == "Australia") |>
  dplyr::select(long, lat) |> as.matrix()

## a tiny region (0.01 is about 1000m) https://xkcd.com/2170/
ex <- c(-1, 1, -1, 1) * 0.04 + rep(pt, each = 2)

vaster::align_extent(ex, info$extent, info$dimension)
vinfo <- vaster::vcrop(ex, info$extent, info$dimension)

library(whatarelief)
mat <- elevation(extent = vinfo$extent, dimension = vinfo$dimension)
plot(matrix(vinfo$extent, ncol = 2), asp = 1/cos(mean(vinfo$extent[3:4] * pi/180)))
rasterImage(t(scales::rescale(mat)[,ncol(mat):1]), vinfo$extent[1], vinfo$extent[3], vinfo$extent[2], vinfo$extent[4])
library(mapdata)
maps::map("worldHires", add = T)

plot(raster(t(mat[,ncol(mat):1])))
ri <- function(x) t(x[,ncol(x):1])
ir <- function(x) t(x)[, ncol(x):1]
## these are equivalent
image(mat)
image(ir(ri(mat)))
image(ir(ir(ir(ir(mat)))))

## rasterImage is the other way
plot(0:1, 0:1)
rasterImage(ri(scales::rescale(mat)), 0, 0, 1, 1)
rasterImage(ri(ir(ri(scales::rescale(mat)))), 0, 0, 1, 1)
rasterImage(ri(ri(ri(ri(ri(scales::rescale(mat)))))), 0, 0, 1, 1)

plot(terra::crop(terra::rast(src), terra::ext(ex), snap = "out"))
