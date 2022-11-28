## code to prepare `DATASET` dataset goes here

library(png)
# read a sample file (R logo)
logo_a <- readPNG(system.file("img", "Rlogo.png", package="png"))
# read it also in native format
logo_n <- readPNG(system.file("img", "Rlogo.png", package="png"), TRUE)
usethis::use_data(logo_a, overwrite = TRUE)
usethis::use_data(logo_n, overwrite = TRUE)

library(whatarelief)
topo <- elevation(dimension = c(720, 360))
mode(topo) <- "integer"
usethis::use_data(topo, overwrite = TRUE, compress = "bzip2")


## https://github.com/pierreroudier/hillshader/blob/master/data-raw/maungawhau.R

xvolcano <- datasets::volcano[nrow(datasets::volcano):1, ncol(datasets::volcano):1]
ex_volcano <- c(2667400 + c(0, 10) * ncol(xvolcano), 6478700 + c(0, 10) * nrow(xvolcano))
pr_volcano <- "EPSG:27200"

## get SRTM from NASADEM
srtm <- elevation(extent = ex_volcano, dimension = dim(xvolcano),
                  projection = pr_volcano)

## get Virtual Earth image
library(gdalio)
gdalio_set_default_grid(list(extent = ex_volcano, dimension = dim(volcano) * 10, projection = pr_volcano))
tr <- gdalio_array(src$source[14], bands = 1:3, resample = "cubic", band_output_type = "integer")


ximage(xvolcano, ex_volcano, asp = 1)

xcontour(srtm, ex_volcano, add = TRUE)
plotRGB(tr)
ximage(as.array(tr), ex_volcano, asp = 1)
xcontour(srtm, ex_volcano, add = TRUE, col = "green", lwd = 4)



