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

