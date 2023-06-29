## sf.rds: a file used to construct ximage_sf_data

## constructed with this code using {dsn}, {vaster} and GDAL 3.7.0 (2023-06-29)
#dsn <- sprintf("vrt://%s?ovr=8", dsn::gebco())
#d <- sf::gdal_read(dsn, RasterIO_parameters = vaster::raster_sfio(info$dimension, fact = 8, resample = "cubic"), read_data = TRUE)

#saveRDS(d, "sf.rds")
