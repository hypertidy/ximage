% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ximage-package.R
\docType{data}
\name{topo}
\alias{topo}
\title{topographic elevation}
\description{
Data obtained from GEBCO 2019, via whatarelief package from AAD COG (GeoTIFF).
}
\details{
A matrix of global elevation values in a smallish matrix, extent is -180,180,-90,90,
crs is "OGC:CRS84".
}
\examples{
ximage(topo, extent = c(-180, 180, -90, 90))
ximage(logo_n, extent = c(135, 155, -48, -30), add = TRUE)
}
