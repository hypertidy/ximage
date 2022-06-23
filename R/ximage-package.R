#' @keywords internal
#' @aliases ximage-package
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' topographic elevation
#'
#' Data obtained from GEBCO 2019, via whatarelief package from AAD COG (GeoTIFF).
#'
#' A matrix of global elevation values in a smallish matrix, extent is -180,180,-90,90,
#' crs is "OGC:CRS84".
#' @examples
#' ximage(topo, extent = c(-180, 180, -90, 90))
#' ximage(logo_n, extent = c(135, 155, -48, -30), add = TRUE)
#' @docType data
#' @name topo
NULL

#' R logo as an RGB image
#'
#' Data obtained from png package.
#'
#' `logo_n` A matrix of colour values in native raster form.
#'
#' `logo_a` An array of RGB colour values.
#'
#' @docType data
#' @name logo_a
#' @aliases logo_n
#' @examples
#' ximage(logo_n, asp = .3)
#' ximage(logo_a, extent = c(8, 18, 60, 80), add = TRUE)
#' rect(8, 60, 18, 80)
NULL

