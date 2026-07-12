# topographic elevation

Data obtained from GEBCO 2019, via whatarelief package from AAD COG
(GeoTIFF).

## Details

A matrix of global elevation values in a smallish matrix, extent is
-180,180,-90,90, crs is "OGC:CRS84".

## Examples

``` r
ximage(topo, extent = c(-180, 180, -90, 90))
ximage(logo_n, extent = c(135, 155, -48, -30), add = TRUE)
```
