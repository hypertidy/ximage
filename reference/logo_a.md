# R logo as an RGB image

Data obtained from png package.

## Details

`logo_n` A matrix of colour values in native raster form.

`logo_a` An array of RGB colour values.

## Examples

``` r
ximage(logo_n, asp = .3)
ximage(logo_a, extent = c(8, 18, 60, 80), add = TRUE)
rect(8, 60, 18, 80)
```
