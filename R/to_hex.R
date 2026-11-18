## Internal colour conversion for ximage ------------------------------------
##
## to_hex() is the single choke point that turns any supported input into a
## character matrix of hex colours ready for graphics::rasterImage(), see
## palr::image_hex() for the rules (palette mapping of single-band data,
## grey/alpha, RGB, RGBA scaling, NA handling, zlim and alpha).

#' @importFrom palr image_hex
to_hex <- function(x, col = NULL, breaks = NULL, zlim = NULL,
                   alpha = NULL, na.col = "transparent") {
  palr::image_hex(x, col = col, breaks = breaks, zlim = zlim, alpha = alpha,
                  na.col = na.col)
}
