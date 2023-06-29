
dm <- c(360, 180) / 4
z <- whatarelief::elevation(dimension = dm)

## we need to be in R matrix orientation now
z <- t(z[dm[2]:1, ])
quadmesh <- textures::quad(dm, ydown = FALSE)
## we can use extent =  in quad() or we can
quadmesh$vb[1,] <- scales::rescale(quadmesh$vb[1,], c(-180, 180))
quadmesh$vb[2,] <- scales::rescale(quadmesh$vb[2,], c(-90, 90))


## now, distribute the matrix onto the quad corners
quadmesh$vb[3, ] <- colMeans(matrix(c(tl(z), tr(z), bl(z), br(z)), 4L, byrow = TRUE), na.rm = TRUE)
## colorize it
quadmesh$material$color <- palr::d_pal(quadmesh$vb[3, quadmesh$ib[1, ]])

## plot it
anglr::mesh_plot(quadmesh)


## this is cool, because we can totally subvert the georeferencing up there

lon <- matrix(vaster::x_centre(dm, c(-180, 180, -90, 90)), dm[1], dm[2])
lat <- matrix(rep(vaster::y_centre(dm, c(-180, 180, -90, 90)), each = dm[1]), dm[1], dm[2])

## now, distribute the matrix onto the quad corners
quadmesh$vb[1, ] <- cxy(lon)
quadmesh$vb[2, ] <- cxy(lat)
quadmesh$vb[3, ] <- cxy(z)

anglr::mesh_plot(quadmesh)
maps::map(add = TRUE)

for (loni in seq(-180, 180, by = 10)) {
## now we can subvert this for reals
xy <- reproj::reproj(matrix(c(lon, lat), ncol = 2), sprintf("+proj=ortho +lon_0=%i", loni), source = "OGC:CRS84")
x <- lon; x[] <- xy[,1]
y <- lat; y[] <- xy[,2]
## clean up a little first
x[abs(lon) > 179] <- NA
y[abs(lon) > 179] <- NA
x[abs(lat) > 89] <- NA
y[abs(lat) > 89] <- NA

quadmesh$vb[1, ] <- cxy(x)
quadmesh$vb[2, ] <- cxy(y)
quadmesh$vb[3, ] <- cxy(z)
quadmesh$material$color <- palr::d_pal(quadmesh$vb[3, quadmesh$ib[1, ]])

anglr::mesh_plot(quadmesh, asp = 1)
Sys.sleep(1)
}

## now


rp <- function(x, target) {
  xy <- reproj::reproj(t(x$vb[1:2, ]), target, source = "OGC:CRS84")[,1:2, drop = F]
  x$vb[1:2, ] <- t(xy)
  x
}
anglr::mesh_plot(rp(quadmesh, "+proj=laea"), asp = 1)

ximage_meshplot <- function(x, extent, add = FALSE) {
     exy <- .edges_xy(x)

      xx <- list(x = xx, y = yy, id = ID, col = cols)

    ## if (isLL) 1/cos(mean(xx$y, na.rm = TRUE) * pi/180) else 1
    if (!add) {
      graphics::plot.new()
      graphics::plot.window(xlim = range(xx$x, finite = TRUE), ylim = range(xx$y, finite = TRUE),
                            ...)
    }
    vps <- gridBase::baseViewports()

    grid::pushViewport(vps$inner, vps$figure, vps$plot)

    ## just so we get points, we aren't doing lines properly yet, but they draw ok
if (plot_points) {
 grid::grid.points(xx$x, xx$y, pch = ".", gp = grid::gpar(col = xx$col), default.units = "native")
} else {
    grid::grid.polygon(xx$x, xx$y, xx$id, gp = grid::gpar(col = xx$col, fill = xx$col),
                       default.units = "native")
}

    grid::popViewport(3)
    #if (debug) return(xx)

    invisible(NULL)

}

