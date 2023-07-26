#' Construct a regular grid of n x n points in a window
#'
#' @param W A window of type owin
#' @param n The number of points in the enclosing rectangle is n x n.
#'
#' @return The regular grid as an object of type ppp
#' @export
regular_points_in_owin <- function(W, n) {
  spatstat.geom::verifyclass(W, "owin")

  xy <- spatstat.geom::gridcentres(W, n, n)
  ok <- spatstat.geom::inside.owin(xy$x, xy$y, W)
  spatstat.geom::ppp(x = xy$x[ok], y = xy$y[ok], window = W)
}

points_inside_owin <- function(points, W) {
  spatstat.geom::verifyclass(points, "ppp")
  spatstat.geom::verifyclass(W, "owin")

  ok <- spatstat.geom::inside.owin(points$x, points$y, W)
  spatstat.geom::ppp(x = points$x[ok], y = points$y[ok], window = W)
}

grid_from_im <- function(im, W) {
  spatstat.geom::verifyclass(im, "im")
  spatstat.geom::verifyclass(W, "owin")
  points <- spatstat.geom::ppp(
    x = rep(im$xcol, times = length(im$yrow)),
    y = rep(im$yrow, each = length(im$xcol)),
    window = spatstat.geom::as.owin(im),
    check = FALSE
  )
  points_inside_owin(points, W)
}

nn_im_grid <- function(x, y, im, W){

  spatstat.geom::verifyclass(x, "numeric")
  spatstat.geom::verifyclass(x, "numeric")
  spatstat.geom::verifyclass(im, "im")
  spatstat.geom::verifyclass(W, "owin")

  v <- spatstat.geom::ppp(x, y, W)
  grid <- grid_from_im(im, W)
  n <- spatstat.geom::nncross(v, grid)$which
  c(grid$x[n], grid$y[n])
}
