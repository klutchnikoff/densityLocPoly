#' LocPoly Density estimation on complicated domains
#'
#' @param data The data as a `ppp` object
#' @param domain A window of class `owin`
#' @param at_points The points
#' @param resolution Not used
#' @param bandwidth The multivariate bandwidth as a named vector `c(x=hx, y=hy)`
#' @param degree The degree of the local polynomial approximation
#'
#' @return An estimation of the density of the `data` (on `domain`) computed at `at_points`
#' @export
density_estimation <- function(data,
                               domain,
                               at_points = NULL,
                               resolution = NULL,
                               bandwidth = NULL,
                               degree = 3) {
  ## ==========
  ## == Init ==
  ## ==========

  x <- at_points$x
  y <- at_points$y

  H <- spatstat.geom::owin(c(x - bandwidth, x + bandwidth), c(y - bandwidth, y + bandwidth))
  neighborhood <- spatstat.geom::intersect.owin(H, domain)

  ##
  ## Computations
  ##

  ortho.poly <- orthonormal_polynomials(degree, W = neighborhood)

  datum <- spatstat.geom::subset.ppp(data, subset = neighborhood)

  a <- list()
  for (k in seq_along(ortho.poly)) {
    eta_k <- as.function(ortho.poly[[k]])
    a[[k]] <- sum(eta_k(datum)) / data$n
  }
  loc_poly <- Reduce("+", mapply(a, ortho.poly, FUN = "*", SIMPLIFY = FALSE))
  xy <- nn_im_grid(x, y, ortho.poly[[1]], neighborhood)
  as.function(loc_poly)(xy[1], xy[2])
}
