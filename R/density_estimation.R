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
density_estimation <- function(
    data,
    domain,
    at_points = NULL,
    resolution = NULL,
    bandwidth = NULL,
    degree = 3){

  ## ==========
  ## == Init ==
  ## ==========

  if (is.null(at_points)){
    if (is.null(resolution)) {
      stop("Either at_points or resolution should be specified.")
    }
    at_points <- regular_points_in_owin(domain, resolution)
  }

  spatstat.geom::verifyclass(data, "ppp")
  spatstat.geom::verifyclass(at_points, "ppp")
  spatstat.geom::verifyclass(domain, "owin")
  spatstat.geom::verifyclass(degree, "numeric")

  if (is.null(bandwidth)) {
    bandwidth <- c(x = (max(at_points$x) - min(at_points$x))/4,
                   y = (max(at_points$y) - min(at_points$y))/4)
  } else {
    spatstat.geom::verifyclass(bandwidth, "numeric")
  }

  ## ==============================
  ## == Main loop over at_points ==
  ## ==============================

  res <- rep(NA, at_points$n)

  for (i in 1:at_points$n) {

    ##
    ## current point and its neighborhood
    ##

    x <- at_points$x[i]
    y <- at_points$y[i]

    H <- spatstat.geom::owin(
      c(x - bandwidth["x"], x + bandwidth["x"]),
      c(y - bandwidth["y"], y + bandwidth["y"])
    )
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
    loc_poly <- Reduce(
      "+",
      mapply(a, ortho.poly, FUN = "*", SIMPLIFY = FALSE)
    )
    xy <- nn_im_grid(x, y, ortho.poly[[1]], neighborhood)
    res[i] <- as.function(loc_poly)(xy[1], xy[2])
  }

  res
}









