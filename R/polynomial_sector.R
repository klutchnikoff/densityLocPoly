#' Polynomial sector Window
#'
#' @param k Degree of the polynomial sector
#' @param npoly number of points (resolution) in the polynomial line (boundary)
#'
#' @return An object of class "owin" specifying a window
#' @export
#'
#' @examples
#'plot(polynomial_sector(1.8))
#'
polynomial_sector <- function(k, npoly = 128){
  x1 <- c(1, 1)
  y1 <- c(0, 1)
  x2 <- seq(1, 0, length.out = npoly)
  y2 <- x2**k
  x3 <- c(0, 1)
  y3 <- c(0, 0)

  x <- c(x1, x2, x3)
  y <- c(y1, y2, y3)
  spatstat.geom::owin(poly =  list(x = x, y = y))
}
