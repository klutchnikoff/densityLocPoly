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

#' Title
#'
#' @param x
#' @param y
#' @param k
#'
#' @return
#' @export
#'
#' @examples
f_norm <-  function(x, y, k) {
  domain <- polynomial_sector(k)
  a1 <- 1/3
  b1 <- (1/3)**k/2
  a2 <- 3/4
  b2 <- (3/4)**k/2
  g <- function(u, v) {
    exp(-((u-a1)**2 + (v-b1)**2)/(2*0.2**2)) +
      exp(-((u-a2)**2 + (v-b2)**2)/(2*0.15**2))
  }
  A <- spatstat.geom::integral(spatstat.geom::as.im(g, domain), domain = domain)
  g(x,y)/A
}

#' Title
#'
#' @param x
#' @param y
#' @param k
#'
#' @return
#' @export
#'
#' @examples
f_poly <-  function(x, y, k) {
  domain <- polynomial_sector(k)
  a <- 0.6
  b <- 0.2
  g <- function(u, v) {(u-a)**2 + (v-b)**2}
  A <- spatstat.geom::integral(spatstat.geom::as.im(g, domain), domain = domain)
  g(x,y)/A
}
