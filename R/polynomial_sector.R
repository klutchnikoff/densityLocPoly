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
#' @param x,y we evaluate f(x,y)
#' @param k degree of polynomial sector
#'
#' @return f(x,y)
#' @export
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
#' @param x,y we evaluate f(x,y)
#' @param k degree of polynomial sector
#'
#' @return f(x,y)
#' @export
f_poly <-  function(x, y, k) {
  domain <- polynomial_sector(k)
  a <- 0.6
  b <- 0.2
  g <- function(u, v) {(u-a)**2 + (v-b)**2}
  A <- spatstat.geom::integral(spatstat.geom::as.im(g, domain), domain = domain)
  g(x,y)/A
}


#' Title
#'
#' @param NN iterator - nb of observations
#' @param RR iterator -  replications
#' @param HH iterator - family of bandwidth
#' @param KK iterator - degree poly sector
#' @param density_type norm or poly
#'
#' @importFrom foreach %dopar%
#' @importFrom foreach %:%
#' @importFrom foreach foreach
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom parallel detectCores
#' @importFrom parallel clusterSetRNGStream
#' @importFrom doParallel registerDoParallel
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#'
#' @return a tibble of simulated risks for sparr and LP
#' @export
#'
simulate_risk_polysec <- function(NN, RR, HH, KK, density_type) {
  risk <-
    tibble(
      rep = numeric(),
      method = character(),
      k = numeric(),
      n = numeric(),
      h = numeric(),
      value = numeric()
    )

  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  clusterSetRNGStream(cl, iseed = 123)

  # hack for eliminating a note:
  # 'no visible binding for global variable'
  n <- k <- NULL
  risk <-
    foreach(
      n = NN,
      .combine = "bind_rows",
      .packages = c('dplyr', 'densityLocPoly')
    ) %:% foreach(k = KK) %dopar% {
      set.seed(n*k)
      res_tmp <-
        tibble(
          rep = numeric(),
          method = character(),
          k = numeric(),
          n = numeric(),
          h = numeric(),
          value = numeric()
        )

      domain <- polynomial_sector(k)
      f <- function(x, y) {
        do.call(density_type, list(x, y, k))
      }
      f00 <- f(0, 0)

      # Strangely (0,0) does not belong to domain for spatstat ! Bug?
      eps <- 0.001
      zero <- spatstat.geom::ppp(eps, eps^k / 2, domain)
      idx_k <- NULL # small hack for sparr package (see below)

      for (r in RR) {
        data <- spatstat.random::rpoint(n, f, win = domain)

        for (h in HH) {
          ##
          ## Risk of sparr method
          ##

          # Due to a bug in sparr f_sparr_tmp[1,1] is not always defined
          # Here is a hack to overcome this drawback
          f_sparr_tmp <- sparr::bivariate.density(data, h)$z
          if (is.null(idx_k)) {
            l <- 2
            i <- 1
            while (TRUE) {
              f_sparr <- as.vector(f_sparr_tmp[l - i, i])
              if (!is.na(f_sparr)) {
                break
              }
              i <- i + 1
              if (i >= l) {
                l <- l + 1
                i <- 1
              }
              if (l > sum(dim(f_sparr_tmp))) {
                break
              }
            }
            idx_k <- c(l - i, i)
          }
          f_sparr <- as.vector(f_sparr_tmp[idx_k[1], idx_k[2]])

          f_lp0 <- density_estimation(data, domain, at_points = zero, bandwidth = c(x = h, y = h), degree = 0)
          f_lp1 <- density_estimation(data, domain, at_points = zero, bandwidth = c(x = h, y = h), degree = 1)

          res_tmp <- bind_rows(
            res_tmp,
            tibble(rep = r, h = h, n = n, k = k, method = "LP0", value = (f_lp0 - f00)**2),
            tibble(rep = r, h = h, n = n, k = k, method = "LP1", value = (f_lp1 - f00)**2),
            tibble(rep = r, h = h, n = n, k = k, method = "SPARR", value = (f_sparr - f00)**2)
          )
        }
      }
      res_tmp
    }
  stopCluster(cl)
  risk
}

