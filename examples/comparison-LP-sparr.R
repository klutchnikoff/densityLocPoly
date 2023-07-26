##
## This example allows to produce the .csv files contained in data/
## These files were used to asses the quality (quadratic error)
## of the procedure implemented
## in the package against the method proposed in the `sparr` package
## The results were used in the paper:
##
## [2023] K. Bertin, N. Klutchnikoff and F. Ouimet
## A NEW ADAPTIVE LOCAL POLYNOMIAL DENSITY ESTIMATION PROCEDURE ON COMPLICATED DOMAINS
## ArXiv:
##

library(tidyverse)

##
## Init
##

NN <- c(200, 500, 1000, 2000) # nb observation points
RR <- 1:500 # RR = 1:500  # nb of replications
HH <- seq(0.01, 0.5, by = 0.02) # for bandwidth
KK <- c(1, 2.1) # polynomial sector

density_type <-  "f_norm" # "f_poly"

##
## Start
##

risk <- tibble(
  rep = numeric(),
  method = character(),
  k = numeric(),
  n = numeric(),
  h = numeric(),
  value = numeric())

for (n in NN) {
  cat("n = ", n, "\n")

  for (k in KK) {
    cat("  |- k = ", k, "\n")

    domain <- polynomial_sector(k)
    f <- function(x, y) {do.call(density_type, list(x, y , k))}
    f00 <- f(0,0)

    # Strangely (0,0) does not belong to domain for spatstat ! Bug?
    eps <- 0.001
    zero <- spatstat.geom::ppp(eps, eps^k/2, domain)
    idx_k <- NULL # small hack for sparr package (see below)

    ##
    ## MAIN LOOP (replications)
    ## TODO: parallelize this loop
    ##
    for (r in RR) {
      cat(r, " - ")

      data <- spatstat.random::rpoint(n, f, win = domain)

      for (h in HH) {
        f_lp0 <- density_estimation(data, domain, at_points=zero, bandwidth=c(x = h, y = h), degree=0)
        f_lp1 <- density_estimation(data, domain, at_points=zero, bandwidth=c(x = h, y = h), degree=1)
        f_sparr_tmp <- sparr::bivariate.density(data, h)$z

        # Due to a bug in sparr f_sparr_tmp[1,1] is not always defined
        # Here is a hack to overcome this drawback
        if (is.null(idx_k)) {
          l <-  2
          i <- 1
          while (TRUE) {
            f_sparr <- as.vector(f_sparr_tmp[l-i, i])
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
          idx_k <- c(l-i, i)
        }
        f_sparr <- as.vector(f_sparr_tmp[idx_k[1], idx_k[2]])

        risk <- bind_rows(
          risk,
          tibble(rep = r, h = h, n = n, k = k, method = "LP0", value = (f_lp0 - f00)**2),
          tibble(rep = r, h = h, n = n, k = k, method = "LP1", value = (f_lp1 - f00)**2),
          tibble(rep = r, h = h, n = n, k = k, method = "SPARR", value = (f_sparr - f00)**2))
      }
    }
  }
}

write_csv(risk, file = str_c("data/risk_norm.csv"))
