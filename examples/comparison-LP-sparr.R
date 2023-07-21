library(tidyverse)
##
## domain
##

k <- 1.6
eps <- 0.01
domain <- polynomial_sector(k)
at <- spatstat.geom::ppp(eps, eps^k/2, domain)

Nobs <- 200

##
## densities
##

f1 <- function(x,y) {
  1 / spatstat.geom::integral(spatstat.geom::as.im(domain, domain))
}

f2 <-  function(x, y) {
  a <- 0.6
  b <- 0.2
  g <- function(u, v) {(u-a)**2 + (v-b)**2}
  A <- spatstat.geom::integral(spatstat.geom::as.im(g, domain), domain = domain)
  g(x,y)/A
}

f <- f2
imf <- spatstat.geom::as.im(f, domain)

##
## Bandwidth and degree
##

h_min <- 0.01
h_max <- 1
HH <- seq(h_min, h_max, by = 0.01)

##
## Monte-Carlo
##

Nrep <- 10


r_lp0 <- tibble(replication = NA, h = NA, value = NA)
r_lp1 <- tibble(replication = NA, h = NA, value = NA)
r_lp2 <- tibble(replication = NA, h = NA, value = NA)
r_sparr <- tibble(replication = NA, h = NA, value = NA)

for (rep in seq_along(Nrep)) {
  data <- spatstat.random::rpoint(Nobs, f, win = domain)

  for (h in HH) {
    ## lp
    f_lp0 <- density_estimation(data, domain, at_points=at, bandwidth=c(x = h, y = h), degree=0)
    f_lp1 <- density_estimation(data, domain, at_points=at, bandwidth=c(x = h, y = h), degree=1)
    f_lp2 <- density_estimation(data, domain, at_points=at, bandwidth=c(x = h, y = h), degree=2)

    ## sparr
    f_sparr <- sparr::bivariate.density(data, h)
    f_sparr <- f_sparr$z[1,5] # at ??

    val <- f(at$x, at$y)
    r_lp0 <- bind_rows(r_lp0, tibble(rep = rep, h = h, val = (f_lp0 - val)**2))
    r_lp1 <-  bind_rows(r_lp1, tibble(rep = rep, h = h, val = (f_lp1 - val)**2))
    r_lp2 <-  bind_rows(r_lp2, tibble(rep = rep, h = h, val = (f_lp2 - val)**2))
    r_sparr <-  bind_rows(r_sparr, tibble(rep = rep, h = h, val = (f_sparr - val)**2))
  }
}

