library(tidyverse)
##
## domain
##

k <- 1.6
eps <- 0.01
domain <- polynomial_sector(k)
at <- spatstat.geom::ppp(eps, eps^k/2, domain)

Nobs <- 2000

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
HH <- seq(h_min, h_max, by = 0.02)

##
## Monte-Carlo
##

Nrep <- 500


r_lp0 <- tibble(replication = numeric(), h = numeric(), value = numeric())
r_lp1 <- tibble(replication = numeric(), h = numeric(), value = numeric())
r_lp2 <- tibble(replication = numeric(), h = numeric(), value = numeric())
r_sparr <- tibble(replication = numeric(), h = numeric(), value = numeric())

for (rep in 1:Nrep) {
  print(rep)

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
    r_lp0 <- bind_rows(r_lp0, tibble(replication = rep, h = h, value = (f_lp0 - val)**2))
    r_lp1 <-  bind_rows(r_lp1, tibble(replication = rep, h = h, value = (f_lp1 - val)**2))
    r_lp2 <-  bind_rows(r_lp2, tibble(replication = rep, h = h, value = (f_lp2 - val)**2))
    r_sparr <-  bind_rows(r_sparr, tibble(replication = rep, h = h, value = (f_sparr - val)**2))
  }
}

saveRDS(r_lp0, file = "data/rlp0-k1.6-Nobs2000.rds")
saveRDS(r_lp1, file = "data/rlp1-k1.6-Nobs2000.rds")
saveRDS(r_lp2, file = "data/rlp2-k1.6-Nobs2000.rds")
saveRDS(r_sparr, file = "data/rsparr-k1.6-Nobs2000.rds")



