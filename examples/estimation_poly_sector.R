##
## Seed
##

set.seed(1234)
M <- spatstat.random::runifpoint(1, win = domain)
a <- M$x
b <- M$y

##
## Initialization
##

k <- 1.6
eps <- 0.01
domain <- polynomial_sector(k)
at <- spatstat.geom::ppp(eps, eps^k/2, domain)


f0 <-  function(x, y) {
  g <- function(u, v) {(u-a)**2 + (v-b)**2}
  A <- spatstat.geom::integral(spatstat.geom::as.im(g, domain), domain = domain)
  g(x,y)/A
}

f1 <- function(x,y) {
  1 / spatstat.geom::integral(spatstat.geom::as.im(domain, domain))
}

f <- f0
data <- spatstat.random::rpoint(1000, f, win = domain)


##
## Visualization
##

plot(domain, main = "Disque")
points(data, pch = 20, col = "red")
points(at, pch = 20, lwd = 10, col = "blue")

##
## Computations
##

bandwidth <- c(x = 0.5, y = 0.5)
degree <- 1
fd <- density_estimation(data, domain, at_points=at, bandwidth=bandwidth, degree=degree)

mean((fd - f(at$x, at$y))**2)
f(at$x, at$y)
fd

