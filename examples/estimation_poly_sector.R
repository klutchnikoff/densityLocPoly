##
## Initialization
##

k <- 1
eps <- 0.01
domain <- polynomial_sector(k)
at <- spatstat.geom::ppp(eps, eps^k/2, domain)


set.seed(1234)
M <- spatstat.random::runifpoint(1, win = domain)
a <- M$x
b <- M$y
f0 <-  function(x, y) {
  g <- function(u, v) {(u-a)**2 + (v-b)**2}
  A <- spatstat.geom::integral(spatstat.geom::as.im(g, domain), domain = domain)
  g(x,y)/A
}
data <- spatstat.random::rpoint(1000, f0, win = domain)


##
## Visualization
##

par(mfrow = c(1,1))
plot(domain, main = "Disque")
points(data, pch = 20, col = "red")
points(at, pch = 20, lwd = 10, col = "blue")
points(M, pch = 20, lwd = 5, col = "green")

##
## Computations
##
par(mfrow = c(1,1))

bandwidth <- c(x = 0.07, y = 0.05)
degree <- 1
fd <- density_estimation(data, domain, at_points=at, bandwidth=bandwidth, degree=degree)

mean((fd[[1]] - f0(at$x, at$y))**2)
f0(at$x, at$y)
fd[[1]]
