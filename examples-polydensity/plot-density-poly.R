library(tidyverse)
##
## domain
##

KK = c(1, 1.6, 2.1)

for (k in KK){
  domain <- polynomial_sector(k)

  f <-  function(x, y) {
    a <- 0.6
    b <- 0.2
    g <- function(u, v) {(u-a)**2 + (v-b)**2}
    A <- spatstat.geom::integral(spatstat.geom::as.im(g, domain), domain = domain)
    g(x,y)/A
  }


  imf <- spatstat.geom::as.im(f, domain)

  pdf(file = str_c("img/poly-density-k",k,".pdf"), width = 4, height = 4)
  plot(imf,
       col = grey(seq(1, 0, length = 256)),
       main = "", ribbon=FALSE,
       ylab="", yaxt="n",
       xlab="", xaxt="n")
  plot(domain, add = TRUE)
  contour(imf, add = TRUE, nlevels = 10, lwd = 1, lty = 1)
  dev.off()
}

