##
## domain
##


type <- "poly" # "norm"
KK = c(1, 1.6, 2.1)

for (k in KK){
  domain <- polynomial_sector(k)
  f <-  function(x, y) {f_poly(x, y, k)} #TODO
  imf <- spatstat.geom::as.im(f, domain)

  pdf(file = paste0("img/", type, "-density-k", k,".pdf"),
      width = 4, height = 4)
  plot(imf,
       col = grey(seq(1, 0.2, length = 256)),
       main = "", ribbon=FALSE,
       ylab="", yaxt="n",
       xlab="", xaxt="n")
  plot(domain, add = TRUE)
  contour(imf, add = TRUE, nlevels = 10, lwd = 1, lty = 1)
  dev.off()
}

