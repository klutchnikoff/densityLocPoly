library(tidyverse)
library(spatstat)
library(densityLocPoly)

k <- 1
domain <- polynomial_sector(k)

pdf(file = "density-poly-1.pdf", height = 3, width = 3)
par(mar=rep(0, 4))
f <- function(x, y) {f_poly(x, y, k)}
imf <- as.im(f, domain, dimyx = 512)
plot(imf,
     col = grey(seq(1, 0.2, length = 256)),
     main = "", ribbon=FALSE)
plot(domain, add = TRUE)
contour(imf, add = TRUE, nlevels = 12, lwd = 1, lty = 1)
dev.off()

pdf(file = "density-norm-1.pdf", height = 3, width = 3)
par(mar=rep(0, 4))
f <- function(x, y) {f_norm(x, y, k)}
imf <- as.im(f, domain, dimyx = 512)
plot(imf,
     col = grey(seq(1, 0.2, length = 256)),
     main = "", ribbon=FALSE)
plot(domain, add = TRUE)
contour(imf, add = TRUE, nlevels = 12, lwd = 1, lty = 1)
dev.off()


k <- 2.1
domain <- polynomial_sector(k)

pdf(file = "density-poly-21.pdf", height = 3, width = 3)
par(mar=rep(0, 4))
f <- function(x, y) {f_poly(x, y, k)}
imf <- as.im(f, domain, dimyx = 512)
plot(imf,
     col = grey(seq(1, 0.2, length = 256)),
     main = "", ribbon=FALSE)
plot(domain, add = TRUE)
contour(imf, add = TRUE, nlevels = 12, lwd = 1, lty = 1)
dev.off()

pdf(file = "density-norm-21.pdf", height = 3, width = 3)
par(mar=rep(0, 4))
f <- function(x, y) {f_norm(x, y, k)}
imf <- as.im(f, domain, dimyx = 512)
plot(imf,
     col = grey(seq(1, 0.2, length = 256)),
     main = "", ribbon=FALSE)
plot(domain, add = TRUE)
contour(imf, add = TRUE, nlevels = 12, lwd = 1, lty = 1)
dev.off()
