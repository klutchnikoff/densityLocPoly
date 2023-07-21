projection <- function(u, v, W) {
  spatstat.geom::integral(u*v, W = W) / spatstat.geom::integral(v*v, W = W) * v
}

usual_polynomials <- function(deg, W){
  out <- list()
  l <- 1
  for(d in 0:deg){
    for(i in 0:d){
      j <- d-i
      out[[l]] <- spatstat.geom::as.im(function(x,y){x^i * y^j}, W = W)
      l <- l+1
    }
  }
  out
}

orthonormal_polynomials <- function(deg, W){
  out <- list()
  plist <- usual_polynomials(deg, W)
  for(i in seq_along(plist)){
    u <- plist[[i]]
    for(j in seq_along(out)){
      v <- out[[j]]
      u <- u - projection(u, v, W)
    }
    out[[i]] <- u
  }
  out <- lapply(out, function(u){u/sqrt(spatstat.geom::integral(u*u, W = W))})
  out
}









