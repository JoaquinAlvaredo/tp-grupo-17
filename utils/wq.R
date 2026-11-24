wq <- function(x, w, q) {
  o  <- order(x)
  xs <- x[o]
  cw <- cumsum(w[o]) / sum(w)
  vapply(q, function(p) xs[min(which(cw >= p - 1e-12))], numeric(1))
}
