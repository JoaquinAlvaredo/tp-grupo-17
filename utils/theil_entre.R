theil_entre <- function(x, w, g) {
  Y <- sum(x * w); tot <- sum(w); entre <- 0
  for (k in unique(g)) {
    m  <- g == k
    pk <- sum(w[m]) / tot
    sk <- sum(x[m] * w[m]) / Y
    if (sk > 0) entre <- entre + sk * log(sk / pk)
  }
  100 * entre / theil(x, w)
}
