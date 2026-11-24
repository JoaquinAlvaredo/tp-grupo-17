gini <- function(x, w) {
  o <- order(x); x <- x[o]; w <- w[o]
  p  <- cumsum(w) / sum(w)
  L  <- cumsum(w * x) / sum(w * x)
  p0 <- c(0, p[-length(p)]); L0 <- c(0, L[-length(L)])
  1 - sum((p - p0) * (L + L0))
}
