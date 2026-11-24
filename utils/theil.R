theil <- function(x, w) {
  m <- weighted.mean(x, w)
  t <- if_else(x > 0, (x / m) * log(if_else(x > 0, x, 1) / m), 0)
  weighted.mean(t, w)
}
