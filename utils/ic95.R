ic95 <- function(z) {
  as.numeric(quantile(z, c(0.025, 0.975)))
}
