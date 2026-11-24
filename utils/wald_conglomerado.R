wald_conglomerado <- function(estimador, vivienda, B = 1000) {
  obs <- estimador(seq_along(vivienda))
  sorteo <- muestreador(vivienda)
  est <- replicate(B, estimador(sorteo()))
  if (is.null(dim(est))) est <- matrix(est, nrow = 1)
  S <- cov(t(est))
  W <- as.numeric(t(obs) %*% solve(S, obs))
  gl <- length(obs)
  list(wald = W, gl = gl, p = pchisq(W, gl, lower.tail = FALSE))
}
