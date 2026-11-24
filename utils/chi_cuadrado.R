chi_cuadrado <- function(tabla) {
  ch <- suppressWarnings(chisq.test(tabla, correct = FALSE))
  n  <- sum(tabla)
  rs <- rowSums(tabla) / n; cs <- colSums(tabla) / n
  list(chi2 = as.numeric(ch$statistic), gl = as.integer(ch$parameter), p = ch$p.value,
       V = sqrt(as.numeric(ch$statistic) / (n * (min(dim(tabla)) - 1))),
       esperada = ch$expected, residuo_adj = (tabla - ch$expected) / sqrt(ch$expected * (1 - outer(rs, cs))))
}
