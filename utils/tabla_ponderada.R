tabla_ponderada <- function(df, columna, dummy) {
  t <- tapply(df$PONDERA, list(as.character(df[[columna]]), factor(df[[dummy]], levels = c(TRUE, FALSE))), sum)
  t[is.na(t)] <- 0
  t <- matrix(t, nrow = nrow(t), dimnames = dimnames(t))
  t * nrow(df) / sum(t)
}
