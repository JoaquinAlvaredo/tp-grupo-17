muestreador <- function(vivienda) {
  idx <- split(seq_along(vivienda), vivienda)
  K   <- length(idx)
  function() unlist(idx[sample.int(K, K, replace = TRUE)], use.names = FALSE)
}
