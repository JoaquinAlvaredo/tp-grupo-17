division_caes <- function(cod) {
  c <- suppressWarnings(as.numeric(cod))
  if_else(c < 100, c, c %/% 100)
}
