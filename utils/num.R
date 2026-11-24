num <- function(x) {
  as.numeric(sub(",", ".", x, fixed = TRUE))
}
