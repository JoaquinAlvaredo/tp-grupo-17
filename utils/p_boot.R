p_boot <- function(z) {
  min(1, 2 * max(1 / length(z), min(mean(z >= 0), mean(z <= 0))))
}
