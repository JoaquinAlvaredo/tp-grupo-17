fmt_es <- function(x, d = 1, tipo = "num") {
  if (tipo == "pct") {
    scales::label_percent(accuracy = 10^-d, decimal.mark = ",", big.mark = ".")(x)
  } else {
    scales::label_number(accuracy = 10^-d, decimal.mark = ",", big.mark = ".")(x)
  }
}
