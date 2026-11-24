agrupar_ramas <- function(df, columna = "division", dummy = "perc") {
  g <- df[[columna]]
  repeat {
    df$grupo <- g
    t <- tabla_ponderada(df, "grupo", dummy)
    E <- chi_cuadrado(t)$esperada
    if (min(E) >= 5 || nrow(t) <= 3) break
    minimas <- apply(E, 1, min); names(minimas) <- rownames(t)
    minimas <- minimas[names(minimas) != "-1"]
    if (length(minimas) == 0) break
    g[as.character(g) == names(which.min(minimas))] <- -1
  }
  g
}
