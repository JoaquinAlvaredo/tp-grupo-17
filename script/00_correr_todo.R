scripts <- c("01_armar_base.R", "02_descriptivas.R", "03_faltantes_outliers.R", "04_desigualdad.R",
             "05_chi2_rama.R", "06_theil_sector_kruskal.R", "07_vcr_rama.R", "08_graficos_finales.R",
             "09_robustez_trimestres.R", "10_p_valores_por_vivienda.R", "11_composicion_y_evolucion.R")
for (s in scripts) {
  cat("\n>>>", s, "\n")
  source(file.path("script", s), encoding = "UTF-8")
}
