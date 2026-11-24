library(tidyverse)
instub  <- "input"
outtab  <- "output/tablas"
outgraf <- "output/graficos"
dir.create(outtab, showWarnings = FALSE, recursive = TRUE)
dir.create(outgraf, showWarnings = FALSE, recursive = TRUE)
for (f in list.files("utils", full.names = TRUE)) source(f)

ocupados <- read_csv(file.path(instub, "ocupados_menores.csv"), show_col_types = FALSE, guess_max = 100000)
hogares  <- read_csv(file.path(instub, "hogares_menores.csv"), show_col_types = FALSE, guess_max = 100000)
cat("Ocupados 18-64 en hogares con menores:", nrow(ocupados), "| en hogares con AUH:", sum(ocupados$AUH), "\n")

t15 <- tabla_ponderada(ocupados, "rama", "AUH")
r15 <- chi_cuadrado(t15)
cat(sprintf("Chi2(%d) = %.1f, p %s, V de Cramér = %.2f, esperada mínima = %.1f\n", r15$gl, r15$chi2,
            ifelse(r15$p < 0.001, "< 0,001", round(r15$p, 3)), r15$V, min(r15$esperada)))

obs_esp <- tibble(rama = rownames(t15),
                  obs_con_AUH = t15[, 1], esp_con_AUH = r15$esperada[, 1],
                  obs_sin_AUH = t15[, 2], esp_sin_AUH = r15$esperada[, 2],
                  pct_con_AUH = 100 * t15[, 1] / rowSums(t15),
                  residuo_ajustado = r15$residuo_adj[, 1]) %>%
  arrange(desc(residuo_ajustado)) %>%
  mutate(across(where(is.double), ~ round(.x, 1)))
print(obs_esp, n = 20, width = 200)
write_csv(obs_esp, file.path(outtab, "05_chi2_observados_esperados.csv"))

resumen <- tibble(chequeo = "Base: 15 ramas x hogar con AUH", n = nrow(ocupados), categorias = nrow(t15),
                  chi2 = r15$chi2, gl = r15$gl, p = r15$p, V = r15$V, esperada_min = min(r15$esperada))

agregar <- function(resumen, nombre, df, columna, dummy, agrupar = FALSE) {
  if (agrupar) df$cat_ <- agrupar_ramas(df, columna, dummy) else df$cat_ <- df[[columna]]
  t <- tabla_ponderada(df, "cat_", dummy); r <- chi_cuadrado(t)
  bind_rows(resumen, tibble(chequeo = nombre, n = nrow(df), categorias = nrow(t), chi2 = r$chi2, gl = r$gl,
                            p = r$p, V = r$V, esperada_min = min(r$esperada)))
}

resumen <- agregar(resumen, "Divisiones CAES (2 dígitos), agrupadas", ocupados, "division", "AUH", agrupar = TRUE)

resumen <- agregar(resumen, "15 ramas x ocupada que cobra AUH (individual)", ocupados, "rama", "perc")
resumen <- agregar(resumen, "Divisiones x ocupada que cobra AUH (individual)", ocupados, "division", "perc", agrupar = TRUE)

resumen <- agregar(resumen, "Solo mujeres, 15 ramas x hogar con AUH", filter(ocupados, mujer), "rama", "AUH")
resumen <- agregar(resumen, "Solo varones, 15 ramas x hogar con AUH", filter(ocupados, !mujer), "rama", "AUH")

resumen <- agregar(resumen, "Estatal vs privado x hogar con AUH", filter(ocupados, PP04A %in% c(1, 2)), "PP04A", "AUH")

v <- hogares %>% filter(ingreso_valido)
cortes <- wq(v$IPCF_n, v$PONDIH, c(1 / 3, 2 / 3))
oi <- ocupados %>%
  inner_join(v %>% select(HOG, IPCF_n), by = "HOG") %>%
  mutate(tercil = case_when(IPCF_n <= cortes[1] ~ "Bajo", IPCF_n <= cortes[2] ~ "Medio", TRUE ~ "Alto"))
for (tc in c("Bajo", "Medio", "Alto")) {
  resumen <- agregar(resumen, paste0("Tercil de ingreso ", tc, ", 15 ramas x hogar con AUH"), filter(oi, tercil == tc), "rama", "AUH")
}
resumen <- resumen %>% mutate(across(c(chi2, V, esperada_min), ~ round(.x, 2)), p = signif(p, 3))
print(resumen, width = 200)
write_csv(resumen, file.path(outtab, "05_chi2_resumen_chequeos.csv"))

g_res <- obs_esp %>%
  mutate(rama = fct_reorder(rama, residuo_ajustado), signo = residuo_ajustado > 0) %>%
  ggplot(aes(x = residuo_ajustado, y = rama, fill = signo)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = GRIS_OSC, linewidth = 0.4) +
  scale_fill_manual(values = c(`TRUE` = ROJO, `FALSE` = GRIS), guide = "none") +
  labs(title = str_wrap("Construcción, servicio doméstico y comercio tienen más ocupados en hogares con AUH de los esperados", 70),
       subtitle = "Residuo estandarizado ajustado del chi-cuadrado, por rama (positivo: más ocupados en hogares con AUH que si la rama no importara)",
       x = "Residuo ajustado (|z| > 3 se considera una diferencia grande)", y = NULL,
       caption = str_wrap(paste0("Chi-cuadrado de independencia: χ²(", r15$gl, ") = ", fmt_es(r15$chi2, 0), ", V de Cramér = ", fmt_es(r15$V, 2),
                        ". Fuente: elaboración propia en base a la EPH continua (INDEC), 1T-2024 a 1T-2026; PONDERA reescalado a n."), 110)) +
  theme_g17() + theme(panel.grid.major.y = element_blank())
ggsave(file.path(outgraf, "05_residuos_ajustados_rama.jpg"), g_res, width = 24, height = 14, units = "cm", dpi = 200, device = ragg::agg_jpeg)
cat("Listo: chi-cuadrado.\n")
