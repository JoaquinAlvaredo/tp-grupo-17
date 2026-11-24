library(tidyverse)
instub  <- "input"
outtab  <- "output/tablas"
outgraf <- "output/graficos"
dir.create(outtab, showWarnings = FALSE, recursive = TRUE)
dir.create(outgraf, showWarnings = FALSE, recursive = TRUE)
for (f in list.files("utils", full.names = TRUE)) source(f)

hogares <- read_csv(file.path(instub, "hogares_menores.csv"), show_col_types = FALSE, guess_max = 100000)

faltantes <- tibble(
  variable = c("IPCF / ITF (0 = no declara)", "PONDIH (0 = sin ingreso válido)", "V5_01 (9 = Ns/Nr)"),
  casos = nrow(hogares),
  faltantes = c(sum(hogares$IPCF_n == 0), sum(hogares$PONDIH == 0), sum(is.na(hogares$AUH)))) %>%
  mutate(pct = round(100 * faltantes / casos, 1))
print(faltantes)
write_csv(faltantes, file.path(outtab, "03_faltantes_tabla.csv"))

region <- c(`1` = "GBA", `40` = "NOA", `41` = "NEA", `42` = "Cuyo", `43` = "Pampeana", `44` = "Patagonia")
por_region <- hogares %>%
  mutate(region = region[as.character(REGION)]) %>%
  group_by(region) %>%
  summarise(hogares = n(), pct_sin_ingreso = round(100 * mean(!ingreso_valido), 1), .groups = "drop") %>%
  arrange(desc(pct_sin_ingreso))
print(por_region)
write_csv(por_region, file.path(outtab, "03_faltantes_por_region.csv"))

tb <- table(hogares$REGION, hogares$ingreso_valido)
ch <- chisq.test(tb)
cat(sprintf("Región x ingreso válido: chi2(%d) = %.1f, V de Cramér = %.2f\n",
            ch$parameter, ch$statistic, sqrt(ch$statistic / (sum(tb) * (min(dim(tb)) - 1)))))
tb2 <- table(hogares$AUH, hogares$ingreso_valido)
cat(sprintf("AUH x ingreso válido: V de Cramér = %.2f\n", sqrt(chisq.test(tb2)$statistic / sum(tb2))))

v <- hogares %>% filter(ingreso_valido, !is.na(AUH))
chequeo <- tibble(
  indicador = c("% de hogares del GBA, todos (PONDERA)", "% de hogares del GBA, válidos (PONDIH)",
                "% de hogares con AUH, todos (PONDERA)", "% de hogares con AUH, válidos (PONDIH)"),
  valor = round(c(100 * sum(hogares$PONDERA[hogares$REGION == 1]) / sum(hogares$PONDERA),
                  100 * sum(v$PONDIH[v$REGION == 1]) / sum(v$PONDIH),
                  100 * weighted.mean(hogares$AUH, hogares$PONDERA, na.rm = TRUE),
                  100 * weighted.mean(v$AUH, v$PONDIH)), 1))
print(chequeo)
write_csv(chequeo, file.path(outtab, "03_faltantes_chequeo_ponderador.csv"))

g_falt <- ggplot(por_region, aes(x = pct_sin_ingreso, y = fct_reorder(region, pct_sin_ingreso))) +
  geom_col(fill = GRIS, width = 0.65) +
  geom_col(data = por_region %>% filter(region == "GBA"), fill = ROJO, width = 0.65) +
  geom_text(aes(label = paste0(fmt_es(pct_sin_ingreso, 1), "%")), hjust = -0.15, size = 3.4, colour = GRIS_OSC) +
  scale_x_continuous(limits = c(0, 55), labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
  labs(title = str_wrap("La falta de respuesta de ingresos se concentra en el GBA", 70),
       subtitle = "% de hogares con menores sin ingreso válido, por región (sin ponderar)", x = NULL, y = NULL,
       caption = str_wrap("Fuente: elaboración propia en base a la EPH continua (INDEC), 1T-2024 a 1T-2026. Hogares con menores de 18.", 110)) +
  theme_g17() + theme(panel.grid.major.y = element_blank())
ggsave(file.path(outgraf, "03_faltantes_por_region.jpg"), g_falt, width = 24, height = 11, units = "cm", dpi = 200, device = ragg::agg_jpeg)

x <- v$IPCF_n; w <- v$PONDIH; g <- v$AUH == 1; lx <- log(x)
q <- quantile(lx, c(.25, .75)); ric <- q[2] - q[1]
lim_lo <- q[1] - 1.5 * ric; lim_hi <- q[2] + 1.5 * ric
alto <- lx > lim_hi; bajo <- lx < lim_lo
tukey_crudo <- { qq <- quantile(x, c(.25, .75)); sum(x > qq[2] + 1.5 * (qq[2] - qq[1])) }
asim <- function(z) mean((z - mean(z))^3) / sd(z)^3
curt <- function(z) mean((z - mean(z))^4) / sd(z)^4

atipicos <- tibble(
  concepto = c("Límite inferior ($ por persona por mes)", "Límite superior ($ por persona por mes)",
               "Atípicos altos (hogares-trimestre)", "  de los cuales con AUH", "Atípicos bajos (hogares-trimestre)", "  de los cuales con AUH",
               "% ponderado altos", "% ponderado bajos", "Atípicos con Tukey sobre el IPCF sin transformar",
               "Asimetría del IPCF", "Curtosis del IPCF", "Asimetría de ln(IPCF)"),
  valor = c(round(exp(lim_lo)), round(exp(lim_hi)), sum(alto), sum(alto & g), sum(bajo), sum(bajo & g),
            round(100 * sum(w[alto]) / sum(w), 1), round(100 * sum(w[bajo]) / sum(w), 1), tukey_crudo,
            round(asim(x), 2), round(curt(x), 1), round(asim(lx), 2)))

perfil_atip <- tibble(
  grupo = c("Atípicos altos", "Atípicos bajos"),
  hogares = c(sum(alto), sum(bajo)),
  itf_min = c(min(v$ITF_n[alto]), min(v$ITF_n[bajo])), itf_mediana = c(median(v$ITF_n[alto]), median(v$ITF_n[bajo])),
  itf_max = c(max(v$ITF_n[alto]), max(v$ITF_n[bajo])),
  integrantes_mediana = c(median(v$IX_TOT[alto]), median(v$IX_TOT[bajo])),
  pct_jefe_ocupado = c(100 * mean(v$ESTADO_J[alto] == 1), 100 * mean(v$ESTADO_J[bajo] == 1))) %>%
  mutate(across(where(is.double), ~ round(.x, 1)))
print(perfil_atip, width = 200)
write_csv(perfil_atip, file.path(outtab, "03_outliers_perfil.csv"))
print(atipicos, n = 20)
write_csv(atipicos, file.path(outtab, "03_outliers_resumen.csv"))

p1p99 <- wq(x, w, c(.01, .99))
indices <- function(xx, ww) c(gini = gini(xx, ww), theil = theil(xx, ww))
variantes <- list("Base (se mantienen)" = rep(TRUE, length(x)), "Sin atípicos (ambos extremos)" = !(alto | bajo),
                  "Sin atípicos altos" = !alto, "Sin el 1% más alto (P99)" = x <= p1p99[2])
sens <- map_dfr(names(variantes), function(k) {
  m <- variantes[[k]]; P <- indices(x[m & g], w[m & g]); N <- indices(x[m & !g], w[m & !g])
  tibble(variante = k, n = sum(m), gini_perceptores = P[1], gini_no_perceptores = N[1], dif_gini = P[1] - N[1],
         theil_perceptores = P[2], theil_no_perceptores = N[2], dif_theil = P[2] - N[2])
})
xw <- pmin(pmax(x, p1p99[1]), p1p99[2]); P <- indices(xw[g], w[g]); N <- indices(xw[!g], w[!g])
sens <- bind_rows(sens, tibble(variante = "Winsorizado P1-P99", n = length(x), gini_perceptores = P[1], gini_no_perceptores = N[1],
                               dif_gini = P[1] - N[1], theil_perceptores = P[2], theil_no_perceptores = N[2], dif_theil = P[2] - N[2])) %>%
  mutate(across(where(is.double), ~ round(.x, 3)))
print(sens, width = 200)
write_csv(sens, file.path(outtab, "03_outliers_sensibilidad.csv"))

g_out <- ggplot(v, aes(x = IPCF_n / 1000, weight = PONDIH / sum(PONDIH))) +
  geom_histogram(aes(y = after_stat(count)), bins = 60, fill = GRIS, colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = exp(c(lim_lo, lim_hi)) / 1000, colour = ROJO, linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = exp(lim_lo) / 1000, y = Inf, label = "Límite inferior", hjust = 1.1, vjust = 1.8, colour = ROJO, size = 3.2) +
  annotate("text", x = exp(lim_hi) / 1000, y = Inf, label = "Límite superior", hjust = -0.1, vjust = 1.8, colour = ROJO, size = 3.2) +
  scale_x_log10(breaks = c(10, 30, 100, 300, 1000, 3000), labels = function(x) fmt_es(x, 0)) +
  scale_y_continuous(labels = NULL) +
  labs(title = str_wrap("Los atípicos son el 2,5% de los hogares y parecen ingresos reales: los mantenemos", 70),
       subtitle = "Distribución del IPCF (miles de $ de 1T-2026 por mes, escala logarítmica) y límites de Tukey calculados sobre ln(IPCF)",
       x = "IPCF (miles de $, escala logarítmica)", y = NULL,
       caption = str_wrap("Fuente: elaboración propia en base a la EPH continua (INDEC), 1T-2024 a 1T-2026. Hogares con menores de 18 e ingreso válido; ponderado con PONDIH.", 110)) +
  theme_g17() + theme(panel.grid.major.y = element_blank())
ggsave(file.path(outgraf, "03_outliers_ipcf.jpg"), g_out, width = 24, height = 12, units = "cm", dpi = 200, device = ragg::agg_jpeg)
cat("Listo: faltantes y outliers.\n")
