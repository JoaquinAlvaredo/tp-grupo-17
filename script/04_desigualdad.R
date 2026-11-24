library(tidyverse)
instub  <- "input"
outtab  <- "output/tablas"
outgraf <- "output/graficos"
dir.create(outtab, showWarnings = FALSE, recursive = TRUE)
dir.create(outgraf, showWarnings = FALSE, recursive = TRUE)
for (f in list.files("utils", full.names = TRUE)) source(f)
B_BOOT  <- 1000
set.seed(17)

hogares <- read_csv(file.path(instub, "hogares_menores.csv"), show_col_types = FALSE, guess_max = 100000)
v <- hogares %>% filter(ingreso_valido, !is.na(AUH))
g <- v$AUH == 1; w <- v$PONDIH; x <- v$IPCF_n

normalizar <- function(z) {
  out <- z
  for (q in unique(v$Q)) { m <- v$Q == q; out[m] <- z[m] / weighted.mean(z[m], w[m]) }
  out
}
VAR <- list(`Pesos 1T-2026, con AUH` = x, `Normalizado, con AUH` = normalizar(x),
            `Pesos 1T-2026, sin AUH` = v$IPCF_sin_auh, `Normalizado, sin AUH` = normalizar(v$IPCF_sin_auh))

sorteo <- muestreador(v$VIV)
REMUESTRAS <- replicate(B_BOOT, sorteo(), simplify = FALSE)
estimar <- function(z, ii) {
  a <- ii[g[ii]]; b <- ii[!g[ii]]
  c(gini_P = gini(z[a], w[a]), gini_N = gini(z[b], w[b]), theil_P = theil(z[a], w[a]), theil_N = theil(z[b], w[b]),
    entre = theil_entre(z[ii], w[ii], g[ii]))
}
resultados <- imap_dfr(VAR, function(z, nombre) {
  obs <- estimar(z, seq_along(z))
  bs  <- t(sapply(REMUESTRAS, function(ii) estimar(z, ii)))
  dG <- bs[, "gini_P"] - bs[, "gini_N"]; dT <- bs[, "theil_P"] - bs[, "theil_N"]
  tibble(ingreso = nombre,
         gini_perc = obs["gini_P"], gini_perc_lo = ic95(bs[, "gini_P"])[1], gini_perc_hi = ic95(bs[, "gini_P"])[2],
         gini_no = obs["gini_N"], gini_no_lo = ic95(bs[, "gini_N"])[1], gini_no_hi = ic95(bs[, "gini_N"])[2],
         dif_gini = obs["gini_P"] - obs["gini_N"], dif_gini_lo = ic95(dG)[1], dif_gini_hi = ic95(dG)[2], p_boot_gini = p_boot(dG),
         theil_perc = obs["theil_P"], theil_perc_lo = ic95(bs[, "theil_P"])[1], theil_perc_hi = ic95(bs[, "theil_P"])[2],
         theil_no = obs["theil_N"], theil_no_lo = ic95(bs[, "theil_N"])[1], theil_no_hi = ic95(bs[, "theil_N"])[2],
         dif_theil = obs["theil_P"] - obs["theil_N"], dif_theil_lo = ic95(dT)[1], dif_theil_hi = ic95(dT)[2],
         pct_theil_entre = obs["entre"], entre_lo = ic95(bs[, "entre"])[1], entre_hi = ic95(bs[, "entre"])[2])
}) %>% mutate(across(where(is.double), ~ round(.x, 4)))
print(resultados %>% select(ingreso, gini_perc, gini_no, dif_gini, dif_gini_lo, dif_gini_hi, dif_theil, pct_theil_entre), width = 200)
write_csv(resultados, file.path(outtab, "04_gini_theil_por_grupo.csv"))

peso_auh <- 100 * sum((v$AUH_monto * w)[g]) / sum((v$ITF_n * w)[g])
sin_ingreso <- sum(v$IPCF_sin_auh[g] <= 0)
cat(sprintf("La AUH es el %.0f%% del ITF de los hogares perceptores; %d hogares perceptores quedan con ingreso 0 sin AUH.\n", peso_auh, sin_ingreso))

cuantiles <- tibble(grupo = c("Hogares con AUH", "Hogares sin AUH")) %>%
  bind_cols(bind_rows(set_names(as.list(wq(x[g], w[g], c(.1, .25, .5, .75, .9))), paste0("P", c(10, 25, 50, 75, 90))),
                      set_names(as.list(wq(x[!g], w[!g], c(.1, .25, .5, .75, .9))), paste0("P", c(10, 25, 50, 75, 90))))) %>%
  mutate(P90_P10 = P90 / P10, across(P10:P90, round), P90_P10 = round(P90_P10, 2))
print(cuantiles)
write_csv(cuantiles, file.path(outtab, "04_cuantiles.csv"))

rc <- wq(x[g], w[g], c(.05, .95)); m <- x >= rc[1] & x <= rc[2]
rango_comun <- tibble(rango_min = round(rc[1]), rango_max = round(rc[2]),
                      gini_perc = gini(x[m & g], w[m & g]), gini_no = gini(x[m & !g], w[m & !g]))
print(rango_comun)
write_csv(rango_comun, file.path(outtab, "04_gini_rango_comun.csv"))

lx <- log(x); grupo <- factor(if_else(g, "P", "N"))
bf <- anova(lm(abs(lx - ave(lx, grupo, FUN = median)) ~ grupo))
mw <- wilcox.test(x[g], x[!g], exact = FALSE)
tests <- tibble(test = c("Brown-Forsythe (varianza de ln IPCF)", "Mann-Whitney (IPCF)"),
                estadistico = c(bf$`F value`[1], as.numeric(mw$statistic)),
                p = c(bf$`Pr(>F)`[1], mw$p.value),
                nota = c(sprintf("F(1; %d)", bf$Df[2]), sprintf("P(perceptor > no perceptor) = %.3f", as.numeric(mw$statistic) / (sum(g) * sum(!g)))))
print(tests)
write_csv(tests, file.path(outtab, "04_tests_complementarios.csv"))

lorenz <- function(z, ww, etiqueta) {
  o <- order(z); zs <- z[o]; ws <- ww[o]
  tibble(p = cumsum(ws) / sum(ws), L = cumsum(ws * zs) / sum(ws * zs), curva = etiqueta) %>%
    slice(round(seq(1, n(), length.out = 400)))
}
curvas <- bind_rows(lorenz(x[g], w[g], "Hogares con AUH"), lorenz(x[!g], w[!g], "Hogares sin AUH"),
                    lorenz(v$IPCF_sin_auh[g], w[g], "Hogares con AUH, sin contar la AUH"))
g_lor <- ggplot(curvas, aes(x = p, y = L, colour = curva, linetype = curva)) +
  geom_abline(slope = 1, intercept = 0, colour = GRIS, linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("Hogares con AUH" = ROJO, "Hogares sin AUH" = "#C08A1E", "Hogares con AUH, sin contar la AUH" = "#4A0F0F"), name = NULL) +
  scale_linetype_manual(values = c("Hogares con AUH" = "solid", "Hogares sin AUH" = "solid", "Hogares con AUH, sin contar la AUH" = "dashed"), name = NULL) +
  scale_x_continuous(labels = scales::label_percent(), expand = c(0, 0)) + scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
  labs(title = str_wrap("Con la AUH adentro, la curva de los hogares que la cobran queda más cerca de la igualdad; sin ella, ya no", 70),
       subtitle = "Curvas de Lorenz del IPCF. Cuanto más cerca de la diagonal, más igualitaria es la distribución",
       x = "% acumulado de hogares (de menor a mayor ingreso)", y = "% acumulado del ingreso",
       caption = str_wrap("Fuente: elaboración propia en base a la EPH continua (INDEC), 1T-2024 a 1T-2026. Hogares con menores de 18 e ingreso válido; ponderado con PONDIH.", 110)) +
  theme_g17() + theme(legend.position = "top", aspect.ratio = 0.75)
ggsave(file.path(outgraf, "04_lorenz.jpg"), g_lor, width = 24, height = 15, units = "cm", dpi = 200, device = ragg::agg_jpeg)

gi <- resultados %>% filter(str_detect(ingreso, "^Pesos")) %>%
  transmute(ingreso = if_else(str_detect(ingreso, "sin AUH"), "Sin contar la AUH", "Con la AUH incluida"),
            `Hogares con AUH` = gini_perc, `Hogares sin AUH` = gini_no,
            lo_p = gini_perc_lo, hi_p = gini_perc_hi, lo_n = gini_no_lo, hi_n = gini_no_hi) %>%
  pivot_longer(c(`Hogares con AUH`, `Hogares sin AUH`), names_to = "grupo", values_to = "gini") %>%
  mutate(lo = if_else(grupo == "Hogares con AUH", lo_p, lo_n), hi = if_else(grupo == "Hogares con AUH", hi_p, hi_n),
         ingreso = factor(ingreso, levels = c("Con la AUH incluida", "Sin contar la AUH")))
g_gini <- ggplot(gi, aes(x = ingreso, y = gini, colour = grupo)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.4), size = 0.7, linewidth = 0.9) +
  geom_text(aes(label = fmt_es(gini, 3)), position = position_dodge(width = 0.4), hjust = -0.45, size = 3.3, show.legend = FALSE) +
  scale_colour_manual(values = c("Hogares con AUH" = ROJO, "Hogares sin AUH" = "#7F7F7F"), name = NULL) +
  scale_y_continuous(limits = c(0.28, 0.45), labels = function(x) fmt_es(x, 2)) +
  labs(title = str_wrap("Los hogares con AUH tienen menos desigualdad solo cuando la AUH cuenta como ingreso", 70),
       subtitle = "Gini del IPCF con IC95% (bootstrap por vivienda, 1.000 remuestras)", x = NULL, y = "Gini",
       caption = str_wrap("Fuente: elaboración propia en base a la EPH continua (INDEC), 1T-2024 a 1T-2026. Hogares con menores de 18 e ingreso válido; ponderado con PONDIH.", 80)) +
  theme_g17() + theme(legend.position = "top")
ggsave(file.path(outgraf, "04_gini_con_y_sin_auh.jpg"), g_gini, width = 24, height = 13, units = "cm", dpi = 200, device = ragg::agg_jpeg)
cat("Listo: desigualdad.\n")
