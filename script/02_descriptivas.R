library(tidyverse)
instub  <- "input"
outtab  <- "output/tablas"
outgraf <- "output/graficos"
dir.create(outtab, showWarnings = FALSE, recursive = TRUE)
dir.create(outgraf, showWarnings = FALSE, recursive = TRUE)
for (f in list.files("utils", full.names = TRUE)) source(f)

hogares     <- read_csv(file.path(instub, "hogares_menores.csv"), show_col_types = FALSE, guess_max = 100000)
perceptoras <- read_csv(file.path(instub, "perceptoras.csv"), show_col_types = FALSE, guess_max = 100000)
glimpse(hogares)

v <- hogares %>% filter(ingreso_valido, !is.na(AUH)) %>%
  mutate(grupo = if_else(AUH == 1, "Hogares con AUH", "Hogares sin AUH"))

diccionario <- tribble(
  ~variable,     ~base,       ~tipo,                 ~descripcion,
  "V5_01",       "hogar",     "categórica (1/2/9)",  "El hogar recibe AUH (1 = sí, 2 = no, 9 = Ns/Nr)",
  "V5_01_M",     "individual","numérica ($)",        "Monto de AUH que cobra la persona (0 = no cobra)",
  "IPCF",        "hogar",     "numérica ($)",        "Ingreso per cápita familiar; 0 si el hogar no declara ingresos",
  "ITF",         "hogar",     "numérica ($)",        "Ingreso total familiar",
  "IX_TOT",      "hogar",     "numérica (conteo)",   "Cantidad de integrantes del hogar",
  "REGION",      "hogar",     "categórica (6)",      "Región estadística (GBA, NOA, NEA, Cuyo, Pampeana, Patagonia)",
  "PONDIH",      "hogar",     "numérica",            "Ponderador de ingresos del hogar (0 = sin ingreso válido)",
  "PONDERA",     "ambas",     "numérica",            "Ponderador general",
  "CH04 / CH06", "individual","categórica / numérica","Sexo y edad",
  "ESTADO",      "individual","categórica (0-4)",    "Condición de actividad (1 = ocupado)",
  "CAT_OCUP",    "individual","categórica (1-4)",    "Categoría ocupacional (3 = asalariado)",
  "PP04A",       "individual","categórica (1-3)",    "Carácter jurídico del empleo (1 = estatal, 2 = privado)",
  "PP04B_COD",   "individual","categórica (CAES)",   "Código de actividad de la ocupación principal (CAES-Mercosur)",
  "PP07H",       "individual","categórica (1/2)",    "El asalariado tiene descuento jubilatorio (2 = no, es decir informal)")
write_csv(diccionario, file.path(outtab, "02_diccionario_variables.csv"))

asimetria <- function(x, w) { m <- weighted.mean(x, w); sum(w * (x - m)^3) / sum(w) / (sum(w * (x - m)^2) / sum(w))^1.5 }
resumen_ipcf <- v %>%
  bind_rows(v %>% mutate(grupo = "Todos los hogares")) %>%
  group_by(grupo) %>%
  summarise(hogares_trim = n(), media = weighted.mean(IPCF_n, PONDIH),
            P10 = wq(IPCF_n, PONDIH, .1), P25 = wq(IPCF_n, PONDIH, .25), mediana = wq(IPCF_n, PONDIH, .5),
            P75 = wq(IPCF_n, PONDIH, .75), P90 = wq(IPCF_n, PONDIH, .9),
            desvio = sqrt(weighted.mean((IPCF_n - weighted.mean(IPCF_n, PONDIH))^2, PONDIH)),
            asimetria = asimetria(IPCF_n, PONDIH), asimetria_ln = asimetria(log(IPCF_n), PONDIH),
            .groups = "drop") %>%
  mutate(cv = desvio / media, across(c(media, P10:desvio), round))
print(resumen_ipcf, width = 200)
write_csv(resumen_ipcf, file.path(outtab, "02_descriptivas_ipcf.csv"))

cortes <- wq(v$IPCF_n, v$PONDIH, seq(.1, .9, .1))
v <- v %>% mutate(decil = 1 + findInterval(IPCF_n, cortes, left.open = TRUE))
cobertura <- v %>%
  group_by(decil) %>%
  summarise(pct_con_auh = 100 * weighted.mean(AUH, PONDIH),
            ipcf_mediana = wq(IPCF_n, PONDIH, .5),
            peso_auh_en_itf = 100 * sum((AUH_monto * PONDIH)[AUH == 1]) / sum((ITF_n * PONDIH)[AUH == 1]),
            .groups = "drop") %>%
  mutate(across(c(pct_con_auh, peso_auh_en_itf), ~ round(.x, 1)), ipcf_mediana = round(ipcf_mediana))
print(cobertura)
write_csv(cobertura, file.path(outtab, "02_cobertura_auh_por_decil.csv"))
peso_total <- 100 * sum((v$AUH_monto * v$PONDIH)[v$AUH == 1]) / sum((v$ITF_n * v$PONDIH)[v$AUH == 1])
cat(sprintf("La AUH es el %.0f%% del ITF de los hogares que la cobran.\n", peso_total))

perfil <- perceptoras %>%
  summarise(personas_muestra = n(), pct_mujeres = 100 * weighted.mean(CH04 == "2", PONDERA),
            edad_mediana = wq(CH06, PONDERA, .5),
            pct_ocupadas = 100 * weighted.mean(ESTADO == "1", PONDERA),
            pct_desocupadas = 100 * weighted.mean(ESTADO == "2", PONDERA),
            pct_inactivas = 100 * weighted.mean(ESTADO == "3", PONDERA),
            pct_estatal_entre_ocupadas = 100 * sum(PONDERA[ESTADO == "1" & PP04A == "1"]) / sum(PONDERA[ESTADO == "1"])) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))
print(t(perfil))
write_csv(perfil, file.path(outtab, "02_perfil_perceptoras.csv"))

g_dist <- ggplot(v, aes(x = IPCF_n / 1000, weight = PONDIH / sum(PONDIH), fill = grupo)) +
  geom_density(aes(y = after_stat(density)), alpha = 0.55, colour = NA, adjust = 1.3) +
  scale_x_log10(breaks = c(30, 100, 300, 1000, 3000), labels = function(x) fmt_es(x, 0)) +
  scale_fill_manual(values = c("Hogares con AUH" = ROJO, "Hogares sin AUH" = GRIS), name = NULL) +
  labs(title = str_wrap("Los hogares con AUH se concentran en ingresos más bajos, pero las dos distribuciones se superponen", 70),
       subtitle = "Densidad del ingreso per cápita familiar (IPCF), en miles de pesos de 1T-2026 por mes (escala logarítmica)",
       x = "IPCF (miles de $, escala logarítmica)", y = "Densidad",
       caption = str_wrap(paste0("Fuente: elaboración propia en base a la EPH continua (INDEC), 1T-2024 a 1T-2026. Hogares con menores de 18 e ingreso válido (n = ",
                        fmt_es(nrow(v), 0), "); ponderado con PONDIH."), 110)) +
  theme_g17() + theme(legend.position = "top", axis.text.y = element_blank(), panel.grid.major.y = element_blank())
ggsave(file.path(outgraf, "02_distribucion_ipcf.jpg"), g_dist, width = 24, height = 13, units = "cm", dpi = 200, device = ragg::agg_jpeg)

g_cob <- ggplot(cobertura, aes(x = factor(decil), y = pct_con_auh)) +
  geom_col(fill = ROJO, width = 0.7) +
  geom_text(aes(label = paste0(fmt_es(pct_con_auh, 0), "%")), vjust = -0.5, size = 3.4, colour = GRIS_OSC) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
  labs(title = str_wrap("La AUH llega a la mayoría de los hogares con menores del primer decil y casi no aparece en los últimos", 70),
       subtitle = "% de hogares con menores que cobran AUH, según decil de IPCF (1 = más pobre)",
       x = "Decil de IPCF", y = NULL,
       caption = str_wrap("Fuente: elaboración propia en base a la EPH continua (INDEC), 1T-2024 a 1T-2026. Hogares con menores de 18 e ingreso válido; ponderado con PONDIH.", 110)) +
  theme_g17() + theme(panel.grid.major.x = element_blank())
ggsave(file.path(outgraf, "02_cobertura_por_decil.jpg"), g_cob, width = 24, height = 13, units = "cm", dpi = 200, device = ragg::agg_jpeg)
cat("Listo: tablas en", outtab, "y gráficos en", outgraf, "\n")
