library(tidyverse)
instub  <- "input"
outtab  <- "output/tablas"
outgraf <- "output/graficos"
dir.create(outtab, showWarnings = FALSE, recursive = TRUE)
dir.create(outgraf, showWarnings = FALSE, recursive = TRUE)
for (f in list.files("utils", full.names = TRUE)) source(f)

hogares  <- read_csv(file.path(instub, "hogares_menores.csv"), show_col_types = FALSE, guess_max = 100000)
ocupados <- read_csv(file.path(instub, "ocupados_menores.csv"), show_col_types = FALSE, guess_max = 100000)
v <- hogares %>% filter(ingreso_valido, !is.na(AUH))
orden_q <- hogares %>% distinct(Q, ANO4, TRIMESTRE) %>% arrange(ANO4, TRIMESTRE) %>% pull(Q)

por_q <- map_dfr(orden_q, function(q) {
  h <- v %>% filter(Q == q); g <- h$AUH == 1; w <- h$PONDIH; x <- h$IPCF_n
  o <- ocupados %>% filter(Q == q)
  vcr_q <- o %>% group_by(rama) %>% summarise(vcr = weighted.mean(AUH, PONDERA) / weighted.mean(o$AUH, o$PONDERA), .groups = "drop")
  ocup <- h$ESTADO_J == 1
  tibble(trimestre = q, hogares_validos = nrow(h), hogares_con_auh = sum(g),
         gini_perc = gini(x[g], w[g]), gini_no = gini(x[!g], w[!g]),
         dif_gini = gini(x[g], w[g]) - gini(x[!g], w[!g]),
         dif_gini_sin_auh = gini(h$IPCF_sin_auh[g], w[g]) - gini(h$IPCF_sin_auh[!g], w[!g]),
         dif_theil = theil(x[g], w[g]) - theil(x[!g], w[!g]),
         V_rama = chi_cuadrado(tabla_ponderada(o, "rama", "AUH"))$V,
         vcr_serv_domestico = vcr_q$vcr[vcr_q$rama == "Servicio doméstico"],
         vcr_construccion = vcr_q$vcr[vcr_q$rama == "Construcción"],
         vcr_informacion = vcr_q$vcr[vcr_q$rama == "Información, comunicaciones y finanzas"],
         pct_theil_sector_perc = theil_entre(x[g], w[g], h$sector_jefe[g]),
         mediana_perc = wq(x[g], w[g], .5), mediana_no = wq(x[!g], w[!g], .5))
}) %>% mutate(across(where(is.double), ~ round(.x, 3)))
print(por_q, width = 250)
write_csv(por_q, file.path(outtab, "09_robustez_por_trimestre.csv"))
cat(sprintf("Dif. de Gini < 0 en %d de %d trimestres; dif. sin AUH > 0 en %d de %d; V de la rama entre %.2f y %.2f.\n",
            sum(por_q$dif_gini < 0), nrow(por_q), sum(por_q$dif_gini_sin_auh > 0), nrow(por_q), min(por_q$V_rama), max(por_q$V_rama)))

g_q <- por_q %>% select(trimestre, `Hogares con AUH` = gini_perc, `Hogares sin AUH` = gini_no) %>%
  pivot_longer(-trimestre, names_to = "grupo", values_to = "gini") %>%
  mutate(trimestre = factor(trimestre, levels = orden_q)) %>%
  ggplot(aes(x = trimestre, y = gini, colour = grupo, group = grupo)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2.4) +
  scale_colour_manual(values = c("Hogares con AUH" = ROJO, "Hogares sin AUH" = "#7F7F7F"), name = NULL) +
  scale_y_continuous(limits = c(0.25, 0.45), labels = function(x) fmt_es(x, 2)) +
  labs(title = str_wrap("El Gini de los hogares con AUH es menor que el de los demás en los nueve trimestres", 70),
       subtitle = "Gini del IPCF por trimestre (la AUH cuenta como ingreso)", x = NULL, y = "Gini",
       caption = str_wrap("Fuente: elaboración propia en base a la EPH continua (INDEC), 1T-2024 a 1T-2026. Hogares con menores de 18 e ingreso válido; ponderado con PONDIH.", 110)) +
  theme_g17() + theme(legend.position = "top")
ggsave(file.path(outgraf, "09_gini_por_trimestre.jpg"), g_q, width = 24, height = 13, units = "cm", dpi = 200, device = ragg::agg_jpeg)
cat("Listo: robustez por trimestre.\n")
