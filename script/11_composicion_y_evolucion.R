library(tidyverse)
instub  <- "input"
outtab  <- "output/tablas"
outgraf <- "output/graficos"
dir.create(outtab, showWarnings = FALSE, recursive = TRUE)
dir.create(outgraf, showWarnings = FALSE, recursive = TRUE)
for (f in list.files("utils", full.names = TRUE)) source(f)

hogares  <- read_csv(file.path(instub, "hogares_menores.csv"), show_col_types = FALSE, guess_max = 100000)
ocupados <- read_csv(file.path(instub, "ocupados_menores.csv"), show_col_types = FALSE, guess_max = 100000)
v <- hogares %>% filter(!is.na(AUH), ingreso_valido)

comp <- v %>%
  group_by(grupo = if_else(AUH == 1, "Hogares con AUH", "Hogares sin AUH")) %>%
  summarise(hogares_trim = n(),
            menores_prom = weighted.mean(n_menores, PONDIH),
            adultos_prom = weighted.mean(n_adultos, PONDIH),
            adultos_ocup_prom = weighted.mean(n_adultos_ocup, PONDIH),
            menores_por_adulto_ocup = sum(n_menores * PONDIH) / sum(n_adultos_ocup * PONDIH),
            pct_sin_adulto_ocup = 100 * weighted.mean(n_adultos_ocup == 0, PONDIH),
            pct_jefe_no_ocupado = 100 * weighted.mean(ESTADO_J != 1, PONDIH),
            pct_jefa_mujer = 100 * weighted.mean(SEXO_J == 2, PONDIH),
            edad_mediana_jefe = wq(EDAD_J, PONDIH, .5), .groups = "drop") %>%
  mutate(across(where(is.double), ~ round(.x, 2)))
print(comp, width = 200)
write_csv(comp, file.path(outtab, "11_composicion_hogar.csv"))

inf <- ocupados %>%
  filter(asalariado) %>%
  group_by(grupo = case_when(perc ~ "Asalariadas que cobran AUH", AUH ~ "Asalariados de hogares con AUH (sin perceptora)", TRUE ~ "Asalariados de hogares sin AUH")) %>%
  summarise(asalariados = n(), pct_informal = 100 * weighted.mean(informal, PONDERA), .groups = "drop") %>%
  mutate(pct_informal = round(pct_informal, 1))
print(inf)
write_csv(inf, file.path(outtab, "11_informalidad_asalariados.csv"))

orden_q <- v %>% distinct(Q, ANO4, TRIMESTRE) %>% arrange(ANO4, TRIMESTRE) %>% pull(Q)
evol <- v %>% filter(ingreso_valido) %>%
  group_by(Q, grupo = if_else(AUH == 1, "Hogares con AUH", "Hogares sin AUH")) %>%
  summarise(hogares = n(), mediana = wq(IPCF_n, PONDIH, .5), .groups = "drop") %>%
  mutate(Q = factor(Q, levels = orden_q)) %>% arrange(Q, grupo)
print(evol, n = 20)
write_csv(evol %>% mutate(mediana = round(mediana)), file.path(outtab, "11_mediana_ipcf_por_trimestre.csv"))

g_evol <- ggplot(evol, aes(x = Q, y = mediana / 1000, colour = grupo, group = grupo)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_colour_manual(values = c("Hogares con AUH" = ROJO, "Hogares sin AUH" = "#7F7F7F"), name = NULL) +
  scale_y_continuous(limits = c(0, NA), labels = function(z) fmt_es(z, 0)) +
  labs(title = str_wrap("La brecha de ingreso entre hogares con y sin AUH se mantiene en los nueve trimestres", 70),
       subtitle = "Mediana del IPCF por trimestre (miles de pesos de 1T-2026 por mes)", x = NULL, y = NULL,
       caption = str_wrap("Fuente: elaboración propia en base a la EPH continua (INDEC), 1T-2024 a 1T-2026. Hogares con menores de 18 e ingreso válido; ponderado con PONDIH.", 110)) +
  theme_g17() + theme(legend.position = "top")
ggsave(file.path(outgraf, "11_mediana_ipcf_por_trimestre.jpg"), g_evol, width = 24, height = 13, units = "cm", dpi = 200, device = ragg::agg_jpeg)
cat("Listo: composición del hogar y evolución trimestral.\n")
