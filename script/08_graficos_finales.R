library(tidyverse)
library(ggrepel)
instub  <- "input"
outtab  <- "output/tablas"
outgraf <- "output/graficos"
dir.create(outgraf, showWarnings = FALSE, recursive = TRUE)
for (f in list.files("utils", full.names = TRUE)) source(f)
ANCHO <- 24; ALTO <- 16.5
B_BOOT <- 1000

vcr <- read_csv(file.path(outtab, "07_vcr_por_rama.csv"), show_col_types = FALSE)
res <- read_csv(file.path(outtab, "07_vcr_resumen.csv"), show_col_types = FALSE)
o   <- read_csv(file.path(instub, "ocupados_menores.csv"), show_col_types = FALSE, guess_max = 100000)
p_tot   <- res$valor[1] / 100
inf_tot <- res$valor[2] / 100
rho     <- res$valor[3]
n_ocup  <- nrow(o)
fuente  <- paste0("Fuente: elaboración propia en base a la EPH continua (INDEC), 9 trimestres, 1T-2024 a 1T-2026; ponderado con PONDERA. n = ",
                  fmt_es(n_ocup, 0), " ocupados.")

cm <- vcr %>%
  mutate(rama = fct_reorder(rama, VCR),
         dest = rama %in% c("Servicio doméstico", "Construcción"),
         etiqueta = if_else(dest, paste0(fmt_es(VCR, 2), "  ·  ", fmt_es(p_auh, 0, "pct"), " vive en hogar con AUH"), fmt_es(VCR, 2)))
g_com <- ggplot(cm, aes(y = rama)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = GRIS_OSC, linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = VCR, yend = rama, colour = dest), linewidth = 1.1) +
  geom_point(aes(x = VCR, colour = dest, size = dest)) +
  geom_label(aes(x = VCR, label = etiqueta, colour = dest, fontface = if_else(dest, "bold", "plain")), hjust = 0, nudge_x = 0.04, size = 3.3,
             fill = "white", label.size = 0, label.padding = unit(0.12, "lines")) +
  scale_colour_manual(values = c(`TRUE` = ROJO, `FALSE` = GRIS), guide = "none") +
  scale_size_manual(values = c(`TRUE` = 4.2, `FALSE` = 2.6), guide = "none") +
  scale_x_continuous(limits = c(0, 3.15), breaks = c(0, 0.5, 1, 1.5, 2), labels = function(x) fmt_es(x, 1), expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.2))) +
  annotate("text", x = 1.03, y = nlevels(cm$rama) + 0.75, hjust = 0, vjust = 0.5, size = 3, colour = GRIS_OSC,
           label = paste0("Promedio de los ocupados: ", fmt_es(p_tot, 0, "pct"), " vive en un hogar con AUH (VCR = 1)")) +
  annotate("segment", x = 0.6, xend = 0.6, y = 0.7, yend = 4.3, colour = GRIS_OSC, linewidth = 0.5) +
  annotate("label", x = 0.65, y = 2.5, hjust = 0, vjust = 0.5, size = 3.1, colour = GRIS_OSC, lineheight = 1.1, fill = "white", label.size = 0,
           label = "Información y finanzas, enseñanza,\ninmobiliarias y administración pública:\nmenos de un tercio del promedio") +
  labs(title = "En construcción y servicio doméstico, más de la mitad de los ocupados\nvive en un hogar con AUH: casi el doble que el promedio",
       subtitle = "VCR de la AUH por rama de actividad. Mide cuántas veces más (o menos) que el promedio se da que un ocupado\nviva en un hogar con AUH. Ocupados de 18 a 64 años en hogares con menores de 18, 1T-2024 a 1T-2026.",
       x = "VCR de AUH", y = NULL,
       caption = paste0("La AUH del hogar se atribuye a todos sus ocupados.\n", fuente)) +
  theme_g17() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = GRIS_CLARO, linewidth = 0.3),
                      axis.text.y = element_text(size = 9.8))
ggsave(file.path(outgraf, "grupo_17_comunicativo.jpg"), g_com, width = ANCHO, height = ALTO, units = "cm", dpi = 300, device = ragg::agg_jpeg)

ex <- vcr %>% mutate(rad = 12 * sqrt(ocupados / max(ocupados)))

ex$ny <- case_when(ex$rama == "Salud y servicios sociales" ~ -0.17, ex$rama == "Administración pública" ~ 0.08, TRUE ~ 0)
ex$nx <- case_when(ex$rama == "Salud y servicios sociales" ~ 0.05, TRUE ~ 0)
cuad <- list(hjust = c(1, 0, 0, 1), x = c(0.99, 0.01, 0.01, 0.99), y = c(3.3, 3.3, 0.1, 0.1), vjust = c(1, 1, 0, 0),
             txt = c("Más informal y más AUH que el promedio", "Más formal y más AUH que el promedio",
                     "Más formal y menos AUH que el promedio", "Más informal y menos AUH que el promedio"))
g_exp <- ggplot(ex, aes(x = informalidad, y = VCR)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = GRIS_OSC, linewidth = 0.4) +
  geom_vline(xintercept = inf_tot, linetype = "dashed", colour = GRIS_OSC, linewidth = 0.4) +
  geom_linerange(aes(ymin = lo, ymax = hi), colour = "#8A8A8A", linewidth = 0.45) +
  geom_point(aes(size = ocupados, fill = pct_mujer), shape = 21, colour = "#1a1a1a", stroke = 0.5, alpha = 0.95) +
  geom_text_repel(aes(label = rama), nudge_x = ex$nx, nudge_y = ex$ny, size = 3.0, colour = "#1a1a1a", min.segment.length = 0.15,
                  segment.colour = "#777777", point.size = ex$rad, segment.size = 0.3, box.padding = 0.55, point.padding = 0.2,
                  force = 3, max.iter = 20000, max.overlaps = Inf, seed = 17, lineheight = 0.95) +
  annotate("text", x = cuad$x, y = cuad$y, label = cuad$txt, hjust = cuad$hjust, vjust = cuad$vjust, size = 3, colour = "#666666", fontface = "italic") +

  annotate("text", x = 0.99, y = 1.04, label = "VCR = 1 (promedio)", hjust = 1, vjust = 0, size = 2.7, colour = GRIS_OSC) +
  annotate("label", x = inf_tot, y = 2.7, label = paste0("Informalidad total: ", fmt_es(inf_tot, 0, "pct")),
           hjust = 0.5, vjust = 0.5, size = 2.7, colour = GRIS_OSC, fill = "white", label.size = 0) +
  scale_fill_viridis_c(option = "viridis", direction = -1, begin = 0.08, end = 0.98, labels = function(x) fmt_es(x, 0, "pct"), limits = c(0, 1),
                       breaks = c(0, .25, .5, .75, 1), name = "Mujeres entre los ocupados de la rama",
                       guide = guide_colourbar(barwidth = 8, barheight = 0.55, title.position = "top")) +
  scale_size_area(max_size = 12, guide = "none") +
  scale_x_continuous(labels = function(x) fmt_es(x, 0, "pct"), limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_log10(breaks = c(0.15, 0.25, 0.5, 1, 2), labels = function(x) fmt_es(x, 2), limits = c(0.095, 3.4)) +
  labs(title = "AUH e informalidad por rama de actividad: ¿en qué ramas se concentran los hogares con AUH?",
       subtitle = paste0("Cada punto es una rama de actividad; su tamaño indica cuántos ocupados tiene. Las barras grises son el IC95% del VCR.\n",
                         "Correlación de Spearman entre informalidad y VCR: ", fmt_es(rho, 2), ". Ocupados de 18 a 64 años en hogares con menores de 18, 1T-2024 a 1T-2026."),
       x = "Informalidad: % de asalariados sin descuento jubilatorio",
       y = "VCR de AUH (escala logarítmica)",
       caption = paste0("VCR = % de ocupados de la rama en hogares con AUH ÷ % en el total (", fmt_es(p_tot, 0, "pct"), "). IC95% por bootstrap de viviendas (", B_BOOT, " remuestras).\n",
                        "Informalidad = asalariados sin descuento jubilatorio (PP07H). La AUH del hogar se atribuye a todos sus ocupados.\n", fuente)) +
  theme_g17() + theme(plot.title = element_text(size = 13.5), legend.position = "bottom", legend.title = element_text(size = 8.8, colour = GRIS_OSC), legend.text = element_text(size = 8.5),
                      legend.margin = margin(t = 2), legend.box.spacing = unit(0.2, "cm"), panel.grid.major = element_line(colour = GRIS_CLARO, linewidth = 0.3))
ggsave(file.path(outgraf, "grupo_17_exploratorio.jpg"), g_exp, width = ANCHO, height = ALTO, units = "cm", dpi = 300, device = ragg::agg_jpeg)
cat("Listo: gráficos finales en", outgraf, "\n")
