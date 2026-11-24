library(tidyverse)
instub  <- "input"
outtab  <- "output/tablas"
outgraf <- "output/graficos"
dir.create(outtab, showWarnings = FALSE, recursive = TRUE)
dir.create(outgraf, showWarnings = FALSE, recursive = TRUE)
for (f in list.files("utils", full.names = TRUE)) source(f)
set.seed(17)

hogares <- read_csv(file.path(instub, "hogares_menores.csv"), show_col_types = FALSE, guess_max = 100000)
v <- hogares %>% filter(ingreso_valido, !is.na(AUH))
g <- v$AUH == 1; w <- v$PONDIH; x <- v$IPCF_n
ORDEN <- c("Comercio", "Serv. personales, doméstico y gastronomía", "Sector público, enseñanza y salud",
           "Industria, construcción y transporte", "Otras ramas", "Jefe/a no ocupado/a")

kruskal <- function(xx, ss) {
  ss <- factor(ss); kt <- kruskal.test(xx ~ ss); k <- nlevels(ss); n <- length(xx)
  tibble(n = n, H = as.numeric(kt$statistic), gl = k - 1, p = kt$p.value, eta2 = (as.numeric(kt$statistic) - k + 1) / (n - k))
}
ocup <- v$ESTADO_J == 1
kw <- bind_rows(
  kruskal(x[g], v$sector_jefe[g]) %>% mutate(grupo = "Perceptores, todos"),
  kruskal(x[g & ocup], v$sector_jefe[g & ocup]) %>% mutate(grupo = "Perceptores, jefe/a ocupado/a"),
  kruskal(x[!g], v$sector_jefe[!g]) %>% mutate(grupo = "No perceptores, todos"),
  kruskal(x[!g & ocup], v$sector_jefe[!g & ocup]) %>% mutate(grupo = "No perceptores, jefe/a ocupado/a")) %>%
  select(grupo, everything()) %>% mutate(across(c(H, eta2), ~ round(.x, 3)), p = signif(p, 3))
print(kw)
write_csv(kw, file.path(outtab, "06_kruskal_wallis.csv"))

descomponer <- function(xx, ww, ss) {
  Y <- sum(xx * ww); tot <- sum(ww)
  filas <- map_dfr(ORDEN[ORDEN %in% ss], function(k) {
    m <- ss == k
    tibble(sector = k, n = sum(m), pct_hogares = 100 * sum(ww[m]) / tot, pct_ingreso = 100 * sum(xx[m] * ww[m]) / Y,
           mediana = wq(xx[m], ww[m], .5), gini = gini(xx[m], ww[m]), theil = theil(xx[m], ww[m]))
  })
  list(theil = theil(xx, ww), pct_entre = theil_entre(xx, ww, ss), filas = filas)
}
grupos <- list(Perceptores = g, `No perceptores` = !g, Todos = rep(TRUE, length(g)))
decomp <- imap(grupos, function(m, nombre) {
  d <- descomponer(x[m], w[m], v$sector_jefe[m])
  ii0 <- which(m); sorteo <- muestreador(v$VIV[m])
  bs <- replicate(1000, { ii <- sorteo(); theil_entre(x[ii0][ii], w[ii0][ii], v$sector_jefe[ii0][ii]) })
  list(nombre = nombre, d = d, ic = ic95(bs))
})
resumen_entre <- map_dfr(decomp, ~ tibble(grupo = .x$nombre, theil_total = .x$d$theil, pct_entre_sectores = .x$d$pct_entre,
                                          ic_lo = .x$ic[1], ic_hi = .x$ic[2])) %>%
  mutate(across(where(is.double), ~ round(.x, 3)))

d_ocup <- descomponer(x[g & ocup], w[g & ocup], v$sector_jefe[g & ocup])
resumen_entre <- bind_rows(resumen_entre, tibble(grupo = "Perceptores, jefe/a ocupado/a", theil_total = round(d_ocup$theil, 3),
                                                 pct_entre_sectores = round(d_ocup$pct_entre, 3), ic_lo = NA, ic_hi = NA))

resumen_entre <- bind_rows(resumen_entre, tibble(grupo = "Partición perceptores / no perceptores",
                                                 theil_total = round(theil(x, w), 3), pct_entre_sectores = round(theil_entre(x, w, g), 3), ic_lo = NA, ic_hi = NA))
print(resumen_entre)
write_csv(resumen_entre, file.path(outtab, "06_theil_entre_sectores.csv"))

tabla_sector <- decomp$Perceptores$d$filas %>% mutate(across(where(is.double), ~ round(.x, 3)))
print(tabla_sector)
write_csv(tabla_sector, file.path(outtab, "06_sector_jefe_perceptores.csv"))
write_csv(decomp[["No perceptores"]]$d$filas %>% mutate(across(where(is.double), ~ round(.x, 3))),
          file.path(outtab, "06_sector_jefe_no_perceptores.csv"))
cat(sprintf("Jefe/a no ocupado/a: %.0f%% de los hogares perceptores vs %.0f%% de los no perceptores\n",
            100 * weighted.mean(v$ESTADO_J[g] != 1, w[g]), 100 * weighted.mean(v$ESTADO_J[!g] != 1, w[!g])))

g_sec <- decomp$Perceptores$d$filas %>%
  mutate(sector = fct_reorder(sector, mediana), sin_jefe = sector == "Jefe/a no ocupado/a") %>%
  ggplot(aes(x = mediana / 1000, y = sector, fill = sin_jefe)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0("$", fmt_es(mediana / 1000, 0), " mil")), hjust = -0.1, size = 3.3, colour = GRIS_OSC) +
  scale_fill_manual(values = c(`TRUE` = ROJO, `FALSE` = GRIS), guide = "none") +
  scale_x_continuous(limits = c(0, 400), expand = expansion(mult = c(0, 0.02))) +
  labs(title = str_wrap("Entre los hogares con AUH, el sector del jefe/a casi no cambia el ingreso; lo que lo baja es no tener jefe/a ocupado/a", 70),
       subtitle = "Mediana del IPCF de los hogares con AUH según el sector donde trabaja el jefe/a (miles de $ de 1T-2026 por mes)",
       x = NULL, y = NULL,
       caption = str_wrap(paste0("Kruskal-Wallis: η² = ", fmt_es(kw$eta2[1], 3), " con todos los hogares y ", fmt_es(kw$eta2[2], 3),
                        " si el jefe/a está ocupado/a. Fuente: elaboración propia en base a la EPH continua (INDEC), 1T-2024 a 1T-2026; PONDIH."), 110)) +
  theme_g17() + theme(panel.grid.major.y = element_blank())
ggsave(file.path(outgraf, "06_mediana_ipcf_por_sector_jefe.jpg"), g_sec, width = 24, height = 12, units = "cm", dpi = 200, device = ragg::agg_jpeg)
cat("Listo: Kruskal-Wallis y descomposición del Theil.\n")
