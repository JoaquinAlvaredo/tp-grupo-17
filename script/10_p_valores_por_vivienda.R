library(tidyverse)
instub  <- "input"
outtab  <- "output/tablas"
dir.create(outtab, showWarnings = FALSE, recursive = TRUE)
for (f in list.files("utils", full.names = TRUE)) source(f)
set.seed(17)
B <- 1000

ocupados <- read_csv(file.path(instub, "ocupados_menores.csv"), show_col_types = FALSE, guess_max = 100000)
hogares  <- read_csv(file.path(instub, "hogares_menores.csv"), show_col_types = FALSE, guess_max = 100000)
v <- hogares %>% filter(ingreso_valido, !is.na(AUH))
g <- v$AUH == 1; x <- v$IPCF_n

wald_chi2 <- function(df, columna, dummy) {
  cat_ <- factor(as.character(df[[columna]])); a <- df$PONDERA * as.numeric(df[[dummy]]); w <- df$PONDERA
  estimador <- function(ii) {
    s <- rowsum(cbind(a[ii], w[ii]), cat_[ii], reorder = TRUE)
    d <- s[, 1] / s[, 2] - sum(s[, 1]) / sum(s[, 2])
    d[-length(d)]
  }
  wald_conglomerado(estimador, df$VIV, B)
}

clasicos <- read_csv(file.path(outtab, "05_chi2_resumen_chequeos.csv"), show_col_types = FALSE)
v_hog <- hogares %>% filter(ingreso_valido)
cortes <- wq(v_hog$IPCF_n, v_hog$PONDIH, c(1 / 3, 2 / 3))
oi <- ocupados %>% inner_join(v_hog %>% select(HOG, IPCF_n), by = "HOG") %>%
  mutate(tercil = case_when(IPCF_n <= cortes[1] ~ "Bajo", IPCF_n <= cortes[2] ~ "Medio", TRUE ~ "Alto"))
chequeos <- tribble(
  ~chequeo, ~df, ~columna, ~dummy, ~agrupar,
  "Base: 15 ramas x hogar con AUH", ocupados, "rama", "AUH", FALSE,
  "15 ramas x ocupada que cobra AUH (individual)", ocupados, "rama", "perc", FALSE,
  "Solo mujeres, 15 ramas x hogar con AUH", filter(ocupados, mujer), "rama", "AUH", FALSE,
  "Solo varones, 15 ramas x hogar con AUH", filter(ocupados, !mujer), "rama", "AUH", FALSE,
  "Estatal vs privado x hogar con AUH", filter(ocupados, PP04A %in% c(1, 2)), "PP04A", "AUH", FALSE,
  "Tercil de ingreso Bajo, 15 ramas x hogar con AUH", filter(oi, tercil == "Bajo"), "rama", "AUH", FALSE,
  "Tercil de ingreso Medio, 15 ramas x hogar con AUH", filter(oi, tercil == "Medio"), "rama", "AUH", FALSE,
  "Tercil de ingreso Alto, 15 ramas x hogar con AUH", filter(oi, tercil == "Alto"), "rama", "AUH", FALSE)
res_chi <- pmap_dfr(chequeos, function(chequeo, df, columna, dummy, agrupar) {
  w <- wald_chi2(df, columna, dummy)
  tibble(chequeo = chequeo, wald = w$wald, gl = w$gl, p_vivienda = w$p)
}) %>% left_join(clasicos %>% select(chequeo, chi2_clasico = chi2), by = "chequeo") %>%
  mutate(razon = wald / chi2_clasico)
chi_principal <- res_chi %>% filter(chequeo == "Base: 15 ramas x hogar con AUH")
cat(sprintf("Chi-cuadrado principal, p por vivienda: Wald = %.0f (clásico %.0f), gl = %d, p %s\n", chi_principal$wald,
            chi_principal$chi2_clasico, chi_principal$gl, ifelse(chi_principal$p_vivienda < 0.001, "< 0,001", signif(chi_principal$p_vivienda, 2))))
print(res_chi, width = 200)
write_csv(res_chi %>% mutate(across(c(wald, razon), ~ round(.x, 3)), p_vivienda = signif(p_vivienda, 3)),
          file.path(outtab, "10_chi2_chequeos_por_vivienda.csv"))

wald_kw <- function(xx, ss, viv) {
  ss <- factor(ss); r <- rank(xx)
  estimador <- function(ii) {
    s <- rowsum(cbind(r[ii], 1), ss[ii], reorder = TRUE)
    d <- s[, 1] / s[, 2] - mean(r[ii])
    d[-length(d)]
  }
  wald_conglomerado(estimador, viv, B)
}
ocup <- v$ESTADO_J == 1
grupos_kw <- list("Perceptores, todos" = g, "Perceptores, jefe/a ocupado/a" = g & ocup,
                  "No perceptores, todos" = !g, "No perceptores, jefe/a ocupado/a" = !g & ocup)
kw_clasico <- read_csv(file.path(outtab, "06_kruskal_wallis.csv"), show_col_types = FALSE)
res_kw <- imap_dfr(grupos_kw, function(m, nombre) {
  w <- wald_kw(x[m], v$sector_jefe[m], v$VIV[m])
  tibble(test = paste0("Kruskal-Wallis: ", nombre), wald = w$wald, gl = w$gl, p_vivienda = w$p)
}) %>% left_join(kw_clasico %>% transmute(test = paste0("Kruskal-Wallis: ", grupo), estadistico = H, p_clasico = p), by = "test")
print(res_kw, width = 200)

lx <- log(x)
est_mw <- local({ r <- rank(x); function(ii) mean(r[ii][g[ii]]) - mean(r[ii][!g[ii]]) })
est_bf <- function(ii) {
  l <- lx[ii]; gg <- g[ii]
  mean(abs(l[gg] - median(l[gg]))) - mean(abs(l[!gg] - median(l[!gg])))
}
w_mw <- wald_conglomerado(est_mw, v$VIV, B)
w_bf <- wald_conglomerado(est_bf, v$VIV, B)
tests_clasicos <- read_csv(file.path(outtab, "04_tests_complementarios.csv"), show_col_types = FALSE)
res_comp <- tibble(test = tests_clasicos$test, estadistico = tests_clasicos$estadistico, p_clasico = tests_clasicos$p,
                   wald = c(w_bf$wald, w_mw$wald), gl = 1, p_vivienda = c(w_bf$p, w_mw$p))
print(res_comp, width = 200)

tabla <- bind_rows(
  clasicos %>% filter(chequeo == "Base: 15 ramas x hogar con AUH") %>%
    transmute(test = "Chi-cuadrado: 15 ramas x hogar con AUH", estadistico = chi2, p_clasico = p) %>%
    bind_cols(chi_principal %>% select(wald, gl, p_vivienda)),
  res_kw %>% select(test, estadistico, p_clasico, wald, gl, p_vivienda),
  res_comp %>% select(test, estadistico, p_clasico, wald, gl, p_vivienda)) %>%
  mutate(razon = if_else(str_detect(test, "Mann"), NA_real_, wald / estadistico), across(c(estadistico, wald), ~ round(.x, 2)), razon = round(razon, 3),
         p_clasico = signif(p_clasico, 3), p_vivienda = signif(p_vivienda, 3))
print(tabla, width = 200)
write_csv(tabla, file.path(outtab, "10_p_valores_por_vivienda.csv"))
cat("Listo: p-valores por vivienda.\n")
