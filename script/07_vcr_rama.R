library(tidyverse)
instub  <- "input"
outtab  <- "output/tablas"
dir.create(outtab, showWarnings = FALSE, recursive = TRUE)
for (f in list.files("utils", full.names = TRUE)) source(f)
B_BOOT <- 1000
set.seed(17)

o <- read_csv(file.path(instub, "ocupados_menores.csv"), show_col_types = FALSE, guess_max = 100000)

p_total <- weighted.mean(o$AUH, o$PONDERA)
informalidad_total <- sum(o$PONDERA[o$informal]) / sum(o$PONDERA[o$asalariado])
NQ <- n_distinct(o$Q)

vcr <- o %>%
  group_by(rama) %>%
  summarise(n = n(), ocupados = sum(PONDERA) / 1,
            p_auh = weighted.mean(AUH, PONDERA), VCR = p_auh / p_total,
            pct_mujer = weighted.mean(mujer, PONDERA),
            informalidad = sum(PONDERA[informal]) / sum(PONDERA[asalariado]), .groups = "drop")

viv <- factor(o$VIV); rama_f <- factor(o$rama, levels = vcr$rama)
dd <- data.frame(viv, rama_f, wa = o$PONDERA * o$AUH, wt = o$PONDERA)
Wa <- xtabs(wa ~ viv + rama_f, dd); Wt <- xtabs(wt ~ viv + rama_f, dd)
K <- nrow(Wa)
vcr_boot <- replicate(B_BOOT, {
  m <- tabulate(sample.int(K, K, replace = TRUE), nbins = K)
  a <- as.numeric(m %*% Wa); t <- as.numeric(m %*% Wt)
  (a / t) / (sum(a) / sum(t))
})
vcr$lo <- apply(vcr_boot, 1, quantile, 0.025)
vcr$hi <- apply(vcr_boot, 1, quantile, 0.975)
vcr <- arrange(vcr, desc(VCR))
print(vcr %>% mutate(across(c(ocupados), round), across(c(p_auh, VCR, pct_mujer, informalidad, lo, hi), ~ round(.x, 3))), n = 20, width = 200)

rho <- cor(vcr$VCR, vcr$informalidad, method = "spearman")
r_pearson <- cor(log(vcr$VCR), vcr$informalidad)
resumen <- tibble(indicador = c("% de ocupados en hogares con AUH (total)", "Informalidad del total (% de asalariados sin descuento)",
                                "Spearman (VCR, informalidad), 15 ramas", "Pearson (ln VCR, informalidad), 15 ramas",
                                "Ramas con VCR > 1", "Ramas con VCR < 1", "Ramas con IC95% que incluye 1"),
                  valor = c(100 * p_total, 100 * informalidad_total, rho, r_pearson, sum(vcr$VCR > 1), sum(vcr$VCR < 1),
                            sum(vcr$lo <= 1 & vcr$hi >= 1))) %>%
  mutate(valor = round(valor, 3))
print(resumen)
write_csv(vcr, file.path(outtab, "07_vcr_por_rama.csv"))
write_csv(resumen, file.path(outtab, "07_vcr_resumen.csv"))
cat("Listo: VCR por rama.\n")
