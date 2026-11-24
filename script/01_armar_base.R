library(tidyverse)
library(readxl)

instub  <- "raw/eph"
auxstub <- "auxiliar"
outstub <- "input"
dir.create(outstub, showWarnings = FALSE, recursive = TRUE)
for (f in list.files("utils", full.names = TRUE)) source(f)

vars_ind <- c("CODUSU", "ANO4", "TRIMESTRE", "NRO_HOGAR", "COMPONENTE", "CH03", "CH04", "CH06",
              "ESTADO", "CAT_OCUP", "PP04A", "PP04B_COD", "PP07H", "V5_01_M", "PONDERA", "PONDII")
vars_hog <- c("CODUSU", "ANO4", "TRIMESTRE", "NRO_HOGAR", "REGION", "V5_01", "IPCF", "ITF", "IX_TOT",
              "PONDERA", "PONDIH")

ind <- leer_eph(instub, "^usu_individual_T[0-9]+\\.txt$", vars_ind)
hog <- leer_eph(instub, "^usu_hogar_T[0-9]+\\.txt$", vars_hog)

ind <- ind %>%
  mutate(ANO4 = as.integer(ANO4), TRIMESTRE = as.integer(TRIMESTRE),
         Q = paste0(TRIMESTRE, "T", ANO4 %% 100),
         HOG = paste(CODUSU, Q, NRO_HOGAR, sep = "|"))
hog <- hog %>%
  mutate(ANO4 = as.integer(ANO4), TRIMESTRE = as.integer(TRIMESTRE),
         Q = paste0(TRIMESTRE, "T", ANO4 %% 100),
         HOG = paste(CODUSU, Q, NRO_HOGAR, sep = "|"))

trimestres <- hog %>% distinct(Q, ANO4, TRIMESTRE) %>% arrange(ANO4, TRIMESTRE)
NQ <- nrow(trimestres)
cat("Trimestres leídos (", NQ, "): ", paste(trimestres$Q, collapse = ", "), "\n", sep = "")
cat("Personas:", nrow(ind), "| hogares:", nrow(hog), "| viviendas:", n_distinct(hog$CODUSU), "\n")

factores <- ipc_trimestral(file.path(auxstub, "sh_ipc_divisiones.xls"), trimestres %>% select(ANO4, TRIMESTRE)) %>%
  mutate(Q = paste0(TRIMESTRE, "T", ANO4 %% 100))
write_csv(factores, file.path(auxstub, "factores_ipc.csv"))
print(factores)

ind <- ind %>%
  mutate(CH06 = num(CH06), V5_01_M = num(V5_01_M),
         PONDERA = num(PONDERA) / NQ, PONDII = num(PONDII) / NQ)
hog <- hog %>%
  left_join(factores %>% select(Q, factor), by = "Q") %>%
  mutate(PONDERA = num(PONDERA) / NQ, PONDIH = num(PONDIH) / NQ,
         IPCF_n = num(IPCF) * factor, ITF_n = num(ITF) * factor,
         IX_TOT = num(IX_TOT), REGION = as.integer(REGION))

con_menores <- ind %>%
  group_by(HOG) %>%
  summarise(menor = any(CH06 < 18)) %>%
  filter(menor) %>%
  pull(HOG)

jefes <- ind %>%
  filter(CH03 == "1", HOG %in% con_menores) %>%
  distinct(HOG, .keep_all = TRUE) %>%
  transmute(HOG, ESTADO_J = ESTADO, CAT_OCUP_J = CAT_OCUP, PP04A_J = PP04A,
            PP04B_COD_J = PP04B_COD, SEXO_J = CH04, EDAD_J = CH06)

composicion <- ind %>%
  group_by(HOG) %>%
  summarise(n_menores = sum(CH06 < 18, na.rm = TRUE),
            n_adultos = sum(CH06 >= 18 & CH06 <= 64, na.rm = TRUE),
            n_adultos_ocup = sum(CH06 >= 18 & CH06 <= 64 & ESTADO == "1", na.rm = TRUE))

monto_auh <- ind %>%
  group_by(HOG) %>%
  summarise(AUH_monto = sum(V5_01_M, na.rm = TRUE))

hogares <- hog %>%
  filter(HOG %in% con_menores) %>%
  left_join(jefes, by = "HOG") %>%
  left_join(monto_auh, by = "HOG") %>%
  left_join(composicion, by = "HOG") %>%
  mutate(AUH = case_when(V5_01 == "1" ~ 1L, V5_01 == "2" ~ 0L, TRUE ~ NA_integer_),
         AUH_monto = replace_na(AUH_monto, 0) * factor,
         IPCF_sin_auh = pmax(ITF_n - AUH_monto, 0) / IX_TOT,
         ingreso_valido = PONDIH > 0 & IPCF_n > 0,
         sector_jefe = if_else(ESTADO_J == "1", sector_jefe(PP04B_COD_J), "Jefe/a no ocupado/a")) %>%
  select(HOG, VIV = CODUSU, Q, ANO4, TRIMESTRE, REGION, AUH, V5_01, AUH_monto, IPCF_n, ITF_n, IPCF_sin_auh,
         IX_TOT, PONDERA, PONDIH, ingreso_valido, ESTADO_J, CAT_OCUP_J, PP04A_J, sector_jefe, SEXO_J, EDAD_J,
         n_menores, n_adultos, n_adultos_ocup)

ocupados <- ind %>%
  filter(HOG %in% con_menores, ESTADO == "1", CH06 >= 18, CH06 <= 64) %>%
  left_join(hogares %>% select(HOG, AUH), by = "HOG") %>%
  mutate(division = division_caes(PP04B_COD), rama = rama_caes(PP04B_COD),
         mujer = CH04 == "2", asalariado = CAT_OCUP == "3",
         informal = asalariado & PP07H == "2",
         perc = V5_01_M > 0) %>%
  filter(!is.na(rama), !is.na(AUH)) %>%
  transmute(HOG, VIV = CODUSU, Q, ANO4, TRIMESTRE, CH04, CH06, mujer, CAT_OCUP, asalariado, PP04A,
            PP04B_COD, division, rama, informal, perc, AUH = AUH == 1, PONDERA)

perceptoras <- ind %>%
  filter(HOG %in% con_menores, CH06 >= 18, CH06 <= 64, V5_01_M > 0) %>%
  transmute(HOG, Q, CH04, CH06, ESTADO, PP04A, PONDERA)

dir.create(file.path("output", "tablas"), showWarnings = FALSE, recursive = TRUE)
dimensiones <- tibble(
  concepto = c("Trimestres", "Personas (base individual)", "Hogares-trimestre (base hogar)", "Viviendas distintas",
               "Hogares-trimestre con menores de 18", "Viviendas con menores de 18",
               "Ocupados 18-64 en hogares con menores", "Personas de 18-64 que cobran AUH"),
  valor = c(NQ, nrow(ind), nrow(hog), n_distinct(hog$CODUSU), length(con_menores), n_distinct(hogares$VIV),
            nrow(ocupados), nrow(perceptoras)))
write_csv(dimensiones, file.path("output", "tablas", "01_dimensiones.csv"))

ramas <- ocupados %>%
  distinct(division) %>% arrange(division) %>%
  mutate(rama = rama_caes(division), sector_jefe = sector_jefe(division))
write_csv(ramas, file.path(auxstub, "ramas_caes.csv"))

write_csv(hogares, file.path(outstub, "hogares_menores.csv"))
write_csv(ocupados, file.path(outstub, "ocupados_menores.csv"))
write_csv(perceptoras, file.path(outstub, "perceptoras.csv"))

cat(sprintf("\nHogares con menores: %d | con ingreso válido: %d (perceptores %d)\n",
            nrow(hogares), sum(hogares$ingreso_valido), sum(hogares$ingreso_valido & hogares$AUH == 1, na.rm = TRUE)))
cat(sprintf("Ocupados 18-64 en hogares con menores: %d (en hogares con AUH: %d, perceptoras: %d)\n",
            nrow(ocupados), sum(ocupados$AUH), sum(ocupados$perc, na.rm = TRUE)))
