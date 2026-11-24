sector_jefe <- function(cod) {
  r <- division_caes(cod)
  case_when(is.na(r) ~ "Otras ramas",
            r == 48 ~ "Comercio",
            r %in% c(97, 56, 96) ~ "Serv. personales, doméstico y gastronomía",
            r %in% 84:88 ~ "Sector público, enseñanza y salud",
            (r >= 10 & r <= 33) | r == 40 | (r >= 49 & r <= 53) ~ "Industria, construcción y transporte",
            TRUE ~ "Otras ramas")
}
