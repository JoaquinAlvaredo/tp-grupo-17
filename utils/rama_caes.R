rama_caes <- function(cod) {
  r <- division_caes(cod)
  case_when(is.na(r) ~ NA_character_,
            r <= 9  ~ "Agro, pesca y minería",
            r <= 33 ~ "Industria",
            r <= 39 ~ "Electricidad, agua y otras",
            r == 40 ~ "Construcción",
            r <= 48 ~ "Comercio",
            r <= 53 ~ "Transporte",
            r <= 56 ~ "Alojamiento y gastronomía",
            r <= 66 ~ "Información, comunicaciones y finanzas",
            r <= 75 ~ "Inmobiliarias y serv. profesionales",
            r <= 82 ~ "Serv. administrativos y de apoyo",
            r == 84 ~ "Administración pública",
            r == 85 ~ "Enseñanza",
            r <= 88 ~ "Salud y servicios sociales",
            r <= 96 ~ "Otros servicios personales",
            r == 97 ~ "Servicio doméstico",
            TRUE    ~ "Electricidad, agua y otras")
}
