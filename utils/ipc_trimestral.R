ipc_trimestral <- function(ruta_xls, trimestres, base_anio = 2026, base_trim = 1) {
  hojas <- readxl::excel_sheets(ruta_xls)
  hoja  <- hojas[grep("ndices IPC Cobertura", hojas)[1]]
  crudo <- suppressMessages(readxl::read_excel(ruta_xls, sheet = hoja, col_names = FALSE))
  stopifnot(grepl("Nivel general", as.character(crudo[10, 1])))
  ipc <- tibble(fecha = suppressWarnings(as.Date(as.numeric(unlist(crudo[6, -1])), origin = "1899-12-30")),
                valor = suppressWarnings(as.numeric(unlist(crudo[10, -1])))) %>%
    filter(!is.na(valor), !is.na(fecha)) %>%
    mutate(anio = year(fecha), mes = month(fecha))
  ipc_mes <- function(a, m) {
    if (m < 1) { a <- a - 1; m <- m + 12 }
    ipc$valor[ipc$anio == a & ipc$mes == m]
  }
  out <- trimestres %>%
    mutate(ipc = map2_dbl(ANO4, TRIMESTRE, ~ mean(map_dbl((3 * (.y - 1)):(3 * (.y - 1) + 2), function(m) ipc_mes(.x, m)))))
  ipc_base <- out$ipc[out$ANO4 == base_anio & out$TRIMESTRE == base_trim]
  out %>% mutate(factor = ipc_base / ipc)
}
