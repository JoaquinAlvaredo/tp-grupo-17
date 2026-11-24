leer_eph <- function(carpeta, patron, columnas) {
  archivos <- list.files(carpeta, pattern = patron, recursive = TRUE, full.names = TRUE)
  if (length(archivos) == 0) stop("No se encontraron archivos ", patron, " en ", carpeta)
  archivos %>%
    map_dfr(~ read_delim(.x, delim = ";", locale = locale(decimal_mark = ",", encoding = "latin1"),
                         col_types = cols(.default = "c"), col_select = any_of(columnas),
                         progress = FALSE))
}
