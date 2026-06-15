# ============================================================
# Resumen parcelario de variables NDVI y VH para replantación
# Año 2024
#
# Entrada: CSV de valores de píxel exportado desde Google Earth Engine.
# Salida: tabla con una fila por parcela y estadísticos por variable.
# ============================================================

library(tidyverse)


# ============================================================
# 1. RUTAS DE ENTRADA Y SALIDA
# ============================================================

archivo_entrada <- file.path(
  "data",
  "processed",
  "valores_pixel_replantacion_cortadas_2017_2023.csv"
)

carpeta_salida <- file.path(
  "outputs",
  "replantacion_2024",
  "resumen_parcela"
)

dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(archivo_entrada)) {
  stop("No existe el archivo de entrada: ", archivo_entrada)
}


# ============================================================
# 2. LECTURA DEL CSV
# ============================================================

primera_linea <- readLines(archivo_entrada, n = 1, warn = FALSE)

delimitador <- ifelse(
  stringr::str_count(primera_linea, ";") > stringr::str_count(primera_linea, ","),
  ";",
  ","
)

datos <- read_delim(
  file = archivo_entrada,
  delim = delimitador,
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE,
  trim_ws = TRUE
)


# ============================================================
# 3. COMPROBACIÓN DE CAMPOS NECESARIOS
# ============================================================

campos_necesarios <- c(
  "ID",
  "REFCAT",
  "F_CORTE",
  "A_CORTE",
  "M_CORTE",
  "NDVI_JUN2024",
  "NDVI_SEP2024",
  "VH_JUNSEP2024"
)

campos_faltantes <- setdiff(campos_necesarios, names(datos))

if (length(campos_faltantes) > 0) {
  stop(
    paste(
      "Faltan campos necesarios en el CSV:",
      paste(campos_faltantes, collapse = ", ")
    )
  )
}


# ============================================================
# 4. PREPARACIÓN DE DATOS
# ============================================================

datos_limpios <- datos %>%
  mutate(
    ID = as.integer(ID),
    REFCAT = as.character(REFCAT),
    F_CORTE = as.character(F_CORTE),
    A_CORTE = as.integer(A_CORTE),
    M_CORTE = as.integer(M_CORTE),
    NDVI_JUN2024 = as.numeric(NDVI_JUN2024),
    NDVI_SEP2024 = as.numeric(NDVI_SEP2024),
    VH_JUNSEP2024 = as.numeric(VH_JUNSEP2024),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    ID_PARCELA = as.character(ID)
  )

write_csv2(
  datos_limpios,
  file.path(carpeta_salida, "00_valores_pixel_replantacion_2024_limpios.csv")
)


# ============================================================
# 5. ATRIBUTOS PARCELARIOS
# ============================================================

campos_parcela_posibles <- c(
  "ID_PARCELA",
  "grupo_analisis",
  "FID",
  "ID",
  "MUNICIPIO",
  "MASA",
  "PARCELA",
  "AREA",
  "REFCAT",
  "nom_muni",
  "Shape_Area",
  "parcelas_a",
  "SUP_TOTAL",
  "AREA_C1",
  "AREA_C2",
  "AREA_C3",
  "AREA_C4",
  "AREA_TOTAL",
  "PCT_1",
  "PCT_2",
  "PCT_3",
  "PCT_4",
  "PUREZA",
  "RANGO_PUR",
  "CLASE_MAYO",
  "CLASE_FIN",
  "CLASE_45",
  "EST_INT",
  "CL_INI",
  "CORTA_OK",
  "SIN_CORT",
  "EXC_MJ",
  "SER_NOV17",
  "DET_JOV",
  "F_CORTE",
  "A_CORTE",
  "M_CORTE",
  "VH_CORTE",
  "F_DET",
  "A_DET",
  "M_DET",
  "VH_DET",
  "VH_NOV17",
  "VH_MIN",
  "N_BAJO",
  "MOTIVO"
)

campos_parcela <- intersect(campos_parcela_posibles, names(datos_limpios))

info_parcela <- datos_limpios %>%
  group_by(ID_PARCELA) %>%
  summarise(
    across(
      all_of(setdiff(campos_parcela, "ID_PARCELA")),
      ~ first(.x)
    ),
    .groups = "drop"
  )


# ============================================================
# 6. FUNCIONES AUXILIARES
# ============================================================

media_segura <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

mediana_segura <- function(x) {
  if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
}

sd_segura <- function(x) {
  if (sum(!is.na(x)) <= 1) NA_real_ else sd(x, na.rm = TRUE)
}

min_seguro <- function(x) {
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}

max_seguro <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

q_seguro <- function(x, p) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    quantile(x, p, na.rm = TRUE, names = FALSE)
  }
}

iqr_seguro <- function(x) {
  if (all(is.na(x))) NA_real_ else IQR(x, na.rm = TRUE)
}


# ============================================================
# 7. RESUMEN POR PARCELA
# ============================================================

res_ndvi_jun <- datos_limpios %>%
  group_by(ID_PARCELA) %>%
  summarise(
    n_pix_total = n(),
    NDVI_JUN2024_n_validos = sum(!is.na(NDVI_JUN2024)),
    NDVI_JUN2024_n_na = sum(is.na(NDVI_JUN2024)),
    NDVI_JUN2024_media = media_segura(NDVI_JUN2024),
    NDVI_JUN2024_mediana = mediana_segura(NDVI_JUN2024),
    NDVI_JUN2024_sd = sd_segura(NDVI_JUN2024),
    NDVI_JUN2024_min = min_seguro(NDVI_JUN2024),
    NDVI_JUN2024_p05 = q_seguro(NDVI_JUN2024, 0.05),
    NDVI_JUN2024_p10 = q_seguro(NDVI_JUN2024, 0.10),
    NDVI_JUN2024_p25 = q_seguro(NDVI_JUN2024, 0.25),
    NDVI_JUN2024_p75 = q_seguro(NDVI_JUN2024, 0.75),
    NDVI_JUN2024_p90 = q_seguro(NDVI_JUN2024, 0.90),
    NDVI_JUN2024_p95 = q_seguro(NDVI_JUN2024, 0.95),
    NDVI_JUN2024_max = max_seguro(NDVI_JUN2024),
    NDVI_JUN2024_iqr = iqr_seguro(NDVI_JUN2024),
    .groups = "drop"
  )

res_ndvi_sep <- datos_limpios %>%
  group_by(ID_PARCELA) %>%
  summarise(
    NDVI_SEP2024_n_validos = sum(!is.na(NDVI_SEP2024)),
    NDVI_SEP2024_n_na = sum(is.na(NDVI_SEP2024)),
    NDVI_SEP2024_media = media_segura(NDVI_SEP2024),
    NDVI_SEP2024_mediana = mediana_segura(NDVI_SEP2024),
    NDVI_SEP2024_sd = sd_segura(NDVI_SEP2024),
    NDVI_SEP2024_min = min_seguro(NDVI_SEP2024),
    NDVI_SEP2024_p05 = q_seguro(NDVI_SEP2024, 0.05),
    NDVI_SEP2024_p10 = q_seguro(NDVI_SEP2024, 0.10),
    NDVI_SEP2024_p25 = q_seguro(NDVI_SEP2024, 0.25),
    NDVI_SEP2024_p75 = q_seguro(NDVI_SEP2024, 0.75),
    NDVI_SEP2024_p90 = q_seguro(NDVI_SEP2024, 0.90),
    NDVI_SEP2024_p95 = q_seguro(NDVI_SEP2024, 0.95),
    NDVI_SEP2024_max = max_seguro(NDVI_SEP2024),
    NDVI_SEP2024_iqr = iqr_seguro(NDVI_SEP2024),
    .groups = "drop"
  )

res_vh <- datos_limpios %>%
  group_by(ID_PARCELA) %>%
  summarise(
    VH_JUNSEP2024_n_validos = sum(!is.na(VH_JUNSEP2024)),
    VH_JUNSEP2024_n_na = sum(is.na(VH_JUNSEP2024)),
    VH_JUNSEP2024_media = media_segura(VH_JUNSEP2024),
    VH_JUNSEP2024_mediana = mediana_segura(VH_JUNSEP2024),
    VH_JUNSEP2024_sd = sd_segura(VH_JUNSEP2024),
    VH_JUNSEP2024_min = min_seguro(VH_JUNSEP2024),
    VH_JUNSEP2024_p05 = q_seguro(VH_JUNSEP2024, 0.05),
    VH_JUNSEP2024_p10 = q_seguro(VH_JUNSEP2024, 0.10),
    VH_JUNSEP2024_p25 = q_seguro(VH_JUNSEP2024, 0.25),
    VH_JUNSEP2024_p75 = q_seguro(VH_JUNSEP2024, 0.75),
    VH_JUNSEP2024_p90 = q_seguro(VH_JUNSEP2024, 0.90),
    VH_JUNSEP2024_p95 = q_seguro(VH_JUNSEP2024, 0.95),
    VH_JUNSEP2024_max = max_seguro(VH_JUNSEP2024),
    VH_JUNSEP2024_iqr = iqr_seguro(VH_JUNSEP2024),
    .groups = "drop"
  )


# ============================================================
# 8. TABLA FINAL POR PARCELA
# ============================================================

resumen_parcela <- info_parcela %>%
  left_join(res_ndvi_jun, by = "ID_PARCELA") %>%
  left_join(res_ndvi_sep, by = "ID_PARCELA") %>%
  left_join(res_vh, by = "ID_PARCELA") %>%
  mutate(
    NDVI_JUN2024_VALOR = NDVI_JUN2024_mediana,
    NDVI_SEP2024_VALOR = NDVI_SEP2024_mediana,
    VH_JUNSEP2024_VALOR = VH_JUNSEP2024_mediana,
    
    pixeles_validos_min = pmin(
      NDVI_JUN2024_n_validos,
      NDVI_SEP2024_n_validos,
      VH_JUNSEP2024_n_validos,
      na.rm = TRUE
    ),
    
    grupo_corta_interpretacion = case_when(
      A_CORTE <= 2022 ~ "corta_2017_2022_evaluable",
      A_CORTE == 2023 ~ "corta_2023_interpretacion_cautelosa",
      TRUE ~ "otro"
    )
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 6)
    )
  ) %>%
  arrange(A_CORTE, M_CORTE, ID)


# ============================================================
# 9. EXPORTACIÓN DE TABLAS
# ============================================================

write_csv2(
  resumen_parcela,
  file.path(carpeta_salida, "01_resumen_parcela_replantacion_2024.csv")
)

campos_compacto <- c(
  "ID_PARCELA",
  "ID",
  "REFCAT",
  "MUNICIPIO",
  "MASA",
  "PARCELA",
  "nom_muni",
  "SUP_TOTAL",
  "EST_INT",
  "CL_INI",
  "F_CORTE",
  "A_CORTE",
  "M_CORTE",
  "VH_CORTE",
  "F_DET",
  "A_DET",
  "M_DET",
  "VH_DET",
  "VH_NOV17",
  "VH_MIN",
  "MOTIVO",
  "n_pix_total",
  "NDVI_JUN2024_VALOR",
  "NDVI_SEP2024_VALOR",
  "VH_JUNSEP2024_VALOR",
  "NDVI_JUN2024_p25",
  "NDVI_SEP2024_p25",
  "VH_JUNSEP2024_p25",
  "NDVI_JUN2024_iqr",
  "NDVI_SEP2024_iqr",
  "VH_JUNSEP2024_iqr",
  "pixeles_validos_min",
  "grupo_corta_interpretacion"
)

resumen_parcela_compacto <- resumen_parcela %>%
  select(any_of(campos_compacto))

write_csv2(
  resumen_parcela_compacto,
  file.path(carpeta_salida, "02_resumen_parcela_replantacion_2024_compacto.csv")
)


# ============================================================
# 10. CONTROLES GENERALES
# ============================================================

control_anio_corta <- resumen_parcela %>%
  count(A_CORTE, grupo_corta_interpretacion, name = "n_parcelas") %>%
  arrange(A_CORTE)

write_csv2(
  control_anio_corta,
  file.path(carpeta_salida, "03_control_numero_parcelas_por_anio_corta.csv")
)

control_pixeles <- resumen_parcela %>%
  summarise(
    n_parcelas = n(),
    n_pix_total_min = min(n_pix_total, na.rm = TRUE),
    n_pix_total_mediana = median(n_pix_total, na.rm = TRUE),
    n_pix_total_media = mean(n_pix_total, na.rm = TRUE),
    n_pix_total_max = max(n_pix_total, na.rm = TRUE),
    n_pix_validos_min = min(pixeles_validos_min, na.rm = TRUE),
    n_pix_validos_mediana = median(pixeles_validos_min, na.rm = TRUE),
    n_pix_validos_media = mean(pixeles_validos_min, na.rm = TRUE),
    n_pix_validos_max = max(pixeles_validos_min, na.rm = TRUE)
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 6)
    )
  )

write_csv2(
  control_pixeles,
  file.path(carpeta_salida, "04_control_pixeles_por_parcela.csv")
)


# ============================================================
# 11. GRÁFICOS DE CONTROL
# ============================================================

graf_ndvi_jun <- ggplot(
  resumen_parcela,
  aes(x = NDVI_JUN2024_VALOR)
) +
  geom_histogram(bins = 50) +
  facet_wrap(~ grupo_corta_interpretacion, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Distribución parcelaria de NDVI junio 2024",
    x = "Mediana NDVI junio 2024 por parcela",
    y = "Número de parcelas"
  )

ggsave(
  file.path(carpeta_salida, "05_histograma_ndvi_jun2024_parcela.png"),
  graf_ndvi_jun,
  width = 10,
  height = 6,
  dpi = 300
)

graf_ndvi_sep <- ggplot(
  resumen_parcela,
  aes(x = NDVI_SEP2024_VALOR)
) +
  geom_histogram(bins = 50) +
  facet_wrap(~ grupo_corta_interpretacion, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Distribución parcelaria de NDVI septiembre 2024",
    x = "Mediana NDVI septiembre 2024 por parcela",
    y = "Número de parcelas"
  )

ggsave(
  file.path(carpeta_salida, "06_histograma_ndvi_sep2024_parcela.png"),
  graf_ndvi_sep,
  width = 10,
  height = 6,
  dpi = 300
)

graf_vh <- ggplot(
  resumen_parcela,
  aes(x = VH_JUNSEP2024_VALOR)
) +
  geom_histogram(bins = 50) +
  facet_wrap(~ grupo_corta_interpretacion, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Distribución parcelaria de VH junio-septiembre 2024",
    x = "Mediana VH junio-septiembre 2024 por parcela",
    y = "Número de parcelas"
  )

ggsave(
  file.path(carpeta_salida, "07_histograma_vh_junsep2024_parcela.png"),
  graf_vh,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# 12. MENSAJE FINAL
# ============================================================

message("Proceso finalizado correctamente.")
message("Parcelas resumidas: ", nrow(resumen_parcela))
message("Resultados exportados en: ", carpeta_salida)