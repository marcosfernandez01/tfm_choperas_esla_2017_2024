# ============================================================
# Estadísticos de NDVI y VH en muestras de choperas
# Año 2017
#
# Entrada: CSV exportado desde Google Earth Engine.
# Salidas: tablas estadísticas y gráficos exploratorios.
# ============================================================

library(tidyverse)


# ============================================================
# 1. RUTAS DE ENTRADA Y SALIDA
# ============================================================

archivo_entrada <- file.path(
  "data",
  "processed",
  "valores_pixel_ndvi_vh_choperas_2017.csv"
)

carpeta_salida <- file.path(
  "outputs",
  "estadisticos_ndvi_vh_2017"
)

dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 2. LECTURA DEL CSV
# ============================================================

primera_linea <- readLines(archivo_entrada, n = 1, warn = FALSE)

delimitador <- ifelse(
  str_count(primera_linea, ";") > str_count(primera_linea, ","),
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
  "fuente",
  "clase_ref",
  "clase_txt",
  "IDn_export",
  "IDn2_export",
  "NDVI_JUN2017",
  "NDVI_SEP2017",
  "VH_JUNSEP2017"
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
    fuente = as.factor(fuente),
    clase_ref = as.integer(clase_ref),
    clase_txt = as.factor(clase_txt),
    IDn_export = as.integer(IDn_export),
    IDn2_export = as.integer(IDn2_export),
    NDVI_JUN2017 = as.numeric(NDVI_JUN2017),
    NDVI_SEP2017 = as.numeric(NDVI_SEP2017),
    VH_JUNSEP2017 = as.numeric(VH_JUNSEP2017)
  )

write_csv2(
  datos_limpios,
  file.path(carpeta_salida, "00_valores_pixel_ndvi_vh_2017_limpios.csv")
)

datos_largos <- datos_limpios %>%
  pivot_longer(
    cols = c(NDVI_JUN2017, NDVI_SEP2017, VH_JUNSEP2017),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  mutate(
    tipo_variable = case_when(
      str_detect(variable, "NDVI") ~ "NDVI",
      str_detect(variable, "VH") ~ "VH",
      TRUE ~ "otra"
    )
  )

write_csv2(
  datos_largos,
  file.path(carpeta_salida, "01_valores_pixel_ndvi_vh_2017_formato_largo.csv")
)


# ============================================================
# 5. FUNCIÓN DE CÁLCULO DE ESTADÍSTICOS
# ============================================================

calcular_estadisticos <- function(df, grupos) {
  
  df %>%
    group_by(across(all_of(grupos))) %>%
    summarise(
      n_total = n(),
      n_validos = sum(!is.na(valor)),
      n_na = sum(is.na(valor)),
      
      media = mean(valor, na.rm = TRUE),
      mediana = median(valor, na.rm = TRUE),
      desviacion_tipica = sd(valor, na.rm = TRUE),
      varianza = var(valor, na.rm = TRUE),
      mad = mad(valor, na.rm = TRUE),
      
      minimo = min(valor, na.rm = TRUE),
      p01 = quantile(valor, 0.01, na.rm = TRUE, names = FALSE),
      p02_5 = quantile(valor, 0.025, na.rm = TRUE, names = FALSE),
      p05 = quantile(valor, 0.05, na.rm = TRUE, names = FALSE),
      p10 = quantile(valor, 0.10, na.rm = TRUE, names = FALSE),
      p15 = quantile(valor, 0.15, na.rm = TRUE, names = FALSE),
      p20 = quantile(valor, 0.20, na.rm = TRUE, names = FALSE),
      p25 = quantile(valor, 0.25, na.rm = TRUE, names = FALSE),
      p50 = quantile(valor, 0.50, na.rm = TRUE, names = FALSE),
      p75 = quantile(valor, 0.75, na.rm = TRUE, names = FALSE),
      p80 = quantile(valor, 0.80, na.rm = TRUE, names = FALSE),
      p85 = quantile(valor, 0.85, na.rm = TRUE, names = FALSE),
      p90 = quantile(valor, 0.90, na.rm = TRUE, names = FALSE),
      p95 = quantile(valor, 0.95, na.rm = TRUE, names = FALSE),
      p97_5 = quantile(valor, 0.975, na.rm = TRUE, names = FALSE),
      p99 = quantile(valor, 0.99, na.rm = TRUE, names = FALSE),
      maximo = max(valor, na.rm = TRUE),
      
      rango = maximo - minimo,
      iqr = IQR(valor, na.rm = TRUE),
      
      media_menos_1sd = media - desviacion_tipica,
      media_menos_2sd = media - 2 * desviacion_tipica,
      mediana_menos_mad = mediana - mad,
      
      .groups = "drop"
    ) %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 6)
      )
    )
}


# ============================================================
# 6. CÁLCULO DE ESTADÍSTICOS
# ============================================================

estad_global <- calcular_estadisticos(
  datos_largos,
  grupos = c("variable", "tipo_variable")
)

estad_por_clase <- calcular_estadisticos(
  datos_largos,
  grupos = c("variable", "tipo_variable", "clase_ref", "clase_txt")
)

estad_por_fuente <- calcular_estadisticos(
  datos_largos,
  grupos = c("variable", "tipo_variable", "fuente")
)

estad_por_fuente_clase <- calcular_estadisticos(
  datos_largos,
  grupos = c("variable", "tipo_variable", "fuente", "clase_ref", "clase_txt")
)


# ============================================================
# 7. EXPORTACIÓN DE ESTADÍSTICOS
# ============================================================

write_csv2(
  estad_global,
  file.path(carpeta_salida, "02_estadisticos_global_todas_las_muestras.csv")
)

write_csv2(
  estad_por_clase,
  file.path(carpeta_salida, "03_estadisticos_por_clase_adulta_joven.csv")
)

write_csv2(
  estad_por_fuente,
  file.path(carpeta_salida, "04_estadisticos_por_fuente_entrenamiento_validacion.csv")
)

write_csv2(
  estad_por_fuente_clase,
  file.path(carpeta_salida, "05_estadisticos_por_fuente_y_clase.csv")
)


# ============================================================
# 8. RESUMEN COMPACTO
# ============================================================

resumen_compacto <- estad_por_fuente_clase %>%
  select(
    fuente,
    clase_ref,
    clase_txt,
    variable,
    n_validos,
    media,
    mediana,
    desviacion_tipica,
    minimo,
    p05,
    p10,
    p15,
    p20,
    p25,
    p50,
    p75,
    p90,
    p95,
    maximo
  ) %>%
  arrange(variable, fuente, clase_ref)

write_csv2(
  resumen_compacto,
  file.path(carpeta_salida, "06_resumen_compacto.csv")
)


# ============================================================
# 9. IDENTIFICACIÓN DE VALORES ATÍPICOS
# ============================================================

limites_outliers <- datos_largos %>%
  group_by(variable, clase_ref, clase_txt) %>%
  summarise(
    q1 = quantile(valor, 0.25, na.rm = TRUE, names = FALSE),
    q3 = quantile(valor, 0.75, na.rm = TRUE, names = FALSE),
    iqr = IQR(valor, na.rm = TRUE),
    limite_inferior = q1 - 1.5 * iqr,
    limite_superior = q3 + 1.5 * iqr,
    .groups = "drop"
  )

valores_atipicos <- datos_largos %>%
  left_join(
    limites_outliers,
    by = c("variable", "clase_ref", "clase_txt")
  ) %>%
  filter(
    valor < limite_inferior | valor > limite_superior
  ) %>%
  arrange(variable, clase_ref, valor)

write_csv2(
  valores_atipicos,
  file.path(carpeta_salida, "07_valores_atipicos_posibles.csv")
)


# ============================================================
# 10. GRÁFICOS EXPLORATORIOS
# ============================================================

graf_boxplot <- ggplot(
  datos_largos,
  aes(x = clase_txt, y = valor, fill = fuente)
) +
  geom_boxplot(outlier.alpha = 0.35) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    title = "Distribución de valores por clase y fuente",
    x = "Clase de referencia",
    y = "Valor"
  ) +
  theme_bw()

ggsave(
  filename = file.path(carpeta_salida, "08_boxplot_variable_clase_fuente.png"),
  plot = graf_boxplot,
  width = 11,
  height = 7,
  dpi = 300
)


graf_hist <- ggplot(
  datos_largos,
  aes(x = valor, fill = clase_txt)
) +
  geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(
    title = "Histogramas de NDVI y VH por clase",
    x = "Valor",
    y = "Frecuencia"
  ) +
  theme_bw()

ggsave(
  filename = file.path(carpeta_salida, "09_histogramas_variable_clase.png"),
  plot = graf_hist,
  width = 11,
  height = 7,
  dpi = 300
)


graf_dens <- ggplot(
  datos_largos,
  aes(x = valor, colour = clase_txt)
) +
  geom_density(linewidth = 1) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(
    title = "Densidad de valores de NDVI y VH por clase",
    x = "Valor",
    y = "Densidad"
  ) +
  theme_bw()

ggsave(
  filename = file.path(carpeta_salida, "10_densidad_variable_clase.png"),
  plot = graf_dens,
  width = 11,
  height = 7,
  dpi = 300
)


# ============================================================
# 11. MENSAJE FINAL
# ============================================================

message("Proceso finalizado correctamente.")
message("Resultados exportados en: ", carpeta_salida)