# =========================================================
# Análisis mensual de VH Sentinel-1 por parcela
# Ejemplo correspondiente al año 2024
#
# El procedimiento se aplicó de forma independiente para
# cada año analizado, ajustando los archivos de entrada.
# =========================================================

rm(list = ls())

library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)


# =========================================================
# 1. PARÁMETROS Y RUTAS
# =========================================================

anio <- 2024

dir_s1 <- file.path("data", "raw", paste0("sentinel1_vh_", anio))

parcelas_path <- file.path(
  "data", "raw",
  paste0("parcelas_cortadas_", anio),
  paste0("parcelas_cortadas_", anio, ".shp")
)

dir_tablas <- file.path("outputs", "tablas")
dir_figuras <- file.path("outputs", "boxplots_vh", as.character(anio))

dir.create(dir_tablas, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_figuras, recursive = TRUE, showWarnings = FALSE)


# =========================================================
# 2. CARGA Y MOSAICO DE SENTINEL-1
# =========================================================

r1 <- rast(file.path(dir_s1, paste0("S1_VH_", anio, "-0000000000-0000000000.tif")))
r2 <- rast(file.path(dir_s1, paste0("S1_VH_", anio, "-0000000000-0000006912.tif")))
r3 <- rast(file.path(dir_s1, paste0("S1_VH_", anio, "-0000006912-0000000000.tif")))
r4 <- rast(file.path(dir_s1, paste0("S1_VH_", anio, "-0000006912-0000006912.tif")))

S1_2024 <- mosaic(r1, r2, r3, r4)

names(S1_2024) <- c(
  "ENE", "FEB", "MAR", "ABR",
  "MAY", "JUN", "JUL", "AGO",
  "SEP", "OCT", "NOV", "DIC"
)


# =========================================================
# 3. CARGA DE PARCELAS
# =========================================================

parcelas_2024 <- vect(parcelas_path)


# =========================================================
# 4. EXTRACCIÓN DE VALORES VH
# =========================================================

extract_2024 <- terra::extract(
  S1_2024,
  parcelas_2024
)


# =========================================================
# 5. INCORPORACIÓN DE ATRIBUTOS PARCELARIOS
# =========================================================

parcelas_df <- as.data.frame(parcelas_2024)
parcelas_df$ID <- seq_len(nrow(parcelas_df))

parcelas_join <- parcelas_df[, c("ID", "PARCELA", "CORTA_OK")]

extract_2024 <- left_join(
  extract_2024,
  parcelas_join,
  by = "ID"
)


# =========================================================
# 6. FORMATO LARGO Y DEPURACIÓN DE LA TABLA
# =========================================================

tabla_long <- extract_2024 %>%
  pivot_longer(
    cols = c(
      ENE, FEB, MAR, ABR,
      MAY, JUN, JUL, AGO,
      SEP, OCT, NOV, DIC
    ),
    names_to = "MES",
    values_to = "VH"
  ) %>%
  filter(!is.na(VH))

tabla_long$MES <- factor(
  tabla_long$MES,
  levels = c(
    "ENE", "FEB", "MAR", "ABR",
    "MAY", "JUN", "JUL", "AGO",
    "SEP", "OCT", "NOV", "DIC"
  )
)

tabla_long <- tabla_long %>%
  mutate(
    LABEL = paste0(
      "Parcela ",
      PARCELA,
      " | Corta: ",
      CORTA_OK
    )
  )


# =========================================================
# 7. RESUMEN ESTADÍSTICO MENSUAL
# =========================================================

tabla_resumen <- tabla_long %>%
  group_by(PARCELA, CORTA_OK, MES) %>%
  summarise(
    MIN = min(VH, na.rm = TRUE),
    Q1 = quantile(VH, 0.25, na.rm = TRUE),
    MEDIANA = median(VH, na.rm = TRUE),
    MEDIA = mean(VH, na.rm = TRUE),
    Q3 = quantile(VH, 0.75, na.rm = TRUE),
    MAX = max(VH, na.rm = TRUE),
    SD = sd(VH, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  )

write.csv(
  tabla_resumen,
  file.path(dir_tablas, paste0("resumen_estadistico_vh_", anio, ".csv")),
  row.names = FALSE
)


# =========================================================
# 8. BOXPLOTS MENSUALES POR PARCELA
# =========================================================

parcelas_unicas <- unique(tabla_long$LABEL)

for (i in parcelas_unicas) {
  
  datos_parcela <- tabla_long %>%
    filter(LABEL == i)
  
  p <- ggplot(
    datos_parcela,
    aes(
      x = MES,
      y = VH
    )
  ) +
    geom_boxplot() +
    xlab(paste0("Meses ", anio)) +
    ylab("VH Sentinel-1") +
    ggtitle(i) +
    theme_bw()
  
  nombre_archivo <- paste0(
    "boxplot_vh_",
    unique(datos_parcela$PARCELA),
    "_",
    unique(datos_parcela$CORTA_OK),
    ".png"
  )
  
  nombre_archivo <- gsub("[^A-Za-z0-9_.-]", "_", nombre_archivo)
  
  ggsave(
    filename = file.path(dir_figuras, nombre_archivo),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}