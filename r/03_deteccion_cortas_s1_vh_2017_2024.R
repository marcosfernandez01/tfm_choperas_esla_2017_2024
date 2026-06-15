# ============================================================
# Detección de eventos de corta en choperas mediante Sentinel-1 VH
#
# Cartografía base: parcelas clasificadas como chopo en 2017.
# Serie temporal: mediana mensual VH Sentinel-1 entre 2017 y 2024.
#
# Criterio general:
#   mediana mensual VH < -18,7 dB
#
# Periodo operativo:
#   noviembre de 2017 - diciembre de 2024
#
# Ajuste específico para 2017:
#   se evalúa si existen descensos claros en octubre o noviembre de 2017
#   antes de clasificar una parcela como iniciada bajo umbral.
# ============================================================

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(lubridate)
library(sf)


# ============================================================
# 1. PARÁMETROS GENERALES
# ============================================================

umbral_vh <- -18.7
umbral_vh_intenso_2017 <- -20
caida_minima_db_2017 <- 2

anios <- 2017:2024

fecha_inicio_operativa <- as.Date("2017-11-01")

fecha_inicio_complementaria_2017 <- as.Date("2017-10-01")
fecha_fin_complementaria_2017 <- as.Date("2017-11-01")

anio_min_corta_joven <- 2023

base_dir <- file.path("outputs", "tablas_parcelas_clasificadas")

rutas_csv <- file.path(
  base_dir,
  paste0("MEDIANAS_VH_PARCELAS_", anios, ".csv")
)

ruta_shp <- file.path(
  "data",
  "raw",
  "choperas_clasificadas_2017",
  "choperas_clase_fin.shp"
)

out_dir <- file.path("outputs", "deteccion_cortas_2017_2024")
out_shp_dir <- file.path(out_dir, "shapefile")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_shp_dir, recursive = TRUE, showWarnings = FALSE)

salida_shp <- file.path(
  out_shp_dir,
  "choperas_cortas_2017_2024.shp"
)

meses_abrev <- c(
  "ENE", "FEB", "MAR", "ABR", "MAY", "JUN",
  "JUL", "AGO", "SEP", "OCT", "NOV", "DIC"
)

meses_num <- setNames(1:12, meses_abrev)


# ============================================================
# 2. LIMPIEZA DE SALIDAS PREVIAS
# ============================================================

archivos_csv_salida <- file.path(
  out_dir,
  c(
    "01_deteccion_cortas_por_parcela.csv",
    "02_serie_vh_completa_y_deteccion.csv",
    "03_serie_vh_formato_largo.csv",
    "04_resumen_interpretacion.csv",
    "05_resumen_interpretacion_por_clase.csv",
    "06_resumen_cortas_operativas_por_anio.csv",
    "07_detecciones_tempranas_jovenes_por_anio.csv",
    "08_resumen_control_interpretacion.csv",
    "09_superficie_cortada_por_anio.csv",
    "10_resumen_cortas_2017_por_mes.csv",
    "11_resumen_deteccion_complementaria_2017.csv"
  )
)

archivos_csv_existentes <- archivos_csv_salida[file.exists(archivos_csv_salida)]

if (length(archivos_csv_existentes) > 0) {
  ok_csv <- file.remove(archivos_csv_existentes)
  
  if (any(!ok_csv)) {
    stop(
      "No se han podido eliminar algunas salidas previas. ",
      "Comprueba que no estén abiertas en otro programa."
    )
  }
}

archivos_shp_previos <- list.files(
  out_shp_dir,
  pattern = "^choperas_cortas_2017_2024\\.",
  full.names = TRUE
)

if (length(archivos_shp_previos) > 0) {
  ok_shp <- file.remove(archivos_shp_previos)
  
  if (any(!ok_shp)) {
    stop(
      "No se han podido eliminar algunos archivos espaciales previos. ",
      "Comprueba que no estén abiertos en otro programa."
    )
  }
}


# ============================================================
# 3. FUNCIONES AUXILIARES
# ============================================================

leer_csv_anual <- function(ruta) {
  
  anio <- as.integer(str_extract(basename(ruta), "\\d{4}"))
  
  if (!file.exists(ruta)) {
    stop("No existe el archivo: ", ruta)
  }
  
  columnas_mensuales <- paste0(meses_abrev, "_", anio)
  
  tabla <- read_delim(
    file = ruta,
    delim = ";",
    locale = locale(decimal_mark = "."),
    show_col_types = FALSE,
    col_types = cols(
      ID = col_integer(),
      REFCAT = col_character(),
      CLASE_FIN = col_integer(),
      .default = col_double()
    )
  )
  
  columnas_necesarias <- c("ID", "REFCAT", "CLASE_FIN", columnas_mensuales)
  faltan <- setdiff(columnas_necesarias, names(tabla))
  
  if (length(faltan) > 0) {
    stop(
      "Al archivo ", basename(ruta),
      " le faltan estas columnas: ",
      paste(faltan, collapse = ", ")
    )
  }
  
  tabla %>%
    select(all_of(columnas_necesarias)) %>%
    arrange(ID)
}


escribir_csv <- function(tabla, ruta) {
  write_delim(
    tabla,
    ruta,
    delim = ";",
    na = ""
  )
}


# ============================================================
# 4. LECTURA Y CONTROL DE LAS SERIES ANUALES
# ============================================================

lista_anual <- map(rutas_csv, leer_csv_anual)
names(lista_anual) <- as.character(anios)

n_filas <- map_int(lista_anual, nrow)

if (length(unique(n_filas)) != 1) {
  stop(
    "Los CSV anuales no tienen el mismo número de filas: ",
    paste(names(n_filas), n_filas, sep = " = ", collapse = "; ")
  )
}

referencia_ids <- lista_anual[["2017"]] %>%
  select(ID, REFCAT, CLASE_FIN)

for (anio in names(lista_anual)) {
  
  comprobacion <- lista_anual[[anio]] %>%
    select(ID, REFCAT, CLASE_FIN)
  
  if (!identical(referencia_ids, comprobacion)) {
    stop(
      "El archivo del año ", anio,
      " no tiene exactamente las mismas parcelas, REFCAT o CLASE_FIN que 2017."
    )
  }
}

if (anyDuplicated(referencia_ids$ID) > 0) {
  stop("Hay valores duplicados en el campo ID.")
}


# ============================================================
# 5. UNIÓN DE SERIES MENSUALES
# ============================================================

tabla_vh_wide <- reduce(
  lista_anual,
  full_join,
  by = c("ID", "REFCAT", "CLASE_FIN")
) %>%
  arrange(ID) %>%
  mutate(
    FID_ARCGIS = ID - 1,
    .after = ID
  )


# ============================================================
# 6. INCORPORACIÓN DE ATRIBUTOS AUXILIARES
# ============================================================

if (file.exists(ruta_shp)) {
  
  shp_tmp <- st_read(ruta_shp, quiet = TRUE)
  
  atributos_shp <- shp_tmp %>%
    st_drop_geometry() %>%
    mutate(ID = row_number())
  
  if ("CLAVE_2" %in% names(atributos_shp)) {
    
    claves_shp <- atributos_shp %>%
      select(ID, REFCAT_SHP = REFCAT, CLAVE_2)
    
    control_refcat <- tabla_vh_wide %>%
      select(ID, REFCAT_CSV = REFCAT) %>%
      left_join(claves_shp, by = "ID") %>%
      mutate(COINCIDE_REFCAT = REFCAT_CSV == REFCAT_SHP)
    
    if (all(control_refcat$COINCIDE_REFCAT, na.rm = TRUE)) {
      
      tabla_vh_wide <- tabla_vh_wide %>%
        left_join(
          claves_shp %>% select(ID, CLAVE_2),
          by = "ID"
        ) %>%
        relocate(CLAVE_2, .after = REFCAT)
    }
  }
}


# ============================================================
# 7. FORMATO LARGO DE LA SERIE TEMPORAL
# ============================================================

columnas_vh <- names(tabla_vh_wide) %>%
  str_subset("^(ENE|FEB|MAR|ABR|MAY|JUN|JUL|AGO|SEP|OCT|NOV|DIC)_\\d{4}$")

orden_columnas <- tibble(COLUMNA = columnas_vh) %>%
  mutate(
    MES_TXT = str_sub(COLUMNA, 1, 3),
    MES_NUM = meses_num[MES_TXT],
    ANIO = as.integer(str_extract(COLUMNA, "\\d{4}$"))
  ) %>%
  arrange(ANIO, MES_NUM)

columnas_vh_ordenadas <- orden_columnas$COLUMNA

tabla_vh_long <- tabla_vh_wide %>%
  select(
    ID,
    FID_ARCGIS,
    any_of("CLAVE_2"),
    REFCAT,
    CLASE_FIN,
    all_of(columnas_vh_ordenadas)
  ) %>%
  pivot_longer(
    cols = all_of(columnas_vh_ordenadas),
    names_to = "MES_ANIO",
    values_to = "VH_MEDIANA"
  ) %>%
  mutate(
    MES_TXT = str_sub(MES_ANIO, 1, 3),
    MES_NUM = meses_num[MES_TXT],
    ANIO = as.integer(str_extract(MES_ANIO, "\\d{4}$")),
    FECHA_MES = as.Date(sprintf("%04d-%02d-01", ANIO, MES_NUM)),
    BAJO_UMBRAL = !is.na(VH_MEDIANA) & VH_MEDIANA < umbral_vh
  ) %>%
  arrange(ID, FECHA_MES)


# ============================================================
# 8. RESUMEN DE LA SERIE OPERATIVA
# ============================================================

tabla_vh_operativa <- tabla_vh_long %>%
  filter(FECHA_MES >= fecha_inicio_operativa)

resumen_serie <- tabla_vh_operativa %>%
  group_by(ID) %>%
  arrange(FECHA_MES, .by_group = TRUE) %>%
  summarise(
    N_MESES_VALIDOS = sum(!is.na(VH_MEDIANA)),
    N_MESES_BAJO_UMBRAL = sum(BAJO_UMBRAL, na.rm = TRUE),
    VH_MIN_SERIE = suppressWarnings(min(VH_MEDIANA, na.rm = TRUE)),
    VH_MEDIA_SERIE = suppressWarnings(mean(VH_MEDIANA, na.rm = TRUE)),
    VH_NOV_2017 = VH_MEDIANA[FECHA_MES == fecha_inicio_operativa][1],
    NOV_2017_BAJO_UMBRAL = BAJO_UMBRAL[FECHA_MES == fecha_inicio_operativa][1],
    .groups = "drop"
  ) %>%
  mutate(
    VH_MIN_SERIE = ifelse(is.infinite(VH_MIN_SERIE), NA_real_, VH_MIN_SERIE),
    VH_MEDIA_SERIE = ifelse(is.nan(VH_MEDIA_SERIE), NA_real_, VH_MEDIA_SERIE),
    NOV_2017_BAJO_UMBRAL = if_else(
      is.na(NOV_2017_BAJO_UMBRAL),
      FALSE,
      NOV_2017_BAJO_UMBRAL
    )
  )


# ============================================================
# 9. PRIMERA DETECCIÓN DESDE NOVIEMBRE DE 2017
# ============================================================

deteccion_desde_nov2017 <- tabla_vh_operativa %>%
  filter(BAJO_UMBRAL) %>%
  group_by(ID) %>%
  slice_min(FECHA_MES, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    ID,
    FECHA_DET = FECHA_MES,
    ANIO_DET = ANIO,
    MES_DET = MES_NUM,
    MES_DET_TXT = MES_TXT,
    VH_DET = VH_MEDIANA
  )


# ============================================================
# 10. DETECCIÓN COMPLEMENTARIA DE 2017
# ============================================================

tabla_2017_complementaria <- tabla_vh_long %>%
  filter(ANIO == 2017) %>%
  group_by(ID) %>%
  arrange(FECHA_MES, .by_group = TRUE) %>%
  mutate(
    BAJO_PREV_1 = lag(BAJO_UMBRAL, 1),
    BAJO_PREV_2 = lag(BAJO_UMBRAL, 2),
    BAJO_POST_1 = lead(BAJO_UMBRAL, 1),
    
    VH_PREV_1 = lag(VH_MEDIANA, 1),
    VH_PREV_2 = lag(VH_MEDIANA, 2),
    VH_POST_1 = lead(VH_MEDIANA, 1),
    
    MEDIA_VH_PREV_2 = (VH_PREV_1 + VH_PREV_2) / 2,
    CAIDA_DB_RESPECTO_PREV = MEDIA_VH_PREV_2 - VH_MEDIANA,
    
    CANDIDATA_2017 = FECHA_MES >= fecha_inicio_complementaria_2017 &
      FECHA_MES <= fecha_fin_complementaria_2017 &
      BAJO_UMBRAL &
      !is.na(BAJO_PREV_1) &
      !is.na(BAJO_PREV_2) &
      !is.na(BAJO_POST_1) &
      !is.na(VH_MEDIANA) &
      !is.na(MEDIA_VH_PREV_2) &
      BAJO_PREV_1 == FALSE &
      BAJO_PREV_2 == FALSE &
      BAJO_POST_1 == TRUE &
      (
        VH_MEDIANA <= umbral_vh_intenso_2017 |
          CAIDA_DB_RESPECTO_PREV >= caida_minima_db_2017
      )
  ) %>%
  ungroup()

deteccion_complementaria_2017 <- tabla_2017_complementaria %>%
  filter(CANDIDATA_2017) %>%
  group_by(ID) %>%
  slice_min(FECHA_MES, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    ID,
    FECHA_DET_2017_COMP = FECHA_MES,
    ANIO_DET_2017_COMP = ANIO,
    MES_DET_2017_COMP = MES_NUM,
    MES_DET_2017_COMP_TXT = MES_TXT,
    VH_DET_2017_COMP = VH_MEDIANA,
    VH_PREV1_2017_COMP = VH_PREV_1,
    VH_PREV2_2017_COMP = VH_PREV_2,
    VH_POST1_2017_COMP = VH_POST_1,
    MEDIA_VH_PREV2_2017_COMP = MEDIA_VH_PREV_2,
    CAIDA_DB_2017_COMP = CAIDA_DB_RESPECTO_PREV
  )


# ============================================================
# 11. INTERPRETACIÓN POR PARCELA
# ============================================================

tabla_deteccion <- tabla_vh_wide %>%
  select(
    ID,
    FID_ARCGIS,
    any_of("CLAVE_2"),
    REFCAT,
    CLASE_FIN
  ) %>%
  left_join(resumen_serie, by = "ID") %>%
  left_join(deteccion_desde_nov2017, by = "ID") %>%
  left_join(deteccion_complementaria_2017, by = "ID") %>%
  mutate(
    CLASE_INICIAL = case_when(
      CLASE_FIN == 1 ~ "Chopera adulta",
      CLASE_FIN == 2 ~ "Chopera joven",
      CLASE_FIN == 3 ~ "Chopera muy joven",
      TRUE ~ "Clase no identificada"
    ),
    
    FECHA_DET_TXT = if_else(
      is.na(FECHA_DET),
      NA_character_,
      format(FECHA_DET, "%Y-%m")
    ),
    
    FECHA_DET_2017_COMP_TXT = if_else(
      is.na(FECHA_DET_2017_COMP),
      NA_character_,
      format(FECHA_DET_2017_COMP, "%Y-%m")
    ),
    
    DETECCION_DESDE_NOV2017 = !is.na(FECHA_DET),
    
    DETECCION_2017_COMPLEMENTARIA = !is.na(FECHA_DET_2017_COMP) &
      NOV_2017_BAJO_UMBRAL,
    
    DETECCION_TEMPRANA_JOVEN = CLASE_FIN == 2 &
      (
        (
          DETECCION_DESDE_NOV2017 &
            ANIO_DET < anio_min_corta_joven
        ) |
          DETECCION_2017_COMPLEMENTARIA
      ),
    
    DETECCION_JOVEN_ACEPTABLE = CLASE_FIN == 2 &
      DETECCION_DESDE_NOV2017 &
      ANIO_DET >= anio_min_corta_joven,
    
    ESTADO_INTERPRETACION = case_when(
      CLASE_FIN == 3 ~
        "Chopera muy joven excluida del análisis de cortas",
      
      CLASE_FIN == 1 & DETECCION_2017_COMPLEMENTARIA ~
        "Corta operativa detectada",
      
      CLASE_FIN == 2 & DETECCION_2017_COMPLEMENTARIA ~
        "Detección temprana en chopera joven",
      
      NOV_2017_BAJO_UMBRAL ~
        "Serie iniciada bajo el umbral en noviembre de 2017",
      
      !DETECCION_DESDE_NOV2017 ~
        "Sin corta detectada hasta 2024",
      
      DETECCION_TEMPRANA_JOVEN ~
        "Detección temprana en chopera joven",
      
      DETECCION_DESDE_NOV2017 ~
        "Corta operativa detectada",
      
      TRUE ~
        "Caso de interpretación incierta"
    ),
    
    CORTA_OPERATIVA = ESTADO_INTERPRETACION == "Corta operativa detectada",
    SIN_CORTA = ESTADO_INTERPRETACION == "Sin corta detectada hasta 2024",
    EXCLUIDA_MUY_JOVEN = ESTADO_INTERPRETACION == "Chopera muy joven excluida del análisis de cortas",
    SERIE_BAJA_NOV2017 = ESTADO_INTERPRETACION == "Serie iniciada bajo el umbral en noviembre de 2017",
    DETECCION_TEMPRANA = ESTADO_INTERPRETACION == "Detección temprana en chopera joven",
    
    FECHA_CORTE = case_when(
      CORTA_OPERATIVA & DETECCION_2017_COMPLEMENTARIA ~ FECHA_DET_2017_COMP,
      CORTA_OPERATIVA ~ FECHA_DET,
      TRUE ~ as.Date(NA)
    ),
    
    FECHA_CORTE_TXT = if_else(
      is.na(FECHA_CORTE),
      NA_character_,
      format(FECHA_CORTE, "%Y-%m")
    ),
    
    ANIO_CORTE = if_else(
      CORTA_OPERATIVA,
      year(FECHA_CORTE),
      NA_integer_
    ),
    
    MES_CORTE = if_else(
      CORTA_OPERATIVA,
      month(FECHA_CORTE),
      NA_integer_
    ),
    
    VH_CORTE = case_when(
      CORTA_OPERATIVA & DETECCION_2017_COMPLEMENTARIA ~ VH_DET_2017_COMP,
      CORTA_OPERATIVA ~ VH_DET,
      TRUE ~ NA_real_
    ),
    
    MOTIVO_INTERPRETACION = case_when(
      ESTADO_INTERPRETACION == "Corta operativa detectada" &
        DETECCION_2017_COMPLEMENTARIA ~
        "Descenso de VH en octubre o noviembre de 2017 con señal previa sobre umbral, persistencia posterior e intensidad suficiente",
      
      ESTADO_INTERPRETACION == "Corta operativa detectada" ~
        "Descenso de VH por debajo del umbral dentro del periodo operativo y compatible con la clase inicial",
      
      ESTADO_INTERPRETACION == "Sin corta detectada hasta 2024" ~
        "No se registra ningún valor mensual inferior al umbral desde noviembre de 2017",
      
      ESTADO_INTERPRETACION == "Chopera muy joven excluida del análisis de cortas" ~
        "Plantación en fase inicial en 2017, fuera del turno ordinario de corta durante el periodo analizado",
      
      ESTADO_INTERPRETACION == "Serie iniciada bajo el umbral en noviembre de 2017" ~
        "La parcela presenta valores bajos de VH en noviembre de 2017 sin transición clara identificada en octubre o noviembre",
      
      ESTADO_INTERPRETACION == "Detección temprana en chopera joven" &
        DETECCION_2017_COMPLEMENTARIA ~
        "Descenso de VH en octubre o noviembre de 2017 en parcela clasificada como joven; se conserva como detección temprana",
      
      ESTADO_INTERPRETACION == "Detección temprana en chopera joven" ~
        "Cruce del umbral anterior a 2023 en una parcela clasificada como joven; requiere interpretación cautelosa",
      
      TRUE ~
        "Caso no clasificado"
    )
  ) %>%
  select(
    ID,
    FID_ARCGIS,
    any_of("CLAVE_2"),
    REFCAT,
    CLASE_FIN,
    CLASE_INICIAL,
    
    ESTADO_INTERPRETACION,
    CORTA_OPERATIVA,
    SIN_CORTA,
    EXCLUIDA_MUY_JOVEN,
    SERIE_BAJA_NOV2017,
    DETECCION_TEMPRANA,
    MOTIVO_INTERPRETACION,
    
    FECHA_CORTE_TXT,
    FECHA_CORTE,
    ANIO_CORTE,
    MES_CORTE,
    VH_CORTE,
    
    FECHA_DET_TXT,
    FECHA_DET,
    ANIO_DET,
    MES_DET,
    VH_DET,
    
    FECHA_DET_2017_COMP_TXT,
    FECHA_DET_2017_COMP,
    ANIO_DET_2017_COMP,
    MES_DET_2017_COMP,
    VH_DET_2017_COMP,
    VH_PREV1_2017_COMP,
    VH_PREV2_2017_COMP,
    VH_POST1_2017_COMP,
    MEDIA_VH_PREV2_2017_COMP,
    CAIDA_DB_2017_COMP,
    
    N_MESES_VALIDOS,
    N_MESES_BAJO_UMBRAL,
    VH_MIN_SERIE,
    VH_MEDIA_SERIE,
    VH_NOV_2017,
    NOV_2017_BAJO_UMBRAL,
    DETECCION_DESDE_NOV2017,
    DETECCION_2017_COMPLEMENTARIA,
    DETECCION_TEMPRANA_JOVEN,
    DETECCION_JOVEN_ACEPTABLE
  ) %>%
  arrange(ID)


# ============================================================
# 12. TABLAS DE SALIDA
# ============================================================

tabla_final_completa <- tabla_vh_wide %>%
  left_join(
    tabla_deteccion %>%
      select(
        ID,
        ESTADO_INTERPRETACION,
        CORTA_OPERATIVA,
        SIN_CORTA,
        EXCLUIDA_MUY_JOVEN,
        SERIE_BAJA_NOV2017,
        DETECCION_TEMPRANA,
        FECHA_CORTE_TXT,
        ANIO_CORTE,
        MES_CORTE,
        FECHA_DET_TXT,
        ANIO_DET,
        MES_DET,
        VH_DET,
        FECHA_DET_2017_COMP_TXT,
        ANIO_DET_2017_COMP,
        MES_DET_2017_COMP,
        VH_DET_2017_COMP,
        MEDIA_VH_PREV2_2017_COMP,
        CAIDA_DB_2017_COMP,
        DETECCION_2017_COMPLEMENTARIA,
        VH_NOV_2017,
        N_MESES_BAJO_UMBRAL
      ),
    by = "ID"
  )

resumen_estado <- tabla_deteccion %>%
  count(ESTADO_INTERPRETACION, name = "N_PARCELAS") %>%
  mutate(PCT = round(100 * N_PARCELAS / sum(N_PARCELAS), 2)) %>%
  arrange(desc(N_PARCELAS))

resumen_estado_por_clase <- tabla_deteccion %>%
  count(CLASE_INICIAL, ESTADO_INTERPRETACION, name = "N_PARCELAS") %>%
  group_by(CLASE_INICIAL) %>%
  mutate(PCT_CLASE = round(100 * N_PARCELAS / sum(N_PARCELAS), 2)) %>%
  ungroup() %>%
  arrange(CLASE_INICIAL, desc(N_PARCELAS))

resumen_cortas_por_anio <- tabla_deteccion %>%
  filter(CORTA_OPERATIVA) %>%
  count(ANIO_CORTE, name = "N_CORTAS_OPERATIVAS") %>%
  arrange(ANIO_CORTE)

resumen_cortas_2017_por_mes <- tabla_deteccion %>%
  filter(CORTA_OPERATIVA, ANIO_CORTE == 2017) %>%
  count(MES_CORTE, name = "N_CORTAS_OPERATIVAS") %>%
  mutate(MES = meses_abrev[MES_CORTE]) %>%
  select(MES_CORTE, MES, N_CORTAS_OPERATIVAS) %>%
  arrange(MES_CORTE)

resumen_deteccion_2017_complementaria <- tabla_deteccion %>%
  filter(DETECCION_2017_COMPLEMENTARIA) %>%
  count(CLASE_INICIAL, MES_DET_2017_COMP, name = "N_PARCELAS") %>%
  mutate(MES = meses_abrev[MES_DET_2017_COMP]) %>%
  select(CLASE_INICIAL, MES_DET_2017_COMP, MES, N_PARCELAS) %>%
  arrange(CLASE_INICIAL, MES_DET_2017_COMP)


# ============================================================
# 13. SUPERFICIE CORTADA POR AÑO
# ============================================================

if (!file.exists(ruta_shp)) {
  stop("No se ha encontrado la capa espacial de referencia.")
}

atributos_area <- st_read(ruta_shp, quiet = TRUE) %>%
  st_drop_geometry() %>%
  mutate(ID = row_number())

if ("SUP_TOTAL" %in% names(atributos_area)) {
  
  tabla_area <- atributos_area %>%
    select(ID, SUP_HA = SUP_TOTAL)
  
} else if ("AREA_TOTAL" %in% names(atributos_area)) {
  
  tabla_area <- atributos_area %>%
    select(ID, AREA_M2 = AREA_TOTAL) %>%
    mutate(SUP_HA = AREA_M2 / 10000) %>%
    select(ID, SUP_HA)
  
} else if ("Shape_Area" %in% names(atributos_area)) {
  
  tabla_area <- atributos_area %>%
    select(ID, AREA_M2 = Shape_Area) %>%
    mutate(SUP_HA = AREA_M2 / 10000) %>%
    select(ID, SUP_HA)
  
} else {
  
  stop("No se encontró ningún campo de superficie válido.")
}

resumen_superficie_cortada_por_anio <- tabla_deteccion %>%
  left_join(tabla_area, by = "ID") %>%
  filter(CORTA_OPERATIVA) %>%
  group_by(ANIO_CORTE) %>%
  summarise(
    N_CORTAS_OPERATIVAS = n(),
    SUP_CORTADA_HA = round(sum(SUP_HA, na.rm = TRUE), 2),
    SUP_MEDIA_HA = round(mean(SUP_HA, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(ANIO_CORTE)

resumen_detecciones_tempranas_jovenes <- tabla_deteccion %>%
  filter(DETECCION_TEMPRANA) %>%
  mutate(
    ANIO_DET_RESUMEN = case_when(
      DETECCION_2017_COMPLEMENTARIA ~ 2017L,
      !is.na(ANIO_DET) ~ ANIO_DET,
      TRUE ~ NA_integer_
    )
  ) %>%
  count(ANIO_DET_RESUMEN, name = "N_DETECCIONES_TEMPRANAS_JOVENES") %>%
  rename(ANIO_DET = ANIO_DET_RESUMEN) %>%
  arrange(ANIO_DET)

resumen_control <- tabla_deteccion %>%
  summarise(
    TOTAL_PARCELAS = n(),
    N_CORTA_OPERATIVA = sum(CORTA_OPERATIVA, na.rm = TRUE),
    N_SIN_CORTA = sum(SIN_CORTA, na.rm = TRUE),
    N_EXCLUIDA_MUY_JOVEN = sum(EXCLUIDA_MUY_JOVEN, na.rm = TRUE),
    N_SERIE_BAJA_NOV2017 = sum(SERIE_BAJA_NOV2017, na.rm = TRUE),
    N_DETECCION_TEMPRANA = sum(DETECCION_TEMPRANA, na.rm = TRUE),
    N_DETECCION_2017_COMPLEMENTARIA = sum(DETECCION_2017_COMPLEMENTARIA, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "INDICADOR",
    values_to = "VALOR"
  )


# ============================================================
# 14. CONTROLES INTERNOS
# ============================================================

total_parcelas_resumen <- sum(resumen_estado$N_PARCELAS)

total_cortas_estado <- resumen_estado %>%
  filter(ESTADO_INTERPRETACION == "Corta operativa detectada") %>%
  pull(N_PARCELAS)

total_cortas_anio <- sum(resumen_cortas_por_anio$N_CORTAS_OPERATIVAS)

total_cortas_superficie <- sum(
  resumen_superficie_cortada_por_anio$N_CORTAS_OPERATIVAS
)

total_cortas_2017 <- resumen_cortas_por_anio %>%
  filter(ANIO_CORTE == 2017) %>%
  pull(N_CORTAS_OPERATIVAS)

total_cortas_2017_mes <- sum(resumen_cortas_2017_por_mes$N_CORTAS_OPERATIVAS)

if (total_parcelas_resumen != nrow(tabla_deteccion)) {
  stop("Control fallido: el resumen general no suma el total de parcelas.")
}

if (length(total_cortas_estado) != 1) {
  stop("Control fallido: no se identifica correctamente el total de cortas operativas.")
}

if (total_cortas_estado != total_cortas_anio) {
  stop("Control fallido: el resumen general no coincide con el resumen anual.")
}

if (total_cortas_estado != total_cortas_superficie) {
  stop("Control fallido: el resumen general no coincide con el resumen de superficie.")
}

if (length(total_cortas_2017) == 1 && total_cortas_2017 != total_cortas_2017_mes) {
  stop("Control fallido: las cortas de 2017 no coinciden entre resumen anual y mensual.")
}


# ============================================================
# 15. EXPORTACIÓN DE TABLAS
# ============================================================

escribir_csv(
  tabla_deteccion,
  file.path(out_dir, "01_deteccion_cortas_por_parcela.csv")
)

escribir_csv(
  tabla_final_completa,
  file.path(out_dir, "02_serie_vh_completa_y_deteccion.csv")
)

escribir_csv(
  tabla_vh_long,
  file.path(out_dir, "03_serie_vh_formato_largo.csv")
)

escribir_csv(
  resumen_estado,
  file.path(out_dir, "04_resumen_interpretacion.csv")
)

escribir_csv(
  resumen_estado_por_clase,
  file.path(out_dir, "05_resumen_interpretacion_por_clase.csv")
)

escribir_csv(
  resumen_cortas_por_anio,
  file.path(out_dir, "06_resumen_cortas_operativas_por_anio.csv")
)

escribir_csv(
  resumen_detecciones_tempranas_jovenes,
  file.path(out_dir, "07_detecciones_tempranas_jovenes_por_anio.csv")
)

escribir_csv(
  resumen_control,
  file.path(out_dir, "08_resumen_control_interpretacion.csv")
)

escribir_csv(
  resumen_superficie_cortada_por_anio,
  file.path(out_dir, "09_superficie_cortada_por_anio.csv")
)

escribir_csv(
  resumen_cortas_2017_por_mes,
  file.path(out_dir, "10_resumen_cortas_2017_por_mes.csv")
)

escribir_csv(
  resumen_deteccion_2017_complementaria,
  file.path(out_dir, "11_resumen_deteccion_complementaria_2017.csv")
)


# ============================================================
# 16. EXPORTACIÓN ESPACIAL
# ============================================================

shp <- st_read(ruta_shp, quiet = TRUE)

shp <- shp %>%
  mutate(ID = row_number())

control_union <- shp %>%
  st_drop_geometry() %>%
  select(ID, REFCAT_SHP = REFCAT) %>%
  left_join(
    tabla_deteccion %>% select(ID, REFCAT_CSV = REFCAT),
    by = "ID"
  ) %>%
  mutate(COINCIDE_REFCAT = REFCAT_SHP == REFCAT_CSV)

n_no_coinciden <- sum(!control_union$COINCIDE_REFCAT, na.rm = TRUE)

if (n_no_coinciden > 0) {
  warning(
    "Hay ", n_no_coinciden,
    " registros donde REFCAT no coincide entre la capa espacial y la tabla de resultados."
  )
}

tabla_join_shp <- tabla_deteccion %>%
  transmute(
    ID,
    
    EST_INT = case_when(
      ESTADO_INTERPRETACION == "Corta operativa detectada" ~ "CORTA_OK",
      ESTADO_INTERPRETACION == "Sin corta detectada hasta 2024" ~ "SIN_CORTA",
      ESTADO_INTERPRETACION == "Chopera muy joven excluida del análisis de cortas" ~ "EXC_MJ",
      ESTADO_INTERPRETACION == "Serie iniciada bajo el umbral en noviembre de 2017" ~ "SER_NOV17",
      ESTADO_INTERPRETACION == "Detección temprana en chopera joven" ~ "DET_JOV",
      TRUE ~ "INCIERTA"
    ),
    
    CL_INI = case_when(
      CLASE_FIN == 1 ~ "ADULTA",
      CLASE_FIN == 2 ~ "JOVEN",
      CLASE_FIN == 3 ~ "MUY_JOV",
      TRUE ~ "SIN_CLASE"
    ),
    
    CORTA_OK = as.integer(CORTA_OPERATIVA),
    SIN_CORT = as.integer(SIN_CORTA),
    EXC_MJ = as.integer(EXCLUIDA_MUY_JOVEN),
    SER_NOV17 = as.integer(SERIE_BAJA_NOV2017),
    DET_JOV = as.integer(DETECCION_TEMPRANA),
    DET17COMP = as.integer(DETECCION_2017_COMPLEMENTARIA),
    
    F_CORTE = FECHA_CORTE_TXT,
    A_CORTE = ANIO_CORTE,
    M_CORTE = MES_CORTE,
    VH_CORTE = round(VH_CORTE, 3),
    
    F_DET = FECHA_DET_TXT,
    A_DET = ANIO_DET,
    M_DET = MES_DET,
    VH_DET = round(VH_DET, 3),
    
    F_DET17 = FECHA_DET_2017_COMP_TXT,
    M_DET17 = MES_DET_2017_COMP,
    VH_DET17 = round(VH_DET_2017_COMP, 3),
    CAIDA17 = round(CAIDA_DB_2017_COMP, 3),
    
    VH_NOV17 = round(VH_NOV_2017, 3),
    VH_MIN = round(VH_MIN_SERIE, 3),
    N_BAJO = N_MESES_BAJO_UMBRAL,
    
    MOTIVO = case_when(
      ESTADO_INTERPRETACION == "Corta operativa detectada" &
        DETECCION_2017_COMPLEMENTARIA ~
        "Corta 2017 transicion clara",
      ESTADO_INTERPRETACION == "Corta operativa detectada" ~
        "Corta operativa",
      ESTADO_INTERPRETACION == "Sin corta detectada hasta 2024" ~
        "Sin corta hasta 2024",
      ESTADO_INTERPRETACION == "Chopera muy joven excluida del análisis de cortas" ~
        "Muy joven excluida",
      ESTADO_INTERPRETACION == "Serie iniciada bajo el umbral en noviembre de 2017" ~
        "Serie baja en nov2017",
      ESTADO_INTERPRETACION == "Detección temprana en chopera joven" ~
        "Joven deteccion temprana",
      TRUE ~
        "Interpretacion incierta"
    )
  )

shp_deteccion <- shp %>%
  left_join(tabla_join_shp, by = "ID")

st_write(
  shp_deteccion,
  dsn = salida_shp,
  driver = "ESRI Shapefile",
  delete_layer = TRUE,
  quiet = TRUE
)

message("Proceso finalizado correctamente.")
message("Resultados exportados en: ", out_dir)