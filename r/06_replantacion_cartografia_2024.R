# ============================================================
# Replantación y cartografía de choperas en 2024
#
# Entradas:
#   - Capa de choperas con detección de cortas 2017-2024.
#   - Resumen parcelario de variables NDVI y VH de 2024.
#
# Salidas:
#   - Capa completa con atributos de replantación.
#   - Cartografía de choperas en pie en 2024.
#   - Tablas de resultados y controles.
# ============================================================

library(tidyverse)
library(sf)


# ============================================================
# 1. RUTAS DE ENTRADA Y SALIDA
# ============================================================

archivo_resumen <- file.path(
  "outputs",
  "replantacion_2024",
  "resumen_parcela",
  "02_resumen_parcela_replantacion_2024_compacto.csv"
)

shp_entrada <- file.path(
  "outputs",
  "deteccion_cortas_2017_2024",
  "shapefile",
  "choperas_cortas_2017_2024.shp"
)

carpeta_salida <- file.path(
  "outputs",
  "replantacion_2024",
  "cartografia_2024"
)

dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(archivo_resumen)) {
  stop("No existe el archivo de resumen parcelario: ", archivo_resumen)
}

if (!file.exists(shp_entrada)) {
  stop("No existe la capa espacial de entrada: ", shp_entrada)
}

shp_salida_completo <- file.path(
  carpeta_salida,
  "choperas_replantacion_2024.shp"
)

shp_salida_carto24 <- file.path(
  carpeta_salida,
  "choperas_cartografia_2024.shp"
)

csv_resultado <- file.path(
  carpeta_salida,
  "01_resultado_replantacion_2024.csv"
)

csv_control_general <- file.path(
  carpeta_salida,
  "02_control_general_replantacion_2024.csv"
)

csv_control_anio <- file.path(
  carpeta_salida,
  "03_control_por_anio_corta_replantacion_2024.csv"
)

csv_control_superficie <- file.path(
  carpeta_salida,
  "04_control_superficie_replantacion_2024.csv"
)

csv_no_evaluables <- file.path(
  carpeta_salida,
  "05_parcelas_no_evaluables.csv"
)

csv_control_carto24 <- file.path(
  carpeta_salida,
  "06_control_cartografia_2024.csv"
)

csv_control_coherencia <- file.path(
  carpeta_salida,
  "07_control_coherencia_cortas.csv"
)


# ============================================================
# 2. FUNCIONES AUXILIARES
# ============================================================

limpiar_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9_\\-\\+\\=\\<\\>\\.\\,\\;\\: ]", "")
  x
}

recortar_txt <- function(x, n = 80) {
  stringr::str_sub(limpiar_txt(x), 1, n)
}

num_seguro <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- stringr::str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

int_seguro <- function(x) {
  suppressWarnings(as.integer(num_seguro(x)))
}

borrar_shp <- function(ruta_shp) {
  base <- tools::file_path_sans_ext(basename(ruta_shp))
  carpeta <- dirname(ruta_shp)
  
  archivos <- list.files(
    carpeta,
    pattern = paste0("^", base, "\\."),
    full.names = TRUE
  )
  
  if (length(archivos) > 0) {
    unlink(archivos, force = TRUE)
  }
}


# ============================================================
# 3. UMBRALES DE REPLANTACIÓN
# ============================================================

anio_referencia <- 2024

# Umbrales definidos según los criterios metodológicos de identificación
# de replantaciones establecidos para 2024.

u3_ndvi_jun <- 0.60
u3_ndvi_sep <- 0.65
u3_vh <- -16.7

u2_ndvi_jun <- 0.55
u2_ndvi_sep <- 0.60
u2_vh <- -18.0

u1_ndvi_jun <- 0.50
u1_ndvi_sep <- 0.50
u1_vh <- -18.0


# ============================================================
# 4. LECTURA DEL RESUMEN PARCELARIO
# ============================================================

resumen <- read_delim(
  file = archivo_resumen,
  delim = ";",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE,
  trim_ws = TRUE
)

campos_necesarios_resumen <- c(
  "ID",
  "ID_PARCELA",
  "A_CORTE",
  "M_CORTE",
  "F_CORTE",
  "NDVI_JUN2024_VALOR",
  "NDVI_SEP2024_VALOR",
  "VH_JUNSEP2024_VALOR",
  "n_pix_total",
  "pixeles_validos_min"
)

faltan_resumen <- setdiff(campos_necesarios_resumen, names(resumen))

if (length(faltan_resumen) > 0) {
  stop(
    paste(
      "Faltan campos necesarios en el resumen parcelario:",
      paste(faltan_resumen, collapse = ", ")
    )
  )
}

resumen <- resumen %>%
  mutate(
    ID_JOIN = as.character(ID),
    A_RPL = int_seguro(A_CORTE),
    M_RPL = int_seguro(M_CORTE),
    F_RPL = as.character(F_CORTE),
    NDVI_JUN2024_VALOR = num_seguro(NDVI_JUN2024_VALOR),
    NDVI_SEP2024_VALOR = num_seguro(NDVI_SEP2024_VALOR),
    VH_JUNSEP2024_VALOR = num_seguro(VH_JUNSEP2024_VALOR),
    n_pix_total = int_seguro(n_pix_total),
    pixeles_validos_min = int_seguro(pixeles_validos_min)
  ) %>%
  distinct(ID_JOIN, .keep_all = TRUE)

if (any(duplicated(resumen$ID_JOIN))) {
  stop("Hay ID duplicados en el resumen parcelario.")
}


# ============================================================
# 5. CLASIFICACIÓN DE REPLANTACIÓN
# ============================================================

repl <- resumen %>%
  mutate(
    DATOS24 = !is.na(NDVI_JUN2024_VALOR) &
      !is.na(NDVI_SEP2024_VALOR) &
      !is.na(VH_JUNSEP2024_VALOR),
    
    CAUT23_SRC = A_RPL == 2023,
    
    NDVI_3 = NDVI_JUN2024_VALOR >= u3_ndvi_jun &
      NDVI_SEP2024_VALOR >= u3_ndvi_sep,
    VH_3 = VH_JUNSEP2024_VALOR >= u3_vh,
    
    NDVI_2 = NDVI_JUN2024_VALOR >= u2_ndvi_jun &
      NDVI_SEP2024_VALOR >= u2_ndvi_sep,
    VH_2 = VH_JUNSEP2024_VALOR >= u2_vh,
    
    NDVI_1 = NDVI_JUN2024_VALOR >= u1_ndvi_jun &
      NDVI_SEP2024_VALOR >= u1_ndvi_sep,
    VH_1 = VH_JUNSEP2024_VALOR >= u1_vh,
    
    RPL_SRC = case_when(
      !DATOS24 ~ 9L,
      NDVI_3 & VH_3 ~ 3L,
      NDVI_2 & VH_2 ~ 2L,
      NDVI_2 & !VH_2 ~ 1L,
      NDVI_1 & VH_1 ~ 1L,
      TRUE ~ 0L
    ),
    
    RPLCONF_SRC = case_when(
      RPL_SRC == 3L ~ "ALTA",
      RPL_SRC == 2L ~ "MEDIA",
      RPL_SRC == 1L ~ "BAJA",
      RPL_SRC == 0L ~ "SIN_EVID",
      RPL_SRC == 9L ~ "NO_EVAL",
      TRUE ~ "NO_CLAS"
    ),
    
    RPLTXT_SRC = case_when(
      RPL_SRC == 3L & CAUT23_SRC ~ "Replantacion probable reciente - confianza alta",
      RPL_SRC == 3L ~ "Replantacion probable - confianza alta",
      
      RPL_SRC == 2L & CAUT23_SRC ~ "Replantacion probable reciente - confianza media",
      RPL_SRC == 2L ~ "Replantacion probable - confianza media",
      
      RPL_SRC == 1L & CAUT23_SRC ~ "Replantacion compatible reciente - confianza baja",
      RPL_SRC == 1L ~ "Replantacion compatible - confianza baja",
      
      RPL_SRC == 0L & CAUT23_SRC ~ "Sin evidencia suficiente; corta 2023 no confirmable",
      RPL_SRC == 0L ~ "Sin evidencia suficiente de replantacion",
      
      RPL_SRC == 9L ~ "No evaluable",
      TRUE ~ "Sin clasificar"
    ),
    
    REPAN_SRC = if_else(
      RPL_SRC %in% c(1L, 2L, 3L),
      A_RPL,
      NA_integer_
    ),
    
    EDAD_SRC = if_else(
      RPL_SRC %in% c(1L, 2L, 3L),
      anio_referencia - REPAN_SRC,
      NA_integer_
    ),
    
    EDRPL_SRC = case_when(
      is.na(EDAD_SRC) ~ NA_character_,
      EDAD_SRC <= 2 ~ "0-2 anos",
      EDAD_SRC <= 4 ~ "3-4 anos",
      EDAD_SRC <= 6 ~ "5-6 anos",
      EDAD_SRC <= 8 ~ "7-8 anos",
      TRUE ~ ">8 anos"
    ),
    
    INCED_SRC = if_else(
      RPL_SRC %in% c(1L, 2L, 3L),
      "+-1-2 anos",
      NA_character_
    )
  ) %>%
  transmute(
    ID_JOIN,
    A_RPL,
    M_RPL,
    F_RPL,
    NDVI_J24_SRC = NDVI_JUN2024_VALOR,
    NDVI_S24_SRC = NDVI_SEP2024_VALOR,
    VH_JS24_SRC = VH_JUNSEP2024_VALOR,
    NPIX24_SRC = n_pix_total,
    NPIXV24_SRC = pixeles_validos_min,
    RPL_SRC,
    RPLCONF_SRC,
    RPLTXT_SRC,
    REPAN_SRC,
    EDAD_SRC,
    EDRPL_SRC,
    INCED_SRC,
    CAUT23_SRC
  )


# ============================================================
# 6. LECTURA DE LA CAPA BASE
# ============================================================

choperas_base <- st_read(
  shp_entrada,
  quiet = TRUE
)

if (!"ID" %in% names(choperas_base)) {
  stop("La capa base no contiene el campo ID.")
}

if (any(duplicated(choperas_base$ID))) {
  stop("La capa base contiene ID duplicados.")
}

campos_esperados_base <- c(
  "REFCAT",
  "nom_muni",
  "SUP_TOTAL",
  "EST_INT",
  "CL_INI",
  "CORTA_OK",
  "SIN_CORT",
  "EXC_MJ",
  "SER_NOV17",
  "DET_JOV",
  "DET17COMP",
  "F_CORTE",
  "A_CORTE",
  "M_CORTE",
  "MOTIVO"
)

faltan_base <- setdiff(campos_esperados_base, names(choperas_base))

if (length(faltan_base) > 0) {
  stop(
    paste(
      "Faltan campos necesarios en la capa base:",
      paste(faltan_base, collapse = ", ")
    )
  )
}


# ============================================================
# 7. LIMPIEZA DE CAMPOS PREVIOS
# ============================================================

campos_resultado_antiguos <- c(
  "ID_JOIN",
  "A_RPL",
  "M_RPL",
  "F_RPL",
  "RPL24",
  "RPL_CONF",
  "RPL_TXT",
  "RPLCONF",
  "REP_AN24",
  "REPAN24",
  "EDAD24",
  "ED_RPL24",
  "EDRPL24",
  "INCED24",
  "NDVI_J24",
  "NDVIJ24",
  "NDVI_S24",
  "NDVIS24",
  "VH_JS24",
  "VHJS24",
  "NPIX24",
  "NPIXV24",
  "INC24",
  "MOTC24",
  "MOT24",
  "ED_MAP24",
  "EDMAP24",
  "EDORC24",
  "ED_ORIG24",
  "FIAB24",
  "CAUT23",
  "CAUT23_I",
  "CAUT23I",
  "SER17_I",
  "SER17I",
  "DETJOV_I",
  "DETJOVI",
  "DET17_I",
  "DET17I",
  "EXCMJ_I",
  "EXCMJI",
  "EXTRA17_I",
  "EXTRA17I",
  "EVALRPL_I",
  "EVALRPLI",
  "C1723_I",
  "C1723I",
  "C2024_I",
  "C2024I",
  "OBSC24"
)

choperas_base_limpia <- choperas_base %>%
  select(-any_of(campos_resultado_antiguos))


# ============================================================
# 8. UNIÓN DE RESULTADOS CON LA CAPA BASE
# ============================================================

choperas_pre <- choperas_base_limpia %>%
  mutate(
    ID_JOIN = as.character(ID),
    
    A_CORTE_OR = int_seguro(A_CORTE),
    M_CORTE_OR = int_seguro(M_CORTE),
    F_CORTE_OR = as.character(F_CORTE),
    
    CORTA_OK_N = int_seguro(CORTA_OK),
    SIN_CORT_N = int_seguro(SIN_CORT),
    EXC_MJ_N = int_seguro(EXC_MJ),
    SER_NOV17_N = int_seguro(SER_NOV17),
    DET_JOV_N = int_seguro(DET_JOV),
    DET17COMP_N = int_seguro(DET17COMP),
    
    CL_INI_TXT = limpiar_txt(CL_INI),
    EST_INT_TXT = limpiar_txt(EST_INT),
    
    CL_INI_UP = stringr::str_to_upper(CL_INI_TXT),
    EST_INT_UP = stringr::str_to_upper(EST_INT_TXT)
  )

choperas_final <- choperas_pre %>%
  left_join(
    repl,
    by = "ID_JOIN"
  )

if (nrow(choperas_final) != nrow(choperas_base_limpia)) {
  stop("La unión ha cambiado el número de entidades.")
}


# ============================================================
# 9. LÓGICA FINAL DE REPLANTACIÓN Y CARTOGRAFÍA 2024
# ============================================================

choperas_final <- choperas_final %>%
  mutate(
    EVAL_REPL = !is.na(RPL_SRC),
    
    FLAG_CORTA_OK = !is.na(CORTA_OK_N) & CORTA_OK_N == 1,
    FLAG_SIN_CORT = !is.na(SIN_CORT_N) & SIN_CORT_N == 1,
    FLAG_EXC_MJ = !is.na(EXC_MJ_N) & EXC_MJ_N == 1,
    FLAG_SER17 = !is.na(SER_NOV17_N) & SER_NOV17_N == 1,
    FLAG_DETJOV = !is.na(DET_JOV_N) & DET_JOV_N == 1,
    FLAG_DET17COMP = !is.na(DET17COMP_N) & DET17COMP_N == 1,
    
    A_CORTF = case_when(
      EVAL_REPL ~ A_RPL,
      TRUE ~ A_CORTE_OR
    ),
    
    M_CORTF = case_when(
      EVAL_REPL ~ M_RPL,
      TRUE ~ M_CORTE_OR
    ),
    
    CORTA_EXTRA17 = !is.na(A_CORTF) &
      A_CORTF == 2017 &
      (FLAG_DET17COMP | (EVAL_REPL & !FLAG_CORTA_OK)),
    
    CORTA_1723 = !is.na(A_CORTF) &
      A_CORTF >= 2017 &
      A_CORTF <= 2023 &
      (FLAG_CORTA_OK | FLAG_DET17COMP | CORTA_EXTRA17 | EVAL_REPL),
    
    CORTA_2024 = !is.na(A_CORTE_OR) &
      A_CORTE_OR == 2024 &
      FLAG_CORTA_OK,
    
    SIN_CORTA_OPER = !CORTA_1723 & !CORTA_2024,
    
    RPL24_FINAL = case_when(
      EVAL_REPL ~ RPL_SRC,
      CORTA_1723 ~ 9L,
      CORTA_2024 ~ 9L,
      TRUE ~ -1L
    ),
    
    RPLCONF_FINAL = case_when(
      EVAL_REPL ~ RPLCONF_SRC,
      CORTA_1723 ~ "NO_EVAL",
      CORTA_2024 ~ "NO_EVAL",
      TRUE ~ "NO_APLICA"
    ),
    
    RPLTXT_FINAL = case_when(
      EVAL_REPL ~ RPLTXT_SRC,
      CORTA_1723 ~ "No evaluable - sin pixeles validos",
      CORTA_2024 ~ "No evaluable - corta 2024",
      TRUE ~ "No aplica - sin corta operativa"
    ),
    
    REPAN_FINAL = case_when(
      EVAL_REPL & RPL24_FINAL %in% c(1L, 2L, 3L) ~ REPAN_SRC,
      TRUE ~ NA_integer_
    ),
    
    EDAD_FINAL = case_when(
      EVAL_REPL & RPL24_FINAL %in% c(1L, 2L, 3L) ~ EDAD_SRC,
      TRUE ~ NA_integer_
    ),
    
    EDRPL_FINAL = case_when(
      EVAL_REPL & RPL24_FINAL %in% c(1L, 2L, 3L) ~ EDRPL_SRC,
      TRUE ~ NA_character_
    ),
    
    INCED_FINAL = case_when(
      EVAL_REPL & RPL24_FINAL %in% c(1L, 2L, 3L) ~ INCED_SRC,
      TRUE ~ NA_character_
    ),
    
    INC24_FINAL = case_when(
      RPL24_FINAL %in% c(1L, 2L, 3L) ~ 1L,
      SIN_CORTA_OPER ~ 1L,
      TRUE ~ 0L
    ),
    
    MOTC24_FINAL = case_when(
      RPL24_FINAL %in% c(1L, 2L, 3L) ~ 1L,
      SIN_CORTA_OPER & FLAG_DETJOV ~ 2L,
      SIN_CORTA_OPER & FLAG_EXC_MJ ~ 3L,
      SIN_CORTA_OPER & FLAG_SER17 ~ 4L,
      SIN_CORTA_OPER ~ 5L,
      CORTA_2024 ~ 6L,
      CORTA_1723 & RPL24_FINAL == 0L ~ 7L,
      CORTA_1723 & RPL24_FINAL == 9L ~ 8L,
      TRUE ~ 99L
    ),
    
    EDMAP_FINAL = case_when(
      RPL24_FINAL %in% c(1L, 2L, 3L) ~ EDRPL_FINAL,
      
      INC24_FINAL == 1L & FLAG_EXC_MJ ~ "8-11 anos",
      INC24_FINAL == 1L & stringr::str_detect(CL_INI_UP, "MUY") ~ "8-11 anos",
      
      INC24_FINAL == 1L & FLAG_DETJOV ~ "11-14 anos",
      INC24_FINAL == 1L &
        stringr::str_detect(CL_INI_UP, "JOV") &
        !stringr::str_detect(CL_INI_UP, "MUY") ~ "11-14 anos",
      
      INC24_FINAL == 1L & stringr::str_detect(CL_INI_UP, "ADULT") ~ ">=14 anos",
      INC24_FINAL == 1L & stringr::str_detect(EST_INT_UP, "ADULT") ~ ">=14 anos",
      
      INC24_FINAL == 1L ~ "Edad no asignada",
      TRUE ~ NA_character_
    ),
    
    EDORC_FINAL = case_when(
      RPL24_FINAL %in% c(1L, 2L, 3L) ~ 1L,
      INC24_FINAL == 1L & FLAG_EXC_MJ ~ 2L,
      INC24_FINAL == 1L & FLAG_DETJOV ~ 3L,
      INC24_FINAL == 1L & FLAG_SER17 ~ 4L,
      INC24_FINAL == 1L ~ 5L,
      TRUE ~ NA_integer_
    ),
    
    FIAB_FINAL = case_when(
      RPL24_FINAL == 3L ~ "ALTA",
      RPL24_FINAL == 2L ~ "MEDIA",
      RPL24_FINAL == 1L ~ "BAJA",
      INC24_FINAL == 1L & FLAG_SER17 ~ "BAJA_SER17",
      INC24_FINAL == 1L & FLAG_DETJOV ~ "MEDIA_DETJOV",
      INC24_FINAL == 1L & FLAG_EXC_MJ ~ "MEDIA_EXCMJ",
      INC24_FINAL == 1L ~ "MEDIA",
      TRUE ~ NA_character_
    ),
    
    OBSC24_FINAL = case_when(
      RPL24_FINAL %in% c(1L, 2L, 3L) & CORTA_EXTRA17 ~ 11L,
      RPL24_FINAL %in% c(1L, 2L, 3L) ~ 10L,
      INC24_FINAL == 1L & FLAG_DETJOV ~ 20L,
      INC24_FINAL == 1L & FLAG_EXC_MJ ~ 30L,
      INC24_FINAL == 1L & FLAG_SER17 ~ 40L,
      INC24_FINAL == 1L ~ 50L,
      CORTA_2024 ~ 60L,
      CORTA_1723 & RPL24_FINAL == 0L ~ 70L,
      CORTA_1723 & RPL24_FINAL == 9L ~ 80L,
      TRUE ~ 99L
    )
  )


# ============================================================
# 10. TABLAS DE RESULTADOS Y CONTROLES
# ============================================================

tabla_resultado <- choperas_final %>%
  st_drop_geometry() %>%
  transmute(
    ID,
    REFCAT,
    nom_muni,
    SUP_TOTAL,
    EST_INT,
    CL_INI,
    CORTA_OK,
    SIN_CORT,
    EXC_MJ,
    SER_NOV17,
    DET_JOV,
    DET17COMP,
    F_CORTE,
    A_CORTE,
    M_CORTE,
    A_CORTF,
    M_CORTF,
    MOTIVO,
    
    NDVI_J24 = NDVI_J24_SRC,
    NDVI_S24 = NDVI_S24_SRC,
    VH_JS24 = VH_JS24_SRC,
    NPIX24 = NPIX24_SRC,
    NPIXV24 = NPIXV24_SRC,
    
    RPL24 = RPL24_FINAL,
    RPL_CONF = RPLCONF_FINAL,
    RPL_TXT = RPLTXT_FINAL,
    REP_AN24 = REPAN_FINAL,
    EDAD24 = EDAD_FINAL,
    ED_RPL24 = EDRPL_FINAL,
    INCED24 = INCED_FINAL,
    
    INC24 = INC24_FINAL,
    MOTC24 = MOTC24_FINAL,
    ED_MAP24 = EDMAP_FINAL,
    EDORC24 = EDORC_FINAL,
    FIAB24 = FIAB_FINAL,
    
    CAUT23_I = as.integer(!is.na(CAUT23_SRC) & CAUT23_SRC),
    SER17_I = as.integer(FLAG_SER17),
    DETJOV_I = as.integer(FLAG_DETJOV),
    DET17_I = as.integer(FLAG_DET17COMP),
    EXCMJ_I = as.integer(FLAG_EXC_MJ),
    EXTRA17_I = as.integer(CORTA_EXTRA17),
    EVALRPL_I = as.integer(EVAL_REPL),
    C1723_I = as.integer(CORTA_1723),
    C2024_I = as.integer(CORTA_2024),
    OBSC24 = OBSC24_FINAL
  )

write_csv2(tabla_resultado, csv_resultado)

control_general <- tabla_resultado %>%
  count(RPL24, RPL_CONF, RPL_TXT, name = "n_parcelas") %>%
  arrange(RPL24)

write_csv2(control_general, csv_control_general)

control_anio <- tabla_resultado %>%
  filter(C1723_I == 1 | C2024_I == 1) %>%
  count(A_CORTF, RPL24, RPL_CONF, RPL_TXT, name = "n_parcelas") %>%
  rename(A_CORTE_FINAL = A_CORTF) %>%
  arrange(A_CORTE_FINAL, RPL24)

write_csv2(control_anio, csv_control_anio)

control_superficie <- tabla_resultado %>%
  mutate(SUP_TOTAL = num_seguro(SUP_TOTAL)) %>%
  group_by(RPL24, RPL_CONF, RPL_TXT) %>%
  summarise(
    n_parcelas = n(),
    superficie_ha = sum(SUP_TOTAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(RPL24)

write_csv2(control_superficie, csv_control_superficie)

no_evaluables <- tabla_resultado %>%
  filter(RPL24 == 9) %>%
  select(
    ID,
    REFCAT,
    nom_muni,
    SUP_TOTAL,
    EST_INT,
    CL_INI,
    F_CORTE,
    A_CORTE,
    M_CORTE,
    A_CORTF,
    M_CORTF,
    RPL_TXT,
    C1723_I,
    C2024_I,
    EXTRA17_I
  ) %>%
  arrange(A_CORTF, ID)

write_csv2(no_evaluables, csv_no_evaluables)

control_carto24 <- tabla_resultado %>%
  count(INC24, MOTC24, ED_MAP24, FIAB24, name = "n_parcelas") %>%
  arrange(desc(INC24), MOTC24, ED_MAP24, FIAB24)

write_csv2(control_carto24, csv_control_carto24)

control_coherencia <- tabla_resultado %>%
  summarise(
    n_total = n(),
    n_eval_replantacion_csv = sum(EVALRPL_I == 1, na.rm = TRUE),
    n_cortas_1723 = sum(C1723_I == 1, na.rm = TRUE),
    n_cortas_2024 = sum(C2024_I == 1, na.rm = TRUE),
    n_cortas_extra17 = sum(EXTRA17_I == 1, na.rm = TRUE),
    n_det17comp = sum(DET17_I == 1, na.rm = TRUE),
    n_no_evaluables_total = sum(RPL24 == 9, na.rm = TRUE),
    n_no_evaluables_1723 = sum(RPL24 == 9 & C1723_I == 1, na.rm = TRUE),
    n_no_evaluables_2024 = sum(RPL24 == 9 & C2024_I == 1, na.rm = TRUE),
    n_incluidas_2024 = sum(INC24 == 1, na.rm = TRUE),
    n_excluidas_2024 = sum(INC24 == 0, na.rm = TRUE),
    n_replantadas_123 = sum(RPL24 %in% c(1, 2, 3), na.rm = TRUE),
    n_sin_evidencia = sum(RPL24 == 0, na.rm = TRUE),
    n_sin_corta_operativa = sum(RPL24 == -1, na.rm = TRUE)
  )

write_csv2(control_coherencia, csv_control_coherencia)


# ============================================================
# 11. CAPA COMPLETA
# ============================================================

campo_geom <- attr(choperas_base_limpia, "sf_column")
campos_originales <- setdiff(names(choperas_base_limpia), campo_geom)

shp_completo <- choperas_final %>%
  select(all_of(campos_originales)) %>%
  mutate(
    ACORTF = if_else(is.na(choperas_final$A_CORTF), 0L, as.integer(choperas_final$A_CORTF)),
    MCORTF = if_else(is.na(choperas_final$M_CORTF), 0L, as.integer(choperas_final$M_CORTF)),
    
    RPL24 = as.integer(choperas_final$RPL24_FINAL),
    RPLCONF = recortar_txt(choperas_final$RPLCONF_FINAL, 15),
    REPAN24 = if_else(is.na(choperas_final$REPAN_FINAL), 0L, as.integer(choperas_final$REPAN_FINAL)),
    EDAD24 = if_else(is.na(choperas_final$EDAD_FINAL), 0L, as.integer(choperas_final$EDAD_FINAL)),
    EDRPL24 = recortar_txt(coalesce(choperas_final$EDRPL_FINAL, ""), 20),
    INCED24 = recortar_txt(coalesce(choperas_final$INCED_FINAL, ""), 20),
    
    NDVIJ24 = round(num_seguro(choperas_final$NDVI_J24_SRC), 6),
    NDVIS24 = round(num_seguro(choperas_final$NDVI_S24_SRC), 6),
    VHJS24 = round(num_seguro(choperas_final$VH_JS24_SRC), 6),
    NPIX24 = if_else(is.na(choperas_final$NPIX24_SRC), 0L, as.integer(choperas_final$NPIX24_SRC)),
    NPIXV24 = if_else(is.na(choperas_final$NPIXV24_SRC), 0L, as.integer(choperas_final$NPIXV24_SRC)),
    
    INC24 = as.integer(choperas_final$INC24_FINAL),
    MOTC24 = as.integer(choperas_final$MOTC24_FINAL),
    EDMAP24 = recortar_txt(coalesce(choperas_final$EDMAP_FINAL, ""), 20),
    EDORC24 = if_else(is.na(choperas_final$EDORC_FINAL), 0L, as.integer(choperas_final$EDORC_FINAL)),
    FIAB24 = recortar_txt(coalesce(choperas_final$FIAB_FINAL, ""), 20),
    
    CAUT23I = as.integer(!is.na(choperas_final$CAUT23_SRC) & choperas_final$CAUT23_SRC),
    SER17I = as.integer(choperas_final$FLAG_SER17),
    DETJOVI = as.integer(choperas_final$FLAG_DETJOV),
    DET17I = as.integer(choperas_final$FLAG_DET17COMP),
    EXCMJI = as.integer(choperas_final$FLAG_EXC_MJ),
    EXTRA17I = as.integer(choperas_final$CORTA_EXTRA17),
    EVALRPLI = as.integer(choperas_final$EVAL_REPL),
    C1723I = as.integer(choperas_final$CORTA_1723),
    C2024I = as.integer(choperas_final$CORTA_2024),
    OBSC24 = as.integer(choperas_final$OBSC24_FINAL)
  ) %>%
  mutate(
    across(
      where(is.character),
      ~ recortar_txt(.x, 80)
    )
  )


# ============================================================
# 12. CARTOGRAFÍA DE CHOPERAS EN PIE EN 2024
# ============================================================

shp_carto24 <- shp_completo %>%
  filter(INC24 == 1)


# ============================================================
# 13. EXPORTACIÓN ESPACIAL
# ============================================================

borrar_shp(shp_salida_completo)
borrar_shp(shp_salida_carto24)

st_write(
  shp_completo,
  shp_salida_completo,
  driver = "ESRI Shapefile",
  layer_options = "ENCODING=UTF-8",
  quiet = TRUE
)

st_write(
  shp_carto24,
  shp_salida_carto24,
  driver = "ESRI Shapefile",
  layer_options = "ENCODING=UTF-8",
  quiet = TRUE
)


# ============================================================
# 14. MENSAJE FINAL
# ============================================================

message("Proceso finalizado correctamente.")
message("Capa completa exportada: ", shp_salida_completo)
message("Cartografía 2024 exportada: ", shp_salida_carto24)
message("Resultados exportados en: ", carpeta_salida)