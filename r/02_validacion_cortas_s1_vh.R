# ============================================================
# Validación de la detección de cortas mediante Sentinel-1 VH
#
# El procedimiento:
# 1. Extrae la mediana mensual de VH por parcela entre 2017 y 2024.
# 2. Identifica, para cada umbral, el primer mes en el que la mediana
#    mensual de VH es inferior al valor definido.
# 3. Compara la fecha detectada con la fecha de referencia de corta.
# 4. Resume los resultados para el conjunto completo de parcelas revisadas
#    y para el subconjunto con COD_OBS == 1.
# ============================================================

rm(list = ls())

# ============================================================
# 1. PAQUETES
# ============================================================

paquetes <- c("terra", "sf", "dplyr", "tidyr", "stringr", "lubridate")

paquetes_faltantes <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]

if (length(paquetes_faltantes) > 0) {
  stop(
    paste(
      "Faltan paquetes necesarios:",
      paste(paquetes_faltantes, collapse = ", ")
    )
  )
}

invisible(lapply(paquetes, library, character.only = TRUE))

terraOptions(progress = 1)


# ============================================================
# 2. RUTAS
# ============================================================

ruta_parcelas_2017 <- file.path(
  "data", "raw", "parcelas_cortas",
  "parcelas_corta_2017_revisado.shp"
)

ruta_parcelas_2018_2024 <- file.path(
  "data", "raw", "parcelas_cortas",
  "parcelas_corta_2018_2024_revisado.shp"
)

rutas_raster <- c(
  "2017" = file.path("data", "raw", "sentinel1_vh", "S1_VH_2017_UNIFICADO.tif"),
  "2018" = file.path("data", "raw", "sentinel1_vh", "S1_VH_2018_UNIFICADO.tif"),
  "2019" = file.path("data", "raw", "sentinel1_vh", "S1_VH_2019_UNIFICADO.tif"),
  "2020" = file.path("data", "raw", "sentinel1_vh", "S1_VH_2020_UNIFICADO.tif"),
  "2021" = file.path("data", "raw", "sentinel1_vh", "S1_VH_2021_UNIFICADO.tif"),
  "2022" = file.path("data", "raw", "sentinel1_vh", "S1_VH_2022_UNIFICADO.tif"),
  "2023" = file.path("data", "raw", "sentinel1_vh", "S1_VH_2023_UNIFICADO.tif"),
  "2024" = file.path("data", "raw", "sentinel1_vh", "S1_VH_2024_UNIFICADO.tif")
)

dir_revisadas <- file.path("outputs", "validacion_cortas", "parcelas_revisadas")
dir_validas <- file.path("outputs", "validacion_cortas", "parcelas_validas_cod1")

dir.create(dir_revisadas, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_validas, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 3. PARÁMETROS
# ============================================================

anios <- 2017:2024

umbrales <- round(seq(-19.0, -18.4, by = 0.1), 1)

meses <- sprintf("M%02d", 1:12)

escenarios_temporales <- dplyr::bind_rows(
  tibble::tibble(
    ESCENARIO_TEMPORAL = "VENTANA_TOTAL",
    VENTANA_DIAS = c(100, 120, 150, 180),
    TOLERANCIA_DIAS = c(50, 60, 75, 90)
  ),
  tibble::tibble(
    ESCENARIO_TEMPORAL = "TOLERANCIA_DIRECTA",
    VENTANA_DIAS = c(100, 120, 150, 180),
    TOLERANCIA_DIAS = c(100, 120, 150, 180)
  )
)


# ============================================================
# 4. FUNCIONES AUXILIARES
# ============================================================

campo_existente <- function(nombres, candidatos) {
  nombres_min <- tolower(nombres)
  candidatos_min <- tolower(candidatos)
  
  pos <- match(candidatos_min, nombres_min)
  pos <- pos[!is.na(pos)]
  
  if (length(pos) == 0) {
    return(NA_character_)
  }
  
  nombres[pos[1]]
}


parsear_fecha <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  
  if (inherits(x, "POSIXct") | inherits(x, "POSIXlt")) {
    return(as.Date(x))
  }
  
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  
  salida <- rep(as.Date(NA), length(x_chr))
  
  x_num <- suppressWarnings(as.numeric(x_chr))
  idx_excel <- !is.na(x_num) &
    x_num > 20000 &
    x_num < 60000 &
    !grepl("[/-]", x_chr)
  
  if (any(idx_excel)) {
    salida[idx_excel] <- as.Date(x_num[idx_excel], origin = "1899-12-30")
  }
  
  idx_texto <- is.na(salida) & !is.na(x_chr)
  
  if (any(idx_texto)) {
    fecha_parseada <- suppressWarnings(
      lubridate::parse_date_time(
        x_chr[idx_texto],
        orders = c(
          "dmy HMS", "dmy HM", "dmy",
          "ymd HMS", "ymd HM", "ymd",
          "Ymd HMS", "Ymd HM", "Ymd",
          "dmY HMS", "dmY HM", "dmY"
        ),
        tz = "UTC",
        truncated = 3
      )
    )
    
    salida[idx_texto] <- as.Date(fecha_parseada)
  }
  
  salida
}


obtener_fecha_desde_campos <- function(sf_obj, candidatos) {
  fecha_final <- rep(as.Date(NA), nrow(sf_obj))
  
  for (candidato in candidatos) {
    campo <- campo_existente(names(sf_obj), candidato)
    
    if (!is.na(campo)) {
      fecha_campo <- parsear_fecha(sf_obj[[campo]])
      idx <- is.na(fecha_final) & !is.na(fecha_campo)
      fecha_final[idx] <- fecha_campo[idx]
    }
  }
  
  fecha_final
}


obtener_columna <- function(sf_obj, candidatos, tipo = "character") {
  campo <- campo_existente(names(sf_obj), candidatos)
  
  if (is.na(campo)) {
    if (tipo == "numeric") {
      return(rep(NA_real_, nrow(sf_obj)))
    }
    
    if (tipo == "integer") {
      return(rep(NA_integer_, nrow(sf_obj)))
    }
    
    return(rep(NA_character_, nrow(sf_obj)))
  }
  
  x <- sf_obj[[campo]]
  
  if (tipo == "numeric") {
    return(suppressWarnings(as.numeric(x)))
  }
  
  if (tipo == "integer") {
    return(suppressWarnings(as.integer(x)))
  }
  
  as.character(x)
}


escribir_csv <- function(tabla, ruta) {
  write.table(
    tabla,
    file = ruta,
    sep = ";",
    dec = ",",
    row.names = FALSE,
    col.names = TRUE,
    fileEncoding = "UTF-8"
  )
}


normalizar_parcelas <- function(sf_obj, fuente) {
  sf_obj <- sf::st_make_valid(sf_obj)
  
  fecha_corta <- obtener_fecha_desde_campos(
    sf_obj,
    c(
      "FECHA_CORT",
      "CORTA_OJ",
      "CORTA_OK",
      "FECHA_COR",
      "FECHA_CORTA",
      "F_CORTA",
      "CERT_CORTA"
    )
  )
  
  anio_campo <- obtener_columna(
    sf_obj,
    c("CORTA", "ANIO_CORTA", "ANO_CORTA", "AÑO_CORTA"),
    tipo = "integer"
  )
  
  anio_fecha <- lubridate::year(fecha_corta)
  
  anio_corta <- anio_campo
  idx <- is.na(anio_corta) & !is.na(anio_fecha)
  anio_corta[idx] <- anio_fecha[idx]
  
  refcat <- obtener_columna(
    sf_obj,
    c("REFCAT", "REF_CAT", "REFERENCIA"),
    tipo = "character"
  )
  
  cod_obs <- obtener_columna(
    sf_obj,
    c("COD_OBS", "COD_OBSERV", "CODIGO_OBS"),
    tipo = "integer"
  )
  
  observacion <- obtener_columna(
    sf_obj,
    c("OBSERVACIO", "OBSERVACION", "OBSERV"),
    tipo = "character"
  )
  
  area_m2 <- obtener_columna(
    sf_obj,
    c("Shape_Area", "SHAPE_Area", "SHAPE_AREA", "AREA", "Area"),
    tipo = "numeric"
  )
  
  idx_area <- is.na(area_m2)
  
  if (any(idx_area)) {
    area_m2[idx_area] <- as.numeric(sf::st_area(sf_obj))[idx_area]
  }
  
  municipio <- obtener_columna(
    sf_obj,
    c("MUNICIPIO", "nom_muni", "NOM_MUNI"),
    tipo = "character"
  )
  
  masa <- obtener_columna(
    sf_obj,
    c("MASA"),
    tipo = "character"
  )
  
  parcela <- obtener_columna(
    sf_obj,
    c("PARCELA"),
    tipo = "character"
  )
  
  salida <- data.frame(
    ID_ORIG_SHP = seq_len(nrow(sf_obj)),
    FUENTE_SHP = fuente,
    REFCAT_TFM = refcat,
    MUNICIPIO_TFM = municipio,
    MASA_TFM = masa,
    PARCELA_TFM = parcela,
    ANIO_CORTA = anio_corta,
    FECHA_CORTA = fecha_corta,
    COD_OBS_TFM = cod_obs,
    AREA_M2_TFM = area_m2,
    OBSERVACION_TFM = observacion,
    stringsAsFactors = FALSE
  )
  
  sf::st_as_sf(
    salida,
    geometry = sf::st_geometry(sf_obj),
    crs = sf::st_crs(sf_obj)
  )
}


extraer_mediana_anual <- function(anio, ruta_raster, parcelas_sf) {
  r <- terra::rast(ruta_raster)
  
  if (terra::nlyr(r) != 12) {
    stop(
      paste0(
        "El raster de ", anio,
        " debe tener 12 bandas. Bandas detectadas: ",
        terra::nlyr(r), "."
      )
    )
  }
  
  names(r) <- meses
  
  v <- terra::vect(parcelas_sf)
  
  if (!terra::same.crs(v, r)) {
    v <- terra::project(v, terra::crs(r))
  }
  
  extraccion <- terra::extract(
    x = r,
    y = v,
    fun = median,
    na.rm = TRUE,
    ID = TRUE,
    touches = FALSE,
    small = TRUE
  )
  
  names(extraccion)[1] <- "ID_EXTRACT"
  extraccion$ID <- parcelas_sf$ID[extraccion$ID_EXTRACT]
  
  diagnostico <- extraccion %>%
    mutate(
      CON_ALGUN_VALOR = rowSums(!is.na(across(all_of(meses)))) > 0
    ) %>%
    summarise(
      ANIO_SERIE = anio,
      PARCELAS_EXTRAIDAS = n(),
      PARCELAS_CON_ALGUN_VALOR = sum(CON_ALGUN_VALOR, na.rm = TRUE),
      PARCELAS_SIN_VALORES = sum(!CON_ALGUN_VALOR, na.rm = TRUE)
    )
  
  tabla_larga <- extraccion %>%
    select(ID, all_of(meses)) %>%
    tidyr::pivot_longer(
      cols = all_of(meses),
      names_to = "MES_COL",
      values_to = "MEDIANA_VH"
    ) %>%
    mutate(
      ANIO_SERIE = anio,
      MES_NUM = as.integer(stringr::str_remove(MES_COL, "M")),
      FECHA_MES = as.Date(sprintf("%s-%02d-01", anio, MES_NUM))
    ) %>%
    select(ID, ANIO_SERIE, MES_NUM, FECHA_MES, MEDIANA_VH)
  
  list(tabla = tabla_larga, diagnostico = diagnostico)
}


validar_conjunto <- function(tabla_medianas,
                             atributos,
                             umbrales,
                             escenarios_temporales,
                             solo_validas = FALSE) {
  if (solo_validas) {
    atributos_eval <- atributos %>%
      dplyr::filter(COD_OBS_TFM == 1)
    
    etiqueta <- "VALIDAS_COD1"
  } else {
    atributos_eval <- atributos
    etiqueta <- "REVISADAS"
  }
  
  atributos_eval <- atributos_eval %>%
    dplyr::filter(!is.na(FECHA_CORTA), !is.na(ANIO_CORTA))
  
  tabla_eval <- tabla_medianas %>%
    dplyr::inner_join(atributos_eval, by = "ID")
  
  diagnostico_validacion <- atributos_eval %>%
    dplyr::group_by(ANIO_CORTA) %>%
    dplyr::summarise(
      PARCELAS_REFERENCIA = dplyr::n(),
      PARCELAS_VALIDAS_COD1 = sum(COD_OBS_TFM == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(ANIO_CORTA)
  
  resultados_lista <- list()
  contador <- 1
  
  for (umbral_i in umbrales) {
    primera_deteccion <- tabla_eval %>%
      dplyr::filter(!is.na(MEDIANA_VH)) %>%
      dplyr::filter(MEDIANA_VH < umbral_i) %>%
      dplyr::arrange(ID, FECHA_MES) %>%
      dplyr::group_by(ID) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        ID,
        UMBRAL = umbral_i,
        FECHA_DETECTADA = FECHA_MES,
        ANIO_DETECTADO = ANIO_SERIE,
        MES_DETECTADO = MES_NUM,
        MEDIANA_VH_DETECCION = MEDIANA_VH
      )
    
    base_umbral <- atributos_eval %>%
      dplyr::left_join(primera_deteccion, by = "ID") %>%
      dplyr::mutate(
        UMBRAL = umbral_i,
        DIF_DIAS = as.numeric(FECHA_DETECTADA - FECHA_CORTA),
        DIF_DIAS_ABS = abs(DIF_DIAS)
      )
    
    for (i in seq_len(nrow(escenarios_temporales))) {
      escenario_i <- escenarios_temporales$ESCENARIO_TEMPORAL[i]
      ventana_i <- escenarios_temporales$VENTANA_DIAS[i]
      tolerancia_i <- escenarios_temporales$TOLERANCIA_DIAS[i]
      
      resultado_i <- base_umbral %>%
        dplyr::mutate(
          ESCENARIO_TEMPORAL = escenario_i,
          VENTANA_DIAS = ventana_i,
          TOLERANCIA_DIAS = tolerancia_i,
          FECHA_INICIO_VENTANA = FECHA_CORTA - tolerancia_i,
          FECHA_FIN_VENTANA = FECHA_CORTA + tolerancia_i,
          CATEGORIA = dplyr::case_when(
            is.na(FECHA_DETECTADA) ~ "NO_DETECTADA",
            DIF_DIAS_ABS <= tolerancia_i ~ "ACIERTO",
            DIF_DIAS_ABS > tolerancia_i ~ "FUERA_MARGEN",
            TRUE ~ "SIN_CLASIFICAR"
          ),
          ACIERTO = ifelse(CATEGORIA == "ACIERTO", 1, 0),
          CONJUNTO = etiqueta
        ) %>%
        dplyr::select(
          CONJUNTO,
          ID,
          ID_ORIG_SHP,
          FUENTE_SHP,
          REFCAT_TFM,
          MUNICIPIO_TFM,
          MASA_TFM,
          PARCELA_TFM,
          ANIO_CORTA,
          FECHA_CORTA,
          COD_OBS_TFM,
          AREA_M2_TFM,
          OBSERVACION_TFM,
          UMBRAL,
          ESCENARIO_TEMPORAL,
          VENTANA_DIAS,
          TOLERANCIA_DIAS,
          FECHA_INICIO_VENTANA,
          FECHA_FIN_VENTANA,
          FECHA_DETECTADA,
          ANIO_DETECTADO,
          MES_DETECTADO,
          MEDIANA_VH_DETECCION,
          DIF_DIAS,
          DIF_DIAS_ABS,
          CATEGORIA,
          ACIERTO
        )
      
      resultados_lista[[contador]] <- resultado_i
      contador <- contador + 1
    }
  }
  
  resultados_parcela <- dplyr::bind_rows(resultados_lista) %>%
    dplyr::arrange(
      CONJUNTO,
      ESCENARIO_TEMPORAL,
      UMBRAL,
      VENTANA_DIAS,
      ANIO_CORTA,
      ID
    )
  
  resumen_global <- resultados_parcela %>%
    dplyr::group_by(
      CONJUNTO,
      ESCENARIO_TEMPORAL,
      UMBRAL,
      VENTANA_DIAS,
      TOLERANCIA_DIAS
    ) %>%
    dplyr::summarise(
      TOTAL = dplyr::n(),
      ACIERTOS = sum(CATEGORIA == "ACIERTO", na.rm = TRUE),
      FUERA_MARGEN = sum(CATEGORIA == "FUERA_MARGEN", na.rm = TRUE),
      NO_DETECTADAS = sum(CATEGORIA == "NO_DETECTADA", na.rm = TRUE),
      PRECISION = round(ACIERTOS * 100 / TOTAL, 2),
      .groups = "drop"
    ) %>%
    dplyr::arrange(ESCENARIO_TEMPORAL, UMBRAL, VENTANA_DIAS)
  
  resumen_anual <- resultados_parcela %>%
    dplyr::group_by(
      CONJUNTO,
      ESCENARIO_TEMPORAL,
      ANIO_CORTA,
      UMBRAL,
      VENTANA_DIAS,
      TOLERANCIA_DIAS
    ) %>%
    dplyr::summarise(
      TOTAL = dplyr::n(),
      ACIERTOS = sum(CATEGORIA == "ACIERTO", na.rm = TRUE),
      FUERA_MARGEN = sum(CATEGORIA == "FUERA_MARGEN", na.rm = TRUE),
      NO_DETECTADAS = sum(CATEGORIA == "NO_DETECTADA", na.rm = TRUE),
      PRECISION = round(ACIERTOS * 100 / TOTAL, 2),
      .groups = "drop"
    ) %>%
    dplyr::arrange(ESCENARIO_TEMPORAL, ANIO_CORTA, UMBRAL, VENTANA_DIAS)
  
  matriz_global_precision <- resumen_global %>%
    dplyr::mutate(
      COLUMNA = paste0(
        ESCENARIO_TEMPORAL,
        "_",
        VENTANA_DIAS,
        "_DIAS_PRECISION"
      )
    ) %>%
    dplyr::select(UMBRAL, COLUMNA, PRECISION) %>%
    tidyr::pivot_wider(
      names_from = COLUMNA,
      values_from = PRECISION
    ) %>%
    dplyr::arrange(UMBRAL)
  
  matriz_anual_precision <- resumen_anual %>%
    dplyr::mutate(
      COLUMNA = paste0(
        ESCENARIO_TEMPORAL,
        "_",
        VENTANA_DIAS,
        "_DIAS_PRECISION"
      )
    ) %>%
    dplyr::select(ANIO_CORTA, UMBRAL, COLUMNA, PRECISION) %>%
    tidyr::pivot_wider(
      names_from = COLUMNA,
      values_from = PRECISION
    ) %>%
    dplyr::arrange(ANIO_CORTA, UMBRAL)
  
  list(
    diagnostico_validacion = diagnostico_validacion,
    resultados_parcela = resultados_parcela,
    resumen_global = resumen_global,
    resumen_anual = resumen_anual,
    matriz_global_precision = matriz_global_precision,
    matriz_anual_precision = matriz_anual_precision
  )
}


# ============================================================
# 5. LECTURA Y NORMALIZACIÓN DE PARCELAS
# ============================================================

parcelas_2017_raw <- sf::st_read(ruta_parcelas_2017, quiet = TRUE)
parcelas_2018_2024_raw <- sf::st_read(ruta_parcelas_2018_2024, quiet = TRUE)

parcelas_2017 <- normalizar_parcelas(parcelas_2017_raw, "2017")
parcelas_2018_2024 <- normalizar_parcelas(parcelas_2018_2024_raw, "2018_2024")

rm(parcelas_2017_raw, parcelas_2018_2024_raw)

if (!is.na(sf::st_crs(parcelas_2017)) && !is.na(sf::st_crs(parcelas_2018_2024))) {
  parcelas_2018_2024 <- sf::st_transform(
    parcelas_2018_2024,
    sf::st_crs(parcelas_2017)
  )
}

parcelas <- dplyr::bind_rows(parcelas_2017, parcelas_2018_2024)
parcelas <- sf::st_make_valid(parcelas)
parcelas$ID <- seq_len(nrow(parcelas))


# ============================================================
# 6. DIAGNÓSTICO INICIAL DE PARCELAS
# ============================================================

diagnostico_parcelas <- parcelas %>%
  sf::st_drop_geometry() %>%
  group_by(FUENTE_SHP, ANIO_CORTA) %>%
  summarise(
    PARCELAS_REVISADAS = n(),
    PARCELAS_VALIDAS_COD1 = sum(COD_OBS_TFM == 1, na.rm = TRUE),
    PARCELAS_CON_FECHA = sum(!is.na(FECHA_CORTA)),
    PARCELAS_SIN_FECHA = sum(is.na(FECHA_CORTA)),
    .groups = "drop"
  ) %>%
  arrange(ANIO_CORTA, FUENTE_SHP)

escribir_csv(
  diagnostico_parcelas,
  file.path(dir_revisadas, "00_diagnostico_inicial_parcelas.csv")
)

parcelas_fecha_na <- parcelas %>%
  sf::st_drop_geometry() %>%
  filter(is.na(FECHA_CORTA))

if (nrow(parcelas_fecha_na) > 0) {
  escribir_csv(
    parcelas_fecha_na,
    file.path(dir_revisadas, "00_parcelas_con_fecha_corta_na.csv")
  )
}


# ============================================================
# 7. EXTRACCIÓN DE MEDIANAS MENSUALES VH
# ============================================================

lista_extracciones <- list()
lista_diagnosticos <- list()

for (anio in anios) {
  ruta_r <- rutas_raster[as.character(anio)]
  
  if (!file.exists(ruta_r)) {
    stop(paste0("No existe el raster del año ", anio, ": ", ruta_r))
  }
  
  res <- extraer_mediana_anual(anio, ruta_r, parcelas)
  
  lista_extracciones[[as.character(anio)]] <- res$tabla
  lista_diagnosticos[[as.character(anio)]] <- res$diagnostico
}

tabla_medianas <- dplyr::bind_rows(lista_extracciones)
diagnostico_extraccion <- dplyr::bind_rows(lista_diagnosticos)

escribir_csv(
  diagnostico_extraccion,
  file.path(dir_revisadas, "01_diagnostico_extraccion_por_anio.csv")
)

atributos_parcelas <- parcelas %>%
  sf::st_drop_geometry() %>%
  select(
    ID,
    ID_ORIG_SHP,
    FUENTE_SHP,
    REFCAT_TFM,
    MUNICIPIO_TFM,
    MASA_TFM,
    PARCELA_TFM,
    ANIO_CORTA,
    FECHA_CORTA,
    COD_OBS_TFM,
    AREA_M2_TFM,
    OBSERVACION_TFM
  )

tabla_medianas_atrib <- tabla_medianas %>%
  left_join(atributos_parcelas, by = "ID") %>%
  arrange(ID, FECHA_MES)

escribir_csv(
  tabla_medianas_atrib,
  file.path(dir_revisadas, "02_medianas_mensuales_vh_parcelas_revisadas.csv")
)

tabla_medianas_validas <- tabla_medianas_atrib %>%
  filter(COD_OBS_TFM == 1)

escribir_csv(
  tabla_medianas_validas,
  file.path(dir_validas, "02_medianas_mensuales_vh_parcelas_validas_cod1.csv")
)


# ============================================================
# 8. VALIDACIÓN DE DETECCIONES
# ============================================================

res_revisadas <- validar_conjunto(
  tabla_medianas = tabla_medianas,
  atributos = atributos_parcelas,
  umbrales = umbrales,
  escenarios_temporales = escenarios_temporales,
  solo_validas = FALSE
)

escribir_csv(
  res_revisadas$diagnostico_validacion,
  file.path(dir_revisadas, "03_diagnostico_validacion_revisadas.csv")
)

escribir_csv(
  res_revisadas$resultados_parcela,
  file.path(dir_revisadas, "04_resultados_por_parcela_primera_deteccion_revisadas.csv")
)

escribir_csv(
  res_revisadas$resumen_global,
  file.path(dir_revisadas, "05_resumen_global_primera_deteccion_revisadas.csv")
)

escribir_csv(
  res_revisadas$resumen_anual,
  file.path(dir_revisadas, "06_resumen_anual_primera_deteccion_revisadas.csv")
)

escribir_csv(
  res_revisadas$matriz_global_precision,
  file.path(dir_revisadas, "07_matriz_precision_global_revisadas.csv")
)

escribir_csv(
  res_revisadas$matriz_anual_precision,
  file.path(dir_revisadas, "08_matriz_precision_anual_revisadas.csv")
)


# ============================================================
# 9. VALIDACIÓN DEL SUBCONJUNTO COD_OBS == 1
# ============================================================

res_validas <- validar_conjunto(
  tabla_medianas = tabla_medianas,
  atributos = atributos_parcelas,
  umbrales = umbrales,
  escenarios_temporales = escenarios_temporales,
  solo_validas = TRUE
)

escribir_csv(
  res_validas$diagnostico_validacion,
  file.path(dir_validas, "03_diagnostico_validacion_validas_cod1.csv")
)

escribir_csv(
  res_validas$resultados_parcela,
  file.path(dir_validas, "04_resultados_por_parcela_primera_deteccion_validas_cod1.csv")
)

escribir_csv(
  res_validas$resumen_global,
  file.path(dir_validas, "05_resumen_global_primera_deteccion_validas_cod1.csv")
)

escribir_csv(
  res_validas$resumen_anual,
  file.path(dir_validas, "06_resumen_anual_primera_deteccion_validas_cod1.csv")
)

escribir_csv(
  res_validas$matriz_global_precision,
  file.path(dir_validas, "07_matriz_precision_global_validas_cod1.csv")
)

escribir_csv(
  res_validas$matriz_anual_precision,
  file.path(dir_validas, "08_matriz_precision_anual_validas_cod1.csv")
)


# ============================================================
# 10. RESÚMENES COMBINADOS
# ============================================================

resumen_global_combinado <- dplyr::bind_rows(
  res_revisadas$resumen_global,
  res_validas$resumen_global
)

resumen_anual_combinado <- dplyr::bind_rows(
  res_revisadas$resumen_anual,
  res_validas$resumen_anual
)

escribir_csv(
  resumen_global_combinado,
  file.path(dir_revisadas, "09_resumen_global_combinado_revisadas_y_validas.csv")
)

escribir_csv(
  resumen_anual_combinado,
  file.path(dir_revisadas, "10_resumen_anual_combinado_revisadas_y_validas.csv")
)


# ============================================================
# 11. CONFIGURACIONES CON MAYOR PRECISIÓN
# ============================================================

mejores_globales <- resumen_global_combinado %>%
  dplyr::arrange(
    CONJUNTO,
    ESCENARIO_TEMPORAL,
    dplyr::desc(PRECISION),
    dplyr::desc(ACIERTOS),
    UMBRAL,
    VENTANA_DIAS
  ) %>%
  dplyr::group_by(CONJUNTO, ESCENARIO_TEMPORAL) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

mejores_anuales <- resumen_anual_combinado %>%
  dplyr::arrange(
    CONJUNTO,
    ESCENARIO_TEMPORAL,
    ANIO_CORTA,
    dplyr::desc(PRECISION),
    dplyr::desc(ACIERTOS),
    UMBRAL,
    VENTANA_DIAS
  ) %>%
  dplyr::group_by(CONJUNTO, ESCENARIO_TEMPORAL, ANIO_CORTA) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

escribir_csv(
  mejores_globales,
  file.path(dir_revisadas, "11_mejores_configuraciones_globales.csv")
)

escribir_csv(
  mejores_anuales,
  file.path(dir_revisadas, "12_mejores_configuraciones_anuales.csv")
)


# ============================================================
# 12. MENSAJE FINAL
# ============================================================

cat("\nProceso finalizado correctamente.\n")
cat("Resultados exportados en:\n")
cat(" - ", dir_revisadas, "\n", sep = "")
cat(" - ", dir_validas, "\n", sep = "")