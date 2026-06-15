# Principales scripts del TFM: cartografía de plantaciones de *Populus* spp. mediante imágenes Sentinel-2 y Sentinel-1 en la cuenca del río Esla (León)

Repositorio con los scripts principales utilizados en el Trabajo Fin de Máster para la cartografía de choperas en la cuenca del río Esla, la detección de cortas entre 2017 y 2024, la evaluación de replantaciones y la obtención de la cartografía de choperas en pie en 2024.

No se incluyen datos brutos, capas vectoriales de entrada ni resultados intermedios. Los scripts requieren las capas de trabajo, los productos exportados desde Google Earth Engine y las tablas intermedias descritas en la memoria del TFM.

## Estructura

```text
tfm_choperas_esla_2017_2024/
├── gee/
│   ├── 01_clasificacion_choperas_2017.js
│   ├── 02_sentinel1_vh_mensual_2018.js
│   ├── 03_extraccion_ndvi_vh_choperas_2017.js
│   └── 04_extraccion_ndvi_vh_replantacion_2024.js
├── r/
│   ├── 01_vh_mensual_cortas_2024.R
│   ├── 02_validacion_cortas_s1_vh.R
│   ├── 03_deteccion_cortas_s1_vh_2017_2024.R
│   ├── 04_estadisticos_ndvi_vh_choperas_2017.R
│   ├── 05_resumen_parcelario_replantacion_2024.R
│   └── 06_replantacion_cartografia_2024.R
├── README.md
└── .gitignore
```

## Secuencia de trabajo

1. `gee/01_clasificacion_choperas_2017.js`
   Clasificación inicial de choperas en 2017 a partir de Sentinel-2, Sentinel-1 y máscaras auxiliares.

2. `gee/02_sentinel1_vh_mensual_2018.js`
   Generación de imágenes mensuales Sentinel-1 VH. El script muestra el caso de 2018 y el procedimiento se aplicó de forma análoga al resto de años analizados.

3. `r/01_vh_mensual_cortas_2024.R`
   Extracción y análisis exploratorio de valores VH mensuales por parcela. El script corresponde a 2024 y se aplicó de forma análoga a otros años.

4. `r/02_validacion_cortas_s1_vh.R`
   Validación de la detección de cortas mediante parcelas de referencia y comparación de umbrales y ventanas temporales.

5. `r/03_deteccion_cortas_s1_vh_2017_2024.R`
   Identificación e interpretación de cortas en la cartografía base de choperas de 2017.

6. `gee/03_extraccion_ndvi_vh_choperas_2017.js`
   Extracción de valores NDVI y VH en muestras de choperas de 2017 para el análisis exploratorio de criterios de replantación.

7. `r/04_estadisticos_ndvi_vh_choperas_2017.R`
   Cálculo de estadísticos de NDVI y VH sobre las muestras de 2017.

8. `gee/04_extraccion_ndvi_vh_replantacion_2024.js`
   Extracción de NDVI y VH en 2024 para parcelas con corta detectada entre 2017 y 2023.

9. `r/05_resumen_parcelario_replantacion_2024.R`
   Agregación de valores de píxel a escala de parcela.

10. `r/06_replantacion_cartografia_2024.R`
    Aplicación de los criterios de replantación y generación de la cartografía de choperas en pie en 2024.

## Nota de uso

Los scripts de Google Earth Engine requieren definir previamente las capas de entrada en el panel de imports. Los scripts de R trabajan con rutas relativas (`data/` y `outputs/`) para evitar rutas locales personales.

El repositorio documenta los códigos esenciales del flujo metodológico. La ejecución completa requiere disponer de las capas de entrada y de los productos intermedios descritos en la memoria del TFM.
