# Código esencial del TFM: cartografía de choperas y detección de cortas

Repositorio con los scripts principales utilizados para generar la cartografía de choperas de 2017, analizar la serie Sentinel-1 VH para la detección de cortas, evaluar replantaciones y obtener la cartografía de choperas en pie en 2024 en la cuenca del río Esla.

No se incluyen datos brutos ni capas de entrada. Los scripts requieren los productos generados en Google Earth Engine, capas vectoriales de trabajo y tablas intermedias descritas en la memoria del TFM.

## Estructura

```text
TFM_choperas_codigo/
├── gee/
│   ├── 01_clasificacion_choperas_2017.js
│   ├── 02_extraccion_ndvi_vh_choperas_2017.js
│   ├── 03_extraccion_ndvi_vh_replantacion_2024.js
│   └── 04_sentinel1_vh_mensual_2018.js
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

## Secuencia general

1. `gee/01_clasificacion_choperas_2017.js`: clasificación de choperas en 2017 a partir de Sentinel-2, Sentinel-1 y máscaras auxiliares.
2. `gee/04_sentinel1_vh_mensual_2018.js`: ejemplo de generación anual de imágenes mensuales Sentinel-1 VH. El procedimiento se replicó por año ajustando fechas y nombres de salida.
3. `r/01_vh_mensual_cortas_2024.R`: extracción y análisis exploratorio de VH mensual por parcela. El ejemplo corresponde a 2024 y el procedimiento se aplicó de forma análoga a otros años.
4. `r/02_validacion_cortas_s1_vh.R`: validación de la detección de cortas con parcelas de referencia y evaluación de umbrales/ventanas temporales.
5. `r/03_deteccion_cortas_s1_vh_2017_2024.R`: detección e interpretación de cortas en la cartografía base de choperas de 2017.
6. `gee/02_extraccion_ndvi_vh_choperas_2017.js`: extracción de NDVI y VH en muestras de choperas de 2017 para el análisis exploratorio de umbrales.
7. `r/04_estadisticos_ndvi_vh_choperas_2017.R`: cálculo de estadísticos de NDVI y VH sobre las muestras de 2017.
8. `gee/03_extraccion_ndvi_vh_replantacion_2024.js`: extracción de NDVI y VH en 2024 para parcelas con corta detectada entre 2017 y 2023.
9. `r/05_resumen_parcelario_replantacion_2024.R`: resumen de valores de píxel a escala de parcela.
10. `r/06_replantacion_cartografia_2024.R`: aplicación de criterios de replantación y generación de la cartografía de choperas en pie en 2024.

## Notas de uso

Los scripts de Google Earth Engine requieren definir previamente las capas de entrada en el panel de imports. Los scripts de R emplean rutas relativas (`data/`, `outputs/`) para evitar rutas locales personales. La estructura concreta de datos debe adaptarse a la organización local de cada usuario.

Los códigos incluidos son los esenciales para documentar el flujo de trabajo; no constituyen un paquete completamente reproducible sin los datos de entrada asociados.
