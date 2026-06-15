// ======================================================
// Extracción de valores NDVI y VH en muestras de choperas
// Año 2017 - Google Earth Engine
// ======================================================

// ======================================================
// 1. VARIABLES DE ENTRADA
// ======================================================

// Variables definidas en el panel de imports de GEE:
//   AESTUDIO
//   ENTRENAMIENTO
//   VALIDACION

var zona = AESTUDIO;
var areasEntrenamiento = ENTRENAMIENTO;
var puntosValidacion = VALIDACION;

var carpetaExportacion = 'TFM_ING_MONTES';

// ======================================================
// 2. PARÁMETROS TEMPORALES
// ======================================================

var cloudPerc = 20;

var fechaS2JunIni = '2017-06-15';
var fechaS2JunFin = '2017-06-30';

var fechaS2SepIni = '2017-09-01';
var fechaS2SepFin = '2017-09-20';

var fechaS1Ini = '2017-06-01';
var fechaS1Fin = '2017-09-16';

// ======================================================
// 3. FUNCIONES AUXILIARES
// ======================================================

function maskS2clouds(image) {
var qa = image.select('QA60');

var cloudBitMask = 1 << 10;
var cirrusBitMask = 1 << 11;

var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
.and(qa.bitwiseAnd(cirrusBitMask).eq(0));

return image
.updateMask(mask)
.divide(10000)
.copyProperties(image, ['system:time_start']);
}

function addNDVI(image) {
var ndvi = image
.normalizedDifference(['B8', 'B4'])
.rename('NDVI');

return image.addBands(ndvi);
}

function cleanS1(image) {
var edge = image.lt(-30.0);
var mask = image.mask().and(edge.not());
return image.updateMask(mask);
}

function prepararMuestras(fc, fuente) {
return fc.map(function(f) {
var props = f.propertyNames();

```
var idn = ee.Algorithms.If(
  props.contains('IDn'),
  f.get('IDn'),
  null
);

var idn2 = ee.Algorithms.If(
  props.contains('IDn2'),
  f.get('IDn2'),
  null
);

var clase = ee.Algorithms.If(
  props.contains('IDn'),
  f.get('IDn'),
  f.get('IDn2')
);

clase = ee.Number(clase);

var claseTxt = ee.Algorithms.If(
  clase.eq(1),
  'chopera_adulta',
  ee.Algorithms.If(
    clase.eq(2),
    'chopera_joven',
    'otra_clase'
  )
);

return f.set({
  'fuente': fuente,
  'IDn_export': idn,
  'IDn2_export': idn2,
  'clase_ref': clase,
  'clase_txt': claseTxt
});
```

})
.filter(ee.Filter.inList('clase_ref', [1, 2]));
}

// ======================================================
// 4. SENTINEL-2: NDVI POR PERIODO
// ======================================================

function getNDVIperiodo(startDate, endDate, nombreBanda) {
var col = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
.filterBounds(zona)
.filterDate(startDate, endDate)
.filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', cloudPerc))
.map(maskS2clouds)
.select(['B4', 'B8'])
.map(addNDVI)
.select('NDVI');

print('Nº imágenes Sentinel-2 para ' + nombreBanda, col.size());

return col
.median()
.rename(nombreBanda)
.clip(zona);
}

var ndviJun2017 = getNDVIperiodo(
fechaS2JunIni,
fechaS2JunFin,
'NDVI_JUN2017'
);

var ndviSep2017 = getNDVIperiodo(
fechaS2SepIni,
fechaS2SepFin,
'NDVI_SEP2017'
);

// ======================================================
// 5. SENTINEL-1: VH MEDIANA JUNIO-SEPTIEMBRE 2017
// ======================================================

var s1col = ee.ImageCollection('COPERNICUS/S1_GRD')
.filterBounds(zona)
.filterDate(fechaS1Ini, fechaS1Fin)
.filter(ee.Filter.eq('instrumentMode', 'IW'))
.filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
.filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
.select(['VV', 'VH'])
.map(cleanS1);

print('Nº imágenes Sentinel-1 junio-septiembre 2017', s1col.size());

var vhJunSep2017 = s1col
.median()
.select('VH')
.rename('VH_JUNSEP2017')
.clip(zona);

// ======================================================
// 6. STACK FINAL DE VARIABLES
// ======================================================

var stack2017 = ee.Image.cat([
ndviJun2017,
ndviSep2017,
vhJunSep2017,
ee.Image.pixelLonLat()
]).clip(zona);

// ======================================================
// 7. PREPARACIÓN DE MUESTRAS
// ======================================================

var entrenamientoPrep = prepararMuestras(
areasEntrenamiento,
'areas_entrenamiento'
);

var validacionPrep = prepararMuestras(
puntosValidacion,
'puntos_validacion'
);

print('Entidades entrenamiento clase 1-2', entrenamientoPrep.size());
print('Entidades validación clase 1-2', validacionPrep.size());

// ======================================================
// 8. EXTRACCIÓN DE VALORES DE PÍXEL
// ======================================================

var propiedadesExportar = [
'fuente',
'clase_ref',
'clase_txt',
'IDn_export',
'IDn2_export'
];

var valoresEntrenamiento = stack2017.sampleRegions({
collection: entrenamientoPrep,
properties: propiedadesExportar,
scale: 10,
tileScale: 4,
geometries: false
});

var valoresValidacion = stack2017.sampleRegions({
collection: validacionPrep,
properties: propiedadesExportar,
scale: 10,
tileScale: 4,
geometries: false
});

var valoresTotales = valoresEntrenamiento.merge(valoresValidacion);

print('Nº valores de píxel entrenamiento', valoresEntrenamiento.size());
print('Nº valores de píxel validación', valoresValidacion.size());
print('Nº valores totales exportables', valoresTotales.size());

// ======================================================
// 9. VISUALIZACIÓN
// ======================================================

var visNDVI = {
min: 0,
max: 1,
palette: ['brown', 'yellow', 'green']
};

var visVH = {
min: -25,
max: -5
};

Map.addLayer(ndviJun2017, visNDVI, 'NDVI junio 2017', false);
Map.addLayer(ndviSep2017, visNDVI, 'NDVI septiembre 2017', false);
Map.addLayer(vhJunSep2017, visVH, 'VH mediana junio-septiembre 2017', false);

Map.addLayer(entrenamientoPrep, {color: '00FF00'}, 'Áreas entrenamiento clases 1-2', false);
Map.addLayer(validacionPrep, {color: 'FF0000'}, 'Puntos validación clases 1-2', false);

// ======================================================
// 10. EXPORTACIÓN A CSV
// ======================================================

Export.table.toDrive({
collection: valoresTotales,
description: 'VALORES_PIXEL_NDVI_VH_CHOPERAS_2017',
folder: carpetaExportacion,
fileNamePrefix: 'valores_pixel_ndvi_vh_choperas_2017',
fileFormat: 'CSV',
selectors: [
'fuente',
'clase_ref',
'clase_txt',
'IDn_export',
'IDn2_export',
'NDVI_JUN2017',
'NDVI_SEP2017',
'VH_JUNSEP2017',
'longitude',
'latitude'
]
});