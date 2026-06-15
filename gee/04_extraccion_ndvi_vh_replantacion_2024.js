// ======================================================
// Extracción de valores NDVI y VH en 2024
// Parcelas de chopera con corta detectada entre 2017 y 2023
// Google Earth Engine
// ======================================================

// ======================================================
// 1. VARIABLES DE ENTRADA
// ======================================================

// Variables definidas en el panel de imports de GEE:
//   AESTUDIO
//   CORTADAS_2017_2023

var zona = AESTUDIO;
var cortadas_2017_2023 = CORTADAS_2017_2023;

var carpetaExportacion = 'TFM_ING_MONTES';

// ======================================================
// 2. PARÁMETROS TEMPORALES
// ======================================================

var cloudPerc = 20;

var fechaS2JunIni = '2024-06-15';
var fechaS2JunFin = '2024-06-30';

var fechaS2SepIni = '2024-09-01';
var fechaS2SepFin = '2024-09-16';

var fechaS1Ini = '2024-06-01';
var fechaS1Fin = '2024-09-20';

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

function prepararParcelas(fc, grupo) {
return fc.map(function(f) {
return f.set({
'grupo_analisis': grupo
});
});
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
print('Fechas Sentinel-2 ' + nombreBanda, col.aggregate_array('system:time_start')
.map(function(t) {
return ee.Date(t).format('YYYY-MM-dd');
})
);

return col
.median()
.rename(nombreBanda)
.clip(zona);
}

var ndviJun2024 = getNDVIperiodo(
fechaS2JunIni,
fechaS2JunFin,
'NDVI_JUN2024'
);

var ndviSep2024 = getNDVIperiodo(
fechaS2SepIni,
fechaS2SepFin,
'NDVI_SEP2024'
);

// ======================================================
// 5. SENTINEL-1: VH MEDIANA JUNIO-SEPTIEMBRE 2024
// ======================================================

var s1col = ee.ImageCollection('COPERNICUS/S1_GRD')
.filterBounds(zona)
.filterDate(fechaS1Ini, fechaS1Fin)
.filter(ee.Filter.eq('instrumentMode', 'IW'))
.filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
.filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
.select(['VV', 'VH'])
.map(cleanS1);

print('Nº imágenes Sentinel-1 junio-septiembre 2024', s1col.size());
print('Fechas Sentinel-1 junio-septiembre 2024', s1col.aggregate_array('system:time_start')
.map(function(t) {
return ee.Date(t).format('YYYY-MM-dd');
})
);

var vhJunSep2024 = s1col
.median()
.select('VH')
.rename('VH_JUNSEP2024')
.clip(zona);

// ======================================================
// 6. STACK FINAL DE VARIABLES
// ======================================================

var stack2024 = ee.Image.cat([
ndviJun2024,
ndviSep2024,
vhJunSep2024,
ee.Image.pixelLonLat()
]).clip(zona);

// ======================================================
// 7. PREPARACIÓN DE PARCELAS
// ======================================================

var parcelas_2017_2023_prep = prepararParcelas(
cortadas_2017_2023,
'cortadas_2017_2023'
);

print('Nº parcelas cortadas 2017-2023', parcelas_2017_2023_prep.size());

// ======================================================
// 8. PROPIEDADES A CONSERVAR
// ======================================================

var propiedadesExportar = [
'grupo_analisis',

'FID',
'ID',
'MUNICIPIO',
'MASA',
'PARCELA',
'AREA',
'REFCAT',
'nom_muni',
'Shape_Area',
'parcelas_a',
'SUP_TOTAL',

'AREA_C1',
'AREA_C2',
'AREA_C3',
'AREA_C4',
'AREA_TOTAL',
'PCT_1',
'PCT_2',
'PCT_3',
'PCT_4',
'PUREZA',
'RANGO_PUR',

'CLASE_MAYO',
'CLASE_FIN',
'CLASE_45',
'EST_INT',
'CL_INI',

'CORTA_OK',
'SIN_CORT',
'EXC_MJ',
'SER_NOV17',
'DET_JOV',

'F_CORTE',
'A_CORTE',
'M_CORTE',
'VH_CORTE',

'F_DET',
'A_DET',
'M_DET',
'VH_DET',
'VH_NOV17',
'VH_MIN',
'N_BAJO',
'MOTIVO'
];

// ======================================================
// 9. EXTRACCIÓN DE VALORES DE PÍXEL
// ======================================================

var valores_2017_2023 = stack2024.sampleRegions({
collection: parcelas_2017_2023_prep,
properties: propiedadesExportar,
scale: 10,
tileScale: 8,
geometries: false
});

print('Nº píxeles exportables cortadas 2017-2023', valores_2017_2023.size());

// ======================================================
// 10. VISUALIZACIÓN
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

Map.addLayer(ndviJun2024, visNDVI, 'NDVI junio 2024', false);
Map.addLayer(ndviSep2024, visNDVI, 'NDVI septiembre 2024', false);
Map.addLayer(vhJunSep2024, visVH, 'VH mediana junio-septiembre 2024', false);

Map.addLayer(parcelas_2017_2023_prep, {color: '00FFFF'}, 'Parcelas cortadas 2017-2023', false);

// ======================================================
// 11. EXPORTACIÓN A CSV
// ======================================================

var selectoresExport = propiedadesExportar.concat([
'NDVI_JUN2024',
'NDVI_SEP2024',
'VH_JUNSEP2024',
'longitude',
'latitude'
]);

Export.table.toDrive({
collection: valores_2017_2023,
description: 'VALORES_PIXEL_REPLANTACION_CORTADAS_2017_2023',
folder: carpetaExportacion,
fileNamePrefix: 'valores_pixel_replantacion_cortadas_2017_2023',
fileFormat: 'CSV',
selectors: selectoresExport
});
