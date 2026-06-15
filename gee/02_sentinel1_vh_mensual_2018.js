// ======================================================
// Sentinel-1 VH mensual para detección de cortas
// Ejemplo correspondiente al año 2018
//
// El procedimiento se aplicó de forma independiente para
// cada año analizado, ajustando el año y los nombres de salida.
// ======================================================

// ================================
// 1. VARIABLES DE ENTRADA
// ================================

// Variables definidas en el panel de imports de GEE:
//   AESTUDIO
//   Parcelas

var zona = AESTUDIO;
var parcelas = Parcelas;

var anio = 2018;
var carpetaExportacion = 'TFM_ING_MONTES';

var geometry = zona.geometry();

// ================================
// 2. COLECCIÓN SENTINEL-1 VH
// ================================

function cleanS1(image) {
var edge = image.lt(-30.0);
var mask = image.mask().and(edge.not());
return image.updateMask(mask);
}

var radar = ee.ImageCollection('COPERNICUS/S1_GRD')
.filterBounds(geometry)
.filter(ee.Filter.eq('instrumentMode', 'IW'))
.filter(ee.Filter.eq('orbitProperties_pass', 'DESCENDING'))
.filterMetadata('resolution_meters', 'equals', 10)
.select('VH')
.map(cleanS1);

// ================================
// 3. MEDIANAS MENSUALES
// ================================

var meses = [
{nombre: 'ENE', numero: 1},
{nombre: 'FEB', numero: 2},
{nombre: 'MAR', numero: 3},
{nombre: 'ABR', numero: 4},
{nombre: 'MAY', numero: 5},
{nombre: 'JUN', numero: 6},
{nombre: 'JUL', numero: 7},
{nombre: 'AGO', numero: 8},
{nombre: 'SEP', numero: 9},
{nombre: 'OCT', numero: 10},
{nombre: 'NOV', numero: 11},
{nombre: 'DIC', numero: 12}
];

var imagenesMensuales = meses.map(function(mes) {
var fechaInicio = ee.Date.fromYMD(anio, mes.numero, 1);
var fechaFin = fechaInicio.advance(1, 'month');

return radar
.filterDate(fechaInicio, fechaFin)
.median()
.rename(mes.nombre)
.clip(geometry);
});

var S1_VH_ANUAL = ee.Image.cat(imagenesMensuales);

// ================================
// 4. EXPORTACIÓN
// ================================

Export.image.toDrive({
image: S1_VH_ANUAL,
description: 'S1_VH_' + anio,
fileNamePrefix: 'S1_VH_' + anio,
folder: carpetaExportacion,
region: geometry,
scale: 10,
crs: 'EPSG:25830',
maxPixels: 1e13
});

// ================================
// 5. VISUALIZACIÓN
// ================================

Map.centerObject(zona);

Map.addLayer(
S1_VH_ANUAL.select('ENE'),
{min: -25, max: 0, gamma: 0.9},
'VH enero ' + anio,
false
);

Map.addLayer(
S1_VH_ANUAL.select('JUL'),
{min: -25, max: 0, gamma: 0.9},
'VH julio ' + anio,
false
);

Map.addLayer(
S1_VH_ANUAL.select('DIC'),
{min: -25, max: 0, gamma: 0.9},
'VH diciembre ' + anio,
false
);

Map.addLayer(
parcelas,
{color: 'purple'},
'Parcelas',
false
);
