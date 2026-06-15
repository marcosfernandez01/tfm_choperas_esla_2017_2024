// =======================================================
// Clasificación de choperas de 2017 en Google Earth Engine
// =======================================================

// ===============================
// 1. VARIABLES DE ENTRADA
// ===============================

// Variables definidas en el panel de imports de GEE:
//   AESTUDIO
//   Parcelas_clasificacion
//   AREAS_ENTRENAMIENTO_4
//   VALIDACION
//   pendiente
//   altitud

var zona = AESTUDIO;
var parcelas = Parcelas_clasificacion;
var entrenamiento = AREAS_ENTRENAMIENTO_4;
var puntosValidacion = VALIDACION;

var mascaraPendiente = pendiente;
var mascaraAltitud = altitud;

var carpetaExportacion = 'TFM_ING_MONTES';

// ===============================
// 2. VISUALIZACIÓN INICIAL
// ===============================

Map.centerObject(zona, 13);

// ===============================
// 3. FUNCIONES AUXILIARES
// ===============================

function maskS2clouds(image) {
var qa = image.select('QA60');
var cloudBitMask = 1 << 10;
var cirrusBitMask = 1 << 11;

var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
.and(qa.bitwiseAnd(cirrusBitMask).eq(0));

return image.updateMask(mask);
}

function cleanS1(image) {
var edge = image.lt(-30.0);
var mask = image.mask().and(edge.not());
return image.updateMask(mask);
}

function validar(imagen, nombre) {
var val = imagen.sampleRegions({
collection: puntosValidacion,
properties: ['IDn2'],
scale: 10
});

var matriz = val.errorMatrix('IDn2', 'classification');

print('======================');
print(nombre);
print('Matriz de confusión:', matriz);
print('Exactitud global:', matriz.accuracy());
print('Precisión usuario:', matriz.consumersAccuracy());
print('Precisión productor:', matriz.producersAccuracy());
}

// ===============================
// 4. SENTINEL-2: COMPOSICIÓN DE VERANO
// ===============================

var s2_verano = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
.filterBounds(zona)
.filterDate('2017-07-05', '2017-07-30')
.filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
.map(maskS2clouds)
.select(['B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B11', 'B12'])
.median()
.clip(zona);

// ===============================
// 5. SENTINEL-2: COMPOSICIÓN DE OCTUBRE
// ===============================

var s2_octubre = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
.filterBounds(zona)
.filterDate('2017-10-07', '2017-10-10')
.filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
.map(maskS2clouds)
.median()
.select(['B4', 'B11', 'B12'])
.rename(['B4_oct', 'B11_oct', 'B12_oct'])
.clip(zona);

// ===============================
// 6. SENTINEL-1
// ===============================

var s1 = ee.ImageCollection('COPERNICUS/S1_GRD')
.filterBounds(zona)
.filterDate('2017-03-01', '2017-07-31')
.filter(ee.Filter.eq('instrumentMode', 'IW'))
.filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
.filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
.select(['VV', 'VH'])
.map(cleanS1)
.median()
.clip(zona);

// ===============================
// 7. COMPOSICIONES DE VARIABLES
// ===============================

var imagen_s1_vvvh = s2_verano
.addBands(s1)
.addBands(s2_octubre);

var imagen_s1_vh = s2_verano
.addBands(s1.select('VH'))
.addBands(s2_octubre);

// ===============================
// 8. MÁSCARA DE PARCELAS
// ===============================

var mascaraParcelas = parcelas
.map(function(f) {
return f.set('mask', 1);
})
.reduceToImage({
properties: ['mask'],
reducer: ee.Reducer.first()
});

imagen_s1_vvvh = imagen_s1_vvvh.updateMask(mascaraParcelas);
imagen_s1_vh = imagen_s1_vh.updateMask(mascaraParcelas);

// ===============================
// 9. BANDAS DE ENTRADA
// ===============================

var bandas_s1_vvvh = [
'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B11', 'B12',
'VH', 'VV',
'B4_oct', 'B11_oct', 'B12_oct'
];

var bandas_s1_vh = [
'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B11', 'B12',
'VH',
'B4_oct', 'B11_oct', 'B12_oct'
];

// ===============================
// 10. MUESTRAS DE ENTRENAMIENTO
// ===============================

var training_s1_vvvh = imagen_s1_vvvh.select(bandas_s1_vvvh).sampleRegions({
collection: entrenamiento,
properties: ['IDn'],
scale: 10
});

var training_s1_vh = imagen_s1_vh.select(bandas_s1_vh).sampleRegions({
collection: entrenamiento,
properties: ['IDn'],
scale: 10
});

// ===============================
// 11. CLASIFICADORES RANDOM FOREST
// ===============================

var rf_s1_vvvh = ee.Classifier.smileRandomForest(50)
.train(training_s1_vvvh, 'IDn', bandas_s1_vvvh);

var rf_s1_vh = ee.Classifier.smileRandomForest(50)
.train(training_s1_vh, 'IDn', bandas_s1_vh);

// ===============================
// 12. CLASIFICACIÓN
// ===============================

var clas_s1_vvvh = imagen_s1_vvvh.classify(rf_s1_vvvh);
var clas_s1_vh = imagen_s1_vh.classify(rf_s1_vh);

// ===============================
// 13. MÁSCARA TOPOGRÁFICA
// ===============================

var mascaraTopografica = mascaraPendiente.eq(1)
.and(mascaraAltitud.eq(1));

var clas_s1_vvvh_mask = clas_s1_vvvh.where(mascaraTopografica.not(), 4);
var clas_s1_vh_mask = clas_s1_vh.where(mascaraTopografica.not(), 4);

// ===============================
// 14. VALIDACIÓN
// ===============================

validar(clas_s1_vvvh_mask, 'RF Sentinel-2 + Sentinel-1 VV/VH + máscara');
validar(clas_s1_vh_mask, 'RF Sentinel-2 + Sentinel-1 VH + máscara');

// ===============================
// 15. VISUALIZACIÓN
// ===============================

var palette = [
'#006400',
'#7FFF00',
'#FFFF00',
'#D3D3D3'
];

Map.addLayer(
clas_s1_vvvh_mask,
{min: 1, max: 4, palette: palette},
'RF S2 + S1 VV/VH'
);

Map.addLayer(
clas_s1_vh_mask,
{min: 1, max: 4, palette: palette},
'RF S2 + S1 VH'
);

// ===============================
// 16. EXPORTACIÓN DE LA CLASIFICACIÓN
// ===============================

Export.image.toDrive({
image: clas_s1_vh_mask.toUint8(),
description: 'CLASIFICACION_CHOPERAS_2017_RF_S1_VH',
folder: carpetaExportacion,
fileNamePrefix: 'clasificacion_choperas_2017_rf_s1_vh',
region: zona.geometry(),
scale: 10,
crs: 'EPSG:25830',
maxPixels: 1e13
});

// ===============================
// 17. EXPORTACIÓN DE LA MATRIZ DE CONFUSIÓN
// ===============================

var val_rf = clas_s1_vh_mask.sampleRegions({
collection: puntosValidacion,
properties: ['IDn2'],
scale: 10
});

var matriz_rf = val_rf.errorMatrix('IDn2', 'classification');

var classLabels = matriz_rf.order();

var classNames = classLabels.map(function(label) {
return ee.Number(label).format('clase_%d');
});

var matrix = matriz_rf.array();

var features = ee.List.sequence(0, classNames.length().subtract(1)).map(function(i) {
i = ee.Number(i);

var rowArray = matrix.slice(0, i, i.add(1));
var rowFlat = rowArray.reshape([-1]);
var rowList = rowFlat.toList();

var properties = ee.Dictionary.fromLists(classNames, rowList);
var rowLabel = classNames.get(i);

properties = properties.set('row_label', rowLabel);

return ee.Feature(null, properties);
});

var matrixFC = ee.FeatureCollection(features);

Export.table.toDrive({
collection: matrixFC,
description: 'MATRIZ_CLASIFICACION_CHOPERAS_2017_RF_S1_VH',
folder: carpetaExportacion,
fileNamePrefix: 'matriz_clasificacion_choperas_2017_rf_s1_vh',
fileFormat: 'CSV'
});
