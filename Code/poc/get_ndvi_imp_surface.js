// Define the region of interest (San Francisco and East Bay region)
var sfEastBayRegion = ee.Geometry.Polygon([
  [-123.0, 37.5],  // West corner (shrinking west side)
  [-122.3, 37.5],  // North-West corner (shrinking north side)
  [-122.3, 37.9],  // North-East corner (shrinking north side)
  [-123.0, 37.9]   // South-East corner (shrinking west side)
]);

// **Impervious Surface Dataset (MODIS)**
// Get the MODIS Land Cover data for impervious surface.
var impervious_surface = ee.Image('MODIS/006/MCD12Q1/2013_01_01')
  .select('LC_Type1')  // Extract the land cover type
  .eq(12)  // Impervious surfaces in the MODIS dataset are class 12
  .clip(sfEastBayRegion);


// Load the Global Impervious Surface Dataset (GISD30) for the period 1985-2020
// The paper: https://essd.copernicus.org/articles/14/1831/2022/

var palette = ["#808080", "#006400", "#228B22", "#32CD32", "#ADFF2F", "#lFFFF00", "#FFA500", "#FF0000"];

var snazzy = require("users/aazuspan/snazzy:styles");
snazzy.addStyle("https://snazzymaps.com/style/132/light-gray", "Grayscale");


var gisd30 = ee.Image("projects/sat-io/open-datasets/GISD30_1985_2020");
Map.addLayer(gisd30,{min:1,max:8,palette:palette},'GISD 30')

// Clip the dataset to the region of interest (San Francisco and East Bay region)
var gisd30_clipped = gisd30.clip(sfEastBayRegion);

// Set the map center and zoom to the region
Map.centerObject(sfEastBayRegion, 9);

// Visualize the GISD30 (Impervious Surface) dataset
Map.addLayer(gisd30_clipped, {min: 0, max: 100, palette: ['white', 'black']}, 'GISD30 Impervious Surface');


// Load the Sentinel-2 image collection for the desired period
var S2_SR = ee.ImageCollection('COPERNICUS/S2_SR')
  .filterDate('2018-01-01', '2019-01-01')
  .filterBounds(sfEastBayRegion);  // Filter by the region of interest

// Add NDVI band to each image
var addNDVI = function(image) {
  var ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI');
  return image.addBands(ndvi);
};

var S2_NDVI = S2_SR.map(addNDVI);

// Calculate the mean NDVI for the region
var ndvi_mean = S2_NDVI.select('NDVI').mean();

// Set the map center and zoom to the region
Map.centerObject(sfEastBayRegion, 9);  // Adjust zoom level as needed

// Define the color palette for NDVI visualization
var NDVIpalette = [
  'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', 
  '99B718', '74A901', '66A000', '529400', '3E8601', 
  '207401', '056201', '004C00', '023B01', '012E01', 
  '011D01', '011301'
];

// Add the mean NDVI layer to the map
Map.addLayer(ndvi_mean.select('NDVI'), {palette: NDVIpalette}, 'Annual mean Sentinel NDVI');

// **Exporting the Data:**
// Export NDVI to Drive
Export.image.toDrive({
  image: ndvi_mean,
  description: 'SF_EastBay_NDVI_Sentinel_10',
  scale: 10,
  region: sfEastBayRegion,
  fileFormat: 'GeoTIFF',
  maxPixels: 1e8
});


// Export Impervious Surface to Drive
Export.image.toDrive({
  image: impervious_surface,
  description: 'SF_EastBay_Impervious_Surface_MODIS_500m',
  scale: 500,
  region: sfEastBayRegion,
  fileFormat: 'GeoTIFF',
  maxPixels: 1e8
});



// Export the clipped GISD30 dataset to Google Drive
Export.image.toDrive({
  image: gisd30_clipped,
  description: 'SF_EastBay_GISD30_Impervious_Surface_30m',
  scale: 30,  // Use 30m resolution as appropriate
  region: sfEastBayRegion,
  fileFormat: 'GeoTIFF',
  maxPixels: 1e8
});

