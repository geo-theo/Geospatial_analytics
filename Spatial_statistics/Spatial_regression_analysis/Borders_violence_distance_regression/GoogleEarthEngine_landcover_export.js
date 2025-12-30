
// This chunks by country to download piecemeal for large datasets

//////////////////
// PARAMETERS

// ESA WorldCover (v200 = 2021).
var wc = ee.ImageCollection('ESA/WorldCover/v200').first();

// land-cover band
var lc = wc.select('Map');  

// Using 30 m for speed, but 10 m is highest
var exportScale  = 30;               
var exportFolder = 'GEE_exports';
var crs          = 'EPSG:4326';
var maxPixels    = 1e13;

// test list first
var africaCountries = [
  'Kenya',
  'Ethiopia',
  'Tanzania'
];
// full list
// var africaCountries = [
//   "Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
//   "Cabo Verde","Cameroon","Central African Republic","Chad","Comoros",
//   "Congo","Democratic Republic of the Congo","Djibouti","Egypt","Equatorial Guinea",
//   "Eritrea","Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea-Bissau",
//   "Kenya","Lesotho","Liberia","Libya","Madagascar","Malawi","Mali","Mauritania",
//   "Mauritius","Morocco","Mozambique","Namibia","Niger","Nigeria","Rwanda",
//   "Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia",
//   "South Africa","South Sudan","Sudan","Tanzania","Togo","Tunisia","Uganda",
//   "Zambia","Zimbabwe","Western Sahara"
// ];

// map friendly country name -> GAUL's ADM0_NAME
// GAUL uses full country name
var gaulNameMap = {
  'Tanzania': 'United Republic of Tanzania',
  // add more as needed
  // 'Eswatini': 'Swaziland',
  // 'Cabo Verde': 'Cape Verde',
  // 'Congo': 'Republic of the Congo'
};

//country boundaries

var gaul = ee.FeatureCollection('FAO/GAUL/2015/level0');

var africaGeom = gaul
  .filter(ee.Filter.inList('ADM0_NAME', africaCountries.map(function(c) {
    // map the names for the zoom too
    var name = ee.String(c);
    var mapped = ee.String(gaulNameMap[name.getInfo()] || name);
    return mapped;
  })))
  .geometry();

Map.centerObject(africaGeom, 4);

//////////////////
// EXPORT FUNCTION

function exportCountryLandcover(countryName) {

  // Use GAUL name has mapping, else use input name
  var gaulName = gaulNameMap[countryName] || countryName;

  var countryFC   = gaul.filter(ee.Filter.eq('ADM0_NAME', gaulName));
  print(countryName, '→ GAUL name:', gaulName,
        ' | feature count:', countryFC.size());

  var countryGeom = countryFC.geometry();

  // clip landcover to this country
  var lcCountry = lc.clip(countryGeom);

  Map.addLayer(
    lcCountry,
    {min: 10, max: 100,
     palette: ['006400','ffbb22','ffff4c','f096ff','fa0000','b4b4b4','f0f0f0']},
    'WC ' + countryName,
    false
  );

  var safeName = countryName.replace(/\s+/g, '_');

  Export.image.toDrive({
    image: lcCountry,
    description: 'WorldCover_' + safeName,
    folder: exportFolder,
    fileNamePrefix: 'worldcover_' + safeName,
    region: countryGeom,
    scale: exportScale,
    maxPixels: maxPixels,
    crs: crs
  });
}

//////////////////
// LOOP OVER COUNTRIES

for (var i = 0; i < africaCountries.length; i++) {
  var countryName = africaCountries[i];
  print('Setting up export for:', countryName);
  exportCountryLandcover(countryName);
}