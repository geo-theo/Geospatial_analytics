# Import and Authenticate GEE
import ee
import geemap

# Change project name to my google account
ee.Authenticate() #if error use: ee.Authenticate(force=True)
ee.Initialize(project='fors351labs-489406')
print("Accept Google permissions if prompted.")

#Connect to all landsat data for 2018 and 2019
dataset = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2').filterDate(
    '2018-01-01', '2019-12-31'
)

# Mount Google Drive and set an output folder for saving figures,
print("Accept Google permissions if prompted.")
from pathlib import Path
from google.colab import drive
drive.mount('/content/drive')

# Preprocessing function
def preprocess_landsat8(image):
    # QA masks
    qa_mask = image.select('QA_PIXEL').bitwiseAnd(int('11111', 2)).eq(0)
    saturation_mask = image.select('QA_RADSAT').eq(0)

    # Apply scaling factors
    optical = image.select('SR_B.*').multiply(0.0000275).add(-0.2)
    thermal = image.select('ST_B.*').multiply(0.00341802).add(149.0)

    return (
        image
        .addBands(optical, None, True)
        .addBands(thermal, None, True)
        .updateMask(qa_mask)
        .updateMask(saturation_mask)
    )

dataset_cleaned = dataset.map(preprocess_landsat8)

##############
### Task 1 ###
##############

# Drive folder with egojson file
geojson_path = '/content/drive/MyDrive/FORS351/'

# Full file path for geojson file
geojson = geojson_path + 'California_Historic_Fire_Perimeters_106541161897094478.geojson'

# Import geopandas
import geopandas as gpd

# Read in geojson
WoolseyFire_Perimeter = gpd.read_file(geojson)

#Converts the file to a GEE feature
WoolseyFire_Perimeter_gee = geemap.geopandas_to_ee(WoolseyFire_Perimeter)

# Prep feature to be used as clip
woolsey_geom = WoolseyFire_Perimeter_gee.geometry()

# Show map
Map = geemap.Map()
Map.centerObject(woolsey_geom, 10)
Map.addLayer(woolsey_geom, {}, 'Woolsey Fire Perimeter')
Map

##############
### Task 2 ###
##############

#Compute NDVI formula
def calc_ndvi(image):
  ndvi_expression = "(NIR - Red) / (NIR + Red)"

  ndvi = image.expression(ndvi_expression,{
          'NIR': image.select('SR_B5'),
          'Red': image.select('SR_B4')}).rename('NDVI')

  image = image.addBands(ndvi)
  return image

#Visualization paramaters
ndvi_vis_params = {
    'min': 0,
    'max': 1,
    'dimensions': 1000,
    'palette': ['A50026', 'D73027', 'F46D43', 'FDAE61', 'FEE08B',
                'FFFFBF','D9EF8B','A6D96A','66BD63','1A9850']
}

# Cleaning dataset by median
image = calc_ndvi(dataset_cleaned.median()).clip(woolsey_geom)

# show ndvi map
Map = geemap.Map()
Map.centerObject(woolsey_geom, 10)
Map.addLayer(image.select('NDVI'), vis_params=ndvi_vis_params, name='NDVI')
Map.addLayer(woolsey_geom, {}, 'Woolsey Fire Perimeter')
Map

##############
### Task 3 ###
##############

ndvi_collection = dataset_cleaned.map(calc_ndvi)

# Convert to dataframe only keeping nvdi and time
def compute_timeseries(ImageCollection, region):
    def per_image(img):
        mean_dict = img.reduceRegion(
            reducer=ee.Reducer.mean(),
            geometry=region,
            scale=30,
        )

        return ee.Feature(
            None,
            {
                'NDVI': mean_dict.get('NDVI'),
                'time': img.date().millis()
            }
        )

    return ee.FeatureCollection(ImageCollection.filterBounds(region).map(per_image))

ndvi_time_series = compute_timeseries(ndvi_collection, woolsey_geom)

ndvi_df = geemap.ee_to_df(ndvi_time_series)
ndvi_df = ndvi_df[['NDVI', 'time']]

ndvi_df.head()

# number of rows = number of NDVI days being exported to the data table
print(ndvi_time_series.size().getInfo())

import pandas as pd

#create a dataframe with NDVI is one column and time in the next column
ndvi_timeseries_df = geemap.ee_to_df(ndvi_time_series, ['NDVI', 'time'], remove_geom=True)

#specify the time units in your dataframe
ndvi_timeseries_df['time'] = pd.to_datetime(ndvi_timeseries_df['time'], unit='ms')

#Drop the NA values, which means there is no data available and sort by time
ndvi_timeseries_df = ndvi_timeseries_df.dropna().sort_values('time')
print(ndvi_timeseries_df)

##############
### Task 4 ###
##############

import matplotlib.pyplot as plt
plt.figure(figsize=(15, 6))
plt.plot(ndvi_timeseries_df['time'], ndvi_timeseries_df['NDVI'],marker='o')
plt.xlabel('x-axis: Date')
plt.ylabel('y-axis: NDVI')
plt.title('Title: Woolsey Fire NDVI Time Series')
plt.show()

##############
### Task 5 ###
##############

# Create new rectangle bounding box
baseline_geom = ee.Geometry.Rectangle([
    -118.66550201118918,
    34.06230249563295,
    -118.52504359504627,
    34.13089722406558
])

baseline_timeseries = compute_timeseries(ndvi_collection, baseline_geom)

baseline_timeseries_df = geemap.ee_to_df(baseline_timeseries, ['NDVI', 'time'], remove_geom=True)

baseline_timeseries_df['time'] = pd.to_datetime(baseline_timeseries_df['time'], unit='ms')
baseline_timeseries_df = baseline_timeseries_df.dropna().sort_values('time')

#Print to see the dataframe your timeseries
print(baseline_timeseries_df)

import matplotlib.pyplot as plt

# Overlay two plots of NDVI for burned area and the baseline
plt.figure(figsize=(10, 4))
plt.plot(ndvi_timeseries_df['time'], ndvi_timeseries_df['NDVI'],marker='o')
plt.plot(baseline_timeseries_df['time'], baseline_timeseries_df['NDVI'],marker='o')
plt.xlabel('Date')
plt.ylabel('NDVI')
plt.title('Woolsey Fire vs Baseline NDVI Time Series')
plt.legend(['Burnt NDVI', 'Baseline NDVI'])
plt.show()

##############
### Task 6 ###
##############

import matplotlib.pyplot as plt
from statsmodels.nonparametric.smoothers_lowess import lowess

def lowess_smooth(df, var, frac=0.15):
    smoothed = lowess(
        endog=df[var],
        exog=df['time'].astype('int64'),
        frac=frac,
        return_sorted=False
    )
    return smoothed

ndvi_timeseries_df['NDVI_smooth'] = lowess_smooth(ndvi_timeseries_df, 'NDVI')
baseline_timeseries_df['NDVI_smooth'] = lowess_smooth(baseline_timeseries_df, 'NDVI')

plt.figure(figsize=(10, 4))
plt.plot(ndvi_timeseries_df['time'], ndvi_timeseries_df['NDVI'],
         marker='o', linestyle='None', color='red', alpha=0.5, label='')
plt.plot(ndvi_timeseries_df['time'], ndvi_timeseries_df['NDVI_smooth'],
         linestyle='-', color='red', label='Woolsey Fire')
plt.plot(baseline_timeseries_df['time'], baseline_timeseries_df['NDVI'],
         marker='o', linestyle='None', color='green', alpha=0.5, label='')
plt.plot(baseline_timeseries_df['time'], baseline_timeseries_df['NDVI_smooth'],
         linestyle='-', color='green', label='Baseline')
plt.xlabel('Date')
plt.ylabel('NDVI')
plt.title('Woolsey Fire vs Baseline NDVI Time Series')
plt.tight_layout()
plt.legend()
plt.show()

##############
### Task 7 ###
##############

#Change directory to a folder on your drive where you put the GeoJSON file
#NOTE YOURS WILL BE DIFFERENT THAN MINE, everying after My Drive/ is your own
#directory. Make sure it ends with a backslash.
geojson_path = '/content/drive/MyDrive/FORS351/'

#This is where it calls the file you downloaded and moved into your Google Drive
#Your file name may be different than mine, so you might have to change this
geojson = geojson_path + 'California_Historic_Fire_Perimeters_-3061168889927148014.geojson'

#This imports a geospatial package called 'geopandas' and then plots the fire permiter
import geopandas as gpd

# Reads in the file, note I'm changing the name to Donnell!
DonnellFire_Perimeter = gpd.read_file(geojson)

#Converts the file to a GEE feature
DonnellFire_Perimeter_gee = geemap.geopandas_to_ee(DonnellFire_Perimeter)

#Turns the feature into a geometry object which we can use to clip our data
donnell_geom = DonnellFire_Perimeter_gee.geometry()

#Create your map of the Donnell Fire Perimeter
Map = geemap.Map()
Map.centerObject(donnell_geom, 10)
Map.addLayer(donnell_geom, {}, 'Donnell Fire Perimeter')
Map

#Connect to all landsat data for 2018 and 2019 - or change to your date of
#interest. Note the Donnell fire occured in 2018, so I want to capture the fire
#year and recovery immediately after
dataset = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2').filterDate(
    '2018-01-01', '2019-12-31'
)

# Mount Google Drive and set an output folder for saving figures,
from pathlib import Path
from google.colab import drive
drive.mount('/content/drive')

# Preprocessing function
def preprocess_landsat8(image):
    # QA masks
    qa_mask = image.select('QA_PIXEL').bitwiseAnd(int('11111', 2)).eq(0)
    saturation_mask = image.select('QA_RADSAT').eq(0)

    # Apply scaling factors
    optical = image.select('SR_B.*').multiply(0.0000275).add(-0.2)
    thermal = image.select('ST_B.*').multiply(0.00341802).add(149.0)

    return (
        image
        .addBands(optical, None, True)
        .addBands(thermal, None, True)
        .updateMask(qa_mask)
        .updateMask(saturation_mask)
    )

dataset_cleaned = dataset.map(preprocess_landsat8)

#Visualization paramaters
ndvi_vis_params = {
    'min': 0,
    'max': 1,
    'dimensions': 1000,
    'palette': ['A50026', 'D73027', 'F46D43', 'FDAE61', 'FEE08B',
                'FFFFBF','D9EF8B','A6D96A','66BD63','1A9850']
}

# Cleaning dataset by median
image = calc_ndvi(dataset_cleaned.median()).clip(donnell_geom)

# show ndvi map
Map = geemap.Map()
Map.centerObject(donnell_geom, 10)
Map.addLayer(image.select('NDVI'), vis_params=ndvi_vis_params, name='NDVI')
Map.addLayer(donnell_geom, {}, 'Donnell Fire Perimeter')
Map

#Compute NDVI for your images, by first defining a function
# Band 5 is the NIR band from Landsat 8 and Band 4 is the Red
def calc_ndvi(image):
  ndvi_expression = "(NIR - Red) / (NIR + Red)"

  ndvi = image.expression(ndvi_expression,{
          'NIR': image.select('SR_B5'),
          'Red': image.select('SR_B4')}).rename('NDVI')

  image = image.addBands(ndvi)
  return image

#You have already defined the NDVI collection above, but we will do it again here,
#note that I am changing the bottom line of code to make sure I am computing
#the timeseries of my Donnell fire. You will need to change this to match yours

ndvi_collection = dataset_cleaned.map(calc_ndvi)

# The last map you made only returns one timestamp because it's using reducer
# over the entire timeseries.
def compute_timeseries(ImageCollection, region):
    def per_image(img):
        mean_dict = img.reduceRegion(
            reducer=ee.Reducer.mean(),
            geometry=region,
            scale=30,
        )

        return ee.Feature(
            None,
            {
                'NDVI': mean_dict.get('NDVI'),
                'time': img.date().millis()
            }
        )

    return ee.FeatureCollection(ImageCollection.filterBounds(region).map(per_image))

ndvi_time_series = compute_timeseries(ndvi_collection, donnell_geom)

#print the data table
print(ndvi_time_series.size().getInfo())

#Now create your data frame below by cleaning it, just like you did for the
#Woolsely fire, but now for the new fire. Nothing needs to change in this code
#but note these variables will now be overwritten.

import pandas as pd

#create a dataframe with NDVI is one column and time in the next column
ndvi_timeseries_df = geemap.ee_to_df(ndvi_time_series, ['NDVI', 'time'], remove_geom=True)

#specify the time units in your dataframe
ndvi_timeseries_df['time'] = pd.to_datetime(ndvi_timeseries_df['time'], unit='ms')

#Drop the NA values, which means there is no data available and sort by time
ndvi_timeseries_df = ndvi_timeseries_df.dropna().sort_values('time')

#print the dataframe
print(ndvi_timeseries_df)

#Now make a plot of NDVI for your fire of interest, change the title of your
#fire so it's not called Lolo - because that's the example here

import matplotlib.pyplot as plt

plt.figure(figsize=(10,4))

plt.plot(ndvi_timeseries_df['time'], ndvi_timeseries_df['NDVI'], marker='o')

plt.xlabel('Date')
plt.ylabel('NDVI')
plt.title('Donnell Fire NDVI Time Series')

plt.show()

#This is a polygon I created around a baseline I knew didn't burn, but you can
#modify this code for a simple rectangle and changing the code to ee.Geometry.Rectangle
baseline_geom = ee.Geometry.Rectangle([
    -120.17681311037637,
    38.50584373060667,
    -119.90897143263585,
    38.612982373112715
])

baseline_timeseries = compute_timeseries(ndvi_collection, baseline_geom)

baseline_timeseries_df = geemap.ee_to_df(baseline_timeseries, ['NDVI', 'time'], remove_geom=True)

baseline_timeseries_df['time'] = pd.to_datetime(baseline_timeseries_df['time'], unit='ms')
baseline_timeseries_df = baseline_timeseries_df.dropna().sort_values('time')

#Print to see the dataframe your timeseries
print(baseline_timeseries_df)

from statsmodels.nonparametric.smoothers_lowess import lowess

def lowess_smooth(df, var, frac=0.15):
    smoothed = lowess(
        endog=df[var],
        exog=df['time'].astype('int64'),
        frac=frac,
        return_sorted=False
    )
    return smoothed

ndvi_timeseries_df['NDVI_smooth'] = lowess_smooth(ndvi_timeseries_df, 'NDVI')
baseline_timeseries_df['NDVI_smooth'] = lowess_smooth(baseline_timeseries_df, 'NDVI')

plt.figure(figsize=(10, 4))

# Raw points
plt.scatter(ndvi_timeseries_df['time'], ndvi_timeseries_df['NDVI'],
            color='red', alpha=0.5, label='Donnell Fire NDVI')
plt.scatter(baseline_timeseries_df['time'], baseline_timeseries_df['NDVI'],
            color='green', alpha=0.5, label='Baseline NDVI')

# Smoothed trend lines
plt.plot(ndvi_timeseries_df['time'], ndvi_timeseries_df['NDVI_smooth'],
         linestyle='-', color='red', label='Donnell Fire Smoothed')
plt.plot(baseline_timeseries_df['time'], baseline_timeseries_df['NDVI_smooth'],
         linestyle='-', color='green', label='Baseline Smoothed')

plt.xlabel('Date')
plt.ylabel('NDVI')
plt.title('Donnell Fire vs Baseline NDVI Time Series')
plt.tight_layout()
plt.legend()
plt.show()