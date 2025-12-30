####################
### pip installs ###
####################
def install_dependencies():
    import sys, subprocess, importlib.util
    REQ = {"numpy":"numpy","pandas":"pandas","matplotlib":"matplotlib","geopandas":"geopandas","fiona":"fiona","rioxarray":"rioxarray","rasterio":"rasterio","rasterstats":"rasterstats","libpysal":"libpysal","esda":"esda","statsmodels":"statsmodels","PIL":"pillow","IPython":"ipython","folium":"folium>=0.12","mapclassify":"mapclassify","jenkspy":"jenkspy"}
    missing = [pip_name for mod, pip_name in REQ.items() if importlib.util.find_spec(mod) is None]
    if missing:
        subprocess.check_call([sys.executable, "-m", "pip", "install", "-U"] + missing)

if __name__ == "__main__":
    install_dependencies()

###############
# imports
###############
import warnings
warnings.filterwarnings('ignore')
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import geopandas as gpd
import fiona
import folium
from folium import Element
import mapclassify
import jenkspy
import rioxarray as rxr
from rasterio.enums import Resampling
from rasterstats import zonal_stats
import libpysal
import esda
import statsmodels.api as sm
import statsmodels.formula.api as smf
from PIL import Image as PILImage
from IPython.display import Image as IPyImage, display

__all__ = ["install_dependencies", "np", "pd", "plt", "gpd","fiona", "folium", "mapclassify","rxr", "Resampling", "zonal_stats","libpysal", "esda", "sm","smf","PILImage", "IPyImage", "display",
           "import_geopackage", "export_geopackage","set_crs","save_grid_gdf","coastal_binary",
           "compute_conflict_counts_aggregated", "compute_conflict_counts_disaggregated", "compute_conflict_counts_subeventcategory", "compute_fatalities",
           "compute_population_estimate", "compute_landcover_majority", "compute_pavedroadkm", "compute_riverskm",
           "compute_autocorrelation", "compute_dist_to_border"]

#####################
##### Functions #####
#####################

##### Data Prep #####
def import_geopackage(path, layer_name=None):
    return gpd.read_file(path, layer=layer_name) if layer_name else gpd.read_file(path)

def export_geopackage(gdf, path, layer_name):
    gdf.to_file(path, layer=layer_name, driver="GPKG")

def set_crs(gdf, target_crs):
    return gdf.to_crs(target_crs)
    
def save_grid_gdf(path, layer_name, target_crs):
    gdf  = import_geopackage(path, layer_name=layer_name)
    gdf = set_crs(gdf, target_crs)
    gdf = gdf.reset_index(drop=True)
    gdf["cell_id"] = gdf.index.astype("int64")
    gdf["area_km2"] = gdf.geometry.area / 1e6
    gdf["geometry"] = gdf.geometry.make_valid() # fix any invalid grid geometries
    return gdf

def coastal_binary(gdf, full_cell_area=100):
    full_area_km2 = full_cell_area # full cell area in km^2 (10km x 10km)
    tol = 0.01
    gdf["is_coastal"] = (gdf["area_km2"] < (full_area_km2 - tol)).astype("int64")
    return gdf

##### Compute statistics per grid cell #####
def compute_conflict_counts_aggregated(gdf, conflict_gdf):
    # spatial join (assign to each point the cell_id it falls in)
    pts_with_cellid = gpd.sjoin(conflict_gdf[["geometry"]],gdf[["cell_id", "geometry"]],how="left",predicate="within") # predicate="within" avoids double-counting boundary points, but may drop exact-on-boundary cases
    incident_counts = pts_with_cellid.groupby("cell_id").size().rename("count_total_aggregated") # count points per cell_id
    gdf = gdf.join(incident_counts, on="cell_id") # join back to grid
    gdf["count_total_aggregated"] = gdf["count_total_aggregated"].fillna(0).astype("int64") # fill nans with zero
    # check number of unmatched points
    print("Total ACLED points:", len(conflict_gdf))
    print("Matched to a cell:", pts_with_cellid["cell_id"].notna().sum())
    print("Unmatched:", pts_with_cellid["cell_id"].isna().sum())
    print("-----")
    return gdf

def compute_conflict_counts_disaggregated(gdf, conflict_gdf):
    # spatial join (assign to each point the cell_id it falls in)
    pts_with_cellid = gpd.sjoin(conflict_gdf[["EVENT_TYPE", "geometry"]],gdf[["cell_id", "geometry"]],how="left",predicate="within")
    counts_wide = (pts_with_cellid.groupby(["cell_id", "EVENT_TYPE"]).size().unstack(fill_value=0)) # counts per cell per event type
    counts_wide.columns = ["count_" + str(c).lower().replace("/", "_").replace(" ", "_").replace("-", "_") for c in counts_wide.columns] # standardize column names
    gdf = gdf.drop(columns=counts_wide.columns, errors="ignore")
    gdf = gdf.join(counts_wide, on="cell_id") # join to grid
    event_cols = counts_wide.columns.tolist()
    gdf[event_cols] = gdf[event_cols].fillna(0).astype("int64") # fill nans with zero
    gdf["count_total_disaggregated"] = gdf[event_cols].sum(axis=1).astype("int64") # Compute total
    return gdf

def compute_conflict_counts_subeventcategory(gdf, conflict_gdf):
    # spatial join (assign to each point the cell_id it falls in)
    pts_with_cellid = gpd.sjoin(conflict_gdf[["SUB_EVENT_TYPE", "geometry"]],gdf[["cell_id", "geometry"]],how="left",predicate="within")
    counts_wide = (pts_with_cellid.groupby(["cell_id", "SUB_EVENT_TYPE"]).size().unstack(fill_value=0)) # counts per cell per event type
    counts_wide.columns = ["count_" + str(c).lower().replace("/", "_").replace(" ", "_").replace("-", "_") for c in counts_wide.columns] # standardize column names
    gdf = gdf.drop(columns=counts_wide.columns, errors="ignore")
    gdf = gdf.join(counts_wide, on="cell_id") # join to grid
    event_cols = counts_wide.columns.tolist()
    gdf[event_cols] = gdf[event_cols].fillna(0).astype("int64") # fill nans with zero
    gdf["count_total_subevent"] = gdf[event_cols].sum(axis=1).astype("int64") # Compute total
    return gdf

def compute_fatalities(gdf, conflict_gdf):
    # spatial join (assign to each point the cell_id it falls in)
    pts_with_cellid = gpd.sjoin(conflict_gdf[["FATALITIES", "geometry"]],gdf[["cell_id", "geometry"]],how="left",predicate="within")
    fat_sums = (pts_with_cellid.groupby("cell_id")["FATALITIES"].sum().rename("fatalities_total")) # sum fatalities per cell_id
    gdf = gdf.drop(columns=["fatalities_total"], errors="ignore")
    gdf = gdf.join(fat_sums, on="cell_id") # join to grid
    gdf["fatalities_total"] = gdf["fatalities_total"].fillna(0).astype("int64") # fill nans with zero
    # check
    print("Total fatalities in points:", conflict_gdf["FATALITIES"].sum())
    print("Total fatalities in grid:", gdf["fatalities_total"].sum())
    print("-----")
    return gdf

def compute_population_estimate(gdf, path, chunk_size=5000, resolution=1000):    
    # estimate population per grid cell based on population density in selected area
    popdens = rxr.open_rasterio(path, masked=True).squeeze()
    nodata = popdens.rio.nodata # no data handling
    if nodata is None:
        nodata = -9999
    if popdens.rio.crs != gdf.crs:
        popdens_aea = popdens.rio.reproject(gdf.crs,resolution=resolution,resampling=Resampling.average,nodata=nodata)
    else:
        popdens_aea = popdens
    px_w, px_h = popdens_aea.rio.resolution()
    pixel_area_km2 = abs(px_w * px_h) / 1e6
    people_per_pixel = popdens_aea * pixel_area_km2
    gdf["pop_est"] = 0.0 # set default population per grid cell to 0
    arr = np.asarray(people_per_pixel.fillna(0).values)
    aff = people_per_pixel.rio.transform()
    for start in range(0, len(gdf), chunk_size):
        chunk = gdf.iloc[start:start+chunk_size]
        zs = zonal_stats(chunk,arr,affine=aff,nodata=nodata,stats=["sum"],all_touched=False)
        gdf.loc[chunk.index, "pop_est"] = [d["sum"] if d["sum"] is not None else 0 for d in zs]
    gdf["pop_est"] = gdf["pop_est"].round().astype("int64")
    return gdf

def compute_landcover_majority(gdf, raster_path, chunk_size=5000, resolution=1000):
    # assign mode/majority land cover classification of each grid cell
    lc = rxr.open_rasterio(raster_path, masked=True).squeeze()
    nodata = lc.rio.nodata
    if nodata is None:
        nodata = -9999
    if lc.rio.crs != gdf.crs:
        lc_proj = lc.rio.reproject(gdf.crs,resolution=resolution,resampling=Resampling.nearest,nodata=nodata)
    else:
        lc_proj = lc
    gdf["landcover_majority"] = pd.Series([pd.NA] * len(gdf), index=gdf.index, dtype="Int64")
    arr = np.asarray(lc_proj.fillna(nodata).values)
    aff = lc_proj.rio.transform()
    for start in range(0, len(gdf), chunk_size):
        chunk = gdf.iloc[start:start + chunk_size]
        zs = zonal_stats(chunk,arr,affine=aff,nodata=nodata,stats=["majority"],all_touched=False)
        gdf.loc[chunk.index, "landcover_majority"] = [d["majority"] if d["majority"] is not None else pd.NA for d in zs]
    return gdf

def compute_pavedroadkm(gdf, target_crs="ESRI:102023"):
    roads = import_geopackage("../Data/Controls_data/Africa_vector_controls_102023.gpkg", layer_name="roads_lines_102023")
    roads_len = set_crs(roads, target_crs)
    grid_len = set_crs(gdf, target_crs)
    # keep only line geometries; drop empties
    roads_len = roads_len[roads_len.geometry.notna()].copy()
    roads_len = roads_len[~roads_len.geometry.is_empty].copy()
    # fix invalid geometries
    roads_len["geometry"] = roads_len.geometry.make_valid()
    grid_len["geometry"] = grid_len.geometry.make_valid()
    # spatial index overlay (intersection) to clip road segments to each cell polygon
    roads_x = gpd.overlay(roads_len[["geometry"]],grid_len[["cell_id", "geometry"]],how="intersection",keep_geom_type=True)
    roads_x["road_km"] = roads_x.geometry.length / 1000.0 # length in km
    road_km_by_cell = roads_x.groupby("cell_id")["road_km"].sum() # sum per cell
    gdf["road_km"] = gdf["cell_id"].map(road_km_by_cell).fillna(0.0) # write back onto main grid_10km (in 102022) by cell_id
    return gdf

def compute_riverskm(gdf, target_crs="ESRI:102023"):
    rivers = import_geopackage("../Data/Controls_data/Africa_vector_controls_102023.gpkg", layer_name="rivers_lines_102023")
    rivers_len = set_crs(rivers, target_crs)
    grid_len = set_crs(gdf, target_crs)
    # keep only line geometries; drop empties
    rivers_len = rivers_len[rivers_len.geometry.notna()].copy()
    rivers_len = rivers_len[~rivers_len.geometry.is_empty].copy()
    # fix invalid geometries
    rivers_len["geometry"] = rivers_len.geometry.make_valid()
    grid_len["geometry"] = grid_len.geometry.make_valid()
    # spatial index overlay (intersection) to clip river segments to each cell polygon
    rivers_x = gpd.overlay(rivers_len[["geometry"]],grid_len[["cell_id", "geometry"]],how="intersection",keep_geom_type=True)
    rivers_x["rivers_km"] = rivers_x.geometry.length / 1000.0 # length in km
    rivers_km_by_cell = rivers_x.groupby("cell_id")["rivers_km"].sum() # sum per cell
    gdf["rivers_km"] = gdf["cell_id"].map(rivers_km_by_cell).fillna(0.0) # write back onto main grid_10km (in 102022) by cell_id
    return gdf

##### Autocorrelation clusters #####
def compute_autocorrelation(gdf):
    gdf["count_total_aggregated"] = gdf["count_total_aggregated"].fillna(0).astype(float)
    y = np.log1p(gdf["count_total_aggregated"].to_numpy()) # Log count for skewed
    # For raw counts instead, use:
    # y = gdf["count_total"].to_numpy()
    # Queen contiguity spatial wieghts (polygon neighbors sharing edge or vertex)
    w = libpysal.weights.Queen.from_dataframe(gdf)
    w.transform = "r"  # row-standardize
    # Global Moran's I
    mi = esda.Moran(y, w, permutations=999)
    print("Global Moran's I:", mi.I)
    print("p-value (sim):", mi.p_sim)
    # Local Moran (LISA)
    li = esda.Moran_Local(y, w, permutations=999)
    gdf["lisa_I"] = li.Is
    gdf["lisa_p"] = li.p_sim
    gdf["lisa_q"] = li.q  # quadrant 1..4
    # Cluster quadrant mapping:
    quad_map = {1: "HH", 2: "LH", 3: "LL", 4: "HL"}
    gdf["lisa_label"] = gdf["lisa_q"].map(quad_map)
    alpha = 0.05
    gdf.loc[gdf["lisa_p"] > alpha, "lisa_label"] = "Not signif"
    return gdf

##### Compute distances #####
def compute_dist_to_border(gdf, border_name, target_crs="ESRI:102023"):
    borders = import_geopackage("../Data/Borders_data/africa_borders_102023.gpkg", layer_name=border_name)
    crs_dist = target_crs # set target crs for distance calculations in meters
    # compute centroids
    centroids = gdf[["cell_id", "geometry"]].copy().to_crs(crs_dist)
    centroids["geometry"] = centroids.geometry.centroid
    # drop empties + make valid
    borders = borders.to_crs(crs_dist)
    borders = borders[borders.geometry.notna() & ~borders.geometry.is_empty].copy()
    borders["geometry"] = borders.geometry.make_valid()
    # columns for m and km measurements
    col_m  = f"dist_{border_name}_m"
    col_km = f"dist_{border_name}_km"
    gdf = gdf.drop(columns=[col_m, col_km], errors="ignore")
    # nearest border distance using spatial index)
    nearest = gpd.sjoin_nearest(centroids,borders[["geometry"]],how="left",distance_col=col_m)
    # attach back to grid
    dist_by_cell = nearest.groupby("cell_id")[col_m].min()
    gdf[col_m] = gdf["cell_id"].map(dist_by_cell.to_dict())
    gdf[col_km] = gdf[col_m] / 1000.0
    return gdf

##### Regressions #####
