#options("install.lock"=FALSE)
#install.packages("lidR")
#install.packages("terra")
#install.packages("RCSF")
setwd("C:/Users/theoj/OneDrive - The University of Montana/Documents/UMT/Courses/FORS491/")

library(lidR)
library(terra)
library(RCSF)

LASfile <- ("Lab 8/Data/USGS_LPC_MT_Statewide_Phase3_2021_B21_299301.laz")
las <- readLAS(LASfile)
plot(las)
print(las)
las_check(las)

?rasterize_terrain
?classify_ground
?clip
?clip_circle

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
dtm_knnidw <- rasterize_terrain(las, algorithm = knnidw(k = 6, p = 2))

plot_dtm3d(dtm_knnidw)
plot_dtm3d(dtm_tin)

plot(dtm_tin, main = "TIN")
plot(dtm_knnidw, main = "KNNIDW")

# clip <- clip_circle(las, Eastings, Northings, radius)
clip194 <- clip_circle(las, 299454.3753, 301583.4606, 30)
clip202 <- clip_circle(las, 299009.7522, 301138.0756, 30)
plot(clip194)
plot(clip202)



# --- Progressive Morphological Filter (PMF) schedule ---
ws <- c(2, 4, 6, 8, 10, 12, 16)           # window sizes (m) used progressively from fine → broad
th <- c(0.15, 0.25, 0.35, 0.50, 0.80, 1.20, 1.80)  # paired elevation thresholds (m), small→large

pmf194 <- classify_ground(clip194, pmf(ws, th))    # classify ground for LAS 'clip194' using PMF
pmf202 <- classify_ground(clip202, pmf(ws, th))    # same PMF settings for 'clip202'

# --- Cloth Simulation Filter (CSF) preset ---
mycsf <- csf(
  sloop_smooth     = TRUE,   # smooths the cloth on slopes to reduce false non-ground on inclines
  class_threshold  = 0.5,    # (m) vertical cutoff between ground / non-ground after cloth fit
  cloth_resolution = 0.5,    # (m) spacing of the simulated cloth grid; finer captures micro-relief
  rigidness        = 2,      # stiffer cloth; resists sagging into gullies/ditches
  time_step        = 0.65    # stable physics step (good balance of speed/accuracy)
)

csf194 <- classify_ground(clip194, mycsf)        # classify ground for 'clip194' with CSF
csf202 <- classify_ground(clip202, mycsf)        # classify ground for 'clip202' with the same CSF

plot(pmf194, color = "Classification")
plot(csf194, color = "Classification")
plot(pmf202, color = "Classification")
plot(csf202, color = "Classification")

dtmPMF194 = rasterize_terrain(pmf194, algorithm = tin())
plot_dtm3d(dtmPMF194)
dtmCSF194 = rasterize_terrain(csf194, algorithm = tin())
plot_dtm3d(dtmCSF194)
dtmPMF202 = rasterize_terrain(pmf202, algorithm = tin())
plot_dtm3d(dtmPMF202)
dtmCSF202 = rasterize_terrain(csf202, algorithm = tin())
plot_dtm3d(dtmCSF202)

#writeLAS(pmf194, file = "pmf194.laz")

# read original TLS data
tls194 <- readLAS("Lab 8/Data/TLS_194_6514.laz")
tls202 <- readLAS("Lab 8/Data/TLS_202_6514.laz")
# clip to 30m radius plots
TLSclip194 <- clip_circle(tls194, 299454.3753, 301583.4606, 30)
TLSclip202 <- clip_circle(tls202, 299009.7522, 301138.0756, 30)
# fast ground-leaning thinning on copies (lowest-per-voxel thins strongly but tends to preserve near-ground structure on flat plots)
TLSclip194_fast <- decimate_points(TLSclip194, lowest_attribute_per_voxel(res = 0.2, attribute = "Z"))
TLSclip202_fast <- decimate_points(TLSclip202, lowest_attribute_per_voxel(res = 0.2, attribute = "Z"))
# Refined CSF (finer cloth, a bit stiffer, stable time step; good detail but still quick on 30m plots)
mycsf <- csf(
  sloop_smooth = TRUE,
  class_threshold = 0.20,
  cloth_resolution = 0.25,
  rigidness = 3,
  time_step = 0.60
)
# classify
csf194TLS <- classify_ground(TLSclip194_fast, mycsf)
csf202TLS <- classify_ground(TLSclip202_fast, mycsf)
plot(csf194TLS, color = "Classification")
plot(csf202TLS, color = "Classification")
# Build DTMs from the classified (thinned) csf clouds
dtm194 <- rasterize_terrain(csf194TLS, algorithm = knnidw(k = 8, p=2), res = 0.2)
dtm202 <- rasterize_terrain(csf202TLS, algorithm = knnidw(k = 8, p=2), res = 0.2)
plot_dtm3d(dtm194)
plot_dtm3d(dtm202)
# save DTMs as GeoTIFFS
writeRaster(dtm194, "Lab 8/Data/TLS_194_DTM_02.tif", overwrite = TRUE)
writeRaster(dtm202, "Lab 8/Data/TLS_202_DTM_02.tif", overwrite = TRUE)
# save classified clips as LAZ
writeLAS(csf194TLS, "Lab 8/Data/TLS_194_6514_ground.laz")
writeLAS(csf202TLS, "Lab 8/Data/TLS_202_6514_ground.laz")

# read original MLS data
mls194 <- readLAS("Lab 8/Data/MLS_194_6514.laz")
mls202 <- readLAS("Lab 8/Data/MLS_202_6514.laz")
# clip to 30m radius plots
MLSclip194 <- clip_circle(mls194, 299454.3753, 301583.4606, 30)
MLSclip202 <- clip_circle(mls202, 299009.7522, 301138.0756, 30)
# fast ground-leaning thinning on copies (lowest-per-voxel thins strongly but tends to preserve near-ground structure on flat plots)
MLSclip194_fast <- decimate_points(MLSclip194, lowest_attribute_per_voxel(res = 0.2, attribute = "Z"))
MLSclip202_fast <- decimate_points(MLSclip202, lowest_attribute_per_voxel(res = 0.2, attribute = "Z"))
# Refined CSF (finer cloth, a bit stiffer, stable time step; good detail but still quick on 30m plots)
mycsf <- csf(
  sloop_smooth = TRUE,
  class_threshold = 0.20,
  cloth_resolution = 0.25,
  rigidness = 3,
  time_step = 0.60
)
# classify
csf194MLS <- classify_ground(MLSclip194_fast, mycsf)
csf202MLS <- classify_ground(MLSclip202_fast, mycsf)
plot(csf194MLS, color = "Classification")
plot(csf202MLS, color = "Classification")
# Build DTMs from the classified (thinned) csf clouds
dtm194_mls <- rasterize_terrain(csf194MLS, algorithm = knnidw(k = 8, p=2), res = 0.2)
dtm202_mls <- rasterize_terrain(csf202MLS, algorithm = knnidw(k = 8, p=2), res = 0.2)
plot_dtm3d(dtm194_mls)
plot_dtm3d(dtm202_mls)
# save DTMs as GeoTIFFS
writeRaster(dtm194_mls, "Lab 8/Data/MLS_194_DTM_02.tif", overwrite = TRUE)
writeRaster(dtm202_mls, "Lab 8/Data/MLS_202_DTM_02.tif", overwrite = TRUE)
# save classified clips as LAZ
writeLAS(csf194MLS, "Lab 8/Data/MLS_194_6514_ground.laz")
writeLAS(csf202MLS, "Lab 8/Data/MLS_202_6514_ground.laz")


# read original dDAP data
ddap194 <- readLAS("Lab 8/Data/dDAP194_6514.laz")
ddap202 <- readLAS("Lab 8/Data/dDAP202_6514.laz")
# clip to 30m radius plots
DDAPclip194 <- clip_circle(ddap194, 299454.3753, 301583.4606, 30)
DDAPclip202 <- clip_circle(ddap202, 299009.7522, 301138.0756, 30)
# fast ground-leaning thinning on copies (lowest-per-voxel thins strongly but tends to preserve near-ground structure on flat plots)
DDAPclip194_fast <- decimate_points(DDAPclip194, lowest_attribute_per_voxel(res = 0.2, attribute = "Z"))
DDAPclip202_fast <- decimate_points(DDAPclip202, lowest_attribute_per_voxel(res = 0.2, attribute = "Z"))
# Refined CSF (finer cloth, a bit stiffer, stable time step; good detail but still quick on 30m plots)
mycsf <- csf(
  sloop_smooth = TRUE,
  class_threshold = 0.20,
  cloth_resolution = 0.25,
  rigidness = 3,
  time_step = 0.60
)
# classify
csf194DDAP <- classify_ground(DDAPclip194_fast, mycsf)
csf202DDAP <- classify_ground(DDAPclip202_fast, mycsf)
plot(csf194DDAP, color = "Classification")
plot(csf202DDAP, color = "Classification")
# Build DTMs from the classified (thinned) csf clouds
dtm194_ddap <- rasterize_terrain(csf194DDAP, algorithm = knnidw(k = 8, p=2), res = 0.2)
dtm202_ddap <- rasterize_terrain(csf202DDAP, algorithm = knnidw(k = 8, p=2), res = 0.2)
plot_dtm3d(dtm194_ddap)
plot_dtm3d(dtm202_ddap)
# save DTMs as GeoTIFFS
writeRaster(dtm194_ddap, "Lab 8/Data/dDAP_194_DTM_02.tif", overwrite = TRUE)
writeRaster(dtm202_ddap, "Lab 8/Data/dDAP_202_DTM_02.tif", overwrite = TRUE)
# save classified clips as LAZ
writeLAS(csf194DDAP, "Lab 8/Data/dDAP_194_6514_ground.laz")
writeLAS(csf202DDAP, "Lab 8/Data/dDAP_202_6514_ground.laz")

# read original full-density inputs (TLS, MLS, dDAP)
tls194 <- readLAS("Lab 8/Data/TLS_194_6514.laz")
tls202 <- readLAS("Lab 8/Data/TLS_202_6514.laz")
mls194 <- readLAS("Lab 8/Data/MLS_194_6514.laz")
mls202 <- readLAS("Lab 8/Data/MLS_202_6514.laz")
ddap194 <- readLAS("Lab 8/Data/dDAP194_6514.laz")
ddap202 <- readLAS("Lab 8/Data/dDAP202_6514.laz")
# clip each dataset to a 30m radius plot
TLSclip194 <- clip_circle(tls194, 299454.3753, 301583.4606, 30)
TLSclip202 <- clip_circle(tls202, 299009.7522, 301138.0756, 30)
MLSclip194 <- clip_circle(mls194, 299454.3753, 301583.4606, 30)
MLSclip202 <- clip_circle(mls202, 299009.7522, 301138.0756, 30)
dDAPclip194 <- clip_circle(ddap194, 299454.3753, 301583.4606, 30)
dDAPclip202 <- clip_circle(ddap202, 299009.7522, 301138.0756, 30)
# load DTMs built earlier (one per plot) for normalization
dtm194 <- terra::rast("Lab 8/Data/TLS_194_DTM_02.tif")
dtm202 <- terra::rast("Lab 8/Data/TLS_202_DTM_02.tif")
# normalize: convert Z to Height-Above-Ground (HAG) using the DTMs
TLSclip194_norm <- normalize_height(TLSclip194, dtm194)
TLSclip202_norm <- normalize_height(TLSclip202, dtm202)
MLSclip194_norm <- normalize_height(MLSclip194, dtm194)
MLSclip202_norm <- normalize_height(MLSclip202, dtm202)
dDAPclip194_norm <- normalize_height(dDAPclip194, dtm194)
dDAPclip202_norm <- normalize_height(dDAPclip202, dtm202)
# classify ground in the normalized clouds by HAG
TLSclip194_norm@data$Classification <- 1L
TLSclip194_norm@data$Classification[ TLSclip194_norm@data$Z <= 0.15 ] <- 2L
TLSclip202_norm@data$Classification <- 1L
TLSclip202_norm@data$Classification[ TLSclip202_norm@data$Z <= 0.15 ] <- 2L
MLSclip194_norm@data$Classification <- 1L
MLSclip194_norm@data$Classification[ MLSclip194_norm@data$Z <= 0.15 ] <- 2L
MLSclip202_norm@data$Classification <- 1L
MLSclip202_norm@data$Classification[ MLSclip202_norm@data$Z <= 0.15 ] <- 2L
dDAPclip194_norm@data$Classification <- 1L
dDAPclip194_norm@data$Classification[ dDAPclip194_norm@data$Z <= 0.15 ] <- 2L
dDAPclip202_norm@data$Classification <- 1L
dDAPclip202_norm@data$Classification[ dDAPclip202_norm@data$Z <= 0.15 ] <- 2L
# save the normalized + ground-classified point clouds (LAZ compression)
writeLAS(TLSclip194_norm, "Lab 8/Data/TLS_194_6514_norm_ground.laz")
writeLAS(TLSclip202_norm, "Lab 8/Data/TLS_202_6514_norm_ground.laz")
writeLAS(MLSclip194_norm, "Lab 8/Data/MLS_194_6514_norm_ground.laz")
writeLAS(MLSclip202_norm, "Lab 8/Data/MLS_202_6514_norm_ground.laz")
writeLAS(dDAPclip194_norm, "Lab 8/Data/dDAP_194_6514_norm_ground.laz")
writeLAS(dDAPclip202_norm, "Lab 8/Data/dDAP_202_6514_norm_ground.laz")
