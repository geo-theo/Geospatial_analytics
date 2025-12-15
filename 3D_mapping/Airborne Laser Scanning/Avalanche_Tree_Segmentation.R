# Install Dependencies
setwd("C:/Users/theoj/Files/GIS/FORS491_Final_Project")
#options("install.lock"=FALSE)
#install.packages("lidR")
#install.packages("terra")
#install.packages("RCSF")
#install.packages("rgl")
#install.packages("sf")
#install.packages("mapview")
library(lidR)
library(terra)
library(RCSF)
library(rgl)
library(sf)
library(mapview)

#######################################
#######################################
##### Avalanche Tree Segmentation #####
#######################################
#######################################

#########################
### Prepare ALS Tiles ###
#########################
# Import 2018 tile in EPSG:6341
ALS_2018 <- readLAS("Data/LAZ/UT_2018_MonitorBowl_EPSG6341.laz")
plot(ALS_2018)
print(ALS_2018)
# Import 2023 tile in EPSG:6350 - reproject to EPSG:6341+5703
ALS_2023_6530 <- readLAS("Data/LAZ/UT_2023_MonitorBowl_EPSG6350.laz")
target <- st_crs("EPSG:6341+5703")
ALS_2023 <- sf::st_transform(ALS_2023_6530, target)
plot(ALS_2023)
print(ALS_2023)

###########################################
### Create Digital Terrain Models (DTM) ###
###########################################
# Build DTM from ground-classified points in ALS for 2018
ALS_2018_ground <- filter_poi(ALS_2018, Classification == 2)
dtm_tin_2018 <- rasterize_terrain(ALS_2018_ground, res = 1, algorithm = tin())
plot(dtm_tin_2018, main = "2018 DTM (TIN)")
plot_dtm3d(dtm_tin_2018)
writeRaster(dtm_tin_2018, "Data/DTM/DTM_2018_tin.tif", overwrite = TRUE)
# Build DTM from ground-classified points in ALS for 2023
ALS_2023_ground <- filter_poi(ALS_2023, Classification == 2)
dtm_tin_2023 <- rasterize_terrain(ALS_2023_ground, res = 1, algorithm = tin())
plot(dtm_tin_2023, main = "2023 DTM (TIN)")
plot_dtm3d(dtm_tin_2023)
writeRaster(dtm_tin_2023, "Data/DTM/DTM_2023_tin.tif", overwrite = TRUE)

#################################################
### Parameters for Canopy Height Models (CHM) ###
#################################################
# set appropriate input parameters
chm_resolution = 0.5
chm_algo <- pitfree(thresholds = c(0, 2, 5, 10, 15), subcircle = 0.5)
variable_windows <- function(x) { x * 0.1 + 3 }
w_matrix <- matrix(1, 5, 5)

### Parameters for Tree Identification Algorithms ###
ttop_hmin <- 1
ttop_tall_hmin <- 5
ttop_newgrowthmask_hmin <- 2

############################
### Tree Counts per Plot ###
############################
tree_counts <- data.frame(
  plot = paste0("Plot", 1:6),
  n_2018_over1m = integer(6),
  n_2018_over5m = integer(6),
  n_2018_newgrowthmask = integer(6),
  n_2023_over1m = integer(6),
  n_2023_over5m = integer(6),
  n_2023_newgrowthmask = integer(6),
  stringsAsFactors = FALSE
)


##########
# Plot 1 #
##########
# Create circle plot clips with 50m radius for 2018
Plot1_2018 <- clip_circle(ALS_2018, 451536.4604, 4498151.6564, 50)
# Create circle plot clips with 50m radius for 2023
Plot1_2023 <- clip_circle(ALS_2023, 451536.4604, 4498151.6564, 50)

### Height Normalization ###
# Normalize 2018 plots
Plot1_2018_norm <- normalize_height(Plot1_2018, dtm_tin_2018)
# Normalize 2023 plots
Plot1_2023_norm <- normalize_height(Plot1_2023, dtm_tin_2023)

### Reclassify ground for normalized plots ###
# 2018 reclassification for ground within 15cm of Z=0
Plot1_2018_norm@data$Classification <- 1L; Plot1_2018_norm@data$Classification[Plot1_2018_norm@data$Z <= 0.15] <- 2L
# 2023 reclassification for ground within 15cm of Z=0
Plot1_2023_norm@data$Classification <- 1L; Plot1_2023_norm@data$Classification[Plot1_2023_norm@data$Z <= 0.15] <- 2L

# 2018 tree detection
Plot1_2018_chm <- rasterize_canopy(Plot1_2018_norm, chm_resolution, chm_algo)
Plot1_2018_chm_smooth <- raster::focal(Plot1_2018_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot1_2018_chm_ttops <- locate_trees(Plot1_2018_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot1_2018_chm_ttops_over5m <- subset(Plot1_2018_chm_ttops, Z >= ttop_tall_hmin)
Plot1_2018_chm_ttops_newgrowthmask <- subset(Plot1_2018_chm_ttops, Z >= ttop_hmin)
# 2023 tree detection
Plot1_2023_chm <- rasterize_canopy(Plot1_2023_norm, chm_resolution, chm_algo)
Plot1_2023_chm_smooth <- raster::focal(Plot1_2023_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot1_2023_chm_ttops <- locate_trees(Plot1_2023_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot1_2023_chm_ttops_over5m <- subset(Plot1_2023_chm_ttops, Z >= ttop_tall_hmin)
Plot1_2023_chm_ttops_newgrowthmask <- subset(Plot1_2023_chm_ttops, Z >= ttop_newgrowthmask_hmin)


# save counts to dataframe
n_2018_over1m<-nrow(Plot1_2018_chm_ttops);n_2018_over5m<-nrow(Plot1_2018_chm_ttops_over5m);n_2018_newgrowthmask<-nrow(Plot1_2018_chm_ttops_newgrowthmask);n_2023_over1m<-nrow(Plot1_2023_chm_ttops);n_2023_over5m<-nrow(Plot1_2023_chm_ttops_over5m);n_2023_newgrowthmask<-nrow(Plot1_2023_chm_ttops_newgrowthmask);tree_counts[tree_counts$plot=="Plot1",2:7]<-c(n_2018_over1m,n_2018_over5m,n_2018_newgrowthmask,n_2023_over1m,n_2023_over5m,n_2023_newgrowthmask)

# plot tree detection counts
n_2018_over1m <- nrow(Plot1_2018_chm_ttops)
n_2018_over5m <- nrow(Plot1_2018_chm_ttops_over5m)
n_2018_newgrowthmask <- nrow(Plot1_2018_chm_ttops_newgrowthmask)
n_2023_over1m <- nrow(Plot1_2023_chm_ttops)
n_2023_over5m <- nrow(Plot1_2023_chm_ttops_over5m)
n_2023_newgrowthmask <- nrow(Plot1_2023_chm_ttops_newgrowthmask)
# Plot 2018 counts over 1m
plot(Plot1_2018_chm_smooth, col = height.colors(20),
     main = "Plot 1 (2018) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot1_2018_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts over 5m
plot(Plot1_2018_chm_smooth, col = height.colors(20),
     main = "Plot 1 (2018) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot1_2018_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2018_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts masking 2020-2023 growth
plot(Plot1_2018_chm_smooth, col = height.colors(20),
     main = "Plot 1 (2018) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot1_2018_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 1m
plot(Plot1_2023_chm_smooth, col = height.colors(20),
     main = "Plot 1 (2023) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot1_2023_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2023_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 5m
plot(Plot1_2023_chm_smooth, col = height.colors(20),
     main = "Plot 1 (2023) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot1_2023_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2023_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts masking 2020-2023 growth
plot(Plot1_2023_chm_smooth, col = height.colors(20),
     main = "Plot 1 (2023) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot1_2023_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_newgrowthmask_hmin, " m: ", n_2023_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 tree tops on 3d render
Tops_PLot1_2018_norm <- plot(Plot1_2018_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot1_2018_norm, Plot1_2018_chm_ttops_newgrowthmask)
# Plot 2023 tree tops on 3d render
Tops_PLot1_2023_norm <- plot(Plot1_2023_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot1_2023_norm, Plot1_2023_chm_ttops_newgrowthmask)


##########
# Plot 2 #
##########
# Create circle plot clips with 30m radius for 2018
Plot2_2018 <- clip_circle(ALS_2018, 451559.65, 4498227.6045, 30)
# Create circle plot clips with 30m radius for 2023
Plot2_2023 <- clip_circle(ALS_2023, 451559.65, 4498227.6045, 30)

### Height Normalization ###
# Normalize 2018 plots
Plot2_2018_norm <- normalize_height(Plot2_2018, dtm_tin_2018)
# Normalize 2023 plots
Plot2_2023_norm <- normalize_height(Plot2_2023, dtm_tin_2023)

### Reclassify ground for normalized plots ###
# 2018 reclassification for ground within 15cm of Z=0
Plot2_2018_norm@data$Classification <- 1L; Plot2_2018_norm@data$Classification[Plot2_2018_norm@data$Z <= 0.15] <- 2L
# 2023 reclassification for ground within 15cm of Z=0
Plot2_2023_norm@data$Classification <- 1L; Plot2_2023_norm@data$Classification[Plot2_2023_norm@data$Z <= 0.15] <- 2L

# 2018 tree detection
Plot2_2018_chm <- rasterize_canopy(Plot2_2018_norm, chm_resolution, chm_algo)
Plot2_2018_chm_smooth <- raster::focal(Plot2_2018_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot2_2018_chm_ttops <- locate_trees(Plot2_2018_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot2_2018_chm_ttops_over5m <- subset(Plot2_2018_chm_ttops, Z >= ttop_tall_hmin)
Plot2_2018_chm_ttops_newgrowthmask <- subset(Plot2_2018_chm_ttops, Z >= ttop_hmin)
# 2023 tree detection
Plot2_2023_chm <- rasterize_canopy(Plot2_2023_norm, chm_resolution, chm_algo)
Plot2_2023_chm_smooth <- raster::focal(Plot2_2023_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot2_2023_chm_ttops <- locate_trees(Plot2_2023_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot2_2023_chm_ttops_over5m <- subset(Plot2_2023_chm_ttops, Z >= ttop_tall_hmin)
Plot2_2023_chm_ttops_newgrowthmask <- subset(Plot2_2023_chm_ttops, Z >= ttop_newgrowthmask_hmin)

# save counts to dataframe
n_2018_over1m<-nrow(Plot2_2018_chm_ttops);n_2018_over5m<-nrow(Plot2_2018_chm_ttops_over5m);n_2018_newgrowthmask<-nrow(Plot2_2018_chm_ttops_newgrowthmask);n_2023_over1m<-nrow(Plot2_2023_chm_ttops);n_2023_over5m<-nrow(Plot2_2023_chm_ttops_over5m);n_2023_newgrowthmask<-nrow(Plot2_2023_chm_ttops_newgrowthmask);tree_counts[tree_counts$plot=="Plot2",2:7]<-c(n_2018_over1m,n_2018_over5m,n_2018_newgrowthmask,n_2023_over1m,n_2023_over5m,n_2023_newgrowthmask)

# plot tree detection counts
n_2018_over1m <- nrow(Plot2_2018_chm_ttops)
n_2018_over5m <- nrow(Plot2_2018_chm_ttops_over5m)
n_2018_newgrowthmask <- nrow(Plot2_2018_chm_ttops_newgrowthmask)
n_2023_over1m <- nrow(Plot2_2023_chm_ttops)
n_2023_over5m <- nrow(Plot2_2023_chm_ttops_over5m)
n_2023_newgrowthmask <- nrow(Plot2_2023_chm_ttops_newgrowthmask)
# Plot 2018 counts over 1m
plot(Plot2_2018_chm_smooth, col = height.colors(20),
     main = "Plot 2 (2018) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot2_2018_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts over 5m
plot(Plot2_2018_chm_smooth, col = height.colors(20),
     main = "Plot 2 (2018) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot2_2018_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2018_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts masking 2020-2023 growth
plot(Plot2_2018_chm_smooth, col = height.colors(20),
     main = "Plot 2 (2018) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot2_2018_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 1m
plot(Plot2_2023_chm_smooth, col = height.colors(20),
     main = "Plot 2 (2023) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot2_2023_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2023_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 5m
plot(Plot2_2023_chm_smooth, col = height.colors(20),
     main = "Plot 2 (2023) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot2_2023_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2023_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts masking 2020-2023 growth
plot(Plot2_2023_chm_smooth, col = height.colors(20),
     main = "Plot 2 (2023) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot2_2023_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_newgrowthmask_hmin, " m: ", n_2023_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 tree tops on 3d render
Tops_PLot2_2018_norm <- plot(Plot2_2018_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot2_2018_norm, Plot2_2018_chm_ttops_newgrowthmask)
# Plot 2023 tree tops on 3d render
Tops_PLot2_2023_norm <- plot(Plot2_2023_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot2_2023_norm, Plot2_2023_chm_ttops_newgrowthmask)

##########
# Plot 3 #
##########
# Create circle plot clips with 50m radius for 2018
Plot3_2018 <- clip_circle(ALS_2018, 451548.5833, 4498306.2127, 50)
# Create circle plot clips with 50m radius for 2023
Plot3_2023 <- clip_circle(ALS_2023, 451548.5833, 4498306.2127, 50)

### Height Normalization ###
# Normalize 2018 plots
Plot3_2018_norm <- normalize_height(Plot3_2018, dtm_tin_2018)
# Normalize 2023 plots
Plot3_2023_norm <- normalize_height(Plot3_2023, dtm_tin_2023)

### Reclassify ground for normalized plots ###
# 2018 reclassification for ground within 15cm of Z=0
Plot3_2018_norm@data$Classification <- 1L; Plot3_2018_norm@data$Classification[Plot3_2018_norm@data$Z <= 0.15] <- 2L
# 2023 reclassification for ground within 15cm of Z=0
Plot3_2023_norm@data$Classification <- 1L; Plot3_2023_norm@data$Classification[Plot3_2023_norm@data$Z <= 0.15] <- 2L

# 2018 tree detection
Plot3_2018_chm <- rasterize_canopy(Plot3_2018_norm, chm_resolution, chm_algo)
Plot3_2018_chm_smooth <- raster::focal(Plot3_2018_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot3_2018_chm_ttops <- locate_trees(Plot3_2018_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot3_2018_chm_ttops_over5m <- subset(Plot3_2018_chm_ttops, Z >= ttop_tall_hmin)
Plot3_2018_chm_ttops_newgrowthmask <- subset(Plot3_2018_chm_ttops, Z >= ttop_hmin)
# 2023 tree detection
Plot3_2023_chm <- rasterize_canopy(Plot3_2023_norm, chm_resolution, chm_algo)
Plot3_2023_chm_smooth <- raster::focal(Plot3_2023_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot3_2023_chm_ttops <- locate_trees(Plot3_2023_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot3_2023_chm_ttops_over5m <- subset(Plot3_2023_chm_ttops, Z >= ttop_tall_hmin)
Plot3_2023_chm_ttops_newgrowthmask <- subset(Plot3_2023_chm_ttops, Z >= ttop_newgrowthmask_hmin)

# save counts to dataframe
n_2018_over1m<-nrow(Plot3_2018_chm_ttops);n_2018_over5m<-nrow(Plot3_2018_chm_ttops_over5m);n_2018_newgrowthmask<-nrow(Plot3_2018_chm_ttops_newgrowthmask);n_2023_over1m<-nrow(Plot3_2023_chm_ttops);n_2023_over5m<-nrow(Plot3_2023_chm_ttops_over5m);n_2023_newgrowthmask<-nrow(Plot3_2023_chm_ttops_newgrowthmask);tree_counts[tree_counts$plot=="Plot3",2:7]<-c(n_2018_over1m,n_2018_over5m,n_2018_newgrowthmask,n_2023_over1m,n_2023_over5m,n_2023_newgrowthmask)

# plot tree detection counts
n_2018_over1m <- nrow(Plot3_2018_chm_ttops)
n_2018_over5m <- nrow(Plot3_2018_chm_ttops_over5m)
n_2018_newgrowthmask <- nrow(Plot3_2018_chm_ttops_newgrowthmask)
n_2023_over1m <- nrow(Plot3_2023_chm_ttops)
n_2023_over5m <- nrow(Plot3_2023_chm_ttops_over5m)
n_2023_newgrowthmask <- nrow(Plot3_2023_chm_ttops_newgrowthmask)
# Plot 2018 counts over 1m
plot(Plot3_2018_chm_smooth, col = height.colors(20),
     main = "Plot 3 (2018) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot3_2018_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts over 5m
plot(Plot3_2018_chm_smooth, col = height.colors(20),
     main = "Plot 3 (2018) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot3_2018_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2018_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts masking 2020-2023 growth
plot(Plot3_2018_chm_smooth, col = height.colors(20),
     main = "Plot 3 (2018) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot3_2018_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 1m
plot(Plot3_2023_chm_smooth, col = height.colors(20),
     main = "Plot 3 (2023) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot3_2023_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2023_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 5m
plot(Plot3_2023_chm_smooth, col = height.colors(20),
     main = "Plot 3 (2023) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot3_2023_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2023_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts masking 2020-2023 growth
plot(Plot3_2023_chm_smooth, col = height.colors(20),
     main = "Plot 3 (2023) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot3_2023_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_newgrowthmask_hmin, " m: ", n_2023_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 tree tops on 3d render
Tops_PLot3_2018_norm <- plot(Plot3_2018_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot3_2018_norm, Plot3_2018_chm_ttops_newgrowthmask)
# Plot 2023 tree tops on 3d render
Tops_PLot3_2023_norm <- plot(Plot3_2023_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot3_2023_norm, Plot3_2023_chm_ttops_newgrowthmask)

##########
# Plot 4 #
##########
# Create circle plot clips with 50m radius for 2018
Plot4_2018 <- clip_circle(ALS_2018, 451645.6464, 4498340.2852, 25)
# Create circle plot clips with 50m radius for 2023
Plot4_2023 <- clip_circle(ALS_2023, 451645.6464, 4498340.2852, 25)

### Height Normalization ###
# Normalize 2018 plots
Plot4_2018_norm <- normalize_height(Plot4_2018, dtm_tin_2018)
# Normalize 2023 plots
Plot4_2023_norm <- normalize_height(Plot4_2023, dtm_tin_2023)

### Reclassify ground for normalized plots ###
# 2018 reclassification for ground within 15cm of Z=0
Plot4_2018_norm@data$Classification <- 1L; Plot4_2018_norm@data$Classification[Plot4_2018_norm@data$Z <= 0.15] <- 2L
# 2023 reclassification for ground within 15cm of Z=0
Plot4_2023_norm@data$Classification <- 1L; Plot4_2023_norm@data$Classification[Plot4_2023_norm@data$Z <= 0.15] <- 2L

# 2018 tree detection
Plot4_2018_chm <- rasterize_canopy(Plot4_2018_norm, chm_resolution, chm_algo)
Plot4_2018_chm_smooth <- raster::focal(Plot4_2018_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot4_2018_chm_ttops <- locate_trees(Plot4_2018_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot4_2018_chm_ttops_over5m <- subset(Plot4_2018_chm_ttops, Z >= ttop_tall_hmin)
Plot4_2018_chm_ttops_newgrowthmask <- subset(Plot4_2018_chm_ttops, Z >= ttop_hmin)
# 2023 tree detection
Plot4_2023_chm <- rasterize_canopy(Plot4_2023_norm, chm_resolution, chm_algo)
Plot4_2023_chm_smooth <- raster::focal(Plot4_2023_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot4_2023_chm_ttops <- locate_trees(Plot4_2023_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot4_2023_chm_ttops_over5m <- subset(Plot4_2023_chm_ttops, Z >= ttop_tall_hmin)
Plot4_2023_chm_ttops_newgrowthmask <- subset(Plot4_2023_chm_ttops, Z >= ttop_newgrowthmask_hmin)

# save counts to dataframe
n_2018_over1m<-nrow(Plot4_2018_chm_ttops);n_2018_over5m<-nrow(Plot4_2018_chm_ttops_over5m);n_2018_newgrowthmask<-nrow(Plot4_2018_chm_ttops_newgrowthmask);n_2023_over1m<-nrow(Plot4_2023_chm_ttops);n_2023_over5m<-nrow(Plot4_2023_chm_ttops_over5m);n_2023_newgrowthmask<-nrow(Plot4_2023_chm_ttops_newgrowthmask);tree_counts[tree_counts$plot=="Plot4",2:7]<-c(n_2018_over1m,n_2018_over5m,n_2018_newgrowthmask,n_2023_over1m,n_2023_over5m,n_2023_newgrowthmask)

# plot tree detection counts
n_2018_over1m <- nrow(Plot4_2018_chm_ttops)
n_2018_over5m <- nrow(Plot4_2018_chm_ttops_over5m)
n_2018_newgrowthmask <- nrow(Plot4_2018_chm_ttops_newgrowthmask)
n_2023_over1m <- nrow(Plot4_2023_chm_ttops)
n_2023_over5m <- nrow(Plot4_2023_chm_ttops_over5m)
n_2023_newgrowthmask <- nrow(Plot4_2023_chm_ttops_newgrowthmask)
# Plot 2018 counts over 1m
plot(Plot4_2018_chm_smooth, col = height.colors(20),
     main = "Plot 4 (2018) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot4_2018_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts over 5m
plot(Plot4_2018_chm_smooth, col = height.colors(20),
     main = "Plot 4 (2018) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot4_2018_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2018_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts masking 2020-2023 growth
plot(Plot4_2018_chm_smooth, col = height.colors(20),
     main = "Plot 4 (2018) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot4_2018_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 1m
plot(Plot4_2023_chm_smooth, col = height.colors(20),
     main = "Plot 4 (2023) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot4_2023_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2023_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 5m
plot(Plot4_2023_chm_smooth, col = height.colors(20),
     main = "Plot 4 (2023) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot4_2023_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2023_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts masking 2020-2023 growth
plot(Plot4_2023_chm_smooth, col = height.colors(20),
     main = "Plot 4 (2023) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot4_2023_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_newgrowthmask_hmin, " m: ", n_2023_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 tree tops on 3d render
Tops_PLot4_2018_norm <- plot(Plot4_2018_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot4_2018_norm, Plot4_2018_chm_ttops_newgrowthmask)
# Plot 2023 tree tops on 3d render
Tops_PLot4_2023_norm <- plot(Plot4_2023_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot4_2023_norm, Plot4_2023_chm_ttops_newgrowthmask)

##########
# Plot 5 #
##########
# Create circle plot clips with 50m radius for 2018
Plot5_2018 <- clip_circle(ALS_2018, 451672.7979, 4498270.3657, 50)
# Create circle plot clips with 50m radius for 2023
Plot5_2023 <- clip_circle(ALS_2023, 451672.7979, 4498270.3657, 50)

### Height Normalization ###
# Normalize 2018 plots
Plot5_2018_norm <- normalize_height(Plot5_2018, dtm_tin_2018)
# Normalize 2023 plots
Plot5_2023_norm <- normalize_height(Plot5_2023, dtm_tin_2023)

### Reclassify ground for normalized plots ###
# 2018 reclassification for ground within 15cm of Z=0
Plot5_2018_norm@data$Classification <- 1L; Plot5_2018_norm@data$Classification[Plot5_2018_norm@data$Z <= 0.15] <- 2L
# 2023 reclassification for ground within 15cm of Z=0
Plot5_2023_norm@data$Classification <- 1L; Plot5_2023_norm@data$Classification[Plot5_2023_norm@data$Z <= 0.15] <- 2L

# 2018 tree detection
Plot5_2018_chm <- rasterize_canopy(Plot5_2018_norm, chm_resolution, chm_algo)
Plot5_2018_chm_smooth <- raster::focal(Plot5_2018_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot5_2018_chm_ttops <- locate_trees(Plot5_2018_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot5_2018_chm_ttops_over5m <- subset(Plot5_2018_chm_ttops, Z >= ttop_tall_hmin)
Plot5_2018_chm_ttops_newgrowthmask <- subset(Plot5_2018_chm_ttops, Z >= ttop_hmin)
# 2023 tree detection
Plot5_2023_chm <- rasterize_canopy(Plot5_2023_norm, chm_resolution, chm_algo)
Plot5_2023_chm_smooth <- raster::focal(Plot5_2023_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot5_2023_chm_ttops <- locate_trees(Plot5_2023_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot5_2023_chm_ttops_over5m <- subset(Plot5_2023_chm_ttops, Z >= ttop_tall_hmin)
Plot5_2023_chm_ttops_newgrowthmask <- subset(Plot5_2023_chm_ttops, Z >= ttop_newgrowthmask_hmin)

# save counts to dataframe
n_2018_over1m<-nrow(Plot5_2018_chm_ttops);n_2018_over5m<-nrow(Plot5_2018_chm_ttops_over5m);n_2018_newgrowthmask<-nrow(Plot5_2018_chm_ttops_newgrowthmask);n_2023_over1m<-nrow(Plot5_2023_chm_ttops);n_2023_over5m<-nrow(Plot5_2023_chm_ttops_over5m);n_2023_newgrowthmask<-nrow(Plot5_2023_chm_ttops_newgrowthmask);tree_counts[tree_counts$plot=="Plot5",2:7]<-c(n_2018_over1m,n_2018_over5m,n_2018_newgrowthmask,n_2023_over1m,n_2023_over5m,n_2023_newgrowthmask)

# plot tree detection counts
n_2018_over1m <- nrow(Plot5_2018_chm_ttops)
n_2018_over5m <- nrow(Plot5_2018_chm_ttops_over5m)
n_2018_newgrowthmask <- nrow(Plot5_2018_chm_ttops_newgrowthmask)
n_2023_over1m <- nrow(Plot5_2023_chm_ttops)
n_2023_over5m <- nrow(Plot5_2023_chm_ttops_over5m)
n_2023_newgrowthmask <- nrow(Plot5_2023_chm_ttops_newgrowthmask)
# Plot 2018 counts over 1m
plot(Plot5_2018_chm_smooth, col = height.colors(20),
     main = "Plot 5 (2018) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot5_2018_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts over 5m
plot(Plot5_2018_chm_smooth, col = height.colors(20),
     main = "Plot 5 (2018) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot5_2018_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2018_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts masking 2020-2023 growth
plot(Plot5_2018_chm_smooth, col = height.colors(20),
     main = "Plot 5 (2018) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot5_2018_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 1m
plot(Plot5_2023_chm_smooth, col = height.colors(20),
     main = "Plot 5 (2023) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot5_2023_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2023_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 5m
plot(Plot5_2023_chm_smooth, col = height.colors(20),
     main = "Plot 5 (2023) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot5_2023_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2023_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts masking 2020-2023 growth
plot(Plot5_2023_chm_smooth, col = height.colors(20),
     main = "Plot 5 (2023) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot5_2023_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_newgrowthmask_hmin, " m: ", n_2023_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 tree tops on 3d render
Tops_PLot5_2018_norm <- plot(Plot5_2018_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot5_2018_norm, Plot5_2018_chm_ttops_newgrowthmask)
# Plot 2023 tree tops on 3d render
Tops_PLot5_2023_norm <- plot(Plot5_2023_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot5_2023_norm, Plot5_2023_chm_ttops_newgrowthmask)

##########
# Plot 6 #
##########
# Create circle plot clips with 50m radius for 2018
Plot6_2018 <- clip_circle(ALS_2018, 451609.2889, 4498099.3733, 50)
# Create circle plot clips with 50m radius for 2023
Plot6_2023 <- clip_circle(ALS_2023, 451609.2889, 4498099.3733, 50)

### Height Normalization ###
# Normalize 2018 plots
Plot6_2018_norm <- normalize_height(Plot6_2018, dtm_tin_2018)
# Normalize 2023 plots
Plot6_2023_norm <- normalize_height(Plot6_2023, dtm_tin_2023)

### Reclassify ground for normalized plots ###
# 2018 reclassification for ground within 15cm of Z=0
Plot6_2018_norm@data$Classification <- 1L; Plot6_2018_norm@data$Classification[Plot6_2018_norm@data$Z <= 0.15] <- 2L
# 2023 reclassification for ground within 15cm of Z=0
Plot6_2023_norm@data$Classification <- 1L; Plot6_2023_norm@data$Classification[Plot6_2023_norm@data$Z <= 0.15] <- 2L

# 2018 tree detection
Plot6_2018_chm <- rasterize_canopy(Plot6_2018_norm, chm_resolution, chm_algo)
Plot6_2018_chm_smooth <- raster::focal(Plot6_2018_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot6_2018_chm_ttops <- locate_trees(Plot6_2018_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot6_2018_chm_ttops_over5m <- subset(Plot6_2018_chm_ttops, Z >= ttop_tall_hmin)
Plot6_2018_chm_ttops_newgrowthmask <- subset(Plot6_2018_chm_ttops, Z >= ttop_hmin)
# 2023 tree detection
Plot6_2023_chm <- rasterize_canopy(Plot6_2023_norm, chm_resolution, chm_algo)
Plot6_2023_chm_smooth <- raster::focal(Plot6_2023_chm, w = w_matrix, fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot6_2023_chm_ttops <- locate_trees(Plot6_2023_chm_smooth, lmf(ws = variable_windows, hmin = ttop_hmin))
Plot6_2023_chm_ttops_over5m <- subset(Plot6_2023_chm_ttops, Z >= ttop_tall_hmin)
Plot6_2023_chm_ttops_newgrowthmask <- subset(Plot6_2023_chm_ttops, Z >= ttop_newgrowthmask_hmin)

# save counts to dataframe
n_2018_over1m<-nrow(Plot6_2018_chm_ttops);n_2018_over5m<-nrow(Plot6_2018_chm_ttops_over5m);n_2018_newgrowthmask<-nrow(Plot6_2018_chm_ttops_newgrowthmask);n_2023_over1m<-nrow(Plot6_2023_chm_ttops);n_2023_over5m<-nrow(Plot6_2023_chm_ttops_over5m);n_2023_newgrowthmask<-nrow(Plot6_2023_chm_ttops_newgrowthmask);tree_counts[tree_counts$plot=="Plot6",2:7]<-c(n_2018_over1m,n_2018_over5m,n_2018_newgrowthmask,n_2023_over1m,n_2023_over5m,n_2023_newgrowthmask)

# plot tree detection counts
n_2018_over1m <- nrow(Plot6_2018_chm_ttops)
n_2018_over5m <- nrow(Plot6_2018_chm_ttops_over5m)
n_2018_newgrowthmask <- nrow(Plot6_2018_chm_ttops_newgrowthmask)
n_2023_over1m <- nrow(Plot6_2023_chm_ttops)
n_2023_over5m <- nrow(Plot6_2023_chm_ttops_over5m)
n_2023_newgrowthmask <- nrow(Plot6_2023_chm_ttops_newgrowthmask)
# Plot 2018 counts over 1m
plot(Plot6_2018_chm_smooth, col = height.colors(20),
     main = "Plot 6 (2018) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot6_2018_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts over 5m
plot(Plot6_2018_chm_smooth, col = height.colors(20),
     main = "Plot 6 (2018) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot6_2018_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2018_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 counts masking 2020-2023 growth
plot(Plot6_2018_chm_smooth, col = height.colors(20),
     main = "Plot 6 (2018) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot6_2018_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2018_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 1m
plot(Plot6_2023_chm_smooth, col = height.colors(20),
     main = "Plot 6 (2023) CHM (smoothed) - Detected Trees >1m")
plot(sf::st_geometry(Plot6_2023_chm_ttops), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_hmin, " m: ", n_2023_over1m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts over 5m
plot(Plot6_2023_chm_smooth, col = height.colors(20),
     main = "Plot 6 (2023) CHM (smoothed) - Detected Trees >5m")
plot(sf::st_geometry(Plot6_2023_chm_ttops_over5m), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_tall_hmin, " m: ", n_2023_over5m),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2023 counts masking 2020-2023 growth
plot(Plot6_2023_chm_smooth, col = height.colors(20),
     main = "Plot 6 (2023) CHM (smoothed) - Detected Trees with new growth mask")
plot(sf::st_geometry(Plot6_2023_chm_ttops_newgrowthmask), add = TRUE,
     col = "black", cex = 2, pch = 2)
usr <- par("usr")
text(usr[1] + 0.02 * (usr[2] - usr[1]),
     usr[4] - 0.02 * (usr[4] - usr[3]),
     labels = paste0("Trees \u2265 ", ttop_newgrowthmask_hmin, " m: ", n_2023_newgrowthmask),
     adj = c(0, 1), cex = 0.9, col = "black")
# Plot 2018 tree tops on 3d render
Tops_PLot6_2018_norm <- plot(Plot6_2018_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot6_2018_norm, Plot6_2018_chm_ttops_newgrowthmask)
# Plot 2023 tree tops on 3d render
Tops_PLot6_2023_norm <- plot(Plot6_2023_norm, bg = "black", size = 2)
add_treetops3d(Tops_PLot6_2023_norm, Plot6_2023_chm_ttops_newgrowthmask)

###########################
# Tree segmentation by ID #
###########################
las<- readLAS("Data/LAZ/UT_2018_MonitorBowl_EPSG6341.laz")
ALSnorm <- normalize_height(las, tin())
chm <- rasterize_canopy(ALSnorm, res = 1, p2r())
plot(chm)
f <- function(x) { x * 0.05 + 2}
ttops <- locate_trees(chm, lmf(f))
ALSnorm <- segment_trees(ALSnorm, silva2016(chm, ttops))
writeLAS(ALSnorm, "Data/LAZ/UT_2018_MonitorBowl_EPSG6341_TreeID.laz")
plot(ALSnorm, color = "treeID")

las2023<- readLAS("Data/LAZ/UT_2023_MonitorBowl_EPSG6341.laz")
ALSnorm2023 <- normalize_height(las2023, tin())
chm2023 <- rasterize_canopy(ALSnorm2023, res = 1, p2r())
plot(chm2023)
f <- function(x) { x * 0.05 + 2}
ttops2023 <- locate_trees(chm2023, lmf(f))
ALSnorm2023 <- segment_trees(ALSnorm2023, silva2016(chm2023, ttops2023))
writeLAS(ALSnorm2023, "Data/LAZ/UT_2023_MonitorBowl_EPSG6341_TreeID.laz")
plot(ALSnorm2023, color = "treeID")



###############
### Results ###
###############

# barplot for trees over 1m per plot
counts_over1 <- rbind(tree_counts$n_2018_over1m, tree_counts$n_2023_over1m)
colnames(counts_over1) <- tree_counts$plot
barplot(counts_over1,
        beside = TRUE,
        names.arg = tree_counts$plot,
        col = c("blue", "red"),
        ylim = c(0, max(counts_over1) * 1.1),
        ylab = "Number of trees ≥ 1 m",
        main = "Trees ≥ 1 m per plot (2018 vs 2023)")
legend("topleft",
       legend = c("2018", "2023"),
       fill = c("blue", "red"),
       bty = "n")

# barplot for trees over 5m per plot
counts_over5 <- rbind(tree_counts$n_2018_over5m, tree_counts$n_2023_over5m)
colnames(counts_over5) <- tree_counts$plot
barplot(counts_over5,
        beside = TRUE,
        names.arg = tree_counts$plot,
        col = c("blue", "red"),
        ylim = c(0, max(counts_over5) * 1.1),
        ylab = "Number of trees ≥ 5 m",
        main = "Trees ≥ 5 m per plot (2018 vs 2023)")
legend("topleft",
       legend = c("2018", "2023"),
       fill = c("blue", "red"),
       bty = "n")

# barplot for new growth mask
counts_newgrowthmask <- rbind(tree_counts$n_2018_newgrowthmask, tree_counts$n_2023_newgrowthmask)
colnames(counts_newgrowthmask) <- tree_counts$plot
barplot(counts_newgrowthmask,
        beside = TRUE,
        names.arg = tree_counts$plot,
        col = c("blue", "red"),
        ylim = c(0, max(counts_newgrowthmask) * 1.1),
        ylab = "Number of trees with new growth mask",
        main = "Trees per plot (2018 vs 2023)")
legend("topleft",
       legend = c("2018", "2023"),
       fill = c("blue", "red"),
       bty = "n")

# barplot for combined plots
total_row <- data.frame(
  plot          = "Total",
  n_2018_over1m = sum(tree_counts$n_2018_over1m),
  n_2018_over5m = sum(tree_counts$n_2018_over5m),
  n_2023_over1m = sum(tree_counts$n_2023_over1m),
  n_2023_over5m = sum(tree_counts$n_2023_over5m),
  n_2018_newgrowthmask = sum(tree_counts$n_2018_newgrowthmask),
  n_2023_newgrowthmask = sum(tree_counts$n_2023_newgrowthmask)
)
tree_counts <- rbind(tree_counts, total_row)

totals <- tree_counts[tree_counts$plot == "Total", ]

tot_2018_over1 <- totals$n_2018_over1m
tot_2018_over5 <- totals$n_2018_over5m
tot_2023_over1 <- totals$n_2023_over1m
tot_2023_over5 <- totals$n_2023_over5m
tot_2018_newgrowthmask <- totals$n_2018_newgrowthmask
tot_2023_newgrowthmask <- totals$n_2023_newgrowthmask

counts_tot <- rbind(
  "≥ 1 m"          = c(tot_2018_over1,        tot_2023_over1),
  "≥ 5 m"          = c(tot_2018_over5,        tot_2023_over5),
  "New growth mask"= c(tot_2018_newgrowthmask,tot_2023_newgrowthmask)
)
colnames(counts_tot) <- c("2018", "2023")


barplot(t(counts_tot),
        beside = TRUE,
        col = c("blue", "red"),
        ylim = c(0, max(counts_tot) * 1.1),
        ylab = "Total number of trees (all plots)",
        main = "Total trees by height threshold (2018 vs 2023)",
        legend.text = TRUE,
        args.legend = list("topleft", fill = c("blue", "red"), bty = "n"))

###########################
### export metrics ###
###########################

# calculate percent change
tree_table_pct <- data.frame(
  Plot              = tree_counts$plot,
  "≥ 1 m (2018)"    = tree_counts$n_2018_over1m,
  "≥ 1 m (2023)"    = tree_counts$n_2023_over1m,
  "Δ ≥ 1 m (%)"     = round(100 * (tree_counts$n_2023_over1m - tree_counts$n_2018_over1m) / ifelse(tree_counts$n_2018_over1m == 0, NA, tree_counts$n_2018_over1m), 1),
  "≥ 5 m (2018)"    = tree_counts$n_2018_over5m,
  "≥ 5 m (2023)"    = tree_counts$n_2023_over5m,
  "Δ ≥ 5 m (%)"     = round(100 * (tree_counts$n_2023_over5m - tree_counts$n_2018_over5m) / ifelse(tree_counts$n_2018_over5m == 0, NA, tree_counts$n_2018_over5m), 1),
  "Mask (2018)"     = tree_counts$n_2018_newgrowthmask,
  "Mask (2023)"     = tree_counts$n_2023_newgrowthmask,
  "Δ Mask (%)"      = round(100 * (tree_counts$n_2023_newgrowthmask - tree_counts$n_2018_newgrowthmask) / ifelse(tree_counts$n_2018_newgrowthmask == 0, NA, tree_counts$n_2018_newgrowthmask), 1),
  check.names = FALSE
)
tree_table_pct

# Add coordinates
plot_meta <- data.frame(
  Plot      = paste0("Plot", 1:6),
  Easting   = c(451536.4604, 451559.65,   451548.5833, 451645.6464, 451672.7979, 451609.2889),
  Northing  = c(4498151.656, 4498227.605, 4498306.213, 4498340.285, 4498270.366, 4498099.373),
  Z         = c(2888.3767,   2853.5649,   2850.8657,   2820.2495,   2824.1584,   2894.5137),
  Plot_size = c(50,          30,          50,          25,          50,          50)
)
tree_table_pct_coords <- merge(tree_table_pct, plot_meta, by = "Plot", all.x = TRUE)
tree_table_pct_coords

# export csv
write.csv(tree_table_pct_coords, "Data/csv/avalanche_plots_tree_loss.csv",row.names = FALSE)
# export geopackage
dropped_totals_row <- subset(tree_table_pct_coords, !is.na(Easting))
plots_sf <- st_as_sf(dropped_totals_row,coords = c("Easting", "Northing"),crs = 6341)
st_write(plots_sf,"Data/gpkg/plots_tree_change.gpkg",layer = "plots_tree_change",delete_layer = TRUE)
#export LAZ files of plots
writeLAS(Plot1_2018, "Data/LAZ/LAZ_Plots/Plot1_6341.laz")
writeLAS(Plot2_2018, "Data/LAZ/LAZ_Plots/Plot2_6341.laz")
writeLAS(Plot3_2018, "Data/LAZ/LAZ_Plots/Plot3_6341.laz")
writeLAS(Plot4_2018, "Data/LAZ/LAZ_Plots/Plot4_6341.laz")
writeLAS(Plot5_2018, "Data/LAZ/LAZ_Plots/Plot5_6341.laz")
writeLAS(Plot6_2018, "Data/LAZ/LAZ_Plots/Plot6_6341.laz")
