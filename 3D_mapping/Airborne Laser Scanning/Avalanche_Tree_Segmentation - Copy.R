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

###################################
### Avalanche Tree Segmentation ###
###################################

### Import ALS tiles ###
# 2018 tile in EPSG:6341
ALS_2018 <- readLAS("Data/LAZ/UT_2018_MonitorBowl_EPSG6341.laz")
plot(ALS_2018)
print(ALS_2018)
# 2023 tile in EPSG:6350 -- reproject to EPSG:6341+5703
ALS_2023_6530 <- readLAS("Data/LAZ/UT_2023_MonitorBowl_EPSG6350.laz")
target <- st_crs("EPSG:6341+5703")
ALS_2023 <- sf::st_transform(ALS_2023_6530, target)
plot(ALS_2023)
print(ALS_2023)

### Digital Terrain Models (DTM) ###
# Build DTM from ground-classified points in ALS for 2018
dtm_tin_2018 <- rasterize_terrain(ALS_2018, res = 1, algorithm = tin())
plot(dtm_tin_2018, main = "2018 DTM (TIN)")
plot_dtm3d(dtm_tin_2018)
writeRaster(dtm_tin_2018, "Data/DTM/DTM_2018_tin.tif", overwrite = TRUE)
# Build DTM from ground-classified points in ALS for 2023
dtm_tin_2023 <- rasterize_terrain(ALS_2023, res = 1, algorithm = tin())
plot(dtm_tin_2023, main = "2023 DTM (TIN)")
plot_dtm3d(dtm_tin_2023)
writeRaster(dtm_tin_2023, "Data/DTM/DTM_2023_tin.tif", overwrite = TRUE)
#dtm_knnidw_2018 <- rasterize_terrain(ALS_2018, algorithm = knnidw(k = 6, p = 2))
#dtm_knnidw_2023 <- rasterize_terrain(ALS_2023, algorithm = knnidw(k = 6, p = 2))

### Plot Circles ###
# Create circle plot clips with 50m radius for 2018
Plot1_2018 <- clip_circle(ALS_2018, 451548.5833, 4498306.2127, 50)
Plot2_2018 <- clip_circle(ALS_2018, 451645.6464, 4498340.2850, 50)
Plot3_2018 <- clip_circle(ALS_2018, 451626.1493, 4498393.3040, 50)
Plot4_2018 <- clip_circle(ALS_2018, 451741.6956, 4498266.1230, 50)
Plot5_2018 <- clip_circle(ALS_2018, 451672.7979, 4498270.3660, 50)
Plot6_2018 <- clip_circle(ALS_2018, 451559.6500, 4498227.6050, 50)
Plot7_2018 <- clip_circle(ALS_2018, 451536.4604, 4498151.6560, 50)
Plot8_2018 <- clip_circle(ALS_2018, 451609.2889, 4498099.3730, 50)
#par(mfrow = c(2, 4)); invisible(lapply(list(Plot1_2018, Plot2_2018, Plot3_2018, Plot4_2018, Plot5_2018, Plot6_2018, Plot7_2018, Plot8_2018), plot))
# Create circle plot clips with 50m radius for 2023
Plot1_2023 <- clip_circle(ALS_2023, 451548.5833, 4498306.2127, 50)
Plot2_2023 <- clip_circle(ALS_2023, 451645.6464, 4498340.2850, 50)
Plot3_2023 <- clip_circle(ALS_2023, 451626.1493, 4498393.3040, 50)
Plot4_2023 <- clip_circle(ALS_2023, 451741.6956, 4498266.1230, 50)
Plot5_2023 <- clip_circle(ALS_2023, 451672.7979, 4498270.3660, 50)
Plot6_2023 <- clip_circle(ALS_2023, 451559.6500, 4498227.6050, 50)
Plot7_2023 <- clip_circle(ALS_2023, 451536.4604, 4498151.6560, 50)
Plot8_2023 <- clip_circle(ALS_2023, 451609.2889, 4498099.3730, 50)
#par(mfrow = c(2, 4)); invisible(lapply(list(Plot1_2023, Plot2_2023, Plot3_2023, Plot4_2023, Plot5_2023, Plot6_2023, Plot7_2023, Plot8_2023), plot))

### Height Normalization ###
# Normalize 2018 plots
Plot1_2018_norm <- normalize_height(Plot1_2018, dtm_tin_2018)
Plot2_2018_norm <- normalize_height(Plot2_2018, dtm_tin_2018)
Plot3_2018_norm <- normalize_height(Plot3_2018, dtm_tin_2018)
Plot4_2018_norm <- normalize_height(Plot4_2018, dtm_tin_2018)
Plot5_2018_norm <- normalize_height(Plot5_2018, dtm_tin_2018)
Plot6_2018_norm <- normalize_height(Plot6_2018, dtm_tin_2018)
Plot7_2018_norm <- normalize_height(Plot7_2018, dtm_tin_2018)
Plot8_2018_norm <- normalize_height(Plot8_2018, dtm_tin_2018)
#par(mfrow=c(2,4)); invisible(lapply(list(Plot1_2018_norm,Plot2_2018_norm,Plot3_2018_norm,Plot4_2018_norm,Plot5_2018_norm,Plot6_2018_norm,Plot7_2018_norm,Plot8_2018_norm), plot))
# Normalize 2023 plots
Plot1_2023_norm <- normalize_height(Plot1_2023, dtm_tin_2023)
Plot2_2023_norm <- normalize_height(Plot2_2023, dtm_tin_2023)
Plot3_2023_norm <- normalize_height(Plot3_2023, dtm_tin_2023)
Plot4_2023_norm <- normalize_height(Plot4_2023, dtm_tin_2023)
Plot5_2023_norm <- normalize_height(Plot5_2023, dtm_tin_2023)
Plot6_2023_norm <- normalize_height(Plot6_2023, dtm_tin_2023)
Plot7_2023_norm <- normalize_height(Plot7_2023, dtm_tin_2023)
Plot8_2023_norm <- normalize_height(Plot8_2023, dtm_tin_2023)
#par(mfrow=c(2,4)); invisible(lapply(list(Plot1_2023_norm,Plot2_2023_norm,Plot3_2023_norm,Plot4_2023_norm,Plot5_2023_norm,Plot6_2023_norm,Plot7_2023_norm,Plot8_2023_norm), plot))

### Reclassify ground for normalized plots ###
# 2018 reclassification for ground within 15cm of Z=0
Plot1_2018_norm@data$Classification <- 1L; Plot1_2018_norm@data$Classification[Plot1_2018_norm@data$Z <= 0.5] <- 2L
Plot2_2018_norm@data$Classification <- 1L; Plot2_2018_norm@data$Classification[Plot2_2018_norm@data$Z <= 0.5] <- 2L
Plot3_2018_norm@data$Classification <- 1L; Plot3_2018_norm@data$Classification[Plot3_2018_norm@data$Z <= 0.5] <- 2L
Plot4_2018_norm@data$Classification <- 1L; Plot4_2018_norm@data$Classification[Plot4_2018_norm@data$Z <= 0.5] <- 2L
Plot5_2018_norm@data$Classification <- 1L; Plot5_2018_norm@data$Classification[Plot5_2018_norm@data$Z <= 0.5] <- 2L
Plot6_2018_norm@data$Classification <- 1L; Plot6_2018_norm@data$Classification[Plot6_2018_norm@data$Z <= 0.5] <- 2L
Plot7_2018_norm@data$Classification <- 1L; Plot7_2018_norm@data$Classification[Plot7_2018_norm@data$Z <= 0.5] <- 2L
Plot8_2018_norm@data$Classification <- 1L; Plot8_2018_norm@data$Classification[Plot8_2018_norm@data$Z <= 0.5] <- 2L
#par(mfrow=c(2,4)); invisible(lapply(list(Plot1_2018_norm,Plot2_2018_norm,Plot3_2018_norm,Plot4_2018_norm,Plot5_2018_norm,Plot6_2018_norm,Plot7_2018_norm,Plot8_2018_norm), \(p) plot(p, color="Classification", legend=FALSE)))
# 2023 reclassification for ground within 15cm of Z=0
Plot1_2023_norm@data$Classification <- 1L; Plot1_2023_norm@data$Classification[Plot1_2023_norm@data$Z <= 0.5] <- 2L
Plot2_2023_norm@data$Classification <- 1L; Plot2_2023_norm@data$Classification[Plot2_2023_norm@data$Z <= 0.5] <- 2L
Plot3_2023_norm@data$Classification <- 1L; Plot3_2023_norm@data$Classification[Plot3_2023_norm@data$Z <= 0.5] <- 2L
Plot4_2023_norm@data$Classification <- 1L; Plot4_2023_norm@data$Classification[Plot4_2023_norm@data$Z <= 0.5] <- 2L
Plot5_2023_norm@data$Classification <- 1L; Plot5_2023_norm@data$Classification[Plot5_2023_norm@data$Z <= 0.5] <- 2L
Plot6_2023_norm@data$Classification <- 1L; Plot6_2023_norm@data$Classification[Plot6_2023_norm@data$Z <= 0.5] <- 2L
Plot7_2023_norm@data$Classification <- 1L; Plot7_2023_norm@data$Classification[Plot7_2023_norm@data$Z <= 0.5] <- 2L
Plot8_2023_norm@data$Classification <- 1L; Plot8_2023_norm@data$Classification[Plot8_2023_norm@data$Z <= 0.5] <- 2L
#par(mfrow=c(2,4)); invisible(lapply(list(Plot1_2023_norm,Plot2_2023_norm,Plot3_2023_norm,Plot4_2023_norm,Plot5_2023_norm,Plot6_2023_norm,Plot7_2023_norm,Plot8_2023_norm), \(p) plot(p, color="Classification", legend=FALSE)))

# save to LAZ file for each plot
# save 2018 plots
writeLAS(Plot1_2018_norm, "Data/LAZ/LAZ_Plots/Plot1_2018_norm_ground.laz")
writeLAS(Plot2_2018_norm, "Data/LAZ/LAZ_Plots/Plot2_2018_norm_ground.laz")
writeLAS(Plot3_2018_norm, "Data/LAZ/LAZ_Plots/Plot3_2018_norm_ground.laz")
writeLAS(Plot4_2018_norm, "Data/LAZ/LAZ_Plots/Plot4_2018_norm_ground.laz")
writeLAS(Plot5_2018_norm, "Data/LAZ/LAZ_Plots/Plot5_2018_norm_ground.laz")
writeLAS(Plot6_2018_norm, "Data/LAZ/LAZ_Plots/Plot6_2018_norm_ground.laz")
writeLAS(Plot7_2018_norm, "Data/LAZ/LAZ_Plots/Plot7_2018_norm_ground.laz")
writeLAS(Plot8_2018_norm, "Data/LAZ/LAZ_Plots/Plot8_2018_norm_ground.laz")
# save 2023 plots
writeLAS(Plot1_2023_norm, "Data/LAZ/LAZ_Plots/Plot1_2023_norm_ground.laz")
writeLAS(Plot2_2023_norm, "Data/LAZ/LAZ_Plots/Plot2_2023_norm_ground.laz")
writeLAS(Plot3_2023_norm, "Data/LAZ/LAZ_Plots/Plot3_2023_norm_ground.laz")
writeLAS(Plot4_2023_norm, "Data/LAZ/LAZ_Plots/Plot4_2023_norm_ground.laz")
writeLAS(Plot5_2023_norm, "Data/LAZ/LAZ_Plots/Plot5_2023_norm_ground.laz")
writeLAS(Plot6_2023_norm, "Data/LAZ/LAZ_Plots/Plot6_2023_norm_ground.laz")
writeLAS(Plot7_2023_norm, "Data/LAZ/LAZ_Plots/Plot7_2023_norm_ground.laz")
writeLAS(Plot8_2023_norm, "Data/LAZ/LAZ_Plots/Plot8_2023_norm_ground.laz")

### Canopy Height Models ###
f <- function(x) { x * 0.08 + 3 }

# 2018 tree detection
Plot1_2018_chm <- rasterize_canopy(Plot1_2018_norm, 1, pitfree(subcircle = 1))
Plot1_2018_chm_smooth <- raster::focal(Plot1_2018_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot1_2018_chm_ttops <- locate_trees(Plot1_2018_chm_smooth, lmf(ws = f, hmin = 1))
plot(Plot1_2018_chm_smooth, col = height.colors(30), main = "Plot 1 (2018) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot1_2018_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot2_2018_chm <- rasterize_canopy(Plot2_2018_norm, 1, pitfree(subcircle = 1))
Plot2_2018_chm_smooth <- raster::focal(Plot2_2018_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot2_2018_chm_ttops <- locate_trees(Plot2_2018_chm_smooth, lmf(ws = f, hmin = 1))
plot(Plot2_2018_chm_smooth, col = height.colors(30), main = "Plot 2 (2018) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot2_2018_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot3_2018_chm <- rasterize_canopy(Plot3_2018_norm, 1, pitfree(subcircle = 1))
Plot3_2018_chm_smooth <- raster::focal(Plot3_2018_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot3_2018_chm_ttops <- locate_trees(Plot3_2018_chm_smooth, lmf(ws = f, hmin = 1))
plot(Plot3_2018_chm_smooth, col = height.colors(30), main = "Plot 3 (2018) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot3_2018_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot4_2018_chm <- rasterize_canopy(Plot4_2018_norm, 1, pitfree(subcircle = 1))
Plot4_2018_chm_smooth <- raster::focal(Plot4_2018_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot4_2018_chm_ttops <- locate_trees(Plot4_2018_chm_smooth, lmf(ws = f, hmin = 1))
plot(Plot4_2018_chm_smooth, col = height.colors(30), main = "Plot 4 (2018) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot4_2018_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot5_2018_chm <- rasterize_canopy(Plot5_2018_norm, 1, pitfree(subcircle = 1))
Plot5_2018_chm_smooth <- raster::focal(Plot5_2018_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot5_2018_chm_ttops <- locate_trees(Plot5_2018_chm_smooth, lmf(ws = f, hmin = 1))
plot(Plot5_2018_chm_smooth, col = height.colors(30), main = "Plot 5 (2018) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot5_2018_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot6_2018_chm <- rasterize_canopy(Plot6_2018_norm, 1, pitfree(subcircle = 1))
Plot6_2018_chm_smooth <- raster::focal(Plot6_2018_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot6_2018_chm_ttops <- locate_trees(Plot6_2018_chm_smooth, lmf(ws = f, hmin = 1))
plot(Plot6_2018_chm_smooth, col = height.colors(30), main = "Plot 6 (2018) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot6_2018_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot7_2018_chm <- rasterize_canopy(Plot7_2018_norm, 1, pitfree(subcircle = 1))
Plot7_2018_chm_smooth <- raster::focal(Plot7_2018_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot7_2018_chm_ttops <- locate_trees(Plot7_2018_chm_smooth, lmf(ws = f, hmin = 1))
plot(Plot7_2018_chm_smooth, col = height.colors(30), main = "Plot 7 (2018) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot7_2018_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot8_2018_chm <- rasterize_canopy(Plot8_2018_norm, 1, pitfree(subcircle = 1))
Plot8_2018_chm_smooth <- raster::focal(Plot8_2018_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot8_2018_chm_ttops <- locate_trees(Plot8_2018_chm_smooth, lmf(ws = f, hmin = 1))
plot(Plot8_2018_chm_smooth, col = height.colors(30), main = "Plot 8 (2018) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot8_2018_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

# 2023 tree detection
Plot1_2023_chm <- rasterize_canopy(Plot1_2023_norm, 1, pitfree(subcircle = 1))
Plot1_2023_chm_smooth <- raster::focal(Plot1_2023_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot1_2023_chm_ttops <- locate_trees(Plot1_2023_chm_smooth, lmf(ws = f, hmin = 2))
plot(Plot1_2023_chm_smooth, col = height.colors(30), main = "Plot 1 (2023) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot1_2023_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot2_2023_chm <- rasterize_canopy(Plot2_2023_norm, 1, pitfree(subcircle = 1))
Plot2_2023_chm_smooth <- raster::focal(Plot2_2023_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot2_2023_chm_ttops <- locate_trees(Plot2_2023_chm_smooth, lmf(ws = f, hmin = 2))
plot(Plot2_2023_chm_smooth, col = height.colors(30), main = "Plot 2 (2023) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot2_2023_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot3_2023_chm <- rasterize_canopy(Plot3_2023_norm, 1, pitfree(subcircle = 1))
Plot3_2023_chm_smooth <- raster::focal(Plot3_2023_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot3_2023_chm_ttops <- locate_trees(Plot3_2023_chm_smooth, lmf(ws = f, hmin = 2))
plot(Plot3_2023_chm_smooth, col = height.colors(30), main = "Plot 3 (2023) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot3_2023_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot4_2023_chm <- rasterize_canopy(Plot4_2023_norm, 1, pitfree(subcircle = 1))
Plot4_2023_chm_smooth <- raster::focal(Plot4_2023_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot4_2023_chm_ttops <- locate_trees(Plot4_2023_chm_smooth, lmf(ws = f, hmin = 2))
plot(Plot4_2023_chm_smooth, col = height.colors(30), main = "Plot 4 (2023) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot4_2023_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot5_2023_chm <- rasterize_canopy(Plot5_2023_norm, 1, pitfree(subcircle = 1))
Plot5_2023_chm_smooth <- raster::focal(Plot5_2023_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot5_2023_chm_ttops <- locate_trees(Plot5_2023_chm_smooth, lmf(ws = f, hmin = 2))
plot(Plot5_2023_chm_smooth, col = height.colors(30), main = "Plot 5 (2023) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot5_2023_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot6_2023_chm <- rasterize_canopy(Plot6_2023_norm, 1, pitfree(subcircle = 1))
Plot6_2023_chm_smooth <- raster::focal(Plot6_2023_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot6_2023_chm_ttops <- locate_trees(Plot6_2023_chm_smooth, lmf(ws = f, hmin = 2))
plot(Plot6_2023_chm_smooth, col = height.colors(30), main = "Plot 6 (2023) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot6_2023_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot7_2023_chm <- rasterize_canopy(Plot7_2023_norm, 1, pitfree(subcircle = 1))
Plot7_2023_chm_smooth <- raster::focal(Plot7_2023_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot7_2023_chm_ttops <- locate_trees(Plot7_2023_chm_smooth, lmf(ws = f, hmin = 2))
plot(Plot7_2023_chm_smooth, col = height.colors(30), main = "Plot 7 (2023) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot7_2023_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

Plot8_2023_chm <- rasterize_canopy(Plot8_2023_norm, 1, pitfree(subcircle = 1))
Plot8_2023_chm_smooth <- raster::focal(Plot8_2023_chm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, pad = TRUE, padValue = NA)
Plot8_2023_chm_ttops <- locate_trees(Plot8_2023_chm_smooth, lmf(ws = f, hmin = 2))
plot(Plot8_2023_chm_smooth, col = height.colors(30), main = "Plot 8 (2023) CHM (smoothed) - Detected Trees")
plot(sf::st_geometry(Plot8_2023_chm_ttops), add = TRUE, col = "pink", cex = 1, pch = 2)

# Tree counts: 2018 vs 2023, plot-by-plot
c(Plot1_2018 = nrow(Plot1_2018_chm_ttops), Plot1_2023 = nrow(Plot1_2023_chm_ttops))
c(Plot2_2018 = nrow(Plot2_2018_chm_ttops), Plot2_2023 = nrow(Plot2_2023_chm_ttops))
c(Plot3_2018 = nrow(Plot3_2018_chm_ttops), Plot3_2023 = nrow(Plot3_2023_chm_ttops))
c(Plot4_2018 = nrow(Plot4_2018_chm_ttops), Plot4_2023 = nrow(Plot4_2023_chm_ttops))
c(Plot5_2018 = nrow(Plot5_2018_chm_ttops), Plot5_2023 = nrow(Plot5_2023_chm_ttops))
c(Plot6_2018 = nrow(Plot6_2018_chm_ttops), Plot6_2023 = nrow(Plot6_2023_chm_ttops))
c(Plot7_2018 = nrow(Plot7_2018_chm_ttops), Plot7_2023 = nrow(Plot7_2023_chm_ttops))
c(Plot8_2018 = nrow(Plot8_2018_chm_ttops), Plot8_2023 = nrow(Plot8_2023_chm_ttops))

# bar chart for results
counts<-rbind(`2018`=c(nrow(Plot1_2018_chm_ttops),nrow(Plot2_2018_chm_ttops),nrow(Plot3_2018_chm_ttops),nrow(Plot4_2018_chm_ttops),nrow(Plot5_2018_chm_ttops),nrow(Plot6_2018_chm_ttops),nrow(Plot7_2018_chm_ttops),nrow(Plot8_2018_chm_ttops)),`2023`=c(nrow(Plot1_2023_chm_ttops),nrow(Plot2_2023_chm_ttops),nrow(Plot3_2023_chm_ttops),nrow(Plot4_2023_chm_ttops),nrow(Plot5_2023_chm_ttops),nrow(Plot6_2023_chm_ttops),nrow(Plot7_2023_chm_ttops),nrow(Plot8_2023_chm_ttops))); colnames(counts)<-paste0("Plot",1:8); write.csv(counts,"Data/tree_counts_2018_2023_by_plot.csv",row.names=TRUE); barplot(counts,beside=TRUE,names.arg=paste0("Plot",1:8),ylab="Detected tree count",main="Tree counts by plot: 2018 vs 2023",col=c("blue","red"),legend.text=rownames(counts),args.legend=list(fill=c("blue","red"),bty="n"))