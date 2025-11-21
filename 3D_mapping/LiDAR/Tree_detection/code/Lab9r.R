#options("install.lock"=FALSE)
#install.packages("lidR")
#install.packages("terra")
#install.packages("RCSF")
#install.packages("mapview")
#setwd("C:/Users/theoj/OneDrive - The University of Montana/Documents/UMT/Courses/FORS491/")

library(lidR)
library(terra)
library(RCSF)
library(rgl)
library(sf)
library(mapview)

ctg <- readLAScatalog("Lab 9/TILES/")
las_check(ctg)
plot(ctg)

plot(ctg, mapview = T, map.types = "Esri.WorldImagery")

TileDTM = rasterize_terrain(ctg, res = 1, algorithm = tin())
plot(TileDTM, main='Two Tile DTM')

plot_dtm3d(TileDTM)

projection(TileDTM)
TileDTM

terra::writeRaster(TileDTM, filename= "Lab 9/Data/TILEdtm.tif",overwrite=TRUE)

mapview(TileDTM, map.types = "Esri.WorldImagery")
?mapview
mapview(TileDTM, map.types = "OpenTopoMap")

ctg194 <- clip_circle(ctg, 299454.3753, 301583.4606, 30)
ctg202 <- clip_circle(ctg, 299009.7522, 301138.0756, 30)
plot(ctg202)

Zras202 <- rasterize_canopy(ctg202, 1, p2r())
plot(Zras202, col = height.colors(50))
plot_dtm3d(Zras202)
Zras194 <- rasterize_canopy(ctg194, 1, p2r())
plot(Zras194, col = height.colors(50))
plot_dtm3d(Zras194)

?normalize_height

NORMclipALS194 <- normalize_height(ctg194, tin())
plot(NORMclipALS194)
NORMclipALS202 <- normalize_height(ctg202, tin())
plot(NORMclipALS202)

Zras202NORM <- rasterize_canopy(NORMclipALS202, 1, p2r())
plot(Zras202NORM, col = height.colors(50))
plot_dtm3d(Zras202NORM)
Zras194NORM <- rasterize_canopy(NORMclipALS194, 1, p2r())
plot(Zras194NORM, col = height.colors(50))
plot_dtm3d(Zras194NORM)

?rasterize_canopy

chm1 <- rasterize_canopy(NORMclipALS194, 0.5, p2r())
plot(chm1, col = height.colors(50), main = "Resolution 0.5, p2r ()")
plot_dtm3d(chm1)
chm2 <- rasterize_canopy(NORMclipALS194, 2, p2r())
plot(chm2, col = height.colors(50), main = "Resolution 2, p2r ()")
plot_dtm3d(chm2)
chm3 <- rasterize_canopy(NORMclipALS194, 0.5, p2r(2))
plot(chm3, col = height.colors(50), main = "Resolution 0.5, p2r (2)")
plot_dtm3d(chm3)
chm4 <- rasterize_canopy(NORMclipALS194, 1, dsmtin())
plot(chm4, col = height.colors(50), main = "DSM TIN")
plot_dtm3d(chm4)
chm5<- rasterize_canopy(NORMclipALS194, 1, pitfree(subcircle = 1))
plot(chm5, col = height.colors(50), main = "Pit Free Algorithm")
plot_dtm3d(chm5)

?terra::focal

chmSmooth <- terra::focal(chm4, w = 5, fun = mean)
plot(chmSmooth, col = height.colors(50), main = "Smoothed CHM")

TLS194norm <- readLAS("Lab 9/Data/TLS_194_6514_norm_ground.laz")
TLS202norm <- readLAS("Lab 9/Data/TLS_202_6514_norm_ground.laz")
dDAP194norm <- readLAS("Lab 9/Data/dDAP_194_6514_norm_ground.laz")
dDAP202norm <- readLAS("Lab 9/Data/dDAP_202_6514_norm_ground.laz")

TLS194chm<- rasterize_canopy(TLS194norm , 0.2, p2r(subcircle = 0.15, na.fill = knnidw(k = 12, p = 2)))
plot(TLS194chm, col = height.colors(50), main = "Plot 194 - TLS Resolution 0.2, p2r (0.15, IDW fill)")
TLS202chm<- rasterize_canopy(TLS202norm , 0.2, p2r(subcircle = 0.15, na.fill = knnidw(k = 12, p = 2)))
plot(TLS202chm, col = height.colors(50), main = "Plot 202 - TLS Resolution 0.2, p2r (0.15, IDW fill)")
TLS194chm1<- rasterize_canopy(TLS194norm , 0.5, p2r(subcircle = 0.15, na.fill = knnidw(k = 12, p = 2)))
plot(TLS194chm1, col = height.colors(50), main = "Plot 194 - TLS Resolution 0.5, p2r ()")
TLS202chm1<- rasterize_canopy(TLS202norm , 0.5, p2r(subcircle = 0.15, na.fill = knnidw(k = 12, p = 2)))
plot(TLS202chm1, col = height.colors(50), main = "Plot 202 - TLS Resolution 0.5, p2r ()")



dDAP194normC <- classify_noise(dDAP194norm, ivf(res = 0.1, n = 5)) 
plot(dDAP194normC, color = "Classification") 
dDAP194normC<- remove_noise(dDAP194normC)
writeLAS(dDAP194normC, "Lab 9/Data/dDAP194normC.laz")
dDAP202normC <- classify_noise(dDAP202norm, ivf(res = 0.1, n = 5)) 
plot(dDAP202normC, color = "Classification") 
dDAP202normC<- remove_noise(dDAP202normC)
writeLAS(dDAP202normC, "Lab 9/Data/dDAP202normC.laz")

dDAP194chm<- rasterize_canopy(dDAP194normC , 0.2, p2r(subcircle = 0.15))
plot(dDAP194chm, col = height.colors(50), main = "Plot 194 - dDAP Resolution 0.2, p2r (0.15)")
dDAP202chm<- rasterize_canopy(dDAP202normC , 0.2, p2r(subcircle = 0.15))
plot(dDAP202chm, col = height.colors(50), main = "Plot 202 - dDAP Resolution 0.2, p2r (0.15)")
dDAP194chm1<- rasterize_canopy(dDAP194normC , 0.5, p2r())
plot(dDAP194chm1, col = height.colors(50), main = "Plot 194 - dDAP Resolution 0.5, p2r ()")
dDAP202chm1<- rasterize_canopy(dDAP202normC , 0.5, p2r())
plot(dDAP202chm1, col = height.colors(50), main = "Plot 202 - dDAP Resolution 0.5, p2r ()")
plot_dtm3d(dDAP194chm)
plot_dtm3d(dDAP202chm)


##### Question 10 #####
?locate_trees

ttops194 <- locate_trees(dDAP194chm, lmf(ws = 2, hmin = 2, shape = "circular")) 
plot(dDAP194chm, col = height.colors(30)) 
plot(sf::st_geometry(ttops194), add = TRUE, col = "black", cex = 1, pch = 1)
f <- function(x) { x * 0.05 + 2} 
ttops194 <- locate_trees(dDAP194chm, lmf(f))
Tops <- plot(dDAP194normC, bg = "white", size = 2)
add_treetops3d(Tops, ttops194)

#TLS194norm <- readLAS  ("Lab 9/Data/TLS_194_6514_norm_ground.laz")   
#TLS202norm <- readLAS  ("Lab 9/Data/TLS_202_6514_norm_ground.laz")  
#dDAP194normC <- readLAS("Lab 9/Data/dDAP194normC.laz") 
#dDAP202normC <- readLAS("Lab 9/Data/dDAP202normC.laz") 

##### Question 11 #####
?segment
?segment_trees
?crown_metrics

##### Question 12 #####
dDAP194normC_treeID <- segment_trees(dDAP194normC, silva2016(dDAP194chm, ttops194))
plot(dDAP194normC_treeID, color = "treeID")

dDAP194normC_treeID <- segment_trees(dDAP194normC, silva2016(dDAP194chm, ttops194))
plot(dDAP194normC_treeID, color = "treeID")

# full code from loading norm data to outputting laz with colorized treeID:
'''
dDAP194normC <- readLAS("Lab 9/dDAP194normC.laz")
dDAP194chm <- rasterize_canopy(dDAP194normC, 0.2, p2r(subcircle = 0.15))
f <- function(x) {x * 0.05 + 2}
ttops194 <- locate_trees(dDAP194chm, lmf(f))
dDAP194normC <- segment_trees(dDAP194normC, silva2016(dDAP194chm, ttops194))
plot(dDAP194normC, color="treedID")
writeLAS(dDAP194normC, "Lab 9/dDAP194normC_treeIDcolor.laz")
'''

##### Question 13 #####
tree24 <- filter_poi(dDAP194normC, treeID == 24)
plot(tree24, size = 5, bg="white")
crowns <- crown_metrics(dDAP194normC, func = .stdtreemetrics, geom = "convex")
summary(crowns)
plot(crowns["convhull_area"], main = "Crown Area (convex hull)")

##### Question 14 #####
plot(dDAP194chm, col = height.colors(50))
plot(sf::st_geometry(crowns), add = TRUE)
mapview(crowns, map.types = "Esri.WorldImagery")

# describe code below:
las<- readLAS("Module9/TILES/USGS_LPC_MT_Statewide_Phase3_2021_B21_299301.laz") #This fist line reads in the ALS lidar file located in the TILES directory 

ALSnorm <- normalize_height(las, tin()) 

chm <- rasterize_canopy(ALSnorm, res = 1, p2r()) 

plot(chm) 

f <- function(x) { x * 0.05 + 2} 

ttops <- locate_trees(chm, lmf(f)) 

ALSnorm <- segment_trees(ALSnorm, silva2016(chm, ttops)) 

writeLAS(ALSnorm, "Module9/ALSnormT.laz") 

plot(ALSnorm, color = "treeID") 

##### Question 15 #####
las_treeID<- readLAS("Lab 9/TILES/USGS_LPC_MT_Statewide_Phase3_2021_B21_299301.laz")
ALSnorm_treeID <- normalize_height(las_treeID, tin()) 
chm_treeID <- rasterize_canopy(ALSnorm_treeID, res = 1, p2r()) 
plot(chm_treeID) 
f <- function(x) { x * 0.05 + 2} 
ttops_treeID <- locate_trees(chm_treeID, lmf(f)) 
ALSnorm_treeID <- segment_trees(ALSnorm_treeID, silva2016(chm_treeID, ttops_treeID)) 
writeLAS(ALSnorm_treeID, "Lab 9/ALSnormT_treeID.laz") 
plot(ALSnorm_treeID, color = "treeID")