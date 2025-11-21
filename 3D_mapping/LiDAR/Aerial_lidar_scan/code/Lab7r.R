#install.packages("lidR")
#install.packages("terra")
setwd("C:/Users/theoj/OneDrive - The University of Montana/Documents/UMT/Courses/FORS491/")

library(lidR)
library(terra)

LASfile <- ("Lab 7/Data/LiDAR/USGS_LPC_MT_Statewide_Phase3_2021_B21_299301.laz")
las <- readLAS(LASfile)
plot(las)
print(las)
print(las@header)
las_check(las)
wkt(las)

plot(las, color="Intensity")
plot(las, color="Intensity", breaks = "quantile", nbreaks = 10)

names(las@data)
range(las@data$ScanAngle, na.rm = TRUE)
plot(las, color="ScanAngle")
plot(las, color = "ScanAngle", breaks = "quantile", nbreaks = 10)
plot(las, color = "ScanAngle", breaks = "equal", nbreaks = 12)

naip <- rast("Lab 7/Data/NAIP/m_4611305_sw_12_060_20210829.tif")
plot(naip)
plotRGB(naip, r=1, g=2, b=3, stretch="lin")
crs(naip)
crs(naip, describe = TRUE)

las_crs <- sf::st_crs(sf::st_as_sf(las, dim = "XY"))$wkt
las_crs
if (!terra::same.crs(naip, las_crs)) {naip <- terra::project(naip, las_crs, method = "near")}

naip <- naip[[1:4]]; names(naip) <- c("R","G","B","NIR")

xmin <- min(las$X, na.rm = TRUE)
xmax <- max(las$X, na.rm = TRUE)
ymin <- min(las$Y, na.rm = TRUE)
ymax <- max(las$Y, na.rm = TRUE)

e <- terra::ext(xmin - 10, xmax + 10, ymin - 10, ymax + 10)
naip <- terra::crop(naip, e)
plot(naip)

rgb <- naip[[1:3]] 
names(rgb) <- c("R","G","B") 
las <- merge_spatial(las, rgb)
plot(las, color = "RGB")

las$ReturnNumber    <- 1L
las$NumberOfReturns <- 1L
writeLAS(las, "Lab 7/Data/LiDAR/RGB_las.laz")

### dDAP ###
dDAP194 <- readLAS("Lab 7/Data/194.laz")
plot(dDAP194)
print(dDAP194)
dDAP202 <- readLAS("Lab 7/Data/202.laz")
plot(dDAP202)
print(dDAP202)

tlas194 <- sf::st_transform(dDAP194, sf::st_crs(6514))
writeLAS(tlas194, "Lab 7/Data/dDAP194_6514.laz")

tlas202 <- sf::st_transform(dDAP202, sf::st_crs(6514))
writeLAS(tlas202, "Lab 7/Data/dDAP202_6514.laz")