#options("install.lock"=FALSE)
#install.packages("lidR")
#install.packages("sf")
#install.packages("terra")
#install.packages("RCSF")
#setwd("C:/Users/theoj/OneDrive - The University of Montana/Documents/UMT/Courses/FORS491/")

library(lidR)
library(sf)
#library(terra)
#library(RCSF)

las_6350 <- readLAS("Final Project/EDA/Utah_Monitor-avalanche/USGS_LPC_NV_USFSR4_D23_1301_2064_EPSG-6350_LAS2023.laz")
plot(las_6350)
print(las_6350)
las_check(las_6350)

st_crs(las_6350)
st_crs(las_6350) <- 6350

las_6341 <- sf::st_transform(las_6350, crs=6341)

writeLAS(las_6341, "Final Project/EDA/Utah_Monitor-avalanche/USGS_LPC_NV_USFSR4_D23_1301_2064_EPSG-6341_LAS2023.laz")