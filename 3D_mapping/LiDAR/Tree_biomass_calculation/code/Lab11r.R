setwd("C:/Users/theoj/Files/FORS491")
#options("install.lock"=FALSE)
install.packages("lidR")
install.packages("terra")
install.packages("RCSF")
install.packages("mapview")

library(lidR)
library(terra)
library(RCSF)
library(rgl)
library(sf)
library(mapview)

ctg <- readLAScatalog("Lab11/Data/Tiles/")
ctg194 <- clip_circle(ctg, 299454.3753, 301583.4606, 30)
ctg202 <- clip_circle(ctg, 299009.7522, 301138.0756, 30)
las_check(ctg)
plot(ctg)

NORMclipALS194 <- normalize_height(ctg194, tin())
plot(NORMclipALS194)
NORMclipALS202 <- normalize_height(ctg202, tin())
plot(NORMclipALS202)

Zras194NORM <- rasterize_canopy(NORMclipALS194, res = 1, p2r())
plot(Zras194NORM, col = height.colors(50))
Zras202NORM <- rasterize_canopy(NORMclipALS202, res = 1, p2r())
plot(Zras202NORM, col = height.colors(50))


?cloud_metrics

# Question 1
TOI <- clip_circle(NORMclipALS202, 299018, 301128, 4)
plot(TOI)

TOI$Z[1:10] #This prints the first 10 Z values 
## [1]  9.00  9.89  9.61 12.76 10.95 15.35 16.81 12.25 12.15  8.76
cloud_metrics(TOI, ~max(Z)) # what unit is the output in?

# Question 2
CMTOI <- cloud_metrics(TOI, .stdmetrics)
CMTOI
CMTOI_z <- cloud_metrics(TOI, .stdmetrics_z)
CMTOI_z


?.stdmetrics
?.stdmetrics_z

# Question 3
hist(TOI$Z, breaks = 100)

# Question 4
over2m <- filter_poi(TOI, Z >= 2)
CMTOIover2m <- cloud_metrics(over2m, .stdmetrics)
CMTOIover2m

names(CMTOIover2m)
hist(over2m$Z, breaks = 100)

# Combine (bind) the two metric tables side-by-side into one data frame
# This lets you easily compare the metrics from all points (CMTOI)
# against those from points above 2 m (CMTOIover2m)
Com <- cbind(CMTOI, CMTOIover2m)

# Display the combined data frame in the console
Com

# Question 5
# rumple


# Question 6
#canopy_openness <- sum(las$Z < 2) / length(las$Z)
#gap_fraction_profile(las$Z[las$Z > 0], dz = 1, z0 = 2)

# --- 0) Read catalog & set options ---
ctg <- readLAScatalog("Lab11/Data/Tiles/")   # Load a LAScatalog from the folder of tiles
#set_lidr_threads(8)         # Use 8 threads for lidR operations (parallel where supported)
opt_laz_compression(ctg) <- TRUE      # Write LAZ (compressed) outputs instead of LAS
opt_progress(ctg)        <- TRUE      # Show a progress bar for catalog operations

plot(ctg, mapview = TRUE, map.types = "Esri.WorldImagery")  # Interactive map of catalog tiles with Esri imagery basemap

# Where to write normalized tiles (one file per tile)
out_norm_tiles <- "Lab11/Data/Tiles_norm" #normalized tiles will be saved here. Only need to do this once.  # Directory path for normalized tile outputs
opt_output_files(ctg) <- file.path(out_norm_tiles, "{XLEFT}_{YBOTTOM}_norm")  # Set filename template for per-tile outputs (uses tile origin coords)

# --- 1) Normalize the ENTIRE catalog (Z -> AGL) ---
# Use TIN ground model; swap to knnidw(k=10, p=2) if ground class is unreliable
#this will take a while, monitor progress. 
#If a single tile is taking more than 10 minutes only do the NW 4 tiles. But try to do all 9.
#If only using the NW 4 tiles, just put them in a folder to point the readLAScatalog to that folder.
#Warnings are fine when processing
ctg_norm <- normalize_height(ctg, tin())   # Create height-normalized copies of tiles (Z becomes above-ground level) using TIN ground model

#pts <- st_read("Lab11/Data/Lubrecht.gdb/", layer = "Lubrecht_PlotLocations", quiet = TRUE)  # Read plot center points from the GDB (as sf)
pts <- sf::st_read("Lab11/Data/Lubrecht_shp/Lubrecht_PlotLocations.shp", quiet = TRUE)

# Align CRS)
pts <- st_transform(pts, st_crs(ctg_norm))    # Reproject points to match the catalog’s CRS
pts_in <- sf::st_crop(pts, lidR::st_bbox(ctg_norm)) # Spatially crop points to the bbox of the normalized catalog (keep only those inside)

#Plot LiDAR tiles (basemap: Esri imagery) and add points on top
m <- plot(ctg_norm, mapview = TRUE, map.types = "Esri.WorldImagery")  # Interactive map of catalog tiles with Esri imagery basemap
m + mapview(pts_in, color = "yellow", col.regions = "red", cex = 4, layer.name = "GDB points")  # Add points layer on top for visual QC


# Question 7
?plot_metrics
plotCM <- plot_metrics(ctg_norm, .stdmetrics_z, pts_in, radius = 11.35)  # Compute standard height metrics per plot within 11.35 m radius
#plot2m <- plot_metrics(ctg_norm, stdmetrics_z(Z[Z >= 2]), pts_in, radius = 11.35) #standard height metrics only considering points above 2m
plot2m <- plot_metrics(ctg_norm, .stdmetrics_z, pts_in, radius = 11.35, filter= ~Z >=2)  #standard height metrics only considering points above 2m
#
plotCO <- plot_metrics(
  ctg_norm,
  ~ sum(Z < 2, na.rm = TRUE) / length(Z),   # Anonymous function: canopy openness = fraction of points below 2 m
  pts_in,
  radius = 11.35
)
plotRumple <- plot_metrics(
  ctg_norm,
  ~ rumple_index(X, Y, Z),   # uses Delaunay on the point cloud    # Anonymous function: Rumple index computed from point geometry
  pts_in,
  radius = 11.35
)


library(dplyr)                                                # Load dplyr for joins and data manipulation
key <- "plot_id"                                           # Name of the plot ID column used as the join key


# minimal joins (keep geometry from plotCM; drop it on RHS)
out <- plotCM %>%
  left_join(st_drop_geometry(plot2m),  by = key, suffix = c("","_2m")) %>%
  left_join(st_drop_geometry(plotCO)   %>% rename(CO = V1),     by = key) %>%
  left_join(st_drop_geometry(plotRumple)%>% rename(Rumple = V1),by = key)
# Write
out_file <- "Lab11/Data/ALS_plot_metrics.csv"     # Output CSV file path
#write.csv(out_sf, out_file, row.names = FALSE)   # Write all metrics + CO + Rumple + XY to CSV
write.csv(out, out_file, row.names = FALSE)   # Write all metrics + CO + Rumple + XY to CSV
message("Wrote: ", out_file)      

str(out)


# Question 8
plot(out$CO)

plot(out$CO, out$Rumple,
     xlab = "Canopy Openness",
     ylab = "Rumple Index",
     main = "Canopy Openness vs Rumple Index")

library(ggplot2); library(dplyr); library(tidyr) # install if needed

# A) CO vs pzabove2
ggplot(out, aes(pzabove2, CO)) +
  geom_point() +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  coord_equal(xlim = c(0,1), ylim = c(0,1)) +
  labs(title = "CO vs pzabove2", x = "pzabove2", y = "CO")

# B) Structure Roughness
ggplot(out, aes(zsd, Rumple)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Rumple vs zsd")

# C) Structure/height
ggplot(out, aes(zmax, Rumple)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Rumple vs zmax")

# D) Openness vs vertical complexity
ggplot(out, aes(zentropy, CO)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "CO vs zentropy")

# E) Per-plot bar charts
ggplot(out, aes(factor(plot_id), CO)) +
  geom_col() +
  labs(title = "CO by Plot", x = "Plot", y = "CO") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(out, aes(factor(plot_id), Rumple)) +
  geom_col() +
  labs(title = "Rumple by Plot", x = "Plot", y = "Rumple") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# save a figure
#ggsave("figure.png", width = 6, height = 4, dpi = 300)


##############
### PART 2 ###
##############

library(lidR)
library(terra)

# --- Read the (already normalized) catalog ---
ctg_norm <- readLAScatalog("Lab11/Data/Tiles_norm/")     # Create a LAScatalog object pointing to the folder of *normalized* tiles

# --- Basic runtime options ---
set_lidr_threads(8)  # use multiple cores Enable multi-threading for faster catalog processing
opt_progress(ctg_norm) <- TRUE   # Turn on progress bars for long-running operations
opt_output_files(ctg_norm) <- "" # disable writing during metrics (in-memory only)  
# Ensure metric functions don’t write temporary clips to disk

# (Optional speed-up: keep only needed attributes; remove Intensity, RGB, etc. if unused)
# opt_select(ctg_norm) <- "xyzcrn" # (Optional) Limit attributes to X,Y,Z, Classification, ReturnNumber

plot(ctg_norm)  # Draw the tile footprints (quick visual sanity check of the catalog extent)


# ---- Choose raster resolution (meters) ----
# this is to approximatly match the plot metrics you did earlier at 11.35 m radius (~22.7 m diameter)
res_m <- 20   # change if you want finer/coarser rasters                       

# ---- Std height metrics (many bands) ----
# Returns a SpatRaster (terra) with bands like zmax, zmean, zsd, zentropy, pzabove2, zq*, etc.
zras <- pixel_metrics(ctg_norm, .stdmetrics_z, res = res_m, pkg = "terra")     # Compute standard Z metrics as a SpatRaster at 20 m resolution

# You can keep track of your processing in the Plots viewer. Warnings are OK
names(zras)  # List of metric names (raster bands) Inspect band names present in the multi-band raster
plot(zras, col = terrain.colors(10))  # Quick browse of all bands with a terrain color palette
# Example: plot max height
plot(zras$zmax, col = height.colors(50), main = paste0("Max height at ", res_m, " m res"))  # Show only the zmax band with a height palette
#code from Ethan
plot(zras$zpcum2, col = c("darkgreen", "yellow"), main = paste0("zpcum2 at ", res_m, "m res"))


# Compute the same standard metrics for z values (stdmetrics_z) BUT only use points above 2m (filter = ~Z >=2)
zras2m <- pixel_metrics(ctg_norm, .stdmetrics_z, res = res_m, pkg = "terra", filter= ~Z >=2)
plot(zras2m) #check to make sure that the results are different than your Zras
plot(zras2m$pzabove2) #this should have near 100 percent of points above 2m at all locations. 
# (Z > 2) is different than (Z >=2) so in some squares, there may be some points that were right at 2m so 100 percent won’t be above 2




# ---- Rumple index per pixel ----
Rumple <- pixel_metrics(
  ctg_norm,      # Use the (assumed) normalized catalog for consistent AGL heights
  ~ rumple_index(X, Y, Z),  # Per-pixel canopy roughness via point-cloud triangulation
  res = res_m, pkg = "terra"     # Output as SpatRaster at the same 20 m resolution
)
names(Rumple) <- "Rumple"    # Rename the single-band raster to “Rumple”
plot(Rumple, col = terrain.colors(10), main = paste0("Rumple index at ", res_m, " m res"))  # Visualize Rumple across the area

# ----  Gap at 2 m resolution (1 = no point > 2 m; 0 = has canopy > 2 m) ----
# Build a 2 m raster of max height, then flag "gap" cells
r2m_max <- pixel_metrics(ctg_norm, ~ max(Z, na.rm = TRUE), res = 2) # Compute per-2m-cell maximum AGL height (SpatRaster if pkg=terra option set)
Gap2m   <- terra::ifel(r2m_max > 2, 0, 1)  # 1 = gap, 0 = canopy   # Classify each 2 m cell as gap (no point >2 m) or canopy (>2 m present)
names(Gap2m) <- "Gap2m"  # Name the binary gap raster
plot(Gap2m, col = c("darkgreen", "yellow"), main = "Gap at 2 m res (1=gap)")   # Plot gaps (1) vs canopy (0)

# Aggregate Gap1m to the same resolution as zras/CO/Rumple to get gap proportion per pixel
# fact must be an integer (how many 2 m cells per coarse cell)
fact <- round(res_m / terra::res(Gap2m)[1])  # Compute aggregation factor to scale 2 m cells up to res_m grid
Gap_prop <- terra::aggregate(Gap2m, fact = fact, fun = mean)  # 0..1 proportion # Aggregate binary gap cells to gap *proportion* at res_m
names(Gap_prop) <- paste0("Gap_prop_", res_m, "m") # Name the aggregated gap-proportion raster
plot(Gap_prop, col = terrain.colors(10), main = paste0("Gap proportion at ", res_m, " m res"))  # Visualize gap proportion per coarse cell


# Question 9

# Question 10

# ---- Stack & write outputs ----
out_dir <- "Lab11/Data/metrics_rasters"   # Destination folder for metric rasters
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE) # Create folder if it doesn’t exist
res_m <- 20   #defineing res_m again in case you are missing it from above



# Save individual rasters
terra::writeRaster(zras,   file.path(out_dir, paste0("stdmetrics_", res_m, "m.tif")), overwrite = TRUE) # Write multi-band std metrics raster

terra::writeRaster(zras2m,   file.path(out_dir, paste0("stdmetrics_2m", res_m, "m.tif")), overwrite = TRUE) # Write multi-band std metrics raster

terra::writeRaster(Rumple, file.path(out_dir, paste0("Rumple_", res_m, "m.tif")), overwrite = TRUE)     # Write Rumple raster

terra::writeRaster(Gap2m,  file.path(out_dir, "Gap_2m.tif"), overwrite = TRUE)                           # Write binary 2 m gap raster
terra::writeRaster(Gap_prop, file.path(out_dir, paste0("Gap_prop_", res_m, "m.tif")), overwrite = TRUE) # Write gap proportion raster at res_m

# ---------------------------
# Build a combined stack for quick viewing/analysis
# ---------------------------

layers <- c(                      # Define the metric band names we want from stdmetrics
  "zmax","zmean","zsd","zentropy","zq25","zq50","zq95","pzabove2"
)

layers <- intersect(             # Keep only names that exist in BOTH rasters (defensive)
  layers,                             #   -> requested names
  intersect(names(zras), names(zras2m))     #   -> common band names across zras and zras2m
)                                 # This prevents errors if a band is missing in either

combo <- c(            # Concatenate multiple rasters into one multi-layer stack
  zras[[layers]],      #   -> selected all-points stdmetrics bands
  zras2m[[layers]],    #   -> the same bands but computed with Z >= 2 m
  Rumple,              #   -> add Rumple index as an extra layer
  Gap_prop             #   -> add gap proportion as an extra layer
)

names(combo) <- c(     # Assign human-readable layer names to the stack
  layers,              #   -> names for the all-points bands
  paste0(layers, "_2m"),   #   -> same names with "_2m" suffix for canopy-only bands
  "Rumple",                #   -> name for the Rumple layer
  names(Gap_prop)          #   -> preserve names coming from Gap_prop (often "Gap_prop")
)

terra::writeRaster(              # Save the combined stack to disk
  combo,                         #   -> the multi-layer SpatRaster we just built
  file.path(out_dir, paste0("metrics_stack_", res_m, "m.tif")),#   -> e.g., ".../metrics_stack_20m.tif"
  overwrite = TRUE                           #   -> overwrite if present
)

# Quick peek
print(combo)    # Print SpatRaster summary (dimensions, resolution, names)
plot(combo, col = terrain.colors(10)) # Quick multi-panel plot of all layers in the combo stack

plot(combo$zmax,combo$Rumple)

# --- 1) Map of maximum canopy height (zmax) ---
plot(combo$zmax,                                     # plot the zmax layer as a map
     col = terrain.colors(50),                       # use a terrain-like palette
     main = "Max canopy height (zmax)")              # title

# --- 2) Histogram of zmax (height distribution) ---
terra::hist(combo$zmax,                              # draw a histogram of zmax values
            main = "Histogram of zmax",              # title
            xlab = "Height (m)")                     # x-axis label

# --- 3) Create a quick canopy openness (CO) layer from pzabove2 ---
#     CO ≈ 1 - pzabove2  (fraction of points below 2 m)
CO <- 1 - combo$pzabove2                             # compute raster of CO from pzabove2
names(CO) <- "CO"                                    # name the layer

# --- 4) Map of canopy openness (CO) ---
plot(CO,                                             # plot CO as a map
     col = rev(terrain.colors(50)),                  # palette (reversed so open areas pop)
     main = "Canopy openness (CO = 1 - pzabove2)")   # title

# --- 5) Scatter: canopy height vs roughness (zmax vs Rumple) ---
#     Sample a manageable number of pixels to plot (avoids plotting millions of points)
samp <- terra::spatSample(c(combo$zmax, combo$Rumple),  # sample two layers together
                          size = 10000,                 # number of sample pixels
                          method = "regular",           # regular grid sampling
                          na.rm = TRUE,                 # drop NAs
                          as.points = FALSE)            # return a data.frame-like matrix
# Fit a simple linear model for a trend line
fit1 <- lm(Rumple ~ zmax, data = as.data.frame(samp))   # linear fit: Rumple ~ zmax

plot(samp[,"zmax"], samp[,"Rumple"],                    # scatter plot of sampled points
     pch = 16, cex = 0.4,                               # small solid points
     xlab = "zmax (m)", ylab = "Rumple",                # axis labels
     main = "zmax vs Rumple (sampled)")                 # title
abline(fit1, col = "red", lwd = 2)                      # add best-fit line

# --- 6) Scatter: canopy cover vs gaps (pzabove2 vs Gap_prop) ---
#     Expect a negative relationship (more cover -> fewer gaps)
samp2 <- terra::spatSample(c(combo$pzabove2, combo[[names(combo)[grep("^Gap_prop_", names(combo))][1]]]),
                           size = 10000, method = "regular", na.rm = TRUE, as.points = FALSE)  # sample cover & first Gap_prop layer
colnames(samp2) <- c("pzabove2","Gap_prop")             # set clean column names
fit2 <- lm(Gap_prop ~ pzabove2, data = as.data.frame(samp2))  # linear fit: gaps ~ cover

plot(samp2[,"pzabove2"], samp2[,"Gap_prop"],            # scatter plot of sampled points
     pch = 16, cex = 0.4,
     xlab = "pzabove2 (proportion of points > 2 m)",    # x label = canopy cover proxy
     ylab = "Gap proportion",                           # y label = fraction of gap cells
     main = "pzabove2 vs Gap proportion (sampled)")     # title
abline(fit2, col = "blue", lwd = 2)                     # add best-fit line

# --- 7) Scatter: vertical variability vs roughness (zsd vs Rumple) ---
samp3 <- terra::spatSample(c(combo$zsd, combo$Rumple),
                           size = 10000, method = "regular", na.rm = TRUE, as.points = FALSE)  # sample SD & Rumple
fit3 <- lm(Rumple ~ zsd, data = as.data.frame(samp3))   # linear fit: Rumple ~ zsd

plot(samp3[,"zsd"], samp3[,"Rumple"],                   # scatter plot of sampled points
     pch = 16, cex = 0.4,
     xlab = "zsd (SD of height, m)",                    # x label
     ylab = "Rumple",                                   # y label
     main = "zsd vs Rumple (sampled)")                  # title
abline(fit3, col = "darkgreen", lwd = 2)                # add best-fit line

# --- 8) Quick multi-panel browse of key bands ---
plot(c(combo$zmax, combo$zsd, combo$zentropy, CO),      # show a small stack: zmax, zsd, zentropy, CO
     col = terrain.colors(50))                          # shared color palette

##########################################
#################################
####################
#Question 11
#Histogram of maximum tree height (zmax)
hist(combo$zmax,
     main = "Histogram of Maximum Height (zmax)",
     xlab = "Height (m)",
     col = "darkgreen")

# Histogram of canopy closure (pzabove2)
hist(combo$pzabove2,
     main = "Histogram of Canopy Closure (pzabove2)",
     xlab = "Proportion of returns > 2 m",
     col = "forestgreen")

# --- 3) Define thresholds for 'tall trees' and 'high canopy cover' ---
# Adjust these based on your data distribution or ecological thresholds
height_thresh <- 30     # e.g., trees taller than 30 m
cover_thresh  <- 60     # e.g., canopy closure greater than 60%

# --- 4) Use raster math to find areas that meet both conditions ---
# Create binary rasters: 1 = meets condition, 0 = does not
Tall <- combo$zmax > height_thresh
HighCover <- combo$pzabove2 > cover_thresh

# Combine: cells that meet BOTH conditions (logical AND)
TallDense <- Tall & HighCover
names(TallDense) <- "TallDense"

# --- 5) Plot the resulting area ---
plot(TallDense,
     col = c("lightgray", "darkgreen"),
     legend = FALSE,
     main = paste0("Areas with Height > ", height_thresh, " m and Canopy Cover > ", cover_thresh, "%"))
legend("topright", legend = c("Other areas", "Tall + Dense canopy"), fill = c("lightgray", "darkgreen"), bty = "n")

# --- 6) (Optional) Save the output map ---
writeRaster(TallDense, "Lab11/Data/metrics_rasters/TallDense_area.tif", overwrite = TRUE)


##########################################
#Next, we want to find areas with complex 
#vertical structure (high zentropy) 
#but lower canopy closure (lower pzabove2), 
#which might indicate gaps or multi-story stands 
#where regeneration and understory light exist.
######################################
#Histogram of complex vertical structure (zentropy)
hist(combo$zentropy,
     main = "Histogram of Complex Vertical Structure (zentropy)",
     xlab = "Entropy",
     col = "lavender")

# Histogram of canopy closure (pzabove2)
hist(combo$pzabove2,
     main = "Histogram of Canopy Closure (pzabove2)",
     xlab = "Proportion of returns > 2 m",
     col = "turquoise")

# --- 3) Define thresholds for 'complex vertical structure' and 'low canopy cover' ---
# Adjust these based on your data distribution or ecological thresholds
height_entropy <- 0.75
cover_thresh  <- 55

# --- 4) Use raster math to find areas that meet both conditions ---
# Create binary rasters: 1 = meets condition, 0 = does not
complex <- combo$zentropy > height_entropy
lowCover <- combo$pzabove2 < cover_thresh

# Combine: cells that meet BOTH conditions (logical AND)
ComplexThin <- complex & lowCover
names(ComplexThin) <- "ComplexThin"

# --- 5) Plot the resulting area ---
plot(ComplexThin,
     col = c("beige","lightgreen"),
     legend = FALSE,
     main = paste0("Areas with Complex Vertical Structure > ", height_entropy, " and Canopy Cover < ", cover_thresh, "%"))
legend("topright", legend = c("Other areas", "Complex + Thin canopy"), fill = c("beige", "lightgreen"), bty = "n")

# --- 6) (Optional) Save the output map ---
writeRaster(ComplexThin, "Lab11/Data/metrics_rasters/ComplexThin_area.tif", overwrite = TRUE)

