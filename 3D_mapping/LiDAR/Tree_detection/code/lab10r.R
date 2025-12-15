#options("install.lock"=FALSE)
#install.packages("lidR")
#install.packages("terra")
#install.packages("RCSF")
#install.packages("mapview")
setwd("C:/Users/theoj/Files/FORS491")
library(lidR)
library(terra)
library(RCSF)
library(rgl)
library(sf)
library(mapview)

# Setup
install.packages("pkgbuild")
pkgbuild::check_build_tools(debug = TRUE)
#Sys.setenv(PATH = paste("C:/rtools44/ucrt64/bin","C:/rtools44/usr/bin",Sys.getenv("PATH"),sep = ";"))
#pkgbuild::check_build_tools(debug = TRUE)

# Windows users: install Rtools first (see above).
# Exact link may differ by version; the rtools45 link in the script is fine for R 4.0-era.
#install.packages("remotes") #this allows for packages to be installed from github
library(remotes)
remotes::install_github('umr-amap/aRchi')     # QSM tools
library(aRchi)
library(lidR)



##########
##########
# Part 1 #
##########
##########

# 1a) Load example TLS point cloud from inside the aRchi package
tls = system.file("extdata","Tree_2_point_cloud.las", package="aRchi")
tls = lidR::readLAS(tls)

# Optional: write it out if you want to open in CloudCompare later
writeLAS(tls, "Lab10/Data/Tree_2_point_cloud.las")

# Quick visual check of the cloud
plot(tls)


# 1b) Build an empty aRchi object and attach the point cloud
aRchi = aRchi::build_aRchi()
aRchi = aRchi::add_pointcloud(aRchi, point_cloud = tls)

# Plot the raw point cloud from the aRchi object
plot(aRchi@pointcloud)
# 1c) Build the skeleton (extracts the woody topology)
aRchi = skeletonize_pc(aRchi)

# Smooth the skeleton to reduce jitter in axes/cylinders
aRchi = smooth_skeleton(aRchi)

# Plot skeleton over the cloud to QA
plot(aRchi, show_point_cloud = TRUE)
# 1d) Estimate radii from distances of points to skeleton; builds the QSM cylinders
aRchi = aRchi::add_radius(aRchi)

# Plot the QSM only (cylinders)
plot(aRchi, skeleton = FALSE, show_point_cloud = FALSE)
# 1e) Volume & biomass
Treevolume(aRchi)                  # whole-tree volume (sometimes 0 if paths not built; see Troubleshooting)
sum(aRchi@QSM[["volume"]])         # sum of cylinder volumes (m^3), always a good sanity check

TreeBiomass(aRchi, WoodDensity=550)         # biomass (kg) with assumed density
sum(aRchi@QSM[["volume"]]) * 550            # should match TreeBiomass above

# 1f) Annual shoot segmentation and leaf-on rendering (visualization)
# Code below can take a while to run. Use caution and it is not required.
aRchi = aRchi::segment_annual_shoots(aRchi, tree_age = 13)
aRchi = aRchi::add_physiological_ages(aRchi)
aRchi = aRchi::add_leaves(aRchi)

plot(aRchi, skeleton = FALSE, leaves = TRUE, bg = "white", color = "chocolate4")

# 2a) Load the larger example TLS cloud
tls2 = system.file("extdata","Tree_1_point_cloud.las", package = "aRchi")
tls2 = lidR::readLAS(tls2)

# Build aRchi object and attach cloud
aRchi2 = aRchi::build_aRchi()
aRchi2 = aRchi::add_pointcloud(aRchi2, point_cloud = tls2)

plot(aRchi2@pointcloud)

# Optional pre-clean (remove obvious noise/outliers)
aRchi2 = aRchi::clean_point_cloud(aRchi2)
# 2b) Skeletonize with parameters favoring speed on complex/occluded data
# D      = voxel size for downsampling/grouping (larger = faster, less detail)
# cl_dist= clustering distance (m) for linking points to axes
# max_d  = max distance (m) considered when attaching points to the skeleton
aRchi2 = skeletonize_pc(aRchi2, D = 0.5, cl_dist = 0.2, max_d = 1)

# Smooth the skeleton
aRchi2 = smooth_skeleton(aRchi2)

# Plot the skeleton
plot(aRchi2, show_point_cloud = FALSE)
# 2c) Build QSM radii/cylinders and visualize
aRchi2 = aRchi::add_radius(aRchi2)
plot(aRchi2, skeleton = FALSE, show_point_cloud = FALSE)
# 2d) Volume & biomass checks
Treevolume(aRchi2)                       # may return 0 if hierarchy not set; see Troubleshooting
sum(aRchi2@QSM[["volume"]])              # manual total m^3

TreeBiomass(aRchi2, WoodDensity=550)     # kg
sum(aRchi2@QSM[["volume"]]) * 550        # sanity check








# Part 2

# ------------------------------------------------------------
# PIPO TLS → TreeLS 2.0.6 end-to-end 
# Produces a CSV with TreeID, X, Y, DBH (cm), BH fit Error, Height (m), Volume (m³)
# Notes: Inline tips show which knobs to tweak for different forest types/species.
# ------------------------------------------------------------

# Install TreeLS (dev) once; comment out after first use
remotes::install_github('tiagodc/TreeLS')

library(TreeLS)      # core TLS mapping, stem classification, inventory & segmentation
library(data.table)  # fast, vectorized grouping for volume + final table/CSV
library(magrittr)    # pipes (optional)

# ------------------------------------------------------------
# Read normalized LAS/TLS inputs (yours are already normalized to ground)
# If your files are NOT normalized, run tlsNormalize() after readTLS().
# Tip (stand type):
#   - Dense mixed stands (fir/spruce): expect more occlusion near BH.
#   - Open PIPO: straighter boles, fewer understory hits, easier BH fitting.
# ------------------------------------------------------------
#?readTLS
TLS194 <- readTLS("Lab10/Data/TLS_194_6514_norm_ground.laz")
TLS202 <- readTLS("Lab10/Data/TLS_202_6514_norm_ground.laz")
MLS194 <- readTLS("Lab10/Data/MLS_194_6514_norm_ground.laz")
MLS202 <- readTLS("Lab10/Data/MLS_202_6514_norm_ground.laz")
dDAP194 <- readTLS("Lab10/Data/dDAP194normC.laz")
dDAP202 <- readTLS("Lab10/Data/dDAP_202_6514_ground_fixed.laz")

# Clip plots to 15 m radius around plot centers (reduces edges/outliers for mapping)
# Tip: In very open stands you can expand radius to 20 m; in cluttered understory, keep tighter (10–12 m).
TLS194  <- clip_circle(TLS194,  299454.37, 301583.46, 15) 
TLS202  <- clip_circle(TLS202,  299009.75, 301138.07, 15) 
dDAP194 <- clip_circle(dDAP194, 299454.37, 301583.46, 15)  
dDAP202 <- clip_circle(dDAP202, 299009.75, 301138.07, 15)  
MLS194  <- clip_circle(MLS194,  299454.37, 301583.46, 15)  
MLS202  <- clip_circle(MLS202,  299009.75, 301138.07, 15)  

####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
# ------------------------------------------------------------------
# 1) Choose one dataset to process.
#    Tip: Keep the CSV filename (step 11) in sync with the selection here.
#    Stand-type tweaks:
#      - Multi-return MLS/backpack → expect sparser BH points; later, relax BH window or pixel_size.
#      - dDAP/mobile among dense fir → consider larger merge (treeMap) to suppress duplicate detections.
# ------------------------------------------------------------------
#tls <- TLS202
#tls <- TLS194
#tls <- dDAP194
tls <- dDAP202
#tls <- MLS194
#tls <- MLS202

# ------------------------------------------------------------------
# 2) Normalize to ground (Z = height above ground)
#    keep_ground=TRUE retains ground class for QA and occasional rechecks.
#    If data were already normalized, this is harmless (idempotent-ish).
#  Time suck if already done, which your plots are. 
# ------------------------------------------------------------------
#tls <- tlsNormalize(tls, keep_ground = TRUE)

# ------------------------------------------------------------------
# 3) Uniform thinning for stable densities & faster mapping.
#    Voxel = 0.03 m (~3 cm) works well for PIPO boles.
#    Stand-type tweaks:
#      - Dense/occluded → 0.025–0.03 m (finer) to preserve BH detail.
#      - Sparse/mobile → 0.04–0.05 m (coarser) to suppress noise & speed up Hough.
# ------------------------------------------------------------------
thin <- tlsSample(tls, smp.voxelize(0.03))

# ------------------------------------------------------------------
# 4) Stem mapping via Hough voting through lower bole.
#    h_step: vertical slice spacing for votes (m). Smaller = more precise, slower.
#    max_h: vote up to ~2.2 m (brackets BH even on uneven ground).
#    merge: fuse detections within 0.20 m; raise to 0.25 m if double-detecting the same stem;
#           lower to 0.15 m if close stems are being merged in dense young stands.
# ------------------------------------------------------------------
map <- treeMap(
  thin,
  map.hough(h_step = 0.10, max_h = 2.2),   #change max_h value if you get an error (2.2 for TLS/MLS, 1.2 for dDAP)
  merge = 0.20,
  positions_only = FALSE
)

# Quick visual QA of mapped centers (yellow dots). In shrub-heavy plots expect some extra candidates.
x <- plot(thin); add_treeMap(x, map, color = "yellow", size = 2)

# ------------------------------------------------------------------
# 5) Assign TreeIDs to points from mapped centers using crop regions.
#    trp.crop(circle=FALSE): rectangular neighborhoods are robust for straight conifers.
#    Alternatives:
#      - trp.crop(circle=TRUE) for circular crops (can help isolate single stems).
#      - trp.voronoi() when stems are tightly packed (mixed fir/pine regeneration).
# ------------------------------------------------------------------
tls <- treePoints(tls, map, trp.crop(circle = FALSE))

# ------------------------------------------------------------------
# 6) Stem points near BH with Hough in a tight vertical window.
#    h_base: BH window; tighten (1.30–1.50) for very straight, well-scanned PIPO;
#            widen (1.10–1.70) for MLS/sparse data or sloped terrain.
#    pixel_size: 2D grid for Hough; match voxel. Increase (0.04–0.05) to reduce false positives
#                in understory; decrease (0.02–0.025) to recover small stems.
# ------------------------------------------------------------------
tls <- stemPoints(
  tls,
  stm.hough(
    h_base     = c(1.20, 1.60),
    pixel_size = 0.03
  )
)

# QA: red = stem-classified points; yellow = TreeIDs. This is a required screenshot!
x <- plot(tls, size = 2)
add_stemPoints(x, tls, color = "red", size = 6)
add_treeIDs(x, tls, cex = 1.6, col = "yellow")
add_treeMap(x, map, color = "yellow", size = 2)

# ------------------------------------------------------------------
# 7) Inventory (per-tree diameter & height).
#    dh = 1.3 m (BH center); dw = 0.4 m thickness around BH (1.1–1.5 m slice).
#    hp = 1 uses the highest point as tree height; for noisy tops use 0.99.
#    d_method: IRLS is robust to bark roughness; if clutter remains, try RANSAC with n=20–25.
#    Stand-type tweaks:
#      - Sloped terrain: widen dw to 0.5 m to stabilize DBH.
#      - Very clean TLS: narrow dw to 0.3 m to sharpen DBH.
# ------------------------------------------------------------------
inv <- tlsInventory(
  tls,
  dh       = 1.3,
  dw       = 0.4,
  hp       = 1,
  d_method = shapeFit(shape = "circle", algorithm = "irls")
)

# ------------------------------------------------------------------
# 8) Stem segmentation along height (RANSAC circles).
#    Output includes: TreeID, Segment, X, Y, Radius (m), Error, AvgHeight (m), N (pts).
#    Alternatives for leaning/crooked stems:
#      - sgt.ransac.cylinder() / sgt.irls.cylinder() to fit axis + radius (reduces tilt bias).
#    Tuning:
#      - Increase n to 30 for high-noise scans; reduce to 18–20 for speed on clean TLS.
# ------------------------------------------------------------------
seg <- stemSegmentation(
  tls,
  sgt.ransac.circle(n = 24)
)

# ------------------------------------------------------------------
# 9) Per-tree STEM VOLUME via Smalian between adjacent segment slices.
#    Uses Radius (m) and AvgHeight (m AGL): V = Σ ((A_i + A_{i+1})/2) * Δz, A = π r².
#    Notes:
#      - If Radius were in cm, convert once (see commented line below).
#      - For merchantable volume, later filter by top diameter/stump height using your taper curve.
# ------------------------------------------------------------------
seg_dt <- as.data.table(seg)
setorder(seg_dt, TreeID, AvgHeight)
# seg_dt[Radius > 3, Radius := Radius / 100]  # uncomment if your Radius is in cm

seg_vol <- seg_dt[
  , {
    A  <- pi * (Radius^2)                                # cross-sectional area per slice (m²)
    v  <- ((A + data.table::shift(A, type="lead"))/2) *  # mean area of adjacent slices
      (data.table::shift(AvgHeight, type="lead") -   # vertical interval
         AvgHeight)
    .(stem_vol_m3 = sum(v, na.rm = TRUE))                # total stem volume per tree (m³)
  },
  by = TreeID
]

# ------------------------------------------------------------------
# 10) Assemble per-tree table: location, DBH (cm), BH Error, height (m), stem volume (m³)
#     DBH from inv$Radius at BH (2*Radius), exported in cm for allometry friendliness.
#     Error = circle fit error at BH from tlsInventory; for a BH-specific segment error,
#             you could pick seg slice nearest 1.3 m instead (not shown to keep code unchanged).
# ------------------------------------------------------------------
inv_dt <- as.data.table(inv)

out <- merge(
  inv_dt[, .(
    TreeID,
    X, Y,                            # coordinates in LAS CRS units (usually meters)
    DBH_cm  = 2 * Radius * 100,      # convert Radius at BH to DBH in cm
    Error,                           # tlsInventory fit error (dimensionless; lower = better)
    height_m = H                     # per-tree top height (m) from hp=1
  )],
  seg_vol, by = "TreeID", all.x = TRUE
)

# ------------------------------------------------------------------
# 11) Write CSV. Keep filename aligned with dataset selection (step 1).
# ------------------------------------------------------------------
data.table::fwrite(out, "Lab10/Data/excel/dDAP202_Inventory.csv") 

# ------------------------------------------------------------------
# 12) Visual QA of the full workflow.
#     If false stems appear: increase map.merge (0.25), raise pixel_size (0.04–0.05),
#     or tighten BH window in stm.hough (1.30–1.50). If missing small stems: lower pixel_size (0.02–0.025),
#     shrink merge (0.15), and/or refine voxel to 0.025–0.03.
# ------------------------------------------------------------------
tlsPlot(tls, map, inv, seg, fast = TRUE) #This is the other required screenshot!

# check out only one tree
# look at the tree ID within your output table
tlsPlot(tls, inv, seg, tree_id = 1)
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################



# Part 3
# =========================================
# TLS understory slice + voxel maps (lidR + terra only)
# =========================================
library(terra)     # modern raster & vector operations
library(lidR)      # LiDAR processing (voxel, metrics, clipping)

# Use all CPU threads for faster lidar processing
#set_lidr_threads(8)

# Set working directory to your lab folder
#setwd("C:/Users/theoj/Files/FORS491")

# --- Read TLS (normalized heights). Keep 0.05–2.0 m AGL (edit if desired) ---
# readTLS() imports terrestrial lidar point clouds.
# The filter keeps only points between 5 cm and 2 m above ground—typical fuel zone.
TLS194 <- readTLS("Lab10/Data/TLS_194norm.laz", filter = "-keep_z 0.05 2")
TLS202 <- readTLS("Lab10/Data/TLS_202norm.laz", filter = "-keep_z 0.05 2")

# --- Clip to 10 m radius around plot centers ---
# Focus on the circular field plots to reduce edges and noise.
TLS194 <- clip_circle(TLS194, 299454.37, 301583.46, 10)
TLS202 <- clip_circle(TLS202, 299009.75, 301138.07, 10)

# --- Choose one plot to analyze ---
# (Both lines shown so you can toggle which plot is active)
las <- TLS202
#las <- TLS194

# Quick sanity check: open interactive 3-D view
plot(las)

# =========================================
# 1) 0.5 m voxels: point count per voxel (3D quick-look)
# =========================================
# Compute metrics inside 0.5 m cubic voxels: length(Z) = number of points per voxel.
vox50 <- voxel_metrics(las, length(Z), res = 0.5)

# Visualize in 3-D, coloring by number of points (V1 column).
plot(vox50, color = "V1", pal = heat.colors(50), voxel = TRUE)

# =========================================
# 2) 0.1 m voxels → collapse to 2D column summaries (stripe-proof)
# =========================================
res10 <- 0.10

# Compute fine-scale 0.1 m voxels (each cell stores point count in V1).
vox10 <- voxel_metrics(las, length(Z), res = res10)

# Convert to a normal data.frame for manipulation.
vox10_df <- as.data.frame(vox10)

# Flag each voxel as occupied (1) if it contains ≥1 point.
vox10_df$occ <- as.integer(vox10_df$V1 > 0)

# Build a base raster grid snapped to exact 0.1 m multiples.
# This ensures all voxels line up cleanly without striping artifacts.
x0 <- floor(min(vox10_df$X) / res10) * res10
y0 <- floor(min(vox10_df$Y) / res10) * res10
x1 <- ceiling(max(vox10_df$X) / res10) * res10
y1 <- ceiling(max(vox10_df$Y) / res10) * res10

r_base <- terra::rast(
  xmin = x0, xmax = x1,
  ymin = y0, ymax = y1,
  resolution = res10
)

# Assign each voxel center (X,Y) to its raster cell index.
cells <- terra::cellFromXY(r_base, vox10_df[, c("X","Y")])

# Summarize by column:
#   occ_sum = how many 0.1 m voxels contain any return (fuel layers)
#   n_sum   = total number of points (fuel density)
occ_sum <- tapply(vox10_df$occ, cells, sum, na.rm = TRUE)
n_sum   <- tapply(vox10_df$V1,  cells, sum, na.rm = TRUE)

# Fill the raster cells with those summaries.
r_occ <- r_base; r_pts <- r_base
if (!is.null(occ_sum)) values(r_occ)[as.integer(names(occ_sum))] <- as.numeric(occ_sum)
if (!is.null(n_sum))   values(r_pts)[as.integer(names(n_sum))]   <- as.numeric(n_sum)

# =========================================
# 3) Quick 2-D plots
# =========================================
# Map the number of occupied voxels (structure height complexity)
plot(r_occ, main = "Occupied voxel count per (X,Y) column (0.10 m)")
# Map the total lidar returns (fuel density proxy)
plot(r_pts, main = "Point-count sum per (X,Y) column (0.10 m)")

# =========================================
# 4) Nested voxel models (0.5 m) built from 0.1 m sub-voxels
# =========================================
# Convert LAS to a voxelized point cloud at 0.1 m resolution.
# Each occupied 0.1 m voxel becomes one point at its center.
voxP <- voxelize_points(las, res = 0.1)
plot(voxP)

# Aggregate those 0.1 m voxels into 0.5 m cubes and count how many sub-voxels they contain.
vox50_1 <- voxel_metrics(voxP, length(Z), res = 0.5)

# Visualize: brighter colors = more occupied 0.1 m sub-voxels in each 0.5 m cube.
plot(vox50_1, color = "V1", pal = heat.colors(50), voxel = TRUE)

# Repeat but only include 1–2 m above ground (ladder-fuel zone)
vox50_2 <- voxel_metrics(voxP, length(Z), res = 0.5, filter = ~ Z >= 1 & Z <= 2)
plot(vox50_2, color = "V1", pal = heat.colors(50), voxel = TRUE)
