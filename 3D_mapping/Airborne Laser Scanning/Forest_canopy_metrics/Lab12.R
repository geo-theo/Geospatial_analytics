setwd("C:/Users/theoj/Files/FORS491")
install.packages("lidR")
install.packages("terra")
install.packages("RCSF")
install.packages("mapview")
install.packages("readr")
library(lidR)
library(terra)
library(RCSF)
library(rgl)
library(sf)
library(mapview)

dbh <- c(14.2,14.3,26.5,12.6,21.7,8.5,17.2,12.0)
QMD_in <- sqrt(mean(dbh^2))
QMD_in

# DBH values in inches for Plot 202
dbh_in <- c(14.2, 14.3, 26.5, 12.6, 21.7, 8.5, 17.2, 12.0)

# Basal area conversion factor (inches² → ft²)
conv <- 0.005454154  # = pi/576

# Basal area per tree (ft²)
ba_ft2 <- dbh_in^2 * conv

# Total plot basal area (ft²)
total_ba <- sum(ba_ft2)

# Print results
ba_ft2
total_ba


# Create a dataframe of the raw inputs for Plot 201 (heights in ft, crown ratios in percent)
plot201 <- data.frame(
  Height    = c(40.7, 23.3, 64.7, 35.0, 54.0, 33.5, 38.0, 6.8),  # total tree height (ft)
  CR_uncomp = c(90,   80,   100,  95,   75,   65,   60,   0),    # Uncompacted crown ratio (%)
  CR_comp   = c(80,   70,   70,   90,   60,   55,   50,   0)     # Compacted live crown ratio (%)
)

# Remove the one tree without usable crown (Tree 8: 0% crown), per your instruction
plot201 <- subset(plot201, CR_uncomp > 0)

# Convert crown ratios from percent to decimals (e.g., 90% -> 0.90)
plot201$CR_uncomp_dec <- plot201$CR_uncomp / 100
plot201$CR_comp_dec   <- plot201$CR_comp   / 100

# Compute crown lengths (L_i = T_i * R_i) for both definitions
plot201$CL_uncomp <- plot201$Height * plot201$CR_uncomp_dec   # uncompacted crown length (ft)
plot201$CL_comp   <- plot201$Height * plot201$CR_comp_dec     # compacted crown length (ft)

# Compute crown base heights (CBH_i = T_i - L_i) for both definitions
plot201$CBH_uncomp <- plot201$Height - plot201$CL_uncomp      # CBH using uncompacted ratio (ft)
plot201$CBH_comp   <- plot201$Height - plot201$CL_comp        # CBH using compacted ratio (ft)

# Averages across the plot (n = 7 trees after filtering)
avg_CL_uncomp <- mean(plot201$CL_uncomp)   # average uncompacted crown length (ft)
avg_CL_comp   <- mean(plot201$CL_comp)     # average compacted crown length (ft)

# Per your stated definition for average crown base height, use the same ratio as the crown length:
avg_CBH_uncomp <- mean(plot201$CBH_uncomp) # average CBH using uncompacted ratio (ft)  

# Print the averages (rounded) for quick reference
round(avg_CL_uncomp, 2)  # ≈ 34.04 ft
round(avg_CL_comp,   2)  # ≈ 27.93 ft
round(avg_CBH_uncomp,2)  # ≈ 7.27  ft

################################################################################
################################################################################
################################################################################
################################################################################
# Question 1 ###################################################################
################################################################################
################################################################################
################################################################################
dbh <- c(52.6,16.5,23.6,47.7,17,31.8,34.9,6.2)
QMD_in <- sqrt(mean(dbh^2))
print(paste("P404 QMD:", QMD_in))

# DBH values in inches for Plot 202
dbh_in <- c(52.6,16.5,23.6,47.7,17,31.8,34.9,6.2)
# Basal area conversion factor (inches² → ft²)
conv <- 0.005454154  # = pi/576
# Basal area per tree (ft²)
ba_ft2 <- dbh_in^2 * conv
# Total plot basal area (ft²)
total_ba <- sum(ba_ft2)
# Print results
ba_ft2
print(paste("P404 total plot basal area (ft²):", total_ba))


# Create a dataframe of the raw inputs for Plot 201 (heights in ft, crown ratios in percent)
plot404 <- data.frame(
  Height    = c(150.8, 38.8, 53.1, 62.9, 68.1, 80.1, 106.1, 18.4),  # total tree height (ft)
  CR_uncomp = c(55,   95,   75,  45,   65,   75,   75, 100),    # Uncompacted crown ratio (%)
  CR_comp   = c(50,   90,   40,   35,   10,   65,   70,   95)     # Compacted live crown ratio (%)
)
# Convert crown ratios from percent to decimals (e.g., 90% -> 0.90)
plot404$CR_uncomp_dec <- plot404$CR_uncomp / 100
plot404$CR_comp_dec   <- plot404$CR_comp   / 100

# Compute crown lengths (L_i = T_i * R_i) for both definitions
plot404$CL_uncomp <- plot404$Height * plot404$CR_uncomp_dec   # uncompacted crown length (ft)
plot404$CL_comp   <- plot404$Height * plot404$CR_comp_dec     # compacted crown length (ft)

# Compute crown base heights (CBH_i = T_i - L_i) for both definitions
plot404$CBH_uncomp <- plot404$Height - plot404$CL_uncomp      # CBH using uncompacted ratio (ft)
plot404$CBH_comp   <- plot404$Height - plot404$CL_comp        # CBH using compacted ratio (ft)

# Averages across the plot (n = 7 trees after filtering)
avg_CL_uncomp <- mean(plot404$CL_uncomp)   # average uncompacted crown length (ft)
avg_CL_comp   <- mean(plot404$CL_comp)     # average compacted crown length (ft)

# Per your stated definition for average crown base height, use the same ratio as the crown length:
avg_CBH_uncomp <- mean(plot404$CBH_uncomp) # average CBH using uncompacted ratio (ft)  

# Print the averages (rounded) for quick reference
round(avg_CL_uncomp, 2)  # ≈ 34.04 ft
round(avg_CL_comp,   2)  # ≈ 27.93 ft
round(avg_CBH_uncomp,2)  # ≈ 7.27  ft

################################################################################
################################################################################
################################################################################
################################################################################
# Question 2, 3, 4, 5 ##########################################################
################################################################################
################################################################################
################################################################################

# ================================
# Lubrecht Stand Metrics (0.1 ac plots)
# Compute QMD, TPA, Basal Area, Biomass, and Carbon from tree lists
# Assumptions:
#   • Only LIVE tally trees are used (STATUS == 1)
#   • Minimum DBH = 5 inches
#   • Circular fixed-area plots of 0.1 acre (so per-acre expansion factor = 10)
#   • Biomass from Jenkins (2003) generic DBH-only equations (softwood vs hardwood)
#   • Carbon fraction = 0.50 (50% of dry biomass is carbon)
# ================================

# Load packages for reading Excel files and data manipulation
library(readxl)   # read_excel()
library(dplyr)    # pipes (%>%), mutate(), summarise(), group_by(), etc.

# --- 1) Load trees (expects clean numeric columns) ---
# 'path' points to your workbook; change if your file lives elsewhere
path <- "Lab12/Data/Lubrecht Carbon Inventory.xlsx"

# Read the "Trees" sheet and keep only the columns we need
trees <- read_excel(path, sheet = "Trees") %>%
  # transmute() selects/renames columns and drops everything else
  transmute(
    PLOT_ID = `Plot ID`,                 # plot identifier
    TREE    = `Tree #`,                  # tree identifier (optional in summaries)
    SPCD    = `FIA Species Code`,        # FIA species code (integer)
    DBH_in  = `DBH/DRC (in)`,            # DBH (inches); DRC entries should have been fixed earlier
    STATUS  = `Status (1=Live, 2=Dead, 3=Cut, 0=Off Plot)`  # tree status
  ) %>%
  # Filter to live tally trees >= 5 inches DBH (common overstory threshold)
  filter(STATUS == 1, DBH_in >= 5)

# --- 2) Constants used throughout ---
expansion <- 10                  # Because plots are 0.1 ac, multiply plot totals by 10 to get per-acre
k_ba      <- 0.005454154         # BA(ft²) per tree from DBH(in): k = π/576

# --- 3) Per-tree derived fields ---
trees <- trees %>%
  mutate(
    # Basal area per tree (ft²) using DBH in inches
    # BA_tree = (DBH_in^2) * (π/576)
    BA_ft2_tree = DBH_in^2 * k_ba,
    
    # DBH in centimeters for Jenkins biomass equations
    DBH_cm      = DBH_in * 2.54
  )

# --- 4) Assign species groups (softwood vs hardwood) for Jenkins generic equations ---
# These vectors list FIA species codes present in your dataset by broad class.
# Adjust if you add species not listed here.
softwood_codes <- c(17, 19, 66, 73, 93, 108, 122, 202)   # grand/subalpine fir, juniper, larch, spruce, lodgepole/ponderosa pine, Douglas-fir
hardwood_codes <- c(321, 350, 746, 747, 920)             # maple, alder, aspen, cottonwood, willow

trees <- trees %>%
  mutate(
    # Tag each tree as softwood or hardwood (defaults to 'hardwood' if not in softwood list)
    group = ifelse(SPCD %in% softwood_codes, "softwood", "hardwood")
  )

# --- 5) Jenkins (2003) generic DBH-only biomass equations ---
# Form: AGB_kg = exp(b0 + b1 * ln(DBH_cm))
# Coefficients below are the widely used generic (national) pairs:
b0_sw <- -2.5356; b1_sw <- 2.4349   # softwoods
b0_hw <- -2.0127; b1_hw <- 2.4342   # hardwoods

trees <- trees %>%
  mutate(
    # Aboveground biomass per tree (kg) using group-specific coefficients
    AGB_kg = ifelse(
      group == "softwood",
      exp(b0_sw + b1_sw * log(DBH_cm)),
      exp(b0_hw + b1_hw * log(DBH_cm))
    ),
    
    # Convert kg → Mg (metric tons)
    AGB_Mg = AGB_kg / 1000,
    
    # Estimate carbon per tree (Mg) using 50% carbon fraction (common default)
    Carbon_Mg = AGB_Mg * 0.50
  )

# --- 6) Per-plot summaries (the main deliverables) ---
plot_summary <- trees %>%
  group_by(PLOT_ID) %>%                                # compute metrics by plot
  summarise(
    n_trees       = n(),                               # number of live tally trees in the plot
    TPA           = n_trees * expansion,               # trees per acre (multiply by 10 for 0.1 ac plots)
    BA_ft2_plot   = sum(BA_ft2_tree, na.rm = TRUE),    # total BA within the 0.1 ac plot (ft²)
    BA_ft2_ac     = BA_ft2_plot * expansion,           # BA per acre (ft²/ac)
    QMD_in        = sqrt(mean(DBH_in^2, na.rm = TRUE)),# Quadratic Mean Diameter (in): sqrt( mean(DBH²) )
    Biomass_Mg_ac = sum(AGB_Mg,  na.rm = TRUE) * expansion,   # Biomass per acre (Mg/ac)
    Carbon_Mg_ac  = sum(Carbon_Mg, na.rm = TRUE) * expansion   # Carbon per acre (Mg C/ac)
  ) %>%
  arrange(PLOT_ID)                                     # nice ordering by plot ID

# Print the per-plot summary table (increase n if you have more plots)
print(plot_summary, n = 100)

# --- (Optional) Species breakdown within plots ---
# This table shows how TPA, BA, biomass, and carbon partition by species (SPCD) in each plot.
plot_species <- trees %>%
  group_by(PLOT_ID, SPCD) %>%                          # group by plot and species code
  summarise(
    n_trees       = n(),                               # count of trees of this species in the plot
    TPA           = n_trees * expansion,               # trees per acre for this species
    BA_ft2_ac     = sum(BA_ft2_tree) * expansion,      # BA per acre (ft²/ac) for this species
    Biomass_Mg_ac = sum(AGB_Mg)     * expansion,       # Biomass per acre (Mg/ac) for this species
    Carbon_Mg_ac  = sum(Carbon_Mg)  * expansion,       # Carbon per acre (Mg C/ac) for this species
    .groups = "drop"
  )

# Print the species-by-plot table (optional; helpful for composition reports)
print(plot_species, n = 100)


# ================================
# Clean ALS_plot_metrics & join plot_summary
# ================================        

library(readr)                            # Loads readr for fast CSV I/O
library(dplyr)                            # Loads dplyr for data wrangling (select, left_join, etc.)

# 1) Read ALS table
als <- read_csv("Lab12/Data/ALS_plot_metrics.csv", show_col_types = FALSE)  # Reads the ALS metrics CSV into a tibble; hides column type message

names(als)                                # Lists column names so you can inspect what was read

#####################################################
#####################################################
# ---- BEFORE: show original names for gut check ----
cat("Original column names:\n")
print(names(als))


# ---- RENAME: .x → ""   and   .y → _2m ----
als <- als %>%
  rename_with(
    ~ str_replace(.x, "\\.x$", ""),        # remove .x
    .cols = ends_with(".x")
  ) %>%
  rename_with(
    ~ str_replace(.x, "\\.y$", "_2m"),     # change .y → _2m
    .cols = ends_with(".y")
  )


# ---- AFTER: show updated names ----
cat("\nUpdated column names:\n")
print(names(als))


# ---- CHECK: detect any duplicate column names, gut check again----
dupes <- names(als)[duplicated(names(als))]

if (length(dupes) > 0) {
  warning("Duplicate column names created: ", paste(dupes, collapse = ", "))
} else {
  cat("\nNo duplicate names created. All renaming successful.\n")
}


#####################################################
#################################################

# Columns you want to KEEP among the coord/strata families
keep_xy <- c("Strata.x", "datum.x", "utm_zone.x", "utm_easting.x", "utm_northing.x")  # Defines the specific coord/strata columns you want to retain

# Find every column that is Strata / datum / utm_* / Shape (any variant: .y, _2m, plain)
to_drop <- grep("^(Strata|datum|utm_zone|utm_easting|utm_northing|Shape)($|_|\\.)",
                names(als), value = TRUE)  # Uses a regex to match any column whose name starts with those tokens,
# followed by end-of-name, underscore, or dot; returns matching names

# Remove the .x keepers from that drop list
to_drop <- setdiff(to_drop, keep_xy)      # Excludes the keepers from the drop list so they won’t be removed

# Drop them
als <- als %>% select(-all_of(to_drop))   # Drops all matched columns except the keepers; keeps everything else

names(als)                                # Shows the remaining column names after the drop

#  Join plot_summary (assumed to exist already; key is PLOT_ID)
# Make a clean join (left join to keep all ALS rows, add plot_summary fields)
als_joined <- als %>%
  left_join(plot_summary, by = c("plot_id" = "PLOT_ID"))  # Adds columns from plot_summary by matching ALS plot_id to PLOT_ID;
# keeps all rows from als even if there’s no match

names(als_joined)                         # Displays column names of the joined table
als_joined$TPA                            # Pulls the TPA column (Trees Per Acre) vector to quickly inspect it

# 6) Save cleaned & joined table
write_csv(als_joined, "Lab12/Data/ALS_plot_metrics_joined.csv")  # Writes the final table to disk as a CSV

# Preview
glimpse(als_joined)                       # Compact structural preview: column types and a few example values


# ==========================================================
# Exhaustive (but simple) search for best ALS models (≤ 4 vars)
# • ALS metrics = predictors only (no plot/measured fields)
# • For each target (TPA, BA_ft2_ac, QMD_in, Biomass_Mg_ac, Carbon_Mg_ac):
#     1) keep complete cases
#     2) preselect top-M ALS vars by |correlation|
#     3) fit ALL 1–4 variable combinations from that pool
#     4) pick the model with the lowest AIC
# • Implemented without explicit for-loops (uses lapply/combn)
# ==========================================================

library(readr)    # read_csv()
library(dplyr)    # select(), mutate(), filter(), %>%, etc.
library(stringr)  # str_detect()
library(tidyr)    # drop_na()

# -----------------------------
# 1) Load the joined dataset
# -----------------------------
df <- read_csv("Lab12/Data/ALS_plot_metrics_joined.csv", show_col_types = FALSE)  # read your merged ALS + plot metrics table

# ---------------------------------------------
# 2) Define responses (plot-measured targets)
# ---------------------------------------------
responses <- c("TPA", "BA_ft2_ac", "QMD_in", "Biomass_Mg_ac", "Carbon_Mg_ac")  # the y-variables we want to model

# -------------------------------------------------------------------------
# 3) Build the ALS-only predictor set by removing non-ALS / plot fields
# -------------------------------------------------------------------------
plot_metrics <- c(responses, "n_trees", "BA_ft2_plot")                          # plot-measured fields to exclude as predictors
drop_patterns <- c("^plot_id$", "^PLOT_ID$", "^TREE$",                          # IDs to drop
                   "^Strata", "^datum", "^utm_", "^Shape")                      # coordinates/strata/shape to drop

# function that returns TRUE if a column name should be excluded from predictors
is_dropped <- function(nm) nm %in% plot_metrics || any(str_detect(nm, paste(drop_patterns, collapse = "|")))

num_cols       <- names(df)[sapply(df, is.numeric)]                              # numeric columns only
predictors_all <- num_cols[!sapply(num_cols, is_dropped)]                        # keep numeric ALS metrics only
predictors_all <- predictors_all[sapply(predictors_all,                         # drop zero-variance predictors
                                        function(x) dplyr::n_distinct(df[[x]], na.rm = TRUE) > 1)]

# ------------------------------------------
# 4) Settings for the search
# ------------------------------------------
max_terms <- 4    # cap at 4 predictors. You can try more, but you risk overfitting. 4 is subjective.
top_m     <- 12   # try all combos but only from the top-M by |cor| for speed

# ---------------------------------------------------------------------------------------
# 5) Helper that returns the best-AIC model for ONE response (no explicit for-loop used)
# ---------------------------------------------------------------------------------------
best_model_for_response <- function(y) {
  
  dat_all <- df %>% select(all_of(c(y, predictors_all))) %>% drop_na()          # keep rows with complete y + predictor values
  if (nrow(dat_all) < 10) return(NULL)                                          # bail out if too few cases
  
  # ---- preselect top-M ALS predictors by absolute correlation with the response ----
  cor_vals <- sapply(predictors_all, function(x) suppressWarnings(cor(dat_all[[y]], dat_all[[x]])))
  preds_pool <- predictors_all[order(abs(cor_vals), decreasing = TRUE)]          # order by |cor|
  preds_pool <- head(preds_pool, min(top_m, length(preds_pool)))                 # take top-M
  
  # ---- generate ALL combinations of size 1..max_terms (as a list of character vectors) ----
  # combn() returns a matrix; lapply over sizes builds a single list of combos
  all_combos <- unlist(
    lapply(1:min(max_terms, length(preds_pool)), function(k) {
      if (k == 1L) {
        as.list(preds_pool)                                                     # list of length-1 character vectors
      } else {
        combn(preds_pool, k, simplify = FALSE)                                  # list of length-k character vectors
      }
    }),
    recursive = FALSE
  )
  
  # ---- fit every combination and keep AIC + model object ----
  fits <- lapply(all_combos, function(vars) {
    f <- as.formula(paste(y, "~", paste(vars, collapse = " + ")))               # build formula: y ~ x1 + x2 + ...
    fit <- try(lm(f, data = dat_all), silent = TRUE)                            # fit lm, guard against errors
    if (inherits(fit, "try-error")) return(NULL)                                # skip combos that fail
    list(aic = AIC(fit), vars = vars, model = fit)                              # store AIC, vars, and model
  })
  
  fits <- Filter(Negate(is.null), fits)                                          # drop failed fits
  if (!length(fits)) return(NULL)                                                # if nothing fit, return NULL
  
  # ---- pick the model with the lowest AIC ----
  best_idx <- which.min(sapply(fits, `[[`, "aic"))                               # index of min AIC
  best     <- fits[[best_idx]]                                                   # best fit info
  
  # ---- print a compact report for the student ----
  s <- summary(best$model)                                                       # model summary
  cat("\n====================================================\n")
  cat("Response: ", y, "\n", sep = "")
  cat("Chosen predictors (", length(best$vars), "): ", paste(best$vars, collapse = ", "), "\n", sep = "")
  cat("Formula:\n  ", deparse(formula(best$model)), "\n", sep = "")
  cat(sprintf("n = %d   R-squared = %.3f   Adj.R2 = %.3f   AIC = %.2f\n",
              nrow(model.frame(best$model)), s$r.squared, s$adj.r.squared, best$aic))
  
  return(best$model)                                                             # return the lm() object
}

# ---------------------------------------------------------
# 6) Run the search for ALL responses 
# ---------------------------------------------------------
best_models <- setNames(lapply(responses, best_model_for_response), responses)   # named list of best lm() per response

# ---------------------------------------------------------
# 7) Plot observed (ground truth) vs predicted for each best model
# ---------------------------------------------------------
library(ggplot2)

# build one plot per response (skip NULL models gracefully)
obs_pred_plots <- lapply(names(best_models), function(y) {
  m <- best_models[[y]]
  if (is.null(m)) return(NULL)
  
  # Data actually used to fit the model (preserves row filtering used by lm)
  mf <- model.frame(m)
  
  # Observed = response column; Predicted = fitted values from model
  plot_df <- data.frame(
    observed  = mf[[1]],
    predicted = fitted(m)
  )
  
  ggplot(plot_df, aes(x = observed, y = predicted)) +
    geom_point(alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    coord_equal() +
    labs(
      title = paste(y, "— Observed vs Predicted"),
      x = paste(y, "(observed ground truth)"),
      y = paste(y, "(model prediction)")
    ) +
    theme_minimal()
})

# print the plots (one after another) in the current device
invisible(lapply(obs_pred_plots, function(p) if (!is.null(p)) print(p)))

################################################################################
################################################################################
################################################################################
################################################################################
# Question 6-7 #################################################################
################################################################################
################################################################################
################################################################################
library(terra)
CMras <- rast("Lab12/Data/metrics_stack_20m.tif")
names(CMras) #if you don’t see all of the names of the metrics that came out in the regression, you need to load in all of the raster stacks and rejoin them

# Read rasters
rump <- rast("Lab12/Data/Rumple_20m.tif")
CM2m <- rast("Lab12/Data/stdmetrics_2m20m.tif")
CM   <- rast("Lab12/Data/stdmetrics_20m.tif")
gap <- rast("Lab12/Data/Gap_prop_20m.tif")

# Append "_2m" to CM2m band names
names(CM2m) <- paste0(names(CM2m), "_2m")

# Combine into one stack
metrics_all <- c(CM, CM2m, rump, gap)

# (Optional) save
writeRaster(metrics_all, "Lab12/Data/metrics_stack_20m_all.tif", overwrite = TRUE)

# (Optional) inspect names
names(metrics_all)


# ============================================================
# One-at-a-time raster prediction from best lm() + nicer plots
# Requires:
#   - best_models: named list of lm() objects (e.g., "TPA","BA_ft2_ac","QMD_in",...)
#   - metrics_all: SpatRaster with predictor bands that match model term names
# ============================================================

# -------------------------------
# 1) Choose ONE response to map
# -------------------------------
response <- "Carbon_Mg_ac"   # options: "TPA","BA_ft2_ac","QMD_in","Biomass_Mg_ac","Carbon_Mg_ac"

# -------------------------------
# 2) Inspect & pull the model
# -------------------------------
best_models                 # (peek) see what's in your named list
mod  <- best_models[[response]]  # extract the lm() for your chosen response
summary(mod)                # regression summary: coefficients, R², AIC, etc.

# Terms (predictor names) required by the model, in formula order
vars <- attr(terms(mod), "term.labels")
vars                          # (peek) confirm the expected predictor names

# Sanity check: make sure all predictors exist in the raster stack
missing_vars <- setdiff(vars, names(metrics_all))
if (length(missing_vars) > 0) {
  stop(sprintf("These predictors are missing in 'metrics_all': %s",
               paste(missing_vars, collapse = ", ")))
}

# -------------------------------------------
# 3) Subset raster stack to those predictors
# -------------------------------------------
# (keeps band order exactly as in the model terms)
rsub <- metrics_all[[vars]]
plot(rsub)                    # (peek) quick check each predictor layer

# -------------------------------------------
# 4) Predict to a single-band raster surface
# -------------------------------------------
# Output path (one TIFF per response)
out_fp <- file.path("Lab12/Data", paste0("pred_", response, ".tif"))

# Predict the response using the lm() over the predictor raster bands
pred <- predict(
  rsub,              # SpatRaster of predictor layers (names must match 'vars')
  mod,               # lm() model
  type = "response", # standard for lm(); returns predicted mean
  filename = out_fp, # write directly to disk (GeoTIFF)
  overwrite = TRUE
)

pred                   # (peek) prints SpatRaster info
plot(pred)             # (peek) quick default map

# -------------------------------------------
# 5) Improved final plot (map + histogram)
#    - robust color scaling (2%–98% quantiles)
#    - readable legend/title
#    - optional PNG export
# -------------------------------------------

# (Optional) simple units to annotate the legend/title
units_map <- c(
  TPA            = "trees/acre",
  BA_ft2_ac      = "ft²/acre",
  QMD_in         = "inches",
  Biomass_Mg_ac  = "Mg/acre",
  Carbon_Mg_ac   = "Mg C/acre"
)
unit_label <- units_map[[response]]
if (is.null(unit_label)) unit_label <- ""   # fallback if not listed

# Robust range to reduce the influence of outliers (2%–98%)
qs <- quantile(values(pred, na.rm = TRUE), probs = c(0.02, 0.98), names = FALSE)
# If the raster has very uniform values, ensure a non-degenerate range
if (!is.finite(qs[1]) || !is.finite(qs[2]) || qs[1] == qs[2]) {
  qs <- range(values(pred, na.rm = TRUE))
}

# Generate breaks for the color scale
nbreaks <- 20
brks <- seq(qs[1], qs[2], length.out = nbreaks)

# A clean, perceptual palette available in base R (no extra packages)
pal <- hcl.colors(nbreaks - 1, palette = "YlGn", rev = FALSE)

# Build a human-readable subtitle from model formula
form_txt <- paste(deparse(formula(mod)), collapse = "")
sub_txt  <- sprintf("Model: %s | n=%d  R²=%.3f  AIC=%.2f",
                    form_txt, length(mod$fitted.values),
                    summary(mod)$r.squared, AIC(mod))

# (Optional) export the final figure as PNG; set to TRUE to save
save_png <- FALSE
png_file <- file.path("Lab12/Data", paste0("pred_", response, "_map.png"))
if (save_png) png(png_file, width = 1600, height = 900, res = 150)

# ---- Map panel ----
plot(
  pred,
  col   = pal,
  breaks = brks,
  main  = sprintf("%s (Predicted)", response),
  sub   = sub_txt,
  mar   = c(4, 4, 4, 6),
  axes  = FALSE,
  plg   = list(title = unit_label)  # legend title with units
)

# ---- Histogram panel ----
# Basic histogram of predicted values (trimmed to robust range)
vals <- values(pred, na.rm = TRUE)
vals <- vals[vals >= qs[1] & vals <= qs[2]]
hist(
  vals,
  breaks = 30,
  main   = sprintf("%s: Value Distribution", response),
  xlab   = unit_label,
  col    = "grey80",
  border = "grey40"
)
abline(v = median(vals, na.rm = TRUE), col = "grey20", lwd = 2, lty = 2) # median line




################################################################################
################################################################################
################################################################################
################################################################################
# Question 8 ###################################################################
################################################################################
################################################################################
################################################################################
# ============================================
# Random Forest (ranger) with 5-fold CV — SIMPLE VERSION
# - Reads the joined table with ground truth + ALS metrics
# - Uses ALS-only numeric predictors (drops IDs/coords/plot fields)
# - For each response: pick top 10 predictors by |correlation|
# - 5-fold cross-validation, median-impute missing values
# - Makes an Observed vs Predicted plot per response
# ============================================

set.seed(42)  # make results repeatable via fixed RNG seed

# ---- Load packages ----
library(readr)     # read_csv()
library(dplyr)     # select(), filter(), mutate(), pipes (%>%)
library(stringr)   # str_detect()
library(ggplot2)   # plotting (ggplot/geom_*)
library(caret)     # train(), trainControl(), varImp(), preProcess
library(ranger)    # fast Random Forest implementation used by caret

# ---- CHOOSE how many predictors to keep (try 6 here) ----
TOP_K <- 6  # use top-K predictors ranked by |correlation| with the response (adjustable; e.g., 4/6/10)

# ---- 1) Read your data (joined ALS + plot metrics) ----
# NOTE: adjust the path if your file lives elsewhere
df <- read_csv("Lab12/Data/ALS_plot_metrics_joined.csv", show_col_types = FALSE) %>% select(-CO) 
# load the merged ALS + field plot table. Remove the column CO
metrics_all <- rast("Lab12/Data/metrics_rasters/metrics_stack_20m_all.tif")

# ---- 2) Define the ground-truth responses we want to predict ----
responses <- c("TPA", "BA_ft2_ac", "QMD_in", "Biomass_Mg_ac", "Carbon_Mg_ac")  # list of y variables to model

# ---- 3) Build the ALS-only predictor list (drop IDs/coords/plot fields) ----
plot_fields <- c(responses, "n_trees", "BA_ft2_plot")  # columns that are ground truth or derived from plots (exclude from predictors)
drop_patterns <- c("^plot_id$", "^PLOT_ID$", "^TREE$", "^Strata", "^datum", "^utm_", "^Shape")  # regex patterns for IDs/coords/shape columns to drop

# helper: TRUE if a column should be dropped from predictors
is_dropped <- function(nm) nm %in% plot_fields || any(str_detect(nm, paste(drop_patterns, collapse = "|")))  # returns TRUE if name is a plot field or matches drop patterns

num_cols       <- names(df)[sapply(df, is.numeric)]               # keep only numeric columns (RF expects numeric predictors here)
predictors_all <- num_cols[!sapply(num_cols, is_dropped)]         # remove plot fields and ID/coord columns from numeric set
# predictors_all now holds only ALS numeric metrics (candidate predictors)

# ---- 4) Simple 5-fold CV setup ----
ctrl <- trainControl(
  method = "cv",                 # k-fold cross-validation
  number = 5,                    # 5 folds
  savePredictions = "final",     # keep out-of-fold predictions for later plots
  verboseIter = FALSE,           # quiet training output
  allowParallel = TRUE           # allow parallel backend if registered
)

# ---- 5) Make an output folder for plots (optional but useful) ----
dir.create("rf_simple_plots", showWarnings = FALSE)  # ensures the plot output directory exists

# ---- (ADDED) create a container to save fitted RF models for later raster prediction ----
rf_models <- list()  # named list to store caret::train objects keyed by response name

# ---- 6) Loop over responses and fit Random Forests ----
for (y in responses) {  # iterate over each response variable
  
  # Keep rows where the response exists (we can impute predictors, but y must be present)
  dat0 <- df %>%
    select(all_of(c(y, predictors_all))) %>%  # select the response + all ALS predictor candidates
    filter(!is.na(.data[[y]]))                # drop rows where the response is missing (cannot train on NA y)
  
  # If we have very few rows, skip to keep CV stable
  if (nrow(dat0) < 15) {                      # small-sample guard; 5-fold CV needs reasonable fold sizes
    message("Skipping ", y, ": not enough rows with non-missing ", y, ".")  # informative skip message
    next                                      # continue to next response
  }
  
  # ---- Select a small, informative predictor set (top-K by |correlation| with y) ----
  # Compute correlation of each predictor with y using available pairs
  cor_vals <- sapply(                         # compute correlations for each candidate predictor
    names(dat0)[names(dat0) != y],            # all columns except the response
    function(v) suppressWarnings(cor(dat0[[y]], dat0[[v]], use = "complete.obs"))  # pearson cor with complete cases
  )
  # Order predictors by absolute correlation (largest first)
  preds_pool <- names(cor_vals)[order(abs(cor_vals), decreasing = TRUE)]  # rank by |correlation|
  # Keep the TOP_K (or fewer if fewer available)
  preds_use <- head(preds_pool, min(TOP_K, length(preds_pool)))           # choose the top-K predictors
  
  # Build the modeling frame with y + chosen predictors
  dat <- dat0 %>%
    select(all_of(c(y, preds_use)))             # modeling data with response and selected predictors
  
  # If after selection we somehow have no predictors, skip
  if (ncol(dat) <= 1) {                         # if only y remains, nothing to fit
    message("Skipping ", y, ": no usable predictors after filtering.")  # informative skip message
    next                                        # continue to next response
  }
  
  # ---- Train a Random Forest with very small tuning (kept simple) ----
  # - preProcess = "medianImpute" fills predictor NAs INSIDE each CV fold
  # - tuneLength = 3 lets caret try a tiny set of RF settings automatically
  fit <- train(
    x = dat %>% select(-all_of(y)),       # predictor matrix (X)
    y = dat[[y]],                         # response vector (y)
    method = "ranger",                    # RF engine
    trControl = ctrl,                     # 5-fold CV control
    preProcess = c("medianImpute"),       # impute predictor NAs via median within each training fold
    tuneLength = 3,                       # small automatic tuning grid
    metric = "RMSE",                      # select best model by lowest RMSE
    importance = "impurity",              # compute impurity-based variable importance
    num.trees = 1000                      # number of trees for stability
  )
  
  # ---- (ADDED) stash the fitted model for later raster prediction ----
  rf_models[[y]] <- fit  # save the caret::train model into the list under its response name
  
  # ---- Print a short, clear summary for the student ----
  best <- fit$bestTune                                        # best hyperparameters chosen by caret
  res  <- fit$results %>%
    dplyr::filter(mtry == best$mtry) %>%                      # keep rows for best mtry
    dplyr::arrange(RMSE) %>%                                  # sort by RMSE ascending
    dplyr::slice(1)                                           # take the top row (best)
  
  cat("\n==========================\n")                        # console separator
  cat("Response: ", y, "\n", sep = "")                        # print which response we modeled
  cat("Rows used: ", nrow(dat), "  Predictors used: ", ncol(dat) - 1, "\n", sep = "")  # sample and predictor count
  cat("Best mtry: ", best$mtry, "   CV RMSE: ", round(res$RMSE, 3),                    # key CV metrics
      "   CV R^2: ", round(res$Rsquared, 3), "   CV MAE: ", round(res$MAE, 3), "\n", sep = "")
  
  # ---- Make an Observed vs Predicted plot using CV predictions ----
  preds_cv <- fit$pred %>%
    dplyr::filter(mtry == best$mtry) %>%                      # keep out-of-fold predictions from best hyperparameters
    dplyr::rename(observed = obs, predicted = pred)           # rename columns for clarity
  
  p <- ggplot(preds_cv, aes(x = observed, y = predicted)) +   # start scatter of observed vs CV predictions
    geom_point(alpha = 0.75) +                                # semi-transparent points
    geom_abline(slope = 1, intercept = 0, linetype = 2) +     # 1:1 reference line
    coord_equal() +                                           # equal scaling on axes
    labs(title = paste(y, "— RF (5-fold CV) Observed vs Predicted"),  # plot title
         x = paste(y, "(observed)"),                          # x-axis label
         y = paste(y, "(CV prediction)")) +                   # y-axis label
    theme_minimal()                                           # clean theme
  
  print(p)  # show in the current plotting device (RStudio viewer or graphics device)
  
  # Save the plot to a file so students can paste it in reports
  ggsave(filename = file.path("rf_simple_plots", paste0(y, "_RF_5foldCV_obs_vs_pred.png")),  # output path
         plot = p, width = 6, height = 5, dpi = 300)  # export PNG with fixed size and DPI
}

caret::predictors(rf_models)

################################################################################
################################################################################
################################################################################
################################################################################
# Question 9 ###################################################################
################################################################################
################################################################################
################################################################################

set.seed(42)

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ranger)

TOP_K <- 6

df <- read_csv("Lab12/Data/ALS_plot_metrics_joined.csv", show_col_types = FALSE) %>% select(-CO)

responses <- c("TPA", "BA_ft2_ac", "QMD_in", "Biomass_Mg_ac", "Carbon_Mg_ac")

plot_fields <- c(responses, "n_trees", "BA_ft2_plot")
drop_patterns <- c("^plot_id$", "^PLOT_ID$", "^TREE$", "^Strata", "^datum", "^utm_", "^Shape")
is_dropped <- function(nm) nm %in% plot_fields || any(str_detect(nm, paste(drop_patterns, collapse="|")))

num_cols       <- names(df)[sapply(df, is.numeric)]
predictors_all <- num_cols[!sapply(num_cols, is_dropped)]

dir.create("rf_simple_plots", showWarnings = FALSE)

# ---- helpers ----
median_impute <- function(trainX, testX) {
  meds <- sapply(trainX, function(col) median(col, na.rm = TRUE))
  for (nm in names(meds)) {
    trainX[[nm]][is.na(trainX[[nm]])] <- meds[[nm]]
    testX[[nm]][is.na(testX[[nm]])]   <- meds[[nm]]
  }
  list(trainX=trainX, testX=testX)
}

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
r2   <- function(y, yhat) {
  ss_res <- sum((y - yhat)^2)
  ss_tot <- sum((y - mean(y))^2)
  1 - ss_res/ss_tot
}

make_folds <- function(n, k=5) {
  f <- sample(rep(1:k, length.out = n))
  split(seq_len(n), f)
}

# store fitted ranger models (one per response)
rf_models <- list()

for (y in responses) {
  
  dat0 <- df %>%
    select(all_of(c(y, predictors_all))) %>%
    filter(!is.na(.data[[y]]))
  
  if (nrow(dat0) < 15) {
    message("Skipping ", y, ": not enough rows with non-missing ", y, ".")
    next
  }
  
  cor_vals <- sapply(
    names(dat0)[names(dat0) != y],
    function(v) suppressWarnings(cor(dat0[[y]], dat0[[v]], use="complete.obs"))
  )
  preds_pool <- names(cor_vals)[order(abs(cor_vals), decreasing = TRUE)]
  preds_use  <- head(preds_pool, min(TOP_K, length(preds_pool)))
  
  dat <- dat0 %>% select(all_of(c(y, preds_use)))
  if (ncol(dat) <= 1) {
    message("Skipping ", y, ": no usable predictors after filtering.")
    next
  }
  
  X <- dat %>% select(-all_of(y))
  Y <- dat[[y]]
  
  # simple "tuneLength = 3" analog for mtry
  p <- ncol(X)
  mtry_grid <- unique(pmax(1, pmin(p, c(floor(sqrt(p)), floor(p/3), floor(p/2)))))
  
  folds <- make_folds(nrow(dat), k = 5)
  
  # out-of-fold predictions for the best mtry
  best_mtry <- NA
  best_rmse <- Inf
  best_pred <- rep(NA_real_, nrow(dat))
  
  for (mtry in mtry_grid) {
    oof <- rep(NA_real_, nrow(dat))
    
    for (idx_test in folds) {
      idx_train <- setdiff(seq_len(nrow(dat)), idx_test)
      
      trainX <- X[idx_train, , drop=FALSE]
      testX  <- X[idx_test,  , drop=FALSE]
      trainY <- Y[idx_train]
      
      imp <- median_impute(trainX, testX)
      trainX <- imp$trainX
      testX  <- imp$testX
      
      fit <- ranger(
        dependent.variable.name = NULL, # using x/y interface below
        x = trainX,
        y = trainY,
        num.trees = 1000,
        mtry = mtry,
        importance = "impurity"
      )
      
      oof[idx_test] <- predict(fit, data = testX)$predictions
    }
    
    this_rmse <- rmse(Y, oof)
    
    if (this_rmse < best_rmse) {
      best_rmse <- this_rmse
      best_mtry <- mtry
      best_pred <- oof
    }
  }
  
  # Train final model on ALL data with best mtry (impute using full-data medians)
  meds_full <- sapply(X, function(col) median(col, na.rm = TRUE))
  for (nm in names(meds_full)) X[[nm]][is.na(X[[nm]])] <- meds_full[[nm]]
  
  final_fit <- ranger(
    x = X,
    y = Y,
    num.trees = 1000,
    mtry = best_mtry,
    importance = "impurity"
  )
  
  rf_models[[y]] <- list(
    model = final_fit,
    predictors = preds_use,
    medians = meds_full
  )
  
  cat("\n==========================\n")
  cat("Response: ", y, "\n", sep="")
  cat("Rows used: ", nrow(dat), "  Predictors used: ", length(preds_use), "\n", sep="")
  cat("Best mtry: ", best_mtry,
      "   CV RMSE: ", round(best_rmse, 3),
      "   CV R^2: ", round(r2(Y, best_pred), 3),
      "   CV MAE: ", round(mae(Y, best_pred), 3), "\n", sep="")
  
  preds_cv <- data.frame(observed = Y, predicted = best_pred)
  
  pplt <- ggplot(preds_cv, aes(x = observed, y = predicted)) +
    geom_point(alpha = 0.75) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    coord_equal() +
    labs(
      title = paste(y, "— RF (5-fold CV) Observed vs Predicted"),
      x = paste(y, "(observed)"),
      y = paste(y, "(CV prediction)")
    ) +
    theme_minimal()
  
  print(pplt)
  
  ggsave(
    filename = file.path("rf_simple_plots", paste0(y, "_RF_5foldCV_obs_vs_pred.png")),
    plot = pplt, width = 6, height = 5, dpi = 300
  )
}

# replaces caret::predictors(rf_models)
lapply(rf_models, `[[`, "predictors")


################################################################################
################################################################################
################################################################################
################################################################################
# Question 10-11 ###############################################################
################################################################################
################################################################################
################################################################################

################################################################################
# Q9 + Q10 (FULL WORKING SCRIPT)
# - Trains RF models with ranger using 5-fold CV (no caret)
# - Saves CV obs vs pred plots
# - Predicts ONE chosen response wall-to-wall from metrics_all SpatRaster
# - Median-imputes raster NAs using training medians
################################################################################

set.seed(42)

# -------------------------------
# Packages
# -------------------------------
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ranger)
library(terra)

# -------------------------------
# Inputs / settings
# -------------------------------
TOP_K <- 6
responses <- c("TPA", "BA_ft2_ac", "QMD_in", "Biomass_Mg_ac", "Carbon_Mg_ac")

df <- read_csv("Lab12/Data/ALS_plot_metrics_joined.csv", show_col_types = FALSE) %>%
  select(-CO)

# If metrics_all is not already loaded earlier, load it here:
# metrics_all <- rast("Lab12/Data/metrics_rasters/metrics_stack_20m_all.tif")

dir.create("rf_simple_plots", showWarnings = FALSE)
dir.create("Lab12/Data", showWarnings = FALSE)

# -------------------------------
# Helper functions
# -------------------------------
median_impute <- function(trainX, testX) {
  meds <- sapply(trainX, function(col) median(col, na.rm = TRUE))
  for (nm in names(meds)) {
    trainX[[nm]][is.na(trainX[[nm]])] <- meds[[nm]]
    testX[[nm]][is.na(testX[[nm]])]   <- meds[[nm]]
  }
  list(trainX = trainX, testX = testX)
}

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
r2   <- function(y, yhat) {
  ss_res <- sum((y - yhat)^2)
  ss_tot <- sum((y - mean(y))^2)
  1 - ss_res / ss_tot
}

make_folds <- function(n, k = 5) {
  f <- sample(rep(1:k, length.out = n))
  split(seq_len(n), f)
}

# -------------------------------
# Build ALS-only predictor list
# -------------------------------
plot_fields <- c(responses, "n_trees", "BA_ft2_plot")
drop_patterns <- c("^plot_id$", "^PLOT_ID$", "^TREE$", "^Strata", "^datum", "^utm_", "^Shape")
is_dropped <- function(nm) nm %in% plot_fields || any(str_detect(nm, paste(drop_patterns, collapse = "|")))

num_cols       <- names(df)[sapply(df, is.numeric)]
predictors_all <- num_cols[!sapply(num_cols, is_dropped)]

# -------------------------------
# Q9: Train RF models (ranger) per response
# -------------------------------
rf_models <- list()

for (y in responses) {
  
  dat0 <- df %>%
    select(all_of(c(y, predictors_all))) %>%
    filter(!is.na(.data[[y]]))
  
  if (nrow(dat0) < 15) {
    message("Skipping ", y, ": not enough rows with non-missing ", y, ".")
    next
  }
  
  cor_vals <- sapply(
    names(dat0)[names(dat0) != y],
    function(v) suppressWarnings(cor(dat0[[y]], dat0[[v]], use = "complete.obs"))
  )
  preds_pool <- names(cor_vals)[order(abs(cor_vals), decreasing = TRUE)]
  preds_use  <- head(preds_pool, min(TOP_K, length(preds_pool)))
  
  dat <- dat0 %>% select(all_of(c(y, preds_use)))
  if (ncol(dat) <= 1) {
    message("Skipping ", y, ": no usable predictors after filtering.")
    next
  }
  
  X <- dat %>% select(-all_of(y))
  Y <- dat[[y]]
  
  p <- ncol(X)
  mtry_grid <- unique(pmax(1, pmin(p, c(floor(sqrt(p)), floor(p/3), floor(p/2)))))
  
  folds <- make_folds(nrow(dat), k = 5)
  
  best_mtry <- NA
  best_rmse <- Inf
  best_pred <- rep(NA_real_, nrow(dat))
  
  for (mtry in mtry_grid) {
    oof <- rep(NA_real_, nrow(dat))
    
    for (idx_test in folds) {
      idx_train <- setdiff(seq_len(nrow(dat)), idx_test)
      
      trainX <- X[idx_train, , drop = FALSE]
      testX  <- X[idx_test,  , drop = FALSE]
      trainY <- Y[idx_train]
      
      imp <- median_impute(trainX, testX)
      trainX <- imp$trainX
      testX  <- imp$testX
      
      fit_tmp <- ranger(
        x = trainX,
        y = trainY,
        num.trees = 1000,
        mtry = mtry,
        importance = "impurity"
      )
      
      oof[idx_test] <- predict(fit_tmp, data = testX)$predictions
    }
    
    this_rmse <- rmse(Y, oof)
    
    if (this_rmse < best_rmse) {
      best_rmse <- this_rmse
      best_mtry <- mtry
      best_pred <- oof
    }
  }
  
  meds_full <- sapply(X, function(col) median(col, na.rm = TRUE))
  for (nm in names(meds_full)) X[[nm]][is.na(X[[nm]])] <- meds_full[[nm]]
  
  final_fit <- ranger(
    x = X,
    y = Y,
    num.trees = 1000,
    mtry = best_mtry,
    importance = "impurity"
  )
  
  rf_models[[y]] <- list(
    model      = final_fit,
    predictors = preds_use,
    medians    = meds_full,
    best_mtry  = best_mtry
  )
  
  cat("\n==========================\n")
  cat("Response: ", y, "\n", sep = "")
  cat("Rows used: ", nrow(dat), "  Predictors used: ", length(preds_use), "\n", sep = "")
  cat("Best mtry: ", best_mtry,
      "   CV RMSE: ", round(best_rmse, 3),
      "   CV R^2: ", round(r2(Y, best_pred), 3),
      "   CV MAE: ", round(mae(Y, best_pred), 3), "\n", sep = "")
  
  preds_cv <- data.frame(observed = Y, predicted = best_pred)
  
  pplt <- ggplot(preds_cv, aes(x = observed, y = predicted)) +
    geom_point(alpha = 0.75) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    coord_equal() +
    labs(
      title = paste(y, "— RF (5-fold CV) Observed vs Predicted"),
      x = paste(y, "(observed)"),
      y = paste(y, "(CV prediction)")
    ) +
    theme_minimal()
  
  print(pplt)
  
  ggsave(
    filename = file.path("rf_simple_plots", paste0(y, "_RF_5foldCV_obs_vs_pred.png")),
    plot = pplt, width = 6, height = 5, dpi = 300
  )
}

# -------------------------------
# Q10: Predict ONE response to raster
# -------------------------------
response <- "Carbon_Mg_ac" # "BA_ft2_ac", "QMD_in", "Biomass_Mg_ac", "Carbon_Mg_ac")
fit_obj  <- rf_models[[response]]
if (is.null(fit_obj)) stop("No RF model found for: ", response)

rf_fit  <- fit_obj$model
rf_vars <- rf_fit$forest$independent.variable.names
rf_vars <- unique(as.character(rf_vars))
meds    <- fit_obj$medians[rf_vars]

# normalize names on BOTH sides to build a match (handles punctuation/case differences)
ras_names_raw <- names(metrics_all)
rf_norm  <- tolower(make.names(rf_vars))
ras_norm <- tolower(make.names(ras_names_raw, unique = TRUE))

idx <- match(rf_norm, ras_norm)
if (anyNA(idx)) {
  missing <- rf_vars[is.na(idx)]
  stop("These model predictors are not in metrics_all: ", paste(missing, collapse = ", "))
}

# subset and then FORCE names to exactly match what the model expects
rsub <- metrics_all[[idx]]
names(rsub) <- rf_vars

# prediction function: terra calls fun(model, data.frame, ...)
pred_fun <- function(model, d, ...) {
  for (nm in rf_vars) {
    m <- meds[[nm]]
    if (!is.na(m)) d[[nm]][is.na(d[[nm]])] <- m
  }
  predict(model, data = d)$predictions
}

out_fp <- file.path("Lab12/Data", paste0("pred_RF_", response, ".tif"))

pred_rf <- terra::predict(
  rsub,
  rf_fit,
  fun = pred_fun,
  filename = out_fp,
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

print(pred_rf)
plot(pred_rf)

# -------------------------------
# Optional nicer plot + histogram
# -------------------------------
units_map <- c(
  TPA            = "trees/acre",
  BA_ft2_ac      = "ft²/acre",
  QMD_in         = "inches",
  Biomass_Mg_ac  = "Mg/acre",
  Carbon_Mg_ac   = "Mg C/acre"
)
unit_label <- units_map[[response]]
if (is.null(unit_label)) unit_label <- ""

qs <- quantile(values(pred_rf, na.rm = TRUE), probs = c(0.02, 0.98), names = FALSE)
if (!is.finite(qs[1]) || !is.finite(qs[2]) || qs[1] == qs[2]) {
  qs <- range(values(pred_rf, na.rm = TRUE))
}

nbreaks <- 20
brks <- seq(qs[1], qs[2], length.out = nbreaks)
pal  <- hcl.colors(nbreaks - 1, palette = "YlGn", rev = FALSE)

sub_txt <- sprintf("RF (5-fold CV)  trees=1000  best mtry=%s", fit_obj$best_mtry)

plot(
  pred_rf,
  col    = pal,
  breaks = brks,
  main   = sprintf("%s (Predicted — Random Forest)", response),
  sub    = sub_txt,
  mar    = c(4, 4, 4, 6),
  axes   = FALSE,
  plg    = list(title = unit_label)
)

vals <- values(pred_rf, na.rm = TRUE)
vals <- vals[vals >= qs[1] & vals <= qs[2]]
hist(
  vals,
  breaks = 30,
  main   = sprintf("%s: Value Distribution (RF)", response),
  xlab   = unit_label,
  col    = "grey80",
  border = "grey40"
)
abline(v = median(vals, na.rm = TRUE), col = "grey20", lwd = 2, lty = 2)


################################################################################
################################################################################
################################################################################
################################################################################
# Question 12 ##################################################################
################################################################################
################################################################################
################################################################################


a <- rast("Lab12/Data/pred_Biomass_Mg_ac.tif")  # RF prediction in Mg per acre
acres_per_cell <- 0.09884215258686613   # 20 m x 20 m cell is 400 m² / 4046.8564224
biomass_cell_Mg_reg <- a * acres_per_cell  # convert each pixel to Mg (area-weighted)
total_biomass_Mg_reg <- global(biomass_cell_Mg_reg, "sum", na.rm = TRUE)$sum
cat(sprintf("Total biomass from Regression (entire raster): %.2f Mg\n", total_biomass_Mg_reg))

b <- rast("Lab12/Data/pred_RF_Biomass_Mg_ac.tif")  # RF prediction in Mg per acre
acres_per_cell <- 0.09884215258686613   # 20 m x 20 m cell is 400 m² / 4046.8564224
biomass_cell_Mg_rf <- b * acres_per_cell  # convert each pixel to Mg (area-weighted)
total_biomass_Mg_rf <- global(biomass_cell_Mg_rf, "sum", na.rm = TRUE)$sum
cat(sprintf("Total biomass from RF (entire raster): %.2f Mg\n", total_biomass_Mg_rf))




################################################################################
# Q9 + Q10 (FULL WORKING SCRIPT)
# - Trains RF models with ranger using 5-fold CV (no caret)
# - Saves CV obs vs pred plots
# - Predicts ONE chosen response wall-to-wall from metrics_all SpatRaster
# - Median-imputes raster NAs using training medians
################################################################################

set.seed(42)

# -------------------------------
# Packages
# -------------------------------
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ranger)
library(terra)

# -------------------------------
# Inputs / settings
# -------------------------------
TOP_K <- 6
responses <- c("TPA", "BA_ft2_ac", "QMD_in", "Biomass_Mg_ac", "Carbon_Mg_ac")

df <- read_csv("Lab12/Data/ALS_plot_metrics_joined.csv", show_col_types = FALSE) %>%
  select(-CO)

# If metrics_all is not already loaded earlier, load it here:
# metrics_all <- rast("Lab12/Data/metrics_rasters/metrics_stack_20m_all.tif")

dir.create("rf_simple_plots", showWarnings = FALSE)
dir.create("Lab12/Data", showWarnings = FALSE)

# -------------------------------
# Helper functions
# -------------------------------
median_impute <- function(trainX, testX) {
  meds <- sapply(trainX, function(col) median(col, na.rm = TRUE))
  for (nm in names(meds)) {
    trainX[[nm]][is.na(trainX[[nm]])] <- meds[[nm]]
    testX[[nm]][is.na(testX[[nm]])]   <- meds[[nm]]
  }
  list(trainX = trainX, testX = testX)
}

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
r2   <- function(y, yhat) {
  ss_res <- sum((y - yhat)^2)
  ss_tot <- sum((y - mean(y))^2)
  1 - ss_res / ss_tot
}

make_folds <- function(n, k = 5) {
  f <- sample(rep(1:k, length.out = n))
  split(seq_len(n), f)
}

# -------------------------------
# Build ALS-only predictor list
# -------------------------------
plot_fields <- c(responses, "n_trees", "BA_ft2_plot")
drop_patterns <- c("^plot_id$", "^PLOT_ID$", "^TREE$", "^Strata", "^datum", "^utm_", "^Shape")
is_dropped <- function(nm) nm %in% plot_fields || any(str_detect(nm, paste(drop_patterns, collapse = "|")))

num_cols       <- names(df)[sapply(df, is.numeric)]
predictors_all <- num_cols[!sapply(num_cols, is_dropped)]

# -------------------------------
# Q9: Train RF models (ranger) per response
# -------------------------------
rf_models <- list()

for (y in responses) {
  
  dat0 <- df %>%
    select(all_of(c(y, predictors_all))) %>%
    filter(!is.na(.data[[y]]))
  
  if (nrow(dat0) < 15) {
    message("Skipping ", y, ": not enough rows with non-missing ", y, ".")
    next
  }
  
  cor_vals <- sapply(
    names(dat0)[names(dat0) != y],
    function(v) suppressWarnings(cor(dat0[[y]], dat0[[v]], use = "complete.obs"))
  )
  preds_pool <- names(cor_vals)[order(abs(cor_vals), decreasing = TRUE)]
  preds_use  <- head(preds_pool, min(TOP_K, length(preds_pool)))
  
  dat <- dat0 %>% select(all_of(c(y, preds_use)))
  if (ncol(dat) <= 1) {
    message("Skipping ", y, ": no usable predictors after filtering.")
    next
  }
  
  X <- dat %>% select(-all_of(y))
  Y <- dat[[y]]
  
  p <- ncol(X)
  mtry_grid <- unique(pmax(1, pmin(p, c(floor(sqrt(p)), floor(p/3), floor(p/2)))))
  
  folds <- make_folds(nrow(dat), k = 5)
  
  best_mtry <- NA
  best_rmse <- Inf
  best_pred <- rep(NA_real_, nrow(dat))
  
  for (mtry in mtry_grid) {
    oof <- rep(NA_real_, nrow(dat))
    
    for (idx_test in folds) {
      idx_train <- setdiff(seq_len(nrow(dat)), idx_test)
      
      trainX <- X[idx_train, , drop = FALSE]
      testX  <- X[idx_test,  , drop = FALSE]
      trainY <- Y[idx_train]
      
      imp <- median_impute(trainX, testX)
      trainX <- imp$trainX
      testX  <- imp$testX
      
      fit_tmp <- ranger(
        x = trainX,
        y = trainY,
        num.trees = 1000,
        mtry = mtry,
        importance = "impurity"
      )
      
      oof[idx_test] <- predict(fit_tmp, data = testX)$predictions
    }
    
    this_rmse <- rmse(Y, oof)
    
    if (this_rmse < best_rmse) {
      best_rmse <- this_rmse
      best_mtry <- mtry
      best_pred <- oof
    }
  }
  
  meds_full <- sapply(X, function(col) median(col, na.rm = TRUE))
  for (nm in names(meds_full)) X[[nm]][is.na(X[[nm]])] <- meds_full[[nm]]
  
  final_fit <- ranger(
    x = X,
    y = Y,
    num.trees = 1000,
    mtry = best_mtry,
    importance = "impurity"
  )
  
  rf_models[[y]] <- list(
    model      = final_fit,
    predictors = preds_use,
    medians    = meds_full,
    best_mtry  = best_mtry
  )
  
  cat("\n==========================\n")
  cat("Response: ", y, "\n", sep = "")
  cat("Rows used: ", nrow(dat), "  Predictors used: ", length(preds_use), "\n", sep = "")
  cat("Best mtry: ", best_mtry,
      "   CV RMSE: ", round(best_rmse, 3),
      "   CV R^2: ", round(r2(Y, best_pred), 3),
      "   CV MAE: ", round(mae(Y, best_pred), 3), "\n", sep = "")
  
  preds_cv <- data.frame(observed = Y, predicted = best_pred)
  
  pplt <- ggplot(preds_cv, aes(x = observed, y = predicted)) +
    geom_point(alpha = 0.75) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    coord_equal() +
    labs(
      title = paste(y, "— RF (5-fold CV) Observed vs Predicted"),
      x = paste(y, "(observed)"),
      y = paste(y, "(CV prediction)")
    ) +
    theme_minimal()
  
  print(pplt)
  
  ggsave(
    filename = file.path("rf_simple_plots", paste0(y, "_RF_5foldCV_obs_vs_pred.png")),
    plot = pplt, width = 6, height = 5, dpi = 300
  )
}

# -------------------------------
# Q10: Predict ONE response to raster
# -------------------------------
response <- "Biomass_Mg_ac" # "BA_ft2_ac", "QMD_in", "Biomass_Mg_ac", "Carbon_Mg_ac")
fit_obj  <- rf_models[[response]]
if (is.null(fit_obj)) stop("No RF model found for: ", response)

rf_fit  <- fit_obj$model
rf_vars <- rf_fit$forest$independent.variable.names
rf_vars <- unique(as.character(rf_vars))
meds    <- fit_obj$medians[rf_vars]

# normalize names on BOTH sides to build a match (handles punctuation/case differences)
ras_names_raw <- names(metrics_all)
rf_norm  <- tolower(make.names(rf_vars))
ras_norm <- tolower(make.names(ras_names_raw, unique = TRUE))

idx <- match(rf_norm, ras_norm)
if (anyNA(idx)) {
  missing <- rf_vars[is.na(idx)]
  stop("These model predictors are not in metrics_all: ", paste(missing, collapse = ", "))
}

# subset and then FORCE names to exactly match what the model expects
rsub <- metrics_all[[idx]]
names(rsub) <- rf_vars

# prediction function: terra calls fun(model, data.frame, ...)
pred_fun <- function(model, d, ...) {
  for (nm in rf_vars) {
    m <- meds[[nm]]
    if (!is.na(m)) d[[nm]][is.na(d[[nm]])] <- m
  }
  predict(model, data = d)$predictions
}

out_fp <- file.path("Lab12/Data", paste0("pred_RF_", response, ".tif"))

pred_rf <- terra::predict(
  rsub,
  rf_fit,
  fun = pred_fun,
  filename = out_fp,
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

print(pred_rf)
plot(pred_rf)

# -------------------------------
# Optional nicer plot + histogram
# -------------------------------
units_map <- c(
  TPA            = "trees/acre",
  BA_ft2_ac      = "ft²/acre",
  QMD_in         = "inches",
  Biomass_Mg_ac  = "Mg/acre",
  Carbon_Mg_ac   = "Mg C/acre"
)
unit_label <- units_map[[response]]
if (is.null(unit_label)) unit_label <- ""

qs <- quantile(values(pred_rf, na.rm = TRUE), probs = c(0.02, 0.98), names = FALSE)
if (!is.finite(qs[1]) || !is.finite(qs[2]) || qs[1] == qs[2]) {
  qs <- range(values(pred_rf, na.rm = TRUE))
}

nbreaks <- 20
brks <- seq(qs[1], qs[2], length.out = nbreaks)
pal  <- hcl.colors(nbreaks - 1, palette = "YlGn", rev = FALSE)

sub_txt <- sprintf("RF (5-fold CV)  trees=1000  best mtry=%s", fit_obj$best_mtry)

plot(
  pred_rf,
  col    = pal,
  breaks = brks,
  main   = sprintf("%s (Predicted — Random Forest)", response),
  sub    = sub_txt,
  mar    = c(4, 4, 4, 6),
  axes   = FALSE,
  plg    = list(title = unit_label)
)

vals <- values(pred_rf, na.rm = TRUE)
vals <- vals[vals >= qs[1] & vals <= qs[2]]
hist(
  vals,
  breaks = 30,
  main   = sprintf("%s: Value Distribution (RF)", response),
  xlab   = unit_label,
  col    = "grey80",
  border = "grey40"
)
abline(v = median(vals, na.rm = TRUE), col = "grey20", lwd = 2, lty = 2)

