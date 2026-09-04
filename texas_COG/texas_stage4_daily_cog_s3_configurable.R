# ==============================================================================
# TEXAS STAGE IV -> SMOOTH DAILY COG
#
# Purpose:
#   Build one statewide smoothed Stage IV daily rainfall COG for web mapping.
#
# Architecture:
#   S3 daily Texas parquet
#       + S3 Texas HRAP geometry
#       -> local temporary/cache copies
#       -> EPSG:3857 native raster
#       -> bilinear upsample
#       -> Gaussian smooth
#       -> local COG
#       -> canonical S3 web-map path
#
# Canonical output:
#   s3://web-map-assets/basemaps/rainfall/texas/daily/YYYY/MM/DD/stage4_daily.tif
#
# Notes:
#   - Stage IV / HRAP source remains authoritative for analysis.
#   - This COG is a cartographic display surface only.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(terra)
  library(arrow)
  library(paws)
})

# ==============================================================================
# CONFIG
# ==============================================================================

# --------------------------------------------------------------------------
# DATE
# --------------------------------------------------------------------------
# Change ONE value here to build another daily COG.
MAP_DATE <- as.Date("2026-07-07")

YYYY <- format(MAP_DATE, "%Y")
MM   <- format(MAP_DATE, "%m")
DD   <- format(MAP_DATE, "%d")

# --------------------------------------------------------------------------
# AWS
# --------------------------------------------------------------------------
AWS_REGION <- "us-east-2"

PIPELINE_BUCKET <- "stg4-24hr-aws-pipeline"
WEB_BUCKET      <- "web-map-assets"

# Source rainfall parquet; date is built automatically from MAP_DATE.
PRECIP_KEY <- sprintf(
  paste0(
    "CONUS_subset/production_areas/texas_mrb/",
    "precip/precip_parquet/",
    "year=%s/month=%s/day=%s/part-0.parquet"
  ),
  YYYY, MM, DD
)

# Texas HRAP geometry is stable and does not change by date.
HRAP_KEY <- paste0(
  "CONUS_subset/config/aoi/texas_mrb/",
  "assets/cells.gpkg"
)

# Canonical web-map output.
OUTPUT_KEY <- sprintf(
  "basemaps/rainfall/texas/daily/%s/%s/%s/stage4_daily.tif",
  YYYY, MM, DD
)

# --------------------------------------------------------------------------
# LOCAL WORKING DIRECTORY
# --------------------------------------------------------------------------
WORK_DIR <- "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/r/MapLibrePOC/texas_COG"

dir.create(
  WORK_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# Keep local source/cache files date-aware.
LOCAL_PRECIP <- file.path(
  WORK_DIR,
  sprintf("texas_%s%s%s.parquet", YYYY, MM, DD)
)

LOCAL_HRAP <- file.path(
  WORK_DIR,
  "texas_hrap_cells.gpkg"
)

# --------------------------------------------------------------------------
# SMOOTHING
# --------------------------------------------------------------------------
#
# Suggested values:
#
#   10L = proven baseline, about 450 m display cells
#   15L = recommended finer web-map test, about 300 m
#   20L = aggressive test, about 225 m
#
SMOOTH_UPSAMPLE_FACT <- 15L

# Existing proven Gaussian setting.
SMOOTH_RADIUS_CELLS <- 3L

# NA = automatic sigma = radius / 2, matching existing ggplot workflow.
# Example explicit override: 1.5
SMOOTH_SIGMA_CELLS <- NA_real_

# --------------------------------------------------------------------------
# OUTPUT / UPLOAD
# --------------------------------------------------------------------------
OVERWRITE_LOCAL <- TRUE
UPLOAD_TO_S3    <- TRUE

# Long cache is appropriate because dated canonical objects should normally
# never be changed once finalized.
COG_CACHE_CONTROL <- "public,max-age=31536000,immutable"

# ==============================================================================
# FIX CONFIG DEPENDENCY
# ==============================================================================
# LOCAL_COG depends on smoothing settings, so define it after those settings.

LOCAL_COG <- file.path(
  WORK_DIR,
  sprintf(
    "stage4_daily_%s%s%s_%02dx_r%02d.tif",
    YYYY, MM, DD,
    SMOOTH_UPSAMPLE_FACT,
    SMOOTH_RADIUS_CELLS
  )
)

# ==============================================================================
# AWS CLIENTS
# ==============================================================================

s3 <- paws::s3(
  config = list(
    region = AWS_REGION
  )
)

sts <- paws::sts(
  config = list(
    region = AWS_REGION
  )
)

message("")
message("AWS identity:")
print(sts$get_caller_identity())

# ==============================================================================
# HELPER: DOWNLOAD S3 OBJECT TO LOCAL FILE
# ==============================================================================

download_s3_object <- function(bucket, key, local_file) {

  message("Downloading:")
  message("s3://", bucket, "/", key)
  message(" -> ", local_file)

  obj <- s3$get_object(
    Bucket = bucket,
    Key = key
  )

  writeBin(
    obj$Body,
    local_file
  )

  invisible(local_file)
}

# ==============================================================================
# 1. GET INPUTS FROM S3
# ==============================================================================

message("")
message("==============================================================")
message("INPUTS")
message("==============================================================")

if (!file.exists(LOCAL_PRECIP)) {

  download_s3_object(
    bucket = PIPELINE_BUCKET,
    key = PRECIP_KEY,
    local_file = LOCAL_PRECIP
  )

} else {

  message("Using cached local rainfall parquet:")
  message(LOCAL_PRECIP)
}

if (!file.exists(LOCAL_HRAP)) {

  download_s3_object(
    bucket = PIPELINE_BUCKET,
    key = HRAP_KEY,
    local_file = LOCAL_HRAP
  )

} else {

  message("Using cached local Texas HRAP grid:")
  message(LOCAL_HRAP)
}

# ==============================================================================
# 2. READ DAILY RAINFALL
# ==============================================================================

message("")
message("==============================================================")
message("READING RAINFALL")
message("==============================================================")

precip <- arrow::read_parquet(
  LOCAL_PRECIP
) |>
  as.data.frame()

message("Map date:     ", MAP_DATE)
message("Parquet rows: ", format(nrow(precip), big.mark = ","))
message("Columns:      ", paste(names(precip), collapse = ", "))

if (!"grib_id" %in% names(precip)) {
  stop("Rainfall parquet does not contain grib_id.")
}

if (!"rain_mm" %in% names(precip)) {
  stop("Rainfall parquet does not contain rain_mm.")
}

precip <- precip |>
  transmute(
    grib_id = as.integer(grib_id),
    rain_mm = as.numeric(rain_mm)
  )

message("")
message("Rainfall summary, mm:")
print(summary(precip$rain_mm))

message(
  "Maximum rainfall = ",
  round(max(precip$rain_mm, na.rm = TRUE), 2),
  " mm = ",
  round(max(precip$rain_mm, na.rm = TRUE) / 25.4, 2),
  " inches"
)

# ==============================================================================
# 3. READ TEXAS HRAP GRID
# ==============================================================================

message("")
message("==============================================================")
message("READING TEXAS HRAP GRID")
message("==============================================================")

cells <- sf::read_sf(
  LOCAL_HRAP
)

message("HRAP polygons:   ", format(nrow(cells), big.mark = ","))
message("Geometry column: ", attr(cells, "sf_column"))
message("Source CRS:      ", sf::st_crs(cells)$input)

if (!"grib_id" %in% names(cells)) {
  stop("Texas cells.gpkg does not contain grib_id.")
}

cells <- cells |>
  mutate(
    grib_id = as.integer(grib_id)
  )

# ==============================================================================
# 4. JOIN RAINFALL TO HRAP
# ==============================================================================

message("")
message("==============================================================")
message("JOINING RAINFALL TO HRAP")
message("==============================================================")

rain_sf <- cells |>
  inner_join(
    precip |>
      select(
        grib_id,
        rain_mm
      ),
    by = "grib_id"
  )

message("Joined HRAP cells:   ", format(nrow(rain_sf), big.mark = ","))
message("Expected HRAP cells: ", format(nrow(cells), big.mark = ","))
message("Missing rain values: ", format(sum(!is.finite(rain_sf$rain_mm)), big.mark = ","))

if (nrow(rain_sf) != nrow(cells)) {
  warning(
    "Joined row count does not equal HRAP row count. ",
    "Inspect grib_id matching before using the output."
  )
}

rain_sf <- rain_sf |>
  filter(
    is.finite(rain_mm)
  ) |>
  mutate(
    support = 1
  )

# ==============================================================================
# 5. PROJECT TO WEB MERCATOR
# ==============================================================================

message("")
message("==============================================================")
message("PROJECTING TO EPSG:3857")
message("==============================================================")

rain_sf <- rain_sf |>
  st_make_valid() |>
  st_transform(
    crs = 3857
  )

# ==============================================================================
# 6. ESTIMATE NATIVE HRAP RASTER RESOLUTION
# ==============================================================================

message("")
message("==============================================================")
message("ESTIMATING NATIVE RASTER")
message("==============================================================")

cell_area_m2 <- suppressWarnings(
  as.numeric(
    st_area(rain_sf)
  )
)

native_res_m <- median(
  sqrt(
    cell_area_m2[
      is.finite(cell_area_m2) &
      cell_area_m2 > 0
    ]
  ),
  na.rm = TRUE
)

if (!is.finite(native_res_m) || native_res_m <= 0) {
  stop("Could not estimate native HRAP resolution.")
}

message(
  "Estimated native HRAP resolution: ",
  format(round(native_res_m, 1), big.mark = ","),
  " m"
)

# ==============================================================================
# 7. CREATE NATIVE RASTER TEMPLATE
# ==============================================================================

v_rain <- terra::vect(
  rain_sf
)

e <- terra::ext(
  v_rain
)

e <- terra::ext(
  terra::xmin(e) - native_res_m,
  terra::xmax(e) + native_res_m,
  terra::ymin(e) - native_res_m,
  terra::ymax(e) + native_res_m
)

r_template <- terra::rast(
  e,
  resolution = c(
    native_res_m,
    native_res_m
  ),
  crs = "EPSG:3857"
)

native_cells <- terra::ncell(r_template)
native_rows  <- terra::nrow(r_template)
native_cols  <- terra::ncol(r_template)

# ==============================================================================
# 8. REPORT OUTPUT SIZE
# ==============================================================================

fine_rows <- native_rows * SMOOTH_UPSAMPLE_FACT
fine_cols <- native_cols * SMOOTH_UPSAMPLE_FACT
fine_cells <- fine_rows * fine_cols
fine_res_m <- native_res_m / SMOOTH_UPSAMPLE_FACT

fine_single_raster_mb <- fine_cells * 4 / 1024^2

message("")
message("--------------------------------------------------------------")
message("STATEWIDE RASTER SIZE")
message("--------------------------------------------------------------")

message("Map date:                ", MAP_DATE)
message("Native resolution:       ", round(native_res_m, 1), " m")
message(
  "Native dimensions:       ",
  format(native_cols, big.mark = ","),
  " cols x ",
  format(native_rows, big.mark = ","),
  " rows"
)
message("Native raster cells:     ", format(native_cells, big.mark = ","))
message("")
message("Requested upsample:      ", SMOOTH_UPSAMPLE_FACT, "x")
message("Fine resolution:         ", round(fine_res_m, 1), " m")
message(
  "Fine dimensions:         ",
  format(fine_cols, big.mark = ","),
  " cols x ",
  format(fine_rows, big.mark = ","),
  " rows"
)
message("Fine raster cells:       ", format(fine_cells, big.mark = ","))
message("One FLT4S layer:         ~", round(fine_single_raster_mb, 1), " MB uncompressed")
message("")
message("Local output:             ", LOCAL_COG)
message("Canonical S3 output:      s3://", WEB_BUCKET, "/", OUTPUT_KEY)
message("--------------------------------------------------------------")

# ==============================================================================
# 9. RASTERIZE ORIGINAL STAGE IV
# ==============================================================================

message("")
message("==============================================================")
message("RASTERIZING ORIGINAL STAGE IV")
message("==============================================================")

t0 <- Sys.time()

r_native <- terra::rasterize(
  v_rain,
  r_template,
  field = "rain_mm",
  background = NA,
  touches = TRUE,
  fun = "mean"
)

r_support <- terra::rasterize(
  v_rain,
  r_template,
  field = "support",
  background = NA,
  touches = TRUE,
  fun = "max"
)

names(r_native) <- "rain_mm"

message(
  "Rasterize seconds: ",
  round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
)

# ==============================================================================
# 10. BILINEAR UPSAMPLE
# ==============================================================================

message("")
message("==============================================================")
message(SMOOTH_UPSAMPLE_FACT, "x BILINEAR UPSAMPLE")
message("==============================================================")

t0 <- Sys.time()

r_fine <- terra::disagg(
  r_native,
  fact = SMOOTH_UPSAMPLE_FACT,
  method = "bilinear"
)

r_support_fine <- terra::disagg(
  r_support,
  fact = SMOOTH_UPSAMPLE_FACT,
  method = "near"
)

message(
  "Bilinear disaggregation seconds: ",
  round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
)

message("")
message("Fine raster:")
print(r_fine)

# ==============================================================================
# 11. GAUSSIAN KERNEL
# ==============================================================================

make_gaussian_kernel <- function(
    radius_cells = 2,
    sigma_cells = NULL
) {

  radius_cells <- as.integer(
    max(
      1,
      radius_cells
    )
  )

  if (is.null(sigma_cells)) {
    sigma_cells <- max(
      0.5,
      radius_cells / 2
    )
  }

  ij <- seq(
    -radius_cells,
    radius_cells,
    by = 1
  )

  w <- outer(
    ij,
    ij,
    function(x, y) {
      exp(
        -(
          (x^2 + y^2) /
          (2 * sigma_cells^2)
        )
      )
    }
  )

  w / sum(
    w,
    na.rm = TRUE
  )
}

sigma_to_use <- if (is.na(SMOOTH_SIGMA_CELLS)) {
  NULL
} else {
  SMOOTH_SIGMA_CELLS
}

gaussian_w <- make_gaussian_kernel(
  radius_cells = SMOOTH_RADIUS_CELLS,
  sigma_cells = sigma_to_use
)

message("")
message("Gaussian radius: ", SMOOTH_RADIUS_CELLS, " fine cells")
message(
  "Gaussian sigma:  ",
  if (is.null(sigma_to_use)) {
    paste0("automatic (", SMOOTH_RADIUS_CELLS / 2, " fine cells)")
  } else {
    sigma_to_use
  }
)

# ==============================================================================
# 12. GAUSSIAN SMOOTHING
# ==============================================================================

message("")
message("==============================================================")
message("GAUSSIAN SMOOTHING")
message("==============================================================")

t0 <- Sys.time()

r_num <- terra::focal(
  r_fine,
  w = gaussian_w,
  fun = "sum",
  na.policy = "omit",
  fillvalue = NA
)

r_valid <- !is.na(
  r_fine
)

r_den <- terra::focal(
  r_valid,
  w = gaussian_w,
  fun = "sum",
  na.policy = "omit",
  fillvalue = NA
)

r_smooth <- r_num / r_den

r_smooth <- terra::mask(
  r_smooth,
  r_support_fine
)

names(r_smooth) <- "rain_mm"

message(
  "Gaussian smoothing seconds: ",
  round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
)

# ==============================================================================
# 13. VALUE QA
# ==============================================================================

message("")
message("==============================================================")
message("VALUE QA")
message("==============================================================")

message("")
message("Original native raster:")

print(
  terra::global(
    r_native,
    c("min", "mean", "max"),
    na.rm = TRUE
  )
)

message("")
message("Final smooth raster:")

print(
  terra::global(
    r_smooth,
    c("min", "mean", "max"),
    na.rm = TRUE
  )
)

# ==============================================================================
# 14. WRITE LOCAL CLOUD OPTIMIZED GEOTIFF
# ==============================================================================

message("")
message("==============================================================")
message("WRITING LOCAL COG")
message("==============================================================")

if (file.exists(LOCAL_COG) && !OVERWRITE_LOCAL) {
  stop(
    "Local output already exists and OVERWRITE_LOCAL = FALSE:\n",
    LOCAL_COG
  )
}

t0 <- Sys.time()

terra::writeRaster(
  r_smooth,
  LOCAL_COG,
  overwrite = OVERWRITE_LOCAL,
  filetype = "COG",
  datatype = "FLT4S",
  NAflag = -9999,
  gdal = c(
    "COMPRESS=DEFLATE",
    "LEVEL=6",
    "BLOCKSIZE=512",
    "OVERVIEWS=AUTO",
    "RESAMPLING=AVERAGE",
    "BIGTIFF=IF_SAFER",
    "NUM_THREADS=ALL_CPUS"
  )
)

message(
  "COG write seconds: ",
  round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
)

# ==============================================================================
# 15. READ LOCAL COG BACK FOR QA
# ==============================================================================

message("")
message("==============================================================")
message("READING FINAL COG BACK FOR QA")
message("==============================================================")

cog_check <- terra::rast(
  LOCAL_COG
)

print(
  cog_check
)

cog_size_mb <- file.info(
  LOCAL_COG
)$size / 1024^2

message("")
message("COG file size: ", round(cog_size_mb, 1), " MB")

# ==============================================================================
# 16. UPLOAD CANONICAL COG TO S3
# ==============================================================================

if (UPLOAD_TO_S3) {

  message("")
  message("==============================================================")
  message("UPLOADING CANONICAL COG TO S3")
  message("==============================================================")

  s3$put_object(
    Bucket = WEB_BUCKET,
    Key = OUTPUT_KEY,
    Body = LOCAL_COG,
    ContentType = "image/tiff",
    CacheControl = COG_CACHE_CONTROL
  )

  message("")
  message(
    "Uploaded:\n",
    "s3://", WEB_BUCKET, "/", OUTPUT_KEY
  )

  check <- s3$head_object(
    Bucket = WEB_BUCKET,
    Key = OUTPUT_KEY
  )

  message("")
  message("S3 object QA:")
  print(
    check[c(
      "ContentLength",
      "ContentType",
      "CacheControl",
      "ETag"
    )]
  )

} else {

  message("")
  message("UPLOAD_TO_S3 = FALSE; skipped S3 upload.")
}

# ==============================================================================
# DONE
# ==============================================================================

message("")
message("==============================================================")
message("DONE")
message("==============================================================")

message("Map date:       ", MAP_DATE)
message("Local COG:      ", LOCAL_COG)
message("Canonical COG:  s3://", WEB_BUCKET, "/", OUTPUT_KEY)
