# ==============================================================================
# 01_parquet_to_cog.R
#
# Purpose:
#   Build one statewide smoothed Stage IV daily rainfall COG for web mapping.
#
# Manual POC architecture:
#   S3 daily Texas parquet
#       + local Texas HRAP geometry (future Docker image asset)
#       -> local cached parquet
#       -> EPSG:3857 native raster
#       -> bilinear upsample
#       -> Gaussian smooth
#       -> canonical local COG
#
# Canonical local output:
#   C:/stg4/MapLibrePOC/poc/manual/cog/YYYYMMDD/stage4_daily.tif
#
# Optional canonical S3 output:
#   s3://web-map-assets/basemaps/rainfall/texas/daily/YYYY/MM/DD/stage4_daily.tif
#
# Notes:
#   - Stage IV / HRAP remains authoritative for analysis.
#   - This COG is a cartographic display surface.
#   - The date lives in the directory path; the filename stays stable.
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

# Change ONE value here to build another daily COG.
MAP_DATE <- as.Date("2026-07-16")

# Manual POC root. Later this can map to Docker work/tmp paths.
ROOT_DIR <- "C:/stg4/MapLibrePOC/poc/manual"

SOURCE_DIR <- file.path(ROOT_DIR, "source")
COG_ROOT   <- file.path(ROOT_DIR, "cog")

# Static HRAP geometry; treat as a baked-in Docker asset later.
LOCAL_HRAP <- file.path(SOURCE_DIR, "texas_hrap_cells.gpkg")

# Smoothing / interpolation config.
SMOOTH_UPSAMPLE_FACT <- 15L
SMOOTH_RADIUS_CELLS  <- 3L
SMOOTH_SIGMA_CELLS   <- NA_real_  # NA = radius / 2

# AWS.
AWS_REGION      <- "us-east-2"
PIPELINE_BUCKET <- "stg4-24hr-aws-pipeline"
WEB_BUCKET      <- "web-map-assets"

# Run behavior.
OVERWRITE_PARQUET <- FALSE
OVERWRITE_COG     <- TRUE

# Keep FALSE while stabilizing the manual workflow.
UPLOAD_COG_TO_S3 <- FALSE

COG_CACHE_CONTROL <- "public,max-age=31536000,immutable"

# ==============================================================================
# DERIVED DATE / PATH CONFIG
# ==============================================================================

YYYY    <- format(MAP_DATE, "%Y")
MM      <- format(MAP_DATE, "%m")
DD      <- format(MAP_DATE, "%d")
DATE_ID <- format(MAP_DATE, "%Y%m%d")

PRECIP_KEY <- sprintf(
  paste0(
    "CONUS_subset/production_areas/texas_mrb/",
    "precip/precip_parquet/",
    "year=%s/month=%s/day=%s/part-0.parquet"
  ),
  YYYY, MM, DD
)

LOCAL_PRECIP <- file.path(
  SOURCE_DIR,
  sprintf("texas_%s.parquet", DATE_ID)
)

COG_DATE_DIR <- file.path(COG_ROOT, DATE_ID)
LOCAL_COG <- file.path(COG_DATE_DIR, "stage4_daily.tif")

OUTPUT_KEY <- sprintf(
  "basemaps/rainfall/texas/daily/%s/%s/%s/stage4_daily.tif",
  YYYY, MM, DD
)

dir.create(SOURCE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(COG_DATE_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(LOCAL_HRAP)) {
  stop("Texas HRAP geometry not found:\n", LOCAL_HRAP)
}

message("")
message("==============================================================")
message("CONFIG")
message("==============================================================")
message("Map date:            ", MAP_DATE)
message("Date ID:             ", DATE_ID)
message("Manual root:         ", ROOT_DIR)
message("Local HRAP:          ", LOCAL_HRAP)
message("Local parquet:       ", LOCAL_PRECIP)
message("Local COG:           ", LOCAL_COG)
message("Upsample factor:     ", SMOOTH_UPSAMPLE_FACT, "x")
message("Gaussian radius:     ", SMOOTH_RADIUS_CELLS, " fine cells")
message(
  "Gaussian sigma:      ",
  if (is.na(SMOOTH_SIGMA_CELLS)) {
    paste0("automatic (", SMOOTH_RADIUS_CELLS / 2, " fine cells)")
  } else {
    SMOOTH_SIGMA_CELLS
  }
)
message("Upload COG to S3:    ", UPLOAD_COG_TO_S3)

# ==============================================================================
# AWS CLIENTS
# ==============================================================================

s3 <- paws::s3(config = list(region = AWS_REGION))
sts <- paws::sts(config = list(region = AWS_REGION))

message("")
message("AWS identity:")
print(sts$get_caller_identity())

download_s3_object <- function(bucket, key, local_file) {
  message("")
  message("Downloading:")
  message("s3://", bucket, "/", key)
  message(" -> ", local_file)

  obj <- s3$get_object(Bucket = bucket, Key = key)
  writeBin(obj$Body, local_file)

  invisible(local_file)
}

# ==============================================================================
# 1. DOWNLOAD DAILY PARQUET
# ==============================================================================

message("")
message("==============================================================")
message("1. DAILY PARQUET")
message("==============================================================")

if (!file.exists(LOCAL_PRECIP) || OVERWRITE_PARQUET) {
  download_s3_object(
    bucket = PIPELINE_BUCKET,
    key = PRECIP_KEY,
    local_file = LOCAL_PRECIP
  )
} else {
  message("Using cached local rainfall parquet:")
  message(LOCAL_PRECIP)
}

if (!file.exists(LOCAL_PRECIP)) {
  stop("Rainfall parquet was not created:\n", LOCAL_PRECIP)
}

# ==============================================================================
# 2. READ DAILY RAINFALL
# ==============================================================================

message("")
message("==============================================================")
message("2. READING RAINFALL")
message("==============================================================")

precip <- arrow::read_parquet(LOCAL_PRECIP) |>
  as.data.frame()

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
# 3. READ LOCAL TEXAS HRAP GRID
# ==============================================================================

message("")
message("==============================================================")
message("3. READING LOCAL TEXAS HRAP GRID")
message("==============================================================")

cells <- sf::read_sf(LOCAL_HRAP)

message("HRAP polygons:   ", format(nrow(cells), big.mark = ","))
message("Geometry column: ", attr(cells, "sf_column"))
message("Source CRS:      ", sf::st_crs(cells)$input)

if (!"grib_id" %in% names(cells)) {
  stop("texas_hrap_cells.gpkg does not contain grib_id.")
}

cells <- cells |>
  mutate(grib_id = as.integer(grib_id))

# ==============================================================================
# 4. JOIN RAINFALL TO HRAP
# ==============================================================================

message("")
message("==============================================================")
message("4. JOINING RAINFALL TO HRAP")
message("==============================================================")

rain_sf <- cells |>
  inner_join(
    precip |>
      select(grib_id, rain_mm),
    by = "grib_id"
  )

message("Joined HRAP cells:   ", format(nrow(rain_sf), big.mark = ","))
message("Expected HRAP cells: ", format(nrow(cells), big.mark = ","))
message(
  "Missing rain values: ",
  format(sum(!is.finite(rain_sf$rain_mm)), big.mark = ",")
)

if (nrow(rain_sf) != nrow(cells)) {
  warning(
    "Joined row count does not equal HRAP row count. ",
    "Inspect grib_id matching before using the output."
  )
}

rain_sf <- rain_sf |>
  filter(is.finite(rain_mm)) |>
  mutate(support = 1)

# ==============================================================================
# 5. PROJECT TO WEB MERCATOR
# ==============================================================================

message("")
message("==============================================================")
message("5. PROJECTING TO EPSG:3857")
message("==============================================================")

rain_sf <- rain_sf |>
  st_make_valid() |>
  st_transform(crs = 3857)

# ==============================================================================
# 6. ESTIMATE NATIVE HRAP RASTER RESOLUTION
# ==============================================================================

message("")
message("==============================================================")
message("6. ESTIMATING NATIVE RASTER")
message("==============================================================")

cell_area_m2 <- suppressWarnings(as.numeric(st_area(rain_sf)))

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

v_rain <- terra::vect(rain_sf)
e <- terra::ext(v_rain)

e <- terra::ext(
  terra::xmin(e) - native_res_m,
  terra::xmax(e) + native_res_m,
  terra::ymin(e) - native_res_m,
  terra::ymax(e) + native_res_m
)

r_template <- terra::rast(
  e,
  resolution = c(native_res_m, native_res_m),
  crs = "EPSG:3857"
)

native_cells <- terra::ncell(r_template)
native_rows  <- terra::nrow(r_template)
native_cols  <- terra::ncol(r_template)

fine_rows  <- native_rows * SMOOTH_UPSAMPLE_FACT
fine_cols  <- native_cols * SMOOTH_UPSAMPLE_FACT
fine_cells <- fine_rows * fine_cols
fine_res_m <- native_res_m / SMOOTH_UPSAMPLE_FACT

message("")
message("--------------------------------------------------------------")
message("STATEWIDE RASTER SIZE")
message("--------------------------------------------------------------")
message("Native resolution:   ", round(native_res_m, 1), " m")
message(
  "Native dimensions:   ",
  format(native_cols, big.mark = ","),
  " cols x ",
  format(native_rows, big.mark = ","),
  " rows"
)
message("Native cells:        ", format(native_cells, big.mark = ","))
message("")
message("Requested upsample:  ", SMOOTH_UPSAMPLE_FACT, "x")
message("Fine resolution:     ", round(fine_res_m, 1), " m")
message(
  "Fine dimensions:     ",
  format(fine_cols, big.mark = ","),
  " cols x ",
  format(fine_rows, big.mark = ","),
  " rows"
)
message("Fine cells:          ", format(fine_cells, big.mark = ","))
message("--------------------------------------------------------------")

# ==============================================================================
# 8. RASTERIZE ORIGINAL STAGE IV
# ==============================================================================

message("")
message("==============================================================")
message("8. RASTERIZING ORIGINAL STAGE IV")
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
# 9. BILINEAR UPSAMPLE
# ==============================================================================

message("")
message("==============================================================")
message("9. ", SMOOTH_UPSAMPLE_FACT, "x BILINEAR UPSAMPLE")
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

# ==============================================================================
# 10. GAUSSIAN KERNEL
# ==============================================================================

make_gaussian_kernel <- function(radius_cells = 2, sigma_cells = NULL) {
  radius_cells <- as.integer(max(1, radius_cells))

  if (is.null(sigma_cells)) {
    sigma_cells <- max(0.5, radius_cells / 2)
  }

  ij <- seq(-radius_cells, radius_cells, by = 1)

  w <- outer(
    ij,
    ij,
    function(x, y) {
      exp(-((x^2 + y^2) / (2 * sigma_cells^2)))
    }
  )

  w / sum(w, na.rm = TRUE)
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
# 11. GAUSSIAN SMOOTHING
# ==============================================================================

message("")
message("==============================================================")
message("11. GAUSSIAN SMOOTHING")
message("==============================================================")

t0 <- Sys.time()

r_num <- terra::focal(
  r_fine,
  w = gaussian_w,
  fun = "sum",
  na.policy = "omit",
  fillvalue = NA
)

r_valid <- !is.na(r_fine)

r_den <- terra::focal(
  r_valid,
  w = gaussian_w,
  fun = "sum",
  na.policy = "omit",
  fillvalue = NA
)

r_smooth <- r_num / r_den
r_smooth <- terra::mask(r_smooth, r_support_fine)
names(r_smooth) <- "rain_mm"

message(
  "Gaussian smoothing seconds: ",
  round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
)

# ==============================================================================
# 12. VALUE QA
# ==============================================================================

message("")
message("==============================================================")
message("12. VALUE QA")
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
# 13. WRITE CANONICAL LOCAL COG
# ==============================================================================

message("")
message("==============================================================")
message("13. WRITING CANONICAL LOCAL COG")
message("==============================================================")

if (file.exists(LOCAL_COG) && !OVERWRITE_COG) {
  stop(
    "Local output already exists and OVERWRITE_COG = FALSE:\n",
    LOCAL_COG
  )
}

t0 <- Sys.time()

terra::writeRaster(
  r_smooth,
  LOCAL_COG,
  overwrite = OVERWRITE_COG,
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
# 14. READ FINAL COG BACK FOR QA
# ==============================================================================

message("")
message("==============================================================")
message("14. READING FINAL COG BACK FOR QA")
message("==============================================================")

cog_check <- terra::rast(LOCAL_COG)
print(cog_check)

cog_size_mb <- file.info(LOCAL_COG)$size / 1024^2

message("")
message("COG file size: ", round(cog_size_mb, 1), " MB")

message("")
message("Final COG value summary:")
print(
  terra::global(
    cog_check,
    c("min", "mean", "max"),
    na.rm = TRUE
  )
)

# ==============================================================================
# 15. OPTIONAL S3 UPLOAD
# ==============================================================================

if (UPLOAD_COG_TO_S3) {
  message("")
  message("==============================================================")
  message("15. UPLOADING CANONICAL COG TO S3")
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
    "s3://",
    WEB_BUCKET,
    "/",
    OUTPUT_KEY
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
  message("UPLOAD_COG_TO_S3 = FALSE; skipped S3 upload.")
}

# ==============================================================================
# DONE
# ==============================================================================

message("")
message("==============================================================")
message("DONE")
message("==============================================================")
message("Map date:       ", MAP_DATE)
message("Local parquet:  ", LOCAL_PRECIP)
message("Local HRAP:     ", LOCAL_HRAP)
message("Local COG:      ", LOCAL_COG)
message(
  "Canonical S3:  s3://",
  WEB_BUCKET,
  "/",
  OUTPUT_KEY
)
