# ==============================================================================
# TEXAS STAGE IV -> DATE-RANGE TOTAL PARQUET + SMOOTH COG
#
# Purpose:
#   Build a one-off/configurable Texas Stage IV rainfall total for a date range
#   by subtracting two Texas YTD precipitation parquets:
#
#       RANGE TOTAL = END-DATE YTD - START-DATE YTD
#
#   grib_id is used to match Stage IV / HRAP cells.
#
#   The resulting range-total parquet is saved locally and then used to build a
#   smoothed Cloud Optimized GeoTIFF (COG) for MapLibre display.
#
# IMPORTANT DATE SEMANTICS:
#   This script does exactly END YTD minus START YTD, as configured below.
#   For the initial meeting map:
#
#       2026-07-18 YTD - 2026-07-11 YTD
#
#   Because this relies on YTD subtraction, START_DATE and END_DATE must be in
#   the same calendar year.
#
# Local-only workflow:
#   - Downloads source YTD parquets from S3 if not already cached locally.
#   - Downloads the Texas HRAP cells.gpkg if not already cached locally.
#   - Writes a local date-range parquet.
#   - Writes a local smoothed COG.
#   - DOES NOT upload anything to S3.
#
# Working root:
#   C:/stg4/MapLibrePOC/texas_COG_range
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
# DATE RANGE
# --------------------------------------------------------------------------
#
# Change these two values for another same-calendar-year accumulation.
#
START_DATE <- as.Date("2026-07-11")
END_DATE   <- as.Date("2026-07-18")

if (is.na(START_DATE) || is.na(END_DATE)) {
  stop("START_DATE and END_DATE must be valid Date values.")
}

if (END_DATE <= START_DATE) {
  stop("END_DATE must be later than START_DATE.")
}

if (format(START_DATE, "%Y") != format(END_DATE, "%Y")) {
  stop(
    "START_DATE and END_DATE must be in the same calendar year because ",
    "this workflow subtracts YTD parquets."
  )
}

START_YYYY <- format(START_DATE, "%Y")
START_MM   <- format(START_DATE, "%m")
START_DD   <- format(START_DATE, "%d")

END_YYYY <- format(END_DATE, "%Y")
END_MM   <- format(END_DATE, "%m")
END_DD   <- format(END_DATE, "%d")

START_TAG <- format(START_DATE, "%Y%m%d")
END_TAG   <- format(END_DATE, "%Y%m%d")
RANGE_TAG <- paste0(START_TAG, "_to_", END_TAG)

# --------------------------------------------------------------------------
# AWS
# --------------------------------------------------------------------------
AWS_REGION     <- "us-east-2"
PIPELINE_BUCKET <- "stg4-24hr-aws-pipeline"

YTD_PREFIX <- paste0(
  "CONUS_subset/production_areas/texas_mrb/",
  "derived_ytd_precip"
)

START_YTD_KEY <- sprintf(
  "%s/year=%s/month=%s/day=%s/part-0.parquet",
  YTD_PREFIX,
  START_YYYY,
  START_MM,
  START_DD
)

END_YTD_KEY <- sprintf(
  "%s/year=%s/month=%s/day=%s/part-0.parquet",
  YTD_PREFIX,
  END_YYYY,
  END_MM,
  END_DD
)

# Texas HRAP geometry is stable and reused across runs.
HRAP_KEY <- paste0(
  "CONUS_subset/config/aoi/texas_mrb/",
  "assets/cells.gpkg"
)

# --------------------------------------------------------------------------
# LOCAL WORKING DIRECTORY
# --------------------------------------------------------------------------
WORK_DIR <- "C:/stg4/MapLibrePOC/texas_COG_range"

SCRIPTS_DIR  <- file.path(WORK_DIR, "scripts")
DOWNLOAD_DIR <- file.path(WORK_DIR, "downloads")
YTD_DIR      <- file.path(DOWNLOAD_DIR, "ytd")
GRID_DIR     <- file.path(DOWNLOAD_DIR, "grid")
OUTPUT_DIR   <- file.path(WORK_DIR, "output")
PARQUET_DIR  <- file.path(OUTPUT_DIR, "parquet")
COG_DIR      <- file.path(OUTPUT_DIR, "cog")
TMP_DIR      <- file.path(WORK_DIR, "tmp", "terra")

dirs_to_make <- c(
  WORK_DIR,
  SCRIPTS_DIR,
  DOWNLOAD_DIR,
  YTD_DIR,
  GRID_DIR,
  OUTPUT_DIR,
  PARQUET_DIR,
  COG_DIR,
  TMP_DIR
)

invisible(
  lapply(
    dirs_to_make,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

terra::terraOptions(
  tempdir = TMP_DIR,
  progress = 1
)

# Mirror the YTD S3 partition structure locally.
START_YTD_DIR <- file.path(
  YTD_DIR,
  sprintf("year=%s", START_YYYY),
  sprintf("month=%s", START_MM),
  sprintf("day=%s", START_DD)
)

END_YTD_DIR <- file.path(
  YTD_DIR,
  sprintf("year=%s", END_YYYY),
  sprintf("month=%s", END_MM),
  sprintf("day=%s", END_DD)
)

dir.create(
  START_YTD_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  END_YTD_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

LOCAL_START_YTD <- file.path(
  START_YTD_DIR,
  "part-0.parquet"
)

LOCAL_END_YTD <- file.path(
  END_YTD_DIR,
  "part-0.parquet"
)

LOCAL_HRAP <- file.path(
  GRID_DIR,
  "texas_hrap_cells.gpkg"
)

LOCAL_RANGE_PARQUET <- file.path(
  PARQUET_DIR,
  sprintf(
    "texas_stage4_range_%s.parquet",
    RANGE_TAG
  )
)

# --------------------------------------------------------------------------
# YTD VALUE COLUMN
# --------------------------------------------------------------------------
#
# Leave NULL to auto-detect a likely precipitation column.
# If your YTD parquet uses a known name, set it explicitly, for example:
#
#   YTD_VALUE_COLUMN <- "rain_mm"
#
YTD_VALUE_COLUMN <- "rain_ytd_mm"

# --------------------------------------------------------------------------
# RANGE QA
# --------------------------------------------------------------------------
#
# Small negative floating-point noise is set to zero. A meaningful negative
# value indicates that the YTD subtraction is not behaving as expected and
# stops the run rather than silently producing bad rainfall.
#
NEGATIVE_TOL_MM <- 0.01

# --------------------------------------------------------------------------
# SMOOTHING
# --------------------------------------------------------------------------
#
# Kept consistent with the daily COG proof-of-concept workflow.
#
#   10L = proven baseline
#   15L = recommended finer display test
#   20L = aggressive test
#
SMOOTH_UPSAMPLE_FACT <- 15L
SMOOTH_RADIUS_CELLS  <- 3L

# NA = automatic sigma = radius / 2.
SMOOTH_SIGMA_CELLS <- NA_real_

# --------------------------------------------------------------------------
# LOCAL OUTPUT
# --------------------------------------------------------------------------
OVERWRITE_RANGE_PARQUET <- TRUE
OVERWRITE_COG           <- TRUE

LOCAL_COG <- file.path(
  COG_DIR,
  sprintf(
    "stage4_range_%s_%02dx_r%02d.tif",
    RANGE_TAG,
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
# HELPERS
# ==============================================================================

download_s3_object <- function(bucket, key, local_file) {

  dir.create(
    dirname(local_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

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


resolve_ytd_value_column <- function(x, requested = NULL) {

  if (!"grib_id" %in% names(x)) {
    stop(
      "YTD parquet does not contain grib_id. Columns are: ",
      paste(names(x), collapse = ", ")
    )
  }

  if (!is.null(requested)) {

    if (!requested %in% names(x)) {
      stop(
        "Configured YTD_VALUE_COLUMN '",
        requested,
        "' was not found. Columns are: ",
        paste(names(x), collapse = ", ")
      )
    }

    return(requested)
  }

  candidates <- c(
    "rain_ytd_mm",
    "rain_mm",
    "ytd_rain_mm",
    "ytd_precip_mm",
    "precip_mm",
    "precipitation_mm",
    "ytd_mm"
  )

  hits <- candidates[
    candidates %in% names(x)
  ]

  if (length(hits) >= 1) {
    message(
      "Auto-detected YTD precipitation column: ",
      hits[1]
    )
    return(hits[1])
  }

  numeric_candidates <- names(x)[
    vapply(
      x,
      is.numeric,
      logical(1)
    )
  ]

  numeric_candidates <- setdiff(
    numeric_candidates,
    "grib_id"
  )

  if (length(numeric_candidates) == 1) {
    message(
      "Using only non-grib_id numeric column as YTD precipitation: ",
      numeric_candidates
    )
    return(numeric_candidates)
  }

  stop(
    "Could not automatically identify the YTD precipitation column.\n",
    "Set YTD_VALUE_COLUMN near the top of this script.\n",
    "Available columns: ",
    paste(names(x), collapse = ", ")
  )
}


check_unique_grib_id <- function(x, label) {

  if (anyDuplicated(x$grib_id) > 0) {
    stop(
      label,
      " contains duplicate grib_id values."
    )
  }

  invisible(TRUE)
}


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

# ==============================================================================
# 1. DOWNLOAD / CACHE INPUTS
# ==============================================================================

message("")
message("==============================================================")
message("INPUTS")
message("==============================================================")

message("Start date: ", START_DATE)
message("End date:   ", END_DATE)
message("")
message("Calculation:")
message("  END YTD - START YTD")
message("  ", END_TAG, " - ", START_TAG)

if (!file.exists(LOCAL_START_YTD)) {

  download_s3_object(
    bucket = PIPELINE_BUCKET,
    key = START_YTD_KEY,
    local_file = LOCAL_START_YTD
  )

} else {

  message("")
  message("Using cached START YTD parquet:")
  message(LOCAL_START_YTD)
}

if (!file.exists(LOCAL_END_YTD)) {

  download_s3_object(
    bucket = PIPELINE_BUCKET,
    key = END_YTD_KEY,
    local_file = LOCAL_END_YTD
  )

} else {

  message("")
  message("Using cached END YTD parquet:")
  message(LOCAL_END_YTD)
}

if (!file.exists(LOCAL_HRAP)) {

  download_s3_object(
    bucket = PIPELINE_BUCKET,
    key = HRAP_KEY,
    local_file = LOCAL_HRAP
  )

} else {

  message("")
  message("Using cached Texas HRAP grid:")
  message(LOCAL_HRAP)
}

# ==============================================================================
# 2. READ YTD PARQUETS
# ==============================================================================

message("")
message("==============================================================")
message("READING YTD PARQUETS")
message("==============================================================")

start_raw <- arrow::read_parquet(
  LOCAL_START_YTD
) |>
  as.data.frame()

end_raw <- arrow::read_parquet(
  LOCAL_END_YTD
) |>
  as.data.frame()

message("")
message("START parquet:")
message("Rows:    ", format(nrow(start_raw), big.mark = ","))
message("Columns: ", paste(names(start_raw), collapse = ", "))

message("")
message("END parquet:")
message("Rows:    ", format(nrow(end_raw), big.mark = ","))
message("Columns: ", paste(names(end_raw), collapse = ", "))

start_value_col <- resolve_ytd_value_column(
  start_raw,
  YTD_VALUE_COLUMN
)

end_value_col <- resolve_ytd_value_column(
  end_raw,
  YTD_VALUE_COLUMN
)

if (start_value_col != end_value_col) {
  warning(
    "START and END YTD precipitation columns were detected with different names: ",
    start_value_col,
    " vs ",
    end_value_col,
    ". The script will still use each detected column."
  )
}

start_ytd <- start_raw |>
  transmute(
    grib_id = as.integer(grib_id),
    start_ytd_mm = as.numeric(.data[[start_value_col]])
  )

end_ytd <- end_raw |>
  transmute(
    grib_id = as.integer(grib_id),
    end_ytd_mm = as.numeric(.data[[end_value_col]])
  )

check_unique_grib_id(
  start_ytd,
  "START YTD parquet"
)

check_unique_grib_id(
  end_ytd,
  "END YTD parquet"
)

# ==============================================================================
# 3. VERIFY GRIB_ID MATCHING
# ==============================================================================

message("")
message("==============================================================")
message("VERIFYING GRIB_ID MATCH")
message("==============================================================")

missing_from_end <- start_ytd |>
  anti_join(
    end_ytd,
    by = "grib_id"
  )

missing_from_start <- end_ytd |>
  anti_join(
    start_ytd,
    by = "grib_id"
  )

message(
  "START rows: ",
  format(nrow(start_ytd), big.mark = ",")
)

message(
  "END rows:   ",
  format(nrow(end_ytd), big.mark = ",")
)

message(
  "START grib_ids missing from END: ",
  format(nrow(missing_from_end), big.mark = ",")
)

message(
  "END grib_ids missing from START: ",
  format(nrow(missing_from_start), big.mark = ",")
)

if (
  nrow(missing_from_end) > 0 ||
  nrow(missing_from_start) > 0
) {

  stop(
    "START and END YTD parquets do not contain identical grib_id sets. ",
    "Stopping before subtraction."
  )
}

# ==============================================================================
# 4. CALCULATE RANGE TOTAL
# ==============================================================================

message("")
message("==============================================================")
message("CALCULATING RANGE TOTAL")
message("==============================================================")

range_precip <- start_ytd |>
  inner_join(
    end_ytd,
    by = "grib_id"
  ) |>
  mutate(
    rain_mm_raw = end_ytd_mm - start_ytd_mm
  )

bad_negative <- range_precip |>
  filter(
    is.finite(rain_mm_raw),
    rain_mm_raw < -NEGATIVE_TOL_MM
  )

if (nrow(bad_negative) > 0) {

  print(
    bad_negative |>
      arrange(rain_mm_raw) |>
      head(20)
  )

  stop(
    "Found ",
    format(nrow(bad_negative), big.mark = ","),
    " range-total cells below -",
    NEGATIVE_TOL_MM,
    " mm. ",
    "YTD totals should not meaningfully decrease within one calendar year. ",
    "Inspect the source parquets before continuing."
  )
}

range_precip <- range_precip |>
  transmute(
    grib_id = grib_id,
    start_ytd_mm = start_ytd_mm,
    end_ytd_mm = end_ytd_mm,
    rain_mm = if_else(
      is.finite(rain_mm_raw),
      pmax(rain_mm_raw, 0),
      NA_real_
    ),
    start_date = as.character(START_DATE),
    end_date = as.character(END_DATE)
  )

message("")
message("Range rainfall summary, mm:")
print(summary(range_precip$rain_mm))

message(
  "Maximum range rainfall = ",
  round(max(range_precip$rain_mm, na.rm = TRUE), 2),
  " mm = ",
  round(max(range_precip$rain_mm, na.rm = TRUE) / 25.4, 2),
  " inches"
)

# ==============================================================================
# 5. WRITE RANGE PARQUET
# ==============================================================================

message("")
message("==============================================================")
message("WRITING RANGE PARQUET")
message("==============================================================")

if (
  file.exists(LOCAL_RANGE_PARQUET) &&
  !OVERWRITE_RANGE_PARQUET
) {
  stop(
    "Range parquet already exists and OVERWRITE_RANGE_PARQUET = FALSE:\n",
    LOCAL_RANGE_PARQUET
  )
}

arrow::write_parquet(
  range_precip,
  LOCAL_RANGE_PARQUET,
  compression = "snappy"
)

message("Wrote:")
message(LOCAL_RANGE_PARQUET)

# ==============================================================================
# 6. READ RANGE PARQUET BACK
# ==============================================================================
#
# The COG is intentionally built from the saved range parquet, not directly
# from the in-memory subtraction object.
#

message("")
message("==============================================================")
message("READING RANGE PARQUET BACK")
message("==============================================================")

precip <- arrow::read_parquet(
  LOCAL_RANGE_PARQUET
) |>
  as.data.frame() |>
  transmute(
    grib_id = as.integer(grib_id),
    rain_mm = as.numeric(rain_mm)
  )

message(
  "Range parquet rows: ",
  format(nrow(precip), big.mark = ",")
)

message(
  "Finite rain values: ",
  format(sum(is.finite(precip$rain_mm)), big.mark = ",")
)

# ==============================================================================
# 7. READ TEXAS HRAP GRID
# ==============================================================================

message("")
message("==============================================================")
message("READING TEXAS HRAP GRID")
message("==============================================================")

cells <- sf::read_sf(
  LOCAL_HRAP
)

message(
  "HRAP polygons:   ",
  format(nrow(cells), big.mark = ",")
)

message(
  "Geometry column: ",
  attr(cells, "sf_column")
)

message(
  "Source CRS:      ",
  sf::st_crs(cells)$input
)

if (!"grib_id" %in% names(cells)) {
  stop("Texas cells.gpkg does not contain grib_id.")
}

cells <- cells |>
  mutate(
    grib_id = as.integer(grib_id)
  )

check_unique_grib_id(
  cells,
  "Texas HRAP grid"
)

# ==============================================================================
# 8. JOIN RANGE RAINFALL TO HRAP
# ==============================================================================

message("")
message("==============================================================")
message("JOINING RANGE RAINFALL TO HRAP")
message("==============================================================")

rain_sf <- cells |>
  inner_join(
    precip,
    by = "grib_id"
  )

message(
  "Joined HRAP cells:   ",
  format(nrow(rain_sf), big.mark = ",")
)

message(
  "Expected HRAP cells: ",
  format(nrow(cells), big.mark = ",")
)

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
  filter(
    is.finite(rain_mm)
  ) |>
  mutate(
    support = 1
  )

# ==============================================================================
# 9. PROJECT TO WEB MERCATOR
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
# 10. ESTIMATE NATIVE HRAP RASTER RESOLUTION
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

if (
  !is.finite(native_res_m) ||
  native_res_m <= 0
) {
  stop("Could not estimate native HRAP resolution.")
}

message(
  "Estimated native HRAP resolution: ",
  format(round(native_res_m, 1), big.mark = ","),
  " m"
)

# ==============================================================================
# 11. CREATE NATIVE RASTER TEMPLATE
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
# 12. REPORT OUTPUT SIZE
# ==============================================================================

fine_rows  <- native_rows * SMOOTH_UPSAMPLE_FACT
fine_cols  <- native_cols * SMOOTH_UPSAMPLE_FACT
fine_cells <- fine_rows * fine_cols
fine_res_m <- native_res_m / SMOOTH_UPSAMPLE_FACT

fine_single_raster_mb <- fine_cells * 4 / 1024^2

message("")
message("--------------------------------------------------------------")
message("STATEWIDE RANGE RASTER SIZE")
message("--------------------------------------------------------------")

message("Start date:              ", START_DATE)
message("End date:                ", END_DATE)
message("Calculation:             END YTD - START YTD")
message("Native resolution:       ", round(native_res_m, 1), " m")

message(
  "Native dimensions:       ",
  format(native_cols, big.mark = ","),
  " cols x ",
  format(native_rows, big.mark = ","),
  " rows"
)

message(
  "Native raster cells:     ",
  format(native_cells, big.mark = ",")
)

message("")
message(
  "Requested upsample:      ",
  SMOOTH_UPSAMPLE_FACT,
  "x"
)

message(
  "Fine resolution:         ",
  round(fine_res_m, 1),
  " m"
)

message(
  "Fine dimensions:         ",
  format(fine_cols, big.mark = ","),
  " cols x ",
  format(fine_rows, big.mark = ","),
  " rows"
)

message(
  "Fine raster cells:       ",
  format(fine_cells, big.mark = ",")
)

message(
  "One FLT4S layer:         ~",
  round(fine_single_raster_mb, 1),
  " MB uncompressed"
)

message("")
message("Range parquet:            ", LOCAL_RANGE_PARQUET)
message("Local COG:                ", LOCAL_COG)
message("--------------------------------------------------------------")

# ==============================================================================
# 13. RASTERIZE RANGE TOTAL
# ==============================================================================

message("")
message("==============================================================")
message("RASTERIZING RANGE TOTAL")
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
  round(
    as.numeric(
      difftime(
        Sys.time(),
        t0,
        units = "secs"
      )
    ),
    2
  )
)

# ==============================================================================
# 14. BILINEAR UPSAMPLE
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
  round(
    as.numeric(
      difftime(
        Sys.time(),
        t0,
        units = "secs"
      )
    ),
    2
  )
)

message("")
message("Fine raster:")
print(r_fine)

# ==============================================================================
# 15. GAUSSIAN KERNEL
# ==============================================================================

sigma_to_use <- if (
  is.na(SMOOTH_SIGMA_CELLS)
) {
  NULL
} else {
  SMOOTH_SIGMA_CELLS
}

gaussian_w <- make_gaussian_kernel(
  radius_cells = SMOOTH_RADIUS_CELLS,
  sigma_cells = sigma_to_use
)

message("")
message(
  "Gaussian radius: ",
  SMOOTH_RADIUS_CELLS,
  " fine cells"
)

message(
  "Gaussian sigma:  ",
  if (is.null(sigma_to_use)) {
    paste0(
      "automatic (",
      SMOOTH_RADIUS_CELLS / 2,
      " fine cells)"
    )
  } else {
    sigma_to_use
  }
)

# ==============================================================================
# 16. GAUSSIAN SMOOTHING
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
  round(
    as.numeric(
      difftime(
        Sys.time(),
        t0,
        units = "secs"
      )
    ),
    2
  )
)

# ==============================================================================
# 17. VALUE QA
# ==============================================================================

message("")
message("==============================================================")
message("VALUE QA")
message("==============================================================")

message("")
message("Original native range raster:")

print(
  terra::global(
    r_native,
    c("min", "mean", "max"),
    na.rm = TRUE
  )
)

message("")
message("Final smooth range raster:")

print(
  terra::global(
    r_smooth,
    c("min", "mean", "max"),
    na.rm = TRUE
  )
)

# ==============================================================================
# 18. WRITE LOCAL CLOUD OPTIMIZED GEOTIFF
# ==============================================================================

message("")
message("==============================================================")
message("WRITING LOCAL COG")
message("==============================================================")

if (
  file.exists(LOCAL_COG) &&
  !OVERWRITE_COG
) {
  stop(
    "Local COG already exists and OVERWRITE_COG = FALSE:\n",
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
  round(
    as.numeric(
      difftime(
        Sys.time(),
        t0,
        units = "secs"
      )
    ),
    2
  )
)

# ==============================================================================
# 19. READ LOCAL COG BACK FOR QA
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
message(
  "COG file size: ",
  round(cog_size_mb, 1),
  " MB"
)

# ==============================================================================
# DONE
# ==============================================================================

message("")
message("==============================================================")
message("DONE")
message("==============================================================")

message("Start date:      ", START_DATE)
message("End date:        ", END_DATE)
message("Calculation:     END YTD - START YTD")
message("Range parquet:   ", LOCAL_RANGE_PARQUET)
message("Local COG:       ", LOCAL_COG)
message("")
message("No files were uploaded to S3.")
