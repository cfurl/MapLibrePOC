# ==============================================================================
# 01_parquet_to_cog.R
#
# Purpose:
#   Build one smoothed Stage IV daily rainfall COG for one configured large area.
#
# Production runtime contract:
#   Rscript 01_parquet_to_cog.R \
#     --area texas \
#     --cycle 2026100212
#
# Runtime inputs:
#   --area   : injected by Step Functions from enabled areas in render_config.json
#   --cycle  : injected by Step Functions from the upstream numerical cycle,
#              formatted YYYYMMDDHH (example: 2026100212)
#
# Config:
#   If --config is not supplied, the script downloads:
#
#   s3://stg4-24hr-large-area-render/
#   CONUS_subset/config/render/large_area/render_config.json
#
# AWS credentials:
#   Use the normal AWS credential chain:
#     - ECS task role in AWS
#     - mounted local ~/.aws credentials/profile for local Docker testing
#
# Stage responsibility:
#   numerical success -> daily parquet + cells.gpkg -> smoothed COG -> upload COG
#   -> COG metadata -> COG _SUCCESS markers
#
# Footprint invariant:
#   cells.gpkg defines a fixed area footprint. Missing rain_mm values remain
#   NoData inside that footprint and must never change raster extent/dimensions.
#
# This stage DOES write COG-scoped completion artifacts under:
#   .../precip/daily/cog/metadata/...
#   .../precip/daily/cog/signals/...
#
# These markers mean the durable COG stage is complete. They do NOT mean the
# browser-delivery / PMTiles render is complete.
#
# This stage DOES NOT write:
#   PMTiles _SUCCESS
#   latest.json
#   radar_frames.json
#
# Public/browser manifests are written only after the complete render chain
# succeeds.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(terra)
  library(arrow)
  library(paws)
  library(jsonlite)
})

# ==============================================================================
# EXTERNAL GDAL TOOLS
# ==============================================================================
#
# The Docker image installs gdal-bin, so these should resolve from PATH.
# We use gdal_translate explicitly for final COG creation and gdalinfo for QA.
# ==============================================================================

GDAL_TRANSLATE <- Sys.which("gdal_translate")
GDALINFO <- Sys.which("gdalinfo")

if (!nzchar(GDAL_TRANSLATE)) {
  stop("gdal_translate was not found on PATH.")
}

if (!nzchar(GDALINFO)) {
  stop("gdalinfo was not found on PATH.")
}

# ==============================================================================
# HELPERS
# ==============================================================================

`%||%` <- function(x, y) {
  if (
    is.null(x) ||
    length(x) == 0 ||
    (is.character(x) && !nzchar(x))
  ) {
    y
  } else {
    x
  }
}

parse_cli <- function(args) {

  value_after <- function(flag, default = NULL) {

    idx <- which(args == flag)

    if (length(idx) == 0) {
      return(default)
    }

    idx <- idx[[1]]

    if (idx >= length(args)) {
      stop("Missing value after ", flag)
    }

    args[[idx + 1]]
  }

  list(
    config = value_after(
      "--config",
      Sys.getenv("RENDER_CONFIG_LOCAL", unset = "")
    ),

    config_bucket = value_after(
      "--config-bucket",
      Sys.getenv(
        "RENDER_CONFIG_BUCKET",
        unset = "stg4-24hr-large-area-render"
      )
    ),

    config_key = value_after(
      "--config-key",
      Sys.getenv(
        "RENDER_CONFIG_KEY",
        unset = "CONUS_subset/config/render/large_area/render_config.json"
      )
    ),

    aws_region = value_after(
      "--aws-region",
      Sys.getenv("AWS_REGION", unset = "us-east-2")
    ),

    area = value_after(
      "--area",
      Sys.getenv("AREA_ID", unset = "")
    ),

    cycle = value_after(
      "--cycle",
      Sys.getenv("CYCLE_ID", unset = "")
    ),

    # Optional manual fallback for local testing only.
    date = value_after(
      "--date",
      Sys.getenv("MAP_DATE", unset = "")
    ),

    work_dir = value_after(
      "--work-dir",
      Sys.getenv("WORK_DIR", unset = "/work")
    )
  )
}

merge_lists <- function(base, override) {
  utils::modifyList(
    base %||% list(),
    override %||% list(),
    keep.null = TRUE
  )
}

render_template <- function(template, values) {

  out <- template

  for (nm in names(values)) {
    out <- gsub(
      paste0("{", nm, "}"),
      as.character(values[[nm]]),
      out,
      fixed = TRUE
    )
  }

  if (grepl("\\{[^}]+\\}", out)) {
    stop(
      "Unresolved placeholder in configured S3 key template:\n",
      out
    )
  }

  out
}

s3_object_exists <- function(s3, bucket, key) {

  tryCatch(
    {
      s3$head_object(
        Bucket = bucket,
        Key = key
      )
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}


verify_object_size <- function(
    s3,
    bucket,
    key,
    expected_bytes
) {

  head <- s3$head_object(
    Bucket = bucket,
    Key = key
  )

  actual_bytes <- as.numeric(
    head$ContentLength
  )

  if (
    !is.finite(actual_bytes) ||
    actual_bytes != expected_bytes
  ) {
    stop(
      "S3 object size verification failed for:\n",
      "s3://", bucket, "/", key,
      "\nExpected bytes: ", expected_bytes,
      "\nActual bytes:   ", actual_bytes
    )
  }

  invisible(head)
}


put_raw <- function(
    s3,
    bucket,
    key,
    body,
    content_type,
    cache_control = NULL
) {

  args <- list(
    Bucket = bucket,
    Key = key,
    Body = body,
    ContentType = content_type
  )

  if (
    !is.null(cache_control) &&
    nzchar(cache_control)
  ) {
    args$CacheControl <- cache_control
  }

  do.call(
    s3$put_object,
    args
  )

  invisible(TRUE)
}


verify_object_exists <- function(
    s3,
    bucket,
    key
) {

  s3$head_object(
    Bucket = bucket,
    Key = key
  )

  invisible(TRUE)
}

download_s3_object <- function(
    s3,
    bucket,
    key,
    local_file,
    overwrite = TRUE
) {

  if (file.exists(local_file) && !overwrite) {
    message("Using existing local file:")
    message(local_file)
    return(invisible(local_file))
  }

  dir.create(
    dirname(local_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  message("")
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

  if (!file.exists(local_file)) {
    stop(
      "S3 download did not create local file:\n",
      local_file
    )
  }

  invisible(local_file)
}

resolve_local_config <- function(cli, s3_bootstrap) {

  # Local config explicitly supplied.
  if (nzchar(cli$config)) {

    if (!file.exists(cli$config)) {
      stop(
        "Local config file does not exist:\n",
        cli$config
      )
    }

    return(
      normalizePath(
        cli$config,
        winslash = "/",
        mustWork = TRUE
      )
    )
  }

  # Otherwise download canonical config from S3.
  if (
    !nzchar(cli$config_bucket) ||
    !nzchar(cli$config_key)
  ) {
    stop(
      "No local --config provided, and config bucket/key are missing.\n",
      "Provide either:\n",
      "  --config /path/to/render_config.json\n",
      "or:\n",
      "  --config-bucket <bucket> --config-key <key>"
    )
  }

  local_config <- file.path(
    cli$work_dir,
    "config",
    basename(cli$config_key)
  )

  download_s3_object(
    s3 = s3_bootstrap,
    bucket = cli$config_bucket,
    key = cli$config_key,
    local_file = local_config,
    overwrite = TRUE
  )

  normalizePath(
    local_config,
    winslash = "/",
    mustWork = TRUE
  )
}

# ==============================================================================
# RUNTIME INPUTS
# ==============================================================================

cli <- parse_cli(
  commandArgs(trailingOnly = TRUE)
)

if (!nzchar(cli$area)) {
  stop(
    "Missing --area.\n\n",
    "Production example:\n",
    "  Rscript 01_parquet_to_cog.R ",
    "--area texas --cycle 2026100212"
  )
}

AREA_ID <- cli$area

# ------------------------------------------------------------------------------
# Cycle is the production runtime clock.
# ------------------------------------------------------------------------------

if (nzchar(cli$cycle)) {

  if (!grepl("^[0-9]{10}$", cli$cycle)) {
    stop(
      "Invalid --cycle value: ",
      cli$cycle,
      "\nExpected YYYYMMDDHH, e.g. 2026100212."
    )
  }

  CYCLE_ID <- cli$cycle

  MAP_DATE <- as.Date(
    substr(CYCLE_ID, 1, 8),
    format = "%Y%m%d"
  )

  CYCLE_HOUR <- substr(
    CYCLE_ID,
    9,
    10
  )

} else if (nzchar(cli$date)) {

  # Manual-development fallback only.
  MAP_DATE <- as.Date(cli$date)

  if (is.na(MAP_DATE)) {
    stop(
      "Invalid --date value: ",
      cli$date,
      "\nExpected YYYY-MM-DD."
    )
  }

  CYCLE_ID <- paste0(
    format(MAP_DATE, "%Y%m%d"),
    "12"
  )

  CYCLE_HOUR <- "12"

  warning(
    "--date was used without --cycle. ",
    "Assuming 12Z cycle for manual testing: ",
    CYCLE_ID
  )

} else {

  stop(
    "Missing --cycle.\n\n",
    "Production example:\n",
    "  Rscript 01_parquet_to_cog.R ",
    "--area texas --cycle 2026100212\n\n",
    "--date YYYY-MM-DD is retained only as a manual fallback."
  )
}

if (is.na(MAP_DATE)) {
  stop(
    "Could not derive MAP_DATE from cycle: ",
    CYCLE_ID
  )
}

WORK_ROOT <- cli$work_dir
BOOTSTRAP_REGION <- cli$aws_region %||% "us-east-2"

dir.create(
  WORK_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==============================================================================
# BOOTSTRAP AWS CLIENT + CONFIG DOWNLOAD
# ==============================================================================

s3_bootstrap <- paws::s3(
  config = list(
    region = BOOTSTRAP_REGION
  )
)

LOCAL_CONFIG <- resolve_local_config(
  cli = cli,
  s3_bootstrap = s3_bootstrap
)

# ==============================================================================
# LOAD + RESOLVE CONFIG
# ==============================================================================

config <- jsonlite::fromJSON(
  LOCAL_CONFIG,
  simplifyVector = FALSE
)

if (is.null(config$areas[[AREA_ID]])) {
  stop(
    "Area is not present in render config: ",
    AREA_ID
  )
}

area_cfg <- config$areas[[AREA_ID]]

if (identical(area_cfg$enabled, FALSE)) {
  stop(
    "Area is disabled in render config: ",
    AREA_ID
  )
}

aws_cfg <- config$aws %||% list()

storage_cfg <- merge_lists(
  config$storage,
  area_cfg$storage
)

smoothing_cfg <- merge_lists(
  config$defaults$smoothing,
  area_cfg$smoothing
)

run_cfg <- merge_lists(
  config$defaults$run,
  area_cfg$run
)

qa_cfg <- merge_lists(
  config$defaults$qa,
  area_cfg$qa
)

cog_cfg <- merge_lists(
  config$defaults$cog,
  area_cfg$cog
)

AWS_REGION <- aws_cfg$region %||% BOOTSTRAP_REGION

required_storage_fields <- c(
  "data_bucket",
  "render_bucket",
  "daily_parquet_key",
  "cells_key",
  "cog_key"
)

missing_storage_fields <- required_storage_fields[
  vapply(
    required_storage_fields,
    function(x) {
      is.null(storage_cfg[[x]]) ||
        !nzchar(storage_cfg[[x]])
    },
    logical(1)
  )
]

if (length(missing_storage_fields) > 0) {
  stop(
    "Missing required storage config field(s): ",
    paste(missing_storage_fields, collapse = ", ")
  )
}

DATA_BUCKET <- storage_cfg$data_bucket
RENDER_BUCKET <- storage_cfg$render_bucket

# ------------------------------------------------------------------------------
# Success-marker preference:
#
# 1. numerical_cycle_success_key if present in config
# 2. numerical_success_key (date-based fallback)
# ------------------------------------------------------------------------------

SUCCESS_KEY_TEMPLATE <- storage_cfg$numerical_cycle_success_key %||%
  storage_cfg$numerical_success_key

if (
  is.null(SUCCESS_KEY_TEMPLATE) ||
  !nzchar(SUCCESS_KEY_TEMPLATE)
) {
  stop(
    "Config must define either:\n",
    "  storage.numerical_cycle_success_key\n",
    "or:\n",
    "  storage.numerical_success_key"
  )
}

SMOOTH_UPSAMPLE_FACT <- as.integer(
  smoothing_cfg$upsample_factor
)

SMOOTH_RADIUS_CELLS <- as.integer(
  smoothing_cfg$radius_cells
)

SMOOTH_SIGMA_CELLS <- if (
  is.null(smoothing_cfg$sigma_cells)
) {
  NA_real_
} else {
  as.numeric(smoothing_cfg$sigma_cells)
}

if (
  !is.finite(SMOOTH_UPSAMPLE_FACT) ||
  SMOOTH_UPSAMPLE_FACT < 1
) {
  stop(
    "upsample_factor must be an integer >= 1."
  )
}

if (
  !is.finite(SMOOTH_RADIUS_CELLS) ||
  SMOOTH_RADIUS_CELLS < 1
) {
  stop(
    "radius_cells must be an integer >= 1."
  )
}

REQUIRE_NUMERICAL_SUCCESS <- isTRUE(
  run_cfg$require_numerical_success
)

OVERWRITE_INPUTS <- isTRUE(
  run_cfg$overwrite_inputs
)

OVERWRITE_COG <- isTRUE(
  run_cfg$overwrite_cog
)

UPLOAD_COG <- isTRUE(
  run_cfg$upload_cog
)

REQUIRE_FULL_CELL_MATCH <- isTRUE(
  qa_cfg$require_full_cell_match
)

COG_CACHE_CONTROL <- cog_cfg$cache_control %||%
  "public,max-age=31536000,immutable"

# COG-scoped metadata / signal paths. These can be promoted into
# render_config.json later as explicit storage keys; the defaults preserve the
# product-local layout:
#
#   cog/
#     year=...
#     metadata/year=.../render.json
#     signals/year=.../_SUCCESS
#     signals/cycle/cycle=.../_SUCCESS
#
COG_METADATA_TEMPLATE <- storage_cfg$cog_metadata_key %||%
  paste0(
    "CONUS_subset/production_areas/{area_id}/precip/daily/cog/",
    "metadata/year={YYYY}/month={MM}/day={DD}/render.json"
  )

COG_SUCCESS_TEMPLATE <- storage_cfg$cog_success_key %||%
  paste0(
    "CONUS_subset/production_areas/{area_id}/precip/daily/cog/",
    "signals/year={YYYY}/month={MM}/day={DD}/_SUCCESS"
  )

COG_CYCLE_SUCCESS_TEMPLATE <- storage_cfg$cog_cycle_success_key %||%
  paste0(
    "CONUS_subset/production_areas/{area_id}/precip/daily/cog/",
    "signals/cycle/cycle={cycle}/_SUCCESS"
  )

# Recreate S3 client using the resolved region.
s3 <- paws::s3(
  config = list(
    region = AWS_REGION
  )
)

# ==============================================================================
# DERIVED DATE + KEY VALUES
# ==============================================================================

YYYY <- format(MAP_DATE, "%Y")
MM <- format(MAP_DATE, "%m")
DD <- format(MAP_DATE, "%d")
DATE_ID <- format(MAP_DATE, "%Y%m%d")

template_values <- list(
  area_id = AREA_ID,
  YYYY = YYYY,
  MM = MM,
  DD = DD,
  DATE_ID = DATE_ID,
  cycle = CYCLE_ID,
  CYCLE_ID = CYCLE_ID,
  HH = CYCLE_HOUR
)

SUCCESS_KEY <- render_template(
  SUCCESS_KEY_TEMPLATE,
  template_values
)

PRECIP_KEY <- render_template(
  storage_cfg$daily_parquet_key,
  template_values
)

CELLS_KEY <- render_template(
  storage_cfg$cells_key,
  template_values
)

COG_KEY <- render_template(
  storage_cfg$cog_key,
  template_values
)

COG_METADATA_KEY <- render_template(
  COG_METADATA_TEMPLATE,
  template_values
)

COG_SUCCESS_KEY <- render_template(
  COG_SUCCESS_TEMPLATE,
  template_values
)

COG_CYCLE_SUCCESS_KEY <- render_template(
  COG_CYCLE_SUCCESS_TEMPLATE,
  template_values
)

# ==============================================================================
# LOCAL SCRATCH LAYOUT
# ==============================================================================

RUN_ROOT <- file.path(
  WORK_ROOT,
  AREA_ID,
  CYCLE_ID
)

SOURCE_DIR <- file.path(
  RUN_ROOT,
  "source"
)

COG_DIR <- file.path(
  RUN_ROOT,
  "cog"
)

LOCAL_PRECIP <- file.path(
  SOURCE_DIR,
  "daily.parquet"
)

LOCAL_HRAP <- file.path(
  SOURCE_DIR,
  "cells.gpkg"
)

LOCAL_COG <- file.path(
  COG_DIR,
  "stage4_daily.tif"
)

TEMP_GTIF <- file.path(
  COG_DIR,
  "stage4_daily_tmp.tif"
)

dir.create(
  SOURCE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  COG_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==============================================================================
# CONFIG LOG
# ==============================================================================

message("")
message("==============================================================")
message("01 PARQUET -> COG")
message("==============================================================")
message("Area:                     ", AREA_ID)
message("Cycle:                    ", CYCLE_ID)
message("Cycle hour:               ", CYCLE_HOUR, "Z")
message("Map date:                 ", MAP_DATE)
message("Date ID:                  ", DATE_ID)
message("Resolved config file:     ", LOCAL_CONFIG)
message("Work root:                ", WORK_ROOT)
message("AWS region:               ", AWS_REGION)
message("gdal_translate:           ", GDAL_TRANSLATE)
message("gdalinfo:                 ", GDALINFO)

message("")
message("Numerical success:")
message(
  "  s3://",
  DATA_BUCKET,
  "/",
  SUCCESS_KEY
)

message("")
message("Daily parquet:")
message(
  "  s3://",
  DATA_BUCKET,
  "/",
  PRECIP_KEY
)

message("")
message("AOI cells:")
message(
  "  s3://",
  DATA_BUCKET,
  "/",
  CELLS_KEY
)

message("")
message("COG output:")
message(
  "  s3://",
  RENDER_BUCKET,
  "/",
  COG_KEY
)

message("")
message("COG metadata:")
message(
  "  s3://",
  RENDER_BUCKET,
  "/",
  COG_METADATA_KEY
)

message("")
message("COG date success:")
message(
  "  s3://",
  RENDER_BUCKET,
  "/",
  COG_SUCCESS_KEY
)

message("")
message("COG cycle success:")
message(
  "  s3://",
  RENDER_BUCKET,
  "/",
  COG_CYCLE_SUCCESS_KEY
)

message("")
message(
  "Upsample factor:          ",
  SMOOTH_UPSAMPLE_FACT,
  "x"
)

message(
  "Gaussian radius:          ",
  SMOOTH_RADIUS_CELLS,
  " fine cells"
)

message(
  "Gaussian sigma:           ",
  if (is.na(SMOOTH_SIGMA_CELLS)) {
    paste0(
      "automatic (",
      SMOOTH_RADIUS_CELLS / 2,
      " fine cells)"
    )
  } else {
    SMOOTH_SIGMA_CELLS
  }
)

message(
  "Require upstream success: ",
  REQUIRE_NUMERICAL_SUCCESS
)

message(
  "Require full cell match:  ",
  REQUIRE_FULL_CELL_MATCH
)

message(
  "Upload COG:               ",
  UPLOAD_COG
)

# ==============================================================================
# 0. VERIFY AUTHORITATIVE NUMERICAL SUCCESS CUE
# ==============================================================================

message("")
message("==============================================================")
message("0. VERIFYING NUMERICAL SUCCESS CUE")
message("==============================================================")

if (REQUIRE_NUMERICAL_SUCCESS) {

  if (!s3_object_exists(
    s3 = s3,
    bucket = DATA_BUCKET,
    key = SUCCESS_KEY
  )) {
    stop(
      "Authoritative numerical success marker is absent:\n",
      "s3://",
      DATA_BUCKET,
      "/",
      SUCCESS_KEY
    )
  }

  message(
    "Numerical success marker found."
  )

} else {

  message(
    "Numerical success check disabled by config."
  )
}

# ==============================================================================
# 1. DOWNLOAD AUTHORITATIVE INPUTS
# ==============================================================================

message("")
message("==============================================================")
message("1. DOWNLOADING AUTHORITATIVE INPUTS")
message("==============================================================")

download_s3_object(
  s3 = s3,
  bucket = DATA_BUCKET,
  key = PRECIP_KEY,
  local_file = LOCAL_PRECIP,
  overwrite = OVERWRITE_INPUTS
)

download_s3_object(
  s3 = s3,
  bucket = DATA_BUCKET,
  key = CELLS_KEY,
  local_file = LOCAL_HRAP,
  overwrite = OVERWRITE_INPUTS
)

# ==============================================================================
# 2. READ + VALIDATE DAILY RAINFALL
# ==============================================================================

message("")
message("==============================================================")
message("2. READING DAILY RAINFALL")
message("==============================================================")

precip <- arrow::read_parquet(
  LOCAL_PRECIP
) |>
  as.data.frame()

message(
  "Parquet rows: ",
  format(
    nrow(precip),
    big.mark = ","
  )
)

message(
  "Columns:      ",
  paste(
    names(precip),
    collapse = ", "
  )
)

canonical_daily_columns <- c(
  "cycle",
  "lat",
  "lon",
  "hrap_x",
  "hrap_y",
  "grib_id",
  "bin_area",
  "rain_mm"
)

missing_daily_columns <- setdiff(
  canonical_daily_columns,
  names(precip)
)

if (length(missing_daily_columns) > 0) {
  stop(
    "Daily parquet does not satisfy the canonical schema. Missing column(s): ",
    paste(
      missing_daily_columns,
      collapse = ", "
    )
  )
}

# ------------------------------------------------------------------------------
# Optional but useful QA:
# make sure the parquet cycle agrees with the runtime cycle.
# ------------------------------------------------------------------------------

precip_cycles <- unique(
  as.character(
    precip$cycle[
      !is.na(precip$cycle)
    ]
  )
)

if (
  length(precip_cycles) > 0 &&
  !CYCLE_ID %in% precip_cycles
) {
  warning(
    "Runtime cycle ",
    CYCLE_ID,
    " was not found in parquet cycle column. ",
    "Observed cycle value(s): ",
    paste(
      head(precip_cycles, 10),
      collapse = ", "
    )
  )
}

precip <- precip |>
  transmute(
    grib_id = as.integer(grib_id),
    rain_mm = as.numeric(rain_mm)
  )

if (anyDuplicated(precip$grib_id) > 0) {
  stop(
    "Daily parquet contains duplicate grib_id values."
  )
}

if (nrow(precip) == 0) {
  stop(
    "Daily parquet contains zero rows."
  )
}

if (!any(is.finite(precip$rain_mm))) {
  stop(
    "Daily parquet contains no finite rain_mm values."
  )
}

message("")
message("Rainfall summary, mm:")

print(
  summary(
    precip$rain_mm
  )
)

message(
  "Maximum rainfall = ",
  round(
    max(
      precip$rain_mm,
      na.rm = TRUE
    ),
    2
  ),
  " mm = ",
  round(
    max(
      precip$rain_mm,
      na.rm = TRUE
    ) / 25.4,
    2
  ),
  " inches"
)

# ==============================================================================
# 3. READ + VALIDATE AOI HRAP GRID
# ==============================================================================

message("")
message("==============================================================")
message("3. READING AOI HRAP GRID")
message("==============================================================")

cells <- sf::read_sf(
  LOCAL_HRAP
)

message(
  "HRAP polygons:   ",
  format(
    nrow(cells),
    big.mark = ","
  )
)

message(
  "Geometry column: ",
  attr(
    cells,
    "sf_column"
  )
)

message(
  "Source CRS:      ",
  sf::st_crs(cells)$input
)

if (!"grib_id" %in% names(cells)) {
  stop(
    "Configured cells.gpkg does not contain grib_id."
  )
}

if (nrow(cells) == 0) {
  stop(
    "Configured cells.gpkg contains zero features."
  )
}

cells <- cells |>
  mutate(
    grib_id = as.integer(grib_id)
  )

if (anyDuplicated(cells$grib_id) > 0) {
  stop(
    "Configured cells.gpkg contains duplicate grib_id values."
  )
}

# ==============================================================================
# 4. JOIN RAINFALL TO HRAP
# ==============================================================================
#
# IMPORTANT FOOTPRINT RULE:
#   The configured AOI geometry defines the raster footprint.
#   Missing rain_mm values must NOT shrink the output extent.
#
#   We therefore preserve every AOI cell through the join, use ALL AOI cells
#   to determine projection / resolution / raster extent, and use only finite
#   rain_mm cells when rasterizing rainfall values.
# ==============================================================================

message("")
message("==============================================================")
message("4. JOINING RAINFALL TO HRAP")
message("==============================================================")

precip_join <- precip |>
  select(
    grib_id,
    rain_mm
  ) |>
  mutate(
    precip_match = TRUE
  )

rain_sf_all <- cells |>
  left_join(
    precip_join,
    by = "grib_id"
  )

matched_cell_count <- sum(
  rain_sf_all$precip_match %in% TRUE
)

missing_match_count <- nrow(cells) -
  matched_cell_count

rain_na_count <- sum(
  !is.finite(rain_sf_all$rain_mm)
)

rain_finite_count <- sum(
  is.finite(rain_sf_all$rain_mm)
)

message(
  "AOI HRAP cells:       ",
  format(
    nrow(cells),
    big.mark = ","
  )
)

message(
  "Matched parquet IDs:  ",
  format(
    matched_cell_count,
    big.mark = ","
  )
)

message(
  "Missing parquet IDs:  ",
  format(
    missing_match_count,
    big.mark = ","
  )
)

message(
  "Finite rain values:   ",
  format(
    rain_finite_count,
    big.mark = ","
  )
)

message(
  "NA/non-finite rain:   ",
  format(
    rain_na_count,
    big.mark = ","
  )
)

if (missing_match_count != 0) {

  msg <- paste0(
    "AOI cells are missing matching grib_id rows in the daily parquet. ",
    "Missing matched cells: ",
    format(
      missing_match_count,
      big.mark = ","
    ),
    "."
  )

  if (REQUIRE_FULL_CELL_MATCH) {
    stop(msg)
  } else {
    warning(msg)
  }
}

if (rain_finite_count == 0) {
  stop(
    "No finite rainfall cells remain after join."
  )
}

# ==============================================================================
# 5. PROJECT FULL AOI TO WEB MERCATOR
# ==============================================================================

message("")
message("==============================================================")
message("5. PROJECTING FULL AOI TO EPSG:3857")
message("==============================================================")

# Keep every configured AOI cell here.  This object is the authoritative
# footprint for the raster, even when some rain_mm values are NA.
rain_sf_all <- rain_sf_all |>
  select(
    -precip_match
  ) |>
  st_make_valid() |>
  st_transform(
    crs = 3857
  )

# Finite rainfall cells are a value/support subset of the fixed AOI footprint.
rain_sf <- rain_sf_all |>
  filter(
    is.finite(rain_mm)
  ) |>
  mutate(
    support = 1
  )

# ==============================================================================
# 6. ESTIMATE NATIVE HRAP RASTER RESOLUTION FROM FULL AOI
# ==============================================================================

message("")
message("==============================================================")
message("6. ESTIMATING NATIVE RASTER FROM FULL AOI")
message("==============================================================")

cell_area_m2 <- suppressWarnings(
  as.numeric(
    st_area(rain_sf_all)
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
  stop(
    "Could not estimate native HRAP resolution."
  )
}

message(
  "Estimated native HRAP resolution: ",
  format(
    round(
      native_res_m,
      1
    ),
    big.mark = ","
  ),
  " m"
)

# ==============================================================================
# 7. CREATE FIXED NATIVE RASTER TEMPLATE FROM FULL AOI
# ==============================================================================
#
# v_aoi defines the geographic footprint.
# v_rain contains only cells with finite rainfall and is used for values.
# This keeps extent/dimensions stable from cycle to cycle.
# ==============================================================================

v_aoi <- terra::vect(
  rain_sf_all
)

v_rain <- terra::vect(
  rain_sf
)

e <- terra::ext(
  v_aoi
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

native_cells <- terra::ncell(
  r_template
)

native_rows <- terra::nrow(
  r_template
)

native_cols <- terra::ncol(
  r_template
)

fine_rows <- native_rows *
  SMOOTH_UPSAMPLE_FACT

fine_cols <- native_cols *
  SMOOTH_UPSAMPLE_FACT

fine_cells <- fine_rows *
  fine_cols

fine_res_m <- native_res_m /
  SMOOTH_UPSAMPLE_FACT

message("")
message("--------------------------------------------------------------")
message("RASTER SIZE")
message("--------------------------------------------------------------")

message(
  "Native resolution:   ",
  round(
    native_res_m,
    1
  ),
  " m"
)

message(
  "Native dimensions:   ",
  format(
    native_cols,
    big.mark = ","
  ),
  " cols x ",
  format(
    native_rows,
    big.mark = ","
  ),
  " rows"
)

message(
  "Native cells:        ",
  format(
    native_cells,
    big.mark = ","
  )
)

message("")

message(
  "Requested upsample:  ",
  SMOOTH_UPSAMPLE_FACT,
  "x"
)

message(
  "Fine resolution:     ",
  round(
    fine_res_m,
    1
  ),
  " m"
)

message(
  "Fine dimensions:     ",
  format(
    fine_cols,
    big.mark = ","
  ),
  " cols x ",
  format(
    fine_rows,
    big.mark = ","
  ),
  " rows"
)

message(
  "Fine cells:          ",
  format(
    fine_cells,
    big.mark = ","
  )
)

message("--------------------------------------------------------------")

message("")
message(
  "Fixed AOI footprint source: all ",
  format(nrow(rain_sf_all), big.mark = ","),
  " configured cells"
)
message(
  "Rainfall value/support source: ",
  format(nrow(rain_sf), big.mark = ","),
  " finite cells"
)

# ==============================================================================
# 8. RASTERIZE ORIGINAL STAGE IV
# ==============================================================================
#
# Do not use touches=TRUE with aggregation functions here.  terra warns that
# touches and aggregate rasterization cannot be combined.  The native template
# is derived from the full AOI and the normal polygon rasterization is used.
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
  fun = "mean"
)

r_support <- terra::rasterize(
  v_rain,
  r_template,
  field = "support",
  background = NA,
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
# 9. BILINEAR UPSAMPLE
# ==============================================================================

message("")
message("==============================================================")
message(
  "9. ",
  SMOOTH_UPSAMPLE_FACT,
  "x BILINEAR UPSAMPLE"
)
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

# ==============================================================================
# 10. GAUSSIAN KERNEL
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

r_smooth <- r_num /
  r_den

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
# 12. VALUE QA
# ==============================================================================

message("")
message("==============================================================")
message("12. VALUE QA")
message("==============================================================")

message("")
message("Original native raster:")

native_stats <- terra::global(
  r_native,
  c(
    "min",
    "mean",
    "max"
  ),
  na.rm = TRUE
)

print(
  native_stats
)

message("")
message("Final smooth raster:")

smooth_stats <- terra::global(
  r_smooth,
  c(
    "min",
    "mean",
    "max"
  ),
  na.rm = TRUE
)

print(
  smooth_stats
)

if (
  !all(
    is.finite(
      unlist(
        smooth_stats
      )
    )
  )
) {
  stop(
    "Final smooth raster QA returned non-finite statistics."
  )
}

if (
  smooth_stats[1, "min"] < -1e-6
) {
  stop(
    "Final smooth raster contains negative rainfall values."
  )
}

# ==============================================================================
# 13. WRITE TEMPORARY GEOTIFF
# ==============================================================================
#
# terra writes a plain GeoTIFF here.  We intentionally do NOT ask terra to
# create the final COG because that previously produced misleading MEM-driver
# creation-option warnings.  GDAL creates the canonical COG in the next step.
# ==============================================================================

message("")
message("==============================================================")
message("13. WRITING TEMPORARY GEOTIFF")
message("==============================================================")

if (file.exists(TEMP_GTIF)) {
  unlink(TEMP_GTIF)
}

if (
  file.exists(LOCAL_COG) &&
  !OVERWRITE_COG
) {
  stop(
    "Local output already exists and overwrite_cog = FALSE:\n",
    LOCAL_COG
  )
}

if (
  file.exists(LOCAL_COG) &&
  OVERWRITE_COG
) {
  unlink(LOCAL_COG)
}

t0 <- Sys.time()

terra::writeRaster(
  r_smooth,
  TEMP_GTIF,
  overwrite = TRUE,
  filetype = "GTiff",
  datatype = "FLT4S",
  NAflag = -9999,
  gdal = c(
    "TILED=YES",
    "BIGTIFF=IF_SAFER"
  )
)

if (!file.exists(TEMP_GTIF)) {
  stop(
    "Temporary GeoTIFF was not created:\n",
    TEMP_GTIF
  )
}

message(
  "Temporary GeoTIFF write seconds: ",
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
# 14. CREATE + VALIDATE CANONICAL COG WITH GDAL
# ==============================================================================

message("")
message("==============================================================")
message("14. CREATING CANONICAL COG WITH GDAL")
message("==============================================================")

message("gdal_translate: ", GDAL_TRANSLATE)
message("gdalinfo:       ", GDALINFO)

translate_args <- c(
  "-of", "COG",
  "-co", "COMPRESS=DEFLATE",
  "-co", "LEVEL=6",
  "-co", "BLOCKSIZE=512",
  "-co", "OVERVIEWS=AUTO",
  "-co", "RESAMPLING=AVERAGE",
  "-co", "BIGTIFF=IF_SAFER",
  "-co", "NUM_THREADS=ALL_CPUS",
  TEMP_GTIF,
  LOCAL_COG
)

t0 <- Sys.time()

translate_status <- system2(
  GDAL_TRANSLATE,
  args = translate_args
)

if (!identical(translate_status, 0L)) {
  stop(
    "gdal_translate failed with exit status ",
    translate_status
  )
}

if (!file.exists(LOCAL_COG)) {
  stop(
    "Canonical COG was not created:\n",
    LOCAL_COG
  )
}

message(
  "COG creation seconds: ",
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

# ------------------------------------------------------------------------------
# GDAL structural QA
# ------------------------------------------------------------------------------

gdalinfo_text <- system2(
  GDALINFO,
  args = c(
    "-json",
    LOCAL_COG
  ),
  stdout = TRUE,
  stderr = TRUE
)

gdalinfo_status <- attr(
  gdalinfo_text,
  "status"
)

if (
  !is.null(gdalinfo_status) &&
  !identical(gdalinfo_status, 0L)
) {
  stop(
    "gdalinfo failed with exit status ",
    gdalinfo_status
  )
}

gdalinfo_json <- jsonlite::fromJSON(
  paste(
    gdalinfo_text,
    collapse = "\n"
  ),
  simplifyVector = FALSE
)

image_structure <- gdalinfo_json$metadata$IMAGE_STRUCTURE %||% list()

cog_layout <- image_structure$LAYOUT %||% ""
cog_compression <- image_structure$COMPRESSION %||% ""

if (!identical(cog_layout, "COG")) {
  stop(
    "GDAL QA failed: expected IMAGE_STRUCTURE LAYOUT=COG, got: ",
    if (nzchar(cog_layout)) cog_layout else "<missing>"
  )
}

if (!identical(cog_compression, "DEFLATE")) {
  stop(
    "GDAL QA failed: expected DEFLATE compression, got: ",
    if (nzchar(cog_compression)) cog_compression else "<missing>"
  )
}

overview_count <- 0L

if (
  length(gdalinfo_json$bands) >= 1 &&
  !is.null(gdalinfo_json$bands[[1]]$overviews)
) {
  overview_count <- length(
    gdalinfo_json$bands[[1]]$overviews
  )
}

if (overview_count < 1L) {
  stop(
    "GDAL QA failed: canonical COG has no internal overviews."
  )
}

message("")
message("GDAL COG QA:")
message("  Layout:       ", cog_layout)
message("  Compression:  ", cog_compression)
message("  Overviews:    ", overview_count)

# ------------------------------------------------------------------------------
# Read final COG back with terra for value / raster QA
# ------------------------------------------------------------------------------

message("")
message("Reading final COG back with terra:")

cog_check <- terra::rast(
  LOCAL_COG
)

print(
  cog_check
)

if (
  terra::nlyr(cog_check) != 1
) {
  stop(
    "Expected one-band COG; found ",
    terra::nlyr(cog_check),
    " bands."
  )
}

cog_size_mb <- file.info(
  LOCAL_COG
)$size / 1024^2

message("")
message(
  "COG file size: ",
  round(
    cog_size_mb,
    1
  ),
  " MB"
)

message("")
message("Final COG value summary:")

cog_stats <- terra::global(
  cog_check,
  c(
    "min",
    "mean",
    "max"
  ),
  na.rm = TRUE
)

print(
  cog_stats
)

if (
  !all(
    is.finite(
      unlist(
        cog_stats
      )
    )
  )
) {
  stop(
    "Final COG QA returned non-finite statistics."
  )
}

if (
  cog_stats[1, "min"] < -1e-6
) {
  stop(
    "Final COG contains negative rainfall values."
  )
}

# ------------------------------------------------------------------------------
# Clean temporary GeoTIFF only after final COG passes QA
# ------------------------------------------------------------------------------

if (file.exists(TEMP_GTIF)) {
  unlink(TEMP_GTIF)
}

# ==============================================================================
# 15. UPLOAD CANONICAL COG + COG-SCOPED COMPLETION ARTIFACTS
# ==============================================================================

if (UPLOAD_COG) {

  message("")
  message("==============================================================")
  message("15A. UPLOADING CANONICAL COG")
  message("==============================================================")

  COG_SIZE_BYTES <- as.numeric(
    file.info(LOCAL_COG)$size
  )

  if (
    !is.finite(COG_SIZE_BYTES) ||
    COG_SIZE_BYTES <= 0
  ) {
    stop(
      "Local COG has invalid file size."
    )
  }

  s3$put_object(
    Bucket = RENDER_BUCKET,
    Key = COG_KEY,
    Body = LOCAL_COG,
    ContentType = "image/tiff",
    CacheControl = COG_CACHE_CONTROL
  )

  verify_object_size(
    s3 = s3,
    bucket = RENDER_BUCKET,
    key = COG_KEY,
    expected_bytes = COG_SIZE_BYTES
  )

  message("")
  message("Uploaded and verified:")
  message(
    "s3://",
    RENDER_BUCKET,
    "/",
    COG_KEY
  )
  message(
    "COG bytes: ",
    format(
      COG_SIZE_BYTES,
      big.mark = ","
    )
  )

  # ---------------------------------------------------------------------------
  # 15B. WRITE COG METADATA
  # ---------------------------------------------------------------------------

  message("")
  message("==============================================================")
  message("15B. WRITING COG METADATA")
  message("==============================================================")

  cog_extent <- list(
    xmin = as.numeric(
      terra::xmin(cog_check)
    ),
    xmax = as.numeric(
      terra::xmax(cog_check)
    ),
    ymin = as.numeric(
      terra::ymin(cog_check)
    ),
    ymax = as.numeric(
      terra::ymax(cog_check)
    )
  )

  cog_resolution <- terra::res(
    cog_check
  )

  cog_metadata <- list(
    schema_version = 1L,
    artifact = "cog",
    area_id = AREA_ID,
    cycle = CYCLE_ID,
    map_date = DATE_ID,
    product = "stage4_daily",
    created_utc = format(
      Sys.time(),
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%SZ"
    ),

    upstream = list(
      numerical_success = paste0(
        "s3://",
        DATA_BUCKET,
        "/",
        SUCCESS_KEY
      ),
      daily_parquet = paste0(
        "s3://",
        DATA_BUCKET,
        "/",
        PRECIP_KEY
      ),
      cells = paste0(
        "s3://",
        DATA_BUCKET,
        "/",
        CELLS_KEY
      )
    ),

    output = list(
      cog = paste0(
        "s3://",
        RENDER_BUCKET,
        "/",
        COG_KEY
      ),
      content_type = "image/tiff",
      bytes = COG_SIZE_BYTES,
      epsg = 3857L,
      rows = terra::nrow(
        cog_check
      ),
      cols = terra::ncol(
        cog_check
      ),
      resolution_x = as.numeric(
        cog_resolution[[1]]
      ),
      resolution_y = as.numeric(
        cog_resolution[[2]]
      ),
      extent = cog_extent,
      compression = cog_compression,
      overview_count = overview_count
    ),

    smoothing = list(
      upsample_factor = SMOOTH_UPSAMPLE_FACT,
      radius_cells = SMOOTH_RADIUS_CELLS,
      sigma_cells = if (
        is.na(SMOOTH_SIGMA_CELLS)
      ) {
        NULL
      } else {
        SMOOTH_SIGMA_CELLS
      }
    ),

    qa = list(
      aoi_cell_count = nrow(
        rain_sf_all
      ),
      matched_parquet_ids = matched_cell_count,
      finite_rain_values = rain_finite_count,
      nonfinite_rain_values = rain_na_count,
      min_rain_mm = as.numeric(
        cog_stats[1, "min"]
      ),
      mean_rain_mm = as.numeric(
        cog_stats[1, "mean"]
      ),
      max_rain_mm = as.numeric(
        cog_stats[1, "max"]
      )
    )
  )

  cog_metadata_text <- jsonlite::toJSON(
    cog_metadata,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  cog_metadata_raw <- charToRaw(
    paste0(
      cog_metadata_text,
      "\n"
    )
  )

  put_raw(
    s3 = s3,
    bucket = RENDER_BUCKET,
    key = COG_METADATA_KEY,
    body = cog_metadata_raw,
    content_type = "application/json",
    cache_control = "no-cache"
  )

  verify_object_size(
    s3 = s3,
    bucket = RENDER_BUCKET,
    key = COG_METADATA_KEY,
    expected_bytes = length(
      cog_metadata_raw
    )
  )

  message("Uploaded and verified COG render.json:")
  message(
    "s3://",
    RENDER_BUCKET,
    "/",
    COG_METADATA_KEY
  )

  # ---------------------------------------------------------------------------
  # 15C. WRITE COG _SUCCESS MARKERS LAST
  # ---------------------------------------------------------------------------

  message("")
  message("==============================================================")
  message("15C. WRITING COG _SUCCESS MARKERS")
  message("==============================================================")

  # Date marker first.
  put_raw(
    s3 = s3,
    bucket = RENDER_BUCKET,
    key = COG_SUCCESS_KEY,
    body = raw(0),
    content_type = "application/octet-stream",
    cache_control = "no-cache"
  )

  verify_object_exists(
    s3 = s3,
    bucket = RENDER_BUCKET,
    key = COG_SUCCESS_KEY
  )

  message("Wrote COG date _SUCCESS.")

  # Cycle marker is intentionally the final durable write from Stage 01.
  # Downstream Container B can gate on this marker.
  put_raw(
    s3 = s3,
    bucket = RENDER_BUCKET,
    key = COG_CYCLE_SUCCESS_KEY,
    body = raw(0),
    content_type = "application/octet-stream",
    cache_control = "no-cache"
  )

  verify_object_exists(
    s3 = s3,
    bucket = RENDER_BUCKET,
    key = COG_CYCLE_SUCCESS_KEY
  )

  message("Wrote COG cycle _SUCCESS LAST.")

} else {

  message("")
  message(
    "upload_cog = FALSE; skipped COG upload, metadata, and COG success markers."
  )
}

# ==============================================================================
# DONE
# ==============================================================================

message("")
message("==============================================================")
message("DONE")
message("==============================================================")
message("Area:          ", AREA_ID)
message("Cycle:         ", CYCLE_ID)
message("Map date:      ", MAP_DATE)
message("Local config:  ", LOCAL_CONFIG)
message("Local parquet: ", LOCAL_PRECIP)
message("Local HRAP:    ", LOCAL_HRAP)
message("Local COG:     ", LOCAL_COG)

message(
  "Canonical COG: s3://",
  RENDER_BUCKET,
  "/",
  COG_KEY
)

message("")

message("")
message("COG metadata: s3://", RENDER_BUCKET, "/", COG_METADATA_KEY)
message("COG date success: s3://", RENDER_BUCKET, "/", COG_SUCCESS_KEY)
message("COG cycle success: s3://", RENDER_BUCKET, "/", COG_CYCLE_SUCCESS_KEY)
message("")
message(
  "Stage 01 complete. COG-scoped metadata and success markers were written; ",
  "PMTiles/public manifests were not."
)
