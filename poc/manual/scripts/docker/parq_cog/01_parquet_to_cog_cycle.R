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
#
# This stage DOES NOT write:
#   render _SUCCESS
#   render.json
#   latest.json
#   radar_frames.json
#
# Those are written only after the complete render chain succeeds.
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

message("")
message("==============================================================")
message("4. JOINING RAINFALL TO HRAP")
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

message(
  "Joined HRAP cells:   ",
  format(
    nrow(rain_sf),
    big.mark = ","
  )
)

message(
  "Expected HRAP cells: ",
  format(
    nrow(cells),
    big.mark = ","
  )
)

missing_cell_count <- nrow(cells) -
  nrow(rain_sf)

if (missing_cell_count != 0) {

  msg <- paste0(
    "Joined row count does not equal AOI HRAP row count. ",
    "Missing matched cells: ",
    format(
      missing_cell_count,
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

rain_sf <- rain_sf |>
  filter(
    is.finite(rain_mm)
  ) |>
  mutate(
    support = 1
  )

if (nrow(rain_sf) == 0) {
  stop(
    "No finite rainfall cells remain after join."
  )
}

# ==============================================================================
# 5. PROJECT TO WEB MERCATOR
# ==============================================================================

message("")
message("==============================================================")
message("5. PROJECTING TO EPSG:3857")
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
message("6. ESTIMATING NATIVE RASTER")
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
# 13. WRITE CANONICAL LOCAL COG
# ==============================================================================

message("")
message("==============================================================")
message("13. WRITING CANONICAL LOCAL COG")
message("==============================================================")

if (
  file.exists(LOCAL_COG) &&
  !OVERWRITE_COG
) {
  stop(
    "Local output already exists and overwrite_cog = FALSE:\n",
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
# 14. READ FINAL COG BACK FOR QA
# ==============================================================================

message("")
message("==============================================================")
message("14. READING FINAL COG BACK FOR QA")
message("==============================================================")

if (!file.exists(LOCAL_COG)) {
  stop(
    "COG was not created:\n",
    LOCAL_COG
  )
}

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

# ==============================================================================
# 15. UPLOAD CANONICAL COG
# ==============================================================================

if (UPLOAD_COG) {

  message("")
  message("==============================================================")
  message("15. UPLOADING CANONICAL COG")
  message("==============================================================")

  s3$put_object(
    Bucket = RENDER_BUCKET,
    Key = COG_KEY,
    Body = LOCAL_COG,
    ContentType = "image/tiff",
    CacheControl = COG_CACHE_CONTROL
  )

  if (!s3_object_exists(
    s3 = s3,
    bucket = RENDER_BUCKET,
    key = COG_KEY
  )) {
    stop(
      "COG upload completed without a readable S3 object:\n",
      "s3://",
      RENDER_BUCKET,
      "/",
      COG_KEY
    )
  }

  message("")
  message("Uploaded:")

  message(
    "s3://",
    RENDER_BUCKET,
    "/",
    COG_KEY
  )

} else {

  message("")
  message(
    "upload_cog = FALSE; skipped S3 upload."
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

message(
  "Stage 01 complete. No render _SUCCESS or manifest was written."
)
