# ==============================================================================
# 02_cog_to_rgba_cycle.R
#
# Purpose:
#   Convert one canonical single-band Stage IV rainfall COG (rain_mm) into a
#   display-ready 4-band RGBA GeoTIFF.
#
# Production-shaped runtime contract:
#   Rscript 02_cog_to_rgba_cycle.R \
#     --area texas \
#     --cycle 2026100312
#
# Inputs:
#   - AREA_ID / --area
#   - CYCLE_ID / --cycle
#   - render_config.json downloaded from S3
#   - canonical Stage 01 COG downloaded from the render bucket
#
# Output for this development step:
#   /work/<area>/<cycle>/rgba/stage4_daily_rgba.tif
#
# This RGBA TIFF is TRANSIENT.  This script does NOT upload it to S3.
#
# Local Docker testing:
#   mount a host folder to /work so the COG and RGBA can be inspected directly.
# ==============================================================================

suppressPackageStartupMessages({
  library(terra)
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

  if (
    !nzchar(cli$config_bucket) ||
    !nzchar(cli$config_key)
  ) {
    stop(
      "No local --config provided, and config bucket/key are missing."
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

hex_to_rgb <- function(hex) {

  if (
    length(hex) != 1 ||
    !grepl("^#[0-9A-Fa-f]{6}$", hex)
  ) {
    stop("Invalid hex color: ", hex)
  }

  clean <- sub("^#", "", hex)

  c(
    strtoi(substr(clean, 1, 2), base = 16L),
    strtoi(substr(clean, 3, 4), base = 16L),
    strtoi(substr(clean, 5, 6), base = 16L)
  )
}

clamp01 <- function(x) {
  pmax(0, pmin(1, x))
}

# Match JavaScript Math.round() behavior for non-negative display values.
js_round <- function(x) {
  as.integer(floor(x + 0.5))
}

# ==============================================================================
# RUNTIME INPUTS
# ==============================================================================

cli <- parse_cli(
  commandArgs(trailingOnly = TRUE)
)

if (!nzchar(cli$area)) {
  stop(
    "Missing --area.\n",
    "Example: --area texas --cycle 2026100312"
  )
}

if (!nzchar(cli$cycle)) {
  stop(
    "Missing --cycle.\n",
    "Example: --area texas --cycle 2026100312"
  )
}

if (!grepl("^[0-9]{10}$", cli$cycle)) {
  stop(
    "Invalid --cycle value: ",
    cli$cycle,
    "\nExpected YYYYMMDDHH."
  )
}

AREA_ID <- cli$area
CYCLE_ID <- cli$cycle
CYCLE_HOUR <- substr(CYCLE_ID, 9, 10)

MAP_DATE <- as.Date(
  substr(CYCLE_ID, 1, 8),
  format = "%Y%m%d"
)

if (is.na(MAP_DATE)) {
  stop(
    "Could not derive date from cycle: ",
    CYCLE_ID
  )
}

YYYY <- format(MAP_DATE, "%Y")
MM <- format(MAP_DATE, "%m")
DD <- format(MAP_DATE, "%d")
DATE_ID <- format(MAP_DATE, "%Y%m%d")

WORK_ROOT <- cli$work_dir
BOOTSTRAP_REGION <- cli$aws_region %||% "us-east-2"

dir.create(
  WORK_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==============================================================================
# GDAL
# ==============================================================================

GDAL_TRANSLATE <- Sys.which("gdal_translate")

if (!nzchar(GDAL_TRANSLATE)) {
  stop("gdal_translate was not found on PATH.")
}

# ==============================================================================
# BOOTSTRAP CONFIG FROM S3
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

display_cfg <- merge_lists(
  config$defaults$display,
  area_cfg$display
)

run_cfg <- merge_lists(
  config$defaults$run,
  area_cfg$run
)

AWS_REGION <- aws_cfg$region %||% BOOTSTRAP_REGION

if (
  is.null(storage_cfg$render_bucket) ||
  !nzchar(storage_cfg$render_bucket)
) {
  stop("storage.render_bucket is missing from render config.")
}

if (
  is.null(storage_cfg$cog_key) ||
  !nzchar(storage_cfg$cog_key)
) {
  stop("storage.cog_key is missing from render config.")
}

RENDER_BUCKET <- storage_cfg$render_bucket

OVERWRITE_INPUTS <- isTRUE(
  run_cfg$overwrite_inputs
)

# RGBA is transient.  Default to overwrite for development / task-local work.
OVERWRITE_RGBA <- if (is.null(run_cfg$overwrite_rgba)) {
  TRUE
} else {
  isTRUE(run_cfg$overwrite_rgba)
}

DELETE_TEMP_RGBA <- if (is.null(run_cfg$delete_temp_rgba)) {
  TRUE
} else {
  isTRUE(run_cfg$delete_temp_rgba)
}

# ==============================================================================
# DISPLAY CONFIG
# ==============================================================================

if (is.null(display_cfg)) {
  stop(
    "defaults.display is missing from render_config.json."
  )
}

DISPLAY_UNITS <- display_cfg$units %||% "inches"

if (!identical(DISPLAY_UNITS, "inches")) {
  stop(
    "This RGBA renderer currently expects display.units = 'inches'. ",
    "Found: ",
    DISPLAY_UNITS
  )
}

TRACE_THRESHOLD_IN <- as.numeric(
  display_cfg$trace_threshold_in %||% 0.001
)

TRACE_ALPHA_MIN <- as.integer(
  display_cfg$trace_alpha_min %||% 115
)

TRACE_ALPHA_MAX <- as.integer(
  display_cfg$trace_alpha_max %||% 255
)

EXTREME_MIN_IN <- as.numeric(
  display_cfg$extreme_min_in %||% 12.0
)

EXTREME_COLOR <- display_cfg$extreme_color %||% "#5a003f"

RAIN_SEGMENTS_RAW <- display_cfg$segments

if (
  is.null(RAIN_SEGMENTS_RAW) ||
  length(RAIN_SEGMENTS_RAW) == 0
) {
  stop(
    "display.segments is missing or empty in render_config.json."
  )
}

RAIN_SEGMENTS <- lapply(
  RAIN_SEGMENTS_RAW,
  function(x) {

    required <- c(
      "min_in",
      "max_in",
      "start",
      "end"
    )

    missing_fields <- required[
      vapply(
        required,
        function(nm) is.null(x[[nm]]),
        logical(1)
      )
    ]

    if (length(missing_fields) > 0) {
      stop(
        "A display segment is missing: ",
        paste(missing_fields, collapse = ", ")
      )
    }

    out <- list(
      min = as.numeric(x$min_in),
      max = as.numeric(x$max_in),
      start = x$start,
      end = x$end
    )

    if (
      !is.finite(out$min) ||
      !is.finite(out$max) ||
      out$max <= out$min
    ) {
      stop(
        "Invalid display segment range: ",
        out$min,
        " -> ",
        out$max
      )
    }

    out$rgb0 <- hex_to_rgb(out$start)
    out$rgb1 <- hex_to_rgb(out$end)

    out
  }
)

# Sort so trace/alpha behavior is deterministic.
RAIN_SEGMENTS <- RAIN_SEGMENTS[
  order(
    vapply(
      RAIN_SEGMENTS,
      function(x) x$min,
      numeric(1)
    )
  )
]

EXTREME_RGB <- hex_to_rgb(
  EXTREME_COLOR
)

FIRST_SEGMENT <- RAIN_SEGMENTS[[1]]

if (
  abs(FIRST_SEGMENT$min - TRACE_THRESHOLD_IN) > 1e-12
) {
  stop(
    "trace_threshold_in must equal the minimum of the first display segment. ",
    "trace_threshold_in = ",
    TRACE_THRESHOLD_IN,
    "; first segment min = ",
    FIRST_SEGMENT$min
  )
}

if (
  EXTREME_MIN_IN < max(
    vapply(
      RAIN_SEGMENTS,
      function(x) x$max,
      numeric(1)
    )
  )
) {
  stop(
    "extreme_min_in overlaps a configured display segment."
  )
}

# ==============================================================================
# DERIVED S3 + LOCAL PATHS
# ==============================================================================

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

COG_KEY <- render_template(
  storage_cfg$cog_key,
  template_values
)

RUN_ROOT <- file.path(
  WORK_ROOT,
  AREA_ID,
  CYCLE_ID
)

COG_DIR <- file.path(
  RUN_ROOT,
  "cog"
)

RGBA_DIR <- file.path(
  RUN_ROOT,
  "rgba"
)

LOCAL_COG <- file.path(
  COG_DIR,
  "stage4_daily.tif"
)

TEMP_RGBA <- file.path(
  RGBA_DIR,
  "stage4_daily_rgba_tmp.tif"
)

OUTPUT_RGBA <- file.path(
  RGBA_DIR,
  "stage4_daily_rgba.tif"
)

dir.create(
  COG_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  RGBA_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==============================================================================
# AWS CLIENT
# ==============================================================================

s3 <- paws::s3(
  config = list(
    region = AWS_REGION
  )
)

# ==============================================================================
# CONFIG LOG
# ==============================================================================

message("")
message("==============================================================")
message("02 COG -> RGBA")
message("==============================================================")
message("Area:                  ", AREA_ID)
message("Cycle:                 ", CYCLE_ID)
message("Map date:              ", MAP_DATE)
message("Config:                ", LOCAL_CONFIG)
message("AWS region:            ", AWS_REGION)
message("gdal_translate:        ", GDAL_TRANSLATE)
message("")
message("Canonical COG:")
message("  s3://", RENDER_BUCKET, "/", COG_KEY)
message("")
message("Local COG:")
message("  ", LOCAL_COG)
message("")
message("Local RGBA:")
message("  ", OUTPUT_RGBA)
message("")
message("Display units:         ", DISPLAY_UNITS)
message("Trace threshold:       ", TRACE_THRESHOLD_IN, " in")
message(
  "Trace alpha:           ",
  TRACE_ALPHA_MIN,
  " -> ",
  TRACE_ALPHA_MAX
)
message("Extreme threshold:     ", EXTREME_MIN_IN, " in")
message("Extreme color:         ", EXTREME_COLOR)
message("Display segments:      ", length(RAIN_SEGMENTS))

for (i in seq_along(RAIN_SEGMENTS)) {
  segment <- RAIN_SEGMENTS[[i]]

  message(
    "  ",
    segment$min,
    " to ",
    segment$max,
    " in: ",
    segment$start,
    " -> ",
    segment$end
  )
}

# ==============================================================================
# 1. DOWNLOAD + READ CANONICAL COG
# ==============================================================================

message("")
message("==============================================================")
message("1. DOWNLOADING CANONICAL COG")
message("==============================================================")

download_s3_object(
  s3 = s3,
  bucket = RENDER_BUCKET,
  key = COG_KEY,
  local_file = LOCAL_COG,
  overwrite = OVERWRITE_INPUTS
)

message("")
message("Reading canonical COG:")

rain <- terra::rast(
  LOCAL_COG
)

print(
  rain
)

if (terra::nlyr(rain) != 1) {
  stop(
    "Expected a single-band rainfall COG; found ",
    terra::nlyr(rain),
    " bands."
  )
}

input_rows <- terra::nrow(rain)
input_cols <- terra::ncol(rain)
input_crs <- terra::crs(rain, proj = TRUE)
input_ext <- terra::ext(rain)

message("")
message("Input rainfall summary, mm:")

input_stats <- terra::global(
  rain,
  c(
    "min",
    "mean",
    "max"
  ),
  na.rm = TRUE
)

print(
  input_stats
)

if (
  !all(
    is.finite(
      unlist(input_stats)
    )
  )
) {
  stop(
    "Input COG has invalid rainfall summary statistics."
  )
}

if (
  input_stats[1, "min"] < -1e-6
) {
  stop(
    "Input COG contains negative rainfall values."
  )
}

# ==============================================================================
# PIXEL COLOR FUNCTION
# ==============================================================================

rain_mm_to_rgba <- function(v) {

  rain_mm <- as.numeric(v)
  rain_in <- rain_mm / 25.4

  n <- length(rain_in)

  # Default is transparent black.
  # This covers:
  #   - source NoData / NA
  #   - rainfall below trace_threshold_in
  out <- matrix(
    0L,
    nrow = n,
    ncol = 4
  )

  valid <- is.finite(rain_in) &
    rain_in >= TRACE_THRESHOLD_IN

  for (segment in RAIN_SEGMENTS) {

    idx <- which(
      valid &
        rain_in >= segment$min &
        rain_in < segment$max
    )

    if (length(idx) == 0) {
      next
    }

    t <- clamp01(
      (rain_in[idx] - segment$min) /
        (segment$max - segment$min)
    )

    for (channel in 1:3) {

      out[idx, channel] <- js_round(
        segment$rgb0[channel] +
          (
            segment$rgb1[channel] -
              segment$rgb0[channel]
          ) * t
      )
    }

    alpha <- rep(
      255L,
      length(idx)
    )

    # Trace alpha ramp applies only to the first configured segment.
    if (identical(segment, FIRST_SEGMENT)) {

      trace_t <- clamp01(
        (rain_in[idx] - FIRST_SEGMENT$min) /
          (FIRST_SEGMENT$max - FIRST_SEGMENT$min)
      )

      alpha <- js_round(
        TRACE_ALPHA_MIN +
          (
            TRACE_ALPHA_MAX -
              TRACE_ALPHA_MIN
          ) * trace_t
      )
    }

    out[idx, 4] <- alpha
  }

  idx_extreme <- which(
    valid &
      rain_in >= EXTREME_MIN_IN
  )

  if (length(idx_extreme) > 0) {

    out[idx_extreme, 1] <- EXTREME_RGB[1]
    out[idx_extreme, 2] <- EXTREME_RGB[2]
    out[idx_extreme, 3] <- EXTREME_RGB[3]
    out[idx_extreme, 4] <- 255L
  }

  out
}

# ==============================================================================
# 2. APPLY DISPLAY COLOR FUNCTION
# ==============================================================================

message("")
message("==============================================================")
message("2. COLORIZING COG -> RGBA")
message("==============================================================")

if (
  file.exists(OUTPUT_RGBA) &&
  !OVERWRITE_RGBA
) {
  stop(
    "RGBA output already exists and overwrite_rgba = FALSE:\n",
    OUTPUT_RGBA
  )
}

if (file.exists(TEMP_RGBA)) {
  unlink(TEMP_RGBA)
}

if (
  file.exists(OUTPUT_RGBA) &&
  OVERWRITE_RGBA
) {
  unlink(OUTPUT_RGBA)
}

t0 <- Sys.time()

rgba_tmp <- terra::app(
  rain,
  fun = rain_mm_to_rgba,
  filename = TEMP_RGBA,
  overwrite = TRUE,
  wopt = list(
    datatype = "INT1U",
    gdal = c(
      "TILED=YES",
      "COMPRESS=DEFLATE",
      "PREDICTOR=2",
      "BIGTIFF=IF_SAFER"
    )
  )
)

names(rgba_tmp) <- c(
  "red",
  "green",
  "blue",
  "alpha"
)

message(
  "Colorization seconds: ",
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
# 3. FINALIZE RGB + ALPHA TIFF
# ==============================================================================

message("")
message("==============================================================")
message("3. FINALIZING RGB + ALPHA TIFF")
message("==============================================================")

translate_args <- c(
  "-of", "GTiff",
  "-a_nodata", "none",
  "-colorinterp", "red,green,blue,alpha",
  "-co", "TILED=YES",
  "-co", "COMPRESS=DEFLATE",
  "-co", "PREDICTOR=2",
  "-co", "BIGTIFF=IF_SAFER",
  TEMP_RGBA,
  OUTPUT_RGBA
)

status <- system2(
  GDAL_TRANSLATE,
  args = translate_args
)

if (!identical(status, 0L)) {
  stop(
    "gdal_translate failed with exit status ",
    status
  )
}

if (!file.exists(OUTPUT_RGBA)) {
  stop(
    "Final RGBA TIFF was not created:\n",
    OUTPUT_RGBA
  )
}

# ==============================================================================
# 4. QA FINAL RGBA
# ==============================================================================

message("")
message("==============================================================")
message("4. RGBA QA")
message("==============================================================")

rgba_check <- terra::rast(
  OUTPUT_RGBA
)

if (terra::nlyr(rgba_check) != 4) {
  stop(
    "Expected four RGBA bands; found ",
    terra::nlyr(rgba_check),
    "."
  )
}

names(rgba_check) <- c(
  "red",
  "green",
  "blue",
  "alpha"
)

print(
  rgba_check
)

# Structural invariants: the RGBA must exactly preserve the COG grid.
if (
  terra::nrow(rgba_check) != input_rows ||
  terra::ncol(rgba_check) != input_cols
) {
  stop(
    "RGBA dimensions do not match the input COG."
  )
}

if (!terra::same.crs(rgba_check, rain)) {
  stop(
    "RGBA CRS does not match the input COG."
  )
}

rgba_ext <- terra::ext(rgba_check)

extent_delta <- max(
  abs(
    c(
      terra::xmin(rgba_ext) - terra::xmin(input_ext),
      terra::xmax(rgba_ext) - terra::xmax(input_ext),
      terra::ymin(rgba_ext) - terra::ymin(input_ext),
      terra::ymax(rgba_ext) - terra::ymax(input_ext)
    )
  )
)

if (
  !is.finite(extent_delta) ||
  extent_delta > 1e-6
) {
  stop(
    "RGBA extent does not match the input COG."
  )
}

message("")
message("RGBA band summaries:")

rgba_stats <- terra::global(
  rgba_check,
  c(
    "min",
    "mean",
    "max"
  ),
  na.rm = TRUE
)

print(
  rgba_stats
)

if (
  any(
    unlist(rgba_stats) < 0,
    na.rm = TRUE
  ) ||
  any(
    unlist(rgba_stats) > 255,
    na.rm = TRUE
  )
) {
  stop(
    "RGBA band values fall outside the expected 0-255 range."
  )
}

alpha_stats <- terra::global(
  rgba_check[["alpha"]],
  c(
    "min",
    "max"
  ),
  na.rm = TRUE
)

if (
  alpha_stats[1, "min"] < 0 ||
  alpha_stats[1, "max"] > 255
) {
  stop(
    "Alpha values fall outside the expected 0-255 range."
  )
}

rgba_size_mb <- file.info(
  OUTPUT_RGBA
)$size / 1024^2

message("")
message(
  "RGBA file size: ",
  round(
    rgba_size_mb,
    1
  ),
  " MB"
)

message(
  "Alpha range:    ",
  alpha_stats[1, "min"],
  " to ",
  alpha_stats[1, "max"]
)

message("")
message("Grid QA:")
message("  rows:   ", terra::nrow(rgba_check))
message("  cols:   ", terra::ncol(rgba_check))
message("  CRS:    matches input COG")
message("  extent: matches input COG")

# ==============================================================================
# 5. CLEAN TEMP FILE
# ==============================================================================

if (
  DELETE_TEMP_RGBA &&
  file.exists(TEMP_RGBA)
) {
  unlink(TEMP_RGBA)
}

# ==============================================================================
# DONE
# ==============================================================================

message("")
message("==============================================================")
message("DONE")
message("==============================================================")
message("Area:         ", AREA_ID)
message("Cycle:        ", CYCLE_ID)
message("Map date:     ", MAP_DATE)
message("Input COG:    ", LOCAL_COG)
message("RGBA output:  ", OUTPUT_RGBA)
message("")
message(
  "Stage 02 local RGBA complete. ",
  "No RGBA upload, render _SUCCESS, or manifest was written."
)
