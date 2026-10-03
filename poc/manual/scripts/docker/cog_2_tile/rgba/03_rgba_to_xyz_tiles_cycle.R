# ==============================================================================
# 03_rgba_to_xyz_tiles_cycle.R
#
# Purpose:
#   Convert the transient RGBA GeoTIFF produced by Stage 02 into an XYZ PNG
#   tile pyramid using the same proven gdal2tiles settings as the manual POC.
#
# Runtime:
#   Rscript 03_rgba_to_xyz_tiles_cycle.R \
#     --area texas \
#     --cycle 2026100312
#
# Input:
#   /work/<area>/<cycle>/rgba/stage4_daily_rgba.tif
#
# Output:
#   /work/<area>/<cycle>/tiles/{z}/{x}/{y}.png
#
# Config:
#   Reads render_config.json from S3 unless --config points to a local file.
#
# This stage is transient/local only. It does NOT upload tiles to S3 and does
# NOT write render _SUCCESS or manifests.
# ==============================================================================

suppressPackageStartupMessages({
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

download_s3_object <- function(
    s3,
    bucket,
    key,
    local_file
) {

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

  # Reuse the config already downloaded to /work when possible.
  local_config <- file.path(
    cli$work_dir,
    "config",
    basename(cli$config_key)
  )

  if (file.exists(local_config)) {
    message(
      "Using existing local render config: ",
      local_config
    )

    return(
      normalizePath(
        local_config,
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
      "No local config exists and S3 config bucket/key are missing."
    )
  }

  download_s3_object(
    s3 = s3_bootstrap,
    bucket = cli$config_bucket,
    key = cli$config_key,
    local_file = local_config
  )

  normalizePath(
    local_config,
    winslash = "/",
    mustWork = TRUE
  )
}

find_gdal2tiles <- function() {

  candidates <- c(
    "gdal2tiles.py",
    "gdal2tiles"
  )

  for (candidate in candidates) {
    resolved <- Sys.which(candidate)

    if (nzchar(resolved)) {
      return(unname(resolved))
    }
  }

  stop(
    "Could not find gdal2tiles.py or gdal2tiles on PATH."
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
WORK_ROOT <- cli$work_dir
BOOTSTRAP_REGION <- cli$aws_region %||% "us-east-2"

dir.create(
  WORK_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==============================================================================
# CONFIG
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

tiles_cfg <- merge_lists(
  config$defaults$tiles,
  area_cfg$tiles
)

if (is.null(tiles_cfg)) {
  stop(
    "defaults.tiles is missing from render_config.json."
  )
}

MIN_ZOOM <- as.integer(
  tiles_cfg$min_zoom
)

MAX_ZOOM <- as.integer(
  tiles_cfg$max_zoom
)

RESAMPLING <- tiles_cfg$resampling %||% "bilinear"
WEBVIEWER <- tiles_cfg$webviewer %||% "none"
SCHEME <- tiles_cfg$scheme %||% "xyz"
TILE_SIZE <- as.integer(
  tiles_cfg$tile_size %||% 256
)

if (
  !is.finite(MIN_ZOOM) ||
  !is.finite(MAX_ZOOM) ||
  MIN_ZOOM < 0 ||
  MAX_ZOOM < MIN_ZOOM
) {
  stop(
    "Invalid tile zoom range: ",
    MIN_ZOOM,
    "-",
    MAX_ZOOM
  )
}

if (!identical(SCHEME, "xyz")) {
  stop(
    "This pipeline currently requires tiles.scheme = 'xyz'. ",
    "Found: ",
    SCHEME
  )
}

if (TILE_SIZE != 256L) {
  stop(
    "This pipeline currently expects tile_size = 256. ",
    "Found: ",
    TILE_SIZE
  )
}

GDAL2TILES <- find_gdal2tiles()

# ==============================================================================
# PATHS
# ==============================================================================

RUN_ROOT <- file.path(
  WORK_ROOT,
  AREA_ID,
  CYCLE_ID
)

RGBA_DIR <- file.path(
  RUN_ROOT,
  "rgba"
)

TILES_DIR <- file.path(
  RUN_ROOT,
  "tiles"
)

INPUT_RGBA <- file.path(
  RGBA_DIR,
  "stage4_daily_rgba.tif"
)

if (!file.exists(INPUT_RGBA)) {
  stop(
    "Input RGBA TIFF not found:\n",
    INPUT_RGBA,
    "\n\nRun Stage 02 first."
  )
}

# ==============================================================================
# CONFIG LOG
# ==============================================================================

message("")
message("==============================================================")
message("03 RGBA -> XYZ TILES")
message("==============================================================")
message("Area:          ", AREA_ID)
message("Cycle:         ", CYCLE_ID)
message("Config:        ", LOCAL_CONFIG)
message("Input RGBA:    ", INPUT_RGBA)
message("Output tiles:  ", TILES_DIR)
message("Zooms:         ", MIN_ZOOM, "-", MAX_ZOOM)
message("Resampling:    ", RESAMPLING)
message("Webviewer:     ", WEBVIEWER)
message("Scheme:        ", SCHEME)
message("Tile size:     ", TILE_SIZE)
message("gdal2tiles:    ", GDAL2TILES)

message("")
message("GDAL2Tiles version:")

version_status <- system2(
  GDAL2TILES,
  args = "--version"
)

if (!identical(version_status, 0L)) {
  stop(
    "gdal2tiles did not start correctly."
  )
}

# ==============================================================================
# CLEAN OUTPUT DIRECTORY
# ==============================================================================

if (dir.exists(TILES_DIR)) {

  message("")
  message(
    "Removing existing tile directory: ",
    TILES_DIR
  )

  unlink(
    TILES_DIR,
    recursive = TRUE,
    force = TRUE
  )
}

dir.create(
  TILES_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==============================================================================
# BUILD XYZ TILES
# ==============================================================================

message("")
message("==============================================================")
message("BUILDING XYZ TILES")
message("==============================================================")
message("")

args <- c(
  "--xyz",
  paste0(
    "--zoom=",
    MIN_ZOOM,
    "-",
    MAX_ZOOM
  ),
  paste0(
    "--resampling=",
    RESAMPLING
  ),
  paste0(
    "--webviewer=",
    WEBVIEWER
  ),
  INPUT_RGBA,
  TILES_DIR
)

t0 <- Sys.time()

status <- system2(
  GDAL2TILES,
  args = args
)

if (!identical(status, 0L)) {
  stop(
    "gdal2tiles failed with exit status ",
    status
  )
}

message("")
message(
  "Tile build seconds: ",
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
# QA
# ==============================================================================

message("")
message("==============================================================")
message("XYZ TILE QA")
message("==============================================================")

zoom_dirs <- file.path(
  TILES_DIR,
  as.character(
    MIN_ZOOM:MAX_ZOOM
  )
)

missing_zoom_dirs <- zoom_dirs[
  !dir.exists(zoom_dirs)
]

if (length(missing_zoom_dirs) > 0) {
  stop(
    "Missing expected zoom director",
    if (length(missing_zoom_dirs) == 1) "y" else "ies",
    ":\n",
    paste(
      missing_zoom_dirs,
      collapse = "\n"
    )
  )
}

png_files <- list.files(
  TILES_DIR,
  pattern = "\\.png$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(png_files) == 0) {
  stop(
    "gdal2tiles completed but no PNG tiles were found."
  )
}

zoom_levels <- MIN_ZOOM:MAX_ZOOM

tile_counts <- vapply(
  zoom_levels,
  function(z) {

    length(
      list.files(
        file.path(
          TILES_DIR,
          as.character(z)
        ),
        pattern = "\\.png$",
        recursive = TRUE
      )
    )
  },
  integer(1)
)

message(
  "Total PNG tiles: ",
  format(
    length(png_files),
    big.mark = ","
  )
)

message("")
message("Tiles by zoom:")

for (i in seq_along(tile_counts)) {

  message(
    "  z",
    zoom_levels[[i]],
    ": ",
    format(
      tile_counts[[i]],
      big.mark = ","
    )
  )
}

# Pick a deterministic sample tile for inspection.
sample_tile <- png_files[[1]]

message("")
message("Sample tile:")
message("  ", sample_tile)

# ==============================================================================
# DONE
# ==============================================================================

message("")
message("==============================================================")
message("DONE")
message("==============================================================")
message("Area:       ", AREA_ID)
message("Cycle:      ", CYCLE_ID)
message("XYZ tiles:  ", TILES_DIR)
message("")
message(
  "Stage 03 local XYZ tiles complete. ",
  "No tile upload, PMTiles, render _SUCCESS, or manifest was written."
)
