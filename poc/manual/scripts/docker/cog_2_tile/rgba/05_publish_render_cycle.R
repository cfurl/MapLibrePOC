# ==============================================================================
# 05_publish_render_cycle.R
#
# Purpose:
#   Publish the completed raster PMTiles archive for one area/cycle to the
#   durable render bucket, write per-frame render metadata, then write render
#   _SUCCESS markers LAST.
#
# Runtime:
#   Rscript 05_publish_render_cycle.R \
#     --area texas \
#     --cycle 2026100312
#
# Required local input:
#   /work/<area>/<cycle>/pmtiles/stage4_daily.pmtiles
#
# Durable outputs:
#   .../pmtiles/year=YYYY/month=MM/day=DD/stage4_daily.pmtiles
#   .../metadata/year=YYYY/month=MM/day=DD/render.json
#   .../signals/year=YYYY/month=MM/day=DD/_SUCCESS
#   .../signals/cycle/cycle=YYYYMMDDHH/_SUCCESS
#
# IMPORTANT:
#   The success markers are written only after:
#     1. PMTiles exists locally
#     2. PMTiles is uploaded
#     3. S3 HEAD verifies uploaded byte size
#     4. render.json is uploaded and verified
#
#   No public latest.json / radar_frames.json manifest is written here.
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
    local_file
) {

  dir.create(
    dirname(local_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  message("")
  message("Downloading render config:")
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

put_file <- function(
    s3,
    bucket,
    key,
    local_file,
    content_type,
    cache_control = NULL
) {

  if (!file.exists(local_file)) {
    stop(
      "Local upload file does not exist:\n",
      local_file
    )
  }

  body <- readBin(
    local_file,
    what = "raw",
    n = file.info(local_file)$size
  )

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

verify_object_exists <- function(
    s3,
    bucket,
    key
) {

  head <- s3$head_object(
    Bucket = bucket,
    Key = key
  )

  invisible(head)
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

aws_cfg <- config$aws %||% list()

storage_cfg <- merge_lists(
  config$storage,
  area_cfg$storage
)

run_cfg <- merge_lists(
  config$defaults$run,
  area_cfg$run
)

tiles_cfg <- merge_lists(
  config$defaults$tiles,
  area_cfg$tiles
)

pmtiles_cfg <- merge_lists(
  config$defaults$pmtiles,
  area_cfg$pmtiles
)

delivery_cfg <- merge_lists(
  config$defaults$delivery,
  area_cfg$delivery
)

AWS_REGION <- aws_cfg$region %||% BOOTSTRAP_REGION
RENDER_BUCKET <- storage_cfg$render_bucket

if (
  is.null(RENDER_BUCKET) ||
  !nzchar(RENDER_BUCKET)
) {
  stop("storage.render_bucket is missing from render config.")
}

if (
  is.null(storage_cfg$pmtiles_key) ||
  !nzchar(storage_cfg$pmtiles_key)
) {
  stop("storage.pmtiles_key is missing from render config.")
}

UPLOAD_PMTILES <- if (is.null(run_cfg$upload_pmtiles)) {
  TRUE
} else {
  isTRUE(run_cfg$upload_pmtiles)
}

if (!UPLOAD_PMTILES) {
  stop(
    "Publish stage requested, but run.upload_pmtiles is FALSE."
  )
}

PMTILES_CACHE_CONTROL <- delivery_cfg$cache_control_pmtiles %||%
  "public,max-age=31536000,immutable"

# These can be added to render_config.json later.  Defaults preserve the
# agreed durable layout without requiring a config migration for this test.
RENDER_METADATA_TEMPLATE <- storage_cfg$render_metadata_key %||%
  paste0(
    "CONUS_subset/production_areas/{area_id}/precip/daily/pmtiles/",
    "metadata/year={YYYY}/month={MM}/day={DD}/render.json"
  )

RENDER_SUCCESS_TEMPLATE <- storage_cfg$render_success_key %||%
  paste0(
    "CONUS_subset/production_areas/{area_id}/precip/daily/pmtiles/",
    "signals/year={YYYY}/month={MM}/day={DD}/_SUCCESS"
  )

RENDER_CYCLE_SUCCESS_TEMPLATE <- storage_cfg$render_cycle_success_key %||%
  paste0(
    "CONUS_subset/production_areas/{area_id}/precip/daily/pmtiles/",
    "signals/cycle/cycle={cycle}/_SUCCESS"
  )

# ==============================================================================
# DERIVED PATHS
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

PMTILES_KEY <- render_template(
  storage_cfg$pmtiles_key,
  template_values
)

RENDER_METADATA_KEY <- render_template(
  RENDER_METADATA_TEMPLATE,
  template_values
)

RENDER_SUCCESS_KEY <- render_template(
  RENDER_SUCCESS_TEMPLATE,
  template_values
)

RENDER_CYCLE_SUCCESS_KEY <- render_template(
  RENDER_CYCLE_SUCCESS_TEMPLATE,
  template_values
)

COG_KEY <- if (
  !is.null(storage_cfg$cog_key) &&
  nzchar(storage_cfg$cog_key)
) {
  render_template(
    storage_cfg$cog_key,
    template_values
  )
} else {
  NA_character_
}

RUN_ROOT <- file.path(
  WORK_ROOT,
  AREA_ID,
  CYCLE_ID
)

LOCAL_PMTILES <- file.path(
  RUN_ROOT,
  "pmtiles",
  "stage4_daily.pmtiles"
)

if (!file.exists(LOCAL_PMTILES)) {
  stop(
    "Local PMTiles archive not found:\n",
    LOCAL_PMTILES,
    "\n\nRun Stage 04 first."
  )
}

PMSIZE <- as.numeric(
  file.info(LOCAL_PMTILES)$size
)

if (
  !is.finite(PMSIZE) ||
  PMSIZE <= 0
) {
  stop(
    "Local PMTiles archive has invalid size."
  )
}

# ==============================================================================
# AWS CLIENT
# ==============================================================================

s3 <- paws::s3(
  config = list(
    region = AWS_REGION
  )
)

# ==============================================================================
# LOG
# ==============================================================================

message("")
message("==============================================================")
message("05 PUBLISH RENDER")
message("==============================================================")
message("Area:                  ", AREA_ID)
message("Cycle:                 ", CYCLE_ID)
message("Map date:              ", MAP_DATE)
message("Config:                ", LOCAL_CONFIG)
message("AWS region:            ", AWS_REGION)
message("Local PMTiles:          ", LOCAL_PMTILES)
message("PMTiles bytes:          ", format(PMSIZE, big.mark = ","))
message("")
message("Durable PMTiles:")
message("  s3://", RENDER_BUCKET, "/", PMTILES_KEY)
message("")
message("Render metadata:")
message("  s3://", RENDER_BUCKET, "/", RENDER_METADATA_KEY)
message("")
message("Date success:")
message("  s3://", RENDER_BUCKET, "/", RENDER_SUCCESS_KEY)
message("")
message("Cycle success:")
message("  s3://", RENDER_BUCKET, "/", RENDER_CYCLE_SUCCESS_KEY)

# ==============================================================================
# 1. UPLOAD PMTILES
# ==============================================================================

message("")
message("==============================================================")
message("1. UPLOADING PMTILES")
message("==============================================================")

put_file(
  s3 = s3,
  bucket = RENDER_BUCKET,
  key = PMTILES_KEY,
  local_file = LOCAL_PMTILES,
  content_type = "application/vnd.pmtiles",
  cache_control = PMTILES_CACHE_CONTROL
)

verify_object_size(
  s3 = s3,
  bucket = RENDER_BUCKET,
  key = PMTILES_KEY,
  expected_bytes = PMSIZE
)

message(
  "Verified uploaded PMTiles byte size: ",
  format(PMSIZE, big.mark = ",")
)

# ==============================================================================
# 2. WRITE RENDER.JSON
# ==============================================================================

message("")
message("==============================================================")
message("2. WRITING RENDER METADATA")
message("==============================================================")

render_metadata <- list(
  schema_version = 1L,
  area_id = AREA_ID,
  cycle = CYCLE_ID,
  map_date = DATE_ID,
  product = "stage4_daily",
  created_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  source = list(
    cog = if (!is.na(COG_KEY)) {
      paste0(
        "s3://",
        RENDER_BUCKET,
        "/",
        COG_KEY
      )
    } else {
      NULL
    }
  ),
  delivery = list(
    pmtiles = paste0(
      "s3://",
      RENDER_BUCKET,
      "/",
      PMTILES_KEY
    ),
    content_type = "application/vnd.pmtiles",
    bytes = PMSIZE,
    tile_type = pmtiles_cfg$tile_type %||% "png",
    min_zoom = as.integer(tiles_cfg$min_zoom %||% 4),
    max_zoom = as.integer(tiles_cfg$max_zoom %||% 9),
    scheme = tiles_cfg$scheme %||% "xyz"
  )
)

render_json_text <- jsonlite::toJSON(
  render_metadata,
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)

render_json_raw <- charToRaw(
  paste0(
    render_json_text,
    "\n"
  )
)

put_raw(
  s3 = s3,
  bucket = RENDER_BUCKET,
  key = RENDER_METADATA_KEY,
  body = render_json_raw,
  content_type = "application/json",
  cache_control = "no-cache"
)

verify_object_size(
  s3 = s3,
  bucket = RENDER_BUCKET,
  key = RENDER_METADATA_KEY,
  expected_bytes = length(render_json_raw)
)

message("Verified render.json.")

# ==============================================================================
# 3. WRITE _SUCCESS MARKERS LAST
# ==============================================================================

message("")
message("==============================================================")
message("3. WRITING RENDER _SUCCESS MARKERS")
message("==============================================================")

# Date success marker first.
put_raw(
  s3 = s3,
  bucket = RENDER_BUCKET,
  key = RENDER_SUCCESS_KEY,
  body = raw(0),
  content_type = "application/octet-stream",
  cache_control = "no-cache"
)

verify_object_exists(
  s3 = s3,
  bucket = RENDER_BUCKET,
  key = RENDER_SUCCESS_KEY
)

message("Wrote date _SUCCESS.")

# Cycle success is intentionally the final write.  A downstream consumer that
# gates on this marker can treat it as proof that the durable frame is complete.
put_raw(
  s3 = s3,
  bucket = RENDER_BUCKET,
  key = RENDER_CYCLE_SUCCESS_KEY,
  body = raw(0),
  content_type = "application/octet-stream",
  cache_control = "no-cache"
)

verify_object_exists(
  s3 = s3,
  bucket = RENDER_BUCKET,
  key = RENDER_CYCLE_SUCCESS_KEY
)

message("Wrote cycle _SUCCESS LAST.")

# ==============================================================================
# DONE
# ==============================================================================

message("")
message("==============================================================")
message("DONE")
message("==============================================================")
message("Area:          ", AREA_ID)
message("Cycle:         ", CYCLE_ID)
message("")
message("Published:")
message("  PMTiles:     s3://", RENDER_BUCKET, "/", PMTILES_KEY)
message("  render.json: s3://", RENDER_BUCKET, "/", RENDER_METADATA_KEY)
message("  date success:s3://", RENDER_BUCKET, "/", RENDER_SUCCESS_KEY)
message("  cycle success:s3://", RENDER_BUCKET, "/", RENDER_CYCLE_SUCCESS_KEY)
message("")
message(
  "Render frame is durable and marked complete. ",
  "Public latest/animation manifests were NOT updated."
)
