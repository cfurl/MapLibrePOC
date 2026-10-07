#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(paws.storage)
})

# ------------------------------------------------------------------------------
# STG4 radar_frames.json manifest builder + publisher
# PAWS version
#
# Inputs:
#   --area texas
#   --cycle 2026100512
#
# Environment fallbacks:
#   AREA_ID
#   TRIGGER_CYCLE
#   RENDER_BUCKET
#   RADAR_FRAMES_CONFIG_KEY
#   AWS_REGION / AWS_DEFAULT_REGION
#
# Reads:
#   per-area radar frames config from S3
#
# Checks:
#   expected PMTiles cycle _SUCCESS markers
#   corresponding durable PMTiles objects
#
# Writes:
#   local radar_frames.json
#
# Publishes:
#   radar_frames.json to the manifest_key defined in the area config
# ------------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
}

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) == 1) {
    return(dirname(normalizePath(
      sub("^--file=", "", file_arg),
      winslash = "/",
      mustWork = FALSE
    )))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

parse_cli <- function(args) {
  out <- list()
  i <- 1

  while (i <= length(args)) {
    arg <- args[[i]]

    if (arg %in% c("--area", "--cycle", "--bucket", "--config-key", "--output")) {
      if (i == length(args)) {
        stop("Missing value after ", arg, call. = FALSE)
      }

      key <- sub("^--", "", arg)
      key <- gsub("-", "_", key)
      out[[key]] <- args[[i + 1]]
      i <- i + 2

    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
  }

  out
}

join_url <- function(base, key) {
  paste0(sub("/+$", "", base), "/", sub("^/+", "", key))
}

cycle_to_time <- function(cycle) {
  if (!grepl("^[0-9]{10}$", cycle)) {
    stop(
      "Cycle must be YYYYMMDDHH, for example 2026100512.",
      call. = FALSE
    )
  }

  x <- as.POSIXct(cycle, format = "%Y%m%d%H", tz = "UTC")

  if (is.na(x)) {
    stop("Could not parse trigger cycle: ", cycle, call. = FALSE)
  }

  x
}

raw_to_text <- function(x) {
  if (is.raw(x)) {
    return(rawToChar(x))
  }

  if (is.character(x)) {
    return(paste0(x, collapse = ""))
  }

  stop("Unexpected S3 Body type: ", paste(class(x), collapse = "/"), call. = FALSE)
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
      msg <- conditionMessage(e)

      if (
        grepl("404|Not Found|NoSuchKey|NotFound", msg, ignore.case = TRUE)
      ) {
        return(FALSE)
      }

      stop(
        "S3 HEAD failed for s3://", bucket, "/", key,
        "\n", msg,
        call. = FALSE
      )
    }
  )
}

# ---- paths / environment ------------------------------------------------------

script_dir <- get_script_dir()

local_renviron <- file.path(script_dir, ".Renviron")

if (file.exists(local_renviron)) {
  message("Loading local .Renviron: ", local_renviron)
  readRenviron(local_renviron)
} else {
  message("No local .Renviron found at: ", local_renviron)
  message("Continuing with credentials/environment already available to R.")
}

args <- parse_cli(commandArgs(trailingOnly = TRUE))

area_id <- args$area %||% Sys.getenv("AREA_ID", unset = "")
trigger_cycle <- args$cycle %||% Sys.getenv("TRIGGER_CYCLE", unset = "")
bucket <- args$bucket %||% Sys.getenv(
  "RENDER_BUCKET",
  unset = "stg4-24hr-large-area-render"
)

region <- Sys.getenv(
  "AWS_REGION",
  unset = Sys.getenv("AWS_DEFAULT_REGION", unset = "us-east-2")
)

if (!nzchar(area_id)) {
  stop("AREA_ID is required. Use --area texas or set AREA_ID.", call. = FALSE)
}

if (!nzchar(trigger_cycle)) {
  stop(
    "TRIGGER_CYCLE is required. Use --cycle 2026100512 or set TRIGGER_CYCLE.",
    call. = FALSE
  )
}

default_config_key <- paste0(
  "CONUS_subset/production_areas/",
  area_id,
  "/radar_frames_config/radar_frames_",
  area_id,
  "_config.json"
)

config_key <- args$config_key %||% Sys.getenv(
  "RADAR_FRAMES_CONFIG_KEY",
  unset = default_config_key
)

output_path <- args$output %||% file.path(script_dir, "radar_frames.json")

# ---- AWS client ---------------------------------------------------------------

message("AWS region: ", region)

s3 <- paws.storage::s3(config = list(region = region))

# ---- read config --------------------------------------------------------------

message("")
message("Reading config:")
message("  s3://", bucket, "/", config_key)

config_resp <- tryCatch(
  s3$get_object(
    Bucket = bucket,
    Key = config_key
  ),
  error = function(e) {
    stop(
      "Could not read config from S3.\n",
      "s3://", bucket, "/", config_key, "\n",
      conditionMessage(e),
      call. = FALSE
    )
  }
)

config <- jsonlite::fromJSON(
  raw_to_text(config_resp$Body),
  simplifyVector = FALSE
)

required_fields <- c(
  "schema_version",
  "area_id",
  "product",
  "cadence_hours",
  "window_steps",
  "signals_prefix",
  "pmtiles_prefix",
  "manifest_key",
  "public_base_url",
  "browser_defaults"
)

missing_fields <- setdiff(required_fields, names(config))

if (length(missing_fields) > 0) {
  stop(
    "Config is missing required field(s): ",
    paste(missing_fields, collapse = ", "),
    call. = FALSE
  )
}

if (!identical(tolower(config$area_id), tolower(area_id))) {
  stop(
    "Config area_id ('", config$area_id,
    "') does not match requested area ('", area_id, "').",
    call. = FALSE
  )
}

cadence_hours <- as.integer(config$cadence_hours)
window_steps <- as.integer(config$window_steps)

if (is.na(cadence_hours) || cadence_hours <= 0) {
  stop("cadence_hours must be a positive integer.", call. = FALSE)
}

if (is.na(window_steps) || window_steps <= 0) {
  stop("window_steps must be a positive integer.", call. = FALSE)
}

# ---- build expected cycle window ---------------------------------------------

trigger_time <- cycle_to_time(trigger_cycle)

expected_times <- trigger_time -
  rev(seq.int(0, window_steps - 1)) * cadence_hours * 3600

expected_cycles <- format(
  expected_times,
  "%Y%m%d%H",
  tz = "UTC"
)

message("")
message("Expected cycle window:")

for (x in expected_cycles) {
  message("  ", x)
}

# ---- check success markers and construct frames -------------------------------

frames <- list()
missing_cycles <- character(0)

message("")
message("Checking PMTiles cycle success markers...")

for (i in seq_along(expected_cycles)) {

  cycle <- expected_cycles[[i]]
  cycle_time <- expected_times[[i]]

  success_key <- paste0(
    sub("/+$", "", config$signals_prefix),
    "/cycle=", cycle,
    "/_SUCCESS"
  )

  ok <- s3_object_exists(s3, bucket, success_key)

  if (!ok) {
    message("  MISSING  ", cycle)
    missing_cycles <- c(missing_cycles, cycle)
    next
  }

  yyyy <- format(cycle_time, "%Y", tz = "UTC")
  mm   <- format(cycle_time, "%m", tz = "UTC")
  dd   <- format(cycle_time, "%d", tz = "UTC")

  pmtiles_key <- paste0(
    sub("/+$", "", config$pmtiles_prefix),
    "/year=", yyyy,
    "/month=", mm,
    "/day=", dd,
    "/stage4_daily.pmtiles"
  )

  if (!s3_object_exists(s3, bucket, pmtiles_key)) {
    stop(
      "Found cycle _SUCCESS but PMTiles object is missing:\n",
      "  cycle: ", cycle, "\n",
      "  s3://", bucket, "/", pmtiles_key,
      call. = FALSE
    )
  }

  message("  OK       ", cycle)

  frames[[length(frames) + 1]] <- list(
    cycle = cycle,
    date = format(cycle_time, "%Y-%m-%d", tz = "UTC"),
    pmtiles = join_url(
      config$public_base_url,
      pmtiles_key
    )
  )
}

available_count <- length(frames)

if (available_count == 0) {
  stop(
    "No successful PMTiles frames were found in the configured window. ",
    "Manifest will not be published.",
    call. = FALSE
  )
}

# ---- build manifest -----------------------------------------------------------

manifest <- list(
  schema_version = 1,
  area_id = config$area_id,
  product = config$product,
  trigger_cycle = trigger_cycle,
  updated_utc = format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  ),
  window = list(
    start_cycle = expected_cycles[[1]],
    end_cycle = expected_cycles[[length(expected_cycles)]],
    cadence_hours = cadence_hours,
    expected_count = window_steps,
    available_count = available_count
  ),
  missing_cycles = as.list(missing_cycles),
  browser_defaults = config$browser_defaults,
  frames = frames
)

# ---- write local copy ---------------------------------------------------------

dir.create(
  dirname(output_path),
  recursive = TRUE,
  showWarnings = FALSE
)

jsonlite::write_json(
  manifest,
  path = output_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

message("")
message("Local manifest written:")
message("  ", normalizePath(
  output_path,
  winslash = "\\",
  mustWork = FALSE
))

# ---- publish manifest to S3 ---------------------------------------------------

manifest_key <- config$manifest_key

message("")
message("Publishing manifest:")
message("  s3://", bucket, "/", manifest_key)

manifest_raw <- readBin(
  output_path,
  what = "raw",
  n = file.info(output_path)$size
)

tryCatch(
  s3$put_object(
    Bucket = bucket,
    Key = manifest_key,
    Body = manifest_raw,
    ContentType = "application/json",
    CacheControl = "no-cache"
  ),
  error = function(e) {
    stop(
      "Manifest upload failed:\n",
      conditionMessage(e),
      call. = FALSE
    )
  }
)

# ---- verify published manifest ------------------------------------------------

message("Verifying uploaded manifest...")

verify_resp <- tryCatch(
  s3$get_object(
    Bucket = bucket,
    Key = manifest_key
  ),
  error = function(e) {
    stop(
      "Manifest was uploaded but could not be read back for verification:\n",
      conditionMessage(e),
      call. = FALSE
    )
  }
)

verify_manifest <- jsonlite::fromJSON(
  raw_to_text(verify_resp$Body),
  simplifyVector = FALSE
)

if (!identical(verify_manifest$area_id, manifest$area_id)) {
  stop(
    "Uploaded manifest verification failed: area_id does not match.",
    call. = FALSE
  )
}

if (!identical(verify_manifest$trigger_cycle, manifest$trigger_cycle)) {
  stop(
    "Uploaded manifest verification failed: trigger_cycle does not match.",
    call. = FALSE
  )
}

if (!identical(
  as.integer(verify_manifest$window$available_count),
  as.integer(manifest$window$available_count)
)) {
  stop(
    "Uploaded manifest verification failed: available_count does not match.",
    call. = FALSE
  )
}

# ---- summary ------------------------------------------------------------------

message("")
message("------------------------------------------------------------")
message("Manifest build + publish complete")
message("------------------------------------------------------------")
message("Area:             ", area_id)
message("Trigger cycle:    ", trigger_cycle)
message("Expected frames:  ", window_steps)
message("Available:        ", available_count)
message("Missing:          ", length(missing_cycles))

if (length(missing_cycles) > 0) {
  message(
    "Missing cycles:   ",
    paste(missing_cycles, collapse = ", ")
  )
}

message(
  "Local output:      ",
  normalizePath(
    output_path,
    winslash = "\\",
    mustWork = FALSE
  )
)

message("Published object:  s3://", bucket, "/", manifest_key)
message("Public URL:        ", join_url(config$public_base_url, manifest_key))
message("")
