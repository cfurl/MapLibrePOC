# ==============================================================================
# upload_json_to_s3.R
#
# Generic uploader for a local JSON file -> S3.
#
# Credentials:
#   Reads AWS credentials/settings from .Renviron, then lets paws use the
#   normal environment-variable credential chain.
#
# Expected .Renviron entries may include:
#   AWS_ACCESS_KEY_ID=...
#   AWS_SECRET_ACCESS_KEY=...
#   AWS_SESSION_TOKEN=...        # only if applicable
#   AWS_REGION=us-east-2         # optional; script also sets region below
#
# ==============================================================================

suppressPackageStartupMessages({
  library(paws)
})

# ==============================================================================
# CONFIG - EDIT THESE WHEN NEEDED
# ==============================================================================

LOCAL_FILE <- "C:/stg4/MapLibrePOC/poc/manual/config/render_config.json"

S3_URI <- paste0(
  "s3://stg4-24hr-large-area-render/",
  "CONUS_subset/config/render/large_area/render_config.json"
)

AWS_REGION <- "us-east-2"

# Use "~/.Renviron" for the normal user-level file.
# Change this if you keep a project-specific .Renviron somewhere else.
RENVRON_PATH <- "~/.Renviron"

CONTENT_TYPE <- "application/json"
CACHE_CONTROL <- "no-cache"

# ==============================================================================
# LOAD .Renviron
# ==============================================================================

renviron_expanded <- path.expand(RENVRON_PATH)

if (!file.exists(renviron_expanded)) {
  stop(
    ".Renviron file not found:\n",
    renviron_expanded
  )
}

readRenviron(renviron_expanded)

message("Loaded AWS environment settings from: ", renviron_expanded)

# ==============================================================================
# VALIDATE LOCAL FILE
# ==============================================================================

if (!file.exists(LOCAL_FILE)) {
  stop(
    "Local JSON file not found:\n",
    LOCAL_FILE
  )
}

if (tolower(tools::file_ext(LOCAL_FILE)) != "json") {
  warning(
    "Local file does not have a .json extension:\n",
    LOCAL_FILE
  )
}

# Optional light validation that the file contains valid JSON text.
json_text <- paste(
  readLines(
    LOCAL_FILE,
    warn = FALSE,
    encoding = "UTF-8"
  ),
  collapse = "\n"
)

if (!nzchar(trimws(json_text))) {
  stop("Local JSON file is empty.")
}

# ==============================================================================
# PARSE S3 URI
# ==============================================================================

parse_s3_uri <- function(uri) {

  if (!grepl("^s3://", uri)) {
    stop("S3_URI must begin with s3://")
  }

  x <- sub("^s3://", "", uri)
  parts <- strsplit(x, "/", fixed = TRUE)[[1]]

  if (length(parts) < 2) {
    stop(
      "S3_URI must include both bucket and object key:\n",
      uri
    )
  }

  list(
    bucket = parts[[1]],
    key = paste(parts[-1], collapse = "/")
  )
}

target <- parse_s3_uri(S3_URI)

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
# UPLOAD
# ==============================================================================

message("")
message("Uploading:")
message("  ", LOCAL_FILE)
message("to:")
message("  ", S3_URI)

body_raw <- readBin(
  LOCAL_FILE,
  what = "raw",
  n = file.info(LOCAL_FILE)$size
)

s3$put_object(
  Bucket = target$bucket,
  Key = target$key,
  Body = body_raw,
  ContentType = CONTENT_TYPE,
  CacheControl = CACHE_CONTROL
)

# ==============================================================================
# VERIFY
# ==============================================================================

check <- s3$head_object(
  Bucket = target$bucket,
  Key = target$key
)

message("")
message("Upload verified.")
message("S3 URI:        ", S3_URI)
message("ContentLength: ", check$ContentLength)
message("ContentType:   ", check$ContentType)
message("CacheControl:  ", check$CacheControl)
message("ETag:          ", check$ETag)

message("")
message("DONE")
