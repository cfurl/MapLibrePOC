# ==============================================================================
# 02_cog_to_rgba.R
#
# Purpose:
#   Convert one canonical single-band Stage IV rainfall COG (rain_mm) into a
#   display-ready 4-band RGBA GeoTIFF using the same rainfall color function
#   previously applied in MapLibre/index.html.
#
# Input:
#   C:/stg4/MapLibrePOC/poc/manual/cog/YYYYMMDD/stage4_daily.tif
#
# Output:
#   C:/stg4/MapLibrePOC/poc/manual/colored/YYYYMMDD/stage4_daily_rgba.tif
# ==============================================================================

suppressPackageStartupMessages({
  library(terra)
})

# ==============================================================================
# CONFIG
# ==============================================================================

MAP_DATE <- as.Date("2026-07-13")

ROOT_DIR <- "C:/stg4/MapLibrePOC/poc/manual"

OVERWRITE_RGBA <- TRUE
DELETE_TEMP_RGBA <- TRUE

RAIN_SEGMENTS <- list(
  list(min = 0.001, max = 0.25, start = "#d7dee9", end = "#173f9d"),
  list(min = 0.25,  max = 1.50, start = "#39ef63", end = "#006719"),
  list(min = 1.50,  max = 3.00, start = "#fff23b", end = "#ff8b00"),
  list(min = 3.00,  max = 5.00, start = "#ff5738", end = "#c90e18"),
  list(min = 5.00,  max = 8.00, start = "#c68cff", end = "#64208f"),
  list(min = 8.00,  max = 12.00, start = "#ff5cc8", end = "#b0006f")
)

EXTREME_COLOR <- "#5a003f"

TRACE_ALPHA_MIN <- 115L
TRACE_ALPHA_MAX <- 255L

# ==============================================================================
# DERIVED PATHS
# ==============================================================================

DATE_ID <- format(MAP_DATE, "%Y%m%d")

INPUT_COG <- file.path(
  ROOT_DIR,
  "cog",
  DATE_ID,
  "stage4_daily.tif"
)

OUTPUT_DIR <- file.path(
  ROOT_DIR,
  "colored",
  DATE_ID
)

TEMP_RGBA <- file.path(
  OUTPUT_DIR,
  "stage4_daily_rgba_tmp.tif"
)

OUTPUT_RGBA <- file.path(
  OUTPUT_DIR,
  "stage4_daily_rgba.tif"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(INPUT_COG)) {
  stop(
    "Input COG not found:\n",
    INPUT_COG,
    "\n\nRun 01_parquet_to_cog.R first."
  )
}

if (file.exists(OUTPUT_RGBA) && !OVERWRITE_RGBA) {
  stop(
    "RGBA output already exists and OVERWRITE_RGBA = FALSE:\n",
    OUTPUT_RGBA
  )
}

# ==============================================================================
# FIND GDAL_TRANSLATE
# ==============================================================================



GDAL_TRANSLATE <- "C:/Program Files/QGIS 3.40.14/bin/gdal_translate.exe"

# ==============================================================================
# HELPERS
# ==============================================================================

hex_to_rgb <- function(hex) {

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

js_round <- function(x) {
  as.integer(floor(x + 0.5))
}

for (i in seq_along(RAIN_SEGMENTS)) {
  RAIN_SEGMENTS[[i]]$rgb0 <- hex_to_rgb(RAIN_SEGMENTS[[i]]$start)
  RAIN_SEGMENTS[[i]]$rgb1 <- hex_to_rgb(RAIN_SEGMENTS[[i]]$end)
}

EXTREME_RGB <- hex_to_rgb(EXTREME_COLOR)

# ==============================================================================
# PIXEL COLOR FUNCTION
# ==============================================================================

rain_mm_to_rgba <- function(v) {

  rain_mm <- as.numeric(v)
  rain_in <- rain_mm / 25.4

  n <- length(rain_in)

  out <- matrix(
    0L,
    nrow = n,
    ncol = 4
  )

  valid <- is.finite(rain_in) & rain_in >= 0.001

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

    if (segment$min == 0.001) {

      trace_t <- clamp01(
        (rain_in[idx] - 0.001) /
          (0.25 - 0.001)
      )

      alpha <- js_round(
        TRACE_ALPHA_MIN +
          (TRACE_ALPHA_MAX - TRACE_ALPHA_MIN) * trace_t
      )
    }

    out[idx, 4] <- alpha
  }

  idx_extreme <- which(
    valid &
      rain_in >= 12.0
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
# 1. READ CANONICAL COG
# ==============================================================================

message("")
message("==============================================================")
message("1. READING CANONICAL COG")
message("==============================================================")
message("Map date:   ", MAP_DATE)
message("Input COG:  ", INPUT_COG)
message("Output:     ", OUTPUT_RGBA)
message("GDAL:       ", GDAL_TRANSLATE)

rain <- terra::rast(
  INPUT_COG
)

print(rain)

if (terra::nlyr(rain) != 1) {
  stop(
    "Expected a single-band rainfall COG; found ",
    terra::nlyr(rain),
    " bands."
  )
}

message("")
message("Input rainfall summary, mm:")

print(
  terra::global(
    rain,
    c("min", "mean", "max"),
    na.rm = TRUE
  )
)

# ==============================================================================
# 2. APPLY DISPLAY COLOR FUNCTION
# ==============================================================================

message("")
message("==============================================================")
message("2. COLORIZING COG -> RGBA")
message("==============================================================")

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
# 3. WRITE FINAL RGB + ALPHA TIFF
# ==============================================================================

message("")
message("==============================================================")
message("3. FINALIZING RGB + ALPHA TIFF")
message("==============================================================")

if (file.exists(OUTPUT_RGBA) && OVERWRITE_RGBA) {
  unlink(OUTPUT_RGBA)
}

args <- c(
  "-of", "GTiff",
  "-a_nodata", "none",
  "-colorinterp", "red,green,blue,alpha",
  "-co", "TILED=YES",
  "-co", "COMPRESS=DEFLATE",
  "-co", "PREDICTOR=2",
  "-co", "BIGTIFF=IF_SAFER",
  shQuote(TEMP_RGBA),
  shQuote(OUTPUT_RGBA)
)

status <- system2(
  GDAL_TRANSLATE,
  args = args
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

names(rgba_check) <- c(
  "red",
  "green",
  "blue",
  "alpha"
)

print(rgba_check)

message("")
message("RGBA band summaries:")

print(
  terra::global(
    rgba_check,
    c("min", "mean", "max"),
    na.rm = TRUE
  )
)

rgba_size_mb <- file.info(
  OUTPUT_RGBA
)$size / 1024^2

message("")
message("RGBA file size: ", round(rgba_size_mb, 1), " MB")

alpha_stats <- terra::global(
  rgba_check[["alpha"]],
  c("min", "max"),
  na.rm = TRUE
)

if (
  alpha_stats[1, "min"] > 0 ||
    alpha_stats[1, "max"] < 255
) {
  warning(
    "Unexpected alpha range. Expected the final raster to contain ",
    "both alpha=0 and alpha=255 pixels."
  )
}

# ==============================================================================
# 5. CLEAN TEMP FILE
# ==============================================================================

if (DELETE_TEMP_RGBA && file.exists(TEMP_RGBA)) {
  unlink(TEMP_RGBA)
}

# ==============================================================================
# DONE
# ==============================================================================

message("")
message("==============================================================")
message("DONE")
message("==============================================================")
message("Map date:     ", MAP_DATE)
message("Input COG:    ", INPUT_COG)
message("RGBA output:  ", OUTPUT_RGBA)
message("")
message("Next stage will build XYZ tiles from:")
message(OUTPUT_RGBA)
