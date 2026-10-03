# ==============================================================================
# Container B - production-shaped raster delivery build
#
# Supported stages:
#   rgba     - canonical S3 COG -> transient RGBA GeoTIFF
#   tiles    - existing RGBA -> transient XYZ PNG pyramid
#   pmtiles  - existing XYZ PNG pyramid -> transient/local PMTiles
#   publish  - upload PMTiles + render.json + _SUCCESS markers
#   build    - COG -> RGBA -> XYZ -> PMTiles, no publication
#   all      - COG -> RGBA -> XYZ -> PMTiles -> S3 publication
#
# In ECS/Fargate, /work is task-local ephemeral storage and disappears when the
# task exits. Only COG (from Stage 01), PMTiles, metadata, and success markers
# are durable in S3.
# ==============================================================================

FROM rocker/r2u:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC

# ------------------------------------------------------------------------------
# GDAL / Python GDAL / geospatial runtime
# ------------------------------------------------------------------------------

RUN apt-get update && apt-get install -y --no-install-recommends \
    gdal-bin \
    python3-gdal \
    python3-venv \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    ca-certificates \
    curl \
    bash \
    && rm -rf /var/lib/apt/lists/*

# ------------------------------------------------------------------------------
# R packages
# ------------------------------------------------------------------------------

RUN apt-get update && apt-get install -y --no-install-recommends \
    r-cran-jsonlite \
    r-cran-terra \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e \
    "options(repos=c(CRAN='https://cloud.r-project.org')); install.packages('paws')"

# ------------------------------------------------------------------------------
# PMTiles Python package
# ------------------------------------------------------------------------------

RUN python3 -m venv --system-site-packages /opt/pmtiles-venv \
    && /opt/pmtiles-venv/bin/pip install --no-cache-dir --upgrade pip \
    && /opt/pmtiles-venv/bin/pip install --no-cache-dir pmtiles==3.8.1

ENV PATH="/opt/pmtiles-venv/bin:${PATH}"

# ------------------------------------------------------------------------------
# Runtime layout
# ------------------------------------------------------------------------------

ENV APP_HOME=/app
ENV WORK_DIR=/work
ENV AWS_REGION=us-east-2

ENV RENDER_CONFIG_BUCKET=stg4-24hr-large-area-render
ENV RENDER_CONFIG_KEY=CONUS_subset/config/render/large_area/render_config.json

WORKDIR ${APP_HOME}

RUN mkdir -p \
    ${APP_HOME}/scripts \
    ${WORK_DIR}

COPY 02_cog_to_rgba_cycle.R \
     ${APP_HOME}/scripts/02_cog_to_rgba_cycle.R

COPY 03_rgba_to_xyz_tiles_cycle.R \
     ${APP_HOME}/scripts/03_rgba_to_xyz_tiles_cycle.R

COPY 04_xyz_to_pmtiles_cycle.py \
     ${APP_HOME}/scripts/04_xyz_to_pmtiles_cycle.py

COPY 05_publish_render_cycle.R \
     ${APP_HOME}/scripts/05_publish_render_cycle.R

COPY entrypoint.sh \
     ${APP_HOME}/entrypoint.sh

RUN chmod +x \
    ${APP_HOME}/entrypoint.sh \
    ${APP_HOME}/scripts/04_xyz_to_pmtiles_cycle.py

ENTRYPOINT ["/app/entrypoint.sh"]
