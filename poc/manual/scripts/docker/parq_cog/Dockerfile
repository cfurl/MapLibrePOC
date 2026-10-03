# ==============================================================================
# Stage 01 Docker image
#   canonical daily parquet -> smoothed COG
#
# Runtime:
#   docker run ... stg4-render-stage01:latest \
#     --area texas \
#     --cycle 2026100212
#
# In ECS:
#   AREA_ID / CYCLE_ID may alternatively be supplied as environment variables.
#   AWS credentials come from the ECS task role.
# ==============================================================================

FROM rocker/r2u:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC

# ------------------------------------------------------------------------------
# GDAL / PROJ / geospatial system dependencies
# ------------------------------------------------------------------------------

RUN apt-get update && apt-get install -y --no-install-recommends \
    gdal-bin \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    ca-certificates \
    curl \
    && rm -rf /var/lib/apt/lists/*

# ------------------------------------------------------------------------------
# R packages available through r2u/apt
# ------------------------------------------------------------------------------

RUN apt-get update && apt-get install -y --no-install-recommends \
    r-cran-dplyr \
    r-cran-jsonlite \
    r-cran-sf \
    r-cran-terra \
    r-cran-arrow \
    && rm -rf /var/lib/apt/lists/*

# ------------------------------------------------------------------------------
# AWS SDK for R
# ------------------------------------------------------------------------------

RUN Rscript -e \
    "options(repos=c(CRAN='https://cloud.r-project.org')); install.packages('paws')"

# ------------------------------------------------------------------------------
# Runtime layout
# ------------------------------------------------------------------------------

ENV APP_HOME=/app
ENV WORK_DIR=/work
ENV AWS_REGION=us-east-2
ENV RENDER_CONFIG_BUCKET=stg4-24hr-large-area-render
ENV RENDER_CONFIG_KEY=CONUS_subset/config/render/large_area/render_config.json

WORKDIR ${APP_HOME}

RUN mkdir -p ${APP_HOME}/scripts ${WORK_DIR}

# The file in the build folder is named exactly:
#   01_parquet_to_cog_cycle.R
COPY 01_parquet_to_cog_cycle.R /app/scripts/01_parquet_to_cog_cycle.R

ENTRYPOINT ["Rscript", "/app/scripts/01_parquet_to_cog_cycle.R"]
