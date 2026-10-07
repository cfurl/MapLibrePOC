FROM rocker/r-ver:4.4.2

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      pkg-config \
      ca-certificates \
 && rm -rf /var/lib/apt/lists/*

RUN R -q -e "install.packages(c('jsonlite','paws.storage'), repos='https://cloud.r-project.org')" \
 && R -q -e "stopifnot(requireNamespace('jsonlite', quietly=TRUE)); stopifnot(requireNamespace('paws.storage', quietly=TRUE))"

WORKDIR /app

COPY build_radar_frames_manifest.R /app/build_radar_frames_manifest.R

ENTRYPOINT ["Rscript", "/app/build_radar_frames_manifest.R"]