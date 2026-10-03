#!/usr/bin/env bash
set -euo pipefail

STAGE="${1:-}"

if [[ -z "${STAGE}" ]]; then
  echo "ERROR: first argument must be one of: rgba, tiles, pmtiles, publish, build, all"
  exit 2
fi

shift

case "${STAGE}" in
  rgba)
    exec Rscript /app/scripts/02_cog_to_rgba_cycle.R "$@"
    ;;

  tiles)
    exec Rscript /app/scripts/03_rgba_to_xyz_tiles_cycle.R "$@"
    ;;

  pmtiles)
    exec python3 /app/scripts/04_xyz_to_pmtiles_cycle.py "$@"
    ;;

  publish)
    exec Rscript /app/scripts/05_publish_render_cycle.R "$@"
    ;;

  build)
    Rscript /app/scripts/02_cog_to_rgba_cycle.R "$@"
    Rscript /app/scripts/03_rgba_to_xyz_tiles_cycle.R "$@"
    python3 /app/scripts/04_xyz_to_pmtiles_cycle.py "$@"
    ;;

  all)
    Rscript /app/scripts/02_cog_to_rgba_cycle.R "$@"
    Rscript /app/scripts/03_rgba_to_xyz_tiles_cycle.R "$@"
    python3 /app/scripts/04_xyz_to_pmtiles_cycle.py "$@"
    Rscript /app/scripts/05_publish_render_cycle.R "$@"
    ;;

  *)
    echo "ERROR: unknown stage '${STAGE}'. Expected: rgba, tiles, pmtiles, publish, build, all"
    exit 2
    ;;
esac
