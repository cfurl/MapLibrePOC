#!/usr/bin/env python3
# ==============================================================================
# 04_xyz_to_pmtiles_cycle.py
#
# Purpose:
#   Package the transient XYZ PNG tile pyramid into one raster PMTiles archive.
#
# Runtime:
#   python3 04_xyz_to_pmtiles_cycle.py \
#     --area texas \
#     --cycle 2026100312
#
# Inputs:
#   /work/<area>/<cycle>/tiles/{z}/{x}/{y}.png
#   /work/<area>/<cycle>/rgba/stage4_daily_rgba.tif
#     - used only to derive accurate geographic bounds for PMTiles metadata
#
# Output:
#   /work/<area>/<cycle>/pmtiles/stage4_daily.pmtiles
#
# Config:
#   Uses the render_config.json already downloaded under /work/config.
#   This stage is intentionally local-only for the first test:
#     - no S3 upload
#     - no render _SUCCESS
#     - no manifest publication
# ==============================================================================

from __future__ import annotations

import argparse
import json
import os
import shutil
import sys
import tempfile
from pathlib import Path

from osgeo import gdal, osr
from pmtiles.convert import disk_to_pmtiles
from pmtiles.reader import Reader, MmapSource
from pmtiles.tile import TileType


def deep_merge(base: dict | None, override: dict | None) -> dict:
    out = dict(base or {})
    for key, value in (override or {}).items():
        if isinstance(value, dict) and isinstance(out.get(key), dict):
            out[key] = deep_merge(out[key], value)
        else:
            out[key] = value
    return out


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Package Stage IV XYZ PNG tiles into a PMTiles archive."
    )

    parser.add_argument(
        "--area",
        default=os.environ.get("AREA_ID", ""),
        help="Area id, e.g. texas or serfc.",
    )

    parser.add_argument(
        "--cycle",
        default=os.environ.get("CYCLE_ID", ""),
        help="Cycle YYYYMMDDHH, e.g. 2026100312.",
    )

    parser.add_argument(
        "--work-dir",
        default=os.environ.get("WORK_DIR", "/work"),
        help="Container work directory.",
    )

    parser.add_argument(
        "--config",
        default=os.environ.get("RENDER_CONFIG_LOCAL", ""),
        help="Optional local render_config.json path.",
    )

    return parser.parse_args()


def validate_cycle(cycle: str) -> None:
    if len(cycle) != 10 or not cycle.isdigit():
        raise RuntimeError(
            f"Invalid --cycle value: {cycle!r}. Expected YYYYMMDDHH."
        )


def locate_config(work_dir: Path, explicit_config: str) -> Path:
    if explicit_config:
        path = Path(explicit_config)
    else:
        path = work_dir / "config" / "render_config.json"

    if not path.is_file():
        raise RuntimeError(
            "render_config.json was not found locally:\n"
            f"{path}\n\n"
            "Run Stage 02/03 first or supply --config."
        )

    return path


def raster_bounds_wgs84(raster_path: Path) -> tuple[float, float, float, float]:
    ds = gdal.Open(str(raster_path), gdal.GA_ReadOnly)

    if ds is None:
        raise RuntimeError(f"GDAL could not open RGBA raster:\n{raster_path}")

    gt = ds.GetGeoTransform(can_return_null=True)
    src_srs = ds.GetSpatialRef()

    if gt is None or src_srs is None:
        raise RuntimeError(
            "Could not determine geotransform/CRS from RGBA raster."
        )

    width = ds.RasterXSize
    height = ds.RasterYSize

    def pixel_to_map(px: float, py: float) -> tuple[float, float]:
        x = gt[0] + px * gt[1] + py * gt[2]
        y = gt[3] + px * gt[4] + py * gt[5]
        return x, y

    corners_xy = [
        pixel_to_map(0, 0),
        pixel_to_map(width, 0),
        pixel_to_map(0, height),
        pixel_to_map(width, height),
    ]

    src_srs = src_srs.Clone()
    dst_srs = osr.SpatialReference()
    dst_srs.ImportFromEPSG(4326)

    # Keep lon/lat ordering predictable under GDAL 3+.
    src_srs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    dst_srs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)

    transform = osr.CoordinateTransformation(src_srs, dst_srs)

    lonlat = [
        transform.TransformPoint(x, y)
        for x, y in corners_xy
    ]

    lons = [p[0] for p in lonlat]
    lats = [p[1] for p in lonlat]

    bounds = (
        min(lons),
        min(lats),
        max(lons),
        max(lats),
    )

    ds = None

    return bounds


def write_metadata(
    metadata_dir: Path,
    area_id: str,
    cycle_id: str,
    min_zoom: int,
    max_zoom: int,
    bounds: tuple[float, float, float, float],
) -> Path:
    min_lon, min_lat, max_lon, max_lat = bounds
    center_lon = (min_lon + max_lon) / 2
    center_lat = (min_lat + max_lat) / 2

    metadata = {
        "name": f"Stage IV daily precipitation - {area_id} - {cycle_id}",
        "description": (
            f"Stage IV 24-hour daily precipitation raster tiles "
            f"for {area_id}, cycle {cycle_id}"
        ),
        "version": "1",
        "format": "png",
        "minzoom": min_zoom,
        "maxzoom": max_zoom,
        "bounds": f"{min_lon:.8f},{min_lat:.8f},{max_lon:.8f},{max_lat:.8f}",
        "center": f"{center_lon:.8f},{center_lat:.8f},{min_zoom}",
    }

    path = metadata_dir / "metadata.json"

    with path.open("w", encoding="utf-8") as f:
        json.dump(metadata, f, indent=2)
        f.write("\n")

    return path



def list_png_tiles(tile_dir: Path) -> list[Path]:
    return sorted(
        path
        for path in tile_dir.rglob("*.png")
        if path.is_file()
    )


def list_non_png_files_in_zoom_tree(
    tile_dir: Path,
    min_zoom: int,
    max_zoom: int,
) -> list[Path]:
    extras: list[Path] = []

    for z in range(min_zoom, max_zoom + 1):
        z_dir = tile_dir / str(z)

        if not z_dir.is_dir():
            continue

        for path in z_dir.rglob("*"):
            if path.is_file() and path.suffix.lower() != ".png":
                extras.append(path)

    return sorted(extras)


def make_clean_png_staging(
    source_tile_dir: Path,
    png_files: list[Path],
    area_id: str,
    cycle_id: str,
) -> Path:
    """
    Build a temporary Z/X/Y tree containing PNG files only.

    The PMTiles disk converter walks every file inside numeric zoom/x
    directories. GDAL may leave sidecar files such as *.png.aux.xml, so
    packaging from a PNG-only staging tree prevents those files from being
    mistaken for additional tiles.
    """

    staging_root = Path(
        tempfile.mkdtemp(
            prefix=f"pmtiles_pack_{area_id}_{cycle_id}_"
        )
    )

    for source in png_files:
        rel = source.relative_to(source_tile_dir)
        target = staging_root / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)

    return staging_root

def main() -> int:
    args = parse_args()

    if not args.area:
        raise RuntimeError("Missing --area.")

    if not args.cycle:
        raise RuntimeError("Missing --cycle.")

    validate_cycle(args.cycle)

    area_id = args.area
    cycle_id = args.cycle
    work_dir = Path(args.work_dir)

    config_path = locate_config(
        work_dir,
        args.config,
    )

    with config_path.open("r", encoding="utf-8") as f:
        config = json.load(f)

    areas = config.get("areas", {})

    if area_id not in areas:
        raise RuntimeError(
            f"Area is not present in render config: {area_id}"
        )

    area_cfg = areas[area_id]

    if area_cfg.get("enabled") is False:
        raise RuntimeError(
            f"Area is disabled in render config: {area_id}"
        )

    defaults = config.get("defaults", {})

    tiles_cfg = deep_merge(
        defaults.get("tiles", {}),
        area_cfg.get("tiles", {}),
    )

    pmtiles_cfg = deep_merge(
        defaults.get("pmtiles", {}),
        area_cfg.get("pmtiles", {}),
    )

    min_zoom = int(tiles_cfg.get("min_zoom", 4))
    max_zoom = int(tiles_cfg.get("max_zoom", 9))
    scheme = tiles_cfg.get("scheme", "xyz")
    tile_type = pmtiles_cfg.get("tile_type", "png")

    if scheme != "xyz":
        raise RuntimeError(
            "Stage 04 currently requires tiles.scheme = 'xyz'. "
            f"Found: {scheme}"
        )

    if tile_type != "png":
        raise RuntimeError(
            "Stage 04 currently requires pmtiles.tile_type = 'png'. "
            f"Found: {tile_type}"
        )

    run_root = work_dir / area_id / cycle_id
    tile_dir = run_root / "tiles"
    rgba_path = run_root / "rgba" / "stage4_daily_rgba.tif"
    pmtiles_dir = run_root / "pmtiles"
    output_pmtiles = pmtiles_dir / "stage4_daily.pmtiles"

    if not tile_dir.is_dir():
        raise RuntimeError(
            "XYZ tile directory not found:\n"
            f"{tile_dir}\n\n"
            "Run Stage 03 first."
        )

    if not rgba_path.is_file():
        raise RuntimeError(
            "RGBA raster not found:\n"
            f"{rgba_path}\n\n"
            "Stage 04 uses it to derive PMTiles bounds."
        )

    expected_zoom_dirs = [
        tile_dir / str(z)
        for z in range(min_zoom, max_zoom + 1)
    ]

    missing_zoom_dirs = [
        str(path)
        for path in expected_zoom_dirs
        if not path.is_dir()
    ]

    if missing_zoom_dirs:
        raise RuntimeError(
            "Missing expected tile zoom directories:\n"
            + "\n".join(missing_zoom_dirs)
        )

    png_files = list_png_tiles(tile_dir)
    tile_count = len(png_files)

    if tile_count < 1:
        raise RuntimeError(
            f"No PNG tiles found under:\n{tile_dir}"
        )

    extra_files = list_non_png_files_in_zoom_tree(
        tile_dir=tile_dir,
        min_zoom=min_zoom,
        max_zoom=max_zoom,
    )

    bounds = raster_bounds_wgs84(rgba_path)

    pmtiles_dir.mkdir(
        parents=True,
        exist_ok=True,
    )

    if output_pmtiles.exists():
        output_pmtiles.unlink()

    staging_dir = make_clean_png_staging(
        source_tile_dir=tile_dir,
        png_files=png_files,
        area_id=area_id,
        cycle_id=cycle_id,
    )

    metadata_path = write_metadata(
        metadata_dir=staging_dir,
        area_id=area_id,
        cycle_id=cycle_id,
        min_zoom=min_zoom,
        max_zoom=max_zoom,
        bounds=bounds,
    )

    print("")
    print("==============================================================")
    print("04 XYZ -> PMTILES")
    print("==============================================================")
    print(f"Area:          {area_id}")
    print(f"Cycle:         {cycle_id}")
    print(f"Config:        {config_path}")
    print(f"XYZ tiles:     {tile_dir}")
    print(f"RGBA bounds:   {rgba_path}")
    print(f"Pack staging:  {staging_dir}")
    print(f"Metadata:      {metadata_path}")
    print(f"PMTiles:       {output_pmtiles}")
    print(f"Zooms:         {min_zoom}-{max_zoom}")
    print(f"Tile type:     {tile_type}")
    print(f"Input PNGs:    {tile_count:,}")
    print(f"Non-PNG sidecars ignored: {len(extra_files):,}")
    print(
        "Bounds:        "
        f"{bounds[0]:.6f}, {bounds[1]:.6f}, "
        f"{bounds[2]:.6f}, {bounds[3]:.6f}"
    )

    print("")
    print("==============================================================")
    print("PACKING PMTILES")
    print("==============================================================")
    print("")

    # Pack from the clean PNG-only staging tree.  This avoids GDAL sidecars
    # being interpreted as duplicate tile files by disk_to_pmtiles().
    try:
        disk_to_pmtiles(
            str(staging_dir),
            str(output_pmtiles),
            max_zoom,
            scheme="zxy",
            tile_format="png",
            verbose=True,
        )
    finally:
        shutil.rmtree(
            staging_dir,
            ignore_errors=True,
        )

    if not output_pmtiles.is_file():
        raise RuntimeError(
            "PMTiles archive was not created:\n"
            f"{output_pmtiles}"
        )

    if output_pmtiles.stat().st_size <= 0:
        raise RuntimeError(
            "PMTiles archive exists but has zero bytes."
        )

    # --------------------------------------------------------------------------
    # PMTiles structural QA
    # --------------------------------------------------------------------------

    with output_pmtiles.open("r+b") as f:
        reader = Reader(MmapSource(f))
        header = reader.header()
        metadata = reader.metadata()

    if header["version"] != 3:
        raise RuntimeError(
            f"Expected PMTiles spec version 3; found {header['version']}."
        )

    if header["tile_type"] != TileType.PNG:
        raise RuntimeError(
            "Expected PNG PMTiles tile type; "
            f"found {header['tile_type']}."
        )

    if header["min_zoom"] != min_zoom:
        raise RuntimeError(
            "PMTiles min zoom mismatch: "
            f"expected {min_zoom}, found {header['min_zoom']}."
        )

    if header["max_zoom"] != max_zoom:
        raise RuntimeError(
            "PMTiles max zoom mismatch: "
            f"expected {max_zoom}, found {header['max_zoom']}."
        )

    if header["addressed_tiles_count"] != tile_count:
        raise RuntimeError(
            "PMTiles addressed tile count does not match input PNG count: "
            f"{header['addressed_tiles_count']} vs {tile_count}."
        )

    if metadata.get("format") != "png":
        raise RuntimeError(
            "PMTiles metadata format is not png."
        )

    size_mb = output_pmtiles.stat().st_size / (1024 ** 2)

    print("")
    print("==============================================================")
    print("PMTILES QA")
    print("==============================================================")
    print(f"Spec version:       {header['version']}")
    print(f"Tile type:          {header['tile_type'].name}")
    print(f"Min zoom:           {header['min_zoom']}")
    print(f"Max zoom:           {header['max_zoom']}")
    print(f"Addressed tiles:    {header['addressed_tiles_count']:,}")
    print(f"Tile entries:       {header['tile_entries_count']:,}")
    print(f"Tile contents:      {header['tile_contents_count']:,}")
    print(f"Clustered:          {header['clustered']}")
    print(f"File size:          {size_mb:.2f} MB")

    print("")
    print("==============================================================")
    print("DONE")
    print("==============================================================")
    print(f"Area:       {area_id}")
    print(f"Cycle:      {cycle_id}")
    print(f"PMTiles:    {output_pmtiles}")
    print("")
    print(
        "Stage 04 local PMTiles complete. "
        "No S3 upload, render _SUCCESS, or manifest was written."
    )

    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        raise
