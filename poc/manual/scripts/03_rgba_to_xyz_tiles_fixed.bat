@echo off
setlocal EnableExtensions

REM =============================================================================
REM 03_rgba_to_xyz_tiles_fixed.bat
REM =============================================================================

REM -----------------------------------------------------------------------------
REM CONFIG
REM -----------------------------------------------------------------------------

set "DATE_ID=20260713"
set "ROOT=C:\stg4\MapLibrePOC\poc\manual"

set "MIN_ZOOM=4"
set "MAX_ZOOM=9"

REM Dev machine QGIS install.
REM Change this one line on another machine if needed.
set "QGIS_ROOT=C:\Program Files\QGIS 3.40.14"

REM -----------------------------------------------------------------------------
REM DERIVED PATHS
REM -----------------------------------------------------------------------------

set "INPUT_RGBA=%ROOT%\colored\%DATE_ID%\stage4_daily_rgba.tif"
set "OUTPUT_DIR=%ROOT%\tiles\%DATE_ID%"
set "GDAL2TILES=%QGIS_ROOT%\apps\Python312\Scripts\gdal2tiles.exe"

REM -----------------------------------------------------------------------------
REM INITIALIZE QGIS/GDAL ENVIRONMENT
REM -----------------------------------------------------------------------------

if not exist "%GDAL2TILES%" (
    echo.
    echo ERROR: gdal2tiles.exe not found:
    echo %GDAL2TILES%
    echo.
    exit /b 1
)

set "PATH=%QGIS_ROOT%\bin;%QGIS_ROOT%\apps\Python312;%QGIS_ROOT%\apps\Python312\Scripts;%PATH%"

if exist "%QGIS_ROOT%\apps\gdal\share\gdal" (
    set "GDAL_DATA=%QGIS_ROOT%\apps\gdal\share\gdal"
)

if exist "%QGIS_ROOT%\share\proj" (
    set "PROJ_LIB=%QGIS_ROOT%\share\proj"
)

if not exist "%INPUT_RGBA%" (
    echo.
    echo ERROR: Input RGBA TIFF not found:
    echo %INPUT_RGBA%
    echo.
    exit /b 1
)

echo.
echo ==============================================================
echo CONFIG
echo ==============================================================
echo Date:        %DATE_ID%
echo Input:       %INPUT_RGBA%
echo Output:      %OUTPUT_DIR%
echo Zooms:       %MIN_ZOOM%-%MAX_ZOOM%
echo QGIS root:   %QGIS_ROOT%
echo GDAL2Tiles:  %GDAL2TILES%
echo ==============================================================

echo.
echo GDAL version:
"%GDAL2TILES%" --version

if errorlevel 1 (
    echo.
    echo ERROR: gdal2tiles did not start correctly.
    echo.
    exit /b 1
)

if exist "%OUTPUT_DIR%" (
    echo.
    echo Removing existing tile directory:
    echo %OUTPUT_DIR%
    rmdir /s /q "%OUTPUT_DIR%"
)

mkdir "%OUTPUT_DIR%"

echo.
echo ==============================================================
echo BUILDING XYZ TILES
echo ==============================================================
echo.

"%GDAL2TILES%" ^
  --xyz ^
  --zoom=%MIN_ZOOM%-%MAX_ZOOM% ^
  --resampling=bilinear ^
  --webviewer=none ^
  "%INPUT_RGBA%" ^
  "%OUTPUT_DIR%"

if errorlevel 1 (
    echo.
    echo ERROR: gdal2tiles failed.
    echo.
    exit /b 1
)

echo.
echo ==============================================================
echo DONE
echo ==============================================================
echo XYZ tiles:
echo %OUTPUT_DIR%\{z}\{x}\{y}.png
echo ==============================================================

endlocal
