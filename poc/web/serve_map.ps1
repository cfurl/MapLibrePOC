# Run this file from PowerShell.
# It serves the entire POC folder so /web can access /pmt.

$PocRoot = "C:\Users\cfurl\OneDrive - Edwards Aquifer Authority\r\MapLibrePOC\poc"

Set-Location $PocRoot

Write-Host ""
Write-Host "Serving MapLibre POC from:"
Write-Host $PocRoot
Write-Host ""
Write-Host "Open:"
Write-Host "http://localhost:8000/web/"
Write-Host ""
Write-Host "Press Ctrl+C to stop the server."
Write-Host ""

py -m http.server 8000
