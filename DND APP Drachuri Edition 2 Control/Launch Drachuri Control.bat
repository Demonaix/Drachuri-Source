@echo off
setlocal
cd /d "%~dp0"
title Drachuri Control

set "RSCRIPT="
for /f "delims=" %%I in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%I"

if not defined RSCRIPT (
  for /f "usebackq delims=" %%I in (`powershell -NoProfile -Command "$p = Get-ChildItem 'C:\Program Files\R' -Filter Rscript.exe -Recurse -ErrorAction SilentlyContinue ^| Sort-Object FullName -Descending ^| Select-Object -First 1 -ExpandProperty FullName; if ($p) { $p }"`) do set "RSCRIPT=%%I"
)

if not defined RSCRIPT (
  echo.
  echo R could not be found on this computer.
  echo Install R from https://cran.r-project.org/bin/windows/base/ and try again.
  echo RStudio is not required.
  echo.
  pause
  exit /b 1
)

echo Starting Drachuri Control...
"%RSCRIPT%" --vanilla "launcher\bootstrap_and_run.R"

if errorlevel 1 (
  echo.
  echo Drachuri Control did not start. See launcher\logs\launcher.log
  pause
  exit /b 1
)

endlocal

