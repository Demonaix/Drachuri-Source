@echo off
setlocal
set "DRACHURI_INSTALL_DIR=%~dp0"
set "DRACHURI_APP_DIR=%~dp0app"
set "R_HOME=%~dp0runtime\R"
set "R_LIBS_USER=%~dp0library"
set "RENV_CONFIG_AUTOLOADER_ENABLED=FALSE"
set "R_USER=%LOCALAPPDATA%\Drachuri Player\R-user"
if not exist "%R_USER%" mkdir "%R_USER%"
set "DRACHURI_DATA_DIR=%LOCALAPPDATA%\Drachuri Player"
if not exist "%DRACHURI_DATA_DIR%\storyboards" mkdir "%DRACHURI_DATA_DIR%\storyboards"
set "DND_LAUNCH_BROWSER=true"
set "R_SCRIPT=%R_HOME%\bin\Rscript.exe"
if not exist "%R_SCRIPT%" set "R_SCRIPT=%R_HOME%\bin\x64\Rscript.exe"
if not exist "%R_SCRIPT%" (
  msg * "Drachuri Player could not start because its private R runtime is missing. Please reinstall Drachuri Player."
  exit /b 1
)
set "DRACHURI_APP_VERSION=Unknown version"
if exist "%~dp0VERSION" set /p DRACHURI_APP_VERSION=<"%~dp0VERSION"
"%R_SCRIPT%" --vanilla "%~dp0check_for_update.R" player windows "%DRACHURI_APP_VERSION%" "%~dp0GITHUB_REPOSITORY"
cd /d "%DRACHURI_APP_DIR%"
"%R_SCRIPT%" --vanilla "%DRACHURI_INSTALL_DIR%installed_run.R"
endlocal
