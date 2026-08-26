@echo off
setlocal
set "DRACHURI_INSTALL_DIR=%~dp0"
set "DRACHURI_APP_DIR=%~dp0app"
set "R_HOME=%~dp0runtime\R"
set "R_LIBS_USER=%~dp0library"
set "RENV_CONFIG_AUTOLOADER_ENABLED=FALSE"
set "R_USER=%LOCALAPPDATA%\Drachuri Player\R-user"
if not exist "%R_USER%" mkdir "%R_USER%"
set "DRACHURI_APP_VERSION=Unknown version"
if exist "%~dp0VERSION" set /p DRACHURI_APP_VERSION=<"%~dp0VERSION"
"%R_HOME%\bin\x64\Rscript.exe" --vanilla "%~dp0check_for_update.R" player windows "%DRACHURI_APP_VERSION%" "%~dp0GITHUB_REPOSITORY"
cd /d "%DRACHURI_APP_DIR%"
"%R_HOME%\bin\x64\Rscript.exe" --vanilla "%DRACHURI_INSTALL_DIR%installed_run.R"
endlocal
