@echo off
setlocal
set "DRACHURI_INSTALL_DIR=%~dp0"
set "DRACHURI_APP_DIR=%~dp0app"
set "R_HOME=%~dp0runtime\R"
set "R_LIBS_USER=%~dp0library"
set "RENV_CONFIG_AUTOLOADER_ENABLED=FALSE"
set "R_USER=%LOCALAPPDATA%\Drachuri Player\R-user"
if not exist "%R_USER%" mkdir "%R_USER%"
cd /d "%DRACHURI_APP_DIR%"
"%R_HOME%\bin\x64\Rscript.exe" --vanilla "%DRACHURI_INSTALL_DIR%installed_run.R"
endlocal
