@echo off
setlocal
cd /d "%~dp0"
title Build Drachuri Player Installer
echo This builds the self-contained Windows installer.
echo The first build downloads R and Windows packages and may take some time.
echo.
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "installer\player\build_player_installer.ps1"
if errorlevel 1 (
  echo.
  echo BUILD FAILED. Read the error above.
  pause
  exit /b 1
)
echo.
echo BUILD COMPLETE. The installer is in the releases folder.
pause
endlocal
