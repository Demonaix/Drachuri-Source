@echo off
setlocal
cd /d "%~dp0"
title Build and Publish Drachuri Windows

where git >nul 2>nul
if errorlevel 1 (
  echo Git for Windows is required. Install it from https://git-scm.com/download/win
  pause
  exit /b 1
)

for /f %%i in ('git status --porcelain') do (
  echo This source folder has local changes. Nothing was pulled or published.
  echo Commit or discard the PC changes first; normal development should happen on the Mac.
  pause
  exit /b 1
)

echo Pulling the latest Drachuri source from the private repository...
git pull --ff-only
if errorlevel 1 goto :failed

where gh >nul 2>nul
if errorlevel 1 (
  echo Installing GitHub CLI...
  winget install --id GitHub.cli -e --accept-package-agreements --accept-source-agreements
  if errorlevel 1 goto :failed
  echo GitHub CLI was installed. Close this window, then run this command again.
  pause
  exit /b 0
)

gh auth status >nul 2>nul
if errorlevel 1 (
  echo Sign in to GitHub in the browser window that opens.
  gh auth login --web --git-protocol https
  if errorlevel 1 goto :failed
)

powershell.exe -NoProfile -ExecutionPolicy Bypass -File "installer\player\publish_windows_installer.ps1"
if errorlevel 1 goto :failed

echo.
echo WINDOWS RELEASE COMPLETE.
echo Players can now install or update to the Windows version shown above.
pause
exit /b 0

:failed
echo.
echo WINDOWS RELEASE FAILED. Read the error above; no false update was advertised.
pause
exit /b 1
