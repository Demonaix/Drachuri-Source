param([switch]$KeepBuild)
$ErrorActionPreference = "Stop"

$InstallerDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$Root = (Resolve-Path (Join-Path $InstallerDir "..\..")).Path
$PlayerSource = Join-Path $Root "DND APP Drachuri Edition Player_v2"
$Build = Join-Path $InstallerDir "build"
$Version = (Get-Content (Join-Path $Root "VERSION") -Raw).Trim()
$RVersion = "4.6.1"
$RInstaller = Join-Path $env:TEMP "R-$RVersion-win.exe"
$RUrl = "https://cran.r-project.org/bin/windows/base/R-$RVersion-win.exe"
$MinimumRInstallerBytes = 80MB

Write-Host "Building Drachuri Player $Version installer" -ForegroundColor Cyan
if (Test-Path $Build) { Remove-Item $Build -Recurse -Force }
New-Item -ItemType Directory -Force -Path (Join-Path $Build "app"), (Join-Path $Build "runtime"), (Join-Path $Build "library") | Out-Null

Write-Host "Copying the player application..."
$excludeDirs = @(".Rproj.user", "library", "cache", "bootstrap-library", "logs", "renv_new", "rsconnect", "models")
$excludeFiles = @(".RData", ".Rhistory", ".DS_Store", ".restored-lock-md5")
$robocopyArgs = @($PlayerSource, (Join-Path $Build "app"), "/E", "/NFL", "/NDL", "/NJH", "/NJS", "/NP", "/XD") + $excludeDirs + @("/XF") + $excludeFiles
& robocopy @robocopyArgs | Out-Null
if ($LASTEXITCODE -ge 8) { throw "Application copy failed (robocopy exit $LASTEXITCODE)." }

if ((Test-Path $RInstaller) -and ((Get-Item $RInstaller).Length -lt $MinimumRInstallerBytes)) {
  Write-Host "Removing an incomplete cached R runtime download..." -ForegroundColor Yellow
  Remove-Item $RInstaller -Force
}
if (!(Test-Path $RInstaller)) {
  Write-Host "Downloading the official R $RVersion Windows runtime..."
  Invoke-WebRequest -Uri $RUrl -OutFile $RInstaller
}
$Runtime = Join-Path $Build "runtime\R"
$RuntimeInstallLog = Join-Path $Build "r-runtime-install.log"
$RuntimeInstallArgs = "/SP- /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /CURRENTUSER /DIR=`"$Runtime`" /LOG=`"$RuntimeInstallLog`""
$RuntimeInstalled = $false
for ($attempt = 1; $attempt -le 2; $attempt++) {
  Write-Host "Extracting the private R runtime (attempt $attempt of 2)..."
  if (Test-Path $Runtime) { Remove-Item $Runtime -Recurse -Force }
  if (Test-Path $RuntimeInstallLog) { Remove-Item $RuntimeInstallLog -Force }
  $process = Start-Process -FilePath $RInstaller -ArgumentList $RuntimeInstallArgs -Wait -PassThru
  if ($process.ExitCode -eq 0) {
    $RuntimeInstalled = $true
    break
  }
  if ($attempt -eq 1) {
    Write-Host "R setup returned exit code $($process.ExitCode). Downloading a fresh copy before retrying..." -ForegroundColor Yellow
    Remove-Item $RInstaller -Force
    Invoke-WebRequest -Uri $RUrl -OutFile $RInstaller
  }
}
if (!$RuntimeInstalled) {
  $LogTail = if (Test-Path $RuntimeInstallLog) { (Get-Content $RuntimeInstallLog -Tail 30) -join [Environment]::NewLine } else { "No R setup log was produced." }
  throw "R runtime installer failed with exit code $($process.ExitCode). Log: $RuntimeInstallLog`n$LogTail"
}

$env:DRACHURI_BUILD_APP = Join-Path $Build "app"
$env:DRACHURI_BUILD_LIBRARY = Join-Path $Build "library"
$env:R_LIBS_USER = $env:DRACHURI_BUILD_LIBRARY
$env:RENV_CONFIG_AUTOLOADER_ENABLED = "FALSE"
$RScript = @(
  (Join-Path $Runtime "bin\Rscript.exe"),
  (Join-Path $Runtime "bin\x64\Rscript.exe")
) | Where-Object { Test-Path $_ } | Select-Object -First 1
if (!$RScript) { throw "R installed successfully, but Rscript.exe was not found beneath $Runtime." }
Write-Host "Installing the locked Windows package library (first build may take a while)..."
& $RScript --vanilla (Join-Path $InstallerDir "restore_packages.R")
if ($LASTEXITCODE -ne 0) { throw "R package restore failed." }

$IsccCandidates = @(
  "$env:LOCALAPPDATA\Programs\Inno Setup 6\ISCC.exe",
  "${env:ProgramFiles(x86)}\Inno Setup 6\ISCC.exe",
  "$env:ProgramFiles\Inno Setup 6\ISCC.exe"
)
$ISCC = $IsccCandidates | Where-Object { Test-Path $_ } | Select-Object -First 1
if (!$ISCC) {
  if (!(Get-Command winget -ErrorAction SilentlyContinue)) { throw "Install Inno Setup 6, then run this builder again." }
  Write-Host "Installing Inno Setup compiler..."
  & winget install --id JRSoftware.InnoSetup -e --accept-package-agreements --accept-source-agreements
  if ($LASTEXITCODE -ne 0) { throw "Inno Setup installation failed." }
  $ISCC = $IsccCandidates | Where-Object { Test-Path $_ } | Select-Object -First 1
  if (!$ISCC) { throw "Inno Setup installed but ISCC.exe could not be found." }
}

Write-Host "Compiling the Windows installer..."
& $ISCC "-DAppVersion=$Version" (Join-Path $InstallerDir "DrachuriPlayer.iss")
if ($LASTEXITCODE -ne 0) { throw "Inno Setup compilation failed." }
$Output = Join-Path $Root "releases\Drachuri-Player-Setup-$Version.exe"
if (!(Test-Path $Output)) { throw "Expected installer was not created: $Output" }
Write-Host "Installer ready:" -ForegroundColor Green
Write-Host $Output
if (!$KeepBuild) { Remove-Item $Build -Recurse -Force }
