$ErrorActionPreference = "Stop"

$InstallerDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$Root = (Resolve-Path (Join-Path $InstallerDir "..\..")).Path
$Version = (Get-Content (Join-Path $Root "VERSION") -Raw).Trim()
$Repository = (Get-Content (Join-Path $Root "distribution\GITHUB_REPOSITORY") -Raw).Trim()
$Tag = "v$Version"
$Exe = Join-Path $Root "releases\Drachuri-Player-Setup-$Version.exe"
$Manifest = Join-Path $Root "releases\drachuri-update.json"
$DownloadDir = Join-Path $env:TEMP "drachuri-release-$Version"

if ($Repository -notmatch '^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$') { throw "Invalid release repository: $Repository" }

Write-Host "Building the Windows player installer..." -ForegroundColor Cyan
& powershell.exe -NoProfile -ExecutionPolicy Bypass -File (Join-Path $InstallerDir "build_player_installer.ps1")
if ($LASTEXITCODE -ne 0 -or !(Test-Path $Exe)) { throw "The Windows installer build failed." }

Write-Host "Checking public release $Tag..."
& gh release view $Tag --repo $Repository | Out-Null
if ($LASTEXITCODE -ne 0) { throw "GitHub release $Tag does not exist. Publish the matching Mac release first." }

if (Test-Path $DownloadDir) { Remove-Item $DownloadDir -Recurse -Force }
New-Item -ItemType Directory -Force -Path $DownloadDir | Out-Null
& gh release download $Tag --repo $Repository --pattern "drachuri-update.json" --dir $DownloadDir --clobber
if ($LASTEXITCODE -ne 0) { throw "Could not download the existing update manifest." }

$DownloadedManifest = Join-Path $DownloadDir "drachuri-update.json"
$Data = Get-Content $DownloadedManifest -Raw | ConvertFrom-Json
if ([string]$Data.version -ne $Version) { throw "Manifest version $($Data.version) does not match $Version." }

$Hash = (Get-FileHash -Algorithm SHA256 $Exe).Hash.ToLowerInvariant()
$FileName = Split-Path -Leaf $Exe
$WindowsAsset = [ordered]@{
  url = "https://github.com/$Repository/releases/download/$Tag/$FileName"
  sha256 = $Hash
}
$Data.products.player.windows = $WindowsAsset
$Json = $Data | ConvertTo-Json -Depth 20 -Compress
[System.IO.File]::WriteAllText($Manifest, $Json, (New-Object System.Text.UTF8Encoding($false)))

Write-Host "Uploading Windows installer and updated cross-platform manifest..." -ForegroundColor Cyan
& gh release upload $Tag $Exe $Manifest --repo $Repository --clobber
if ($LASTEXITCODE -ne 0) { throw "GitHub upload failed." }

Remove-Item $DownloadDir -Recurse -Force
Write-Host "Published Drachuri Player Windows $Version" -ForegroundColor Green
Write-Host "https://github.com/$Repository/releases/tag/$Tag"
