#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
version=""
repository=""
notes="Drachuri improvements and fixes."
publish=true
gh_bin=${GH_BIN:-}
[ -n "$gh_bin" ] || gh_bin="$root/.local-tools/gh_2.98.0_macOS_arm64/bin/gh"
[ -x "$gh_bin" ] || gh_bin=$(command -v gh 2>/dev/null || true)

usage() {
  echo "Usage: scripts/release_drachuri.sh --version 0.4.2 --repository owner/repository [--notes 'What changed'] [--build-only]"
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --version) version=${2:-}; shift 2 ;;
    --repository) repository=${2:-}; shift 2 ;;
    --notes) notes=${2:-}; shift 2 ;;
    --build-only) publish=false; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
done

[ -n "$version" ] || { echo "--version is required." >&2; exit 2; }
printf '%s' "$version" | grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+([._-][A-Za-z0-9.-]+)?$' || { echo "Invalid version: $version" >&2; exit 2; }
if [ -z "$repository" ] && [ -f "$root/distribution/GITHUB_REPOSITORY" ]; then repository=$(tr -d '\r\n' < "$root/distribution/GITHUB_REPOSITORY"); fi
printf '%s' "$repository" | grep -Eq '^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$' || { echo "Use --repository owner/repository the first time." >&2; exit 2; }

if $publish; then
  [ -x "$gh_bin" ] || { echo "GitHub CLI is missing. See distribution/README.md." >&2; exit 1; }
fi

printf '%s\n' "$version" > "$root/VERSION"
printf '%s\n' "$repository" > "$root/distribution/GITHUB_REPOSITORY"

cd "$root"
echo "Testing Drachuri $version..."
Rscript tests/run_tests.R
echo "Building macOS installers..."
sh installer/mac/build_mac_installer.sh
sh installer/control/mac/build_mac_control_installer.sh

player_mac=$(find "$root/releases" -maxdepth 1 \( -name "Drachuri-Player-Mac-$version.pkg" -o -name "Drachuri-Player-Mac-$version.dmg" \) -print | head -1)
control_mac=$(find "$root/releases" -maxdepth 1 \( -name "Drachuri-Control-Mac-$version.pkg" -o -name "Drachuri-Control-Mac-$version.dmg" \) -print | head -1)
player_windows="$root/releases/Drachuri-Player-Setup-$version.exe"
[ -f "$player_mac" ] || { echo "Player macOS installer is missing." >&2; exit 1; }
[ -f "$control_mac" ] || { echo "Control macOS installer is missing." >&2; exit 1; }

asset_json() {
  file=$1
  if [ ! -f "$file" ]; then printf 'null'; return; fi
  name=$(basename "$file")
  sha=$(openssl dgst -sha256 "$file" | awk '{print $NF}')
  printf '{"url":"https://github.com/%s/releases/download/v%s/%s","sha256":"%s"}' "$repository" "$version" "$name" "$sha"
}

manifest="$root/releases/drachuri-update.json"
player_mac_json=$(asset_json "$player_mac")
control_mac_json=$(asset_json "$control_mac")
player_windows_json=$(asset_json "$player_windows")
escaped_notes=$(printf '%s' "$notes" | Rscript --vanilla -e 'cat(jsonlite::toJSON(paste(readLines(file("stdin"), warn=FALSE), collapse="\n"), auto_unbox=TRUE))')
cat > "$manifest" <<EOF
{"schema_version":1,"version":"$version","notes":$escaped_notes,"products":{"player":{"mac":$player_mac_json,"windows":$player_windows_json},"control":{"mac":$control_mac_json,"windows":null}}}
EOF

echo "Manifest ready: $manifest"
if ! $publish; then echo "Build-only release complete."; exit 0; fi

set -- "$player_mac" "$control_mac" "$manifest"
[ ! -f "$player_windows" ] || set -- "$@" "$player_windows"
"$gh_bin" release create "v$version" "$@" --repo "$repository" --target main --title "Drachuri $version" --notes "$notes"
echo "Published: https://github.com/$repository/releases/tag/v$version"
