#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
player_dir="$project_dir/DND APP Drachuri Edition Player_v2"
release_dir="$project_dir/releases"
version=$(tr -d '\r\n' < "$project_dir/VERSION")
timestamp=$(date '+%Y-%m-%d_%H-%M-%S')
archive="$release_dir/drachuri-player_${version}_$timestamp.tar.gz"
zip_archive="$release_dir/drachuri-player_${version}_$timestamp.zip"

mkdir -p "$release_dir"

model_exclude='*/www/models'
if [ "${INCLUDE_3D_MODELS:-0}" = "1" ]; then
  model_exclude='__do_not_match_models__'
fi

LC_ALL=C tar -czf "$archive" \
  --exclude='.RData' \
  --exclude='.Rhistory' \
  --exclude='.Rproj.user' \
  --exclude='.DS_Store' \
  --exclude='renv/library' \
  --exclude='renv/cache' \
  --exclude='launcher/bootstrap-library' \
  --exclude='launcher/logs' \
  --exclude='launcher/.restored-lock-md5' \
  --exclude="$model_exclude" \
  -C "$project_dir" "DND APP Drachuri Edition Player_v2"

printf '%s\n' "$archive"

if command -v zip >/dev/null 2>&1; then
  (
    cd "$project_dir"
    zip -qr "$zip_archive" "DND APP Drachuri Edition Player_v2" \
      -x '*/.RData' \
      -x '*/.Rhistory' \
      -x '*/.Rproj.user/*' \
      -x '*/.DS_Store' \
      -x '*/renv/library/*' \
      -x '*/renv/cache/*' \
      -x '*/launcher/bootstrap-library/*' \
      -x '*/launcher/logs/*' \
      -x '*/launcher/.restored-lock-md5' \
      -x "$model_exclude/*"
  )
  printf '%s\n' "$zip_archive"
fi
