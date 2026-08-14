#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
backup_dir="$project_dir/backups"
timestamp=$(date '+%Y-%m-%d_%H-%M-%S')
archive="$backup_dir/drachuri-source-$timestamp.tar.gz"

mkdir -p "$backup_dir"

LC_ALL=C tar -czf "$archive" \
  --exclude='./backups' \
  --exclude='*/www/models' \
  --exclude='*/renv/library' \
  --exclude='*/renv/cache' \
  --exclude='*/.Rproj.user' \
  --exclude='*/.RData' \
  --exclude='*/.Rhistory' \
  --exclude='*/.DS_Store' \
  -C "$project_dir" .

printf '%s\n' "$archive"
