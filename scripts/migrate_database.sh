#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
exec Rscript --vanilla "$project_dir/scripts/migrate_database.R" "$@"
