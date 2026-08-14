#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
r_library="$project_dir/DND APP Drachuri Edition Player_v2/renv/library/R-4.2/x86_64-apple-darwin17.0"

"$project_dir/test-env/reset_db.sh"

set -a
. "$project_dir/test-env/test.env"
DND_PROJECT_DIR="$project_dir"
set +a

R_LIBS="$r_library" Rscript --no-init-file --no-environ \
  "$project_dir/test-env/integration_test.R"
