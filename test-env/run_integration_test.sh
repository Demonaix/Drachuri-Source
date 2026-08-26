#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
r_library="$project_dir/DND APP Drachuri Edition Player_v2/renv/library/R-4.2/x86_64-apple-darwin17.0"

restore_campaign=false
if [ -f "$project_dir/test-env/.campaign-snapshot-loaded" ]; then
  restore_campaign=true
fi
cleanup() {
  if $restore_campaign; then "$project_dir/test-env/restore_campaign_snapshot.sh"; fi
}
trap cleanup EXIT INT TERM

"$project_dir/test-env/reset_db.sh"

set -a
. "$project_dir/test-env/test.env"
DND_PROJECT_DIR="$project_dir"
set +a
export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8
export LC_CTYPE=en_GB.UTF-8

R_LIBS="$r_library" Rscript --no-init-file --no-environ \
  "$project_dir/test-env/integration_test.R"
