#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
r_library="$project_dir/DND APP Drachuri Edition Player_v2/renv/library/R-4.2/x86_64-apple-darwin17.0"
db_lock="$project_dir/test-env/.database-operation.lock"

if ! mkdir "$db_lock" 2>/dev/null; then
  holder=$(cat "$db_lock/pid" 2>/dev/null || printf '')
  if [ -n "$holder" ] && kill -0 "$holder" 2>/dev/null; then
    printf '%s\n' "Test database is busy (PID $holder); integration test not started." >&2
    exit 1
  fi
  rm -f "$db_lock/pid"
  rmdir "$db_lock" 2>/dev/null || true
  mkdir "$db_lock"
fi
printf '%s\n' "$$" > "$db_lock/pid"

restore_campaign=false
if [ -f "$project_dir/test-env/.campaign-snapshot-loaded" ]; then
  restore_campaign=true
fi
cleanup() {
  if $restore_campaign; then "$project_dir/test-env/restore_campaign_snapshot.sh"; fi
  rm -f "$db_lock/pid"
  rmdir "$db_lock" 2>/dev/null || true
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
