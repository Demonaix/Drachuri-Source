#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
pg_bin=/Library/PostgreSQL/18/bin
socket_dir=/tmp/drachuri-postgres
port=55432
user=drachuri_test
database=drachuri_test

snapshot=$(find "$project_dir/test-env/snapshots" -maxdepth 1 -type f -name 'campaign-*.sql' -print | sort | tail -1)
if [ -z "$snapshot" ] || [ ! -f "$snapshot" ]; then
  printf '%s\n' "No campaign snapshot is available; leaving the QA database in place." >&2
  exit 1
fi

"$project_dir/test-env/start_db.sh"
"$pg_bin/psql" -h "$socket_dir" -p "$port" -U "$user" -d postgres -c \
  "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='drachuri_test' AND pid<>pg_backend_pid();" >/dev/null
"$pg_bin/dropdb" -h "$socket_dir" -p "$port" -U "$user" --if-exists "$database"
"$pg_bin/createdb" -h "$socket_dir" -p "$port" -U "$user" "$database"
"$pg_bin/psql" -h "$socket_dir" -p "$port" -U "$user" --quiet -d "$database" --set=ON_ERROR_STOP=1 --file "$snapshot"

# Campaign snapshots contain live data at the schema version they were captured
# from. Always bring the restored copy forward before any app connects to it.
set -a
. "$project_dir/test-env/test.env"
set +a
Rscript --vanilla "$project_dir/scripts/migrate_database.R"

touch "$project_dir/test-env/.campaign-snapshot-loaded"
printf '%s\n' "Restored local visual test server from $(basename "$snapshot")."
