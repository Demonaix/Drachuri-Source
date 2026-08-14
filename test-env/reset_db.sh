#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
pg_bin=/Library/PostgreSQL/18/bin
socket_dir=/tmp/drachuri-postgres
port=55432
user=drachuri_test
database=drachuri_test
r_library="$project_dir/DND APP Drachuri Edition Player_v2/renv/library/R-4.2/x86_64-apple-darwin17.0"

"$project_dir/test-env/start_db.sh"
"$pg_bin/dropdb" -h "$socket_dir" -p "$port" -U "$user" --if-exists "$database"
"$pg_bin/createdb" -h "$socket_dir" -p "$port" -U "$user" "$database"
"$pg_bin/psql" -h "$socket_dir" -p "$port" -U "$user" -d "$database" \
  -v ON_ERROR_STOP=1 -f "$project_dir/test-env/schema.sql"

set -a
. "$project_dir/test-env/test.env"
set +a

R_LIBS="$r_library" Rscript --no-init-file --no-environ \
  "$project_dir/test-env/seed.R"

printf '%s\n' "Local Drachuri test database reset successfully."

