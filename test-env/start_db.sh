#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
pg_bin=/Library/PostgreSQL/18/bin
data_dir=${DND_TEST_PGDATA:-"$HOME/Library/Application Support/Drachuri/test-postgres"}
socket_dir=/tmp/drachuri-postgres
log_file="$project_dir/test-env/postgres.log"

mkdir -p "$socket_dir" "$(dirname "$data_dir")"

# PostgreSQL's live data files must not sit in a Desktop/iCloud-synchronised
# source tree. Initialise the local runtime on first use; campaign data is
# restored separately from the versioned test snapshots.
if [ ! -f "$data_dir/PG_VERSION" ]; then
  "$pg_bin/initdb" -D "$data_dir" --username=drachuri_test --auth=trust --encoding=UTF8 --locale=C >/dev/null
  printf '%s\n' "Initialised the local Drachuri test database runtime."
fi

if "$pg_bin/pg_ctl" -D "$data_dir" status >/dev/null 2>&1; then
  printf '%s\n' "Drachuri test database is already running."
  exit 0
fi

"$pg_bin/pg_ctl" -D "$data_dir" -l "$log_file" \
  -o "-p 55432 -k $socket_dir -c listen_addresses=''" start
