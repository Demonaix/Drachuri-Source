#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
pg_bin=/Library/PostgreSQL/18/bin
data_dir="$project_dir/test-env/.postgres-data"
socket_dir=/tmp/drachuri-postgres
log_file="$project_dir/test-env/postgres.log"

mkdir -p "$socket_dir"

if "$pg_bin/pg_ctl" -D "$data_dir" status >/dev/null 2>&1; then
  printf '%s\n' "Drachuri test database is already running."
  exit 0
fi

"$pg_bin/pg_ctl" -D "$data_dir" -l "$log_file" \
  -o "-p 55432 -k $socket_dir -c listen_addresses=''" start

