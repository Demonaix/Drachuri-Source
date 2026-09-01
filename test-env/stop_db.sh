#!/bin/sh
set -eu

data_dir=${DND_TEST_PGDATA:-"$HOME/Library/Application Support/Drachuri/test-postgres"}

if /Library/PostgreSQL/18/bin/pg_ctl -D "$data_dir" status >/dev/null 2>&1; then
  /Library/PostgreSQL/18/bin/pg_ctl -D "$data_dir" stop -m fast
else
  printf '%s\n' "Drachuri test database is already stopped."
fi
