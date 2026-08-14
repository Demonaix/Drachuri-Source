#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
/Library/PostgreSQL/18/bin/pg_ctl \
  -D "$project_dir/test-env/.postgres-data" stop -m fast

