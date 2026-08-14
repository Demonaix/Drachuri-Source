#!/bin/sh
set -eu

app_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$app_dir"

if ! command -v Rscript >/dev/null 2>&1; then
  printf '%s\n' "Rscript was not found. Install R and try again."
  exit 1
fi

Rscript --vanilla launcher/bootstrap_and_run.R

