#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
Rscript --vanilla "$project_dir/test-env/clone_live_campaign.R" "$project_dir"
