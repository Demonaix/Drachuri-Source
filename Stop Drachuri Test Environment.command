#!/bin/sh
set -u

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$project_dir" || exit 1

sh test-env/stop_local_apps.sh
sh test-env/stop_db.sh

printf '\n%s\n' "Drachuri Test Environment stopped."
printf 'Press Return to close...'
read answer

