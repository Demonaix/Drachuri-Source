#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
pid_file="$project_dir/test-env/logs/app.pids"

if [ ! -f "$pid_file" ]; then
  printf '%s\n' "No local app PID file found."
  exit 0
fi

while IFS= read -r pid; do
  if [ -n "$pid" ]; then kill "$pid" 2>/dev/null || true; fi
done < "$pid_file"

rm -f "$pid_file"
printf '%s\n' "Local Drachuri app processes stopped."

