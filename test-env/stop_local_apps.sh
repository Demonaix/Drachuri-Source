#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
log_dir="$project_dir/test-env/logs"

stop_if_drachuri_app() {
  pid=$1
  [ -n "$pid" ] || return 0
  command=$(ps -p "$pid" -o command= 2>/dev/null || true)
  case "$command" in
    *"shiny::runApp("*) kill "$pid" 2>/dev/null || true ;;
  esac
}

# Finder can preserve older PID lists as "app 2.pids", while an interrupted
# launcher can lose app.pids entirely. Read every Drachuri PID list, but verify
# the process command before stopping it in case an old PID has been reused.
for pid_file in "$log_dir"/app*.pids; do
  [ -f "$pid_file" ] || continue
  while IFS= read -r pid; do stop_if_drachuri_app "$pid"; done < "$pid_file"
  rm -f "$pid_file"
done

# Finally clear orphaned Shiny listeners on the five dedicated test ports.
# Never stop a different application that happens to use one of these ports.
if command -v lsof >/dev/null 2>&1; then
  for port in 3838 3839 3840 3841 3842; do
    for pid in $(lsof -nP -tiTCP:"$port" -sTCP:LISTEN 2>/dev/null || true); do
      stop_if_drachuri_app "$pid"
    done
  done
fi

printf '%s\n' "Local Drachuri app processes stopped."
