#!/bin/sh
set -u

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$project_dir" || exit 1

printf '%s\n' \
  "Starting the isolated Drachuri test environment..." \
  "The apps use only the local drachuri_test database. Supabase is not changed."

if ! sh test-env/run_local_apps.sh; then
  printf '\n%s\n' "The test environment could not start. Check test-env/logs for details."
  printf 'Press Return to close...'
  read answer
  exit 1
fi

wait_for_app() {
  port=$1
  attempt=0
  while [ "$attempt" -lt 60 ]; do
    if curl -fsS --max-time 2 "http://127.0.0.1:$port/" >/dev/null 2>&1; then
      return 0
    fi
    attempt=$((attempt + 1))
    sleep 1
  done
  return 1
}

failed=0
for port in 3838 3839 3840 3841 3842; do
  if wait_for_app "$port"; then
    printf '%s\n' "Ready: http://127.0.0.1:$port"
  else
    printf '%s\n' "Not ready: http://127.0.0.1:$port"
    failed=1
  fi
done

if [ "$failed" -ne 0 ]; then
  printf '\n%s\n' \
    "One or more apps did not become ready." \
    "Check test-env/logs/control.log and player-one.log through player-four.log."
  printf 'Press Return to close...'
  read answer
  exit 1
fi

open "http://127.0.0.1:3838"
open "http://127.0.0.1:3839"
open "http://127.0.0.1:3840"
open "http://127.0.0.1:3841"
open "http://127.0.0.1:3842"

printf '\n%s\n' \
  "Drachuri Test Environment is ready." \
  "Control and all four player pages have been opened." \
  "Use 'Stop Drachuri Test Environment.command' when finished." \
  "You can now close this window; the apps will keep running."
printf '\nPress Return to close this launcher window...'
read answer
