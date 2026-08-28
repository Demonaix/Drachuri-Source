#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
player_dir="$project_dir/DND APP Drachuri Edition Player_v2"
control_dir="$project_dir/DND APP Drachuri Edition 2 Control"
r_library="$player_dir/renv/library/R-4.2/x86_64-apple-darwin17.0"
log_dir="$project_dir/test-env/logs"
db_lock="$project_dir/test-env/.database-operation.lock"

acquire_db_lock() {
  if ! mkdir "$db_lock" 2>/dev/null; then
    holder=$(cat "$db_lock/pid" 2>/dev/null || printf '')
    if [ -n "$holder" ] && kill -0 "$holder" 2>/dev/null; then
      printf '%s\n' "Another Drachuri database operation is running (PID $holder). Please wait and launch again." >&2
      exit 1
    fi
    rm -f "$db_lock/pid"
    rmdir "$db_lock" 2>/dev/null || true
    mkdir "$db_lock"
  fi
  printf '%s\n' "$$" > "$db_lock/pid"
  trap 'rm -f "$db_lock/pid"; rmdir "$db_lock" 2>/dev/null || true' EXIT INT TERM
}

release_db_lock() {
  rm -f "$db_lock/pid"
  rmdir "$db_lock" 2>/dev/null || true
  trap - EXIT INT TERM
}

mkdir -p "$log_dir"
acquire_db_lock
# A previous launcher can survive a closed Terminal window. Clear only stale
# Drachuri Shiny listeners before binding the dedicated local test ports.
"$project_dir/test-env/stop_local_apps.sh"
if [ -f "$project_dir/test-env/.campaign-snapshot-loaded" ]; then
  "$project_dir/test-env/start_db.sh"
  printf '%s\n' "Using the saved campaign snapshot in the local test database."
else
  "$project_dir/test-env/reset_db.sh"
fi

set -a
. "$project_dir/test-env/test.env"
set +a
Rscript --vanilla "$project_dir/scripts/migrate_database.R"
release_db_lock
export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8
export LC_CTYPE=en_GB.UTF-8

R_LIBS="$r_library" Rscript --no-init-file --no-environ "$project_dir/test-env/configure_visual_test_map.R"

launch_app() {
  app_dir=$1
  port=$2
  log=$3
  (
    cd "$app_dir"
    exec nohup env R_LIBS="$r_library" Rscript --no-init-file --no-environ -e \
      "shiny::runApp('.', host='127.0.0.1', port=$port, launch.browser=FALSE)"
  ) >"$log" 2>&1 </dev/null &
  printf '%s' "$!"
}

control_pid=$(launch_app "$control_dir" 3838 "$log_dir/control.log")
player_one_pid=$(launch_app "$player_dir" 3839 "$log_dir/player-one.log")
player_two_pid=$(launch_app "$player_dir" 3840 "$log_dir/player-two.log")
player_three_pid=$(launch_app "$player_dir" 3841 "$log_dir/player-three.log")
player_four_pid=$(launch_app "$player_dir" 3842 "$log_dir/player-four.log")

printf '%s\n' "$control_pid" "$player_one_pid" "$player_two_pid" "$player_three_pid" "$player_four_pid" > "$log_dir/app.pids"

printf '%s\n' \
  "Local apps starting:" \
  "  Control:  http://127.0.0.1:3838" \
  "  Player 1: http://127.0.0.1:3839" \
  "  Player 2: http://127.0.0.1:3840" \
  "  Player 3: http://127.0.0.1:3841" \
  "  Player 4: http://127.0.0.1:3842" \
  "Logs: $log_dir" \
  "Stop them with: sh test-env/stop_local_apps.sh"
