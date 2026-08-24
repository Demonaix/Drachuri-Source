#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
player_dir="$project_dir/DND APP Drachuri Edition Player_v2"
control_dir="$project_dir/DND APP Drachuri Edition 2 Control"
r_library="$player_dir/renv/library/R-4.2/x86_64-apple-darwin17.0"
log_dir="$project_dir/test-env/logs"

mkdir -p "$log_dir"
"$project_dir/test-env/reset_db.sh"

set -a
. "$project_dir/test-env/test.env"
set +a

start_app() {
  app_dir=$1
  port=$2
  log=$3
  (
    cd "$app_dir"
    R_LIBS="$r_library" Rscript --no-init-file --no-environ -e \
      "shiny::runApp('.', host='127.0.0.1', port=$port, launch.browser=FALSE)"
  ) >"$log" 2>&1 &
  last_pid=$!
}

start_app "$control_dir" 3838 "$log_dir/control.log"
control_pid=$last_pid
start_app "$player_dir" 3839 "$log_dir/player-one.log"
player_one_pid=$last_pid
start_app "$player_dir" 3840 "$log_dir/player-two.log"
player_two_pid=$last_pid
start_app "$player_dir" 3841 "$log_dir/player-three.log"
player_three_pid=$last_pid
start_app "$player_dir" 3842 "$log_dir/player-four.log"
player_four_pid=$last_pid

cleanup() {
  kill "$control_pid" "$player_one_pid" "$player_two_pid" "$player_three_pid" "$player_four_pid" 2>/dev/null || true
  wait "$control_pid" "$player_one_pid" "$player_two_pid" "$player_three_pid" "$player_four_pid" 2>/dev/null || true
}
trap cleanup EXIT INT TERM

check_port() {
  port=$1
  output=$2
  attempt=0
  while [ "$attempt" -lt 60 ]; do
    if curl -fsS --max-time 2 "http://127.0.0.1:$port/" > "$output"; then
      return 0
    fi
    attempt=$((attempt + 1))
    sleep 1
  done
  return 1
}

failed=0
for port in 3838 3839 3840 3841 3842; do
  if check_port "$port" "/tmp/drachuri-smoke-$port.html"; then
    bytes=$(wc -c < "/tmp/drachuri-smoke-$port.html")
    printf '%s\n' "PASS: port $port returned Shiny HTML ($bytes bytes)"
  else
    printf '%s\n' "FAIL: port $port did not become healthy"
    failed=1
  fi
done

if [ "$failed" -ne 0 ]; then
  printf '\n%s\n' "Control log:"
  tail -80 "$log_dir/control.log" || true
  printf '\n%s\n' "Player one log:"
  tail -80 "$log_dir/player-one.log" || true
  printf '\n%s\n' "Player two log:"
  tail -80 "$log_dir/player-two.log" || true
  printf '\n%s\n' "Player three log:"
  tail -80 "$log_dir/player-three.log" || true
  printf '\n%s\n' "Player four log:"
  tail -80 "$log_dir/player-four.log" || true
  exit 1
fi

printf '%s\n' "PASS: one control and four independent player servers started together."
