project_dir <- normalizePath(Sys.getenv("DND_PROJECT_DIR"))
player_dir <- file.path(project_dir, "DND APP Drachuri Edition Player_v2")

setwd(player_dir)
source("global.R")
source("session_db.R")

on.exit(close_db_pool(), add = TRUE)
stopifnot(is_db_available())

started <- proc.time()[["elapsed"]]
snapshot <- get_player_live_snapshot(1L, "1001")
elapsed_ms <- round((proc.time()[["elapsed"]] - started) * 1000)
payload_kib <- round(length(serialize(snapshot, NULL, version = 2)) / 1024, 1)

stopifnot(nrow(snapshot$session) == 1L)
stopifnot(nrow(snapshot$players) == 2L)
stopifnot(nrow(snapshot$self_player) == 1L)
stopifnot(nrow(snapshot$encounter) == 1L)
stopifnot(nrow(snapshot$enemies) == 1L)
stopifnot(nrow(snapshot$positions) == 3L)
stopifnot(nrow(snapshot$combat) == 1L)

damage <- damage_session_player(1L, "1002", 3L)
stopifnot(identical(damage$hp_before, 12L))
stopifnot(identical(damage$hp_after, 9L))

stopifnot(isTRUE(advance_turn(1L)))
after <- get_player_live_snapshot(1L, "1002")
stopifnot(identical(after$self_player$current_hp[[1L]], 9L))
stopifnot(identical(after$combat$current_turn_order[[1L]], 2L))
stopifnot(identical(after$combat$active_actor_id[[1L]], "1002"))

cat("PASS: local PostgreSQL snapshot, HP sync, and turn advancement\n")
cat("Snapshot:", elapsed_ms, "ms,", payload_kib, "KiB\n")
