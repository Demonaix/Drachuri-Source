project_dir <- normalizePath(Sys.getenv("DND_PROJECT_DIR"))
player_dir <- file.path(project_dir, "DND APP Drachuri Edition Player_v2")

setwd(player_dir)
source("global.R")
source("session_db.R")
source("server/combat_map_logic.R")

on.exit(close_db_pool(), add = TRUE)
stopifnot(is_db_available())

started <- proc.time()[["elapsed"]]
snapshot <- get_player_live_snapshot(1L, "1001")
elapsed_ms <- round((proc.time()[["elapsed"]] - started) * 1000)
payload_kib <- round(length(serialize(snapshot, NULL, version = 2)) / 1024, 1)

stopifnot(nrow(snapshot$session) == 1L)
stopifnot(nrow(snapshot$players) == 10L)
stopifnot(nrow(snapshot$self_player) == 1L)
stopifnot(nrow(snapshot$encounter) == 1L)
stopifnot(nrow(snapshot$enemies) == 1L)
stopifnot(nrow(snapshot$positions) == 11L)
stopifnot(nrow(snapshot$combat) == 1L)

membership <- get_active_session_for_character("1001")
stopifnot(nrow(membership) == 1L)
stopifnot(membership$session_id[[1L]] == 1L)
stopifnot(membership$active_encounter_id[[1L]] == 1L)

character <- load_character_from_db(1001L)
stopifnot(is.list(character))
character$journal$log <- c(character$journal$log, "Multiplayer regression save marker")
stopifnot(identical(save_character_to_db(character, 1001L), "1001"))
reloaded_character <- load_character_from_db(1001L)
stopifnot(identical(
  tail(reloaded_character$journal$log, 1L),
  "Multiplayer regression save marker"
))

damage <- damage_session_player(1L, "1002", 3L)
stopifnot(identical(damage$hp_before, 60L))
stopifnot(identical(damage$hp_after, 57L))

stopifnot(isTRUE(advance_turn(1L)))
after <- get_player_live_snapshot(1L, "1002")
stopifnot(identical(after$self_player$current_hp[[1L]], 57L))
stopifnot(identical(after$combat$current_turn_order[[1L]], 2L))
stopifnot(identical(after$combat$active_actor_id[[1L]], "1002"))

stopifnot(isTRUE(upsert_encounter_actor_position(1L, "player", "1001", 4L, 5L)))
player_one_after <- get_player_live_snapshot(1L, "1001")
player_two_after <- get_player_live_snapshot(1L, "1002")
position_one <- player_one_after$positions[
  player_one_after$positions$actor_type == "player" &
    player_one_after$positions$actor_id == "1001",
  , drop = FALSE
]
position_two_view <- player_two_after$positions[
  player_two_after$positions$actor_type == "player" &
    player_two_after$positions$actor_id == "1001",
  , drop = FALSE
]
stopifnot(nrow(position_one) == 1L, position_one$x[[1L]] == 4L, position_one$y[[1L]] == 5L)
stopifnot(
  nrow(position_two_view) == 1L,
  position_two_view$x[[1L]] == 4L,
  position_two_view$y[[1L]] == 5L
)
stopifnot(identical(player_one_after$combat$active_actor_id[[1L]], "1002"))
stopifnot(identical(player_two_after$self_player$current_hp[[1L]], 57L))

# Simulate a dropped/restarted client-side pool and verify a new checkout sees
# the same shared state without restarting PostgreSQL.
close_db_pool()
stopifnot(is_db_available())
reconnected <- get_player_live_snapshot(1L, "1002")
stopifnot(identical(reconnected$self_player$current_hp[[1L]], 57L))
stopifnot(identical(reconnected$combat$active_actor_id[[1L]], "1002"))

# The seeded enemy and player 1003 deliberately share turn order 3. Both must
# receive a turn instead of the enemy being collapsed out of the sequence.
stopifnot(isTRUE(advance_turn(1L)))
enemy_turn <- get_player_live_snapshot(1L, "1002")
stopifnot(identical(enemy_turn$combat$active_actor_type[[1L]], "enemy"))
stopifnot(isTRUE(advance_turn(1L)))
after_tie <- get_player_live_snapshot(1L, "1002")
stopifnot(identical(after_tie$combat$active_actor_id[[1L]], "1003"))

grapple <- create_encounter_effect(
  1L, "enemy", as.character(enemy_turn$combat$active_actor_id[[1L]]),
  "grapple", "condition", payload = list(condition = "grappled"),
  target_actor_type = "player", target_actor_id = "1002", starts_round = 1L
)
stopifnot(is.data.frame(grapple), nrow(grapple) == 1L)
stopifnot(isTRUE(end_encounter_condition(1L, "1002", "grappled")))
cleared <- get_player_live_snapshot(1L, "1002")$effects
if (is.data.frame(cleared) && nrow(cleared)) {
  stopifnot(!any(as.character(cleared$target_actor_id) == "1002" & grepl("grappled", as.character(cleared$payload))))
}

double <- create_encounter_summon(1L, "1002", "QA Illusory Double", max_cr = "illusion", hp_max = 1L, ac = 10L)
stopifnot(is.data.frame(double), nrow(double) == 1L)
double_id <- as.character(double$summon_uuid[[1L]])
stopifnot(isTRUE(set_actor_turn_order(1L, double_id, "summon", 12L, 12L)))
summons <- get_encounter_summons(1L)
stopifnot(any(as.character(summons$summon_uuid) == double_id & summons$turn_order == 12L))

reinforcement_id <- add_encounter_enemy(
  1L, "QA Reinforcement", hp_max = 14L, ac = 13L, movement_speed = 30L,
  attack_bonus = 3L, damage_expr = "1d6+1", damage_type = "slashing"
)
stopifnot(is.character(reinforcement_id), nzchar(reinforcement_id))
stopifnot(isTRUE(upsert_encounter_actor_position(1L, "enemy", reinforcement_id, 7L, 7L)))

stopifnot(isTRUE(end_encounter_combat(1L)))
ended_snapshot <- get_player_live_snapshot(1L, "1002")
ended_combat <- get_combat_state(1L)
stopifnot(identical(as.character(ended_combat$phase[[1L]]), "ended"))
stopifnot(is.na(ended_snapshot$session$active_encounter_id[[1L]]))

cat("PASS: character save, two-player sync, HP, movement, turn, and reconnection\n")
cat("Snapshot:", elapsed_ms, "ms,", payload_kib, "KiB\n")

stopifnot(length(.drachuri_db$checked_out) == 0L)
close_db_pool()
stopifnot(is.null(.drachuri_db$pool))
