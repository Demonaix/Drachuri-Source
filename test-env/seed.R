library(DBI)
library(RPostgres)

con <- dbConnect(
  Postgres(),
  host = Sys.getenv("SUPABASE_HOST"),
  port = as.integer(Sys.getenv("SUPABASE_PORT")),
  dbname = Sys.getenv("SUPABASE_DBNAME"),
  user = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_DB_PASSWORD"),
  sslmode = Sys.getenv("SUPABASE_SSLMODE", "disable")
)
on.exit(dbDisconnect(con), add = TRUE)

make_character <- function(name, class, race) {
  list(
    save_version = 2L,
    meta = list(name = name, race = race),
    build = list(class = class, path = "Test Path", level = 3L),
    abilities = list(str = 12, dex = 14, con = 12, int = 10, bld_str = 10, cha = 10),
    resources = list(
      hp = list(cur = 12L, max = 12L, temp = 0L),
      sindre = list(cur = 4L, total = 6L, temp = 0L)
    ),
    status = list(effects = character(), exhaustion = 0L),
    journal = list(log = character()),
    inventory = list(items = data.frame())
  )
}

insert_character <- function(id, character) {
  dbExecute(
    con,
    paste(
      "INSERT INTO character_blobs (id, player_id, char_name, state_blob)",
      "VALUES ($1, 'test', $2, $3)"
    ),
    params = list(id, character$meta$name, list(serialize(character, NULL)))
  )
}

insert_character(1001L, make_character("Eira Test", "Warden", "Human"))
insert_character(1002L, make_character("Bryn Test", "Mage", "Tylwyth Teg"))

dbExecute(con, "INSERT INTO maps (id, name, width, height) VALUES (1, 'Test Arena', 8, 8)")
tiles <- expand.grid(x = 1:8, y = 1:8)
for (i in seq_len(nrow(tiles))) {
  dbExecute(
    con,
    "INSERT INTO map_tiles (map_id, x, y, terrain) VALUES (1, $1, $2, 'grass')",
    params = list(tiles$x[i], tiles$y[i])
  )
}

dbExecute(con, paste(
  "INSERT INTO game_sessions",
  "(id, name, mode, status, active_map_id) VALUES",
  "(1, 'Local Integration Test', 'combat', 'active', 1)"
))
dbExecute(con, paste(
  "INSERT INTO encounters (id, session_id, name, map_id, status)",
  "VALUES (1, 1, 'Two Players vs Bandit', 1, 'active')"
))
dbExecute(con, "UPDATE game_sessions SET active_encounter_id = 1 WHERE id = 1")

for (player in list(c("1001", "Eira Test", "1"), c("1002", "Bryn Test", "2"))) {
  dbExecute(
    con,
    paste(
      "INSERT INTO session_players",
      "(session_id, character_id, display_name, turn_order, current_hp, max_hp)",
      "VALUES (1, $1, $2, $3, 12, 12)"
    ),
    params = as.list(player)
  )
}

enemy <- dbGetQuery(con, paste(
  "INSERT INTO encounter_enemies",
  "(encounter_id, session_id, name, hp_max, hp_current, ac, turn_order)",
  "VALUES (1, 1, 'Test Bandit', 10, 10, 12, 3) RETURNING enemy_uuid"
))$enemy_uuid[[1L]]

positions <- list(
  list("player", "1001", 2L, 2L),
  list("player", "1002", 3L, 2L),
  list("enemy", as.character(enemy), 6L, 6L)
)
for (position in positions) {
  dbExecute(
    con,
    paste(
      "INSERT INTO encounter_positions",
      "(encounter_id, actor_type, actor_id, x, y) VALUES (1, $1, $2, $3, $4)"
    ),
    params = position
  )
}

dbExecute(con, paste(
  "INSERT INTO combat_state",
  "(session_id, encounter_id, round_number, current_turn_order,",
  " active_actor_type, active_actor_id, phase)",
  "VALUES (1, 1, 1, 1, 'player', '1001', 'combat')"
))

dbExecute(con, paste(
  "INSERT INTO npc_templates",
  "(npc_id, name, hp_max, ac, movement_speed, attack_name, attack_bonus, damage_expr, damage_type)",
  "VALUES ('test_bandit', 'Test Bandit', 10, 12, 30, 'Shortsword', 3, '1d6+1', 'slashing')"
))

cat("Seeded local test session 1 with characters 1001 and 1002.\n")
