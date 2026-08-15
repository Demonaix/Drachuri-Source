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

make_weapon <- function(name = "QA Longsword") {
  data.frame(
    id = "qa_weapon", name = name, type = "weapon", desc = "Local QA weapon",
    value = 0, weight = 3, qty = 1, equipped = TRUE, in_bag = FALSE,
    meta = I(list(list(
      stat = "str", adv = "Normal", to_hit_bonus = 0,
      damage1 = "1d8", dmg_type1 = "Slashing", damage2 = "", dmg_type2 = "Other",
      proficient = TRUE
    ))),
    edit = FALSE, stringsAsFactors = FALSE
  )
}

make_character <- function(name, class, subclass, choices = list(), abilities = list()) {
  base_abilities <- list(str = 16L, dex = 16L, con = 14L, int = 14L, bld_str = 18L, cha = 16L)
  for (stat in names(abilities)) base_abilities[[stat]] <- abilities[[stat]]
  list(
    save_version = 2L,
    meta = list(name = name, race = "QA Human"),
    build = list(
      class = class, path = subclass, level = 6L,
      classes = list(list(class = class, level = 6L, subclass = subclass)),
      level_choices = setNames(list(choices), class), level_history = list()
    ),
    abilities = base_abilities,
    resources = list(
      hp = list(cur = 60L, max = 60L, temp = 0L),
      sindre = list(cur = 100L, total = 100L, temp = 0L),
      class_uses = list(), class_pools = list(),
      blood = list(addiction = list(stage = 1L, current_day_intake = 0, previous_day_intake = 0))
    ),
    status = list(effects = character(), exhaustion = 0L, raging = FALSE),
    prof = list(
      skills = list(stealth = "Proficient", investigation = "Proficient", medicine = "Proficient", survival = "Proficient"),
      tools = list()
    ),
    journal = list(log = character()),
    inventory = list(items = make_weapon()),
    combat_profile = list(resistances = character(), immunities = character(), vulnerabilities = character(), speed_ft = 30L)
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

qa_characters <- list(
  list(1001L, make_character("QA Rogue — Thief", "Rogue", "Thief", list(
    `3` = list(subclass = "Thief"),
    `6` = list(expertise_skill_1 = "stealth", expertise_skill_2 = "investigation")
  ))),
  list(1002L, make_character("QA Rogue — Assassin", "Rogue", "Assassin", list(
    `3` = list(subclass = "Assassin"),
    `6` = list(expertise_skill_1 = "stealth", expertise_skill_2 = "investigation")
  ))),
  list(1003L, make_character("QA Fighter — Champion", "Fighter", "Champion", list(
    `1` = list(fighting_style = "Dueling"), `3` = list(subclass = "Champion"),
    `4` = list(asi_first = "str", asi_second = "str"),
    `6` = list(asi_first = "con", asi_second = "con")
  ))),
  list(1004L, make_character("QA Fighter — Battle Master", "Fighter", "Battle Master", list(
    `1` = list(fighting_style = "Defence"),
    `3` = list(
      subclass = "Battle Master", battle_master_manoeuvre_1 = "Precision Attack",
      battle_master_manoeuvre_2 = "Trip Attack", battle_master_manoeuvre_3 = "Riposte",
      student_of_war_tool = "Smith's Tools"
    ), `4` = list(asi_first = "str", asi_second = "con"),
    `6` = list(asi_first = "str", asi_second = "con")
  ))),
  list(1005L, make_character("QA Barbarian — Berserker", "Barbarian", "Berserker", list(
    `3` = list(subclass = "Berserker"), `4` = list(asi_first = "str", asi_second = "con")
  ))),
  list(1006L, make_character("QA Barbarian — Totem", "Barbarian", "Totem Warrior", list(
    `3` = list(subclass = "Totem Warrior", spirit_totem = "Bear"),
    `4` = list(asi_first = "str", asi_second = "con")
  ))),
  list(1007L, make_character("QA Hanianol — Ancestor", "Hanianol Sorcerer", "Path of the Ancestor", list(
    `2` = list(natural_specialty = "Plants"), `3` = list(subclass = "Path of the Ancestor"),
    `4` = list(asi_first = "bld_str", asi_second = "bld_str"), `5` = list(thermal_path = "Exothermic")
  ))),
  list(1008L, make_character("QA Hanianol — Heart Eater", "Hanianol Sorcerer", "Heart Eater", list(
    `2` = list(natural_specialty = "Disease"), `3` = list(subclass = "Heart Eater"),
    `4` = list(asi_first = "bld_str", asi_second = "con"), `5` = list(thermal_path = "Endothermic")
  ))),
  list(1009L, make_character("QA Na'Haran — Warrior", "Na'Haran Sorcerer", "Path of the Warrior", list(
    `3` = list(subclass = "Path of the Warrior"), `4` = list(asi_first = "bld_str", asi_second = "con"),
    `5` = list(thermal_path = "Exothermic"), `6` = list(electromagnetic_path = "Flesh Witherer")
  ))),
  list(1010L, make_character("QA Na'Haran — Prophet", "Na'Haran Sorcerer", "Path of the Prophet", list(
    `3` = list(subclass = "Path of the Prophet"), `4` = list(asi_first = "bld_str", asi_second = "cha"),
    `5` = list(thermal_path = "Endothermic"), `6` = list(electromagnetic_path = "Lightbringer")
  )))
)
for (entry in qa_characters) insert_character(entry[[1]], entry[[2]])

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

for (i in seq_along(qa_characters)) {
  character_id <- qa_characters[[i]][[1]]
  character <- qa_characters[[i]][[2]]
  dbExecute(
    con,
    paste(
      "INSERT INTO session_players",
      "(session_id, character_id, display_name, turn_order, current_hp, max_hp)",
      "VALUES (1, $1, $2, $3, 60, 60)"
    ),
    params = list(character_id, character$meta$name, i)
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
  list("player", "1003", 4L, 2L),
  list("player", "1004", 5L, 2L),
  list("player", "1005", 6L, 2L),
  list("player", "1006", 7L, 2L),
  list("player", "1007", 2L, 3L),
  list("player", "1008", 3L, 3L),
  list("player", "1009", 4L, 3L),
  list("player", "1010", 5L, 3L),
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

cat("Seeded local test session 1 with ten level-6 QA characters (1001-1010).\n")
