file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else "tests/run_tests.R"
project_dir <- normalizePath(file.path(dirname(script_path), ".."))

global_file <- file.path(
  project_dir,
  "DND APP Drachuri Edition Player_v2", "shared", "global_core.R"
)
session_file <- file.path(
  project_dir,
  "DND APP Drachuri Edition Player_v2", "shared", "session_db_core.R"
)

test_env <- new.env(parent = baseenv())
test_env$`%||%` <- function(a, b) if (!is.null(a)) a else b

load_functions <- function(path, names) {
  expressions <- parse(path)
  for (expr in expressions) {
    if (
      is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      is.symbol(expr[[2L]]) && as.character(expr[[2L]]) %in% names
    ) {
      eval(expr, envir = test_env)
    }
  }
}

load_functions(global_file, c("character_save_payload"))
load_functions(
  session_file,
  c(
    "calculate_hp_damage", "next_combat_turn",
    "empty_player_live_snapshot", "get_player_live_snapshot",
    "build_snapshot_encounter_actors", "start_encounter_combat"
  )
)

tests_run <- 0L
test <- function(name, code) {
  force(code)
  tests_run <<- tests_run + 1L
  cat("PASS:", name, "\n")
}

test("character save payload round-trips without data loss", {
  character <- list(
    meta = list(name = "Eira"),
    resources = list(hp = list(cur = 12L, max = 15L, temp = 2L)),
    inventory = list(items = data.frame(name = "Dagger", qty = 1L))
  )
  payload <- test_env$character_save_payload(character)
  stopifnot(identical(payload$name, "Eira"))
  stopifnot(identical(unserialize(payload$state_blob), character))
})

test("temporary HP absorbs damage before current HP", {
  result <- test_env$calculate_hp_damage(12L, 5L, 8L)
  stopifnot(identical(result$hp_after, 9L))
  stopifnot(identical(result$temp_after, 0L))
  stopifnot(identical(result$amount, 8L))
})

test("damage cannot reduce HP below zero", {
  result <- test_env$calculate_hp_damage(3L, 0L, 99L)
  stopifnot(identical(result$hp_after, 0L))
})

actors <- data.frame(
  actor_id = c("p1", "e1", "p2"),
  actor_type = c("player", "enemy", "player"),
  turn_order = c(1L, 2L, 3L),
  stringsAsFactors = FALSE
)

test("combat advances to the next actor", {
  combat <- data.frame(current_turn_order = 1L, round_number = 4L)
  result <- test_env$next_combat_turn(actors, combat)
  stopifnot(identical(result$turn_order, 2L))
  stopifnot(identical(result$actor_id, "e1"))
  stopifnot(identical(result$round_number, 4L))
})

test("combat wraps to a new round", {
  combat <- data.frame(current_turn_order = 3L, round_number = 4L)
  result <- test_env$next_combat_turn(actors, combat)
  stopifnot(identical(result$turn_order, 1L))
  stopifnot(identical(result$actor_id, "p1"))
  stopifnot(identical(result$round_number, 5L))
})

test("session snapshot combines players, enemies, and positions", {
  snapshot <- list(
    players = data.frame(
      character_id = "p1", display_name = "Eira", current_hp = 12L,
      max_hp = 15L, temp_hp = 2L, initiative = 17L, turn_order = 1L,
      is_active = TRUE, stringsAsFactors = FALSE
    ),
    enemies = data.frame(
      enemy_uuid = "e1", name = "Bandit", hp_current = 8L, hp_max = 8L,
      temp_hp = 0L, initiative = 12L, turn_order = 2L, is_active = TRUE,
      ac = 13L, movement_speed = 30L, stringsAsFactors = FALSE
    ),
    positions = data.frame(
      actor_id = c("p1", "e1"), actor_type = c("player", "enemy"),
      x = c(2L, 5L), y = c(3L, 6L), stringsAsFactors = FALSE
    )
  )
  combined <- test_env$build_snapshot_encounter_actors(snapshot)
  stopifnot(nrow(combined) == 2L)
  stopifnot(combined$x[combined$actor_id == "p1"] == 2L)
  stopifnot(combined$y[combined$actor_id == "e1"] == 6L)
  stopifnot(combined$current_hp[combined$actor_id == "e1"] == 8L)
})

test("live session refresh uses one connection and selects the current player", {
  calls <- new.env(parent = emptyenv())
  calls$connections <- 0L
  calls$releases <- 0L
  calls$queries <- 0L

  connection_factory <- function() {
    calls$connections <- calls$connections + 1L
    structure(list(), class = "fake_connection")
  }
  release_connection <- function(con) {
    calls$releases <- calls$releases + 1L
    invisible(TRUE)
  }
  query_function <- function(con, sql, params) {
    calls$queries <- calls$queries + 1L
    if (grepl("FROM game_sessions", sql, fixed = TRUE)) {
      return(data.frame(id = 7L, active_encounter_id = 11L))
    }
    if (grepl("FROM session_players", sql, fixed = TRUE)) {
      return(data.frame(
        id = c(1L, 2L), session_id = 7L,
        character_id = c("mine", "friend"),
        display_name = c("Eira", "Bryn"), current_hp = c(12L, 9L),
        temp_hp = c(0L, 2L), is_active = TRUE,
        stringsAsFactors = FALSE
      ))
    }
    if (grepl("FROM encounters", sql, fixed = TRUE)) return(data.frame(id = 11L, session_id = 7L))
    if (grepl("FROM encounter_positions", sql, fixed = TRUE)) return(data.frame())
    if (grepl("FROM combat_state", sql, fixed = TRUE)) return(data.frame(encounter_id = 11L, round_number = 2L))
    if (grepl("FROM game_events", sql, fixed = TRUE)) return(data.frame())
    if (grepl("FROM encounter_enemies", sql, fixed = TRUE)) return(data.frame())
    stop("Unexpected query: ", sql)
  }

  snapshot <- test_env$get_player_live_snapshot(
    7L,
    "mine",
    connection_factory = connection_factory,
    release_connection = release_connection,
    query_function = query_function
  )

  stopifnot(calls$connections == 1L)
  stopifnot(calls$releases == 1L)
  stopifnot(calls$queries == 7L)
  stopifnot(nrow(snapshot$self_player) == 1L)
  stopifnot(identical(snapshot$self_player$character_id[[1L]], "mine"))
  stopifnot(identical(snapshot$combat$round_number[[1L]], 2L))
})

test("combat map renderer uses the current encounter actors reactive", {
  combat_module_file <- file.path(
    project_dir,
    "DND APP Drachuri Edition Player_v2", "server", "debug_combat_module.R"
  )
  combat_source <- paste(readLines(combat_module_file, warn = FALSE), collapse = "\n")
  stopifnot(grepl("actors_lookup <- encounter_actors_tbl()", combat_source, fixed = TRUE))
  stopifnot(!grepl("encounter_actors_r()", combat_source, fixed = TRUE))
})

test("starting combat always rolls fresh initiative", {
  calls <- new.env(parent = emptyenv())
  calls$rolls <- 0L

  test_env$get_encounter_actors <- function(encounter_id) {
    data.frame(
      actor_id = c("stale-first", "stale-second"),
      actor_type = c("player", "enemy"),
      turn_order = c(1L, 2L),
      is_active = TRUE,
      stringsAsFactors = FALSE
    )
  }
  test_env$roll_encounter_initiative <- function(encounter_id, core_state = NULL) {
    calls$rolls <- calls$rolls + 1L
    data.frame(
      actor_id = c("fresh-first", "fresh-second"),
      actor_type = c("enemy", "player"),
      turn_order = c(1L, 2L),
      stringsAsFactors = FALSE
    )
  }
  test_env$set_combat_state <- function(...) TRUE
  test_env$log_game_event <- function(...) TRUE

  result <- test_env$start_encounter_combat(18L)
  stopifnot(calls$rolls == 1L)
  stopifnot(identical(result$actor_id[[1L]], "fresh-first"))
})

test("player combat UI has no encounter or combat-start administration", {
  combat_module_file <- file.path(
    project_dir,
    "DND APP Drachuri Edition Player_v2", "server", "debug_combat_module.R"
  )
  combat_source <- paste(readLines(combat_module_file, warn = FALSE), collapse = "\n")
  stopifnot(!grepl('ns("encounter_id")', combat_source, fixed = TRUE))
  stopifnot(!grepl('ns("roll_init")', combat_source, fixed = TRUE))
  stopifnot(!grepl('ns("init_combat")', combat_source, fixed = TRUE))
})

cat("\n", tests_run, " tests passed.\n", sep = "")
