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
level_file <- file.path(
  project_dir,
  "DND APP Drachuri Edition Player_v2", "server", "level_module.R"
)
feature_file <- file.path(
  project_dir,
  "DND APP Drachuri Edition Player_v2", "shared", "class_feature_core.R"
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

load_functions(global_file, c(
  "character_save_payload", "restore_sindre", "reset_class_uses_for_rest",
  "calc_auto_ac_for_char", "get_effective_max_hp"
))
load_functions(
  session_file,
  c(
    "calculate_hp_damage", "next_combat_turn",
    "empty_player_live_snapshot", "get_player_live_snapshot",
    "build_snapshot_encounter_actors", "start_encounter_combat"
  )
)
load_functions(
  feature_file,
  c(
    "CLASS_FEATURE_TAGS", "CLASS_FEATURE_INTEGRATION",
    "class_feature_integration", "audit_class_level_integration",
    "CLASS_SPELL_DEFINITIONS",
    "character_level_choice", "class_spellcasting_ability",
    "character_proficiency_bonus", "class_spell_save_dc", "spell_choice_is_unlocked",
    "spell_subclass_is_unlocked",
    "get_unlocked_class_spells", "scale_class_spell", "CLASS_FEATURE_MECHANICS",
    "infer_class_feature_tags", "class_feature_metadata",
    "get_unlocked_class_features", "get_unlocked_combat_actions",
    "resolve_class_action_damage", "resolve_class_action_healing",
    "class_action_use_available", "mark_class_action_used",
    "class_resource_remaining", "spend_class_resource", "barbarian_rage_maximum",
    "new_turn_action_budget", "turn_action_field", "turn_action_available",
    "spend_turn_action", "grant_turn_action", "attacks_per_attack_action",
    "spend_attack_from_budget", "grant_bonus_attack",
    "apply_unlocked_class_effects"
  )
)
load_functions(
  level_file,
  c(
    "normalise_character_classes", "level_options_for",
    "level_features_for", "apply_ability_score_increase",
    "class_max_level", "apply_character_level_up"
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

test("combat does not skip actors tied on turn order", {
  tied <- data.frame(
    actor_id = c("p1", "e1", "p2"), actor_type = c("player", "enemy", "player"),
    turn_order = c(1L, 2L, 2L), stringsAsFactors = FALSE
  )
  combat <- data.frame(
    current_turn_order = 2L, round_number = 4L,
    active_actor_type = "enemy", active_actor_id = "e1", stringsAsFactors = FALSE
  )
  result <- test_env$next_combat_turn(tied, combat)
  stopifnot(identical(result$actor_id, "p2"), identical(result$round_number, 4L))
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
    if (grepl("FROM encounter_effects", sql, fixed = TRUE)) return(data.frame())
    if (grepl("FROM encounter_summons", sql, fixed = TRUE)) return(data.frame())
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
  stopifnot(calls$queries == 9L)
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
  stopifnot(!grepl('uiOutput(session$ns("initiative_ui"))', combat_source, fixed = TRUE))
})

test("party HUD is the authoritative combat roster", {
  party_hud_file <- file.path(
    project_dir,
    "DND APP Drachuri Edition Player_v2", "server", "party_hud_module.R"
  )
  party_hud_source <- paste(readLines(party_hud_file, warn = FALSE), collapse = "\n")
  stopifnot(grepl('snapshot$enemies', party_hud_source, fixed = TRUE))
  stopifnot(grepl('active_actor_id', party_hud_source, fixed = TRUE))
  stopifnot(grepl('Initiative ', party_hud_source, fixed = TRUE))
})

test("Character and Level camp shortcuts route to different modules", {
  camp_module_file <- file.path(
    project_dir,
    "DND APP Drachuri Edition Player_v2", "server", "camp_module.R"
  )
  camp_source <- paste(readLines(camp_module_file, warn = FALSE), collapse = "\n")
  stopifnot(grepl('character    = "character"', camp_source, fixed = TRUE))
  stopifnot(grepl('level        = "level"', camp_source, fixed = TRUE))
})

test("Level tab separates unlocked progression from the level-up workflow", {
  level_source <- paste(readLines(level_file, warn = FALSE), collapse = "\n")
  stopifnot(grepl('actionButton(ns("open_level_up"), "Level Up"', level_source, fixed = TRUE))
  stopifnot(grepl('get_unlocked_class_features(display_character_r())', level_source, fixed = TRUE))
  stopifnot(grepl('get_unlocked_class_spells(display_character_r())', level_source, fixed = TRUE))
  stopifnot(grepl('output$unlocked_proficiencies_ui', level_source, fixed = TRUE))
})

test("every level-one class feature has an integration review", {
  class_defs <- list(
    Rogue = list(levels = list("1" = list(features = list(
      sneak_attack = list(name = "Sneak Attack"), thieves_cant = list(name = "Thieves' Cant")
    )))),
    Fighter = list(levels = list("1" = list(features = list(
      fighting_style = list(name = "Fighting Style"), second_wind = list(name = "Second Wind")
    )))),
    Barbarian = list(levels = list("1" = list(features = list(
      rage = list(name = "Rage"), unarmoured_defence = list(name = "Unarmoured Defence")
    )))),
    "Hanianol Sorcerer" = list(levels = list("1" = list(features = list(
      blood_magic = list(name = "Blood Magic"), fae_blooded = list(name = "Fae Blooded"),
      bloodthirsty = list(name = "Bloodthirsty Action")
    )))),
    "Na'Haran Sorcerer" = list(levels = list("1" = list(features = list(
      desert_wild_magic = list(name = "Desert Wild Magic"),
      survival_mastery = list(name = "Survival Mastery"),
      water_channeler = list(name = "Water Channeler")
    ))))
  )
  audit <- test_env$audit_class_level_integration(1L, class_defs)
  stopifnot(nrow(audit) == 12L)
  stopifnot(!any(audit$status == "unreviewed"))
  stopifnot(identical(audit$status[audit$feature_id == "water_channeler"], "working"))
})

test("every level-two class feature has an integration review", {
  class_defs <- list(
    Rogue = list(levels = list("2" = list(features = list(cunning_action = list(name = "Cunning Action"))))),
    Fighter = list(levels = list("2" = list(features = list(action_surge = list(name = "Action Surge"))))),
    Barbarian = list(levels = list("2" = list(features = list(
      reckless_attack = list(name = "Reckless Attack"), danger_sense = list(name = "Danger Sense")
    )))),
    "Hanianol Sorcerer" = list(levels = list("2" = list(features = list(
      natural_magic = list(name = "Natural Magic")
    )))),
    "Na'Haran Sorcerer" = list(levels = list("2" = list(features = list(
      mind_bender = list(name = "Mind Bender"), detect_undead = list(name = "Detect Undead")
    ))))
  )
  audit <- test_env$audit_class_level_integration(2L, class_defs)
  stopifnot(nrow(audit) == 7L)
  stopifnot(!any(audit$status == "unreviewed"))
  stopifnot(identical(audit$status[audit$feature_id == "action_surge"], "working"))
})

test("every level-three base and subclass feature has an integration review", {
  feature <- function(id) setNames(list(list(name = id)), id)
  subclass <- function(...) list(levels = list("3" = list(features = do.call(c, list(...)))))
  class_defs <- list(
    Rogue = list(
      levels = list("3" = list(features = feature("subclass_unlock"))),
      subclasses = list(
        Thief = subclass(feature("fast_hands"), feature("second_story_work")),
        Assassin = subclass(feature("assassinate"), feature("bonus_proficiencies"))
      )
    ),
    Fighter = list(
      levels = list("3" = list(features = feature("subclass_unlock"))),
      subclasses = list(
        Champion = subclass(feature("improved_critical")),
        `Battle Master` = subclass(feature("combat_superiority"), feature("student_of_war"))
      )
    ),
    Barbarian = list(
      levels = list("3" = list(features = feature("subclass_unlock"))),
      subclasses = list(
        Berserker = subclass(feature("frenzy")),
        `Totem Warrior` = subclass(feature("spirit_totem"))
      )
    ),
    `Hanianol Sorcerer` = list(
      levels = list("3" = list(features = feature("subclass_unlock"))),
      subclasses = list(
        `Path of the Ancestor` = subclass(feature("seer")),
        `Heart Eater` = subclass(feature("exquisite_taste"), feature("shadow_step"))
      )
    ),
    `Na'Haran Sorcerer` = list(
      levels = list("3" = list(features = feature("subclass_unlock"))),
      subclasses = list(
        `Path of the Warrior` = subclass(feature("spellsword")),
        `Path of the Prophet` = subclass(feature("wild_insight"))
      )
    )
  )
  audit <- test_env$audit_class_level_integration(3L, class_defs)
  stopifnot(nrow(audit) == 19L)
  stopifnot(!any(audit$status == "unreviewed"))
  stopifnot(identical(audit$status[audit$feature_id == "improved_critical"], "working"))
  stopifnot(identical(audit$subclass[audit$feature_id == "assassinate"], "Assassin"))
  stopifnot(all(audit$status == "working"))
})

test("level-three subclass choices are conditional and saved", {
  fighter_options <- list(Fighter = list("3" = list(
    list(id = "subclass", label = "Subclass", options = c("Champion", "Battle Master")),
    list(id = "battle_master_manoeuvre_1", label = "Manoeuvre 1", requires_subclass = "Battle Master", options = c("Trip Attack", "Riposte")),
    list(id = "student_of_war_tool", label = "Tool", requires_subclass = "Battle Master", options = c("Smith's Tools"))
  )))
  class_defs <- list(Fighter = list(
    levels = list("2" = list(features = list()), "3" = list(features = list(subclass_unlock = list(name = "Subclass")))),
    subclasses = list(
      Champion = list(levels = list("3" = list(features = list()))),
      `Battle Master` = list(levels = list("3" = list(features = list())))
    )
  ))
  champion <- test_env$level_options_for("Fighter", 3L, fighter_options, class_defs, "Champion")
  battle_master <- test_env$level_options_for("Fighter", 3L, fighter_options, class_defs, "Battle Master")
  stopifnot(identical(vapply(champion, `[[`, character(1), "id"), "subclass"))
  stopifnot(length(battle_master) == 3L)

  char <- list(
    meta = list(name = "Tactician"),
    build = list(class = "Fighter", level = 2L, classes = list(list(class = "Fighter", level = 2L, subclass = ""))),
    abilities = list(str = 14L), resources = list(hp = list(cur = 12L, max = 12L, temp = 0L))
  )
  result <- test_env$apply_character_level_up(
    char, 1L,
    selections = list(subclass = "Battle Master", battle_master_manoeuvre_1 = "Trip Attack", student_of_war_tool = "Smith's Tools"),
    class_defs = class_defs, level_options = fighter_options
  )
  stopifnot(identical(result$character$build$classes[[1]]$subclass, "Battle Master"))
  stopifnot(identical(result$character$build$level_choices$Fighter[["3"]]$student_of_war_tool, "Smith's Tools"))
})

test("class resource pools spend charges and respect their maximum", {
  char <- list(resources = list())
  stopifnot(test_env$class_resource_remaining(char, "superiority_dice", 4L) == 4L)
  spent <- test_env$spend_class_resource(char, "superiority_dice", 4L, "short_rest")
  stopifnot(test_env$class_resource_remaining(spent, "superiority_dice", 4L) == 3L)
  for (i in 1:3) spent <- test_env$spend_class_resource(spent, "superiority_dice", 4L, "short_rest")
  stopifnot(is.null(test_env$spend_class_resource(spent, "superiority_dice", 4L, "short_rest")))
})

test("every level-four class feature has an integration review", {
  class_names <- c("Rogue", "Fighter", "Barbarian", "Hanianol Sorcerer", "Na'Haran Sorcerer")
  class_defs <- stats::setNames(lapply(class_names, function(class_name) {
    list(levels = list("4" = list(features = list(
      asi = list(name = "Ability Score Improvement")
    ))))
  }), class_names)
  audit <- test_env$audit_class_level_integration(4L, class_defs)
  stopifnot(nrow(audit) == 5L)
  stopifnot(all(audit$status == "working"))
  stopifnot(all(audit$feature_id == "asi"))
})

test("ability score improvement enforces its cap and updates Constitution HP", {
  char <- list(
    build = list(class = "Fighter", level = 4L, classes = list(list(class = "Fighter", level = 4L, subclass = "Champion"))),
    abilities = list(str = 18L, dex = 20L, con = 11L, int = 10L, bld_str = 10L, cha = 10L),
    resources = list(hp = list(cur = 24L, max = 30L, temp = 0L))
  )
  stronger <- test_env$apply_ability_score_increase(char, c("str", "str"))
  stopifnot(stronger$abilities$str == 20L)
  healthier <- test_env$apply_ability_score_increase(char, c("con", "cha"))
  stopifnot(healthier$abilities$con == 12L)
  stopifnot(healthier$resources$hp$max == 34L)
  stopifnot(healthier$resources$hp$cur == 28L)
  capped <- tryCatch(test_env$apply_ability_score_increase(char, c("dex", "cha")), error = identity)
  stopifnot(inherits(capped, "error"))
})

test("every level-five class feature has an integration review", {
  class_defs <- list(
    Rogue = list(levels = list("5" = list(features = list(uncanny_dodge = list(name = "Uncanny Dodge"))))),
    Fighter = list(levels = list("5" = list(features = list(extra_attack = list(name = "Extra Attack"))))),
    Barbarian = list(levels = list("5" = list(features = list(
      extra_attack = list(name = "Extra Attack"), fast_movement = list(name = "Fast Movement")
    )))),
    `Hanianol Sorcerer` = list(levels = list("5" = list(features = list(
      thermal_wild_magic = list(name = "Thermal Wild Magic")
    )))),
    `Na'Haran Sorcerer` = list(levels = list("5" = list(features = list(
      adept_sorcerer = list(name = "Adept Sorcerer")
    ))))
  )
  audit <- test_env$audit_class_level_integration(5L, class_defs)
  stopifnot(nrow(audit) == 6L)
  stopifnot(all(audit$status == "working"))
})

test("Extra Attack shares one action while Frenzy grants only one attack", {
  budget <- test_env$new_turn_action_budget("turn")
  first <- test_env$spend_attack_from_budget(budget, 2L)
  stopifnot(first$actions == 0L, first$attack_chain == 1L)
  second <- test_env$spend_attack_from_budget(first, 2L)
  stopifnot(second$actions == 0L, second$attack_chain == 0L)
  stopifnot(is.null(test_env$spend_attack_from_budget(second, 2L)))
  frenzy <- test_env$grant_bonus_attack(second)
  bonus <- test_env$spend_attack_from_budget(frenzy, 2L)
  stopifnot(bonus$bonus_attacks == 0L, bonus$attack_chain == 0L)
})

test("both sorcerer traditions unlock their selected thermal spell", {
  make_char <- function(class_name, choice) list(
    build = list(
      class = class_name, level = 5L,
      classes = list(list(class = class_name, level = 5L, subclass = "")),
      level_choices = setNames(list(list("5" = list(thermal_path = choice))), class_name)
    ),
    abilities = list(bld_str = 16L)
  )
  class_defs <- list(
    `Hanianol Sorcerer` = list(levels = list(), subclasses = list()),
    `Na'Haran Sorcerer` = list(levels = list(), subclasses = list())
  )
  spell_defs <- test_env$CLASS_SPELL_DEFINITIONS
  spell_defs$hanianol_exothermic_burst <- spell_defs$exothermic_burst
  spell_defs$hanianol_exothermic_burst$class <- "Hanianol Sorcerer"
  spell_defs$hanianol_endothermic_grasp <- spell_defs$endothermic_grasp
  spell_defs$hanianol_endothermic_grasp$class <- "Hanianol Sorcerer"
  hanianol <- test_env$get_unlocked_class_spells(
    make_char("Hanianol Sorcerer", "Exothermic"), spell_defs = spell_defs, class_defs = class_defs
  )
  naharan <- test_env$get_unlocked_class_spells(
    make_char("Na'Haran Sorcerer", "Endothermic"), spell_defs = spell_defs, class_defs = class_defs
  )
  stopifnot(any(vapply(hanianol, function(spell) identical(spell$name, "Exothermic Burst"), logical(1))))
  stopifnot(any(vapply(naharan, function(spell) identical(spell$name, "Endothermic Grasp"), logical(1))))
})

test("every level-six class and subclass feature has an integration review", {
  feature <- function(id) setNames(list(list(name = id)), id)
  subclass <- function(...) list(levels = list("6" = list(features = do.call(c, list(...)))))
  class_defs <- list(
    Rogue = list(levels = list("6" = list(features = feature("expertise")))),
    Fighter = list(levels = list("6" = list(features = feature("asi")))),
    Barbarian = list(
      levels = list("6" = list(features = list())),
      subclasses = list(
        Berserker = subclass(feature("mindless_rage")),
        `Totem Warrior` = subclass(feature("aspect_of_the_beast"))
      )
    ),
    `Hanianol Sorcerer` = list(
      levels = list("6" = list(features = list())),
      subclasses = list(
        `Path of the Ancestor` = subclass(feature("balance")),
        `Heart Eater` = subclass(feature("predator"))
      )
    ),
    `Na'Haran Sorcerer` = list(
      levels = list("6" = list(features = feature("electromagnetic"))),
      subclasses = list(
        `Path of the Warrior` = subclass(feature("combat_magic")),
        `Path of the Prophet` = subclass(feature("divination"), feature("mislead"))
      )
    )
  )
  audit <- test_env$audit_class_level_integration(6L, class_defs)
  stopifnot(nrow(audit) == 10L)
  stopifnot(!any(audit$status == "unreviewed"))
  stopifnot(all(audit$status %in% c("working", "manual")))
})

test("Rogue level-six Expertise promotes two distinct saved skills", {
  class_defs <- list(Rogue = list(
    levels = list("6" = list(features = list(expertise = list(name = "Expertise")))), subclasses = list()
  ))
  char <- list(
    build = list(
      class = "Rogue", level = 6L, classes = list(list(class = "Rogue", level = 6L, subclass = "")),
      level_choices = list(Rogue = list("6" = list(
        expertise_skill_1 = "stealth", expertise_skill_2 = "investigation"
      )))
    ),
    prof = list(skills = list(stealth = "Proficient", investigation = "Proficient"))
  )
  result <- test_env$apply_unlocked_class_effects(char, class_defs)
  stopifnot(identical(result$prof$skills$stealth, "Expertise"))
  stopifnot(identical(result$prof$skills$investigation, "Expertise"))
})

test("long rest removes temporary Balance ability boosts", {
  test_env$validate_character <- identity
  char <- list(
    abilities = list(str = 15L, dex = 13L, con = 12L),
    resources = list(class_uses = list(balance = list(used = TRUE, recharge = "long_rest"))),
    status = list(balance_boosts = c("str", "dex", "con"), raging = FALSE)
  )
  rested <- test_env$reset_class_uses_for_rest(char, "long_rest")
  stopifnot(rested$abilities$str == 14L, rested$abilities$dex == 12L, rested$abilities$con == 11L)
  stopifnot(!isTRUE(rested$resources$class_uses$balance$used))
})

test("Hanianol Blood Magic prevents natural Sindre recovery", {
  test_env$validate_character <- identity
  hanianol <- list(
    meta = list(race = "Human"),
    build = list(class = "Hanianol Sorcerer"),
    resources = list(sindre = list(cur = 5, total = 30, regen = 4))
  )
  other <- hanianol
  other$build$class <- "Fighter"
  stopifnot(test_env$restore_sindre(hanianol, hours = 6)$resources$sindre$cur == 5)
  stopifnot(test_env$restore_sindre(other, hours = 6)$resources$sindre$cur > 5)
})

test("Barbarian Unarmoured Defence adds Constitution to AC", {
  test_env$validate_character <- identity
  test_env$inventory_normalize <- function(items) data.frame()
  test_env$get_character_ability_mod <- function(char, stat) {
    floor((as.integer(char$abilities[[stat]]) - 10L) / 2L)
  }
  barbarian <- list(
    build = list(class = "Barbarian"), abilities = list(dex = 14L, con = 16L),
    inventory = list(items = data.frame())
  )
  fighter <- barbarian
  fighter$build$class <- "Fighter"
  stopifnot(test_env$calc_auto_ac_for_char(barbarian) == 15L)
  stopifnot(test_env$calc_auto_ac_for_char(fighter) == 12L)
})

test("Defence Fighting Style adds one AC only while armoured", {
  test_env$validate_character <- identity
  test_env$get_character_ability_mod <- function(char, stat) 2L
  test_env$get_character_prof_bonus <- function(char) 2L
  test_env$inventory_normalize <- function(items) items
  test_env$armor_meta_defaults_global <- identity
  armour <- data.frame(
    type = "armor", equipped = TRUE, in_bag = FALSE,
    stringsAsFactors = FALSE
  )
  armour$meta <- I(list(list(base_ac = 12, type = "Light", proficient = FALSE)))
  fighter <- list(
    build = list(
      class = "Fighter",
      level_choices = list(Fighter = list("1" = list(fighting_style = "Defence")))
    ),
    abilities = list(dex = 14L, con = 12L), inventory = list(items = armour)
  )
  no_style <- fighter
  no_style$build$level_choices <- list()
  stopifnot(test_env$calc_auto_ac_for_char(fighter) == 15L)
  stopifnot(test_env$calc_auto_ac_for_char(no_style) == 14L)
})

test("Second Wind heals by die plus Fighter level and recharges on a short rest", {
  test_env$normalise_character_classes <- function(char, class_defs = NULL) char$build$classes
  character <- list(
    build = list(classes = list(list(class = "Fighter", level = 4L))),
    resources = list(class_uses = list())
  )
  action <- test_env$CLASS_FEATURE_MECHANICS[["Fighter::1::second_wind"]]$action
  healing <- test_env$resolve_class_action_healing(
    action, character, roll_function = function(expr) list(total = 6L)
  )
  stopifnot(healing == 10L)
  character <- test_env$mark_class_action_used(character, action)
  stopifnot(!test_env$class_action_use_available(character, action))
  test_env$validate_character <- identity
  character <- test_env$reset_class_uses_for_rest(character, "short_rest")
  stopifnot(test_env$class_action_use_available(character, action))
})

test("turn action budget tracks action types and Action Surge", {
  budget <- test_env$new_turn_action_budget("round-1-player")
  stopifnot(test_env$turn_action_available(budget, "action"))
  stopifnot(test_env$turn_action_available(budget, "bonus_action"))
  budget <- test_env$spend_turn_action(budget, "bonus_action")
  stopifnot(!test_env$turn_action_available(budget, "bonus_action"))
  stopifnot(is.null(test_env$spend_turn_action(budget, "bonus_action")))
  budget <- test_env$spend_turn_action(budget, "action")
  budget <- test_env$grant_turn_action(budget)
  stopifnot(test_env$turn_action_available(budget, "action"))
})

test("standard combat actions are wired to shared action and effect mechanics", {
  combat_file <- file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "debug_combat_module.R")
  combat_source <- paste(readLines(combat_file, warn = FALSE), collapse = "\n")
  for (control in c(
    "standard_dash", "standard_disengage", "standard_hide", "standard_dodge",
    "standard_help", "standard_grapple", "standard_escape_grapple", "standard_ready",
    "confirm_force_end_turn"
  )) stopifnot(grepl(paste0("input$", control), combat_source, fixed = TRUE))
  stopifnot(grepl('projected_move > base_speed_ft()', combat_source, fixed = TRUE))
  stopifnot(grepl('get_opportunity_attackers(eid', combat_source, fixed = TRUE))
  stopifnot(grepl('player_reaction_available(FALSE)', combat_source, fixed = TRUE))
})

test("combat UI exposes lifecycle, summon control, and module shortcuts", {
  player_file <- file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "debug_combat_module.R")
  control_file <- file.path(project_dir, "DND APP Drachuri Edition 2 Control", "control_app", "modules", "control_live_combat_module.R")
  server_file <- file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server.R")
  player_source <- paste(readLines(player_file, warn = FALSE), collapse = "\n")
  control_source <- paste(readLines(control_file, warn = FALSE), collapse = "\n")
  server_source <- paste(readLines(server_file, warn = FALSE), collapse = "\n")
  stopifnot(grepl('input$shortcut_armoury', server_source, fixed = TRUE))
  stopifnot(grepl('input$shortcut_magic', server_source, fixed = TRUE))
  stopifnot(grepl('input$shortcut_combat', server_source, fixed = TRUE))
  stopifnot(grepl('create_encounter_summon(', player_source, fixed = TRUE))
  stopifnot(grepl('input$confirm_end_combat', control_source, fixed = TRUE))
})

test("Magic and Balance use authoritative class progression", {
  magic_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "magic_module.R"), warn = FALSE), collapse = "\n")
  level_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "level_module.R"), warn = FALSE), collapse = "\n")
  combat_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "debug_combat_module.R"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("get_unlocked_class_spells(validate_character(state$char))", magic_source, fixed = TRUE))
  stopifnot(!grepl("Ember Oath", magic_source, fixed = TRUE))
  stopifnot(grepl('input$confirm_balance', level_source, fixed = TRUE))
  stopifnot(!grepl('input$confirm_balance', combat_source, fixed = TRUE))
  stopifnot(grepl('input$confirm_action_override', combat_source, fixed = TRUE))
})

test("level-two abilities are connected to action and skill interfaces", {
  combat_file <- file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "debug_combat_module.R")
  skills_file <- file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "skills_module.R")
  combat_source <- paste(readLines(combat_file, warn = FALSE), collapse = "\n")
  skills_source <- paste(readLines(skills_file, warn = FALSE), collapse = "\n")
  stopifnot(grepl('input$use_action_surge', combat_source, fixed = TRUE))
  stopifnot(grepl('input$open_cunning_action', combat_source, fixed = TRUE))
  stopifnot(grepl('input$toggle_reckless', combat_source, fixed = TRUE))
  stopifnot(grepl('input$use_detect_undead', combat_source, fixed = TRUE))
  stopifnot(grepl('input$use_mind_bender', skills_source, fixed = TRUE))
  stopifnot(grepl('has_feature("danger_sense")', skills_source, fixed = TRUE))
})

test("level up requires and stores a subclass choice", {
  class_defs <- list(
    Rogue = list(
      levels = list("3" = list(features = list(
        unlock = list(name = "Roguish Archetype", desc = "Choose an archetype.")
      ))),
      subclasses = list(
        Thief = list(levels = list("3" = list(features = list(
          hands = list(name = "Fast Hands", desc = "Use objects quickly.")
        )))),
        Assassin = list(levels = list())
      )
    )
  )
  level_options <- list(Rogue = list("3" = list(list(
    id = "subclass", label = "Choose Archetype", options = c("Thief", "Assassin")
  ))))
  character <- list(
    build = list(class = "Rogue", level = 2L, classes = list(
      list(class = "Rogue", level = 2L, subclass = "")
    )),
    resources = list(hp = list(cur = 12L, max = 15L, temp = 0L))
  )

  missing_choice <- tryCatch(
    test_env$apply_character_level_up(
      character, 1L, class_defs = class_defs, level_options = level_options
    ),
    error = identity
  )
  stopifnot(inherits(missing_choice, "error"))

  result <- test_env$apply_character_level_up(
    character, 1L, selections = list(subclass = "Thief"), hp_gain = 5L,
    class_defs = class_defs, level_options = level_options,
    timestamp = as.POSIXct("2026-01-01", tz = "UTC")
  )
  stopifnot(result$new_level == 3L)
  stopifnot(identical(result$subclass, "Thief"))
  stopifnot(identical(result$character$build$classes[[1L]]$subclass, "Thief"))
  stopifnot(identical(result$character$build$level_choices$Rogue[["3"]]$subclass, "Thief"))
  stopifnot(identical(result$character$resources$hp$max, 20L))
  stopifnot(any(vapply(result$features, function(x) identical(x$name, "Fast Hands"), logical(1))))
})

test("multiclass level up advances only the selected class", {
  class_defs <- list(
    Rogue = list(levels = list("4" = list(features = list())), subclasses = list()),
    Fighter = list(levels = list("3" = list(features = list())), subclasses = list())
  )
  character <- list(
    build = list(class = "Rogue", level = 5L, classes = list(
      list(class = "Rogue", level = 3L, subclass = ""),
      list(class = "Fighter", level = 2L, subclass = "")
    )),
    resources = list(hp = list(cur = 20L, max = 20L, temp = 0L))
  )
  result <- test_env$apply_character_level_up(
    character, 2L, hp_gain = 4L,
    class_defs = class_defs, level_options = list(),
    timestamp = as.POSIXct("2026-01-01", tz = "UTC")
  )
  stopifnot(result$character$build$classes[[1L]]$level == 3L)
  stopifnot(result$character$build$classes[[2L]]$level == 3L)
  stopifnot(result$character$build$level == 6L)
})

test("ability score improvement applies two required stat increases", {
  class_defs <- list(Rogue = list(
    levels = list(
      "3" = list(features = list()),
      "4" = list(features = list(asi = list(
        name = "Ability Score Improvement", desc = "Increase ability scores."
      )))
    ),
    subclasses = list()
  ))
  character <- list(
    build = list(class = "Rogue", level = 3L, classes = list(
      list(class = "Rogue", level = 3L, subclass = "")
    )),
    abilities = list(str = 18L, dex = 20L, con = 10L, int = 10L, bld_str = 10L, cha = 10L),
    resources = list(hp = list(cur = 20L, max = 20L, temp = 0L))
  )
  result <- test_env$apply_character_level_up(
    character, 1L,
    selections = list(asi_first = "str", asi_second = "str"),
    class_defs = class_defs, level_options = list(),
    timestamp = as.POSIXct("2026-01-01", tz = "UTC")
  )
  stopifnot(result$character$abilities$str == 20L)
  stopifnot(result$character$abilities$dex == 20L)
})

test("class features receive tags and explicit combat actions", {
  class_defs <- list(
    "Na'Haran Sorcerer" = list(
      levels = list(
        "1" = list(features = list(water_channeler = list(
          name = "Water Channeler", desc = "Drain water to damage a creature. Costs Sindre."
        ))),
        "11" = list(features = list(improved_channeling = list(
          name = "Improved Water Channeler", desc = "Drain more water and damage."
        )))
      ),
      subclasses = list()
    )
  )
  character <- list(build = list(
    class = "Na'Haran Sorcerer", level = 11L,
    classes = list(list(class = "Na'Haran Sorcerer", level = 11L, subclass = ""))
  ))
  actions <- test_env$get_unlocked_combat_actions(character, class_defs)
  stopifnot(length(actions) == 1L)
  stopifnot(identical(actions[[1L]]$action$name, "Improved Water Channeler"))
  stopifnot(all(c("ability", "combat", "spell") %in% actions[[1L]]$tags))
})

test("class combat actions resolve fixed-percent and dice damage", {
  percent <- test_env$resolve_class_action_damage(
    list(damage = list(mode = "percent_max_hp", value = 0.10, type = "necrotic")),
    target_max_hp = 95L,
    char = list(abilities = list(str = 10L))
  )
  stopifnot(percent$amount == 9L)
  stopifnot(identical(percent$damage_type, "necrotic"))

  dice <- test_env$resolve_class_action_damage(
    list(damage = list(mode = "dice_plus_modifier", value = "1d8", stat = "str", type = "piercing")),
    target_max_hp = 10L,
    char = list(abilities = list(str = 16L)),
    roll_function = function(expr) list(total = 5L)
  )
  stopifnot(dice$amount == 8L)
})

test("combat abilities use the defined damage-trait helper", {
  combat_source <- paste(readLines(file.path(
    project_dir, "DND APP Drachuri Edition Player_v2", "server", "debug_combat_module.R"
  ), warn = FALSE), collapse = "\n")
  stopifnot(!grepl("get_damage_traits(", combat_source, fixed = TRUE))
  stopifnot(grepl("get_character_damage_traits(target_char)", combat_source, fixed = TRUE))
})

test("fixed class spells are gated by level and saved speciality", {
  test_env$normalise_character_classes <- function(char, class_defs = NULL) char$build$classes
  character <- list(build = list(
    classes = list(list(class = "Hanianol Sorcerer", level = 2L, subclass = "")),
    level_choices = list("Hanianol Sorcerer" = list(
      "2" = list(natural_specialty = "Disease")
    ))
  ))
  spells <- test_env$get_unlocked_class_spells(character)
  stopifnot(identical(names(spells), "wasting_sickness"))
  stopifnot(spells$wasting_sickness$cost == 20L)
  stopifnot(identical(spells$wasting_sickness$resolution$ability, "con"))
})

test("offensive magic includes executable resolution and level scaling", {
  spell <- test_env$CLASS_SPELL_DEFINITIONS$lightbringer
  stopifnot(identical(spell$action_type, "action"))
  stopifnot(spell$range_ft == 60L)
  stopifnot(identical(spell$resolution$type, "saving_throw"))
  stopifnot(identical(spell$damage$dice, "3d8"))
  stopifnot(identical(spell$damage$scaling[["17"]], "5d8"))
  stopifnot(identical(spell$effects[[1L]]$value, "blinded"))
})

test("Natural Magic applies its highest eligible class-level upgrade", {
  vines <- test_env$scale_class_spell(test_env$CLASS_SPELL_DEFINITIONS$grasping_vines, 15L)
  rain <- test_env$scale_class_spell(test_env$CLASS_SPELL_DEFINITIONS$calling_rain, 11L)
  stopifnot(vines$target$size_ft == 40L)
  stopifnot(isTRUE(vines$resolved_upgrade$save_disadvantage))
  stopifnot(rain$target$size_ft == 60L)
  stopifnot(rain$resolved_upgrade$cold_lightning_modifier == 4L)
})

test("blood magic uses Blood Strength save DC and multiclass proficiency", {
  test_env$normalise_character_classes <- function(char, class_defs = NULL) char$build$classes
  character <- list(
    build = list(classes = list(
      list(class = "Na'Haran Sorcerer", level = 6L),
      list(class = "Rogue", level = 3L)
    )),
    abilities = list(int = 18L, cha = 18L, bld_str = 16L)
  )
  dc <- test_env$class_spell_save_dc(
    character, test_env$CLASS_SPELL_DEFINITIONS$lightbringer
  )
  stopifnot(test_env$character_proficiency_bonus(character) == 4L)
  stopifnot(identical(test_env$class_spellcasting_ability("Hanianol Sorcerer"), "bld_str"))
  stopifnot(identical(test_env$class_spellcasting_ability("Na'Haran Sorcerer"), "bld_str"))
  stopifnot(dc == 15L)
})

test("subclass magic requires the matching subclass", {
  test_env$normalise_character_classes <- function(char, class_defs = NULL) char$build$classes
  ancestor <- list(build = list(
    classes = list(list(class = "Hanianol Sorcerer", level = 10L, subclass = "Path of the Ancestor")),
    level_choices = list("Hanianol Sorcerer" = list("2" = list(natural_specialty = "Plants")))
  ))
  heart_eater <- ancestor
  heart_eater$build$classes[[1L]]$subclass <- "Heart Eater"
  stopifnot("ancestral_elemental" %in% names(test_env$get_unlocked_class_spells(ancestor)))
  stopifnot(!"shadow_step" %in% names(test_env$get_unlocked_class_spells(ancestor)))
  stopifnot("shadow_step" %in% names(test_env$get_unlocked_class_spells(heart_eater)))
})

test("unlocked class effects grant proficiency without downgrading expertise", {
  class_defs <- list(
    "Hanianol Sorcerer" = list(
      levels = list(
        "1" = list(features = list(fae_blooded = list(
          name = "Fae Blooded", desc = "Double proficiency in Survival."
        ))),
        "2" = list(features = list(natural_magic = list(
          name = "Natural Magic", desc = "Gain Medicine."
        )))
      ),
      subclasses = list()
    )
  )
  character <- list(
    build = list(
      class = "Hanianol Sorcerer", level = 2L,
      classes = list(list(class = "Hanianol Sorcerer", level = 2L, subclass = ""))
    ),
    prof = list(skills = list(medicine = "Expertise"), tools = list()),
    combat_profile = list(
      resistances = "fire", immunities = character(), vulnerabilities = "cold"
    )
  )
  result <- test_env$apply_unlocked_class_effects(character, class_defs)
  stopifnot(identical(result$prof$skills$survival, "Expertise"))
  stopifnot(identical(result$prof$skills$medicine, "Expertise"))
  stopifnot(identical(result$combat_profile$resistances, "fire"))
  stopifnot(identical(result$combat_profile$vulnerabilities, "cold"))
  stopifnot(length(result$derived_effects$sources[["skill:medicine"]]) == 1L)
})

test("conditional resistance is tracked without becoming permanent", {
  class_defs <- list(Barbarian = list(
    levels = list("1" = list(features = list(rage = list(
      name = "Rage", desc = "Resistance to physical damage while raging."
    )))),
    subclasses = list()
  ))
  character <- list(
    build = list(class = "Barbarian", level = 1L, classes = list(
      list(class = "Barbarian", level = 1L, subclass = "")
    )),
    prof = list(skills = list(), tools = list()),
    combat_profile = list()
  )
  result <- test_env$apply_unlocked_class_effects(character, class_defs)
  stopifnot(length(result$combat_profile$resistances) == 0L)
  stopifnot(length(result$derived_effects$conditional) == 1L)
})

test("exhaustion halves effective HP without mutating stored maximum", {
  test_env$validate_character <- identity
  character <- list(
    resources = list(hp = list(max = 20L, cur = 20L, temp = 0L)),
    status = list(exhaustion = 4L)
  )
  stopifnot(test_env$get_effective_max_hp(character) == 10L)
  stopifnot(character$resources$hp$max == 20L)
  stopifnot(test_env$get_effective_max_hp(character) == 10L)
  character$status$exhaustion <- 3L
  stopifnot(test_env$get_effective_max_hp(character) == 20L)
})

cat("\n", tests_run, " tests passed.\n", sep = "")
