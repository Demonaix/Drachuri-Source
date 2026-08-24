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
relational_inventory_file <- file.path(
  project_dir,
  "DND APP Drachuri Edition Player_v2", "shared", "relational_inventory_core.R"
)
enemy_generator_file <- file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","enemy_generator_core.R")
magic_data_file <- file.path(project_dir,"DND APP Drachuri Edition Player_v2","plug","magic_data.R")
glyph_core_file <- file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","glyph_core.R")

test_env <- new.env(parent = baseenv())
test_env$`%||%` <- function(a, b) if (!is.null(a)) a else b
test_env$COMBAT_WEAPON_STATS <- c("str", "dex", "con", "int", "bld_str", "cha")
sys.source(magic_data_file,envir=test_env)

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
  "armor_meta_defaults_global", "calc_auto_ac_for_char", "get_effective_max_hp", "get_weapon_hit_bonus",
  "starting_character_hp", "camp_gathering_yield", "consume_heart_sindre",
  "character_subclass_names", "magical_identity_labels", "skill_identity_labels",
  "character_magic_types", "bloodlust_bite_required", "merchant_pricing_multiplier",
  "merchant_item_stock_weight", "merchant_haggle_terms",
  "food_item_meta", "food_rations_available", "consume_food_ration", "spoil_character_food",
  "camp_foraging_reward", "merchant_stock_category", "merchant_select_stock",
  "equipped_magical_traits", "new_character", "validate_character", "inventory_empty", "inventory_normalize",
  "weapon_meta_defaults_global", "standard_spear_attack_modes",
  "upgrade_weapon_damage_die", "standard_weapon_attack_modes",
  "normalise_weapon_attack_modes", "merge_legacy_weapon_mode_items",
  "combat_grid_distance_ft", "combat_grid_shortest_path", "combat_line_tiles",
  "combat_attack_geometry", "combat_hide_dc"
))
load_functions(relational_inventory_file, c("equipment_material_is_eligible", "inventory_item_category", "equipment_adjusted_value"))
load_functions(enemy_generator_file, c("resolve_layered_damage_traits", "enemy_is_animal", "roll_enemy_mundane_loot", "roll_enemy_food_loot"))
load_functions(glyph_core_file,c("GLYPH_PHYSICAL_TYPES","glyph_character_level","glyph_unlocked_ranks","glyph_mastery_level","glyph_material_requirement","glyph_counter_outcome","glyph_default_identity","normalize_weapon_enchantments","validate_ward_resistances","glyph_zone_colour"))
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

test("higher-level character creation calculates class HP", {
  classes<-list(Rogue=list(hit_die=8L),Barbarian=list(hit_die=12L))
  stopifnot(identical(test_env$starting_character_hp("Rogue",1L,10L,classes),8L))
  stopifnot(identical(test_env$starting_character_hp("Rogue",5L,10L,classes),28L))
  stopifnot(identical(test_env$starting_character_hp("Barbarian",5L,14L,classes),50L))
  stopifnot(identical(test_env$starting_character_hp("Rogue",3L,6L,classes),12L))
})

test("camp gathering checks map to bounded supply yields", {
  stopifnot(identical(vapply(c(1L,9L,10L,14L,15L,19L,20L,24L,25L,40L),test_env$camp_gathering_yield,integer(1)),c(0L,0L,1L,1L,2L,2L,3L,3L,4L,4L)))
})

test("foraging checks turn their yield into named perishable food", {
  low<-test_env$camp_foraging_reward(9L,1L);mushrooms<-test_env$camp_foraging_reward(12L,1L);rabbit<-test_env$camp_foraging_reward(25L,1L)
  stopifnot(is.null(low),mushrooms$name=="Bluecap Mushrooms",mushrooms$meta$ration_value==1L,mushrooms$meta$shelf_life_days==2L)
  stopifnot(rabbit$name=="Trapped Rabbit",rabbit$meta$ration_value==4L,rabbit$meta$shelf_life_days==2L)
})

test("heart consumption grants predictable temporary Sindre", {
  ordinary <- test_env$consume_heart_sindre(90L, 100L, 3L, 25L, FALSE)
  stopifnot(identical(ordinary$current, 100L))
  stopifnot(identical(ordinary$temporary, 18L))
  stopifnot(identical(ordinary$temporary_gained, 15L))

  heart_eater <- test_env$consume_heart_sindre(10L, 100L, 3L, 25L, TRUE)
  stopifnot(identical(heart_eater$current, 100L))
  stopifnot(identical(heart_eater$temporary, 28L))
  stopifnot(identical(heart_eater$temporary_gained, 25L))
})

test("character identities use current subclasses and every skill family", {
  char <- list(build = list(classes = list(
    list(class = "Rogue", subclass = "Thief"),
    list(class = "Hanianol Sorcerer", subclass = "Heart Eater")
  )))
  stopifnot("Heart Eater" %in% test_env$character_subclass_names(char))
  magic <- test_env$magical_identity_labels(110, 75, 0, 0, 0,
                                            test_env$character_subclass_names(char))
  stopifnot(identical(magic$aspect, "Devourer"))
  stopifnot(grepl("Abyss Storm Devourer", magic$title, fixed = TRUE))

  history <- test_env$skill_identity_labels(c("History", "Medicine"), c(8, 7, 2, 1))
  stopifnot(identical(history$core, "Chronicler"))
  stopifnot(identical(history$aspect, "Physician"))
  all_skills <- c("Athletics","Clutch","Wrestling","Throwing","Dead Lift","Acrobatics","Sleight of Hand","Stealth","Precision","Endurance","Tolerance","Fortitude","Arcana","History","Investigation","Nature","Religion","Analysis","Perception","Survival","Insight","Medicine","Animal Handling","Mandred Connection","Deception","Intimidation","Persuasion","Performance","Presence")
  stopifnot(all(vapply(all_skills, function(skill) {
    test_env$skill_identity_labels(c(skill, skill), c(5, 4))$core != "Wanderer"
  }, logical(1))))
})

test("magic schools derive from sorcerer progression rather than manual toggles", {
  hanianol <- list(build = list(classes = list(
    list(class = "Hanianol Sorcerer", level = 5L, subclass = "Heart Eater")
  ), level_choices = list("Hanianol Sorcerer" = list(
    "5" = list(thermal_path = "Exothermic")
  ))))
  stopifnot(identical(test_env$character_magic_types(hanianol), c("Mechanical", "Natural", "Fire")))
  naharan <- list(build = list(classes = list(
    list(class = "Na'Haran Sorcerer", level = 5L, subclass = "Path of the Prophet")
  ), level_choices = list("Na'Haran Sorcerer" = list(
    "5" = list(thermal_path = "Endothermic")
  ))))
  stopifnot(identical(test_env$character_magic_types(naharan), c("Mechanical", "Cold")))
  rogue <- list(build = list(classes = list(list(class = "Rogue", level = 5L))))
  stopifnot(length(test_env$character_magic_types(rogue)) == 0L)
})

test("bloodlust triggers a bite on stage-three fumbles and stage-four turns", {
  make_char <- function(stage, active=TRUE) list(
    resources=list(blood=list(addiction=list(stage=stage))), status=list(bloodlust=active)
  )
  test_env$validate_character <- identity
  stopifnot(test_env$bloodlust_bite_required(make_char(3L), 1L, FALSE))
  stopifnot(!test_env$bloodlust_bite_required(make_char(3L), 2L, FALSE))
  stopifnot(test_env$bloodlust_bite_required(make_char(4L), NA, TRUE))
  stopifnot(!test_env$bloodlust_bite_required(make_char(4L, FALSE), NA, TRUE))
})

test("rest fire visuals only reference assets shipped with each app", {
  app_dirs <- c(
    file.path(project_dir, "DND APP Drachuri Edition Player_v2"),
    file.path(project_dir, "DND APP Drachuri Edition 2 Control")
  )
  for (app_dir in app_dirs) {
    module_text <- paste(readLines(file.path(app_dir, "server", "rest_module.R"), warn = FALSE), collapse = "\n")
    stopifnot(file.exists(file.path(app_dir, "www", "fire.png")))
    stopifnot(!grepl('src = "embers.png"', module_text, fixed = TRUE))
    stopifnot(grepl('alt = "Unlit campfire"', module_text, fixed = TRUE))
  }
})

test("Control map builder exposes Ravine as a movement-blocking sight line", {
  builder <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition 2 Control", "control_app", "modules", "control_map_builder_module.R"), warn=FALSE), collapse="\n")
  colours <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition 2 Control", "www", "js", "mapBuilder2d.js"), warn=FALSE), collapse="\n")
  stopifnot(grepl('"ravine"', builder, fixed=TRUE))
  stopifnot(grepl('paint_blocks_movement", value = TRUE', builder, fixed=TRUE))
  stopifnot(grepl('paint_blocks_vision", value = FALSE', builder, fixed=TRUE))
  stopifnot(grepl('ravine: "#111015"', colours, fixed=TRUE))
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

test("combat skips defeated enemies but retains downed players", {
  defeated <- data.frame(
    actor_id = c("p1", "e1", "p2"), actor_type = c("player", "enemy", "player"),
    turn_order = c(1L, 2L, 3L), current_hp = c(0L, 0L, 5L), is_active = TRUE,
    stringsAsFactors = FALSE
  )
  result <- test_env$next_combat_turn(defeated, data.frame(current_turn_order = 1L, round_number = 2L))
  stopifnot(identical(result$actor_id, "p2"))
  result <- test_env$next_combat_turn(defeated, data.frame(current_turn_order = 3L, round_number = 2L))
  stopifnot(identical(result$actor_id, "p1"), identical(result$round_number, 3L))
})

test("legacy spear variants become one weapon with three attack modes", {
  items <- data.frame(
    id = c("thrown", "two"), name = c("Spear (Thrown)", "Spear (Two-handed)"),
    type = "weapon", desc = "", value = 0, weight = 3, qty = 1,
    equipped = c(TRUE, FALSE), in_bag = FALSE, edit = FALSE,
    stringsAsFactors = FALSE
  )
  items$meta <- list(list(), list())
  merged <- test_env$merge_legacy_weapon_mode_items(items)
  stopifnot(nrow(merged) == 1L, identical(merged$name[[1L]], "Spear"))
  stopifnot(length(merged$meta[[1L]]$attack_modes) == 3L)
})

test("standard weapon families expose their valid attack modes", {
  longsword <- test_env$standard_weapon_attack_modes(
    "Ember Longsword", list(damage1 = "1d8", dmg_type1 = "slashing")
  )
  dagger <- test_env$standard_weapon_attack_modes(
    "Frostbite Dagger", list(damage1 = "1d6", dmg_type1 = "piercing")
  )
  javelin <- test_env$standard_weapon_attack_modes(
    "Comet Javelin", list(damage1 = "1d8", dmg_type1 = "piercing")
  )
  stopifnot(length(longsword) == 2L, identical(longsword[[2L]]$damage, "1d10"))
  stopifnot(length(dagger) == 2L, identical(dagger[[1L]]$stat, "finesse"))
  stopifnot(length(javelin) == 2L, identical(javelin[[2L]]$long_range_ft, 120L))
})

test("combat pathfinding routes around walls and charges the actual route", {
  tiles <- expand.grid(x = 1:5, y = 1:5)
  tiles$map_id <- 1L; tiles$move_cost <- 1; tiles$blocks_movement <- FALSE
  tiles$blocks_vision <- FALSE; tiles$terrain <- "grass"
  tiles$blocks_movement[tiles$x == 3L & tiles$y <= 4L] <- TRUE
  tiles$terrain[tiles$x == 3L & tiles$y <= 4L] <- "wall"
  route <- test_env$combat_grid_shortest_path(tiles, data.frame(), 1L, 3L, 5L, 3L, map_id = 1L)
  stopifnot(isTRUE(route$ok), route$cost_ft > 20L)
  stopifnot(!any(route$path$x == 3L & route$path$y <= 4L))
  too_slow <- test_env$combat_grid_shortest_path(tiles, data.frame(), 1L, 3L, 5L, 3L, map_id = 1L, max_cost_ft = 20L)
  stopifnot(!isTRUE(too_slow$ok))
  shadow_step <- test_env$combat_grid_shortest_path(tiles, data.frame(), 1L, 3L, 5L, 3L, map_id = 1L, allow_blocked = TRUE)
  stopifnot(isTRUE(shadow_step$ok), shadow_step$cost_ft == 20L)
})

test("weapon range and sight-blocking terrain gate attacks", {
  tiles <- expand.grid(x = 1:6, y = 1:3)
  tiles$map_id <- 1L; tiles$blocks_vision <- FALSE; tiles$terrain <- "grass"
  clear <- test_env$combat_attack_geometry(tiles, 1L, 2L, 5L, 2L, 20L, 60L, 1L)
  stopifnot(isTRUE(clear$ok), clear$distance_ft == 20L, isTRUE(clear$normal_range))
  tiles$blocks_vision[tiles$x == 3L & tiles$y == 2L] <- TRUE
  blocked <- test_env$combat_attack_geometry(tiles, 1L, 2L, 5L, 2L, 20L, 60L, 1L)
  stopifnot(!isTRUE(blocked$ok), !isTRUE(blocked$line_clear))
  distant <- test_env$combat_attack_geometry(tiles, 1L, 1L, 6L, 1L, 5L, 20L, 1L)
  stopifnot(!isTRUE(distant$in_range))
})

test("hide difficulty reflects terrain, light, cover and perception", {
  grass <- test_env$combat_hide_dc(12L,"grass","full",FALSE,TRUE)
  forest <- test_env$combat_hide_dc(12L,"forest","full",FALSE,TRUE)
  dark <- test_env$combat_hide_dc(12L,"grass","dark",FALSE,TRUE)
  wall_cover <- test_env$combat_hide_dc(12L,"grass","full",TRUE,FALSE)
  watchful <- test_env$combat_hide_dc(17L,"forest","full",FALSE,TRUE)
  stopifnot(forest < grass, dark < grass, wall_cover < grass, watchful > forest)
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

test("Body armour, shield and helm use separate non-stacking slots", {
  test_env$validate_character <- identity
  test_env$get_character_ability_mod <- function(char, stat) 2L
  test_env$get_character_prof_bonus <- function(char) 2L
  test_env$inventory_normalize <- function(items) items
  armour <- data.frame(type="armor", equipped=TRUE, in_bag=FALSE, stringsAsFactors=FALSE)
  armour <- armour[rep(1,4),,drop=FALSE]
  armour$meta <- I(list(
    list(base_ac=12,type="Light",proficient=FALSE,equipment_slot="body"),
    list(base_ac=2,type="Shield",proficient=FALSE,equipment_slot="shield",ac_bonus=2),
    list(base_ac=0,type="Unarmoured",proficient=FALSE,equipment_slot="head",ac_bonus=1),
    list(base_ac=0,type="Unarmoured",proficient=FALSE,equipment_slot="head",ac_bonus=1)
  ))
  char <- list(build=list(class="Fighter",level_choices=list()),abilities=list(dex=14L,con=12L),inventory=list(items=armour))
  stopifnot(test_env$calc_auto_ac_for_char(char) == 17L)
  char$inventory$items <- armour[-1,,drop=FALSE]
  stopifnot(test_env$calc_auto_ac_for_char(char) == 15L)
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
  stopifnot(grepl('get_opportunity_attackers(', combat_source, fixed = TRUE))
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
  stopifnot(identical(actions[[1L]]$action$damage$mode, "dice"))
  stopifnot(identical(actions[[1L]]$action$damage$value, "2d8"))
  stopifnot(identical(actions[[1L]]$action$required_target_condition, "grappled"))
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

test("all Natural Magic specialties retain their homebrew combat contracts", {
  spells <- test_env$CLASS_SPELL_DEFINITIONS[c(
    "grasping_vines", "calling_rain", "call_beast", "wasting_sickness"
  )]
  stopifnot(length(spells) == 4L)
  stopifnot(identical(
    unname(vapply(spells, function(spell) unname(spell$choice[["natural_specialty"]]), character(1))),
    c("Plants", "Rain", "Animals", "Disease")
  ))
  stopifnot(all(vapply(spells, function(spell) spell$cost == 20L, logical(1))))
  stopifnot(all(vapply(spells, function(spell) identical(spell$action_type, "action"), logical(1))))
  stopifnot(all(vapply(spells, function(spell) isTRUE(spell$concentration), logical(1))))

  vines <- spells$grasping_vines
  stopifnot(identical(vines$resolution$ability, "str"))
  stopifnot(any(vapply(vines$effects, function(effect) identical(effect$value, "difficult"), logical(1))))
  stopifnot(any(vapply(vines$effects, function(effect) identical(effect$value, "restrained"), logical(1))))

  rain <- spells$calling_rain
  stopifnot(any(vapply(rain$effects, function(effect) identical(effect$damage_type, "fire") && effect$value == -4L, logical(1))))
  stopifnot(any(vapply(rain$effects, function(effect) identical(effect$damage_type, c("cold", "lightning")) && effect$value == 2L, logical(1))))

  beast <- spells$call_beast
  stopifnot(identical(beast$effects[[1L]]$type, "summon"))
  stopifnot(identical(unname(unlist(beast$effects[[1L]]$scaling[c("2", "11", "15")])), c("CR 1/2", "CR 1", "CR 2")))

  disease <- spells$wasting_sickness
  stopifnot(identical(disease$resolution$ability, "con"))
  stopifnot(identical(disease$effects[[1L]]$value, "poisoned"))
  stopifnot(identical(disease$target$origin, "self"))
  stopifnot(identical(disease$target$affects, "all_other_creatures"))

  hand <- test_env$CLASS_SPELL_DEFINITIONS$flesh_witherers_hand
  stopifnot(identical(hand$damage$dice, "2d8"))
  stopifnot(identical(hand$damage$scaling[["11"]], "3d8"))
  stopifnot(identical(hand$damage$scaling[["17"]], "4d8"))
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

test("weapon attack bonus includes material and build quality", {
  test_env$validate_character <- identity
  test_env$get_character_ability_mod <- function(char, stat) 3L
  test_env$get_character_prof_bonus <- function(char) 3L
  weapon <- data.frame(
    stat = "dex", proficient = TRUE, to_hit_bonus = 1,
    material_attack_bonus = 1, quality_attack_bonus = 2
  )
  stopifnot(test_env$get_weapon_hit_bonus(list(), weapon) == 10L)
})

test("finesse weapons automatically use the better STR or DEX modifier", {
  test_env$validate_character <- identity
  test_env$get_character_ability_mod <- function(char, stat) {
    if (identical(stat, "str")) 1L else if (identical(stat, "dex")) 4L else 0L
  }
  test_env$get_character_prof_bonus <- function(char) 3L
  weapon <- data.frame(
    stat = "finesse", proficient = TRUE, to_hit_bonus = 0,
    material_attack_bonus = 0, quality_attack_bonus = 0
  )
  stopifnot(test_env$get_weapon_hit_bonus(list(), weapon) == 7L)
})

test("material eligibility enforces Fae and Boss loot rules", {
  iron <- data.frame(excluded_enemy_types = I(list("Fae")), required_characteristics = I(list(character())))
  titanium <- data.frame(excluded_enemy_types = I(list(character())), required_characteristics = I(list("Boss")))
  stopifnot(!test_env$equipment_material_is_eligible(iron, "Fae", character()))
  stopifnot(test_env$equipment_material_is_eligible(iron, "Bandit", character()))
  stopifnot(!test_env$equipment_material_is_eligible(titanium, "Bandit", character()))
  stopifnot(test_env$equipment_material_is_eligible(titanium, "Bandit", "Boss"))
  postgres_iron <- data.frame(excluded_enemy_types = "{Fae}", required_characteristics = "{}")
  postgres_boss <- data.frame(excluded_enemy_types = "{}", required_characteristics = "{Boss}")
  stopifnot(!test_env$equipment_material_is_eligible(postgres_iron, "Fae", character()))
  stopifnot(test_env$equipment_material_is_eligible(postgres_boss, "Bandit", "Boss"))
})

test("equipment material and build quality alter merchant base value", {
  item<-list(type="weapon",value=100,meta=list(material_cost_modifier=.5,quality_cost_modifier=1.4))
  stopifnot(test_env$equipment_adjusted_value(item)==70)
  item$meta$material_cost_modifier<-5
  stopifnot(test_env$equipment_adjusted_value(item)==700)
})

test("ordinary non-equipment loot receives an authoritative category", {
  mundane <- data.frame(type = "item", meta = I(list(list())))
  crafting <- data.frame(type = "item", meta = I(list(list(category = "crafting"))))
  potion <- data.frame(type = "consumable", meta = I(list(list())))
  stopifnot(identical(test_env$inventory_item_category(mundane), "mundane_loot"))
  stopifnot(identical(test_env$inventory_item_category(crafting), "crafting"))
  stopifnot(identical(test_env$inventory_item_category(potion), "consumable"))
})

test("non-animal NPCs receive a small universal mundane loot roll", {
  catalogue<-list(
    spoon=list(meta=list(category="mundane_loot")),
    rope=list(meta=list(category="mundane_loot")),
    torch=list(meta=list(category="mundane_loot")),
    sword=list(meta=list(category="weapon"))
  )
  humanoid<-test_env$roll_enemy_mundane_loot(catalogue,"Bandit",character(),count=2L)
  stopifnot(length(humanoid)==2L,all(humanoid%in%c("spoon","rope","torch")))
  stopifnot(!length(test_env$roll_enemy_mundane_loot(catalogue,"Animal",character(),count=2L)))
  stopifnot(!length(test_env$roll_enemy_mundane_loot(catalogue,"Bandit","Animal",count=2L)))
})

test("food supplies rations, is consumed earliest-first, and spoils by campaign day", {
  food<-data.frame(id=c("bread","cheese"),name=c("Bread","Cheese"),type="consumable",desc="",value=1,weight=1,qty=1,equipped=FALSE,in_bag=FALSE,meta=I(list(list(category="food",ration_value=2L,shelf_life_days=1L,food_acquired_day=5L),list(category="food",ration_value=4L,shelf_life_days=10L,food_acquired_day=5L))),edit=FALSE,stringsAsFactors=FALSE)
  x<-list(meta=list(day=5L),inventory=list(items=food));stopifnot(test_env$food_rations_available(x)==6L)
  eaten<-test_env$consume_food_ration(x);stopifnot(eaten$applied,eaten$item_name=="Bread",eaten$remaining==5L)
  spoiled<-test_env$spoil_character_food(eaten$char,7L);stopifnot("Bread"%in%spoiled$spoiled,test_env$food_rations_available(spoiled$char,7L)==4L)
})

test("non-animal NPCs receive food but animal NPCs do not", {
  catalogue<-list(apple=list(meta=list(category="food")),bread=list(meta=list(category="food")),rope=list(meta=list(category="mundane_loot")))
  stopifnot(length(test_env$roll_enemy_food_loot(catalogue,"Bandit",character(),count=2L))==2L)
  stopifnot(!length(test_env$roll_enemy_food_loot(catalogue,"Animal",character(),count=1L)))
})

test("merchant temperament and haggling produce bounded buy and sell prices", {
  hard_fail <- test_env$merchant_haggle_terms(100, "buy", "hard", 5)
  hard_win <- test_env$merchant_haggle_terms(100, "buy", "hard", 21)
  generous_sell <- test_env$merchant_haggle_terms(100, "sell", "generous", 15)
  rejected_once <- test_env$merchant_haggle_terms(100, "buy", "fair", 14, dc_penalty = 2L)
  stopifnot(hard_fail$dc == 16L, !hard_fail$success, hard_fail$price == 125)
  stopifnot(hard_win$success, hard_win$price < hard_fail$price)
  stopifnot(generous_sell$success, generous_sell$price >= 75)
  stopifnot(rejected_once$dc == 15L, !rejected_once$success)
  stopifnot(test_env$merchant_haggle_terms(100,"buy","fair",13,pricing_style="cheap")$price==65L)
  stopifnot(test_env$merchant_haggle_terms(100,"buy","fair",13,pricing_style="very_expensive")$price==165L)
})

test("general merchants draw a balanced mix instead of armour-heavy stock", {
  make<-function(id,type="item",category="mundane_loot")list(id=id,name=id,type=type,meta=list(category=category))
  catalogue<-c(lapply(1:20,function(i)make(paste0("armour",i),"armor","")),lapply(1:20,function(i)make(paste0("general",i))),lapply(1:20,function(i)make(paste0("food",i),"consumable","food")),lapply(1:10,function(i)make(paste0("weapon",i),"weapon","")))
  stock<-test_env$merchant_select_stock(catalogue,14L,"general");categories<-vapply(stock,test_env$merchant_stock_category,character(1))
  stopifnot(length(stock)==14L,sum(categories=="armour")<=3L,any(categories=="general"),any(categories=="food"),any(categories=="weapon"))
})

test("equipped magical armour contributes its damage and condition wards", {
  test_env$validate_character<-identity;test_env$inventory_normalize<-function(items)items
  item<-data.frame(id="ward",name="Ward",type="armor",desc="",value=1,weight=1,qty=1,equipped=TRUE,in_bag=FALSE,meta=I(list(list(is_magical=TRUE,resistances=c("Piercing","Fire"),condition_immunities="frightened"))),edit=FALSE,stringsAsFactors=FALSE)
  traits<-test_env$equipped_magical_traits(list(inventory=list(items=item)))
  stopifnot(setequal(traits$resistances,c("piercing","fire")),identical(traits$condition_immunities,"frightened"))
  item$equipped<-FALSE;stopifnot(!length(test_env$equipped_magical_traits(list(inventory=list(items=item)))$resistances))
})

test("layered NPC damage traits escalate duplicates and resolve conflicts", {
  traits <- test_env$resolve_layered_damage_traits(list(
    list(resistances=c("fire","cold"),vulnerabilities="acid"),
    list(resistances=c("fire","acid"),immunities="poison",condition_immunities="frightened")
  ))
  stopifnot("fire"%in%traits$immunities,"poison"%in%traits$immunities)
  stopifnot("cold"%in%traits$resistances,!"acid"%in%traits$resistances,!"acid"%in%traits$vulnerabilities)
  stopifnot("frightened"%in%traits$condition_immunities)
})

test("glyph rules cover rune, ward, and replenishable enhancement contracts", {
  rune<-test_env$get_glyph_rule("rune","Arcane",7,"Metal")
  ward<-test_env$get_glyph_rule("ward","Major",7,"Heartwood Dust",20)
  enhancement<-test_env$get_glyph_rule("enhancement","Minor",7,enhancement_days=3)
  stopifnot(rune$instability_damage=="3d20",rune$arcane_score==27L,rune$active_time_rounds==6)
  stopifnot(ward$minimum_size_ft==15,ward$crafting_hours==4,ward$arcane_score==27L)
  stopifnot(enhancement$crafting_hours==1,enhancement$arcane_score==37L,enhancement$cost_multiplier==3L)
  stopifnot(enhancement$damage=="1d4",test_env$RUNE_DAMAGE_BY_RANK[["Arcane"]]=="3d6")
  stopifnot(test_env$glyph_counter_outcome("rune",20,21)$outcome=="unstable")
  stopifnot(test_env$glyph_counter_outcome("ward",20,20)$outcome=="broken")
  identity<-test_env$glyph_default_identity("rune","Minor","Fire","Clay")
  stopifnot(identity$name=="Minor Fire Rune",grepl("1d6 Fire",identity$description,fixed=TRUE))
  stopifnot(test_env$glyph_mastery_level(list(build=list(level=1L)))==1L)
  stopifnot(identical(test_env$glyph_unlocked_ranks(list(build=list(level=10L))),c("Minor","Major","Arcane")))
  stopifnot(identical(test_env$validate_ward_resistances("Minor","Fire"),"Fire"))
  stopifnot(setequal(test_env$validate_ward_resistances("Major",c("Cold","Fire")),c("Cold","Fire")))
  stopifnot(setequal(test_env$validate_ward_resistances("Arcane",c("Fire","Slashing")),c("Fire","Slashing")))
  stopifnot(inherits(try(test_env$validate_ward_resistances("Minor","Slashing"),silent=TRUE),"try-error"))
  stopifnot(inherits(try(test_env$validate_ward_resistances("Major",c("Fire","Slashing")),silent=TRUE),"try-error"))
  stopifnot(test_env$glyph_zone_colour("Fire")=="#d63b2f",test_env$glyph_zone_colour("ward")=="#3a78c2")
})

test("manual magic types use the same damage vocabulary as glyphs", {
  x <- list(build=list(), magic=list(types=c("Chemical","Thunder","Nuclear","Bogus")))
  stopifnot(setequal(test_env$character_magic_types(x), c("Poison","Thunder","Radiant")))
  stopifnot(all(test_env$GLYPH_DAMAGE_TYPES %in% c("Fire","Cold","Lightning","Acid","Poison","Force","Necrotic","Radiant","Psychic","Thunder")))
})

cat("\n", tests_run, " tests passed.\n", sep = "")
