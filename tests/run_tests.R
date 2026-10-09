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
combat_map_file <- file.path(project_dir,"DND APP Drachuri Edition 2 Control","server","combat_map_logic.R")
map_builder_file <- file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_map_builder_module.R")
encounter_generator_file <- file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_encounter_generator_core.R")
control_global_file <- file.path(project_dir,"DND APP Drachuri Edition 2 Control","global.R")

test_env <- new.env(parent = baseenv())
test_env$`%||%` <- function(a, b) if (!is.null(a)) a else b
test_env$APP_SAVE_VERSION <- 2L
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
  "starting_character_hp", "camp_gathering_yield", "consume_heart_sindre", "consume_blood_effects", "blood_sindre_per_pint", "blood_donor_sindre_cost_per_pint", "blood_draw_result",
  "character_subclass_names", "magical_identity_labels", "skill_identity_labels",
  "character_magic_types", "bloodlust_bite_required", "merchant_pricing_multiplier",
  "merchant_item_stock_weight", "merchant_haggle_terms",
  "food_item_meta", "food_rations_available", "consume_food_ration", "spoil_character_food",
  "camp_foraging_reward", "merchant_stock_category", "merchant_select_stock",
  "required_intake", "warmth_requirement_hours",
  "character_skill_modifier", "skill_card_count_for_rank", "character_skill_cards", "resolve_skill_card_roll", "triggered_skill_cards", "party_skill_support_result",
  "status_condition_definitions", "active_character_conditions", "character_condition_status_cards", "encounter_condition_values", "exhaustion_effect_text", "character_roll_status",
  "equipped_magical_traits", "new_character", "validate_character", "inventory_empty", "inventory_normalize",
  "weapon_meta_defaults_global", "standard_spear_attack_modes", "equipment_thumbnail_src",
  "upgrade_weapon_damage_die", "standard_weapon_attack_modes",
  "normalise_weapon_attack_modes", "merge_legacy_weapon_mode_items",
  "combat_grid_distance_ft", "combat_grid_shortest_path", "combat_line_tiles",
  "combat_attack_geometry", "combat_hide_dc", "dice_card_supported_sides", "dice_card_src", "dice_result_card_ui", "modifier_result_card_ui", "manual_dice_values"
  , "combat_player_marker_palette", "combat_player_marker_assignments",
  "combat_player_marker_identity", "apply_unique_player_3d_colours"
))
load_functions(relational_inventory_file, c("equipment_material_is_eligible", "inventory_item_category", "equipment_adjusted_value"))
load_functions(enemy_generator_file, c("enemy_special_attack", "enemy_attack_catalog", "enemy_loot_catalog", "resolve_layered_damage_traits", "enemy_is_animal", "roll_enemy_mundane_loot", "roll_enemy_food_loot", "npc_feature_definition", "npc_feature_catalogue", "npc_feature_effect_summary"))
load_functions(glyph_core_file,c("GLYPH_KNOTS","GLYPH_PHYSICAL_TYPES","glyph_character_level","glyph_unlocked_ranks","glyph_mastery_level","glyph_unlocked_types","glyph_type_unlocked","glyph_material_requirement","glyph_counter_outcome","glyph_default_identity","normalize_weapon_enchantments","validate_ward_resistances","glyph_zone_colour"))
load_functions(combat_map_file,c("empty_map_tiles","create_square_map_tiles"))
load_functions(map_builder_file,c("control_map_presets","generate_control_map_tiles"))
load_functions(encounter_generator_file,c("encounter_damage_average","encounter_template_threat","encounter_party_budget","compose_encounter_draft","encounter_draft_dimensions","encounter_draft_positions"))
load_functions(
  session_file,
  c(
    "calculate_hp_damage", "next_combat_turn",
    "empty_player_live_snapshot", "filter_player_snapshot_visibility", "get_player_live_snapshot", "session_notification_dedupe_key",
    "build_snapshot_encounter_actors", "get_encounter_actors", "start_encounter_combat",
    "session_watch_slots"
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

test("skill card requirements follow training rank", {
  stopifnot(identical(test_env$skill_card_count_for_rank("None"), 0L))
  stopifnot(identical(test_env$skill_card_count_for_rank("Proficient"), 1L))
  stopifnot(identical(test_env$skill_card_count_for_rank("Expertise"), 2L))
})

test("trained character skill cards are persisted and rank-limited", {
  original_validator <- test_env$validate_character
  test_env$validate_character <- identity
  on.exit(test_env$validate_character <- original_validator, add = TRUE)
  character <- list(prof = list(skills = list(athletics = "Expertise"), skill_cards = list(
    athletics = c("reliable", "wild_card", "inspired", "unknown")
  )))
  stopifnot(identical(test_env$character_skill_cards(character, "Athletics"), c("reliable", "wild_card")))
  character$prof$skills$athletics <- "Proficient"
  stopifnot(identical(test_env$character_skill_cards(character, "athletics"), "reliable"))
})

test("skill card roll effects combine independently", {
  safe_wild <- test_env$resolve_skill_card_roll(3L, 5L, 3L, c("reliable", "wild_card"), wild_roll = 1L)
  stopifnot(safe_wild$card_bonus == 0L, safe_wild$total == 8L)
  inspired_wild <- test_env$resolve_skill_card_roll(18L, 5L, 3L, c("inspired", "wild_card"), wild_roll = 6L)
  stopifnot(inspired_wild$card_bonus == 6L, inspired_wild$total == 29L)
  quiet_wild <- test_env$resolve_skill_card_roll(10L, 2L, 3L, "wild_card", wild_roll = 4L)
  stopifnot(quiet_wild$card_bonus == 0L, quiet_wild$total == 12L)
  stopifnot(test_env$resolve_skill_card_roll(1L,0L,3L,"reliable")$card_bonus==0L)
  stopifnot(test_env$resolve_skill_card_roll(4L,0L,3L,"reliable")$card_bonus==3L)
  stopifnot(test_env$resolve_skill_card_roll(5L,0L,3L,"reliable")$card_bonus==0L)
  stopifnot(test_env$resolve_skill_card_roll(17L,0L,3L,"inspired")$card_bonus==0L)
  stopifnot(test_env$resolve_skill_card_roll(18L,0L,3L,"inspired")$card_bonus==3L)
})

test("skill result displays only cards that actually trigger", {
  quiet<-test_env$resolve_skill_card_roll(11L,2L,3L,c("reliable","inspired","wild_card"),wild_roll=3L)
  low<-test_env$resolve_skill_card_roll(3L,2L,3L,c("reliable","inspired"))
  high<-test_env$resolve_skill_card_roll(19L,2L,3L,c("reliable","inspired"))
  wild<-test_env$resolve_skill_card_roll(11L,2L,3L,"wild_card",wild_roll=6L)
  stopifnot(!length(test_env$triggered_skill_cards(quiet)),identical(test_env$triggered_skill_cards(low),"reliable"),identical(test_env$triggered_skill_cards(high),"inspired"),identical(test_env$triggered_skill_cards(wild),"wild_card"))
})

test("required skill-card setup takes priority over opportunity prompts", {
  skills<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","skills_module.R"),warn=FALSE),collapse="\n")
  combat<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","debug_combat_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("required_character_setup",skills,fixed=TRUE))
  stopifnot(grepl("required_character_setup<-isolate",skills,fixed=TRUE))
  stopifnot(grepl("isTRUE(session$userData$required_character_setup)",combat,fixed=TRUE))
})

test("card-led rolls include sound, explanations, weapons, grapples and saves", {
  player_dir<-file.path(project_dir,"DND APP Drachuri Edition Player_v2")
  ui<-paste(readLines(file.path(player_dir,"ui.R"),warn=FALSE),collapse="\n")
  skills<-paste(readLines(file.path(player_dir,"server","skills_module.R"),warn=FALSE),collapse="\n")
  combat<-paste(readLines(file.path(player_dir,"server","debug_combat_module.R"),warn=FALSE),collapse="\n")
  sound<-file.path(player_dir,"www","assets","sounds","card-hover.mp3")
  stopifnot(file.exists(sound),file.info(sound)$size>1000L)
  stopifnot(grepl("drachuri-card-hover-audio",ui,fixed=TRUE),grepl("mouseover",ui,fixed=TRUE))
  stopifnot(grepl("What ",skills,fixed=TRUE),grepl("SKILL_DESC[[label]]",skills,fixed=TRUE))
  stopifnot(grepl("attack_ability=attack_ability",combat,fixed=TRUE),grepl("character_skill_modifier(char,\"Wrestling\"",combat,fixed=TRUE))
  stopifnot(grepl("attack_ability<-weapon_attack_ability(attacker_char,weapon_row)",combat,fixed=TRUE))
  stopifnot(grepl("save_detail<-list",combat,fixed=TRUE),grepl("combat-resolution-cards",combat,fixed=TRUE))
})

test("opportunity attacks retain their reaction context and allow melee only", {
  combat<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","debug_combat_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("is_reaction_attack <- isTRUE(verified_opportunity)",combat,fixed=TRUE))
  stopifnot(grepl("is_opp <- isTRUE(verified_opportunity)",combat,fixed=TRUE))
  stopifnot(grepl("Opportunity attacks require an equipped melee weapon",combat,fixed=TRUE))
  stopifnot(grepl("!vapply(seq_len(nrow(weapons)),function(i)weapon_is_ranged",combat,fixed=TRUE))
})

test("party skill support is useful, risky, and bounded", {
  mixed<-test_env$party_skill_support_result(16L,c(18L,12L,7L),3L);stopifnot(mixed$total==16L,mixed$support==0L)
  strong<-test_env$party_skill_support_result(16L,c(18L,19L,20L,17L),3L);stopifnot(strong$total==19L,strong$support==3L)
  poor<-test_env$party_skill_support_result(16L,c(2L,3L,4L,5L),3L);stopifnot(poor$total==13L,poor$support== -3L)
})

test("group skill checks persist every response and have a bounded response window", {
  migration<-paste(readLines(file.path(project_dir,"database","migrations","039_party_skill_check_contributions.sql"),warn=FALSE),collapse="\n")
  skills<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","skills_module.R"),warn=FALSE),collapse="\n")
  shared<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","session_db_core.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("party_skill_check_contributions",migration,fixed=TRUE))
  stopifnot(grepl("respond_party_skill_check",skills,fixed=TRUE),grepl("finalize_party_skill_check",skills,fixed=TRUE))
  stopifnot(grepl("party_skill_support_result(leader,helpers",shared,fixed=TRUE))
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

test("ordinary blood restores Sindre and Heart Eaters also heal", {
  char <- test_env$validate_character(list(
    resources = list(
      hp = list(cur = 10L, max = 20L),
      sindre = list(cur = 20L, total = 100L, temp = 0L),
      blood = list(addiction = list(current_day_intake = 0))
    ),
    status = list(needs_hours = list(blood = 18))
  ))
  ordinary <- test_env$consume_blood_effects(char, 0.5)
  stopifnot(ordinary$sindre_gained == 5L)
  stopifnot(ordinary$character$resources$sindre$cur == 25L)
  stopifnot(ordinary$character$resources$hp$cur == 10L)
  stopifnot(ordinary$character$resources$blood$addiction$current_day_intake == 0.5)
  stopifnot(ordinary$character$status$needs_hours$blood == 0)

  eater <- test_env$consume_blood_effects(char, 0.5, heart_eater = TRUE)
  stopifnot(eater$sindre_gained == 5L, eater$hp_gained == 2L)
  stopifnot(eater$character$resources$hp$cur == 12L)
})

test("bottled blood transfers capacity, usable Sindre and exhaustion separately", {
  char<-list(resources=list(sindre=list(cur=180,total=180,locked=160)))
  stopifnot(test_env$blood_sindre_per_pint(char)==42L)
  stopifnot(test_env$blood_donor_sindre_cost_per_pint(char)==22L)
  first<-test_env$blood_draw_result(0,1,0L);second<-test_env$blood_draw_result(1,2,0L)
  stopifnot(first$exhaustion_gained==0L,second$exhaustion_gained==2L,second$total_pints==3)
  refused<-try(test_env$blood_draw_result(7,2,0L),silent=TRUE)
  stopifnot(inherits(refused,"try-error"))
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

test("third-ranked skill differentiates otherwise similar skill identities", {
  stealthy <- test_env$skill_identity_labels(c("Survival","Perception","Stealth"),c(8,5,4,1,0))
  learned <- test_env$skill_identity_labels(c("Survival","Perception","History"),c(8,5,4,1,0))
  stopifnot(stealthy$title != learned$title,grepl("Elusive",stealthy$title),grepl("Learned",learned$title))
})

test("session notification keys deduplicate simultaneous party toasts", {
  moment<-as.POSIXct("2026-08-24 12:00:02",tz="UTC")
  a<-test_env$session_notification_dedupe_key(10L,"Party Athletics: 18",moment)
  b<-test_env$session_notification_dedupe_key(10L,"Party Athletics: 18",moment+1)
  different<-test_env$session_notification_dedupe_key(10L,"Party Athletics: 19",moment)
  stopifnot(identical(a,b),!identical(a,different))
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
  stopifnot(grepl('ravine=c(1,1,0)', gsub(" ","",builder), fixed=TRUE))
  stopifnot(grepl('updateCheckboxInput(session, "paint_blocks_movement"', builder, fixed=TRUE))
  stopifnot(grepl('updateCheckboxInput(session, "paint_blocks_vision"', builder, fixed=TRUE))
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
  tiles$terrain[tiles$x == 3L & tiles$y == 2L] <- "water"
  water_line <- test_env$combat_attack_geometry(tiles, 1L, 2L, 5L, 2L, 20L, 60L, 1L)
  stopifnot(isTRUE(water_line$ok), isTRUE(water_line$line_clear))
  tiles$blocks_vision[tiles$x == 3L & tiles$y == 2L] <- TRUE
  blocked <- test_env$combat_attack_geometry(tiles, 1L, 2L, 5L, 2L, 20L, 60L, 1L)
  stopifnot(!isTRUE(blocked$ok), !isTRUE(blocked$line_clear))
  distant <- test_env$combat_attack_geometry(tiles, 1L, 1L, 6L, 1L, 5L, 20L, 1L)
  stopifnot(!isTRUE(distant$in_range))
  open <- data.frame()
  normal <- test_env$combat_attack_geometry(open, 1L, 1L, 31L, 1L, 150L, 600L)
  long <- test_env$combat_attack_geometry(open, 1L, 1L, 32L, 1L, 150L, 600L)
  edge <- test_env$combat_attack_geometry(open, 1L, 1L, 121L, 1L, 150L, 600L)
  beyond <- test_env$combat_attack_geometry(open, 1L, 1L, 122L, 1L, 150L, 600L)
  stopifnot(isTRUE(normal$normal_range), !isTRUE(long$normal_range), isTRUE(long$in_range),
            edge$distance_ft == 600L, isTRUE(edge$in_range), !isTRUE(beyond$in_range))
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
    summons = data.frame(
      summon_uuid="s1",name="Called Wolf",hp_current=11L,hp_max=11L,temp_hp=0L,
      initiative=17L,turn_order=3L,is_active=TRUE,owner_actor_id="p1",
      attack_name="Bite",attack_bonus=4L,damage_expr="2d4+2",damage_type="piercing",
      portrait_asset="summoned-beast.png",stringsAsFactors=FALSE
    ),
    positions = data.frame(
      actor_id = c("p1", "e1", "s1"), actor_type = c("player", "enemy", "summon"),
      x = c(2L, 5L, 3L), y = c(3L, 6L, 3L), stringsAsFactors = FALSE
    )
  )
  combined <- test_env$build_snapshot_encounter_actors(snapshot)
  stopifnot(nrow(combined) == 3L)
  stopifnot(combined$x[combined$actor_id == "p1"] == 2L)
  stopifnot(combined$y[combined$actor_id == "e1"] == 6L)
  stopifnot(combined$current_hp[combined$actor_id == "e1"] == 8L)
  stopifnot(combined$owner_actor_id[combined$actor_id == "s1"] == "p1")
  stopifnot(combined$attack_name[combined$actor_id == "s1"] == "Bite")
})

test("player snapshots conceal exploration and hidden enemies", {
  base<-list(enemies=data.frame(enemy_uuid=c("e1","e2"),name=c("Seen","Hidden")),positions=data.frame(actor_type=c("enemy","enemy","player"),actor_id=c("e1","e2","p1")),effects=data.frame(target_actor_type="enemy",target_actor_id="e2",payload=I(list(list(condition="hidden")))),events=data.frame(actor_id=c("e1","e2"),target_id=c("p1","p1")),combat=data.frame(phase="combat"))
  filtered<-test_env$filter_player_snapshot_visibility(base);stopifnot(identical(as.character(filtered$enemies$enemy_uuid),"e1"),!"e2"%in%filtered$positions$actor_id)
  base$combat$phase<-"exploration";explore<-test_env$filter_player_snapshot_visibility(base);stopifnot(nrow(explore$enemies)==0L,!any(explore$positions$actor_type=="enemy"))
  base$combat$phase<-"combat";base$effects<-data.frame(target_actor_type=NA_character_,target_actor_id=NA_character_,payload=I(list(list(name="Calling Rain"))))
  area<-test_env$filter_player_snapshot_visibility(base);stopifnot(nrow(area$effects)==1L,nrow(area$enemies)==2L)
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

test("encounter actors tolerate empty enemy and summon categories", {
  test_env$get_encounter <- function(encounter_id) data.frame(id = encounter_id, session_id = 10L)
  test_env$get_session_players <- function(session_id) data.frame(
    character_id = "party-one", display_name = "Party One", current_hp = 12L,
    temp_hp = 0L, initiative = NA_integer_, turn_order = NA_integer_, is_active = TRUE,
    stringsAsFactors = FALSE
  )
  test_env$get_encounter_enemies <- function(encounter_id) data.frame()
  test_env$get_encounter_summons <- function(encounter_id) data.frame()
  test_env$get_encounter_positions <- function(encounter_id) data.frame()
  actors <- test_env$get_encounter_actors(19L)
  stopifnot(nrow(actors) == 1L)
  stopifnot(identical(actors$actor_id[[1L]], "party-one"))
  stopifnot(identical(actors$actor_type[[1L]], "player"))
})

test("control combat assigns missing actors to open map tiles", {
  control_combat <- paste(readLines(file.path(
    project_dir, "DND APP Drachuri Edition 2 Control", "control_app", "modules",
    "control_live_combat_module.R"
  ), warn = FALSE), collapse = "\n")
  stopifnot(grepl("ensure_actor_map_positions", control_combat, fixed = TRUE))
  stopifnot(grepl("assigned missing map positions", control_combat, fixed = TRUE))
  stopifnot(grepl("upsert_encounter_actor_position", control_combat, fixed = TRUE))
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
  stopifnot(grepl('class = paste("party-portrait"', party_hud_source, fixed = TRUE))
  stopifnot(grepl('portrait_base = "assets/player-posters"', party_hud_source, fixed = TRUE))
  stopifnot(grepl('paste0(sub("/$", "", portrait_base)', party_hud_source, fixed = TRUE))
  control_server <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","server.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl('portrait_base="player-assets/assets/player-posters"',control_server,fixed=TRUE))
  stopifnot(grepl('dewydd_troell = "dewydd-troell.png"', party_hud_source, fixed = TRUE))
  stopifnot(grepl('eleri = "eleri.png"', party_hud_source, fixed = TRUE))
  stopifnot(grepl('party-portrait-initial', party_hud_source, fixed = TRUE))
  stopifnot(grepl('open_character_deck', party_hud_source, fixed = TRUE))
  stopifnot(grepl('character_rest_status_cards', party_hud_source, fixed = TRUE))
  stopifnot(grepl('character_condition_status_cards', party_hud_source, fixed = TRUE))
  stopifnot(grepl('Skill Cards', party_hud_source, fixed = TRUE))
  stopifnot(grepl('Back to Deck', party_hud_source, fixed = TRUE))
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

test("Bloodthirsty Bite records ordinary blood without crashing combat", {
  combat_source <- paste(readLines(file.path(
    project_dir, "DND APP Drachuri Edition Player_v2", "server", "debug_combat_module.R"
  ), warn = FALSE), collapse = "\n")
  stopifnot(grepl('"blood",\n          paste0("Bloodthirsty Bite', combat_source, fixed = TRUE))
  stopifnot(!grepl('record_blood_consumption(core$state$char_id,core$state$active_session_id,char$meta$day,"bite"', combat_source, fixed = TRUE))
  stopifnot(!grepl('toast=TRUE,flash="gold"', combat_source, fixed = TRUE))
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

test("ready loadout keeps three ordinary weapons and one ranged weapon", {
  load_functions(global_file, c("inventory_normalize"))
  ready <- test_env$inventory_empty()
  for (i in 1:5) ready <- rbind(ready, data.frame(
    id=paste0("ready_",i), name=if(i==5)"Longbow"else paste("Weapon",i), type="weapon", desc="", value=0,
    weight=1, qty=1, equipped=TRUE, in_bag=FALSE, meta=I(list(list())), edit=FALSE,
    stringsAsFactors=FALSE
  ))
  ready <- test_env$inventory_normalize(ready)
  stopifnot(nrow(ready) == 5L, sum(ready$equipped) == 4L,
            sum(ready$equipped & ready$name == "Longbow") == 1L)
})

test("glyph families require their matching inventory Knot", {
  char <- list(inventory=list(items=test_env$inventory_empty()))
  stopifnot(length(test_env$glyph_unlocked_types(char)) == 0L)
  knot <- data.frame(id="knot",name="Knot of Warding",type="item",desc="",value=0,weight=.1,qty=1,
                     equipped=FALSE,in_bag=FALSE,meta=I(list(list())),edit=FALSE,stringsAsFactors=FALSE)
  char$inventory$items <- knot
  stopifnot(identical(test_env$glyph_unlocked_types(char),"ward"),
            isTRUE(test_env$glyph_type_unlocked(char,"ward")),
            !isTRUE(test_env$glyph_type_unlocked(char,"rune")))
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

test("control combat exposes the shared standard action set", {
  control_file <- file.path(project_dir, "DND APP Drachuri Edition 2 Control", "control_app", "modules", "control_live_combat_module.R")
  source_text <- paste(readLines(control_file, warn = FALSE), collapse = "\n")
  required <- c("control_dash_action", "control_disengage_action", "control_hide_action",
                "control_dodge_action", "control_help_action", "control_grapple_action",
                "escape_grapple", "control_ready_action")
  stopifnot(all(vapply(required, grepl, logical(1), x = source_text, fixed = TRUE)))
  stopifnot(grepl("control_disengage()", source_text, fixed = TRUE))
  stopifnot(all(vapply(c("toggle_control_movement", "toggle_control_actions", "toggle_control_glyphs", "toggle_control_turn"), grepl, logical(1), x = source_text, fixed = TRUE)))
  stopifnot(grepl('shinyjs::show("control_actions_panel")', source_text, fixed = TRUE))
  fullscreen_js <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition 2 Control", "www", "js", "combat2d_simple.js"), warn = FALSE), collapse = "\n")
  fullscreen_css <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition 2 Control", "www", "css", "combat.css"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("control-combat-document-fullscreen", fullscreen_js, fixed = TRUE))
  stopifnot(grepl('shell.closest(".combat-map-card")', fullscreen_js, fixed = TRUE))
  stopifnot(grepl("#control_partyhud-partyhud_root", fullscreen_css, fixed = TRUE))
  stopifnot(grepl(".control-shell > .tabbable > .nav-tabs{display:none", fullscreen_css, fixed = TRUE))
  stopifnot(grepl("panHandlerAttached", fullscreen_js, fixed = TRUE))
  stopifnot(grepl("moveOverlayElement", fullscreen_js, fixed = TRUE))
  stopifnot(grepl("overflow:visible !important", fullscreen_css, fixed = TRUE))
  stopifnot(grepl("NPC opportunity attack", source_text, fixed = TRUE))
  stopifnot(grepl("opportunity_attack_context", source_text, fixed = TRUE))
})

test("party notes include the Game Master and persisted acknowledgements", {
  player_notes <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","private_notes_module.R"),warn=FALSE),collapse="\n")
  shared_notes <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","session_db_core.R"),warn=FALSE),collapse="\n")
  control_notes <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_notes_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl('"Game Master"="__dm__"',player_notes,fixed=TRUE))
  stopifnot(grepl("claim_private_note_acknowledgements",player_notes,fixed=TRUE))
  stopifnot(grepl("get_session_private_notes",shared_notes,fixed=TRUE))
  stopifnot(grepl("sender_notified_at",shared_notes,fixed=TRUE))
  stopifnot(grepl("The sender will be told",control_notes,fixed=TRUE))
})

test("party quests are shared between Control and the player journal", {
  shared <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","session_db_core.R"),warn=FALSE),collapse="\n")
  player <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","quest_module.R"),warn=FALSE),collapse="\n")
  control <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_quests_module.R"),warn=FALSE),collapse="\n")
  migration <- paste(readLines(file.path(project_dir,"database","migrations","054_party_quests.sql"),warn=FALSE),collapse="\n")
  stopifnot(all(vapply(c("list_party_quests","get_party_quest","save_party_quest","add_party_quest_objective","set_party_quest_objective"),grepl,logical(1),x=shared,fixed=TRUE)))
  stopifnot(grepl("Party Quest Journal",player,fixed=TRUE))
  stopifnot(grepl("bottom:306px",player,fixed=TRUE))
  stopifnot(grepl("Hidden from players",control,fixed=TRUE))
  stopifnot(grepl("Edit Selected",control,fixed=TRUE))
  stopifnot(grepl("input$edit_quest",control,fixed=TRUE))
  stopifnot(grepl("CREATE TABLE IF NOT EXISTS party_quests",migration,fixed=TRUE))
  stopifnot(grepl("CREATE TABLE IF NOT EXISTS party_quest_objectives",migration,fixed=TRUE))
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

test("player combat prefers local equipment while Armoury autosave is pending", {
  combat <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","debug_combat_module.R"),warn=FALSE),collapse="\n")
  loader <- sub(".*load_actor_for_combat <- function", "load_actor_for_combat <- function", combat)
  local_pos <- regexpr("return(validate_character(core$state$char))", loader, fixed=TRUE)[[1L]]
  db_pos <- regexpr("load_character_from_db(actor_id)", loader, fixed=TRUE)[[1L]]
  stopifnot(local_pos > 0L, db_pos > 0L, local_pos < db_pos)
})

test("combat Hide always returns integer passive perception values", {
  combat <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","debug_combat_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("as.integer(10L + floor((wis - 10L) / 2L))", combat, fixed=TRUE))
  stopifnot(grepl("enemy_passive_perception,integer(1)", combat, fixed=TRUE))
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

test("Continue Adventure reports load and validation failures", {
  landing_source <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","landing_module.R"),warn=FALSE),collapse="\n")
  session_source <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","session_db_core.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl('showNotification("Continue Adventure could not reach any saved characters.',landing_source,fixed=TRUE))
  stopifnot(grepl('restored<-tryCatch(validate_character(x)',landing_source,fixed=TRUE))
  stopifnot(grepl('selectInput(ns("db_session"), "Select Adventure"',landing_source,fixed=TRUE))
  stopifnot(grepl("list_active_sessions_for_character(char_id)",landing_source,fixed=TRUE))
  stopifnot(grepl('gs.name AS session_name',session_source,fixed=TRUE))
  stopifnot(!grepl("ORDER BY gs.updated_at DESC NULLS LAST, gs.id DESC\n      LIMIT 1",session_source,fixed=TRUE))
})

test("Control safely manages player resources, conditions and card decks", {
  players <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_players_module.R"),warn=FALSE),collapse="\n")
  control_server <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","server.R"),warn=FALSE),collapse="\n")
  control_global <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","global.R"),warn=FALSE),collapse="\n")
  player_server <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server.R"),warn=FALSE),collapse="\n")
  hud <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","party_hud_module.R"),warn=FALSE),collapse="\n")
  stopifnot(all(vapply(c("Refill HP","Refill Sindre","Refill Both","Add Status","Remove Status"),grepl,logical(1),x=players,fixed=TRUE)))
  stopifnot(grepl("set_session_hp(sid,cid",players,fixed=TRUE))
  stopifnot(grepl("status_condition_definitions()",players,fixed=TRUE))
  stopifnot(grepl('card_asset_base="player-assets/assets"',control_server,fixed=TRUE))
  stopifnot(grepl('file.path(player_app_dir, "plug", "skills_data.R")',control_global,fixed=TRUE))
  stopifnot(grepl("deck<-tryCatch(build_character_deck(cid)",hud,fixed=TRUE))
  stopifnot(grepl("invalidateLater(2500, session)",player_server,fixed=TRUE))
  stopifnot(grepl('core$add_log("Character updated by the DM."',player_server,fixed=TRUE))
  stopifnot(grepl('name = as.character(ch$meta$name',hud,fixed=TRUE))
})

test("character level display trusts the canonical stored total", {
  level_source <- paste(readLines(level_file,warn=FALSE),collapse="\n")
  stopifnot(grepl("stored_total <- suppressWarnings(as.integer(char$build$level",level_source,fixed=TRUE))
  stopifnot(grepl("stored_total else class_total",level_source,fixed=TRUE))
})

test("uploaded storyboards install against the currently revealed story id", {
  core <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","story_core.R"),warn=FALSE),collapse="\n")
  module <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","story_module.R"),warn=FALSE),collapse="\n")
  mac_launcher <- paste(readLines(file.path(project_dir,"installer","mac","Drachuri Player"),warn=FALSE),collapse="\n")
  windows_launcher <- paste(readLines(file.path(project_dir,"installer","player","Drachuri Player.cmd"),warn=FALSE),collapse="\n")
  stopifnot(grepl("storyboard_id_override=NULL",core,fixed=TRUE))
  stopifnot(grepl("storyboard_id_override=expected",module,fixed=TRUE))
  stopifnot(grepl('export DRACHURI_DATA_DIR="$support_dir"',mac_launcher,fixed=TRUE))
  stopifnot(grepl('set "DRACHURI_DATA_DIR=%LOCALAPPDATA%\\Drachuri Player"',windows_launcher,fixed=TRUE))
  stopifnot(grepl("Storyboard asset could not be installed",core,fixed=TRUE))
  stopifnot(grepl("Storyboard installed without required picture",core,fixed=TRUE))
  stopifnot(grepl("local_revision<-reactiveVal(0L)",module,fixed=TRUE))
  stopifnot(grepl("revision();local_revision();slide_ui()",module,fixed=TRUE))
  stopifnot(grepl("local_revision(isolate(local_revision())+1L)",module,fixed=TRUE))
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
  bands<-beast$effects[[1L]]$scaling
  stopifnot(identical(unname(unlist(bands[["2"]])),c(1,.5,.25)))
  stopifnot(identical(unname(unlist(bands[["11"]])),c(2,1,.5,.25)))
  stopifnot(identical(unname(unlist(bands[["15"]])),c(4,2,1,.5)))
  migration<-readLines(file.path(project_dir,"database","migrations","045_call_beast_catalogue.sql"),warn=FALSE)
  stopifnot(any(grepl("summonable BOOLEAN",migration,fixed=TRUE)),sum(grepl("'summon_",migration,fixed=TRUE))>=10L)
  stopifnot(!any(grepl("Elephant|Crocodile|Polar Bear",migration)))

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
  blood<-data.frame(type="blood",meta=I(list(list(category="blood"))))
  heart<-data.frame(type="heart",meta=I(list(list(category="heart"))))
  stopifnot(identical(test_env$inventory_item_category(blood),"blood"),identical(test_env$inventory_item_category(heart),"heart"))
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

test("Annwn NPC attacks retain lore, range and effect contracts", {
  attacks<-test_env$enemy_attack_catalog()
  forbidden<-c("fae_bolt","fire_breath","necrotic_touch","stone_fist","claw","bite")
  stopifnot(!any(forbidden%in%names(attacks)))
  stopifnot(attacks$mandred_bolt$ability=="bld_str",attacks$mandred_bolt$range_ft==60L)
  stopifnot(attacks$blood_feed$requires=="target_grappled_restrained_or_incapacitated",attacks$blood_feed$heal_fraction==1)
  stopifnot(attacks$great_beast_grab$on_hit_condition=="grappled")
  stopifnot(attacks$integrated_projectile$lore_status=="provisional")
})

test("Annwn NPC features replace generic traits with layered lore-safe definitions", {
  features<-test_env$npc_feature_catalogue()
  stopifnot(!any(c("Barbarian","Speedy","Boss","Fae","Animal","Armoured","Brute","Archer","Spellcaster","Regenerator")%in%names(features)))
  stopifnot(features$mandred_sorcery$category=="magical_discipline")
  stopifnot(features$necromancy$requires_features=="mandred_sorcery")
  stopifnot(features$cythraul_restoration$compatible_pools=="cythraul")
  stopifnot(features$boss_encounter$category=="encounter_modifier",!features$boss_encounter$enabled_for_generation)
  attacks<-test_env$enemy_attack_catalog()
  stopifnot(!length(unique(unlist(lapply(features,function(x)setdiff(x$attacks,names(attacks)))))))
  trap<-attacks$set_hunting_trap
  stopifnot("set_hunting_trap"%in%features$trap_setter$attacks,trap$on_hit_condition=="restrained",trap$usage=="once_per_encounter")
  concrete<-function(x)length(x$ability_bonus)||x$hp_multiplier!=1||x$ac_bonus!=0||x$movement_bonus!=0||x$initiative_bonus!=0||x$gold_multiplier!=1||length(x$attacks)||length(x$loot_ids)||length(x$resistances)||length(x$immunities)||length(x$vulnerabilities)||length(x$condition_immunities)
  stopifnot(all(vapply(features,concrete,logical(1))))
  summary<-test_env$npc_feature_effect_summary(features$trap_setter)
  stopifnot(any(grepl("Spring Hunting Trap",summary,fixed=TRUE)),any(grepl("DEX +1",summary,fixed=TRUE)))
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

test("map autogenerator creates deterministic editable terrain presets", {
  presets<-unname(test_env$control_map_presets());clutter<-c("table","bar","chair","bench","crate","barrel","bed","shelf","rubble","campfire","torch","brazier");allowed<-c("grass","sand","stone","forest","swamp","water","wall","ravine","road","mandred_convergence",clutter)
  maps<-setNames(lapply(presets,function(p)test_env$generate_control_map_tiles(42L,16L,12L,p,seed=77L,density=40)),presets)
  stopifnot(all(vapply(maps,nrow,integer(1))==192L),all(vapply(maps,function(x)all(x$terrain%in%allowed),logical(1))))
  stopifnot(identical(maps$forest,test_env$generate_control_map_tiles(42L,16L,12L,"forest",77L,40)))
  stopifnot(any(maps$tavern$terrain=="wall"),any(maps$tavern$terrain=="bar"),any(maps$tavern$terrain=="table"),any(maps$prison$terrain=="wall"),any(maps$prison$terrain=="bed"),any(maps$forest$terrain=="road"))
  stopifnot(all(maps$tavern$blocks_movement[maps$tavern$terrain=="wall"]),all(maps$forest$move_cost[maps$forest$terrain=="forest"]==2))
  stopifnot(any(maps$ravine$terrain=="ravine"),all(maps$ravine$blocks_movement[maps$ravine$terrain=="ravine"]))
  stopifnot(any(maps$river$terrain=="water"),any(maps$river$terrain=="road"),any(maps$coast$terrain=="sand"),any(maps$coast$terrain=="water"))
  generated_water<-maps$river[maps$river$terrain=="water",,drop=FALSE]
  stopifnot(nrow(generated_water)>0,all(generated_water$move_cost==2),!any(generated_water$blocks_movement),!any(generated_water$blocks_vision))
})

test("improvised encounter drafts are deterministic, party-aware and editable", {
  players<-data.frame(character_id=c("p1","p2","p3"),max_hp=c(24L,30L,18L),is_active=TRUE)
  templates<-data.frame(npc_id=c("scout","guard","brute"),name=c("Scout","Guard","Brute"),hp_max=c(12L,22L,38L),ac=c(11L,14L,15L),attack_bonus=c(3L,4L,5L),damage_expr=c("1d6+1","1d8+2","2d8+3"),stringsAsFactors=FALSE)
  standard<-test_env$compose_encounter_draft(players,templates,"standard",seed=44L)
  repeat_draft<-test_env$compose_encounter_draft(players,templates,"standard",seed=44L)
  dangerous<-test_env$compose_encounter_draft(players,templates,"dangerous",seed=44L)
  overwhelming<-test_env$compose_encounter_draft(players,templates,"overwhelming",seed=44L)
  stopifnot(identical(standard$enemies$npc_id,repeat_draft$enemies$npc_id))
  stopifnot(standard$party_size==3L,nrow(standard$enemies)>=2L,dangerous$party_budget>standard$party_budget)
  stopifnot(nrow(dangerous$enemies)>=nrow(standard$enemies),nrow(overwhelming$enemies)>=nrow(dangerous$enemies),nrow(overwhelming$enemies)>=3L)
  dims<-test_env$encounter_draft_dimensions(standard$party_size,nrow(standard$enemies));stopifnot(dims[["width"]]>=12L,dims[["height"]]>=10L)
  tiles<-test_env$generate_control_map_tiles(42L,dims[["width"]],dims[["height"]],"forest",44L,35)
  enemy_ids<-paste0("e",seq_len(nrow(standard$enemies)));positions<-test_env$encounter_draft_positions(tiles,players$character_id,enemy_ids,44L)
  stopifnot(nrow(positions)==nrow(players)+length(enemy_ids),length(unique(paste(positions$x,positions$y)))==nrow(positions))
  stopifnot(max(positions$y[positions$actor_type=="player"])<min(positions$y[positions$actor_type=="enemy"]))
  setup_source<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_encounter_setup_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("Generate Improvised Encounter",setup_source,fixed=TRUE),grepl("create_generated_encounter",setup_source,fixed=TRUE))
  stopifnot(grepl('create_encounter(sid,encounter_name,map_id,"setup")',setup_source,fixed=TRUE),grepl("set_shared_map_tiles(ctrl,map_id,tiles)",setup_source,fixed=TRUE))
  stopifnot(grepl("merge_npc_pool_catalogue",setup_source,fixed=TRUE),grepl("Review it before starting combat",setup_source,fixed=TRUE))
  stopifnot(grepl("generate_encounter_pool_candidates(pool,18L,seed)",setup_source,fixed=TRUE),!grepl("generator_templates <-",setup_source,fixed=TRUE))
  generator_source<-paste(readLines(encounter_generator_file,warn=FALSE),collapse="\n")
  stopifnot(grepl("authored creatures must never leak",generator_source,fixed=TRUE),grepl('identical(as.character(pool$id), "custom")',generator_source,fixed=TRUE))
})

test("lean 3D renderer keeps costly features optional", {
  js_path <- file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js")
  js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")
  stopifnot(grepl("InstancedMesh", js, fixed = TRUE))
  stopifnot(grepl("buildSurfaceGeometry", js, fixed = TRUE))
  stopifnot(grepl("buildTabletop", js, fixed = TRUE))
  stopifnot(grepl("tableWidth", js, fixed = TRUE), grepl("wallH=24", js, fixed = TRUE), grepl("roofRise=12", js, fixed = TRUE))
  stopifnot(grepl("controls.maxPolarAngle=Math.PI*.47", js, fixed = TRUE))
  stopifnot(grepl('ravine:{color:0x17151a,tex:"ravine.jpg",h:-2.65}', js, fixed = TRUE))
  stopifnot(grepl('const height=name==="wall"?1.65', js, fixed = TRUE))
  stopifnot(grepl("ruinedWallGeometry", js, fixed = TRUE))
  stopifnot(grepl("state.decorRoot.add(trunks,lower,upper)", js, fixed = TRUE))
  stopifnot(grepl("combat3d-lean-init", js, fixed = TRUE))
  stopifnot(grepl("combat3d_lean_ready", js, fixed = TRUE))
  stopifnot(grepl("AmbientLight", js, fixed = TRUE))
  stopifnot(grepl("boundsSignature", js, fixed = TRUE))
  stopifnot(grepl("updateOverlays", js, fixed = TRUE))
  stopifnot(grepl("is_pending_move", js, fixed = TRUE))
  stopifnot(grepl("CircleGeometry", js, fixed = TRUE), grepl("RingGeometry", js, fixed = TRUE))
  stopifnot(grepl('quality==="decorative"', js, fixed = TRUE))
  stopifnot(!grepl("GLTFLoader", js, fixed = TRUE))
  stopifnot(grepl("makeMiniature", js, fixed = TRUE), grepl("actorColour", js, fixed = TRUE))
  stopifnot(grepl("row?.marker_3d_color", js, fixed = TRUE))
  stopifnot(!grepl("if(self)return new THREE.Color", js, fixed = TRUE))
  stopifnot(grepl("PointLight", js, fixed = TRUE), grepl("chandelierLight", js, fixed = TRUE))
  stopifnot(grepl("SpotLight", js, fixed = TRUE), grepl("buildTerrainTransitions", js, fixed = TRUE))
  stopifnot(!grepl("function animate", js, fixed = TRUE))
})

test("every player receives a stable, visible temporary 3D marker colour", {
  ids <- c("player-c", "player-a", "player-b", "player-a")
  assignments <- test_env$combat_player_marker_assignments(ids)
  stopifnot(identical(assignments$actor_id, c("player-a", "player-b", "player-c")))
  stopifnot(length(unique(assignments$colour)) == 3L)
  identity <- test_env$combat_player_marker_identity(ids, "player-b")
  stopifnot(identical(identity$name, "Crimson"), identical(identity$colour, "#C6473A"))
  map <- data.frame(
    occupant_id = c("player-c", "enemy-1", "player-a", "player-b", "player-a"),
    occupant_type = c("player", "enemy", "player", "player", "player"),
    stringsAsFactors = FALSE
  )
  coloured <- test_env$apply_unique_player_3d_colours(map)
  player_colours <- coloured$marker_3d_color[coloured$occupant_type == "player"]
  stopifnot(length(unique(player_colours)) == 3L)
  stopifnot(identical(player_colours[[2L]], "#2F80C3"))
  player_combat <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","debug_combat_module.R"),warn=FALSE),collapse="\n")
  control_combat <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_live_combat_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("Your 3D marker:", player_combat, fixed = TRUE))
  stopifnot(all(vapply(c(player_combat, control_combat), function(src) grepl("apply_unique_player_3d_colours(render_df)", src, fixed = TRUE), logical(1))))
})

test("lean 3D renderer pins player posters to the tavern wall", {
  js <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("posterCanvas", js, fixed = TRUE))
  stopifnot(grepl("updateWallPosters", js, fixed = TRUE))
  stopifnot(grepl("occupant_current_hp", js, fixed = TRUE))
  stopifnot(grepl("occupant_sindre_cur", js, fixed = TRUE))
  stopifnot(grepl("canvas.width=640", js, fixed = TRUE))
  stopifnot(grepl("state.roomDepth/2", js, fixed = TRUE))
  stopifnot(grepl("addCards(players,6.25", js, fixed = TRUE))
  stopifnot(!grepl("addCards(enemies,2.5", js, fixed = TRUE))
  stopifnot(grepl('eman:"eman.png"', js, fixed = TRUE))
  stopifnot(grepl('dewydd_troell:"dewydd-troell.png"', js, fixed = TRUE))
  stopifnot(grepl('eleri:"eleri.png"', js, fixed = TRUE))
  stopifnot(grepl('nefretari:"nefretari.png"', js, fixed = TRUE))
  stopifnot(grepl("playerPosterFile", js, fixed = TRUE))
  stopifnot(file.exists(file.path("DND APP Drachuri Edition Player_v2","www","assets","player-posters","eman.png")))
  stopifnot(file.exists(file.path("DND APP Drachuri Edition Player_v2","www","assets","player-posters","dewydd-troell.png")))
  stopifnot(file.exists(file.path("DND APP Drachuri Edition Player_v2","www","assets","player-posters","eleri.png")))
  stopifnot(file.exists(file.path("DND APP Drachuri Edition Player_v2","www","assets","player-posters","nefretari.png")))
  player_combat <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "server", "debug_combat_module.R"), warn = FALSE), collapse = "\n")
  control_combat <- paste(readLines(file.path("DND APP Drachuri Edition 2 Control", "control_app", "modules", "control_live_combat_module.R"), warn = FALSE), collapse = "\n")
  stopifnot(all(vapply(c(player_combat, control_combat), function(src) grepl("occupant_conditions", src, fixed = TRUE), logical(1))))
  stopifnot(all(vapply(c(player_combat, control_combat), function(src) grepl("poster_resource_cache", src, fixed = TRUE), logical(1))))
})

test("lean 3D tavern has a door, pitched roof, rafters, chandelier and camera bounds", {
  js <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("tavern_wall_door_complete.png", js, fixed = TRUE))
  stopifnot(grepl("roomSpan=Math.max(48", js, fixed = TRUE))
  stopifnot(grepl("slopeLength=Math.hypot", js, fixed = TRUE))
  stopifnot(grepl("gableGeometry", js, fixed = TRUE))
  stopifnot(grepl("rafterCount", js, fixed = TRUE))
  stopifnot(grepl("const chandelier=new THREE.Group", js, fixed = TRUE))
  stopifnot(grepl("constrainCamera", js, fixed = TRUE))
  stopifnot(grepl("controls.maxDistance", js, fixed = TRUE))
})

test("control loads the Three import map before its 3D module", {
  control_ui <- paste(readLines(file.path("DND APP Drachuri Edition 2 Control", "ui.R"), warn = FALSE), collapse = "\n")
  import_at <- regexpr('type = "importmap"', control_ui, fixed = TRUE)[[1L]]
  module_at <- regexpr('type = "module"', control_ui, fixed = TRUE)[[1L]]
  stopifnot(import_at > 0L, module_at > import_at)
})

test("3D camera and canvas survive ordinary turn and movement refreshes", {
  player_combat <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "server", "debug_combat_module.R"), warn = FALSE), collapse = "\n")
  renderer <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js"), warn = FALSE), collapse = "\n")
  stopifnot(grepl('combat_layout_phase <- reactiveVal("")', player_combat, fixed = TRUE))
  stopifnot(grepl("phase <- combat_layout_phase()", player_combat, fixed = TRUE))
  stopifnot(!grepl("output$combat_layout_ui <- renderUI({\n      combat <- combat_tbl()", player_combat, fixed = TRUE))
  stopifnot(grepl("restoredView=previous?.camera", renderer, fixed = TRUE))
  stopifnot(grepl("state.camera.position.copy(state.restoredView.position)", renderer, fixed = TRUE))
})

test("Dracnos wanted sketch is pinned away from the live HUD wall", {
  js <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js"), warn = FALSE), collapse = "\n")
  poster <- file.path("DND APP Drachuri Edition Player_v2", "www", "assets", "textures", "wanted_dracnos_text.png")
  stopifnot(file.exists(poster), file.info(poster)$size > 100000)
  stopifnot(grepl("wanted_dracnos_text.png", js, fixed = TRUE))
  stopifnot(grepl("dracnos.rotation.z=.035", js, fixed = TRUE))
  stopifnot(grepl("dracnosX=-roomWidth/2", js, fixed = TRUE))
  stopifnot(grepl("dracnos.rotation.y=Math.PI/2", js, fixed = TRUE))
})

test("Lord Blacklyn dangerous Fae ally notice joins the wanted wall", {
  js <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js"), warn = FALSE), collapse = "\n")
  poster <- file.path("DND APP Drachuri Edition Player_v2", "www", "assets", "textures", "wanted_lord_blacklyn_text.png")
  stopifnot(file.exists(poster), file.info(poster)$size > 100000)
  stopifnot(grepl("wanted_lord_blacklyn_text.png", js, fixed = TRUE))
  stopifnot(grepl("blacklyn.rotation.z=-.025", js, fixed = TRUE))
  stopifnot(grepl("blacklynX=-roomWidth/2", js, fixed = TRUE))
})

test("disbanded Iron Vow propaganda is pinned on the side wall", {
  js <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js"), warn = FALSE), collapse = "\n")
  poster <- file.path("DND APP Drachuri Edition Player_v2", "www", "assets", "textures", "propaganda_iron_vow_disbanded.png")
  stopifnot(file.exists(poster), file.info(poster)$size > 100000)
  stopifnot(grepl("propaganda_iron_vow_disbanded.png", js, fixed = TRUE))
  stopifnot(grepl("vowX=-roomWidth/2", js, fixed = TRUE))
  stopifnot(grepl("vow.rotation.y=Math.PI/2", js, fixed = TRUE))
})

test("Order of Succession anti-Fae propaganda is pinned on the side wall", {
  js <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js"), warn = FALSE), collapse = "\n")
  poster <- file.path("DND APP Drachuri Edition Player_v2", "www", "assets", "textures", "propaganda_order_succession_fae.png")
  stopifnot(file.exists(poster), file.info(poster)$size > 100000)
  stopifnot(grepl("propaganda_order_succession_fae.png", js, fixed = TRUE))
  stopifnot(grepl("successionX=-roomWidth/2", js, fixed = TRUE))
  stopifnot(grepl("succession.rotation.y=Math.PI/2", js, fixed = TRUE))
})

test("Heart Eater sightings warning joins the cluttered poster wall", {
  js <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js"), warn = FALSE), collapse = "\n")
  poster <- file.path("DND APP Drachuri Edition Player_v2", "www", "assets", "textures", "warning_heart_eater.png")
  stopifnot(file.exists(poster), file.info(poster)$size > 100000)
  stopifnot(grepl("warning_heart_eater.png", js, fixed = TRUE))
  stopifnot(grepl("heartX=-roomWidth/2", js, fixed = TRUE))
  stopifnot(grepl("heart.rotation.y=Math.PI/2", js, fixed = TRUE))
})

test("generated notices mask dark outer borders without erasing their ink", {
  js <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("function posterMaterial", js, fixed = TRUE))
  stopifnot(grepl("posterEdge<0.045", js, fixed = TRUE))
  stopifnot(grepl("posterDark<0.16) discard", js, fixed = TRUE))
})

test("Annwn world map fills the wall opposite the poster display", {
  js <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js"), warn = FALSE), collapse = "\n")
  map <- file.path("DND APP Drachuri Edition Player_v2", "www", "assets", "textures", "map_annwn_world.jpg")
  stopifnot(file.exists(map), file.info(map)$size > 100000)
  stopifnot(grepl("map_annwn_world.jpg", js, fixed = TRUE))
  stopifnot(grepl("worldMapH=12.2", js, fixed = TRUE))
  stopifnot(grepl("tatteredPlaneGeometry(worldMapW,worldMapH)", js, fixed = TRUE))
  stopifnot(grepl("worldMapX=roomWidth/2", js, fixed = TRUE))
  stopifnot(grepl("worldMap.rotation.y=-Math.PI/2", js, fixed = TRUE))
})

test("control encounter workflow uses named selectors", {
  live <- paste(readLines(file.path("DND APP Drachuri Edition 2 Control", "control_app", "modules", "control_live_combat_module.R"), warn = FALSE), collapse = "\n")
  setup <- paste(readLines(file.path("DND APP Drachuri Edition 2 Control", "control_app", "modules", "control_encounter_setup_module.R"), warn = FALSE), collapse = "\n")
  stopifnot(grepl('selectInput(ns("encounter_select"), "Encounter"', live, fixed = TRUE))
  stopifnot(grepl('as.integer(input$encounter_select', live, fixed = TRUE))
  stopifnot(grepl('selectInput(ns("map_id"), "Map"', setup, fixed = TRUE))
  stopifnot(!grepl('numericInput(ns("map_id"), "Map ID"', setup, fixed = TRUE))
})

test("map builder retries 3D preview after renderer readiness", {
  builder <- paste(readLines(file.path("DND APP Drachuri Edition 2 Control", "control_app", "modules", "control_map_builder_module.R"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("send_3d_preview <- function", builder, fixed = TRUE))
  stopifnot(grepl("combat3d_lean_ready", builder, fixed = TRUE))
  stopifnot(grepl("preview_3d_quality", builder, fixed = TRUE))
})

test("local launch configures a feature-rich visual map", {
  visual_map <- paste(readLines(file.path("test-env", "configure_visual_test_map.R"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("3D Visual Test — Forest Crossing", visual_map, fixed = TRUE))
  stopifnot(all(vapply(c("forest", "road", "ravine", "water", "wall"), function(x) grepl(paste0('"', x, '"'), visual_map, fixed = TRUE), logical(1))))
})

test("generated environment textures are shipped and wired into 3D", {
  texture_dir <- file.path("DND APP Drachuri Edition Player_v2", "www", "assets", "textures")
  assets <- c(
    "grass.jpg", "dirt.jpg", "forest.jpg", "swamp.jpg", "stone.jpg",
    "water.jpg", "ravine.jpg", "pit.jpg", "bark.jpg", "leaves.jpg",
    "tavern_table_oak.jpg", "tavern_floorboards.jpg",
    "tavern_plaster_timbers.jpg", "tavern_wall_door_complete.png",
    "wanted_dracnos_text.png", "wanted_lord_blacklyn_text.png", "battlefield_fieldstone.jpg", "clutter_oak.png"
  )
  stopifnot(all(file.exists(file.path(texture_dir, assets))))
  stopifnot(all(file.info(file.path(texture_dir, assets))$size > 100000))
  js <- paste(readLines(file.path("DND APP Drachuri Edition Player_v2", "www", "js", "combat3d_lean.js"), warn=FALSE), collapse="\n")
  stopifnot(all(vapply(assets, function(asset) grepl(asset, js, fixed=TRUE), logical(1))))
  stopifnot(grepl("import.meta.url", js, fixed=TRUE))
})

test("Windows player installer is self-contained and does not require RStudio", {
  installer_dir <- file.path("installer", "player")
  required <- c(
    "DrachuriPlayer.iss", "build_player_installer.ps1", "restore_packages.R",
    "installed_run.R", "Drachuri Player.cmd", "Drachuri Player.vbs", "README.md"
  )
  stopifnot(all(file.exists(file.path(installer_dir, required))))
  stopifnot(file.exists("Build Drachuri Player Installer.cmd"))
  iss <- paste(readLines(file.path(installer_dir, "DrachuriPlayer.iss"), warn = FALSE), collapse = "\n")
  build <- paste(readLines(file.path(installer_dir, "build_player_installer.ps1"), warn = FALSE), collapse = "\n")
  restore <- paste(readLines(file.path(installer_dir, "restore_packages.R"), warn = FALSE), collapse = "\n")
  launch <- paste(readLines(file.path(installer_dir, "Drachuri Player.cmd"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("PrivilegesRequired=lowest", iss, fixed = TRUE))
  stopifnot(grepl("runtime\\R\\*", iss, fixed = TRUE))
  stopifnot(grepl("library\\*", iss, fixed = TRUE))
  stopifnot(grepl('RVersion = "4.6.1"', build, fixed = TRUE))
  stopifnot(grepl('"models"', build, fixed = TRUE))
  stopifnot(grepl('type = "binary"', restore, fixed = TRUE))
  stopifnot(!grepl("renv::restore", restore, fixed = TRUE))
  stopifnot(grepl("R_HOME=%~dp0runtime\\R", launch, fixed = TRUE))
  stopifnot(grepl("bin\\Rscript.exe", launch, fixed = TRUE))
  stopifnot(grepl("DND_LAUNCH_BROWSER=true", launch, fixed = TRUE))
  stopifnot(!grepl('"%R_HOME%\\bin\\x64\\Rscript.exe" --vanilla', launch, fixed = TRUE))
  stopifnot(grepl("Rscript.exe", launch, fixed = TRUE))
  stopifnot(!grepl("RStudio", launch, fixed = TRUE))
})

test("tracked source paths can be checked out on Windows", {
  tracked <- system2("git", "ls-files", stdout=TRUE)
  components <- unlist(strsplit(tracked,"/",fixed=TRUE),use.names=FALSE)
  invalid_chars <- grepl('[:*?"<>|]',components)
  reserved <- grepl('^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(\\..*)?$',components,ignore.case=TRUE)
  trailing <- grepl('[ .]$',components)
  stopifnot(!any(invalid_chars|reserved|trailing))
})

test("Mac player installer builds one DMG with app, R and locked packages", {
  installer_dir <- file.path("installer", "mac")
  required <- c("Drachuri Player", "Info.plist", "component.plist", "prepare_mac_library.R", "build_mac_installer.sh", "README.md")
  stopifnot(all(file.exists(file.path(installer_dir, required))))
  stopifnot(file.exists("Build Drachuri Player Mac Installer.command"))
  build <- paste(readLines(file.path(installer_dir, "build_mac_installer.sh"), warn = FALSE), collapse = "\n")
  launch <- paste(readLines(file.path(installer_dir, "Drachuri Player"), warn = FALSE), collapse = "\n")
  runner <- paste(readLines(file.path("installer", "player", "installed_run.R"), warn = FALSE), collapse = "\n")
  stopifnot(grepl('r_version="4.2.3"', build, fixed = TRUE))
  stopifnot(grepl('package_version="$numeric_version.9"', build, fixed = TRUE))
  stopifnot(grepl("RFramework.pkg", build, fixed = TRUE))
  stopifnot(grepl("--component-plist", build, fixed = TRUE))
  component <- paste(readLines(file.path(installer_dir, "component.plist"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("BundleIsRelocatable", component, fixed = TRUE), grepl("<false/>", component, fixed = TRUE))
  stopifnot(grepl("hdiutil create", build, fixed = TRUE))
  stopifnot(grepl("RENV_CONFIG_AUTOLOADER_ENABLED=FALSE", launch, fixed = TRUE))
  stopifnot(grepl("Install Rosetta", launch, fixed = TRUE))
  stopifnot(grepl('LC_ALL="en_GB.UTF-8"', launch, fixed = TRUE))
  stopifnot(grepl('launch_url="http://127.0.0.1:48627"', launch, fixed = TRUE))
  stopifnot(grepl("kill -0", launch, fixed = TRUE), grepl("nohup", launch, fixed = TRUE))
  stopifnot(grepl('chmod -R a+rX "$app"', build, fixed = TRUE))
  stopifnot(grepl("DRACHURI_LOG_DIR", runner, fixed = TRUE))
  stopifnot(grepl("Database probe", runner, fixed = TRUE))
  stopifnot(grepl("DND_LAUNCH_PORT", runner, fixed = TRUE))
})

test("loading and camp screens use the revised illustrated artwork", {
  ui_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "ui.R"), warn = FALSE), collapse = "\n")
  camp_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "camp_module.R"), warn = FALSE), collapse = "\n")
  www_dir <- file.path(project_dir, "DND APP Drachuri Edition Player_v2", "www")
  stopifnot(file.exists(file.path(www_dir, "loading_screen_parchment.png")))
  stopifnot(file.exists(file.path(www_dir, "camp_realistic_hud_safe.png")))
  stopifnot(file.exists(file.path(www_dir, "camp_realistic_dawn_hud_safe.png")))
  stopifnot(file.exists(file.path(www_dir, "camp_realistic_day_hud_safe.png")))
  stopifnot(file.exists(file.path(www_dir, "camp_realistic_dusk_hud_safe.png")))
  stopifnot(grepl("loading_screen_parchment.png", ui_source, fixed = TRUE))
  stopifnot(grepl("camp_realistic_hud_safe.png", ui_source, fixed = TRUE))
  stopifnot(grepl("camp_realistic_hud_safe.png", camp_source, fixed = TRUE))
  stopifnot(grepl('left: 52%; top: 34%; width: 13%; height: 13%;', camp_source, fixed = TRUE))
  stopifnot(grepl('left: 86%; top: 38%; width: 11%; height: 18%;', camp_source, fixed = TRUE))
  stopifnot(grepl('left: 69%; top: 75%; width: 21%; height: 21%;', camp_source, fixed = TRUE))
})

test("party time is shared across camp, rest, runes and DM geography controls", {
  shared <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","session_db_core.R"),warn=FALSE),collapse="\n")
  camp <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","camp_module.R"),warn=FALSE),collapse="\n")
  hud <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","HUD.R"),warn=FALSE),collapse="\n")
  rest <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","rest_module.R"),warn=FALSE),collapse="\n")
  runes <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","rune_crafting_module.R"),warn=FALSE),collapse="\n")
  control <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_geography_climate_module.R"),warn=FALSE),collapse="\n")
  migration <- paste(readLines(file.path(project_dir,"database","migrations","040_session_environment_clock.sql"),warn=FALSE),collapse="\n")
  stopifnot(grepl('c("dawn", "day", "dusk", "night")',shared,fixed=TRUE))
  stopifnot(grepl("FOR UPDATE",shared,fixed=TRUE),grepl("revision=revision+1",shared,fixed=TRUE))
  stopifnot(grepl("session_environment_state",migration,fixed=TRUE))
  stopifnot(grepl("camp_realistic_dawn_hud_safe.png",camp,fixed=TRUE),!grepl("shared_clock_badge",camp,fixed=TRUE))
  stopifnot(grepl("time_of_day_label(env$time_of_day",hud,fixed=TRUE))
  stopifnot(grepl("phase-clock",rest,fixed=TRUE),grepl("Organise Watches",rest,fixed=TRUE))
  stopifnot(grepl('allocate_session_phase_time(p$id[[1L]],cid,type,label,hours',rest,fixed=TRUE))
  stopifnot(grepl("allocate_session_phase_time(phase$id[[1L]],cid(),\"glyph_work\"",runes,fixed=TRUE))
  stopifnot(file.exists(file.path(project_dir,"database","migrations","041_session_clock_elapsed_hours.sql")))
  stopifnot(file.exists(file.path(project_dir,"database","migrations","042_dm_rest_phases.sql")))
  stopifnot(file.exists(file.path(project_dir,"database","migrations","043_repeatable_camp_gathering.sql")))
  stopifnot(grepl("Begin Standard Phase",control,fixed=TRUE),grepl("Begin Rest Phase",control,fixed=TRUE),grepl("12 hours (two phases)",control,fixed=TRUE),grepl("Resolve & Begin Next Phase",control,fixed=TRUE))
  stopifnot(!grepl('numericInput(ns("day_number")',control,fixed=TRUE),!grepl('selectInput(ns("time_of_day")',control,fixed=TRUE))
  stopifnot(grepl("day_number=NULL,time_of_day=NULL",control,fixed=TRUE))
  stopifnot(grepl("resolve_session_phase(x$id[[1L]],next_kind",control,fixed=TRUE),grepl("next_phase_kind=NULL",shared,fixed=TRUE))
  stopifnot(grepl('budget<-as.numeric(p$duration_hours[[1L]])',rest,fixed=TRUE))
  stopifnot(grepl('gather_label(resource)),1)',rest,fixed=TRUE),grepl('Help with gathering",1',rest,fixed=TRUE))
  stopifnot(!grepl("one camp-gathering action",rest,fixed=TRUE))
  stopifnot(!grepl("add_rations_btn",rest,fixed=TRUE),!grepl("remove_rations_btn",rest,fixed=TRUE),!grepl("add_water_btn",rest,fixed=TRUE),!grepl("remove_water_btn",rest,fixed=TRUE))
  stopifnot(grepl("restore_sindre(char,hours=hours)",shared,fixed=TRUE),grepl("resolve_session_phase <-",shared,fixed=TRUE))
})

test("rest status cards present the existing survival and phase rules", {
  shared_core<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","global_core.R"),warn=FALSE),collapse="\n")
  session_core<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","session_db_core.R"),warn=FALSE),collapse="\n")
  rest<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","rest_module.R"),warn=FALSE),collapse="\n")
  hud<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","HUD.R"),warn=FALSE),collapse="\n")
  blood<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","blood_module.R"),warn=FALSE),collapse="\n")
  card_dir<-file.path(project_dir,"DND APP Drachuri Edition Player_v2","www","assets","status-cards")
  required<-c("rest/warm.png","rest/cold.png","rest/hungry.png","rest/well_fed.png","rest/parched.png","rest/hydrated.png","rest/insufficient_blood.png","rest/sufficient_blood.png","blood-addiction/blood_addiction_2_craving.png","conditions/exhaustion_1_weary.png","conditions/exhaustion_6_collapsed.png")
  stopifnot(all(file.exists(file.path(card_dir,required))))
  stopifnot(grepl("character_rest_status_cards <-",shared_core,fixed=TRUE),grepl("Active Rest Cards",rest,fixed=TRUE))
  stopifnot(!grepl("Recovery is applied from your phase plan",rest,fixed=TRUE),grepl("status-card-fan",hud,fixed=TRUE),grepl("status_card_click",hud,fixed=TRUE),grepl("status-card-modal",hud,fixed=TRUE))
  stopifnot(grepl("blood_status_cards",blood,fixed=TRUE),grepl("blood-status-hand",blood,fixed=TRUE),grepl('"sufficient_blood", "insufficient_blood"',blood,fixed=TRUE))
  stopifnot(grepl("Hungry",session_core,fixed=TRUE),grepl("Parched",session_core,fixed=TRUE),grepl("Cold",session_core,fixed=TRUE),grepl("phase_resolution",session_core,fixed=TRUE))
  stopifnot(grepl("A fire can only be lit during an official Rest Phase.",rest,fixed=TRUE),grepl('toggleState("light_fire",condition=rest_open)',rest,fixed=TRUE))
  stopifnot(grepl('sleep="No Long Rest"',session_core,fixed=TRUE),grepl('if("long_rest"%in%types)next_hours[["sleep"]]<-0',session_core,fixed=TRUE))
  stopifnot(test_env$warmth_requirement_hours(list(climate="Temperate"))==24,test_env$warmth_requirement_hours(list(climate="Cold"))==12,test_env$warmth_requirement_hours(list(climate="Alpine"))==6,is.infinite(test_env$warmth_requirement_hours(list(climate="Tropical"))))
  stopifnot(test_env$required_intake(list(stage=2,previous_day_intake=0))==1)
})

test("installed player UI re-anchors late Shiny sessions to the app root", {
  ui_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "ui.R"), warn = FALSE), collapse = "\n")
  server_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server.R"), warn = FALSE), collapse = "\n")
  stopifnot(grepl('Sys.getenv("DRACHURI_APP_DIR", "")', ui_source, fixed = TRUE))
  stopifnot(grepl("setwd(normalizePath(drachuri_app_root", ui_source, fixed = TRUE))
  stopifnot(grepl('Sys.getenv("DRACHURI_APP_DIR", "")', server_source, fixed = TRUE))
  stopifnot(grepl("setwd(normalizePath(drachuri_app_root", server_source, fixed = TRUE))
})

test("hidden player modules pause their expensive database polling", {
  server_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server.R"), warn = FALSE), collapse = "\n")
  core_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "core_character.R"), warn = FALSE), collapse = "\n")
  camp_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "camp_module.R"), warn = FALSE), collapse = "\n")
  rest_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "rest_module.R"), warn = FALSE), collapse = "\n")
  inventory_source <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "inventory_module.R"), warn = FALSE), collapse = "\n")
  stopifnot(grepl('active_tab = "camp"', core_source, fixed = TRUE))
  stopifnot(grepl("input$main_tabs", server_source, fixed = TRUE))
  stopifnot(grepl('state$active_tab%||%"camp"),"camp"', camp_source, fixed = TRUE))
  stopifnot(grepl('rest_visible<-function()', rest_source, fixed = TRUE))
  stopifnot(grepl('state$active_tab%||%"camp"),"inventory"', inventory_source, fixed = TRUE))
})

test("local test launcher clears only stale Drachuri Shiny servers", {
  start_source <- paste(readLines(file.path(project_dir, "test-env", "run_local_apps.sh"), warn = FALSE), collapse = "\n")
  stop_source <- paste(readLines(file.path(project_dir, "test-env", "stop_local_apps.sh"), warn = FALSE), collapse = "\n")
  stopifnot(grepl('stop_local_apps.sh', start_source, fixed = TRUE))
  stopifnot(grepl('app*.pids', stop_source, fixed = TRUE))
  stopifnot(grepl('shiny::runApp(', stop_source, fixed = TRUE))
  stopifnot(grepl('lsof -nP -tiTCP:', stop_source, fixed = TRUE))
})

test("local PostgreSQL runtime stays outside the synced source tree", {
  start_db<-paste(readLines(file.path(project_dir,"test-env","start_db.sh"),warn=FALSE),collapse="\n")
  stop_db<-paste(readLines(file.path(project_dir,"test-env","stop_db.sh"),warn=FALSE),collapse="\n")
  stopifnot(grepl("Library/Application Support/Drachuri/test-postgres",start_db,fixed=TRUE))
  stopifnot(grepl("initdb",start_db,fixed=TRUE),grepl("DND_TEST_PGDATA",stop_db,fixed=TRUE))
  stopifnot(!grepl('data_dir="$project_dir/test-env/.postgres-data"',start_db,fixed=TRUE))
})

test("Mac player waits in a branded launcher before opening the browser", {
  launch <- paste(readLines(file.path("installer", "mac", "Drachuri Player"), warn = FALSE), collapse = "\n")
  runner <- paste(readLines(file.path("installer", "player", "installed_run.R"), warn = FALSE), collapse = "\n")
  stopifnot(grepl('buttons {"Quit", "Settings", "Launch"}', launch, fixed = TRUE))
  stopifnot(grepl('with icon POSIX file iconPath', launch, fixed = TRUE))
  stopifnot(grepl('DND_LAUNCH_BROWSER="false"', launch, fixed = TRUE))
  stopifnot(grepl('curl --silent --fail', launch, fixed = TRUE))
  stopifnot(grepl('running-build-id', launch, fixed = TRUE))
  stopifnot(grepl('if [ "$running_build_id" = "$app_build_id" ]', launch, fixed = TRUE))
  stopifnot(grepl('Sys.getenv("DND_LAUNCH_BROWSER", "false")', runner, fixed = TRUE))
})

test("installed launchers check one cross-platform GitHub release manifest", {
  updater <- paste(readLines(file.path("installer", "shared", "check_for_update.R"), warn = FALSE), collapse = "\n")
  mac_player <- paste(readLines(file.path("installer", "mac", "Drachuri Player"), warn = FALSE), collapse = "\n")
  mac_control <- paste(readLines(file.path("installer", "control", "mac", "Drachuri Control"), warn = FALSE), collapse = "\n")
  windows_player <- paste(readLines(file.path("installer", "player", "Drachuri Player.cmd"), warn = FALSE), collapse = "\n")
  windows_installer <- paste(readLines(file.path("installer", "player", "DrachuriPlayer.iss"), warn = FALSE), collapse = "\n")
  mac_player_build <- paste(readLines(file.path("installer", "mac", "build_mac_installer.sh"), warn = FALSE), collapse = "\n")
  release_script <- paste(readLines(file.path("scripts", "release_drachuri.sh"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("releases/latest/download/drachuri-update.json", updater, fixed = TRUE))
  stopifnot(grepl("base::numeric_version", updater, fixed = TRUE))
  stopifnot(!grepl("utils::numeric_version", updater, fixed = TRUE))
  stopifnot(grepl("manifest$products[[product]][[platform]]", updater, fixed = TRUE))
  stopifnot(grepl('"--", shQuote(message)', updater, fixed = TRUE))
  stopifnot(grepl("utils::download.file(asset$url", updater, fixed = TRUE))
  stopifnot(grepl('system2("/usr/bin/shasum"', updater, fixed = TRUE))
  stopifnot(grepl('system2("open", shQuote(destination)', updater, fixed = TRUE))
  stopifnot(grepl("player mac", mac_player, fixed = TRUE))
  stopifnot(grepl("control mac", mac_control, fixed = TRUE))
  stopifnot(grepl("player windows", windows_player, fixed = TRUE))
  stopifnot(grepl("check_for_update.R", windows_installer, fixed = TRUE))
  stopifnot(grepl('resources/update/check_for_update.R', mac_player_build, fixed = TRUE))
  stopifnot(grepl('"windows":$player_windows_json', release_script, fixed = TRUE))
  stopifnot(grepl('"control":{"mac":$control_mac_json,"windows":null}', release_script, fixed = TRUE))
  stopifnot(grepl('"$gh_bin" release create', release_script, fixed = TRUE))
  windows_publish <- paste(readLines(file.path(project_dir,"Build and Publish Drachuri Windows.cmd"),warn=FALSE),collapse="\n")
  windows_publish_ps <- paste(readLines(file.path(project_dir,"installer","player","publish_windows_installer.ps1"),warn=FALSE),collapse="\n")
  mac_publish <- paste(readLines(file.path(project_dir,"Publish Drachuri Update.command"),warn=FALSE),collapse="\n")
  stopifnot(grepl("git pull --ff-only",windows_publish,fixed=TRUE))
  stopifnot(grepl("gh release upload",windows_publish_ps,fixed=TRUE))
  stopifnot(grepl("products.player.windows",windows_publish_ps,fixed=TRUE))
  stopifnot(grepl('git -C "$root" push origin main',mac_publish,fixed=TRUE))
})

test("control dashboard ships its branded lightweight DM desk theme", {
  control_ui <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition 2 Control", "ui.R"), warn = FALSE), collapse = "\n")
  control_www <- file.path(project_dir, "DND APP Drachuri Edition 2 Control", "www")
  stopifnot(file.exists(file.path(control_www, "drachuri-control-logo.png")))
  stopifnot(file.exists(file.path(control_www, "control-desk-bg.jpg")))
  stopifnot(file.info(file.path(control_www, "control-desk-bg.jpg"))$size < 600000)
  stopifnot(grepl("DM desk theme", control_ui, fixed = TRUE))
  stopifnot(grepl('class = "control-brand-mark"', control_ui, fixed = TRUE))
})

test("players can submit bounded diagnostic reports for persistent DM review", {
  migration <- paste(readLines(file.path("database", "migrations", "038_player_issue_reports.sql"), warn = FALSE), collapse = "\n")
  sidebar <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "sidebar_module.R"), warn = FALSE), collapse = "\n")
  shared <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "shared", "session_db_core.R"), warn = FALSE), collapse = "\n")
  control <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition 2 Control", "control_app", "modules", "control_issue_reports_module.R"), warn = FALSE), collapse = "\n")
  stopifnot(grepl("CREATE TABLE IF NOT EXISTS player_issue_reports", migration, fixed = TRUE))
  stopifnot(grepl('c("error", "feature_upgrade")', shared, fixed = TRUE))
  stopifnot(grepl("session_notifications", shared, fixed = TRUE))
  stopifnot(grepl("collect_issue_logs <- function(max_bytes = 200000L)", sidebar, fixed = TRUE))
  stopifnot(grepl("Send Report to DM", sidebar, fixed = TRUE))
  stopifnot(grepl("Attached diagnostic log", control, fixed = TRUE))
  stopifnot(grepl("Mark Resolved", control, fixed = TRUE))
})

test("live encounter conditions become detailed player status cards", {
  char <- test_env$validate_character(list(meta=list(name="Tester")))
  snapshot <- list(effects=data.frame(effect_type="condition",target_actor_id="player-1",payload='{"condition":"blinded"}',stringsAsFactors=FALSE))
  conditions <- test_env$encounter_condition_values(snapshot,"player-1")
  cards <- test_env$character_condition_status_cards(char,conditions)
  stopifnot(identical(conditions,"blinded"))
  stopifnot(length(cards)==1L,grepl("automatically fail",cards[[1L]]$reason,fixed=TRUE))
  stopifnot(file.exists(file.path(project_dir,"DND APP Drachuri Edition Player_v2","www",cards[[1L]]$image)))
})

test("conditions and exhaustion enact skill and save rules", {
  char <- test_env$validate_character(list(status=list(conditions=c("poisoned","restrained"),exhaustion=3L)))
  ability <- test_env$character_roll_status(char,"ability","dex")
  save <- test_env$character_roll_status(char,"save","dex")
  stopifnot(ability$mode=="Disadvantage",save$mode=="Disadvantage")
  char$status$conditions <- c("blinded","stunned")
  sight <- test_env$character_roll_status(char,"ability","int",context="look for a hidden mark")
  strength_save <- test_env$character_roll_status(char,"save","str")
  stopifnot(isTRUE(sight$auto_fail),isTRUE(strength_save$auto_fail))
  stopifnot(grepl("hit point maximum halved",test_env$exhaustion_effect_text(4L),fixed=TRUE))
})

test("combat fullscreen expands only the map beneath persistent HUDs", {
  fullscreen_js <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","www","js","combat2d_simple.js"),warn=FALSE),collapse="\n")
  combat_css <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","www","css","combat.css"),warn=FALSE),collapse="\n")
  combat_server <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","debug_combat_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("document.body.appendChild(mapCard)",fullscreen_js,fixed=TRUE))
  stopifnot(grepl("document.body.appendChild(controls)",fullscreen_js,fixed=TRUE))
  stopifnot(!grepl("const fullscreenRoot = document.documentElement",fullscreen_js,fixed=TRUE))
  stopifnot(grepl("body.combat-document-fullscreen>.combat-map-card",combat_css,fixed=TRUE))
  stopifnot(grepl("body.combat-document-fullscreen>.combat-fullscreen-controls",combat_css,fixed=TRUE))
  stopifnot(grepl("[id$='partyhud_root']",combat_css,fixed=TRUE))
  stopifnot(grepl("[id$='status_card_dock']",combat_css,fixed=TRUE))
  stopifnot(grepl("body.combat-document-fullscreen .modal",combat_css,fixed=TRUE))
  stopifnot(grepl("background:rgba(255,255,245,.48)",combat_css,fixed=TRUE))
  stopifnot(grepl("left:max(24px, env(safe-area-inset-left))!important",combat_css,fixed=TRUE))
  stopifnot(grepl("close_actions_menu",combat_server,fixed=TRUE))
  stopifnot(grepl("Natural Magic can be cast on your turn",combat_server,fixed=TRUE))
  stopifnot(grepl('combat.css?v=',combat_server,fixed=TRUE))
  stopifnot(grepl('combat2d_simple.js?v=',combat_server,fixed=TRUE))
})

test("complete dice result card library is shipped", {
  dice_root <- file.path(project_dir, "DND APP Drachuri Edition Player_v2", "www", "assets", "dice-cards")
  dice_sides <- c(4L, 6L, 8L, 10L, 12L, 20L)
  expected <- unlist(lapply(dice_sides, function(sides) {
    file.path(dice_root, paste0("d", sides), sprintf("d%d-%02d.png", sides, seq_len(sides)))
  }), use.names = FALSE)
  stopifnot(length(expected) == 60L)
  stopifnot(all(file.exists(expected)))
})

test("dice cards and opt-in manual rolls share the normal resolution paths", {
  core <- test_env
  stopifnot(identical(core$dice_card_src(20L, 1L), "assets/dice-cards/d20/d20-01.png"))
  stopifnot(identical(core$manual_dice_values("4, 17", 20L, 2L), c(4L, 17L)))
  stopifnot(is.null(core$manual_dice_values("0, 21", 20L, 2L)))
  settings <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","www","settings.js"),warn=FALSE),collapse="\n")
  sidebar <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","sidebar_module.R"),warn=FALSE),collapse="\n")
  dice <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","dice_module.R"),warn=FALSE),collapse="\n")
  skills <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","skills_module.R"),warn=FALSE),collapse="\n")
  combat <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","debug_combat_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("manual_roll_mode",sidebar,fixed=TRUE),grepl("drachuri.manualRollMode",settings,fixed=TRUE))
  stopifnot(grepl("finish_roll(req$sides",dice,fixed=TRUE),grepl("roll(req$mode,values)",skills,fixed=TRUE))
  stopifnot(grepl("attack_rolls_override = manual_attack_rolls",combat,fixed=TRUE))
  stopifnot(grepl("dice_result_card_ui",dice,fixed=TRUE),grepl("dice_result_card_ui",skills,fixed=TRUE),grepl("dice_card_src(20L",combat,fixed=TRUE))
  stopifnot(grepl("modifier_result_card_ui",dice,fixed=TRUE),grepl("modifier_result_card_ui",skills,fixed=TRUE),grepl("modifier_result_card_ui(preview$attack_bonus",combat,fixed=TRUE))
})

test("blood inventory actions initialise and large-screen player UI remains usable", {
  blood <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "blood_module.R"), warn = FALSE), collapse = "\n")
  markers <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "character_3d_module.R"), warn = FALSE), collapse = "\n")
  party_hud <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "party_hud_module.R"), warn = FALSE), collapse = "\n")
  player_ui <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "ui.R"), warn = FALSE), collapse = "\n")
  stopifnot(grepl('observeEvent(blood_items()', blood, fixed = TRUE))
  stopifnot(grepl('observeEvent(heart_items()', blood, fixed = TRUE))
  stopifnot(length(gregexpr('ignoreInit = FALSE', blood, fixed = TRUE)[[1L]]) >= 2L)
  stopifnot(grepl('biological_item_kinds(df) == "blood"', blood, fixed = TRUE))
  stopifnot(grepl('biological_item_kinds(df) == "heart"', blood, fixed = TRUE))
  stopifnot(grepl('output$marker_preview <- renderUI', markers, fixed = TRUE))
  stopifnot(grepl('input$marker_2d_color', markers, fixed = TRUE))
  stopifnot(grepl('input$marker_3d_color', markers, fixed = TRUE))
  stopifnot(grepl('@media (min-width:1800px)', party_hud, fixed = TRUE))
  stopifnot(grepl('@media (min-width: 1800px)', player_ui, fixed = TRUE))
  stopifnot(grepl('max-width:none', player_ui, fixed = TRUE))
})

test("combat marker colour and aura changes reach live 3D tokens", {
  markers <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "character_3d_module.R"), warn = FALSE), collapse = "\n")
  player_combat <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "www", "js", "combat3d.js"), warn = FALSE), collapse = "\n")
  control_combat <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition 2 Control", "www", "js", "combat3d.js"), warn = FALSE), collapse = "\n")
  control_server <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition 2 Control", "control_app", "modules", "control_live_combat_module.R"), warn = FALSE), collapse = "\n")
  player_server <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "debug_combat_module.R"), warn = FALSE), collapse = "\n")

  stopifnot(grepl("Shiny.shinyapp.$inputValues", markers, fixed = TRUE))
  stopifnot(grepl("char_rev(char_rev() + 1L)", markers, fixed = TRUE))
  stopifnot(grepl("if (is_current_player) core$state$char", player_server, fixed = TRUE))
  stopifnot(grepl("marker_3d_color", control_server, fixed = TRUE))
  stopifnot(grepl("applyTokenMarkerAppearance(token, tile)", player_combat, fixed = TRUE))
  stopifnot(grepl("applyTokenMarkerAppearance(token, t)", player_combat, fixed = TRUE))
  stopifnot(grepl("applyTokenMarkerAppearance(token, tile)", control_combat, fixed = TRUE))
  stopifnot(grepl("applyTokenMarkerAppearance(token, t)", control_combat, fixed = TRUE))
})

test("blood stock, card sound preference and nested module sizing remain player friendly", {
  blood <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "blood_module.R"), warn = FALSE), collapse = "\n")
  control_inventory <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition 2 Control", "control_app", "modules", "control_inventory_module.R"), warn = FALSE), collapse = "\n")
  player_inventory <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "inventory_module.R"), warn = FALSE), collapse = "\n")
  player_ui <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "ui.R"), warn = FALSE), collapse = "\n")
  settings <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "www", "settings.js"), warn = FALSE), collapse = "\n")
  sidebar <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "sidebar_module.R"), warn = FALSE), collapse = "\n")
  marker <- paste(readLines(file.path(project_dir, "DND APP Drachuri Edition Player_v2", "server", "character_3d_module.R"), warn = FALSE), collapse = "\n")
  stopifnot(grepl('concentrations <- c(Diluted=10L,Standard=25L,Potent=50L,Concentrated=100L)', control_inventory, fixed = TRUE))
  stopifnot(grepl('cat_biological', control_inventory, fixed = TRUE))
  stopifnot(grepl('current < donor_cost', blood, fixed = TRUE))
  stopifnot(grepl('disabled=if(affordable)NULL else "disabled"', blood, fixed = TRUE))
  stopifnot(grepl('card_hover_sound', sidebar, fixed = TRUE))
  stopifnot(grepl('drachuri.cardHoverSound', settings, fixed = TRUE))
  stopifnot(grepl('drachuriCardHoverSoundEnabled()', player_ui, fixed = TRUE))
  stopifnot(grepl('#app-panel > .tab-content > .tab-pane.active', player_ui, fixed = TRUE))
  stopifnot(grepl('> .tabbable > .tab-content > .tab-pane{width:100%', player_inventory, fixed = TRUE))
  stopifnot(grepl('setInterval(syncMarkerPreview,250)', marker, fixed = TRUE))
})

test("shared chests use a persistent Sleight of Hand lockpicking flow", {
  migration<-paste(readLines(file.path(project_dir,"database","migrations","048_chests_and_lockpicking.sql"),warn=FALSE),collapse="\n")
  shared<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","session_db_core.R"),warn=FALSE),collapse="\n")
  chest<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","chest_module.R"),warn=FALSE),collapse="\n")
  control<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_merchants_module.R"),warn=FALSE),collapse="\n")
  player_ui<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","ui.R"),warn=FALSE),collapse="\n")
  player_server<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("CREATE TABLE chests",migration,fixed=TRUE),grepl("CREATE TABLE chest_items",migration,fixed=TRUE),grepl("CREATE TABLE chest_invitations",migration,fixed=TRUE))
  stopifnot(grepl("chest_lockpick_attempt",shared,fixed=TRUE),grepl("FOR UPDATE",shared,fixed=TRUE),grepl("take_chest_item",shared,fixed=TRUE))
  stopifnot(grepl("character_skill_cards(char,\"sleight of hand\")",chest,fixed=TRUE),grepl("tolerance_bonus",chest,fixed=TRUE),grepl("lockpicks_remaining",shared,fixed=TRUE),grepl("no usable lockpicks",shared,fixed=TRUE))
  stopifnot(grepl("Chest Generator",control,fixed=TRUE),grepl("Reveal Chest",control,fixed=TRUE))
  stopifnot(grepl("remaining<=0L",shared,fixed=TRUE),grepl("confirm_remove_chest",control,fixed=TRUE),grepl("confirm_close_merchant",control,fixed=TRUE))
  stopifnot(grepl("COALESCE(c.lock_kind,'chest')='chest'",shared,fixed=TRUE))
  stopifnot(grepl('if(kind!="chest")return(tagList(h3(tools::toTitleCase(noun)," Open")',chest,fixed=TRUE))
  stopifnot(grepl('source("server/chest_module.R")',player_ui,fixed=TRUE),grepl('chestUI("chests")',player_ui,fixed=TRUE),grepl('chestServer("chests",core$state)',player_server,fixed=TRUE))
})

test("unlocked doors and gates override blocking wall terrain", {
  player_map<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","combat_map_logic.R"),warn=FALSE),collapse="\n")
  control_map<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","server","combat_map_logic.R"),warn=FALSE),collapse="\n")
  expected<-"CASE WHEN o.object_type IN ('door','gate') THEN COALESCE(c.locked,TRUE) ELSE (t.blocks_movement OR o.object_type='chest') END AS effective_blocks_movement"
  sight<-"CASE WHEN o.object_type IN ('door','gate') AND COALESCE(c.locked,FALSE)=FALSE THEN FALSE"
  stopifnot(grepl(expected,player_map,fixed=TRUE),grepl(expected,control_map,fixed=TRUE))
  stopifnot(grepl(sight,player_map,fixed=TRUE),grepl(sight,control_map,fixed=TRUE))
})

test("enemy pools provide parchment portraits in the combat HUD", {
  hud<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","party_hud_module.R"),warn=FALSE),collapse="\n")
  snapshot<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","session_db_core.R"),warn=FALSE),collapse="\n")
  assets<-file.path(project_dir,"DND APP Drachuri Edition Player_v2","www","assets","enemy-portraits",paste0(c("humanoid","sorcerer","undead","beast","construct"),".png"))
  control_server<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","server.R"),warn=FALSE),collapse="\n")
  stopifnot(all(file.exists(assets)),grepl("enemy_portrait_file",hud,fixed=TRUE),grepl("enemy_portrait_base",hud,fixed=TRUE),grepl("player-assets/assets/enemy-portraits",control_server,fixed=TRUE),grepl("enemy_type = as.character(enemies$enemy_type",snapshot,fixed=TRUE))
})

test("party HUD keeps a fixed unclipped rail at laptop Safari widths", {
  hud<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","party_hud_module.R"),warn=FALSE),collapse="\n")
  player_fullscreen<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","www","css","combat.css"),warn=FALSE),collapse="\n")
  control_ui<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","ui.R"),warn=FALSE),collapse="\n")
  control_fullscreen<-paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","www","css","combat.css"),warn=FALSE),collapse="\n")
  stopifnot(grepl("padding: 0 10px 0 5px",hud,fixed=TRUE))
  stopifnot(grepl("align-items: stretch",hud,fixed=TRUE))
  stopifnot(!grepl("#control_partyhud-partyhud_root { position:relative",control_ui,fixed=TRUE))
  stopifnot(grepl("width: calc(100vw - 248px)",control_ui,fixed=TRUE))
  stopifnot(grepl("padding:0 10px 0 4px!important",player_fullscreen,fixed=TRUE))
  stopifnot(grepl("padding:0 10px 0 4px !important",control_fullscreen,fixed=TRUE))
})

test("Control encounter and map selectors survive shared Supabase refreshes", {
  setup <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_encounter_setup_module.R"),warn=FALSE),collapse="\n")
  maps <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_map_builder_module.R"),warn=FALSE),collapse="\n")
  live <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_live_combat_module.R"),warn=FALSE),collapse="\n")
  sessions <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_sessions_module.R"),warn=FALSE),collapse="\n")
  players <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_players_module.R"),warn=FALSE),collapse="\n")
  inventory <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_inventory_module.R"),warn=FALSE),collapse="\n")
  climate <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_geography_climate_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("encounter_choice_signature <- reactiveVal(NULL)",setup,fixed=TRUE))
  stopifnot(grepl("encounter_map_choice_signature <- reactiveVal(NULL)",setup,fixed=TRUE))
  stopifnot(grepl("npc_template_choice_signature <- reactiveVal(NULL)",setup,fixed=TRUE))
  stopifnot(grepl("as.integer(input$encounter_id %||% NA)",setup,fixed=TRUE))
  stopifnot(grepl("isolate(input$map_id) %||% ctrl$map_id",setup,fixed=TRUE))
  stopifnot(!grepl("ctrl$encounter_id %||% input$encounter_id",setup,fixed=TRUE))
  stopifnot(grepl("map_choice_signature <- reactiveVal(NULL)",maps,fixed=TRUE))
  stopifnot(grepl("isolate(input$map_select) %||% ctrl$map_id",maps,fixed=TRUE))
  stopifnot(!grepl("ctrl$map_id %||% input$map_select",maps,fixed=TRUE))
  stopifnot(grepl("live_encounter_choice_signature <- reactiveVal(NULL)",live,fixed=TRUE))
  stopifnot(grepl("isolate(input$encounter_select) %||% current_encounter_id()",live,fixed=TRUE))
  stopifnot(grepl("attack_target_choice_signature <- reactiveVal(NULL)",live,fixed=TRUE))
  stopifnot(grepl("reinforcement_enemy_choice_signature <- reactiveVal(NULL)",live,fixed=TRUE))
  stopifnot(grepl("isolate(input$target_id)",live,fixed=TRUE))
  stopifnot(grepl("session_choice_signature <- reactiveVal(NULL)",sessions,fixed=TRUE))
  stopifnot(grepl("isolate(input$session_select) %||% ctrl$session_id",sessions,fixed=TRUE))
  stopifnot(grepl("remove_player_choice_signature <- reactiveVal(NULL)",players,fixed=TRUE))
  stopifnot(grepl("player_choice_signature <- reactiveVal(NULL)",inventory,fixed=TRUE))
  stopifnot(grepl("clock_signature<-reactiveVal(NULL)",climate,fixed=TRUE))
})

test("polled session state reads do not write-lock shared live rows", {
  shared <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","session_db_core.R"),warn=FALSE),collapse="\n")
  environment_helper <- sub(".*get_session_environment <- function", "get_session_environment <- function", shared)
  environment_helper <- sub("set_session_environment <- function.*", "", environment_helper)
  supplies_helper <- sub(".*get_session_supplies <- function", "get_session_supplies <- function", shared)
  supplies_helper <- sub("adjust_session_supply <- function.*", "", supplies_helper)
  stopifnot(grepl("SELECT * FROM session_environment_state WHERE session_id=$1",environment_helper,fixed=TRUE))
  stopifnot(grepl("ON CONFLICT(session_id) DO NOTHING RETURNING *",environment_helper,fixed=TRUE))
  stopifnot(!grepl("DO UPDATE SET session_id=EXCLUDED.session_id",environment_helper,fixed=TRUE))
  stopifnot(grepl("SELECT * FROM session_supplies WHERE session_id=$1",supplies_helper,fixed=TRUE))
  stopifnot(grepl("ON CONFLICT(session_id) DO NOTHING RETURNING *",supplies_helper,fixed=TRUE))
  stopifnot(!grepl("DO UPDATE SET session_id=EXCLUDED.session_id",supplies_helper,fixed=TRUE))
})

test("rest allocations do not queue the whole party or trigger polling autosaves", {
  shared <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","session_db_core.R"),warn=FALSE),collapse="\n")
  rest <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","rest_module.R"),warn=FALSE),collapse="\n")
  player_server <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server.R"),warn=FALSE),collapse="\n")
  allocation <- sub(".*allocate_session_phase_time <- function", "allocate_session_phase_time <- function", shared)
  allocation <- sub("set_phase_watches_open <- function.*", "", allocation)
  stopifnot(grepl("pg_advisory_xact_lock",allocation,fixed=TRUE))
  stopifnot(grepl("FOR SHARE",allocation,fixed=TRUE))
  stopifnot(!grepl("FOR UPDATE",allocation,fixed=TRUE))
  stopifnot(grepl("shared_supplies<-reactiveVal(NULL)",rest,fixed=TRUE))
  stopifnot(!grepl("if(!identical(as.integer(x$meta$day",rest,fixed=TRUE))
  stopifnot(grepl("last_saved_character(character_signature(update$character))",player_server,fixed=TRUE))
})

test("watch rotas scale to party size and cover the whole rest phase", {
  two_players <- test_env$session_watch_slots(12, 2L)
  three_players <- test_env$session_watch_slots(12, 3L)
  four_players <- test_env$session_watch_slots(12, 4L)
  stopifnot(identical(two_players$hours, c(6, 6)))
  stopifnot(identical(three_players$hours, c(4, 4, 4)))
  stopifnot(identical(four_players$hours, c(3, 3, 3, 3)))
  stopifnot(two_players$start[[1L]] == 0, tail(two_players$end, 1L) == 12)
  stopifnot(all(head(three_players$end, -1L) == tail(three_players$start, -1L)))
  rest <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","rest_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("watch length adapts", rest, fixed = TRUE))
  stopifnot(grepl("session_watch_slots", rest, fixed = TRUE))
})

test("combat refresh and turn advancement avoid duplicate database work", {
  shared <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","shared","session_db_core.R"),warn=FALSE),collapse="\n")
  player_server <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server.R"),warn=FALSE),collapse="\n")
  control_server <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","server.R"),warn=FALSE),collapse="\n")
  advance <- sub(".*advance_turn <- function", "advance_turn <- function", shared)
  advance <- sub("end_encounter_combat <- function.*", "", advance)
  movement <- sub(".*upsert_encounter_actor_position <- function", "upsert_encounter_actor_position <- function", shared)
  movement <- sub("get_encounter_positions <- function.*", "", movement)
  stopifnot(grepl("pg_advisory_xact_lock",advance,fixed=TRUE))
  stopifnot(!grepl("get_encounter_actors(encounter_id)",advance,fixed=TRUE))
  stopifnot(!grepl("set_combat_state(",advance,fixed=TRUE))
  stopifnot(grepl("ON CONFLICT(encounter_id,actor_type,actor_id)",movement,fixed=TRUE))
  stopifnot(grepl("no unique or exclusion constraint",movement,fixed=TRUE))
  stopifnot(grepl("UPDATE encounter_positions SET x=$4,y=$5",movement,fixed=TRUE))
  migration <- paste(readLines(file.path(project_dir,"database","migrations","055_encounter_position_actor_unique.sql"),warn=FALSE),collapse="\n")
  stopifnot(grepl("CREATE UNIQUE INDEX IF NOT EXISTS encounter_positions_actor_unique_idx",migration,fixed=TRUE))
  stopifnot(!grepl("get_session_overview(sid)",player_server,fixed=TRUE))
  stopifnot(!grepl("get_session_overview(sid)",control_server,fixed=TRUE))
  stopifnot(grepl("positions_tbl <- reactive({control_live_snapshot()$positions",control_server,fixed=TRUE))
})

test("equipment catalogue resolves to illustrated weapon and armour thumbnails", {
  asset_root <- file.path(project_dir,"DND APP Drachuri Edition Player_v2","www","assets","equipment-thumbnails")
  expected <- file.path(asset_root,c(
    "weapons/longsword.png","weapons/dagger.png","weapons/battleaxe.png","weapons/spear.png",
    "weapons/bow.png","weapons/staff.png","weapons/hammer.png","weapons/mace.png",
    "weapons/flail.png","weapons/polearm.png","weapons/trident.png","weapons/sickle.png",
    "weapons/club.png","weapons/greatsword.png","weapons/rapier.png",
    "armour/leather-armour.png","armour/chain-mail.png","armour/plate-armour.png",
    "armour/hide-armour.png","armour/scale-mail.png","armour/splint-armour.png",
    "armour/padded-armour.png","armour/round-shield.png","armour/kite-shield.png",
    "armour/helm.png","armour/chain-coif.png","armour/circlet.png"
  ))
  stopifnot(all(file.exists(expected)))
  stopifnot(identical(test_env$equipment_thumbnail_src("Rimeblade Rapier","weapon"),"assets/equipment-thumbnails/weapons/rapier.png"))
  stopifnot(identical(test_env$equipment_thumbnail_src("Drachuri Tower Shield","armor"),"assets/equipment-thumbnails/armour/kite-shield.png"))
  stopifnot(identical(test_env$equipment_thumbnail_src("Twilight Veil","armor"),"assets/equipment-thumbnails/armour/helm.png"))
  stopifnot(identical(test_env$equipment_thumbnail_src("Entirely New Blade","weapon"),"assets/equipment-thumbnails/weapons/longsword.png"))
})

test("player HUD exposes the shared Annwn wall map as a fullscreen overlay", {
  module <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","world_map_module.R"),warn=FALSE),collapse="\n")
  ui <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","ui.R"),warn=FALSE),collapse="\n")
  server <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server.R"),warn=FALSE),collapse="\n")
  combat_css <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","www","css","combat.css"),warn=FALSE),collapse="\n")
  stopifnot(grepl("assets/textures/map_annwn_world.jpg",module,fixed=TRUE))
  stopifnot(grepl("position:fixed;inset:0",module,fixed=TRUE))
  stopifnot(grepl('worldMapUI("worldmap")',ui,fixed=TRUE))
  stopifnot(grepl('worldMapServer("worldmap")',server,fixed=TRUE))
  stopifnot(grepl("worldmap-wrap",combat_css,fixed=TRUE))
})

test("Control attack previews cannot survive movement or reuse stale range state", {
  live <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_live_combat_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("validate_pending_attack_position <- function(preview)",live,fixed=TRUE))
  stopifnot(grepl("The attacker or target moved after this roll",live,fixed=TRUE))
  stopifnot(grepl("pending_attack(NULL)\n      \n      attacker_id <- active_actor_id()",live,fixed=TRUE))
  stopifnot(grepl("pending_attack(NULL)\n\n      opportunity_context <- opportunity_attack_context()",live,fixed=TRUE))
  stopifnot(grepl("range_ft = suppressWarnings(as.integer(atk$range_ft",live,fixed=TRUE))
  stopifnot(grepl("position_check <- validate_pending_attack_position(preview)",live,fixed=TRUE))
})

test("Control loads canonical glyph zone handlers before live combat", {
  control_global <- paste(readLines(control_global_file, warn = FALSE), collapse = "\n")
  live <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_live_combat_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl('file.path(player_app_dir, "shared", "glyph_core.R")', control_global, fixed = TRUE))
  stopifnot(grepl("trigger_rune_zone_entry", live, fixed = TRUE))
  stopifnot(grepl("get_active_glyph_zones", live, fixed = TRUE))
})

test("Control refreshes encounter status after explicit combat mutations", {
  live <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition 2 Control","control_app","modules","control_live_combat_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("encounter_key <- reactiveVal(0L)", live, fixed = TRUE))
  stopifnot(grepl("bump_encounter <- function()", live, fixed = TRUE))
  stopifnot(grepl("bump_enemies()\n      bump_encounter()\n      bump_map_visual()", live, fixed = TRUE))
  stopifnot(grepl("ctrl$refresh_key\n      encounter_key()\n      sid <- current_session_id()", live, fixed = TRUE))
})

test("Player waits for combat canvas replacement and retries map delivery", {
  player_combat <- paste(readLines(file.path(project_dir,"DND APP Drachuri Edition Player_v2","server","debug_combat_module.R"),warn=FALSE),collapse="\n")
  stopifnot(grepl("session$onFlushed(function()", player_combat, fixed = TRUE))
  stopifnot(grepl("later::later(send_map_message, delay = 0.35)", player_combat, fixed = TRUE))
  stopifnot(grepl("later::later(send_map_message, delay = 0.9)", player_combat, fixed = TRUE))
})

cat("\n", tests_run, " tests passed.\n", sep = "")
