project_dir <- normalizePath(Sys.getenv("DND_PROJECT_DIR"))
player_dir <- file.path(project_dir, "DND APP Drachuri Edition Player_v2")

setwd(player_dir)
source("global.R")
source("session_db.R")
source("plug/game_data.R")
source("server/level_module.R")
source("shared/class_feature_core.R")
on.exit(close_db_pool(), add = TRUE)

expected <- list(
  `1001` = c("sneak_attack", "cunning_action", "fast_hands", "second_story_work", "uncanny_dodge", "expertise"),
  `1002` = c("sneak_attack", "cunning_action", "assassinate", "bonus_proficiencies", "uncanny_dodge", "expertise"),
  `1003` = c("fighting_style", "second_wind", "action_surge", "improved_critical", "extra_attack", "asi"),
  `1004` = c("fighting_style", "second_wind", "action_surge", "combat_superiority", "student_of_war", "extra_attack", "asi"),
  `1005` = c("rage", "reckless_attack", "frenzy", "extra_attack", "fast_movement", "mindless_rage"),
  `1006` = c("rage", "reckless_attack", "spirit_totem", "extra_attack", "fast_movement", "aspect_of_the_beast"),
  `1007` = c("blood_magic", "bloodthirsty", "natural_magic", "seer", "thermal_wild_magic", "balance"),
  `1008` = c("blood_magic", "bloodthirsty", "natural_magic", "exquisite_taste", "shadow_step", "thermal_wild_magic", "predator"),
  `1009` = c("water_channeler", "mind_bender", "detect_undead", "spellsword", "adept_sorcerer", "electromagnetic", "combat_magic"),
  `1010` = c("water_channeler", "mind_bender", "detect_undead", "wild_insight", "adept_sorcerer", "electromagnetic", "divination", "mislead")
)

for (character_id in names(expected)) {
  char <- validate_character(load_character_from_db(character_id))
  features <- get_unlocked_class_features(char)
  feature_ids <- vapply(features, function(feature) as.character(feature$id), character(1))
  missing <- setdiff(expected[[character_id]], feature_ids)
  stopifnot(length(missing) == 0L)
  stopifnot(as.integer(char$resources$hp$max) == 60L)
}

hanianol <- validate_character(load_character_from_db("1008"))
bite <- Filter(function(feature) identical(feature$id, "bloodthirsty"), get_unlocked_combat_actions(hanianol))[[1L]]$action
bite_damage <- resolve_class_action_damage(
  bite, target_max_hp = 10L, char = hanianol,
  roll_function = function(expr) list(total = 5L, rolls = 5L)
)
stopifnot(identical(bite_damage$amount, 8L), identical(bite_damage$damage_type, "piercing"))

snapshot <- get_player_live_snapshot(1L, "1008")
enemy_id <- as.character(snapshot$enemies$enemy_uuid[[1L]])
before <- as.integer(snapshot$enemies$hp_current[[1L]])
damaged <- damage_encounter_enemy(1L, enemy_id, bite_damage$amount)
stopifnot(identical(damaged$hp_before, before), identical(damaged$hp_after, max(0L, before - 8L)))

for (character_id in c("1007", "1008", "1009", "1010")) {
  char <- validate_character(load_character_from_db(character_id))
  spells <- get_unlocked_class_spells(char)
  stopifnot(length(spells) > 0L)
  stopifnot(any(vapply(spells, function(spell) as.integer(spell$level) >= 5L, logical(1))))
}

cat("PASS: ten QA characters unlock expected features; Bite and level 5-6 spell paths resolve locally.\n")
