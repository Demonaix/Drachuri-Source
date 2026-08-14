CLASS_FEATURE_TAGS <- c(
  "stat_increase", "ability", "combat", "spell", "passive",
  "reaction", "movement", "healing", "utility", "subclass"
)

CLASS_FEATURE_MECHANICS <- list(
  "Hanianol Sorcerer::1::fae_blooded" = list(
    tags = c("passive", "utility"),
    effects = list(skills = c("survival" = "Expertise"))
  ),
  "Hanianol Sorcerer::2::natural_magic" = list(
    tags = c("ability", "utility", "spell"),
    effects = list(skills = c("medicine" = "Proficient"))
  ),
  "Na'Haran Sorcerer::1::survival_mastery" = list(
    tags = c("passive", "utility"),
    effects = list(skills = c("survival" = "Expertise"))
  ),
  "Rogue::3::bonus_proficiencies" = list(
    tags = c("passive", "utility"),
    effects = list(tools = c("disguise_kit", "poisoners_kit"))
  ),
  "Barbarian::1::rage" = list(
    tags = c("ability", "combat"),
    effects = list(conditional = list(list(
      when = "raging",
      resistances = c("bludgeoning", "piercing", "slashing")
    )))
  ),
  "Barbarian::6::mindless_rage" = list(
    tags = c("passive", "combat"),
    effects = list(conditional = list(list(
      when = "raging",
      condition_immunities = c("charmed", "frightened")
    )))
  ),
  "Hanianol Sorcerer::1::bloodthirsty" = list(
    tags = c("ability", "combat", "healing"),
    action = list(
      name = "Bloodthirsty Bite",
      target = "enemy",
      damage = list(mode = "dice_plus_modifier", value = "1d8", stat = "str", type = "piercing"),
      note = "Counts as drinking half a pint of blood. Blood restoration remains narrative/DM controlled."
    )
  ),
  "Na'Haran Sorcerer::1::water_channeler" = list(
    tags = c("ability", "combat", "spell"),
    action = list(
      name = "Water Channeler",
      group = "water_channeler",
      target = "enemy",
      damage = list(mode = "percent_max_hp", value = 0.10, type = "necrotic"),
      resource = list(name = "sindre", cost = 10L)
    )
  ),
  "Na'Haran Sorcerer::11::improved_channeling" = list(
    tags = c("ability", "combat", "spell"),
    action = list(
      name = "Improved Water Channeler",
      group = "water_channeler",
      target = "enemy",
      damage = list(mode = "percent_max_hp", value = 0.20, type = "necrotic"),
      resource = list(name = "sindre", cost = 10L)
    )
  )
)

infer_class_feature_tags <- function(feature_id, feature) {
  feature_id <- tolower(as.character(feature_id %||% ""))
  text <- tolower(paste(
    feature_id,
    as.character(feature$name %||% ""),
    as.character(feature$desc %||% "")
  ))

  tags <- character()
  add <- function(tag, pattern) if (grepl(pattern, text, perl = TRUE)) tag else character()
  tags <- c(
    tags,
    add("stat_increase", "ability score|boost .*abilit|\\basi\\b"),
    add("subclass", "subclass|archetype|primal path|sorcerous path"),
    add("spell", "spell|magic|sindre|conjur|divination|radiant|necrotic"),
    add("combat", "attack|damage|armour|armor|\\bac\\b|critical|resistance|rage|combat"),
    add("reaction", "reaction"),
    add("movement", "movement|dash|teleport|shadow step|climb|jump"),
    add("healing", "regain hp|restore hp|overheal|healing"),
    add("utility", "proficien|check|detect|identify|insight|survival|persuasion|stealth"),
    add("passive", "advantage|double proficiency|no longer|while |gain resistance")
  )
  tags <- unique(tags)
  if (!length(tags)) tags <- "ability"
  intersect(tags, CLASS_FEATURE_TAGS)
}

class_feature_metadata <- function(class_name, level, feature_id, feature, subclass = "") {
  key <- paste(class_name, level, feature_id, sep = "::")
  override <- CLASS_FEATURE_MECHANICS[[key]] %||% list()
  tags <- unique(c(infer_class_feature_tags(feature_id, feature), override$tags %||% character()))

  list(
    id = as.character(feature_id),
    name = as.character(feature$name %||% feature_id),
    desc = as.character(feature$desc %||% ""),
    tags = tags,
    class = as.character(class_name),
    subclass = as.character(subclass %||% ""),
    level = as.integer(level),
    action = override$action %||% NULL,
    effects = override$effects %||% NULL
  )
}

get_unlocked_class_features <- function(char, class_defs = CLASSES) {
  classes <- normalise_character_classes(char, class_defs)
  out <- list()

  append_levels <- function(class_name, subclass, levels, max_level) {
    if (!is.list(levels)) return()
    for (level_name in names(levels)) {
      level <- suppressWarnings(as.integer(level_name))
      if (is.na(level) || level > max_level) next
      features <- levels[[level_name]]$features %||% list()
      for (feature_id in names(features)) {
        metadata <- class_feature_metadata(
          class_name, level, feature_id, features[[feature_id]], subclass
        )
        out[[paste(class_name, subclass, level, feature_id, sep = "::")]] <<- metadata
      }
    }
  }

  for (entry in classes) {
    class_name <- as.character(entry$class %||% "")
    class_data <- class_defs[[class_name]] %||% list()
    level <- suppressWarnings(as.integer(entry$level %||% 1L))
    append_levels(class_name, "", class_data$levels, level)

    subclass <- as.character(entry$subclass %||% "")
    if (nzchar(subclass)) {
      append_levels(class_name, subclass, class_data$subclasses[[subclass]]$levels, level)
    }
  }

  unname(out)
}

get_unlocked_combat_actions <- function(char, class_defs = CLASSES) {
  features <- get_unlocked_class_features(char, class_defs)
  actions <- Filter(function(feature) is.list(feature$action), features)
  if (!length(actions)) return(actions)

  groups <- vapply(actions, function(feature) {
    as.character(feature$action$group %||% paste0("feature:", feature$id))
  }, character(1))
  keep <- !duplicated(groups, fromLast = TRUE)
  actions[keep]
}

resolve_class_action_damage <- function(action, target_max_hp, char,
                                        roll_function = roll_dice_expr) {
  damage <- action$damage %||% list()
  mode <- as.character(damage$mode %||% "fixed")
  target_max_hp <- suppressWarnings(as.integer(target_max_hp))
  if (is.na(target_max_hp) || target_max_hp < 1L) target_max_hp <- 1L

  amount <- switch(
    mode,
    percent_max_hp = max(1L, floor(target_max_hp * as.numeric(damage$value %||% 0))),
    dice_plus_modifier = {
      rolled <- roll_function(as.character(damage$value %||% "1d4"))
      stat <- as.character(damage$stat %||% "str")
      ability <- suppressWarnings(as.integer(char$abilities[[stat]] %||% 10L))
      if (is.na(ability)) ability <- 10L
      modifier <- floor((ability - 10L) / 2L)
      max(0L, as.integer(rolled$total %||% 0L) + modifier)
    },
    suppressWarnings(as.integer(damage$value %||% 0L))
  )
  if (is.na(amount)) amount <- 0L

  list(
    amount = as.integer(amount),
    damage_type = as.character(damage$type %||% "")
  )
}

apply_unlocked_class_effects <- function(char, class_defs = CLASSES) {
  features <- get_unlocked_class_features(char, class_defs)
  rank <- c("None" = 0L, "Proficient" = 1L, "Expertise" = 2L)

  char$prof <- char$prof %||% list()
  char$prof$skills <- char$prof$skills %||% list()
  char$prof$tools <- char$prof$tools %||% list()
  char$combat_profile <- char$combat_profile %||% list()
  for (field in c("resistances", "immunities", "vulnerabilities")) {
    char$combat_profile[[field]] <- unique(tolower(as.character(
      char$combat_profile[[field]] %||% character()
    )))
  }

  sources <- list()
  conditional <- list()

  for (feature in features) {
    effects <- feature$effects %||% list()
    if (!length(effects)) next
    source_key <- paste(feature$class, feature$subclass, feature$level, feature$id, sep = "::")

    skills <- effects$skills %||% character()
    for (skill in names(skills)) {
      granted <- as.character(skills[[skill]])
      current <- as.character(char$prof$skills[[skill]] %||% "None")
      if ((rank[[granted]] %||% 0L) > (rank[[current]] %||% 0L)) {
        char$prof$skills[[skill]] <- granted
      }
      sources[[paste0("skill:", skill)]] <- unique(c(
        sources[[paste0("skill:", skill)]] %||% character(), source_key
      ))
    }

    for (tool in as.character(effects$tools %||% character())) {
      char$prof$tools[[tool]] <- TRUE
      sources[[paste0("tool:", tool)]] <- unique(c(
        sources[[paste0("tool:", tool)]] %||% character(), source_key
      ))
    }

    trait_map <- c(
      resistances = "resistances",
      immunities = "immunities",
      vulnerabilities = "vulnerabilities"
    )
    for (effect_name in names(trait_map)) {
      values <- tolower(as.character(effects[[effect_name]] %||% character()))
      field <- trait_map[[effect_name]]
      char$combat_profile[[field]] <- unique(c(char$combat_profile[[field]], values[nzchar(values)]))
      for (value in values[nzchar(values)]) {
        sources[[paste0(effect_name, ":", value)]] <- unique(c(
          sources[[paste0(effect_name, ":", value)]] %||% character(), source_key
        ))
      }
    }

    feature_conditional <- effects$conditional %||% list()
    if (length(feature_conditional)) {
      conditional[[source_key]] <- feature_conditional
    }
  }

  char$derived_effects <- char$derived_effects %||% list()
  char$derived_effects$sources <- sources
  char$derived_effects$conditional <- conditional
  char
}
