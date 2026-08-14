# =============================
# Character Migration Utilities
# =============================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# -----------------------------
# MAIN MIGRATION FUNCTION
# -----------------------------
migrate_v1_to_v2 <- function(old) {
  
  now <- Sys.time()
  
  new <- list()
  
  # =============================
  # VERSION
  # =============================
  new$save_version <- 2
  
  # =============================
  # META
  # =============================
  new$meta <- list(
    race = old$char_race %||% "",
    name = old$char_profile %||% "Unknown",
    created_at = old$timestamp %||% now,
    updated_at = now
  )
  
  # =============================
  # BUILD
  # =============================
  new$build <- list(
    class = old$char_class %||% "",
    path = old$char_sub_class %||% "",
    level = old$level %||% 1,
    classes = list(list(
      class = old$char_class %||% "",
      level = as.numeric(old$class_total %||% old$level %||% 1),
      subclass = old$char_sub_class %||% ""
    ))
  )
  
  # =============================
  # ABILITIES
  # =============================
  new$abilities <- list(
    str = old$str %||% 10,
    dex = old$dex %||% 10,
    con = old$con %||% 10,
    int = old$int %||% 10,
    cha = old$cha %||% 10,
    bld_str = old$bld_str %||% 10
  )
  
  # =============================
  # PROFICIENCIES
  # =============================
  
  all_skills <- c(
    "acrobatics","animal_handling","arcana","athletics","deception",
    "history","insight","intimidation","investigation","medicine",
    "nature","perception","performance","persuasion","religion",
    "sleight_of_hand","stealth","survival"
  )
  
  skill_list <- setNames(rep("None", length(all_skills)), all_skills)
  
  if (!is.null(old$skills)) {
    for (n in names(old$skills)) {
      clean <- sub("^prof_", "", n)
      if (clean %in% all_skills) {
        skill_list[[clean]] <- old$skills[[n]]
      }
    }
  }
  
  new$prof <- list(
    saves = list(
      str = old$save_prof_str %||% FALSE,
      dex = old$save_prof_dex %||% FALSE,
      con = old$save_prof_con %||% FALSE,
      int = old$save_prof_int %||% FALSE,
      bld_str = old$save_prof_bld_str %||% FALSE,
      cha = old$save_prof_cha %||% FALSE
    ),
    skills = skill_list
  )
  
  # =============================
  # BLOOD
  # =============================
  new$blood <- list(
    inventory = old$blood_inventory %||% data.frame(),
    addiction = old$addiction_state %||% list(
      stage = 1,
      days_at_stage = 0,
      previous_day_intake = 0,
      current_day_intake = 0
    )
  )
  
  # =============================
  # RESOURCES
  # =============================
  
  sindre_total_fixed <- if (!is.null(old$sindre_total) && old$sindre_total > 0) {
    old$sindre_total
  } else {
    old$current_sindre %||% 0
  }
  
  new$resources <- list(
    hp = list(
      max = old$max_hp %||% 1,
      cur = old$current_hp %||% 1,
      temp = old$temp_hp %||% 0
    ),
    
    sindre = list(
      cur = old$current_sindre %||% 0,
      total = sindre_total_fixed,
      regen = old$sindre_regen %||% 0,
      flow = old$sindre_flow %||% 0,
      locked = old$sindre_locked %||% 0,
      bound = old$bound_sindre %||% 0,
      class = old$sindre_class %||% 1,
      
      tiers = list(
        refill = as.numeric(old$class_refill %||% 0),
        total  = as.numeric(old$class_total %||% 0),
        flow   = as.numeric(old$class_flow %||% 0),
        locked = as.numeric(old$class_locked %||% 0),
        bound  = as.numeric(old$class_bound %||% 0)
      )
    ),
    
    rations = list(cur = 0),
    
    blood = list(
      inventory = old$blood_inventory %||% data.frame(),
      addiction = old$addiction_state %||% list()
    )
  )
  
  # =============================
  # STATUS
  # =============================
  new$status <- list(
    conditions = character(0),
    effects = character(0),
    exhaustion = 0
  )
  
  # =============================
  # JOURNAL LOG
  # =============================
  new$journal <- list(
    log = if (!is.null(old$diary) && old$diary != "") {
      paste0(format(now, "%H:%M:%S"), " | Imported old diary")
    } else {
      character(0)
    }
  )
  
  # =============================
  # COMBAT - WEAPONS
  # =============================
  weapons <- old$weapons %||% data.frame()
  
  if (nrow(weapons) > 0) {
    
    weapons$name <- ifelse(
      grepl("^Weapon w", weapons$name),
      "Unarmed / Default",
      weapons$name
    )
    
    weapons$adv <- "Normal"
    weapons$damage1 <- weapons$damage
    weapons$dmg_type1 <- weapons$dmg_type
    weapons$damage2 <- ""
    weapons$dmg_type2 <- "Other"
    weapons$in_bag <- FALSE
    weapons$edit <- FALSE
    
    weapons$damage <- NULL
    weapons$dmg_type <- NULL
    
    weapons <- unique(weapons)
  }
  
  # =============================
  # COMBAT - ARMOR
  # =============================
  armor <- old$armor %||% data.frame()
  
  if (nrow(armor) > 0) {
    armors <- data.frame(
      id = paste0("a", seq_len(nrow(armor))),
      name = armor$name,
      base_ac = armor$base_ac,
      type = armor$type,
      custom_max_dex = NA,
      proficient = armor$proficient,
      worn = armor$worn,
      in_bag = FALSE,
      edit = FALSE
    )
  } else {
    armors <- data.frame(
      id = character(),
      name = character(),
      base_ac = numeric(),
      type = character(),
      custom_max_dex = numeric(),
      proficient = logical(),
      worn = logical(),
      in_bag = logical(),
      edit = logical()
    )
  }
  
  new$combat <- list(
    weapons = weapons,
    armors = armors
  )
  
  # =============================
  # DIARY (STRUCTURED)
  # =============================
  if (!is.null(old$diary) && old$diary != "") {
    
    new$diary <- list(
      imported_old_journal = TRUE,
      entries = data.frame(
        id = paste0("d", as.integer(now)),
        ts = old$timestamp %||% now,
        title = "Imported Entry",
        mood = "Neutral",
        tags = "",
        body = old$diary,
        hp_cur = old$current_hp %||% 0,
        hp_max = old$max_hp %||% 0,
        hp_temp = old$temp_hp %||% 0,
        effects = ""
      )
    )
    
  } else {
    
    new$diary <- list(
      imported_old_journal = FALSE,
      entries = data.frame()
    )
  }
  
  return(new)
}

# -----------------------------
# SAFE LOAD FUNCTION
# -----------------------------
load_character <- function(path) {
  
  char <- readRDS(path)
  
  # Detect old format (no version)
  if (is.null(char$save_version)) {
    message("Migrating old save file...")
    char <- migrate_v1_to_v2(char)
  }
  
  return(char)
}

# -----------------------------
# OPTIONAL VALIDATION
# -----------------------------
validate_character <- function(char) {
  
  stopifnot(!is.null(char$meta$name))
  stopifnot(!is.null(char$abilities$str))
  stopifnot(!is.null(char$resources$hp))
  stopifnot(!is.null(char$combat$weapons))
  
  TRUE
}


char <- load_character("Eman_character-1.rds")

validate_character(char)

saveRDS(char, "Eman_migrated.rds")