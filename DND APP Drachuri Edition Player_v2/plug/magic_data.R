# plug/magic_data.R
classification_table <- data.frame(
  ClassLevel = 0:6,
  RefillRate = c(0, 4, 6, 8, 10, 15, 20),
  SindreLevel = c(0,30, 50, 70, 90, 140, 180),
  MaxFlow = c(0, 20, 40, 60, 80, 120, 160),
  Locked = c(0, 40, 80,120, 160, 240, 480),
  Bound = c(0, 1, 2, 3, 4, 5, 6),
  stringsAsFactors = FALSE
)


# =============================
# Rune Rules
# =============================

RUNE_TYPES <- c("Minor", "Major", "Arcane", "Cursed")
RUNE_MATERIALS <- c("Clay", "Wood", "Stone", "Metal")

RUNE_LEVEL_UNLOCKS <- list(
  Minor = 1,
  Major = 5,
  Arcane = 10,
  Cursed = 15
)

RUNE_MATERIAL_RULES <- list(
  Clay = list(
    tools = "None",
    active_time_sec = 6,
    arcane_score_base = 5
  ),
  Wood = list(
    tools = "Wooden chisel",
    active_time_sec = 12,
    arcane_score_base = 10
  ),
  Stone = list(
    tools = "Stone chisel",
    active_time_sec = 18,
    arcane_score_base = 15
  ),
  Metal = list(
    tools = "Welding tools",
    active_time_sec = 36,
    arcane_score_base = 20
  )
)

RUNE_CRAFTING_RULES <- list(
  Minor = list(
    Clay = list(time = "1d8", cost = "1d4 Sindre", instability = "1d4"),
    Wood = list(time = "1d12", cost = "1d4 Sindre", instability = "1d8"),
    Stone = list(time = "1d20", cost = "1d4 Sindre", instability = "1d12"),
    Metal = list(time = "1d100", cost = "1d4 Sindre", instability = "1d20")
  ),
  Major = list(
    Clay = list(time = "2d8", cost = "1d8 Sindre", instability = "2d4"),
    Wood = list(time = "2d12", cost = "1d8 Sindre", instability = "2d8"),
    Stone = list(time = "2d20", cost = "1d8 Sindre", instability = "2d12"),
    Metal = list(time = "2d100", cost = "1d8 Sindre", instability = "2d20")
  ),
  Arcane = list(
    Clay = list(time = "3d8", cost = "1d12 Sindre", instability = "3d4"),
    Wood = list(time = "3d12", cost = "1d12 Sindre", instability = "3d8"),
    Stone = list(time = "3d20", cost = "1d12 Sindre", instability = "3d12"),
    Metal = list(time = "3d100", cost = "1d12 Sindre", instability = "4d20")
  ),
  Cursed = list(
    Metal = list(time = "3d100", cost = "1d12 HP", instability = "3d20")
  )
)

get_available_rune_types <- function(level) {
  names(Filter(function(req) level >= req, RUNE_LEVEL_UNLOCKS))
}

get_available_rune_materials <- function(rune_type) {
  names(RUNE_CRAFTING_RULES[[rune_type]])
}

get_rune_rule <- function(rune_type, material, arcana_skill = 0) {
  craft <- RUNE_CRAFTING_RULES[[rune_type]][[material]]
  mat <- RUNE_MATERIAL_RULES[[material]]
  
  if (is.null(craft) || is.null(mat)) {
    return(NULL)
  }
  
  list(
    rune_type = rune_type,
    material = material,
    required_blank = paste(material, "blank"),
    required_tools = mat$tools,
    crafting_time = craft$time,
    cost = craft$cost,
    active_time_sec = mat$active_time_sec,
    active_time_rounds = mat$active_time_sec / 6,
    arcane_score = mat$arcane_score_base + arcana_skill,
    instability_trigger = "Natural 1 when used in combat",
    instability_damage = craft$instability,
    instability_radius_ft = 5,
    usage = "Bonus action: throw at target within 30ft",
    counter_dodge = "Target makes DEX save against player's DEX",
    counter_disrupt = "Target uses reaction to make Arcana check against rune Arcane Score",
    disrupt_near_success = "If disrupt roll is only 1-2 higher, rune becomes unstable and damages opponent"
  )
}