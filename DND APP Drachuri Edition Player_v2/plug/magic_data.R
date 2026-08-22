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
    Metal = list(time = "3d100", cost = "1d12 Sindre", instability = "3d20")
  ),
  Cursed = list(
    Metal = list(time = "3d100", cost = "1d12 HP", instability = "4d20")
  )
)

get_available_rune_types <- function(level) {
  names(Filter(function(req) level >= req, RUNE_LEVEL_UNLOCKS))
}

WARD_RULES <- list(
  Minor=list(time_divisor=10,cost="1d8 Sindre",min_size_ft=10,materials=c("Arcane Chalk","Iron Filings","Moonwater","Raven Feather")),
  Major=list(time_divisor=5,cost="2d8 Sindre",min_size_ft=15,materials=c("Sindre-Infused Obsidian","Heartwood Dust","Burned Ash","Vial of Willing Sorcerer's Blood","Enchanted Ink")),
  Arcane=list(time_divisor=1,cost="3d8 Sindre",min_size_ft=20,materials=c("Etherglass Shard","Meteorite Fragment","Elemental Scale","Banshee Tear","Silver Thread")),
  Cursed=list(time_divisor=1,cost="2d4 HP",min_size_ft=20,materials=c("Rot-Root","Betrayer's Bone Dust","Drowned Corpse Blood","Cursed Charcoal","Namebone"))
)

ENHANCEMENT_RULES <- list(
  Minor=list(crafting_hours=1,cost_die="1d4"),Major=list(crafting_hours=2,cost_die="2d4"),
  Arcane=list(crafting_hours=3,cost_die="3d4"),Cursed=list(crafting_hours=3,cost_die="4d4")
)

glyph_resource_cost <- function(cost_text) {
  parts<-strsplit(trimws(as.character(cost_text))," +")[[1L]]
  list(dice=parts[[1L]],resource=if(length(parts)>1L&&toupper(parts[[2L]])=="HP")"hp"else"sindre")
}

get_glyph_rule <- function(glyph_type,rank,arcana_skill=0,material=NULL,size_ft=NULL,enhancement_days=NULL) {
  glyph_type<-tolower(as.character(glyph_type));rank<-tools::toTitleCase(tolower(as.character(rank)));arcana_skill<-as.integer(arcana_skill%||%0L)
  if(glyph_type=="rune")return(get_rune_rule(rank,as.character(material),arcana_skill))
  if(glyph_type=="ward"){
    rule<-WARD_RULES[[rank]];if(is.null(rule))return(NULL);size<-max(as.numeric(size_ft%||%rule$min_size_ft),rule$min_size_ft)
    return(list(glyph_type="Ward",rank=rank,material=as.character(material),materials=rule$materials,crafting_hours=size/rule$time_divisor,cost=rule$cost,active_time="Indefinite until broken",arcane_score=as.integer(round(size+arcana_skill)),minimum_size_ft=rule$min_size_ft,size_ft=size,usage="Draw outside combat; remains active until broken.",counter="As a turn action within 30ft, make an Arcana check against its Arcane Score."))
  }
  if(glyph_type=="enhancement"){
    rule<-ENHANCEMENT_RULES[[rank]];if(is.null(rule))return(NULL);days<-max(1L,as.integer(enhancement_days%||%1L));cost<-paste(days,paste0(rule$cost_die," Sindre"),sep=" x ")
    return(list(glyph_type="Enhancement",rank=rank,material="None",crafting_hours=rule$crafting_hours,cost=cost,cost_die=rule$cost_die,cost_multiplier=days,active_time=paste(days,"day(s)"),active_days=days,arcane_score=30L+arcana_skill,usage="Enchant an owned item outside combat.",replenishment=paste0(days," x ",rule$cost_die," Sindre; 1 minute (10 rounds)."),counter="As a turn action while touching the item, make an Arcana check against its Arcane Score."))
  }
  NULL
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
