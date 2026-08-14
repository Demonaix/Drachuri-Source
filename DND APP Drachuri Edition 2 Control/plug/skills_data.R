# plug/skills_data.R

SKILLS_ABILITIES <- c("str", "dex", "con", "int", "bld_str", "cha")

SKILLS_ABILITY_NAMES <- c(
  str = "Strength",
  dex = "Dexterity",
  con = "Constitution",
  int = "Intelligence",
  bld_str = "Blood Strength",
  cha = "Charisma"
)

SKILLS_LIST <- data.frame(
  Skill = c(
    
    # =====================
    # Strength
    # =====================
    "Athletics", "Clutch", "Wrestling", "Throwing", "Dead Lift",
    
    # =====================
    # Dexterity
    # =====================
    "Acrobatics", "Sleight of Hand", "Stealth", "Precision",
    
    # =====================
    # Constitution
    # =====================
    "Endurance", "Tolerance", "Fortitude",
    
    # =====================
    # Intelligence
    # =====================
    "Arcana", "History", "Investigation", "Nature", "Religion", "Analysis",
    
    # =====================
    # Blood Strength
    # =====================
    "Perception", "Survival", "Insight", "Medicine", "Animal Handling", "Mandred Connection",
    
    # =====================
    # Charisma
    # =====================
    "Deception", "Intimidation", "Persuasion", "Performance", "Presence"
    
  ),
  
  Ability = c(
    
    # Strength
    "str", "str", "str", "str", "str",
    
    # Dexterity
    "dex", "dex", "dex", "dex",
    
    # Constitution
    "con", "con", "con",
    
    # Intelligence
    "int", "int", "int", "int", "int", "int",
    
    # Blood Strength
    "bld_str", "bld_str", "bld_str", "bld_str", "bld_str", "bld_str",
    
    # Charisma
    "cha", "cha", "cha", "cha", "cha"
    
  ),
  
  stringsAsFactors = FALSE
)

SKILL_DESC <- list(
  
  # Strength
  Athletics = "Climbing, jumping, swimming, general physical exertion.",
  Clutch = "Holding on, pulling others up, resisting falls or slips.",
  Wrestling = "Grappling, shoving, overpowering opponents.",
  Throwing = "Throwing objects or creatures with force or accuracy.",
  `Dead Lift` = "Lifting, carrying, and moving heavy weight.",
  
  # Dexterity
  Acrobatics = "Balance, flips, nimble movement, avoiding falls.",
  `Sleight of Hand` = "Pickpocketing, palming objects, subtle hand tricks.",
  Stealth = "Moving unseen and unheard.",
  Precision = "Fine control, delicate manipulation, careful aim.",
  
  # Constitution
  Endurance = "Sustained physical effort over time.",
  Tolerance = "Resisting poison, alcohol, pain, or harsh conditions.",
  Fortitude = "Raw physical resilience and toughness.",
  
  # Intelligence
  Arcana = "Knowledge of magic, spells, and arcane systems.",
  History = "Knowledge of past events, cultures, and lore.",
  Investigation = "Searching, deduction, and clue analysis.",
  Nature = "Understanding the natural world and creatures.",
  Religion = "Knowledge of gods, rituals, and divine systems.",
  Analysis = "Problem-solving, logic, and pattern recognition.",
  
  # Blood Strength
  Perception = "Noticing details through senses or instinct.",
  Survival = "Tracking, foraging, and navigating the wild.",
  Insight = "Reading intentions, emotions, and behaviour.",
  Medicine = "Stabilising, diagnosing, and treating wounds.",
  `Animal Handling` = "Calming, controlling, or understanding animals.",
  `Mandred Connection` = "Attunement to deeper magical or primal forces.",
  
  # Charisma
  Deception = "Lying, bluffing, and misleading others.",
  Intimidation = "Threatening or coercing through fear.",
  Persuasion = "Influencing others through reason or charm.",
  Performance = "Acting, storytelling, or entertaining.",
  Presence = "Commanding attention through sheer aura or authority."
)
