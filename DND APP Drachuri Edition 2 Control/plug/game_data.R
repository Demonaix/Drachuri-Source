# =========================
# Races
# =========================

RACES <- list(
  
  # =========================
  # NA'HARAN
  # =========================
  "Na'Haran" = list(
    
    desc = "People of the desert, divided between noble houses and wandering tribes. Their lives are shaped by harsh climates, ancient traditions, and survival.",
    
    core = list(
      size = "Medium",
      speed = 30,
      languages = c("Na'Haran", "Cymry", "Choice"),
      ability_bonus = list(str = 1)
    ),
    
    traits = list(
      desert_endurance = list(
        name = "Desert Endurance",
        desc = "Resistant to dehydration and extreme heat."
      ),
      nomads_and_nobles = list(
        name = "Nomads and Nobles",
        desc = "The House of the Nameless City rarely marries outside itself, while others roam as desert nomads."
      )
    ),
    
    subraces = list(
      "House of the Nameless City" = list(
        desc = "Elite descendants of the great house, trained as warriors and nobles.",
        ability_bonus = list(bld_str = 2),
        features = list(
          warrior_training = list(
            name = "Warrior Training",
            desc = "Proficient with longsword, longbow, daggers, and one additional weapon."
          ),
          noble_wealth = list(
            name = "Na'Haran Noble",
            desc = "Increase starting gold by 1.5x."
          )
        )
      ),
      
      "Desert Tribes" = list(
        desc = "Nomadic survivors of the desert, adaptable and cunning traders.",
        ability_bonus = list(con = 2, str = 1, cha = 1, bld_str = -2),
        features = list(
          adept_trader = list(
            name = "Adept Trader",
            desc = "Roll 1d4 to modify trade values by 10, 20, 40, or 50%."
          )
        )
      )
    )
  ),
  
  # =========================
  # TYLWYTH TEG
  # =========================
  "Tylwyth Teg" = list(
    
    desc = "Ancient fae folk of Annwn, scattered across forests, mountains, and waters. Once powerful, now fractured — with some turning to darker paths.",
    
    core = list(
      size = "Medium",
      speed = 30,
      languages = c("Old Cymry"),
      ability_bonus = list()
    ),
    
    traits = list(
      dogham_resilience = list(
        name = "Dogham Resilience",
        desc = "Resistant to cold."
      ),
      blood_tithe = list(
        name = "Blood Tithe",
        desc = "You may drink blood to regain Sindre and gain the Cythraul overlay."
      )
    ),
    
    subraces = list(
      "Ellylon" = list(
        desc = "Descendants of the Wild Hunt, echoing the lost spirits of Annwn.",
        ability_bonus = list(bld_str = 3),
        features = list(
          additional_languages = list(
            name = "Additional Languages",
            desc = "Gain Cymry and one additional language."
          ),
          warrior_training = list(
            name = "Warrior Training",
            desc = "Proficient with longsword, longbow, daggers, and one additional weapon."
          ),
          nobility = list(
            name = "Tylwyth Teg Nobility",
            desc = "Increase gold by 1.2x and gain one precious item and trinket."
          )
        )
      ),
      
      "Coblynau" = list(
        desc = "Mountain-dwelling folk of great strength and resilience.",
        ability_bonus = list(str = 3, con = 2, int = -2),
        features = list(
          hardened_warrior = list(
            name = "Hardened Warrior",
            desc = "Gain Relentless Endurance."
          ),
          weapon_training = list(
            name = "Weapon Training",
            desc = "Proficient with greataxes, battleaxes, and spears."
          ),
          trading_nation = list(
            name = "Trading Nation",
            desc = "Gold halved. Gain one precious item and trinket."
          )
        )
      ),
      
      "Bwbachod" = list(
        desc = "Forest folk tied to hidden places and quiet magic.",
        ability_bonus = list(dex = 2, cha = 1),
        features = list(
          small_folk = list(
            name = "Small Folk",
            desc = "Advantage on stealth in natural environments."
          ),
          hearth_magic = list(
            name = "Hearth Magic",
            desc = "Minor nature-based magical effects."
          )
        )
      ),
      
      "Gwragedd" = list(
        desc = "Fae of lakes and rivers, remnants of a drowned civilisation.",
        ability_bonus = list(cha = 2, con = 1),
        features = list(
          water_affinity = list(
            name = "Water Affinity",
            desc = "Can breathe underwater."
          ),
          sirens_voice = list(
            name = "Siren’s Voice",
            desc = "Advantage on persuasion checks."
          )
        )
      )
    ),
    
    overlays = list(
      "Cythraul" = list(
        desc = "Corrupted Tylwyth Teg who regain magic through blood.",
        type = "additive",
        features = list(
          blood_tithe = list(
            name = "Blood Tithe",
            desc = "Gain Sindre from the blood of others."
          ),
          old_magic = list(
            name = "Old Magic",
            desc = "Access Hanianol Sorcerer class."
          )
        )
      )
    )
  ),
  
  # =========================
  # ISILDUR
  # =========================
  "Isildur" = list(
    
    desc = "Survivors of Cantre'r Gwaelod, a fallen city cursed by time. They endure through trade, crime, and resilience.",
    
    core = list(
      size = "Medium",
      speed = 30,
      languages = c("Isildur", "Cymry"),
      ability_bonus = list(con = 1)
    ),
    
    traits = list(
      dirty_water = list(
        name = "Dirty Water",
        desc = "Resistant to alcohol-based poisoning."
      )
    ),
    
    subraces = list(
      "Vagabond" = list(
        desc = "Street-raised survivors and sailors.",
        ability_bonus = list(dex = 2, cha = 1),
        features = list(
          proficiencies = list(
            name = "Street Proficiencies",
            desc = "Daggers, deception, gambling."
          ),
          dirt_poor = list(
            name = "Dirt Poor",
            desc = "Gold reduced to 0.2x."
          )
        )
      ),
      
      "Brotherhood Priest" = list(
        desc = "Scholars and healers devoted to Arawn.",
        ability_bonus = list(int = 2),
        features = list(
          medical_knowledge = list(
            name = "Medical Knowledge",
            desc = "Healing is 4x faster."
          ),
          llosgi = list(
            name = "The Llosgi",
            desc = "Sense lies through a burning sensation."
          )
        )
      )
    )
  ),
  
  # =========================
  # RHODRIAN
  # =========================
  "Rhodrian" = list(
    
    desc = "A fractured land of former kingdoms, where lineage and status define one's place in society.",
    
    core = list(
      size = "Medium",
      speed = 30,
      languages = c("Cymry"),
      ability_bonus = list()
    ),
    
    traits = list(
      fractured_kingdoms = list(
        name = "Fractured Kingdoms",
        desc = "A heritage shaped by division and diversity."
      )
    ),
    
    subraces = list(
      "Commoner" = list(
        desc = "Adaptable workers shaped by hardship.",
        ability_bonus = list(free_points = 5, bld_str = -2),
        features = list(
          labourer = list(
            name = "Labourer",
            desc = "Proficient in a trade of your choice."
          ),
          low_born = list(
            name = "Low Born",
            desc = "Gold reduced by 30%."
          )
        )
      ),
      
      "Noble" = list(
        desc = "Descendants of powerful houses.",
        ability_bonus = list(bld_str = 2, cha = 1),
        features = list(
          prestigious_education = list(
            name = "Prestigious Education",
            desc = "Proficient in Arcana, History, and one skill."
          ),
          family_money = list(
            name = "Family Money",
            desc = "Gold increased by 1.5x."
          )
        )
      )
    )
  )
)

CLASSES <- list(
  
  "Rogue" = list(
    
    desc = "A master of stealth, deception, and precision strikes. Rogues excel at exploiting weaknesses and avoiding danger.",
    
    hit_die = 8,
    
    primary_abilities = c("Dexterity"),
    
    proficiencies = list(
      armor = c("Light"),
      weapons = c("Simple Weapons", "Hand Crossbows", "Longswords", "Rapiers", "Shortswords"),
      tools = c("Thieves' Tools"),
      saving_throws = c("Dexterity", "Intelligence"),
      skills = "Choose 4 from Acrobatics, Athletics, Deception, Insight, Intimidation, Investigation, Perception, Performance, Persuasion, Sleight of Hand, Stealth"
    ),
    
    # -------------------------
    # LEVEL PROGRESSION
    # -------------------------
    levels = list(
      
      "1" = list(
        features = list(
          
          sneak_attack = list(
            name = "Sneak Attack",
            desc = "Once per turn, deal extra damage when you have advantage or an ally is adjacent to the target.",
            scaling = "1d6"
          ),
          
          thieves_cant = list(
            name = "Thieves' Cant",
            desc = "A secret mix of dialect, jargon, and code used by rogues."
          )
          
        )
      ),
      
      "2" = list(
        features = list(
          
          cunning_action = list(
            name = "Cunning Action",
            desc = "You can Dash, Disengage, or Hide as a bonus action."
          )
          
        )
      ),
      
      "3" = list(
        features = list(
          
          subclass_unlock = list(
            name = "Roguish Archetype",
            desc = "Choose a subclass that defines your style of roguery."
          )
          
        )
      ),
      
      "4" = list(
        features = list(
          asi = list(
            name = "Ability Score Improvement",
            desc = "Increase ability scores or take a feat."
          )
        )
      ),
      
      "5" = list(
        features = list(
          
          uncanny_dodge = list(
            name = "Uncanny Dodge",
            desc = "Use your reaction to halve damage from an attacker you can see."
          )
          
        )
      ),
      
      "6" = list(
        features = list(
          
          expertise = list(
            name = "Expertise",
            desc = "Double proficiency bonus for chosen skills."
          )
          
        )
      ),
      
      "7" = list(
        features = list(
          
          evasion = list(
            name = "Evasion",
            desc = "Take no damage on successful Dex saves, half on failure."
          )
          
        )
      ),
      
      "8" = list(
        features = list(
          asi = list(
            name = "Ability Score Improvement",
            desc = "Increase ability scores or take a feat."
          )
        )
      ),
      
      "9" = list(
        features = list() # subclass heavy level
      ),
      
      "10" = list(
        features = list(
          asi = list(
            name = "Ability Score Improvement",
            desc = "Increase ability scores or take a feat."
          )
        )
      )
    ),
    
    # -------------------------
    # SCALING FEATURES
    # -------------------------
    scaling = list(
      
      sneak_attack = list(
        "1" = "1d6",
        "3" = "2d6",
        "5" = "3d6",
        "7" = "4d6",
        "9" = "5d6",
        "11" = "6d6",
        "13" = "7d6",
        "15" = "8d6",
        "17" = "9d6",
        "19" = "10d6"
      )
      
    ),
    
    # -------------------------
    # SUBCLASSES
    # -------------------------
    subclasses = list(
      
      # =====================
      # THIEF
      # =====================
      "Thief" = list(
        
        desc = "Quick, nimble, and skilled in infiltration. Masters of climbing and sleight of hand.",
        
        levels = list(
          
          "3" = list(
            features = list(
              
              fast_hands = list(
                name = "Fast Hands",
                desc = "Use Cunning Action to make Sleight of Hand checks or use objects."
              ),
              
              second_story_work = list(
                name = "Second-Story Work",
                desc = "Climbing no longer costs extra movement. Jump distance increased."
              )
              
            )
          ),
          
          "9" = list(
            features = list(
              
              supreme_sneak = list(
                name = "Supreme Sneak",
                desc = "Advantage on Stealth if you move slowly."
              )
              
            )
          )
          
        )
      ),
      
      # =====================
      # ASSASSIN
      # =====================
      "Assassin" = list(
        
        desc = "Deadly killers trained in infiltration and eliminating targets before they react.",
        
        levels = list(
          
          "3" = list(
            features = list(
              
              assassinate = list(
                name = "Assassinate",
                desc = "Advantage on creatures that haven’t acted. Crit on surprised targets."
              ),
              
              bonus_proficiencies = list(
                name = "Bonus Proficiencies",
                desc = "Gain proficiency with disguise kit and poisoner’s kit."
              )
              
            )
          ),
          
          "9" = list(
            features = list(
              
              infiltration_expertise = list(
                name = "Infiltration Expertise",
                desc = "Create false identities and backgrounds."
              )
              
            )
          )
          
        )
      )
      
    )
    
  ),
"Fighter" = list(
    
    desc = "A master of martial combat, skilled with a variety of weapons and armour. Fighters rely on discipline, endurance, and combat prowess.",
    
    hit_die = 10,
    
    primary_abilities = c("Strength", "Dexterity"),
    
    proficiencies = list(
      armor = c("Light", "Medium", "Heavy", "Shields"),
      weapons = c("Simple Weapons", "Martial Weapons"),
      tools = c(),
      saving_throws = c("Strength", "Constitution"),
      skills = "Choose 2 from Acrobatics, Animal Handling, Athletics, History, Insight, Intimidation, Perception, Survival"
    ),
    
    # -------------------------
    # LEVEL PROGRESSION
    # -------------------------
    levels = list(
      
      "1" = list(
        features = list(
          
          fighting_style = list(
            name = "Fighting Style",
            desc = "Adopt a fighting style (e.g. Archery, Defence, Dueling, Great Weapon Fighting)."
          ),
          
          second_wind = list(
            name = "Second Wind",
            desc = "Once per rest, regain HP equal to 1d10 + your Fighter level."
          )
          
        )
      ),
      
      "2" = list(
        features = list(
          
          action_surge = list(
            name = "Action Surge",
            desc = "Take one additional action on your turn (once per rest)."
          )
          
        )
      ),
      
      "3" = list(
        features = list(
          
          subclass_unlock = list(
            name = "Martial Archetype",
            desc = "Choose a Fighter archetype that shapes your combat style."
          )
          
        )
      ),
      
      "4" = list(
        features = list(
          asi = list(
            name = "Ability Score Improvement",
            desc = "Increase ability scores or take a feat."
          )
        )
      ),
      
      "5" = list(
        features = list(
          
          extra_attack = list(
            name = "Extra Attack",
            desc = "You can attack twice instead of once when you take the Attack action."
          )
          
        )
      ),
      
      "6" = list(
        features = list(
          asi = list(
            name = "Ability Score Improvement",
            desc = "Increase ability scores or take a feat."
          )
        )
      ),
      
      "7" = list(features = list()),
      
      "8" = list(
        features = list(
          asi = list(
            name = "Ability Score Improvement",
            desc = "Increase ability scores or take a feat."
          )
        )
      ),
      
      "9" = list(
        features = list(
          
          indomitable = list(
            name = "Indomitable",
            desc = "Reroll a failed saving throw once per rest."
          )
          
        )
      ),
      
      "10" = list(
        features = list(
          asi = list(
            name = "Ability Score Improvement",
            desc = "Increase ability scores or take a feat."
          )
        )
      )
    ),
    
    # -------------------------
    # SCALING
    # -------------------------
    scaling = list(
      
      extra_attack = list(
        "5" = "2 attacks",
        "11" = "3 attacks",
        "20" = "4 attacks"
      ),
      
      indomitable = list(
        "9" = "1 use",
        "13" = "2 uses",
        "17" = "3 uses"
      )
      
    ),
    
    # -------------------------
    # SUBCLASSES
    # -------------------------
    subclasses = list(
      
      # =====================
      # CHAMPION
      # =====================
      "Champion" = list(
        
        desc = "A straightforward but deadly warrior who relies on physical excellence and critical strikes.",
        
        levels = list(
          
          "3" = list(
            features = list(
              
              improved_critical = list(
                name = "Improved Critical",
                desc = "Your weapon attacks score a critical hit on a roll of 19–20."
              )
              
            )
          ),
          
          "7" = list(
            features = list(
              
              remarkable_athlete = list(
                name = "Remarkable Athlete",
                desc = "Add half proficiency to physical checks not already using proficiency."
              )
              
            )
          )
          
        )
      ),
      
      # =====================
      # BATTLE MASTER
      # =====================
      "Battle Master" = list(
        
        desc = "A tactical warrior who uses combat superiority dice to perform manoeuvres in battle.",
        
        levels = list(
          
          "3" = list(
            features = list(
              
              combat_superiority = list(
                name = "Combat Superiority",
                desc = "Gain superiority dice to fuel special manoeuvres."
              ),
              
              student_of_war = list(
                name = "Student of War",
                desc = "Gain proficiency with artisan's tools of your choice."
              )
              
            )
          ),
          
          "7" = list(
            features = list(
              
              know_your_enemy = list(
                name = "Know Your Enemy",
                desc = "Study a creature to learn information about its capabilities."
              )
              
            )
          )
          
        )
      )
      
    )
  ),
"Barbarian" = list(
  
  desc = "A fierce warrior of primal rage, drawing on raw strength and instinct to overwhelm enemies.",
  
  hit_die = 12,
  
  primary_abilities = c("Strength", "Constitution"),
  
  proficiencies = list(
    armor = c("Light", "Medium", "Shields"),
    weapons = c("Simple Weapons", "Martial Weapons"),
    tools = c(),
    saving_throws = c("Strength", "Constitution"),
    skills = "Choose 2 from Animal Handling, Athletics, Intimidation, Nature, Perception, Survival"
  ),
  
  # -------------------------
  # LEVEL PROGRESSION
  # -------------------------
  levels = list(
    
    "1" = list(
      features = list(
        
        rage = list(
          name = "Rage",
          desc = "Enter a rage to gain bonus damage, resistance to physical damage, and advantage on Strength checks and saves."
        ),
        
        unarmoured_defence = list(
          name = "Unarmoured Defence",
          desc = "While not wearing armour, AC = 10 + Dex + Con."
        )
        
      )
    ),
    
    "2" = list(
      features = list(
        
        reckless_attack = list(
          name = "Reckless Attack",
          desc = "Gain advantage on melee attacks using Strength, but attacks against you have advantage."
        ),
        
        danger_sense = list(
          name = "Danger Sense",
          desc = "Advantage on Dex saves against effects you can see."
        )
        
      )
    ),
    
    "3" = list(
      features = list(
        
        subclass_unlock = list(
          name = "Primal Path",
          desc = "Choose a path that shapes your rage."
        )
        
      )
    ),
    
    "4" = list(
      features = list(
        asi = list(
          name = "Ability Score Improvement",
          desc = "Increase ability scores or take a feat."
        )
      )
    ),
    
    "5" = list(
      features = list(
        
        extra_attack = list(
          name = "Extra Attack",
          desc = "Attack twice when taking the Attack action."
        ),
        
        fast_movement = list(
          name = "Fast Movement",
          desc = "Your speed increases by 10 ft while not wearing heavy armour."
        )
        
      )
    ),
    
    "6" = list(features = list()),
    
    "7" = list(
      features = list(
        
        feral_instinct = list(
          name = "Feral Instinct",
          desc = "Advantage on initiative rolls."
        )
        
      )
    ),
    
    "8" = list(
      features = list(
        asi = list(
          name = "Ability Score Improvement",
          desc = "Increase ability scores or take a feat."
        )
      )
    ),
    
    "9" = list(
      features = list(
        
        brutal_critical = list(
          name = "Brutal Critical",
          desc = "Roll one additional weapon damage die when scoring a critical hit."
        )
        
      )
    ),
    
    "10" = list(features = list())
  ),
  
  # -------------------------
  # SCALING
  # -------------------------
  scaling = list(
    
    rage_damage = list(
      "1" = "+2",
      "9" = "+3",
      "16" = "+4"
    )
    
  ),
  
  # -------------------------
  # SUBCLASSES
  # -------------------------
  subclasses = list(
    
    # =====================
    # BERSERKER
    # =====================
    "Berserker" = list(
      
      desc = "A warrior who embraces fury to become a whirlwind of destruction.",
      
      levels = list(
        
        "3" = list(
          features = list(
            
            frenzy = list(
              name = "Frenzy",
              desc = "While raging, you can make a bonus action attack each turn."
            )
            
          )
        ),
        
        "6" = list(
          features = list(
            
            mindless_rage = list(
              name = "Mindless Rage",
              desc = "You cannot be charmed or frightened while raging."
            )
            
          )
        )
        
      )
    ),
    
    # =====================
    # TOTEM WARRIOR
    # =====================
    "Totem Warrior" = list(
      
      desc = "A spiritual warrior who draws power from animal spirits.",
      
      levels = list(
        
        "3" = list(
          features = list(
            
            spirit_totem = list(
              name = "Spirit Totem",
              desc = "Choose an animal spirit (e.g. Bear, Wolf, Eagle) granting unique bonuses."
            )
            
          )
        ),
        
        "6" = list(
          features = list(
            
            aspect_of_the_beast = list(
              name = "Aspect of the Beast",
              desc = "Gain additional traits based on your chosen totem."
            )
            
          )
        )
        
      )
    )
    
  )
),
"Hanianol Sorcerer" = list(
  
  desc = "A blood-bound sorcerer drawing power from nature and the Mandred. Unable to regenerate magic naturally, they must consume blood to fuel their spells.",
  
  hit_die = 8,
  
  primary_abilities = c("Blood Strength", "Intelligence"),
  
  proficiencies = list(
    armor = c("Light"),
    weapons = c("Shortbow", "Dagger", "Longsword", "One of choice"),
    tools = c(),
    saving_throws = c("Blood Strength", "Intelligence"),
    skills = "Choose from Survival, Animal Handling, Arcana, Intimidation, Nature"
  ),
  
  equipment = c(
    "Shortbow",
    "Throwing daggers",
    "Longsword",
    "Explorer’s pack",
    "Quiver of arrows"
  ),
  
  # -------------------------
  # CORE FEATURES
  # -------------------------
  levels = list(
    
    "1" = list(
      features = list(
        
        blood_magic = list(
          name = "Blood Magic",
          desc = "You cannot regenerate Sindre naturally and must drink blood to restore it."
        ),
        
        fae_blooded = list(
          name = "Fae Blooded",
          desc = "Gain access to natural magic and the Bloodthirsty action. Double proficiency in Survival."
        ),
        
        bloodthirsty = list(
          name = "Bloodthirsty Action",
          desc = "Bite attack: 1d8 + Strength modifier. Counts as drinking half a pint of blood."
        )
      )
    ),
    
    "2" = list(
      features = list(
        
        natural_magic = list(
          name = "Natural Magic",
          desc = "Identify plants and gain knowledge for potion-making (gain Medicine). Choose a specialty: Plants, Rain, Animals, or Disease. Gain associated Deep Magic spell."
        )
      )
    ),
    
    "3" = list(
      features = list(
        subclass_unlock = list(
          name = "Sorcerous Path",
          desc = "Choose between Path of the Ancestor or Heart Eater."
        )
      )
    ),
    
    "4" = list(
      features = list(
        asi = list(name = "Ability Score Improvement", desc = "Increase ability scores.")
      )
    ),
    
    "5" = list(
      features = list(
        thermal_wild_magic = list(
          name = "Thermal Wild Magic",
          desc = "Manipulate exothermic or endothermic energy."
        )
      )
    ),
    
    "7" = list(
      features = list(
        mandred_manipulator = list(
          name = "Mandred Manipulator",
          desc = "Reroll failed wild magic once per short rest or after drinking blood."
        )
      )
    ),
    
    "9" = list(
      features = list(
        divergent = list(
          name = "Divergent",
          desc = "Advantage on saving throws against spells. Reaction: gain resistance to one spell type."
        )
      )
    ),
    
    "11" = list(
      features = list(
        deep_magic_adept = list(
          name = "Deep Magic Adept",
          desc = "Your summoner spells are upgraded."
        )
      )
    ),
    
    "13" = list(
      features = list(
        flesh_of_magic = list(
          name = "Flesh of Magic",
          desc = "Your bound magic is strengthened."
        )
      )
    ),
    
    "15" = list(
      features = list(
        expert_summoner = list(
          name = "Expert Summoner",
          desc = "Further upgrades to summoner spells."
        )
      )
    ),
    
    "17" = list(
      features = list(
        master_sorcerer = list(
          name = "Master Sorcerer",
          desc = "All spells count as practiced. Bound magic strengthened."
        )
      )
    ),
    
    "20" = list(
      features = list(
        ascended = list(
          name = "Ascended Sorcerer",
          desc = "Ultimate mastery of blood and deep magic."
        )
      )
    )
  ),
  
  # -------------------------
  # SPECIAL SYSTEMS
  # -------------------------
  systems = list(
    
    deep_magic = list(
      
      plant = "Create grasping vines in 20ft area (difficult terrain). Cost: 20 Sindre.",
      
      rain = "Summon storm. Fire -4, Lightning/Cold +2. Lasts 1 hour. Cost: 20 Sindre.",
      
      animal = "Summon beasts (CR scaling). Friendly. Lasts 30 mins. Cost: 20 Sindre.",
      
      disease = "Enemies in 60ft make CON save or become poisoned. Cost: 20 Sindre."
    ),
    
    matter_manipulation = list(
      plant = "Create plant life",
      rain = "Create rain, clouds, ice",
      animal = "Create natural matter (stone, soil)",
      disease = "Destroy matter"
    )
  ),
  
  # -------------------------
  # SUBCLASSES
  # -------------------------
  subclasses = list(
    
    # =====================
    # PATH OF THE ANCESTOR
    # =====================
    "Path of the Ancestor" = list(
      
      desc = "Walk the path of the Mandred, drawing power from ancestral convergence points.",
      
      levels = list(
        
        "3" = list(
          features = list(
            
            seer = list(
              name = "Seer",
              desc = "Detect Mandred convergence points. Spells cost half Sindre there and gain advantage."
            )
          )
        ),
        
        "6" = list(
          features = list(
            
            balance = list(
              name = "Balance",
              desc = "Convert Sindre to boost 3 ability scores by +1 until rest."
            )
          )
        ),
        
        "10" = list(
          features = list(
            
            immersed = list(
              name = "Immersed",
              desc = "Gain Conjure Elemental deep magic."
            )
          )
        ),
        
        "14" = list(
          features = list(
            
            gwynns_horde = list(
              name = "Gwynn’s Horde",
              desc = "Gain a spectral mount bound to your life force."
            )
          )
        ),
        
        "20" = list(
          features = list(
            
            one_with_annwn = list(
              name = "One with Annwn",
              desc = "No longer suffer blood addiction penalties. Can create temporary convergence points."
            )
          )
        )
      )
    ),
    
    # =====================
    # HEART EATER
    # =====================
    "Heart Eater" = list(
      
      desc = "A terrifying predator who feeds on blood and hearts to gain immense power.",
      
      levels = list(
        
        "3" = list(
          features = list(
            
            exquisite_taste = list(
              name = "Exquisite Taste",
              desc = "Drinking blood restores HP. Eating hearts fully restores Sindre and grants the heart's value as temporary Sindre."
            ),
            
            shadow_step = list(
              name = "Shadow Step",
              desc = "Teleport through shadows within movement range."
            )
          )
        ),
        
        "6" = list(
          features = list(
            
            predator = list(
              name = "Predator",
              desc = "Frighten nearby enemies based on Blood Strength."
            )
          )
        ),
        
        "10" = list(
          features = list(
            
            improved_shadow_step = list(
              name = "Improved Shadow Step",
              desc = "Double distance and enhanced mobility."
            )
          )
        ),
        
        "14" = list(
          features = list(
            
            abyss_caller = list(
              name = "Abyss Caller",
              desc = "Explode Sindre on death to damage enemies. Lose magic permanently unless restored."
            )
          )
        ),
        
        "18" = list(
          features = list(
            
            realm_walker = list(
              name = "Realm Walker",
              desc = "Shadow step becomes reaction-based and more powerful."
            )
          )
        ),
        
        "20" = list(
          features = list(
            
            walking_with_gods = list(
              name = "Walking with Gods",
              desc = "Heart consumption permanently increases max Sindre."
            )
          )
        )
      )
    )
    
  )
),
"Na'Haran Sorcerer" = list(
  
  desc = "Honoured members of the House of the Nameless City. Their magic is volatile, powerful, and feared by other houses.",
  
  hit_die = 10,
  
  primary_abilities = c("Blood Strength", "Charisma"),
  
  proficiencies = list(
    armor = c("Medium"),
    weapons = c("Quarterstaff", "Spear", "Longbow", "One of choice"),
    tools = c(),
    saving_throws = c("Blood Strength", "Charisma"),
    skills = "Choose from Arcana, Deception, Insight, Intimidation, Persuasion, Religion"
  ),
  
  equipment = c(
    "Spear",
    "Quarterstaff",
    "Two daggers",
    "Longbow",
    "Explorer’s pack",
    "Quiver of arrows"
  ),
  
  # -------------------------
  # LEVEL PROGRESSION
  # -------------------------
  levels = list(
    
    "1" = list(
      features = list(
        
        desert_wild_magic = list(
          name = "Desert Wild Magic",
          desc = "You wield unstable desert magic. Mechanical wild magic only. Rocks and minerals have reduced DC requirements."
        ),
        
        survival_mastery = list(
          name = "Survival Mastery",
          desc = "Gain double proficiency in Survival."
        ),
        
        water_channeler = list(
          name = "Water Channeler",
          desc = "Drain water from creatures or objects via touch. Costs 10 Sindre. Deals 10% max HP damage or extracts 1000ml water."
        )
      )
    ),
    
    "2" = list(
      features = list(
        
        mind_bender = list(
          name = "Mind Bender",
          desc = "Gain advantage on a persuasion check. Cost: 10 Sindre."
        ),
        
        detect_undead = list(
          name = "Detect Undead",
          desc = "Sense undead within 60ft. Cost: 10 Sindre."
        )
      )
    ),
    
    "3" = list(
      features = list(
        subclass_unlock = list(
          name = "Sorcerous Path",
          desc = "Choose Path of the Warrior or Path of the Prophet."
        )
      )
    ),
    
    "5" = list(
      features = list(
        
        adept_sorcerer = list(
          name = "Adept Sorcerer",
          desc = "Choose Thermal Wild Magic: Exothermic or Endothermic."
        )
      )
    ),
    
    "6" = list(
      features = list(
        
        electromagnetic = list(
          name = "Electromagnetic Magic",
          desc = "Choose Flesh Witherer (necrotic) or Lightbringer (radiant)."
        )
      )
    ),
    
    "7" = list(
      features = list(
        
        controlled_wild_magic = list(
          name = "Controlled Wild Magic",
          desc = "Failed spells only consume Sindre equal to spell cost, not max flow."
        )
      )
    ),
    
    "9" = list(
      features = list(
        
        shield_of_sindre = list(
          name = "Shield of Sindre",
          desc = "Advantage on saving throws vs spells. Reaction: gain resistance to a spell."
        )
      )
    ),
    
    "10" = list(
      features = list(
        
        predict_spell = list(
          name = "Predict Spell",
          desc = "Roll to anticipate enemy actions. Success imposes disadvantage on their next move."
        )
      )
    ),
    
    "11" = list(
      features = list(
        
        improved_channeling = list(
          name = "Improved Water Channeler",
          desc = "Now drains 20% max HP or 2000ml water for same cost."
        )
      )
    ),
    
    "15" = list(
      features = list(
        
        death_scourge = list(
          name = "Death Scourge",
          desc = "Deal +1d8 radiant damage to undead."
        )
      )
    ),
    
    "17" = list(
      features = list(
        
        honoured = list(
          name = "Honoured",
          desc = "Gain enchanted armour from the House of the Nameless City."
        )
      )
    ),
    
    "20" = list(
      features = list(
        
        star_walker = list(
          name = "Star Walker",
          desc = "Always predict enemy actions. Gain advantage on all attacks. Damage dice explode."
        )
      )
    )
  ),
  
  # -------------------------
  # SUBCLASSES
  # -------------------------
  subclasses = list(
    
    # =====================
    # PATH OF THE WARRIOR
    # =====================
    "Path of the Warrior" = list(
      
      desc = "A fusion of martial prowess and destructive magic.",
      
      levels = list(
        
        "3" = list(
          features = list(
            
            spellsword = list(
              name = "Spellsword",
              desc = "Reduce enemy resistances or immunities when combat begins."
            )
          )
        ),
        
        "6" = list(
          features = list(
            
            combat_magic = list(
              name = "Combat Magic",
              desc = "Choose Flesh Witherer’s Hand (necrotic aura) or Lightbringer’s Sword (blinding radiant attack)."
            )
          )
        ),
        
        "14" = list(
          features = list(
            
            way_of_light = list(
              name = "Way of Light",
              desc = "Gain Evasion and Extra Attack."
            )
          )
        ),
        
        "18" = list(
          features = list(
            
            warrior_trance = list(
              name = "Warrior of the Nameless City",
              desc = "Enter a trance: enemies have disadvantage, damage halved, AC +2."
            )
          )
        )
      )
    ),
    
    # =====================
    # PATH OF THE PROPHET
    # =====================
    "Path of the Prophet" = list(
      
      desc = "A seer guided by ancestral whispers and visions of the Mandred.",
      
      levels = list(
        
        "3" = list(
          features = list(
            
            wild_insight = list(
              name = "Wild Insight",
              desc = "Roll on wild magic table as a bonus action."
            )
          )
        ),
        
        "6" = list(
          features = list(
            
            divination = list(
              name = "Divination",
              desc = "Gain insight into past and future under the stars."
            ),
            
            mislead = list(
              name = "Mislead",
              desc = "Create illusionary double and become invisible."
            )
          )
        ),
        
        "14" = list(
          features = list(
            
            enhanced_wild = list(
              name = "Enhanced Wild Magic",
              desc = "Reroll wild magic and recover faster."
            ),
            
            star_meditation = list(
              name = "Star Meditation",
              desc = "Triple Sindre recovery when resting under stars."
            )
          )
        ),
        
        "18" = list(
          features = list(
            
            true_projection = list(
              name = "True Projection",
              desc = "No longer blind/deaf when using illusion. Can create multiple doubles."
            )
          )
        )
      )
    )
  )
)
)


LEVEL_OPTIONS <- list(
  "Rogue" = list(
    "3" = list(
      list(
        id = "subclass",
        label = "Choose Archetype",
        options = names(CLASSES[["Rogue"]]$subclasses)
      )
    )
  ),
"Fighter" = list(
    "3" = list(
      list(
        id = "subclass",
        label = "Choose Martial Archetype",
        options = names(CLASSES[["Fighter"]]$subclasses)
      )
    )
  ),
"Barbarian" = list(
  "3" = list(
    list(
      id = "subclass",
      label = "Choose Primal Path",
      options = names(CLASSES[["Barbarian"]]$subclasses)
    )
  )
),
"Hanianol Sorcerer" = list(
  "3" = list(
    list(
      id = "subclass",
      label = "Choose Sorcerous Path",
      options = names(CLASSES[["Hanianol Sorcerer"]]$subclasses)
    )
  )
),
"Na'Haran Sorcerer" = list(
  "3" = list(
    list(
      id = "subclass",
      label = "Choose Sorcerous Path",
      options = names(CLASSES[["Na'Haran Sorcerer"]]$subclasses)
    )
  )
)
)
