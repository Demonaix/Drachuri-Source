CLASS_FEATURE_TAGS <- c(
  "stat_increase", "ability", "combat", "spell", "passive",
  "reaction", "movement", "healing", "utility", "subclass"
)

CLASS_FEATURE_INTEGRATION <- list(
  "Rogue::1::sneak_attack" = list(
    status = "partial", note = "Damage and level scaling work in combat and use is limited to once per turn; advantage/ally eligibility remains player-confirmed."
  ),
  "Rogue::1::thieves_cant" = list(
    status = "manual", note = "A narrative language feature; shown on the sheet and adjudicated through roleplay."
  ),
  "Fighter::1::fighting_style" = list(
    status = "partial", note = "The choice is saved; Defence applies +1 AC in armour. Weapon classification is still needed to automate the other three styles safely."
  ),
  "Fighter::1::second_wind" = list(
    status = "working", note = "Available in combat as a self-heal and automatically recharges on a short or long rest."
  ),
  "Barbarian::1::rage" = list(
    status = "working", note = "Rage has limited long-rest uses, costs a bonus action, adds scaling Strength weapon damage and applies physical resistance."
  ),
  "Barbarian::1::unarmoured_defence" = list(
    status = "working", note = "While no armour is equipped, AC automatically uses 10 + Dexterity + Constitution modifiers."
  ),
  "Hanianol Sorcerer::1::blood_magic" = list(
    status = "working", note = "Natural Sindre recovery is disabled for Hanianol characters."
  ),
  "Hanianol Sorcerer::1::fae_blooded" = list(
    status = "working", note = "Survival Expertise is derived automatically and Bloodthirsty is unlocked."
  ),
  "Hanianol Sorcerer::1::bloodthirsty" = list(
    status = "partial", note = "The bite attack works in combat; recording blood intake and Sindre restoration remains manual."
  ),
  "Na'Haran Sorcerer::1::desert_wild_magic" = list(
    status = "partial", note = "The wild-magic casting system works; the reduced rock and mineral difficulty is not yet automatic."
  ),
  "Na'Haran Sorcerer::1::survival_mastery" = list(
    status = "working", note = "Survival Expertise is derived automatically."
  ),
  "Na'Haran Sorcerer::1::water_channeler" = list(
    status = "working", note = "Combat targeting, Sindre cost and dice-based damage are automated."
  ),
  "Rogue::2::cunning_action" = list(
    status = "working", note = "Dash, Disengage and Hide use the character's bonus action and reset at the start of each turn."
  ),
  "Fighter::2::action_surge" = list(
    status = "working", note = "Grants one additional action and recharges on a short or long rest."
  ),
  "Barbarian::2::reckless_attack" = list(
    status = "working", note = "Can be enabled before attacking; Strength attacks gain advantage and attacks against the Barbarian gain advantage until their next turn."
  ),
  "Barbarian::2::danger_sense" = list(
    status = "working", note = "Dexterity saves default to advantage; use the explicit normal/disadvantage controls when the effect is unseen or another rule overrides it."
  ),
  "Hanianol Sorcerer::2::natural_magic" = list(
    status = "working", note = "Medicine, speciality selection and synchronized combat casting work across all four paths, including level-11 and level-15 upgrades."
  ),
  "Na'Haran Sorcerer::2::mind_bender" = list(
    status = "working", note = "Can be activated from Skills for 10 Sindre and is consumed by the next Persuasion check."
  ),
  "Na'Haran Sorcerer::2::detect_undead" = list(
    status = "partial", note = "Usable in combat for 10 Sindre and reports recognised undead; explicit enemy creature-type data is still needed for perfect detection."
  ),
  "Rogue::3::subclass_unlock" = list(status = "working", note = "Subclass choice is required, saved and restored."),
  "Fighter::3::subclass_unlock" = list(status = "working", note = "Subclass choice is required, saved and restored."),
  "Barbarian::3::subclass_unlock" = list(status = "working", note = "Subclass choice is required, saved and restored."),
  "Hanianol Sorcerer::3::subclass_unlock" = list(status = "working", note = "Subclass choice is required, saved and restored."),
  "Na'Haran Sorcerer::3::subclass_unlock" = list(status = "working", note = "Subclass choice is required, saved and restored."),
  "Rogue::3::fast_hands" = list(status = "working", note = "A combat entry point spends the bonus action for object use, Sleight of Hand, locks or simple traps."),
  "Rogue::3::second_story_work" = list(status = "working", note = "The map movement toggle permits climbing or vaulting blocked tiles while still charging movement."),
  "Rogue::3::assassinate" = list(status = "working", note = "First-round advantage applies before a target acts; attacks that hit a target carrying the surprised condition are critical hits."),
  "Rogue::3::bonus_proficiencies" = list(status = "working", note = "Disguise kit and poisoner's kit proficiency are derived automatically."),
  "Fighter::3::improved_critical" = list(status = "working", note = "Weapon attacks now score critical hits on natural 19 or 20."),
  "Fighter::3::combat_superiority" = list(status = "working", note = "Choose three manoeuvres; four d8 superiority dice recharge on a short or long rest and apply to attacks."),
  "Fighter::3::student_of_war" = list(status = "working", note = "The saved artisan's tool choice is granted automatically."),
  "Barbarian::3::frenzy" = list(status = "working", note = "While raging, a bonus action grants an additional attack action for the turn."),
  "Barbarian::3::spirit_totem" = list(status = "working", note = "Bear expands Rage resistance, Wolf grants Strength attack advantage, and Eagle grants bonus-action Dash."),
  "Hanianol Sorcerer::3::seer" = list(status = "working", note = "Control can paint Mandred convergence terrain; Natural Magic costs half Sindre there and enemy saves roll with disadvantage."),
  "Hanianol Sorcerer::3::exquisite_taste" = list(status = "working", note = "Blood restores HP; hearts fully restore Sindre and grant their listed value as temporary Sindre."),
  "Hanianol Sorcerer::3::shadow_step" = list(status = "working", note = "Heart Eaters can phase through blocked map tiles within their normal movement range."),
  "Na'Haran Sorcerer::3::spellsword" = list(status = "working", note = "Magical combat abilities reduce matching immunity to resistance and resistance to normal damage."),
  "Na'Haran Sorcerer::3::wild_insight" = list(status = "working", note = "Available in combat as a bonus-action d100 Wild Magic roll."),
  "Rogue::4::asi" = list(status = "working", note = "Two points are allocated to one or two abilities, capped at 20; Constitution updates HP retroactively."),
  "Fighter::4::asi" = list(status = "working", note = "Two points are allocated to one or two abilities, capped at 20; Constitution updates HP retroactively."),
  "Barbarian::4::asi" = list(status = "working", note = "Two points are allocated to one or two abilities, capped at 20; Constitution updates HP retroactively."),
  "Hanianol Sorcerer::4::asi" = list(status = "working", note = "Two points are allocated to one or two abilities, capped at 20; Blood Strength immediately updates spell attacks and save DC."),
  "Na'Haran Sorcerer::4::asi" = list(status = "working", note = "Two points are allocated to one or two abilities, capped at 20; Blood Strength immediately updates spell attacks and save DC."),
  "Rogue::5::uncanny_dodge" = list(status = "working", note = "When hit by a visible attacker, the Rogue may spend their reaction to halve the final damage."),
  "Fighter::5::extra_attack" = list(status = "working", note = "One Attack action supplies two weapon attacks and works correctly with Action Surge."),
  "Barbarian::5::extra_attack" = list(status = "working", note = "One Attack action supplies two weapon attacks; Frenzy still grants only one bonus attack."),
  "Barbarian::5::fast_movement" = list(status = "working", note = "Movement speed increases by 10 feet unless heavy armour is equipped."),
  "Hanianol Sorcerer::5::thermal_wild_magic" = list(status = "working", note = "The saved Exothermic or Endothermic path unlocks an executable, Blood Strength-based combat spell."),
  "Na'Haran Sorcerer::5::adept_sorcerer" = list(status = "working", note = "The saved Exothermic or Endothermic path unlocks an executable, Blood Strength-based combat spell."),
  "Rogue::6::expertise" = list(status = "working", note = "Two different saved skill choices are promoted to Expertise without reducing existing ranks."),
  "Fighter::6::asi" = list(status = "working", note = "Uses the validated two-point ability improvement system, including the cap and Constitution HP adjustment."),
  "Barbarian::6::mindless_rage" = list(status = "working", note = "Charm and fear conditions are suppressed while Rage is active."),
  "Barbarian::6::aspect_of_the_beast" = list(status = "manual", note = "The level-3 Totem determines the exploration benefit and it is displayed for checks and travel adjudication."),
  "Hanianol Sorcerer::6::balance" = list(status = "working", note = "Spend 15 Sindre once per long rest to add +1 to three different abilities until the next rest."),
  "Hanianol Sorcerer::6::predator" = list(status = "working", note = "A short-rest combat action frightens nearby enemies that fail a Blood Strength-based save."),
  "Na'Haran Sorcerer::6::electromagnetic" = list(status = "working", note = "The saved Flesh Witherer or Lightbringer path unlocks its executable combat spell."),
  "Na'Haran Sorcerer::6::combat_magic" = list(status = "working", note = "Path of the Warrior uses the selected electromagnetic spell through the shared combat casting system."),
  "Na'Haran Sorcerer::6::divination" = list(status = "manual", note = "A narrative, under-the-stars information feature displayed for roleplay and DM adjudication."),
  "Na'Haran Sorcerer::6::mislead" = list(status = "working", note = "A 25-Sindre combat action creates an illusory double and invisibility effect.")
)

class_feature_integration <- function(class_name, level, feature_id) {
  key <- paste(class_name, as.integer(level), feature_id, sep = "::")
  CLASS_FEATURE_INTEGRATION[[key]] %||% list(
    status = "unreviewed", note = "This feature has not yet received a mechanical integration review."
  )
}

audit_class_level_integration <- function(level, class_defs = CLASSES) {
  level <- as.integer(level)
  rows <- list()
  for (class_name in names(class_defs)) {
    feature_sets <- list(
      list(subclass = "", features = class_defs[[class_name]]$levels[[as.character(level)]]$features %||% list())
    )
    subclasses <- class_defs[[class_name]]$subclasses %||% list()
    for (subclass_name in names(subclasses)) {
      feature_sets[[length(feature_sets) + 1L]] <- list(
        subclass = subclass_name,
        features = subclasses[[subclass_name]]$levels[[as.character(level)]]$features %||% list()
      )
    }

    for (feature_set in feature_sets) {
      for (feature_id in names(feature_set$features)) {
        feature <- feature_set$features[[feature_id]]
        review <- class_feature_integration(class_name, level, feature_id)
        rows[[length(rows) + 1L]] <- data.frame(
          class = class_name, subclass = feature_set$subclass, level = level,
          feature_id = feature_id,
          feature = as.character(feature$name %||% feature_id),
          status = as.character(review$status), note = as.character(review$note),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

# Canonical rules for fixed class magic. Descriptions in game_data.R remain the
# player-facing lore; these records are deliberately explicit enough for the
# combat engine to resolve without interpreting prose.
CLASS_SPELL_DEFINITIONS <- list(
  grasping_vines = list(
    name = "Grasping Vines", class = "Hanianol Sorcerer", level = 2L,
    choice = c(natural_specialty = "Plants"), tags = c("spell", "combat", "control"),
    action_type = "action", cost = 20L, range_ft = 60L,
    target = list(type = "area", shape = "radius", size_ft = 20L, affects = "enemies"),
    resolution = list(type = "saving_throw", ability = "str", on_success = "not_restrained"),
    effects = list(
      list(type = "terrain", value = "difficult", duration = "1_minute"),
      list(type = "condition", value = "restrained", duration = "1_minute",
           repeat_save = "end_of_turn")
    ),
    duration = "1_minute", concentration = TRUE,
    scaling = list(
      `11` = list(radius_ft = 30L, save_disadvantage = TRUE),
      `15` = list(radius_ft = 40L, save_disadvantage = TRUE, repeat_save = "action")
    ),
    description = "Vines fill a 20-foot-radius area. Enemies that fail a Strength save are restrained and may repeat the save at the end of each turn; the whole area is difficult terrain."
  ),
  calling_rain = list(
    name = "Calling Rain", class = "Hanianol Sorcerer", level = 2L,
    choice = c(natural_specialty = "Rain"), tags = c("spell", "combat", "control"),
    action_type = "action", cost = 20L, range_ft = 120L,
    target = list(type = "area", shape = "radius", size_ft = 40L, affects = "all"),
    resolution = list(type = "automatic"),
    effects = list(
      list(type = "damage_modifier", damage_type = "fire", value = -4L, minimum = 0L),
      list(type = "damage_modifier", damage_type = c("cold", "lightning"), value = 2L)
    ),
    duration = "1_hour", concentration = TRUE,
    scaling = list(
      `11` = list(radius_ft = 60L, fire_modifier = -6L, cold_lightning_modifier = 4L),
      `15` = list(radius_ft = 90L, fire_modifier = -8L, cold_lightning_modifier = 6L)
    ),
    description = "A storm fills a 40-foot-radius area. Fire damage dealt inside it is reduced by 4; cold and lightning damage is increased by 2."
  ),
  call_beast = list(
    # NEXT CHANGE: improve beast selection, placement, stat cards and player control.
    name = "Call Beast", class = "Hanianol Sorcerer", level = 2L,
    choice = c(natural_specialty = "Animals"), tags = c("spell", "combat", "summoning"),
    action_type = "action", cost = 20L, range_ft = 30L,
    target = list(type = "point", affects = "empty_space"),
    resolution = list(type = "automatic"),
    effects = list(list(type = "summon", allegiance = "friendly", creature_type = "beast",
                        scaling = list(`2` = "CR 1/2", `11` = "CR 1", `15` = "CR 2"))),
    duration = "30_minutes", concentration = TRUE,
    description = "Summon a friendly beast in an empty space. Its maximum CR is 1/2, becoming CR 1 at level 11 and CR 2 at level 15. It acts immediately after you."
  ),
  wasting_sickness = list(
    name = "Wasting Sickness", class = "Hanianol Sorcerer", level = 2L,
    choice = c(natural_specialty = "Disease"), tags = c("spell", "combat", "condition"),
    action_type = "action", cost = 20L, range_ft = 0L,
    target = list(type = "area", shape = "radius", size_ft = 60L, origin = "self", affects = "all_other_creatures"),
    resolution = list(type = "saving_throw", ability = "con", on_success = "no_effect"),
    effects = list(list(type = "condition", value = "poisoned", duration = "1_minute",
                        repeat_save = "end_of_turn")),
    duration = "1_minute", concentration = TRUE,
    scaling = list(
      `11` = list(radius_ft = 60L, repeat_save = "end_of_turn_disadvantage"),
      `15` = list(radius_ft = 90L, repeat_save = "end_of_turn_disadvantage", healing_received = "half")
    ),
    description = "Sickness emanates 60 feet from you. Every other creature in the area, including allies, makes a Constitution save or becomes poisoned for up to 1 minute, repeating the save at the end of each turn."
  ),
  mind_bender = list(
    name = "Mind Bender", class = "Na'Haran Sorcerer", level = 2L,
    tags = c("spell", "utility", "social"), action_type = "action",
    cost = 10L, range_ft = 30L,
    target = list(type = "creature", count = 1L, affects = "non_hostile_creature"),
    resolution = list(type = "automatic"),
    effects = list(list(type = "advantage", applies_to = "next_persuasion_check",
                        restriction = "directed_at_target")),
    duration = "10_minutes", concentration = TRUE,
    description = "For 10 minutes, gain advantage on your next Persuasion check directed at one non-hostile creature you can see. The effect ends after that check."
  ),
  detect_undead = list(
    name = "Detect Undead", class = "Na'Haran Sorcerer", level = 2L,
    tags = c("spell", "utility", "detection"), action_type = "action",
    cost = 10L, range_ft = 0L,
    target = list(type = "area", shape = "radius", size_ft = 60L, origin = "self"),
    resolution = list(type = "automatic"),
    effects = list(list(type = "detect", creature_type = "undead",
                        information = c("direction", "distance"), blocked_by = "total_cover")),
    duration = "10_minutes", concentration = TRUE,
    description = "Sense the direction and distance of undead within 60 feet for 10 minutes. Total cover blocks the sense."
  ),
  exothermic_burst = list(
    name = "Exothermic Burst", class = "Na'Haran Sorcerer", level = 5L,
    choice = c(thermal_path = "Exothermic"), tags = c("spell", "combat", "damage"),
    action_type = "action", cost = 20L, range_ft = 60L,
    target = list(type = "creature", count = 1L, affects = "enemy"),
    resolution = list(type = "saving_throw", ability = "dex", on_success = "half_damage"),
    damage = list(dice = "2d8", type = "fire", scaling = list(`11` = "3d8", `17` = "4d8")),
    duration = "instantaneous", concentration = FALSE,
    description = "Release stored heat at one creature. It makes a Dexterity save, taking half damage on success."
  ),
  endothermic_grasp = list(
    name = "Endothermic Grasp", class = "Na'Haran Sorcerer", level = 5L,
    choice = c(thermal_path = "Endothermic"), tags = c("spell", "combat", "damage", "control"),
    action_type = "action", cost = 20L, range_ft = 60L,
    target = list(type = "creature", count = 1L, affects = "enemy"),
    resolution = list(type = "saving_throw", ability = "con", on_success = "half_damage_no_slow"),
    damage = list(dice = "2d8", type = "cold", scaling = list(`11` = "3d8", `17` = "4d8")),
    effects = list(list(type = "speed_modifier", value_ft = -10L, duration = "until_casters_next_turn")),
    duration = "until_casters_next_turn", concentration = FALSE,
    description = "Draw heat from one creature. On a failed Constitution save it also loses 10 feet of speed until your next turn."
  ),
  flesh_witherer = list(
    name = "Flesh Witherer", class = "Na'Haran Sorcerer", level = 6L,
    choice = c(electromagnetic_path = "Flesh Witherer"), tags = c("spell", "combat", "damage"),
    action_type = "action", cost = 25L, range_ft = 60L,
    target = list(type = "creature", count = 1L, affects = "enemy"),
    resolution = list(type = "saving_throw", ability = "con", on_success = "half_damage"),
    damage = list(dice = "3d8", type = "necrotic", scaling = list(`11` = "4d8", `17` = "5d8")),
    effects = list(list(type = "healing_block", duration = "until_casters_next_turn")),
    duration = "until_casters_next_turn", concentration = FALSE,
    description = "Wither a creature's flesh. A successful Constitution save halves the damage; on failure it cannot regain HP until your next turn."
  ),
  lightbringer = list(
    name = "Lightbringer", class = "Na'Haran Sorcerer", level = 6L,
    choice = c(electromagnetic_path = "Lightbringer"), tags = c("spell", "combat", "damage", "condition"),
    action_type = "action", cost = 25L, range_ft = 60L,
    target = list(type = "creature", count = 1L, affects = "enemy"),
    resolution = list(type = "saving_throw", ability = "dex", on_success = "half_damage_no_condition"),
    damage = list(dice = "3d8", type = "radiant", scaling = list(`11` = "4d8", `17` = "5d8")),
    effects = list(list(type = "condition", value = "blinded", duration = "until_casters_next_turn")),
    duration = "until_casters_next_turn", concentration = FALSE,
    description = "Strike one creature with searing light. A successful Dexterity save halves the damage; on failure it is blinded until your next turn."
  ),
  shadow_step = list(
    name = "Shadow Step", class = "Hanianol Sorcerer", subclass = "Heart Eater", level = 3L,
    tags = c("spell", "combat", "movement"), action_type = "bonus_action",
    cost = 10L, range_ft = "movement_speed",
    target = list(type = "point", affects = "unoccupied_space_in_dim_light_or_darkness"),
    resolution = list(type = "automatic"),
    effects = list(list(type = "teleport", scaling = list(
      `3` = "movement_speed", `10` = "double_movement_speed", `18` = "reaction_or_bonus_action"
    ))),
    duration = "instantaneous", concentration = FALSE,
    description = "Teleport between shadows to an unoccupied space you can see within your movement speed. At level 10 the range doubles; at level 18 it may also be used as a reaction when targeted by an attack."
  ),
  ancestral_elemental = list(
    name = "Conjure Ancestral Elemental", class = "Hanianol Sorcerer",
    subclass = "Path of the Ancestor", level = 10L,
    tags = c("spell", "combat", "summoning"), action_type = "action",
    cost = 40L, range_ft = 60L,
    target = list(type = "point", affects = "empty_space"), resolution = list(type = "automatic"),
    effects = list(list(type = "summon", allegiance = "friendly", creature_type = "elemental",
                        scaling = list(`10` = "CR 5", `15` = "CR 7", `17` = "CR 9"))),
    duration = "1_hour", concentration = TRUE,
    description = "Summon a friendly ancestral elemental (maximum CR 5) which acts immediately after you. Its limit becomes CR 7 at level 15 and CR 9 at level 17."
  ),
  flesh_witherers_hand = list(
    name = "Flesh Witherer's Hand", class = "Na'Haran Sorcerer",
    subclass = "Path of the Warrior", level = 6L,
    choice = c(electromagnetic_path = "Flesh Witherer"),
    tags = c("spell", "combat", "damage", "aura"), action_type = "bonus_action",
    cost = 25L, range_ft = 0L,
    target = list(type = "area", shape = "radius", size_ft = 10L, origin = "self", affects = "enemies"),
    resolution = list(type = "saving_throw", ability = "con", on_success = "half_damage"),
    damage = list(dice = "2d8", type = "necrotic", scaling = list(`11` = "3d8", `17` = "4d8")),
    duration = "instantaneous", concentration = FALSE,
    description = "Pulse 2d8 necrotic decay through enemies within 10 feet as a bonus action. A Constitution save halves the damage. Damage becomes 3d8 at level 11 and 4d8 at level 17."
  ),
  lightbringers_sword = list(
    name = "Lightbringer's Sword", class = "Na'Haran Sorcerer",
    subclass = "Path of the Warrior", level = 6L,
    choice = c(electromagnetic_path = "Lightbringer"),
    tags = c("spell", "combat", "damage", "condition"), action_type = "action",
    cost = 25L, range_ft = 5L,
    target = list(type = "creature", count = 1L, affects = "enemy"),
    resolution = list(type = "spell_attack", ability = "cha", on_miss = "no_effect"),
    damage = list(dice = "4d8", type = "radiant", scaling = list(`11` = "5d8", `17` = "6d8")),
    effects = list(list(type = "condition_save", value = "blinded", save = "con",
                        duration = "until_casters_next_turn")),
    duration = "until_casters_next_turn", concentration = FALSE,
    description = "Make a melee spell attack with a radiant blade. On a hit, the target also makes a Constitution save or is blinded until your next turn."
  ),
  mislead = list(
    name = "Mislead", class = "Na'Haran Sorcerer", subclass = "Path of the Prophet", level = 6L,
    tags = c("spell", "combat", "illusion"), action_type = "action",
    cost = 25L, range_ft = 0L, target = list(type = "self"),
    resolution = list(type = "automatic"),
    effects = list(
      list(type = "condition", value = "invisible", ends_on = c("attack", "damage", "spell")),
      list(type = "illusory_double", range_ft = 60L, movement_ft = 30L)
    ),
    duration = "1_minute", concentration = TRUE,
    description = "Become invisible and create an illusory double for up to 1 minute. The invisibility ends when you attack, deal damage or cast another spell; the double can move 30 feet on your turn."
  ),
  shield_of_sindre = list(
    name = "Shield of Sindre", class = "Na'Haran Sorcerer", level = 9L,
    tags = c("spell", "combat", "reaction", "defence"), action_type = "reaction",
    trigger = "targeted_by_spell_or_magical_effect", cost = 20L, range_ft = 0L,
    target = list(type = "self"), resolution = list(type = "automatic"),
    effects = list(
      list(type = "saving_throw_advantage", against = "triggering_effect"),
      list(type = "resistance", damage_type = "triggering_damage", duration = "until_end_of_triggering_effect")
    ),
    duration = "triggering_effect", concentration = FALSE,
    description = "As a reaction when magic targets you, gain advantage on its saving throw and resistance to damage from that effect."
  ),
  predict_spell = list(
    name = "Predict Spell", class = "Na'Haran Sorcerer", level = 10L,
    tags = c("spell", "combat", "control"), action_type = "bonus_action",
    cost = 15L, range_ft = 60L,
    target = list(type = "creature", count = 1L, affects = "enemy"),
    resolution = list(type = "saving_throw", ability = "wis", on_success = "no_effect"),
    effects = list(list(type = "disadvantage", applies_to = "next_attack_or_ability_check",
                        duration = "until_end_of_targets_next_turn")),
    duration = "until_end_of_targets_next_turn", concentration = FALSE,
    description = "Read one enemy's immediate future. On a failed Wisdom save, its next attack roll or ability check is made with disadvantage."
  )
)

CLASS_SPELL_DEFINITIONS$hanianol_exothermic_burst <- CLASS_SPELL_DEFINITIONS$exothermic_burst
CLASS_SPELL_DEFINITIONS$hanianol_exothermic_burst$class <- "Hanianol Sorcerer"
CLASS_SPELL_DEFINITIONS$hanianol_endothermic_grasp <- CLASS_SPELL_DEFINITIONS$endothermic_grasp
CLASS_SPELL_DEFINITIONS$hanianol_endothermic_grasp$class <- "Hanianol Sorcerer"

character_level_choice <- function(char, class_name, level, choice_id) {
  choices <- char$build$level_choices[[class_name]][[as.character(level)]] %||% list()
  as.character(choices[[choice_id]] %||% "")
}

class_spellcasting_ability <- function(class_name) {
  # Both traditions draw magic through their blood connection to the Mandred.
  # Intelligence and Charisma still support their mundane class skills, but do
  # not determine the raw strength of their magic.
  if (as.character(class_name %||% "") %in%
      c("Hanianol Sorcerer", "Na'Haran Sorcerer")) "bld_str" else "int"
}

character_proficiency_bonus <- function(char) {
  classes <- normalise_character_classes(char)
  total_level <- sum(vapply(classes, function(entry) {
    level <- suppressWarnings(as.integer(entry$level %||% 0L))
    if (is.na(level)) 0L else max(0L, level)
  }, integer(1)))
  2L + max(0L, floor((max(1L, total_level) - 1L) / 4L))
}

class_spell_save_dc <- function(char, spell) {
  ability_name <- as.character(spell$casting_ability %||%
    class_spellcasting_ability(spell$class))
  score <- suppressWarnings(as.integer(char$abilities[[ability_name]] %||% 10L))
  if (is.na(score)) score <- 10L
  8L + character_proficiency_bonus(char) + floor((score - 10L) / 2L)
}

spell_choice_is_unlocked <- function(spell, char) {
  required <- spell$choice %||% character()
  if (!length(required)) return(TRUE)
  all(vapply(names(required), function(choice_id) {
    identical(
      character_level_choice(char, spell$class, spell$level, choice_id),
      as.character(required[[choice_id]])
    )
  }, logical(1)))
}

spell_subclass_is_unlocked <- function(spell, char, class_defs = CLASSES) {
  required <- as.character(spell$subclass %||% "")
  if (!nzchar(required)) return(TRUE)
  classes <- normalise_character_classes(char, class_defs)
  any(vapply(classes, function(entry) {
    identical(as.character(entry$class %||% ""), as.character(spell$class %||% "")) &&
      identical(as.character(entry$subclass %||% ""), required)
  }, logical(1)))
}

get_unlocked_class_spells <- function(char, spell_defs = CLASS_SPELL_DEFINITIONS,
                                      class_defs = CLASSES) {
  classes <- normalise_character_classes(char, class_defs)
  class_levels <- vapply(classes, function(entry) as.integer(entry$level %||% 0L), integer(1))
  names(class_levels) <- vapply(classes, function(entry) as.character(entry$class %||% ""), character(1))
  Filter(function(spell) {
    class_name <- as.character(spell$class %||% "")
    level <- if (class_name %in% names(class_levels)) class_levels[[class_name]] else 0L
    level >= as.integer(spell$level %||% 99L) &&
      spell_subclass_is_unlocked(spell, char, class_defs) && spell_choice_is_unlocked(spell, char)
  }, spell_defs)
}

scale_class_spell <- function(spell, class_level) {
  scaled <- spell
  scaling <- spell$scaling %||% list()
  thresholds <- suppressWarnings(as.integer(names(scaling)))
  eligible <- which(!is.na(thresholds) & thresholds <= as.integer(class_level %||% 0L))
  if (!length(eligible)) return(scaled)
  upgrade <- scaling[[eligible[which.max(thresholds[eligible])]]]
  if (!is.null(upgrade$radius_ft)) {
    scaled$target$size_ft <- as.integer(upgrade$radius_ft)
  }
  scaled$resolved_upgrade <- upgrade
  scaled
}

CLASS_FEATURE_MECHANICS <- list(
  "Fighter::1::second_wind" = list(
    tags = c("ability", "combat", "healing"),
    action = list(
      name = "Second Wind",
      target = "self",
      action_type = "bonus_action",
      healing = list(mode = "dice_plus_class_level", value = "1d10", class = "Fighter"),
      usage = list(key = "second_wind", recharge = "short_rest", uses = 1L)
    )
  ),
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
      action_type = "action",
      range_ft = 5L,
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
      required_target_condition = "grappled",
      action_type = "action",
      damage = list(mode = "dice", value = "1d8", type = "necrotic"),
      resource = list(name = "sindre", cost = 10L)
    )
  ),
  "Na'Haran Sorcerer::11::improved_channeling" = list(
    tags = c("ability", "combat", "spell"),
    action = list(
      name = "Improved Water Channeler",
      group = "water_channeler",
      target = "enemy",
      required_target_condition = "grappled",
      action_type = "action",
      damage = list(mode = "dice", value = "2d8", type = "necrotic"),
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
    integration = class_feature_integration(class_name, level, feature_id),
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
    dice = as.integer(roll_function(as.character(damage$value %||% "1d4"))$total %||% 0L),
    suppressWarnings(as.integer(damage$value %||% 0L))
  )
  if (is.na(amount)) amount <- 0L

  list(
    amount = as.integer(amount),
    damage_type = as.character(damage$type %||% "")
  )
}

resolve_class_action_healing <- function(action, char, roll_function = roll_dice_expr) {
  healing <- action$healing %||% list()
  mode <- as.character(healing$mode %||% "fixed")
  amount <- switch(
    mode,
    dice_plus_class_level = {
      rolled <- roll_function(as.character(healing$value %||% "1d4"))
      class_name <- as.character(healing$class %||% "")
      classes <- normalise_character_classes(char)
      class_level <- sum(vapply(classes, function(entry) {
        if (identical(as.character(entry$class %||% ""), class_name)) {
          as.integer(entry$level %||% 0L)
        } else 0L
      }, integer(1)))
      as.integer(rolled$total %||% 0L) + class_level
    },
    suppressWarnings(as.integer(healing$value %||% 0L))
  )
  if (is.na(amount)) amount <- 0L
  max(0L, as.integer(amount))
}

class_action_use_available <- function(char, action) {
  usage <- action$usage %||% list()
  key <- as.character(usage$key %||% "")
  if (!nzchar(key)) return(TRUE)
  !isTRUE(char$resources$class_uses[[key]]$used %||% FALSE)
}

class_resource_remaining <- function(char, key, maximum) {
  maximum <- max(0L, as.integer(maximum %||% 0L))
  value <- suppressWarnings(as.integer(char$resources$class_pools[[key]]$remaining %||% maximum))
  if (is.na(value)) value <- maximum
  max(0L, min(maximum, value))
}

spend_class_resource <- function(char, key, maximum, recharge = "long_rest", amount = 1L) {
  remaining <- class_resource_remaining(char, key, maximum)
  amount <- max(1L, as.integer(amount %||% 1L))
  if (remaining < amount) return(NULL)
  char$resources <- char$resources %||% list()
  char$resources$class_pools <- char$resources$class_pools %||% list()
  char$resources$class_pools[[key]] <- list(
    remaining = remaining - amount,
    maximum = as.integer(maximum),
    recharge = as.character(recharge)
  )
  char
}

barbarian_rage_maximum <- function(char) {
  classes <- normalise_character_classes(char)
  level <- sum(vapply(classes, function(entry) {
    if (identical(as.character(entry$class %||% ""), "Barbarian")) as.integer(entry$level %||% 0L) else 0L
  }, integer(1)))
  if (level < 1L) return(0L)
  if (level >= 17L) return(6L)
  if (level >= 12L) return(5L)
  if (level >= 6L) return(4L)
  if (level >= 3L) return(3L)
  2L
}

new_turn_action_budget <- function(turn_key = "") {
  list(
    key = as.character(turn_key), actions = 1L, bonus_actions = 1L, reactions = 1L,
    attack_chain = 0L, bonus_attacks = 0L
  )
}

attacks_per_attack_action <- function(char) {
  classes <- normalise_character_classes(char)
  fighter_level <- sum(vapply(classes, function(entry) {
    if (identical(as.character(entry$class %||% ""), "Fighter")) as.integer(entry$level %||% 0L) else 0L
  }, integer(1)))
  if (fighter_level >= 20L) return(4L)
  if (fighter_level >= 11L) return(3L)
  has_extra_attack <- any(vapply(get_unlocked_class_features(char), function(feature) {
    identical(as.character(feature$id %||% ""), "extra_attack")
  }, logical(1)))
  if (has_extra_attack) 2L else 1L
}

spend_attack_from_budget <- function(budget, attacks_per_action = 1L) {
  if (as.integer(budget$bonus_attacks %||% 0L) > 0L) {
    budget$bonus_attacks <- as.integer(budget$bonus_attacks) - 1L
    return(budget)
  }
  if (as.integer(budget$attack_chain %||% 0L) > 0L) {
    budget$attack_chain <- as.integer(budget$attack_chain) - 1L
    return(budget)
  }
  if (!turn_action_available(budget, "action")) return(NULL)
  budget <- spend_turn_action(budget, "action")
  budget$attack_chain <- max(0L, as.integer(attacks_per_action %||% 1L) - 1L)
  budget
}

grant_bonus_attack <- function(budget, amount = 1L) {
  budget$bonus_attacks <- as.integer(budget$bonus_attacks %||% 0L) + max(0L, as.integer(amount))
  budget
}

turn_action_field <- function(action_type) {
  switch(as.character(action_type %||% "action"),
         bonus_action = "bonus_actions", reaction = "reactions", "actions")
}

turn_action_available <- function(budget, action_type = "action") {
  field <- turn_action_field(action_type)
  as.integer(budget[[field]] %||% 0L) > 0L
}

spend_turn_action <- function(budget, action_type = "action") {
  field <- turn_action_field(action_type)
  if (!turn_action_available(budget, action_type)) return(NULL)
  budget[[field]] <- as.integer(budget[[field]] %||% 0L) - 1L
  budget
}

grant_turn_action <- function(budget, amount = 1L) {
  budget$actions <- as.integer(budget$actions %||% 0L) + max(0L, as.integer(amount))
  budget
}

mark_class_action_used <- function(char, action) {
  usage <- action$usage %||% list()
  key <- as.character(usage$key %||% "")
  if (!nzchar(key)) return(char)
  char$resources <- char$resources %||% list()
  char$resources$class_uses <- char$resources$class_uses %||% list()
  char$resources$class_uses[[key]] <- list(
    used = TRUE,
    recharge = as.character(usage$recharge %||% "long_rest")
  )
  char
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

  artisan_tool <- character_level_choice(char, "Fighter", 3L, "student_of_war_tool")
  if (nzchar(artisan_tool)) {
    tool_key <- tolower(gsub("[^a-z0-9]+", "_", artisan_tool))
    tool_key <- gsub("^_|_$", "", tool_key)
    char$prof$tools[[tool_key]] <- TRUE
    sources[[paste0("tool:", tool_key)]] <- unique(c(
      sources[[paste0("tool:", tool_key)]] %||% character(),
      "Fighter::Battle Master::3::student_of_war"
    ))
  }

  expertise_choices <- c(
    character_level_choice(char, "Rogue", 6L, "expertise_skill_1"),
    character_level_choice(char, "Rogue", 6L, "expertise_skill_2")
  )
  for (skill in unique(expertise_choices[nzchar(expertise_choices)])) {
    char$prof$skills[[skill]] <- "Expertise"
    sources[[paste0("skill:", skill)]] <- unique(c(
      sources[[paste0("skill:", skill)]] %||% character(), "Rogue::::6::expertise"
    ))
  }

  totem <- character_level_choice(char, "Barbarian", 3L, "spirit_totem")
  if (nzchar(totem) && any(vapply(features, function(feature) identical(feature$id, "aspect_of_the_beast"), logical(1)))) {
    char$derived_effects <- char$derived_effects %||% list()
    char$derived_effects$aspect_of_the_beast <- switch(
      totem,
      Bear = "Double carrying capacity; advantage on Strength checks to push, pull, lift or break objects.",
      Eagle = "See distant detail up to one mile and suffer no Perception disadvantage in dim light.",
      Wolf = "Track at a fast pace and move stealthily at a normal travel pace.",
      ""
    )
  }

  char$derived_effects <- char$derived_effects %||% list()
  char$derived_effects$sources <- sources
  char$derived_effects$conditional <- conditional
  char
}
