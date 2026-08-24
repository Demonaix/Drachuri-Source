enemy_damage_types <- function() c("slashing","piercing","bludgeoning","fire","cold","lightning","acid","poison","necrotic","radiant","psychic","force","thunder","iron","silver")
enemy_materials <- function() c("natural","wood","stone","metal","bronze","iron","steel","silver","bone","chemical","magic")
enemy_conditions <- function() c("blinded","charmed","deafened","frightened","grappled","incapacitated","paralysed","petrified","poisoned","prone","restrained","stunned","unconscious")
resolve_layered_damage_traits <- function(layers=list()) {
  layers<-Filter(is.list,layers);res_layers<-lapply(layers,function(x)unique(tolower(as.character(x$resistances%||%character()))));imm<-unique(unlist(lapply(layers,function(x)tolower(as.character(x$immunities%||%character())))));vul<-unique(unlist(lapply(layers,function(x)tolower(as.character(x$vulnerabilities%||%character())))))
  all_res<-unlist(res_layers);if(length(all_res)){counts<-table(all_res);imm<-unique(c(imm,names(counts[counts>=2L])))};res<-setdiff(unique(all_res),imm);both<-intersect(res,vul);res<-setdiff(res,both);vul<-setdiff(vul,c(both,imm));conditions<-unique(unlist(lapply(layers,function(x)tolower(as.character(x$condition_immunities%||%character())))))
  list(resistances=res,immunities=imm,vulnerabilities=vul,condition_immunities=conditions)
}

enemy_special_attack <- function(name,hit=2L,dmg="1d4",type="bludgeoning",material="natural",kind="natural",action="action",range_ft=5L,long_range_ft=5L,ability="str",rarity="core",lore_status="canonical",desc="",requires="",on_hit_condition="",duration="",push_ft=0L,heal_fraction=0,usage="unlimited") {
  list(name=name,hit=as.integer(hit),dmg=dmg,type=type,material=material,kind=kind,action=action,range_ft=as.integer(range_ft),long_range_ft=as.integer(long_range_ft),ability=ability,rarity=rarity,lore_status=lore_status,desc=desc,requires=requires,on_hit_condition=on_hit_condition,duration=duration,push_ft=as.integer(push_ft),heal_fraction=as.numeric(heal_fraction),usage=usage)
}

enemy_attack_catalog <- function() list(
  unarmed_strike=enemy_special_attack("Unarmed Strike",2L,"1",desc="An ordinary physical fallback."),
  animal_claw=enemy_special_attack("Claw",4L,"1d6+2","slashing",desc="Only for animals with effective claws."),
  animal_bite=enemy_special_attack("Bite",4L,"1d6+2","piercing",desc="Only for animals with an appropriate bite."),
  animal_gore=enemy_special_attack("Gore",4L,"1d8+2","piercing",desc="Only for horned or tusked animals."),
  animal_kick=enemy_special_attack("Kick",4L,"1d6+2","bludgeoning",desc="Only for hoofed or suitably large animals."),
  great_beast_maul=enemy_special_attack("Maul",6L,"2d8+4","slashing",rarity="elite",desc="A heavy predator attack."),
  great_beast_grab=enemy_special_attack("Seizing Bite",6L,"2d6+4","piercing",rarity="elite",on_hit_condition="grappled",duration="until_escape",usage="once_per_turn",desc="Seizes and grapples its target."),
  great_beast_pounce=enemy_special_attack("Pounce",6L,"2d6+4","slashing",rarity="elite",requires="moved_20ft_straight",on_hit_condition="prone",usage="once_per_turn",desc="An anatomy-dependent running pounce."),
  great_beast_charge=enemy_special_attack("Charge",6L,"2d8+4","piercing",rarity="elite",requires="moved_20ft_straight",on_hit_condition="prone",push_ft=10L,usage="once_per_turn",desc="An anatomy-dependent charge."),
  desperate_shove=enemy_special_attack("Desperate Shove",2L,"0","bludgeoning",kind="tactical",on_hit_condition="prone",push_ft=5L,desc="A mundane attempt to create an escape route."),
  dirty_kick=enemy_special_attack("Dirty Kick",3L,"1","bludgeoning",kind="tactical",action="bonus",on_hit_condition="slowed",duration="end_next_turn",usage="once_per_turn",rarity="uncommon",desc="Reduces the target's speed until its next turn."),
  pocket_sand=enemy_special_attack("Throw Dirt",3L,"0","bludgeoning",kind="tactical",requires="loose_dirt",on_hit_condition="distracted",duration="end_next_turn",usage="once_per_encounter",rarity="uncommon",desc="Imposes disadvantage on the target's next attack."),
  dead_grasp=enemy_special_attack("Dead Grasp",3L,"1d4+2","bludgeoning",on_hit_condition="grappled",duration="until_escape",desc="A restless corpse closes its grip."),
  servitor_strike=enemy_special_attack("Servitor Strike",4L,"1d6+2","bludgeoning",desc="A raised servitor's basic strike."),
  restraining_grip=enemy_special_attack("Restraining Grip",4L,"1d4+2","bludgeoning",on_hit_condition="grappled",duration="until_escape",rarity="uncommon",desc="A servitor seizes its target."),
  mandred_bolt=enemy_special_attack("Mandred Bolt",5L,"1d8+3","force","magic","magical",range_ft=60L,long_range_ft=120L,ability="bld_str",desc="A direct projection of manipulated mandred."),
  mandred_push=enemy_special_attack("Mandred Push",5L,"0","force","magic","magical",range_ft=30L,long_range_ft=30L,ability="bld_str",rarity="uncommon",push_ft=10L,desc="Mandred force drives the target backwards."),
  mandred_grasp=enemy_special_attack("Mandred Grasp",5L,"0","force","magic","magical",range_ft=30L,long_range_ft=30L,ability="bld_str",rarity="uncommon",on_hit_condition="grappled",duration="end_next_turn",usage="recharge_5_6",desc="Mandred holds the target in place."),
  withering_touch=enemy_special_attack("Withering Touch",5L,"2d6+3","necrotic","magic","magical",ability="bld_str",on_hit_condition="healing_blocked",duration="start_attacker_next_turn",desc="A necromantic touch that briefly prevents healing."),
  grave_bolt=enemy_special_attack("Grave Bolt",6L,"1d10+3","necrotic","magic","magical",range_ft=60L,long_range_ft=120L,ability="bld_str",desc="Necrotic force lashes from the caster."),
  spectral_grasp=enemy_special_attack("Spectral Grasp",6L,"1d6+3","necrotic","magic","magical",range_ft=30L,long_range_ft=30L,ability="bld_str",rarity="uncommon",on_hit_condition="grappled",duration="until_escape",usage="recharge_5_6",desc="A spectral grip catches the target."),
  life_drain=enemy_special_attack("Life Drain",7L,"2d8+4","necrotic","magic","magical",range_ft=30L,long_range_ft=30L,ability="bld_str",rarity="elite",heal_fraction=.5,usage="once_per_encounter",desc="Life bleeds into the necromancer."),
  blood_feed=enemy_special_attack("Drink Blood",7L,"1d6+2d6","necrotic","natural","special",ability="bld_str",rarity="elite",requires="target_grappled_restrained_or_incapacitated",heal_fraction=1,usage="once_per_turn",desc="A Cythraul feeds from a vulnerable biological target."),
  overwhelming_mandred=enemy_special_attack("Overwhelming Mandred",8L,"3d8+4","force","magic","magical",range_ft=15L,long_range_ft=15L,ability="bld_str",rarity="boss",lore_status="provisional",push_ft=10L,usage="recharge_5_6",desc="A deliberately enabled cone of restored mandred force."),
  shadow_strike=enemy_special_attack("Shadow Strike",7L,"2d6+4","slashing",ability="dex",desc="The Llechwyr strikes from darkness."),
  shadow_pounce=enemy_special_attack("Shadow Pounce",7L,"3d6+4","slashing",ability="dex",rarity="elite",requires="hidden_in_dim_or_dark",usage="once_per_encounter",desc="The Llechwyr springs from darkness."),
  drag_into_darkness=enemy_special_attack("Drag into Darkness",7L,"1d6+4","slashing",ability="str",rarity="elite",on_hit_condition="grappled",duration="until_escape",usage="recharge_5_6",desc="The Llechwyr seizes a victim."),
  integrated_strike=enemy_special_attack("Integrated Strike",6L,"2d8+4","bludgeoning","metal",ability="str",desc="A weapon or limb built into a sorcerous construct."),
  integrated_projectile=enemy_special_attack("Integrated Projectile",5L,"1d8+2","piercing","metal",range_ft=30L,long_range_ft=90L,ability="dex",rarity="uncommon",lore_status="provisional",requires="authored_projectile_mechanism",usage="ammunition",desc="A specifically authored projectile mechanism."),
  mandred_discharge=enemy_special_attack("Mandred Discharge",6L,"2d8+3","force","magic","magical",range_ft=30L,long_range_ft=60L,ability="bld_str",rarity="uncommon",lore_status="provisional",requires="sorcerous_power_mechanism",usage="recharge_5_6",desc="A construct releases stored mandred."),
  alchemical_flask=enemy_special_attack("Alchemical Flask",4L,"1d6","acid","chemical","special",range_ft=20L,long_range_ft=60L,ability="dex",rarity="uncommon",lore_status="provisional",requires="authored_alchemical_flask",usage="consumable",desc="A whitelisted authored alchemical substance."),
  tinkerer_device=enemy_special_attack("Discharge Device",5L,"1d8+2","force","magic","special",range_ft=30L,long_range_ft=60L,ability="bld_str",rarity="uncommon",lore_status="provisional",requires="authored_tinkerer_device",usage="recharge_5_6",desc="A specifically authored sorcerous device."),
  club=list(name="Club",hit=2L,dmg="1d4+1",type="bludgeoning",material="wood",loot_id="club"),
  longsword=list(name="Longsword",hit=3L,dmg="1d8+1",type="slashing",material="",loot_id="longsword"),
  shortsword=list(name="Shortsword",hit=3L,dmg="1d6+1",type="slashing",material="",loot_id="shortsword"),
  dagger=list(name="Dagger",hit=3L,dmg="1d4+1",type="piercing",material="",loot_id="dagger"),
  battleaxe=list(name="Battleaxe",hit=4L,dmg="1d8+2",type="slashing",material="",loot_id="battleaxe"),
  spear=list(name="Spear",hit=3L,dmg="1d6+1",type="piercing",material="",loot_id="spear"),
  shortbow=list(name="Shortbow",hit=3L,dmg="1d6+1",type="piercing",material="wood",loot_id="shortbow")
)

enemy_loot_catalog <- function() list(
  club=list(name="Club",type="weapon",desc="A plain wooden club or sturdy work tool.",value=1,weight=2,qty=1,meta=list(stat="str",adv="Normal",to_hit_bonus=0,damage1="1d4",dmg_type1="Bludgeoning",damage2="",dmg_type2="Other",material="wood",proficient=TRUE)),
  longsword=list(name="Longsword",type="weapon",desc="A well-balanced martial blade associated with trained and wealthy bearers.",value=15,weight=3,qty=1,meta=list(stat="str",adv="Normal",to_hit_bonus=0,damage1="1d8",dmg_type1="Slashing",damage2="",dmg_type2="Other",proficient=TRUE)),
  shortsword=list(name="Shortsword",type="weapon",desc="A serviceable shortsword.",value=10,weight=2,qty=1,meta=list(stat="dex",adv="Normal",to_hit_bonus=0,damage1="1d6",dmg_type1="Slashing",damage2="",dmg_type2="Other",proficient=TRUE)),
  dagger=list(name="Dagger",type="weapon",desc="A balanced dagger.",value=5,weight=1,qty=1,meta=list(stat="dex",adv="Normal",to_hit_bonus=0,damage1="1d4",dmg_type1="Piercing",damage2="",dmg_type2="Other",proficient=TRUE)),
  battleaxe=list(name="Battleaxe",type="weapon",desc="A heavy battleaxe.",value=15,weight=4,qty=1,meta=list(stat="str",adv="Normal",to_hit_bonus=0,damage1="1d8",dmg_type1="Slashing",damage2="",dmg_type2="Other",proficient=TRUE)),
  spear=list(name="Spear",type="weapon",desc="A practical spear.",value=6,weight=3,qty=1,meta=list(stat="str",adv="Normal",to_hit_bonus=0,damage1="1d6",dmg_type1="Piercing",damage2="",dmg_type2="Other",proficient=TRUE)),
  shortbow=list(name="Shortbow",type="weapon",desc="A compact hunting bow.",value=20,weight=2,qty=1,meta=list(stat="dex",adv="Normal",to_hit_bonus=0,damage1="1d6",dmg_type1="Piercing",damage2="",dmg_type2="Other",material="wood",proficient=TRUE)),
  leather_armor=list(name="Leather Armour",type="armor",desc="Light leather armour.",value=10,weight=10,qty=1,meta=list(base_ac=11,type="Light",custom_max_dex=0,proficient=TRUE)),
  padded_armor=list(name="Padded Armour",type="armor",desc="Quilted light armour.",value=5,weight=8,qty=1,meta=list(base_ac=11,type="Light",custom_max_dex=0,proficient=TRUE)),
  studded_leather=list(name="Studded Leather",type="armor",desc="Reinforced light leather armour.",value=45,weight=13,qty=1,meta=list(base_ac=12,type="Light",custom_max_dex=0,proficient=TRUE)),
  hide_armor=list(name="Hide Armour",type="armor",desc="Tough layered hide armour.",value=10,weight=12,qty=1,meta=list(base_ac=12,type="Medium",custom_max_dex=2,proficient=TRUE)),
  chain_shirt=list(name="Chain Shirt",type="armor",desc="A fitted shirt of interlocking iron rings.",value=50,weight=20,qty=1,meta=list(base_ac=13,type="Medium",custom_max_dex=2,proficient=TRUE)),
  scale_mail=list(name="Scale Mail",type="armor",desc="Overlapping metal scale armour.",value=50,weight=45,qty=1,meta=list(base_ac=14,type="Medium",custom_max_dex=2,proficient=TRUE)),
  breastplate=list(name="Breastplate",type="armor",desc="A fitted metal breastplate.",value=400,weight=20,qty=1,meta=list(base_ac=14,type="Medium",custom_max_dex=2,proficient=TRUE)),
  half_plate=list(name="Half Plate",type="armor",desc="Heavy sections of fitted plate.",value=750,weight=40,qty=1,meta=list(base_ac=15,type="Medium",custom_max_dex=2,proficient=TRUE)),
  ring_mail=list(name="Ring Mail",type="armor",desc="Leather reinforced with heavy rings.",value=30,weight=40,qty=1,meta=list(base_ac=14,type="Heavy",custom_max_dex=0,proficient=TRUE)),
  chain_mail=list(name="Chain Mail",type="armor",desc="Interlocking heavy chain armour.",value=75,weight=55,qty=1,meta=list(base_ac=16,type="Heavy",custom_max_dex=0,proficient=TRUE)),
  splint_armor=list(name="Splint Armour",type="armor",desc="Vertical metal strips over padding.",value=200,weight=60,qty=1,meta=list(base_ac=17,type="Heavy",custom_max_dex=0,proficient=TRUE)),
  plate_armor=list(name="Plate Armour",type="armor",desc="A complete fitted suit of plate.",value=1500,weight=65,qty=1,meta=list(base_ac=18,type="Heavy",custom_max_dex=0,proficient=TRUE)),
  shield=list(name="Shield",type="armor",desc="A sturdy shield granting +2 AC.",value=10,weight=6,qty=1,meta=list(base_ac=2,type="Shield",custom_max_dex=0,proficient=TRUE)),
  animal_pelt=list(name="Animal Pelt",type="item",desc="A usable hide taken from an animal.",value=4,weight=5,qty=1,meta=list()),
  bear_pelt=list(name="Bear Pelt",type="item",desc="A thick and valuable bear pelt.",value=12,weight=12,qty=1,meta=list()),
  fae_dust=list(name="Fae Dust",type="item",desc="Faintly luminous residue from a fae creature.",value=25,weight=.1,qty=1,meta=list()),
  bone_fragment=list(name="Bone Fragment",type="item",desc="A useful crafting component.",value=2,weight=.5,qty=1,meta=list()),
  healing_draught=list(name="Healing Draught",type="consumable",desc="A prepared restorative draught.",value=20,weight=.5,qty=1,meta=list(effect="heal",amount="1d6+2"))
)

enemy_armor_catalog <- function() list(
  unarmoured=list(name="Unarmoured",ac_base=10L,dex_cap=99L,loot_id="",desc="No armour; AC comes from body, agility, and creature traits."),
  padded=list(name="Padded Armour",ac_base=11L,dex_cap=99L,loot_id="padded_armor",desc="Light armour: AC 11 + DEX."),
  leather=list(name="Leather Armour",ac_base=11L,dex_cap=99L,loot_id="leather_armor",desc="Light armour: AC 11 + DEX."),
  studded_leather=list(name="Studded Leather",ac_base=12L,dex_cap=99L,loot_id="studded_leather",desc="Light armour: AC 12 + DEX."),
  hide=list(name="Hide Armour",ac_base=12L,dex_cap=2L,loot_id="hide_armor",desc="Medium armour: AC 12 + DEX (maximum +2)."),
  chain_shirt=list(name="Chain Shirt",ac_base=13L,dex_cap=2L,loot_id="chain_shirt",desc="Medium armour: AC 13 + DEX (maximum +2)."),
  scale_mail=list(name="Scale Mail",ac_base=14L,dex_cap=2L,loot_id="scale_mail",desc="Medium armour: AC 14 + DEX (maximum +2)."),
  breastplate=list(name="Breastplate",ac_base=14L,dex_cap=2L,loot_id="breastplate",desc="Medium armour: AC 14 + DEX (maximum +2)."),
  half_plate=list(name="Half Plate",ac_base=15L,dex_cap=2L,loot_id="half_plate",desc="Medium armour: AC 15 + DEX (maximum +2)."),
  ring_mail=list(name="Ring Mail",ac_base=14L,dex_cap=0L,loot_id="ring_mail",desc="Heavy armour: AC 14."),
  chain_mail=list(name="Chain Mail",ac_base=16L,dex_cap=0L,loot_id="chain_mail",desc="Heavy armour: AC 16."),
  splint=list(name="Splint Armour",ac_base=17L,dex_cap=0L,loot_id="splint_armor",desc="Heavy armour: AC 17."),
  plate=list(name="Plate Armour",ac_base=18L,dex_cap=0L,loot_id="plate_armor",desc="Heavy armour: AC 18."),
  shield=list(name="Shield",ac_base=2L,dex_cap=99L,loot_id="shield",desc="Adds +2 AC; select alongside body armour only through a characteristic later.")
)

enemy_generator_types <- function() list(
  Custom=list(desc="A neutral foundation for a bespoke enemy.",hp_max=10L,ac=10L,armor_id="unarmoured",movement_speed=30L,abilities=c(str=10L,dex=10L,con=10L,int=10L,cha=10L,bld_str=10L),attack_ids="unarmed_strike",loot_ids=character(),gold=c(1L,6L)),
  Bandit=list(desc="A lightly armoured opportunist with blade and coin.",hp_max=12L,ac=13L,armor_id="leather",movement_speed=30L,abilities=c(str=12L,dex=14L,con=12L,int=10L,cha=10L,bld_str=10L),attack_ids=c("shortsword","shortbow"),loot_ids=character(),gold=c(3L,12L)),
  Guard=list(desc="A trained defensive humanoid carrying practical equipment.",hp_max=18L,ac=14L,armor_id="chain_shirt",movement_speed=30L,abilities=c(str=14L,dex=12L,con=14L,int=10L,cha=10L,bld_str=10L),attack_ids="spear",loot_ids=character(),gold=c(4L,10L)),
  Cultist=list(desc="Legacy magical humanoid foundation; prefer an authored Oldrin sorcerer pool.",hp_max=14L,ac=12L,armor_id="leather",movement_speed=30L,abilities=c(str=10L,dex=12L,con=12L,int=11L,cha=13L,bld_str=15L),attack_ids=c("dagger","mandred_bolt"),loot_ids=character(),gold=c(5L,15L)),
  Animal=list(desc="A natural beast; carries no gold or manufactured equipment.",hp_max=11L,ac=12L,armor_id="unarmoured",movement_speed=40L,abilities=c(str=12L,dex=14L,con=12L,int=3L,cha=6L,bld_str=8L),attack_ids=c("animal_claw","animal_bite"),loot_ids="animal_pelt",gold=c(0L,0L)),
  Fae=list(desc="A fae foundation for deliberately magical or altered fae only.",hp_max=10L,ac=12L,armor_id="unarmoured",movement_speed=30L,abilities=c(str=8L,dex=14L,con=10L,int=12L,cha=14L,bld_str=12L),attack_ids="unarmed_strike",loot_ids=character(),gold=c(0L,4L)),
  Undead=list(desc="A deathless creature resistant to decay and immune to poison.",hp_max=16L,ac=14L,armor_id="ring_mail",movement_speed=25L,abilities=c(str=13L,dex=8L,con=15L,int=6L,cha=5L,bld_str=4L),attack_ids="dead_grasp",loot_ids="bone_fragment",gold=c(0L,8L),immunities="poison",condition_immunities=c("poisoned","frightened")),
  Construct=list(desc="A context-restricted sorcerous or tinkered creation.",hp_max=24L,ac=16L,armor_id="unarmoured",movement_speed=20L,abilities=c(str=16L,dex=6L,con=18L,int=5L,cha=3L,bld_str=8L),attack_ids="integrated_strike",loot_ids=character(),gold=c(0L,0L),resistances=c("slashing","piercing"),immunities="poison",condition_immunities=c("poisoned","charmed"))
)

enemy_generator_characteristics <- function() list(
  Barbarian=list(desc="+2 STR and CON; adds a battleaxe and bear pelt.",ability_bonus=c(str=2L,con=2L),attack_ids="battleaxe",loot_ids="bear_pelt"),
  Speedy=list(desc="+2 DEX and +10 ft movement.",ability_bonus=c(dex=2L),movement_bonus=10L),
  Boss=list(desc="Double HP, +1 AC and +2 to every attribute.",ability_bonus=c(str=2L,dex=2L,con=2L,int=2L,cha=2L,bld_str=2L),hp_multiplier=2,ac_bonus=1L,gold_multiplier=2),
  Fae=list(desc="Adds vulnerability to iron and immunity to being charmed.",vulnerabilities="iron",condition_immunities="charmed"),
  Animal=list(desc="Adds anatomy-dependent claw and bite attacks, a pelt, and removes gold.",attack_ids=c("animal_claw","animal_bite"),loot_ids="animal_pelt",no_gold=TRUE),
  Armoured=list(desc="+3 AC and guarantees lootable leather armour.",ac_bonus=3L,loot_ids="leather_armor"),
  Brute=list(desc="+4 STR, +2 CON, +50% HP, but -2 DEX and 10 ft speed.",ability_bonus=c(str=4L,con=2L,dex=-2L),hp_multiplier=1.5,movement_bonus=-10L),
  Archer=list(desc="+2 DEX and adds a lootable shortbow attack.",ability_bonus=c(dex=2L),attack_ids="shortbow"),
  Venomous=list(desc="Adds poison resistance; any poison attack still requires an authored anatomy.",resistances="poison",attack_ids="animal_bite"),
  Undead=list(desc="Poison immune; cannot be poisoned or frightened.",immunities="poison",condition_immunities=c("poisoned","frightened")),
  Fire_Touched=list(name="Fire-touched",desc="Resists fire and is vulnerable to cold; it does not grant fire breath.",resistances="fire",vulnerabilities="cold"),
  Regenerator=list(desc="A durable creature: +50% HP and +2 CON.",ability_bonus=c(con=2L),hp_multiplier=1.5),
  Spellcaster=list(desc="+2 Blood Strength and adds a basic Mandred Bolt.",ability_bonus=c(bld_str=2L),attack_ids="mandred_bolt")
)

npc_feature_definition <- function(id,name,category,desc="",compatible_pools=character(),incompatible_pools=character(),
                                   requires_features=character(),requires_equipment=character(),exclusive_group="",
                                   lore_status="mechanically_inferred",rarity="core",generation_weight=0,
                                   ability_bonus=numeric(),hp_multiplier=1,ac_bonus=0,movement_bonus=0,initiative_bonus=0,
                                   gold_multiplier=1,attacks=character(),loot_ids=character(),resistances=character(),
                                   immunities=character(),vulnerabilities=character(),condition_immunities=character(),
                                   ai_tags=character(),display_name_rule="never",enabled_for_generation=TRUE) {
  list(id=id,name=name,category=category,desc=desc,compatible_pools=compatible_pools,incompatible_pools=incompatible_pools,
       requires_features=requires_features,requires_equipment=requires_equipment,exclusive_group=exclusive_group,
       lore_status=lore_status,rarity=rarity,generation_weight=as.numeric(generation_weight),ability_bonus=ability_bonus,
       hp_multiplier=as.numeric(hp_multiplier),ac_bonus=as.integer(ac_bonus),movement_bonus=as.integer(movement_bonus),
       initiative_bonus=as.integer(initiative_bonus),gold_multiplier=as.numeric(gold_multiplier),attacks=attacks,
       loot_ids=loot_ids,resistances=resistances,immunities=immunities,vulnerabilities=vulnerabilities,
       condition_immunities=condition_immunities,ai_tags=ai_tags,display_name_rule=display_name_rule,
       enabled_for_generation=isTRUE(enabled_for_generation),feature_version=1L)
}

npc_feature_catalogue <- function() {
  f<-npc_feature_definition
  list(
    farmer=f("farmer","Farmer","occupation","A rural civilian accustomed to physical work.","oldrin_civilian",exclusive_group="civilian_occupation",generation_weight=20,ability_bonus=c(str=1,con=1),gold_multiplier=.6,loot_ids="club",ai_tags=c("defend_home","avoid_combat"),display_name_rule="occupation"),
    labourer=f("labourer","Labourer","occupation","A strong but untrained civilian worker.","oldrin_civilian",exclusive_group="civilian_occupation",generation_weight=15,ability_bonus=c(str=2,con=1),loot_ids="club",display_name_rule="occupation"),
    craftsperson=f("craftsperson","Craftsperson","occupation","A skilled maker with tools and materials.","oldrin_civilian",exclusive_group="civilian_occupation",generation_weight=15,ability_bonus=c(int=1,dex=1),display_name_rule="occupation"),
    merchant=f("merchant","Merchant","occupation","A civilian whose wealth is held in goods and trade.","oldrin_civilian",exclusive_group="civilian_occupation",rarity="uncommon",generation_weight=10,ability_bonus=c(cha=2,int=1),gold_multiplier=2.5,ai_tags=c("negotiate","flee"),display_name_rule="occupation"),
    servant_attendant=f("servant_attendant","Household Attendant","occupation","A household servant, messenger or attendant.","oldrin_civilian",exclusive_group="civilian_occupation",generation_weight=12,ability_bonus=c(dex=1,cha=1),ai_tags=c("raise_alarm","flee"),display_name_rule="contextual"),
    scribe_clerk=f("scribe_clerk","Clerk / Scribe","occupation","A literate civilian responsible for records.","oldrin_civilian",exclusive_group="civilian_occupation",rarity="uncommon",generation_weight=8,ability_bonus=c(int=2,str=-1),display_name_rule="occupation"),
    traveller=f("traveller","Traveller","occupation","A person equipped for roads and temporary shelter.",c("oldrin_civilian","fae_wanderer"),exclusive_group="civilian_occupation",generation_weight=20,ability_bonus=c(con=1),ai_tags="seek_escape",display_name_rule="contextual"),
    ambusher=f("ambusher","Ambusher","training","Training in exploiting concealment and surprise.",c("oldrin_bandit_raider","forest_outlaw","oldrin_hunter","fae_hunter"),generation_weight=30,ability_bonus=c(dex=1),initiative_bonus=2,ai_tags="seek_hidden_opening"),
    dirty_fighter=f("dirty_fighter","Dirty Fighter","training","Mundane fighting methods that exploit distraction and terrain.",c("oldrin_bandit_raider","forest_outlaw","mercenary_soldier"),rarity="uncommon",generation_weight=30,ability_bonus=c(dex=1),attacks=c("dirty_kick","pocket_sand"),ai_tags="exploit_weakness"),
    reckless_fighter=f("reckless_fighter","Reckless Fighter","training","An aggressive fighting style, not a culture or player class.",c("oldrin_bandit_raider","mercenary_soldier","house_soldier","fae_warrior"),exclusive_group="combat_style",rarity="uncommon",generation_weight=15,ability_bonus=c(str=2,con=1,dex=-1),hp_multiplier=1.1,ai_tags="close_aggressively"),
    skirmisher_training=f("skirmisher_training","Skirmisher","training","Mobile training intended to avoid being pinned.",c("oldrin_bandit_raider","forest_outlaw","oldrin_hunter","mercenary_soldier","fae_hunter","fae_warrior"),rarity="uncommon",generation_weight=30,ability_bonus=c(dex=1),movement_bonus=5,ai_tags="reposition"),
    trap_setter=f("trap_setter","Trapper","training","Preparation and placement of mundane traps.",c("forest_outlaw","oldrin_hunter","fae_hunter"),requires_equipment="mundane_043",rarity="uncommon",generation_weight=30,ability_bonus=c(dex=1,int=1),loot_ids="mundane_043",ai_tags="prepare_terrain"),
    trained_archer=f("trained_archer","Trained Archer","training","Formal or practical bow training.",c("forest_outlaw","oldrin_hunter","town_guard","house_soldier","mercenary_soldier","fae_hunter","fae_warrior"),requires_equipment="shortbow",generation_weight=65,ability_bonus=c(dex=1),loot_ids="shortbow",ai_tags=c("maintain_range","seek_cover")),
    tracker=f("tracker","Tracker","training","Reading signs and following trails without magical certainty.",c("forest_outlaw","oldrin_hunter","fae_hunter"),generation_weight=60,ability_bonus=c(bld_str=1),ai_tags="track"),
    wilderness_survivor=f("wilderness_survivor","Wilderness Survivor","training","Practical survival in Annwn's wilderness.",c("forest_outlaw","oldrin_hunter","fae_wanderer","fae_hunter"),generation_weight=55,ability_bonus=c(con=1,bld_str=1),ai_tags="terrain_confident"),
    scout_training=f("scout_training","Scout","training","Observation, light equipment and cautious movement.",c("oldrin_hunter","house_soldier","mercenary_soldier","fae_hunter"),rarity="uncommon",generation_weight=30,ability_bonus=c(dex=1,bld_str=1),ai_tags="observe_first"),
    guard_training=f("guard_training","Guard Training","training","Training to arrest, contain and raise an alarm.","town_guard",generation_weight=100,ability_bonus=c(str=1,con=1),ai_tags=c("arrest","hold_position")),
    soldier_training=f("soldier_training","Soldier Training","training","Professional military discipline.",c("house_soldier","mercenary_soldier"),generation_weight=100,ability_bonus=c(str=1,con=1),hp_multiplier=1.1,ai_tags="hold_objective"),
    formation_training=f("formation_training","Formation Training","training","Disciplined coordination with adjacent allies.",c("house_soldier","veteran_house_guard"),rarity="uncommon",generation_weight=55,ai_tags="stay_in_formation"),
    shield_training=f("shield_training","Shield Training","training","Training enabled by an actual carried shield.",c("town_guard","house_soldier","veteran_house_guard","mercenary_soldier","fae_warrior"),requires_equipment="shield",rarity="uncommon",generation_weight=40,loot_ids="shield",ai_tags="protect_flank"),
    spear_training=f("spear_training","Spear Training","training","Training in bracing and disciplined spear use.",c("town_guard","house_soldier","veteran_house_guard","mercenary_soldier","oldrin_hunter","fae_warrior"),requires_equipment="spear",generation_weight=45,ability_bonus=c(str=1),loot_ids="spear"),
    bodyguard_training=f("bodyguard_training","Bodyguard","training","Training to remain near and protect an assigned person.",c("veteran_house_guard","mercenary_soldier","fae_warrior"),rarity="elite",generation_weight=30,ability_bonus=c(con=1),hp_multiplier=1.1,ai_tags="protect_assigned_target",display_name_rule="role"),
    veteran_training=f("veteran_training","Veteran","training","Experience without supernatural embellishment.",c("veteran_house_guard","mercenary_soldier","house_soldier","fae_warrior"),rarity="elite",generation_weight=15,ability_bonus=c(str=1,dex=1,con=1),hp_multiplier=1.25,initiative_bonus=1,ai_tags="tactical"),
    officer=f("officer","Officer","rank","An authored leader of nearby allies; exact House rank remains contextual.",c("veteran_house_guard","house_soldier","town_guard","mercenary_soldier"),rarity="elite",generation_weight=8,ability_bonus=c(cha=2,int=1),hp_multiplier=1.1,initiative_bonus=1,ai_tags="coordinate_allies",display_name_rule="rank"),
    professional_mercenary=f("professional_mercenary","Professional Mercenary","training","Experienced contractual soldiering, not a universal mercenary culture.","mercenary_soldier",generation_weight=60,ability_bonus=c(int=1,con=1),initiative_bonus=1,ai_tags=c("preserve_self","complete_contract")),
    campaign_hardened=f("campaign_hardened","Campaign-Hardened","training","Experience of sustained conflict without fear immunity.",c("mercenary_soldier","veteran_house_guard","house_soldier"),rarity="elite",generation_weight=12,ability_bonus=c(con=2,cha=1),hp_multiplier=1.2,ai_tags="steady"),
    light_armour=f("light_armour","Light Armour","equipment","Actual light armour that affects AC and remains lootable.",c("oldrin_bandit_raider","forest_outlaw","oldrin_hunter","town_guard","mercenary_soldier","fae_hunter","fae_warrior"),exclusive_group="armour_class",generation_weight=35,loot_ids="leather_armor"),
    medium_armour=f("medium_armour","Medium Armour","equipment","Actual medium armour that affects AC and remains lootable.",c("town_guard","house_soldier","veteran_house_guard","mercenary_soldier","fae_warrior"),exclusive_group="armour_class",rarity="uncommon",generation_weight=35,loot_ids="scale_mail"),
    heavy_armour=f("heavy_armour","Heavy Armour","equipment","Costly heavy armour that affects AC and remains lootable.",c("house_soldier","veteran_house_guard","mercenary_soldier"),exclusive_group="armour_class",rarity="elite",generation_weight=15,loot_ids="chain_mail"),
    shield_equipped=f("shield_equipped","Shield","equipment","An actual shield, not an abstract AC bonus.",c("town_guard","house_soldier","veteran_house_guard","mercenary_soldier","fae_warrior"),requires_equipment="shield",generation_weight=40,loot_ids="shield"),
    powerful_build=f("powerful_build","Powerful Build","physical","An unusually strong, heavy individual rather than a species trait.",character(),incompatible_pools="llechwyr",exclusive_group="body_build",generation_weight=15,ability_bonus=c(str=2,con=1),hp_multiplier=1.1),
    fleet_footed=f("fleet_footed","Fleet-Footed","physical","Unusually quick movement.",character(),incompatible_pools="sorcerous_construct",generation_weight=12,ability_bonus=c(dex=1),movement_bonus=5,initiative_bonus=1),
    hardy=f("hardy","Hardy","physical","Unusually robust health.",character(),exclusive_group="body_build",generation_weight=15,ability_bonus=c(con=2),hp_multiplier=1.15),
    frail=f("frail","Frail","physical","A physically fragile individual.",c("oldrin_civilian","fae_wanderer"),exclusive_group="body_build",generation_weight=10,ability_bonus=c(str=-1,con=-2),hp_multiplier=.8),
    keen_reflexes=f("keen_reflexes","Keen Reflexes","physical","Fast reactions without supernatural implications.",character(),generation_weight=10,ability_bonus=c(dex=2),initiative_bonus=2),
    strong_blood=f("strong_blood","Strong Blood","physical","An unusually strong connection to mandred that does not itself grant magic.",c("oldrin_civilian","oldrin_bandit_raider","forest_outlaw","oldrin_hunter","town_guard","house_soldier","veteran_house_guard","mercenary_soldier","fae_wanderer","fae_hunter","fae_warrior"),rarity="rare",generation_weight=3,ability_bonus=c(bld_str=2)),
    mandred_sorcery=f("mandred_sorcery","Mandred Sorcery","magical_discipline","Actual sorcerous capability, distinct from merely high Blood Strength.","oldrin_sorcerer",generation_weight=100,ability_bonus=c(bld_str=2),attacks="mandred_bolt",ai_tags="use_mandred"),
    necromancy=f("necromancy","Necromancy","magical_discipline","Necromantic specialisation without implied Abyss allegiance.",c("oldrin_necromancer","oldrin_sorcerer"),requires_features="mandred_sorcery",exclusive_group="primary_sorcerous_discipline",generation_weight=100,ability_bonus=c(int=1,bld_str=2),attacks=c("withering_touch","grave_bolt")),
    alchemy=f("alchemy","Alchemy","magical_discipline","An established discipline whose combat items remain finite and authored.","oldrin_sorcerer",requires_features="mandred_sorcery",exclusive_group="primary_sorcerous_discipline",lore_status="canonical",rarity="uncommon",generation_weight=15,ability_bonus=c(int=2,dex=1),attacks="alchemical_flask"),
    tinkering=f("tinkering","Tinkering","magical_discipline","An established discipline using deliberately authored devices.","oldrin_sorcerer",requires_features="mandred_sorcery",exclusive_group="primary_sorcerous_discipline",lore_status="canonical",rarity="uncommon",generation_weight=15,ability_bonus=c(int=2,bld_str=1),attacks="tinkerer_device"),
    mandred_control=f("mandred_control","Mandred Control","magical_discipline","Focused manipulation of mandred.","oldrin_sorcerer",requires_features="mandred_sorcery",exclusive_group="primary_sorcerous_discipline",generation_weight=50,ability_bonus=c(bld_str=1),attacks=c("mandred_push","mandred_grasp")),
    undead_controller=f("undead_controller","Undead Controller","training","A necromancer trained to direct nearby undead.","oldrin_necromancer",requires_features="necromancy",rarity="uncommon",generation_weight=60,ai_tags="command_undead"),
    life_drain_adept=f("life_drain_adept","Life-Drain Adept","magical_discipline","Elite necromantic life-draining practice.","oldrin_necromancer",requires_features="necromancy",rarity="elite",generation_weight=15,attacks="life_drain"),
    fae_survivor=f("fae_survivor","Survivor","training","Fae survival experience rather than a racial power.",c("fae_wanderer","fae_hunter","fae_warrior"),generation_weight=55,ability_bonus=c(con=1,bld_str=1),ai_tags="survive"),
    fae_hunter_training=f("fae_hunter_training","Fae Hunter Training","training","Practical hunting and ambush training.","fae_hunter",generation_weight=100,ability_bonus=c(dex=1),ai_tags="hunt"),
    fae_martial_training=f("fae_martial_training","Fae Martial Training","training","Martial experience without automatic fae magic.","fae_warrior",generation_weight=100,ability_bonus=c(dex=1),ai_tags="mobile_melee"),
    old_instincts=f("old_instincts","Old Instincts","training","Exceptional experience in a specifically established long-lived fae.",c("fae_wanderer","fae_hunter","fae_warrior"),rarity="elite",generation_weight=12,initiative_bonus=1),
    cythraul_restoration=f("cythraul_restoration","Restored Fae Magic","supernatural_state","The defining restored magical state of a Cythraul.","cythraul",generation_weight=100,ability_bonus=c(bld_str=3),attacks="mandred_bolt",ai_tags="restored_mandred"),
    blood_fuelled_recovery=f("blood_fuelled_recovery","Blood-Fuelled Recovery","supernatural_state","Drink Blood restores health; this is not passive regeneration.","cythraul",requires_features="cythraul_restoration",rarity="uncommon",generation_weight=40,attacks="blood_feed"),
    degraded_corpse=f("degraded_corpse","Degraded","physical","A physically degraded corpse, not a new undead species.",c("restless_dead","necromantic_servitor"),exclusive_group="corpse_condition",generation_weight=40,ability_bonus=c(str=-1,dex=-2),hp_multiplier=.85,ac_bonus=-1),
    fresh_corpse=f("fresh_corpse","Recently Dead","physical","A relatively fresh corpse.",c("restless_dead","necromantic_servitor"),exclusive_group="corpse_condition",generation_weight=20,ability_bonus=c(dex=1)),
    relentless_dead=f("relentless_dead","Relentless","supernatural_state","Uncontrolled persistence through debris and obstruction.","restless_dead",generation_weight=30,ai_tags="relentless_advance"),
    tenacious_dead=f("tenacious_dead","Tenacious Dead","supernatural_state","An elite corpse that may refuse to fall once.","restless_dead",rarity="elite",generation_weight=8,ai_tags="refuse_to_fall"),
    bound_servitor=f("bound_servitor","Bound Servitor","supernatural_state","A servitor bound to a controller and simple commands.","necromantic_servitor",generation_weight=100,ai_tags="obey_command"),
    guardian_servitor=f("guardian_servitor","Guardian Servitor","behaviour","A bound servitor assigned to protect its controller.","necromantic_servitor",requires_features="bound_servitor",rarity="uncommon",generation_weight=20,hp_multiplier=1.15,ai_tags="interpose_for_master"),
    restraining_servitor=f("restraining_servitor","Restraining Servitor","design","A servitor built or trained to seize targets.","necromantic_servitor",rarity="uncommon",generation_weight=30,ability_bonus=c(str=1),attacks="restraining_grip"),
    shadow_stalker=f("shadow_stalker","Shadow Stalker","behaviour","Llechwyr stalking behaviour, not a subspecies.","llechwyr",generation_weight=70,ability_bonus=c(dex=1),ai_tags=c("fade_in_shadow","stalk_unseen")),
    patient_lurker=f("patient_lurker","Patient Lurker","behaviour","Waits in concealment for an advantageous moment.","llechwyr",generation_weight=40,initiative_bonus=1,ai_tags="remain_hidden"),
    shadow_pouncer=f("shadow_pouncer","Shadow Pouncer","behaviour","A Llechwyr that springs from darkness.","llechwyr",rarity="uncommon",generation_weight=30,ability_bonus=c(dex=1),attacks="shadow_pounce"),
    dragging_hunter=f("dragging_hunter","Dragging Hunter","behaviour","A powerful Llechwyr that drags prey into darkness.","llechwyr",rarity="elite",generation_weight=20,ability_bonus=c(str=2),attacks="drag_into_darkness"),
    strong_jaws=f("strong_jaws","Strong Jaws","anatomy","An anatomy-approved bite.",c("wild_animal","great_beast"),generation_weight=0,attacks="animal_bite",enabled_for_generation=FALSE),
    clawed=f("clawed","Clawed","anatomy","An anatomy-approved claw attack.",c("wild_animal","great_beast"),generation_weight=0,attacks="animal_claw",enabled_for_generation=FALSE),
    horned_or_tusked=f("horned_or_tusked","Horned / Tusked","anatomy","An anatomy-approved gore attack.",c("wild_animal","great_beast"),generation_weight=0,attacks="animal_gore",enabled_for_generation=FALSE),
    powerful_kick=f("powerful_kick","Powerful Kick","anatomy","An anatomy-approved kick.",c("wild_animal","great_beast"),generation_weight=0,attacks="animal_kick",enabled_for_generation=FALSE),
    venomous_anatomy=f("venomous_anatomy","Venomous","anatomy","Only for a species profile explicitly marked venomous.",c("wild_animal","great_beast"),generation_weight=0,enabled_for_generation=FALSE),
    massive_beast=f("massive_beast","Massive","physical","An exceptionally large specimen of an established animal.","great_beast",generation_weight=100,ability_bonus=c(str=3,con=2),hp_multiplier=1.5),
    pouncing_beast=f("pouncing_beast","Pouncing Predator","anatomy","Requires a pouncing predator body plan.","great_beast",exclusive_group="great_beast_mobility",generation_weight=0,ability_bonus=c(dex=1),attacks="great_beast_pounce",enabled_for_generation=FALSE),
    charging_beast=f("charging_beast","Charging Beast","anatomy","Requires a horned or heavy charging body plan.","great_beast",exclusive_group="great_beast_mobility",generation_weight=0,ability_bonus=c(str=2),attacks="great_beast_charge",enabled_for_generation=FALSE),
    constructed_body=f("constructed_body","Constructed Body","supernatural_state","The core condition of an authored sorcerous construct.","sorcerous_construct",generation_weight=100,immunities="poison",condition_immunities=c("poisoned","charmed")),
    heavy_frame=f("heavy_frame","Heavy Frame","physical","A heavy constructed frame resistant to displacement.","sorcerous_construct",generation_weight=40,ability_bonus=c(str=2,con=2,dex=-1),hp_multiplier=1.25,ai_tags="resist_forced_movement"),
    elite_combatant=f("elite_combatant","Elite","encounter_modifier","Improves existing legitimate capabilities without granting new magic or anatomy.",character(),rarity="elite",generation_weight=5,ability_bonus=c(str=1,con=1),hp_multiplier=1.35,ac_bonus=1,initiative_bonus=1),
    leader=f("leader","Leader","rank","Coordinates allies without inventing a faction or rank title.",character(),rarity="elite",generation_weight=5,ability_bonus=c(cha=2,int=1),hp_multiplier=1.15,initiative_bonus=1,ai_tags="coordinate_allies",display_name_rule="role"),
    boss_encounter=f("boss_encounter","Boss Encounter","encounter_modifier","Encounter importance only; grants no new magic, anatomy or supernatural state.",character(),rarity="boss",generation_weight=0,ability_bonus=c(str=1,dex=1,con=1),hp_multiplier=2,ac_bonus=1,initiative_bonus=2,enabled_for_generation=FALSE),
    injured=f("injured","Injured","temporary_state","Begins combat below maximum HP.",character(),exclusive_group="injury_state",generation_weight=8,ai_tags="cautious"),
    badly_injured=f("badly_injured","Badly Injured","temporary_state","Begins badly wounded and avoids direct combat.",character(),exclusive_group="injury_state",generation_weight=3,ability_bonus=c(str=-1,dex=-1),movement_bonus=-5,ai_tags="avoid_combat"),
    exhausted=f("exhausted","Exhausted","temporary_state","A tired living creature with reduced movement and reactions.",character(),incompatible_pools=c("restless_dead","necromantic_servitor","sorcerous_construct"),generation_weight=5,ability_bonus=c(con=-1,dex=-1),movement_bonus=-5,initiative_bonus=-2),
    desperate=f("desperate","Desperate","behaviour","Accepts greater tactical risk because safer choices have failed.",character(),generation_weight=8,ability_bonus=c(cha=-1),initiative_bonus=1,ai_tags="take_risks")
  )
}

enemy_characteristic_labels <- function() {
  values <- vapply(enemy_generator_characteristics(),function(x) as.character(x$name %||% ""),character(1))
  setNames(ifelse(nzchar(values),values,gsub("_"," ",names(values))),names(values))
}

enemy_loot_records <- function(ids) unname(enemy_loot_catalog()[unique(intersect(as.character(ids),names(enemy_loot_catalog())))])

enemy_is_animal <- function(enemy_type="", characteristics=character()) {
  identical(tolower(trimws(as.character(enemy_type%||%""))),"animal") ||
    any(tolower(as.character(characteristics%||%character()))=="animal")
}

roll_enemy_mundane_loot <- function(catalogue, enemy_type="", characteristics=character(), count=NULL) {
  if(enemy_is_animal(enemy_type,characteristics)||!length(catalogue))return(character())
  mundane<-names(Filter(function(x)identical(as.character((x$meta%||%list())$category%||%""),"mundane_loot"),catalogue))
  if(!length(mundane))return(character())
  if(is.null(count))count<-sample(1:3,1,prob=c(.5,.35,.15))
  count<-max(0L,min(length(mundane),as.integer(count%||%0L)))
  if(!count)character()else sample(mundane,count,replace=FALSE)
}

roll_enemy_food_loot <- function(catalogue, enemy_type="", characteristics=character(), count=NULL) {
  if(enemy_is_animal(enemy_type,characteristics)||!length(catalogue))return(character())
  food<-names(Filter(function(x)identical(as.character((x$meta%||%list())$category%||%""),"food"),catalogue))
  if(!length(food))return(character())
  if(is.null(count))count<-sample(1:3,1,prob=c(.6,.3,.1))
  count<-max(0L,min(length(food),as.integer(count%||%0L)))
  if(!count)character()else sample(food,count,replace=FALSE)
}

resolve_enemy_blueprint <- function(enemy_type="Custom", characteristics=character()) {
  types<-enemy_generator_types(); mods<-enemy_generator_characteristics(); type<-if(enemy_type%in%names(types))enemy_type else "Custom"; out<-types[[type]]
  out$enemy_type<-type; out$characteristics<-unique(intersect(as.character(characteristics),names(mods))); out$attack_ids<-as.character(out$attack_ids%||%character()); out$loot_ids<-as.character(out$loot_ids%||%character())
  for(nm in out$characteristics){mod<-mods[[nm]]; if(!is.null(mod$ability_bonus))for(stat in names(mod$ability_bonus))out$abilities[[stat]]<-out$abilities[[stat]]+mod$ability_bonus[[stat]]
    out$hp_max<-as.integer(round(out$hp_max*(mod$hp_multiplier%||%1))); out$ac<-as.integer(out$ac+(mod$ac_bonus%||%0L)); out$movement_speed<-as.integer(max(0,out$movement_speed+(mod$movement_bonus%||%0L)))
    out$attack_ids<-unique(c(out$attack_ids,mod$attack_ids%||%character())); out$loot_ids<-unique(c(out$loot_ids,mod$loot_ids%||%character()))
    if(isTRUE(mod$no_gold))out$gold<-c(0L,0L) else out$gold<-as.integer(round((out$gold%||%c(0,0))*(mod$gold_multiplier%||%1)))
    for(field in c("resistances","immunities","vulnerabilities","condition_immunities"))out[[field]]<-unique(c(out[[field]]%||%character(),mod[[field]]%||%character())) }
  attacks<-unname(enemy_attack_catalog()[out$attack_ids]); carried<-Filter(nzchar,vapply(attacks,function(a)as.character(a$loot_id%||%""),character(1))); armour_loot<-as.character(enemy_armor_catalog()[[out$armor_id%||%"unarmoured"]]$loot_id%||%""); out$loot_ids<-unique(c(out$loot_ids,carried,Filter(nzchar,armour_loot))); out$attacks<-attacks; out$loot<-enemy_loot_records(out$loot_ids)
  for(field in c("resistances","immunities","vulnerabilities","condition_immunities"))out[[field]]<-as.character(out[[field]]%||%character()); out
}

enemy_json <- function(x) jsonlite::toJSON(x%||%list(),auto_unbox=TRUE,null="null")
enemy_text_values <- function(x) unique(trimws(Filter(nzchar,unlist(strsplit(as.character(x%||%""),",",fixed=TRUE)))))
enemy_pg_array <- function(x){x<-as.character(x%||%character());if(!length(x))return("{}");paste0("{",paste(sprintf('"%s"',gsub('(["\\\\])','\\\\\\1',x)),collapse=","),"}")}
enemy_db_json <- function(x,default=list()){if(is.list(x)&&!is.data.frame(x))return(x);tryCatch(jsonlite::fromJSON(as.character(x%||%""),simplifyVector=FALSE),error=function(e)default)}
enemy_db_values <- function(x){if(is.null(x)||!length(x))return(character());if(is.list(x))x<-unlist(x,use.names=FALSE);text<-as.character(x);text<-gsub('^\\{|\\}$','',text);text<-gsub('^"|"$','',unlist(strsplit(text,',',fixed=TRUE)));unique(trimws(text[nzchar(trimws(text))]))}

enemy_loot_to_inventory_row <- function(item, id=NULL) data.frame(id=as.character(id%||%paste0("loot_",as.integer(Sys.time()),"_",sample(1000:9999,1))),name=as.character(item$name%||%"Loot"),type=as.character(item$type%||%"item"),desc=as.character(item$desc%||%""),value=as.numeric(item$value%||%0),weight=as.numeric(item$weight%||%0),qty=as.numeric(item$qty%||%1),equipped=FALSE,in_bag=FALSE,meta=I(list(item$meta%||%list())),edit=FALSE,stringsAsFactors=FALSE)
