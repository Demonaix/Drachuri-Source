enemy_damage_types <- function() c("slashing","piercing","bludgeoning","fire","cold","lightning","acid","poison","necrotic","radiant","psychic","force","thunder","iron","silver")
enemy_materials <- function() c("natural","wood","stone","bronze","iron","steel","silver","bone","magic")
enemy_conditions <- function() c("blinded","charmed","deafened","frightened","grappled","incapacitated","paralysed","petrified","poisoned","prone","restrained","stunned","unconscious")
resolve_layered_damage_traits <- function(layers=list()) {
  layers<-Filter(is.list,layers);res_layers<-lapply(layers,function(x)unique(tolower(as.character(x$resistances%||%character()))));imm<-unique(unlist(lapply(layers,function(x)tolower(as.character(x$immunities%||%character())))));vul<-unique(unlist(lapply(layers,function(x)tolower(as.character(x$vulnerabilities%||%character())))))
  all_res<-unlist(res_layers);if(length(all_res)){counts<-table(all_res);imm<-unique(c(imm,names(counts[counts>=2L])))};res<-setdiff(unique(all_res),imm);both<-intersect(res,vul);res<-setdiff(res,both);vul<-setdiff(vul,c(both,imm));conditions<-unique(unlist(lapply(layers,function(x)tolower(as.character(x$condition_immunities%||%character())))))
  list(resistances=res,immunities=imm,vulnerabilities=vul,condition_immunities=conditions)
}

enemy_attack_catalog <- function() list(
  unarmed=list(name="Unarmed Strike",hit=2L,dmg="1d4",type="bludgeoning",material="natural"),
  claw=list(name="Claw",hit=4L,dmg="1d6+2",type="slashing",material="natural"),
  bite=list(name="Bite",hit=4L,dmg="1d6+2",type="piercing",material="natural"),
  shortsword=list(name="Shortsword",hit=3L,dmg="1d6+1",type="slashing",material="",loot_id="shortsword"),
  dagger=list(name="Dagger",hit=3L,dmg="1d4+1",type="piercing",material="",loot_id="dagger"),
  battleaxe=list(name="Battleaxe",hit=4L,dmg="1d8+2",type="slashing",material="",loot_id="battleaxe"),
  spear=list(name="Spear",hit=3L,dmg="1d6+1",type="piercing",material="",loot_id="spear"),
  shortbow=list(name="Shortbow",hit=3L,dmg="1d6+1",type="piercing",material="wood",loot_id="shortbow"),
  fae_bolt=list(name="Fae Bolt",hit=4L,dmg="1d6+2",type="force",material="magic"),
  fire_breath=list(name="Fire Breath",hit=5L,dmg="2d6",type="fire",material="natural"),
  necrotic_touch=list(name="Necrotic Touch",hit=4L,dmg="1d8+2",type="necrotic",material="magic"),
  stone_fist=list(name="Stone Fist",hit=5L,dmg="1d10+3",type="bludgeoning",material="stone")
)

enemy_loot_catalog <- function() list(
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
  Custom=list(desc="A neutral foundation for a bespoke enemy.",hp_max=10L,ac=10L,armor_id="unarmoured",movement_speed=30L,abilities=c(str=10L,dex=10L,con=10L,int=10L,cha=10L,bld_str=10L),attack_ids="unarmed",loot_ids=character(),gold=c(1L,6L)),
  Bandit=list(desc="A lightly armoured opportunist with blade and coin.",hp_max=12L,ac=13L,armor_id="leather",movement_speed=30L,abilities=c(str=12L,dex=14L,con=12L,int=10L,cha=10L,bld_str=10L),attack_ids=c("shortsword","shortbow"),loot_ids=character(),gold=c(3L,12L)),
  Guard=list(desc="A trained defensive humanoid carrying practical equipment.",hp_max=18L,ac=14L,armor_id="chain_shirt",movement_speed=30L,abilities=c(str=14L,dex=12L,con=14L,int=10L,cha=10L,bld_str=10L),attack_ids="spear",loot_ids=character(),gold=c(4L,10L)),
  Cultist=list(desc="A blood-strength devotee using a dagger and dark magic.",hp_max=14L,ac=12L,armor_id="leather",movement_speed=30L,abilities=c(str=10L,dex=12L,con=12L,int=11L,cha=13L,bld_str=15L),attack_ids=c("dagger","necrotic_touch"),loot_ids=character(),gold=c(5L,15L)),
  Animal=list(desc="A natural beast; carries no gold or manufactured equipment.",hp_max=11L,ac=12L,armor_id="unarmoured",movement_speed=40L,abilities=c(str=12L,dex=14L,con=12L,int=3L,cha=6L,bld_str=8L),attack_ids=c("claw","bite"),loot_ids="animal_pelt",gold=c(0L,0L)),
  Fae=list(desc="An elusive magical creature vulnerable to iron.",hp_max=10L,ac=12L,armor_id="unarmoured",movement_speed=30L,abilities=c(str=8L,dex=14L,con=10L,int=12L,cha=14L,bld_str=12L),attack_ids="fae_bolt",loot_ids="fae_dust",gold=c(2L,10L),vulnerabilities="iron"),
  Undead=list(desc="A deathless creature resistant to decay and immune to poison.",hp_max=16L,ac=14L,armor_id="ring_mail",movement_speed=25L,abilities=c(str=13L,dex=8L,con=15L,int=6L,cha=5L,bld_str=4L),attack_ids="necrotic_touch",loot_ids="bone_fragment",gold=c(0L,8L),immunities="poison",condition_immunities=c("poisoned","frightened")),
  Construct=list(desc="A made creature with a stone body and no purse.",hp_max=24L,ac=16L,armor_id="unarmoured",movement_speed=20L,abilities=c(str=16L,dex=6L,con=18L,int=5L,cha=3L,bld_str=2L),attack_ids="stone_fist",loot_ids=character(),gold=c(0L,0L),resistances=c("slashing","piercing"),immunities="poison",condition_immunities=c("poisoned","charmed")),
  Dragonkin=list(desc="A powerful scaled predator with elemental breath.",hp_max=30L,ac=16L,armor_id="unarmoured",movement_speed=35L,abilities=c(str=18L,dex=12L,con=16L,int=12L,cha=14L,bld_str=15L),attack_ids=c("bite","fire_breath"),loot_ids=character(),gold=c(10L,30L),resistances="fire")
)

enemy_generator_characteristics <- function() list(
  Barbarian=list(desc="+2 STR and CON; adds a battleaxe and bear pelt.",ability_bonus=c(str=2L,con=2L),attack_ids="battleaxe",loot_ids="bear_pelt"),
  Speedy=list(desc="+2 DEX and +10 ft movement.",ability_bonus=c(dex=2L),movement_bonus=10L),
  Boss=list(desc="Double HP, +1 AC and +2 to every attribute.",ability_bonus=c(str=2L,dex=2L,con=2L,int=2L,cha=2L,bld_str=2L),hp_multiplier=2,ac_bonus=1L,gold_multiplier=2),
  Fae=list(desc="Adds vulnerability to iron and immunity to being charmed.",vulnerabilities="iron",condition_immunities="charmed"),
  Animal=list(desc="Adds claw and bite attacks, a pelt, and removes gold.",attack_ids=c("claw","bite"),loot_ids="animal_pelt",no_gold=TRUE),
  Armoured=list(desc="+3 AC and guarantees lootable leather armour.",ac_bonus=3L,loot_ids="leather_armor"),
  Brute=list(desc="+4 STR, +2 CON, +50% HP, but -2 DEX and 10 ft speed.",ability_bonus=c(str=4L,con=2L,dex=-2L),hp_multiplier=1.5,movement_bonus=-10L),
  Archer=list(desc="+2 DEX and adds a lootable shortbow attack.",ability_bonus=c(dex=2L),attack_ids="shortbow"),
  Venomous=list(desc="Adds poison resistance and a poisonous bite.",resistances="poison",attack_ids="bite"),
  Undead=list(desc="Poison immune; cannot be poisoned or frightened.",immunities="poison",condition_immunities=c("poisoned","frightened")),
  Fire_Touched=list(name="Fire-touched",desc="Resists fire, is vulnerable to cold, and gains fire breath.",resistances="fire",vulnerabilities="cold",attack_ids="fire_breath"),
  Regenerator=list(desc="A durable creature: +50% HP and +2 CON.",ability_bonus=c(con=2L),hp_multiplier=1.5),
  Spellcaster=list(desc="+2 Blood Strength and adds a magical necrotic attack.",ability_bonus=c(bld_str=2L),attack_ids="necrotic_touch")
)

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
