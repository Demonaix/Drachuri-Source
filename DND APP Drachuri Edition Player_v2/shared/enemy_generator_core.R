enemy_damage_types <- function() c("slashing","piercing","bludgeoning","fire","cold","lightning","acid","poison","necrotic","radiant","psychic","force","thunder","iron","silver")
enemy_materials <- function() c("natural","wood","stone","bronze","iron","steel","silver","bone","magic")
enemy_conditions <- function() c("blinded","charmed","deafened","frightened","grappled","incapacitated","paralysed","petrified","poisoned","prone","restrained","stunned","unconscious")

enemy_attack_catalog <- function() list(
  unarmed=list(name="Unarmed Strike",hit=2L,dmg="1d4",type="bludgeoning",material="natural"),
  claw=list(name="Claw",hit=4L,dmg="1d6+2",type="slashing",material="natural"),
  bite=list(name="Bite",hit=4L,dmg="1d6+2",type="piercing",material="natural"),
  shortsword=list(name="Shortsword",hit=3L,dmg="1d6+1",type="slashing",material="steel",loot_id="shortsword"),
  dagger=list(name="Dagger",hit=3L,dmg="1d4+1",type="piercing",material="steel",loot_id="dagger"),
  battleaxe=list(name="Battleaxe",hit=4L,dmg="1d8+2",type="slashing",material="steel",loot_id="battleaxe"),
  spear=list(name="Spear",hit=3L,dmg="1d6+1",type="piercing",material="iron",loot_id="spear"),
  shortbow=list(name="Shortbow",hit=3L,dmg="1d6+1",type="piercing",material="wood",loot_id="shortbow"),
  fae_bolt=list(name="Fae Bolt",hit=4L,dmg="1d6+2",type="force",material="magic"),
  fire_breath=list(name="Fire Breath",hit=5L,dmg="2d6",type="fire",material="natural"),
  necrotic_touch=list(name="Necrotic Touch",hit=4L,dmg="1d8+2",type="necrotic",material="magic"),
  stone_fist=list(name="Stone Fist",hit=5L,dmg="1d10+3",type="bludgeoning",material="stone")
)

enemy_loot_catalog <- function() list(
  shortsword=list(name="Shortsword",type="weapon",desc="A serviceable steel shortsword.",value=10,weight=2,qty=1,meta=list(stat="dex",adv="Normal",to_hit_bonus=0,damage1="1d6",dmg_type1="Slashing",damage2="",dmg_type2="Other",material="steel",proficient=TRUE)),
  dagger=list(name="Dagger",type="weapon",desc="A balanced steel dagger.",value=5,weight=1,qty=1,meta=list(stat="dex",adv="Normal",to_hit_bonus=0,damage1="1d4",dmg_type1="Piercing",damage2="",dmg_type2="Other",material="steel",proficient=TRUE)),
  battleaxe=list(name="Battleaxe",type="weapon",desc="A heavy steel battleaxe.",value=15,weight=4,qty=1,meta=list(stat="str",adv="Normal",to_hit_bonus=0,damage1="1d8",dmg_type1="Slashing",damage2="",dmg_type2="Other",material="steel",proficient=TRUE)),
  spear=list(name="Iron Spear",type="weapon",desc="An iron-headed spear.",value=6,weight=3,qty=1,meta=list(stat="str",adv="Normal",to_hit_bonus=0,damage1="1d6",dmg_type1="Piercing",damage2="",dmg_type2="Other",material="iron",proficient=TRUE)),
  shortbow=list(name="Shortbow",type="weapon",desc="A compact hunting bow.",value=20,weight=2,qty=1,meta=list(stat="dex",adv="Normal",to_hit_bonus=0,damage1="1d6",dmg_type1="Piercing",damage2="",dmg_type2="Other",material="wood",proficient=TRUE)),
  leather_armor=list(name="Leather Armour",type="armor",desc="Light leather armour.",value=10,weight=10,qty=1,meta=list(base_ac=11,type="Light",custom_max_dex=0,proficient=TRUE)),
  chain_shirt=list(name="Chain Shirt",type="armor",desc="A fitted shirt of interlocking iron rings.",value=50,weight=20,qty=1,meta=list(base_ac=13,type="Medium",custom_max_dex=2,proficient=TRUE)),
  animal_pelt=list(name="Animal Pelt",type="item",desc="A usable hide taken from an animal.",value=4,weight=5,qty=1,meta=list()),
  bear_pelt=list(name="Bear Pelt",type="item",desc="A thick and valuable bear pelt.",value=12,weight=12,qty=1,meta=list()),
  fae_dust=list(name="Fae Dust",type="item",desc="Faintly luminous residue from a fae creature.",value=25,weight=.1,qty=1,meta=list()),
  bone_fragment=list(name="Bone Fragment",type="item",desc="A useful crafting component.",value=2,weight=.5,qty=1,meta=list()),
  healing_draught=list(name="Healing Draught",type="consumable",desc="A prepared restorative draught.",value=20,weight=.5,qty=1,meta=list(effect="heal",amount="1d6+2"))
)

enemy_generator_types <- function() list(
  Custom=list(desc="A neutral foundation for a bespoke enemy.",hp_max=10L,ac=12L,movement_speed=30L,abilities=c(str=10L,dex=10L,con=10L,int=10L,cha=10L,bld_str=10L),attack_ids="unarmed",loot_ids=character(),gold=c(1L,6L)),
  Bandit=list(desc="A lightly armoured opportunist with blade and coin.",hp_max=12L,ac=12L,movement_speed=30L,abilities=c(str=12L,dex=14L,con=12L,int=10L,cha=10L,bld_str=10L),attack_ids=c("shortsword","shortbow"),loot_ids=c("leather_armor"),gold=c(3L,12L)),
  Guard=list(desc="A trained defensive humanoid carrying practical equipment.",hp_max=18L,ac=15L,movement_speed=30L,abilities=c(str=14L,dex=12L,con=14L,int=10L,cha=10L,bld_str=10L),attack_ids="spear",loot_ids=c("chain_shirt"),gold=c(4L,10L)),
  Cultist=list(desc="A blood-strength devotee using a dagger and dark magic.",hp_max=14L,ac=12L,movement_speed=30L,abilities=c(str=10L,dex=12L,con=12L,int=11L,cha=13L,bld_str=15L),attack_ids=c("dagger","necrotic_touch"),loot_ids=character(),gold=c(5L,15L)),
  Animal=list(desc="A natural beast; carries no gold or manufactured equipment.",hp_max=11L,ac=12L,movement_speed=40L,abilities=c(str=12L,dex=14L,con=12L,int=3L,cha=6L,bld_str=8L),attack_ids=c("claw","bite"),loot_ids="animal_pelt",gold=c(0L,0L)),
  Fae=list(desc="An elusive magical creature vulnerable to iron.",hp_max=10L,ac=13L,movement_speed=30L,abilities=c(str=8L,dex=14L,con=10L,int=12L,cha=14L,bld_str=12L),attack_ids="fae_bolt",loot_ids="fae_dust",gold=c(2L,10L),vulnerabilities="iron"),
  Undead=list(desc="A deathless creature resistant to decay and immune to poison.",hp_max=16L,ac=12L,movement_speed=25L,abilities=c(str=13L,dex=8L,con=15L,int=6L,cha=5L,bld_str=4L),attack_ids="necrotic_touch",loot_ids="bone_fragment",gold=c(0L,8L),immunities="poison",condition_immunities=c("poisoned","frightened")),
  Construct=list(desc="A made creature with a stone body and no purse.",hp_max=24L,ac=16L,movement_speed=20L,abilities=c(str=16L,dex=6L,con=18L,int=5L,cha=3L,bld_str=2L),attack_ids="stone_fist",loot_ids=character(),gold=c(0L,0L),resistances=c("slashing","piercing"),immunities="poison",condition_immunities=c("poisoned","charmed")),
  Dragonkin=list(desc="A powerful scaled predator with elemental breath.",hp_max=30L,ac=16L,movement_speed=35L,abilities=c(str=18L,dex=12L,con=16L,int=12L,cha=14L,bld_str=15L),attack_ids=c("bite","fire_breath"),loot_ids=character(),gold=c(10L,30L),resistances="fire")
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

resolve_enemy_blueprint <- function(enemy_type="Custom", characteristics=character()) {
  types<-enemy_generator_types(); mods<-enemy_generator_characteristics(); type<-if(enemy_type%in%names(types))enemy_type else "Custom"; out<-types[[type]]
  out$enemy_type<-type; out$characteristics<-unique(intersect(as.character(characteristics),names(mods))); out$attack_ids<-as.character(out$attack_ids%||%character()); out$loot_ids<-as.character(out$loot_ids%||%character())
  for(nm in out$characteristics){mod<-mods[[nm]]; if(!is.null(mod$ability_bonus))for(stat in names(mod$ability_bonus))out$abilities[[stat]]<-out$abilities[[stat]]+mod$ability_bonus[[stat]]
    out$hp_max<-as.integer(round(out$hp_max*(mod$hp_multiplier%||%1))); out$ac<-as.integer(out$ac+(mod$ac_bonus%||%0L)); out$movement_speed<-as.integer(max(0,out$movement_speed+(mod$movement_bonus%||%0L)))
    out$attack_ids<-unique(c(out$attack_ids,mod$attack_ids%||%character())); out$loot_ids<-unique(c(out$loot_ids,mod$loot_ids%||%character()))
    if(isTRUE(mod$no_gold))out$gold<-c(0L,0L) else out$gold<-as.integer(round((out$gold%||%c(0,0))*(mod$gold_multiplier%||%1)))
    for(field in c("resistances","immunities","vulnerabilities","condition_immunities"))out[[field]]<-unique(c(out[[field]]%||%character(),mod[[field]]%||%character())) }
  attacks<-unname(enemy_attack_catalog()[out$attack_ids]); carried<-Filter(nzchar,vapply(attacks,function(a)as.character(a$loot_id%||%""),character(1))); out$loot_ids<-unique(c(out$loot_ids,carried)); out$attacks<-attacks; out$loot<-enemy_loot_records(out$loot_ids)
  for(field in c("resistances","immunities","vulnerabilities","condition_immunities"))out[[field]]<-as.character(out[[field]]%||%character()); out
}

enemy_json <- function(x) jsonlite::toJSON(x%||%list(),auto_unbox=TRUE,null="null")
enemy_text_values <- function(x) unique(trimws(Filter(nzchar,unlist(strsplit(as.character(x%||%""),",",fixed=TRUE)))))
enemy_pg_array <- function(x){x<-as.character(x%||%character());if(!length(x))return("{}");paste0("{",paste(sprintf('"%s"',gsub('(["\\\\])','\\\\\\1',x)),collapse=","),"}")}
enemy_db_json <- function(x,default=list()){if(is.list(x)&&!is.data.frame(x))return(x);tryCatch(jsonlite::fromJSON(as.character(x%||%""),simplifyVector=FALSE),error=function(e)default)}
enemy_db_values <- function(x){if(is.null(x)||!length(x))return(character());if(is.list(x))x<-unlist(x,use.names=FALSE);text<-as.character(x);text<-gsub('^\\{|\\}$','',text);text<-gsub('^"|"$','',unlist(strsplit(text,',',fixed=TRUE)));unique(trimws(text[nzchar(trimws(text))]))}

enemy_loot_to_inventory_row <- function(item, id=NULL) data.frame(id=as.character(id%||%paste0("loot_",as.integer(Sys.time()),"_",sample(1000:9999,1))),name=as.character(item$name%||%"Loot"),type=as.character(item$type%||%"item"),desc=as.character(item$desc%||%""),value=as.numeric(item$value%||%0),weight=as.numeric(item$weight%||%0),qty=as.numeric(item$qty%||%1),equipped=FALSE,in_bag=FALSE,meta=I(list(item$meta%||%list())),edit=FALSE,stringsAsFactors=FALSE)
