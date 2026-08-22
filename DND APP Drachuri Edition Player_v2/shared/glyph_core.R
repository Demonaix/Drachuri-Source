glyph_character_level <- function(char) {
  classes<-char$build$classes%||%list()
  if(length(classes))sum(vapply(classes,function(x)as.integer(x$level%||%0L),integer(1)))else as.integer(char$build$level%||%1L)
}

glyph_arcana_bonus <- function(char) character_skill_modifier(char,"Arcana")

glyph_mastery_level <- function(char) {
  explicit<-suppressWarnings(as.integer((char$magic%||%list())$glyph_level%||%NA_integer_));if(!is.na(explicit))return(max(1L,min(4L,explicit)))
  length(get_available_rune_types(glyph_character_level(char)))
}
glyph_unlocked_ranks <- function(char) RUNE_TYPES[seq_len(glyph_mastery_level(char))]

glyph_roll_total <- function(expr) as.integer(roll_dice_expr(as.character(expr))$total)

glyph_material_requirement <- function(glyph_type,material) {
  if(tolower(glyph_type)=="rune")paste(tools::toTitleCase(tolower(material)),"Rune Blank")else as.character(material%||%"")
}

glyph_default_identity <- function(glyph_type,rank,damage_type,material=NULL,target_name=NULL) {
  type<-tolower(as.character(glyph_type));rank<-tools::toTitleCase(tolower(as.character(rank)));damage_type<-tools::toTitleCase(tolower(as.character(damage_type%||%"Arcane")))
  if(type=="rune")return(list(name=paste(rank,damage_type,"Rune"),description=paste0("A ",tolower(rank)," ",tolower(damage_type)," rune carved into ",tolower(as.character(material)),". When released it deals ",unname(RUNE_DAMAGE_BY_RANK[[rank]])," ",damage_type," damage in a ",unname(RUNE_RADIUS_BY_RANK[[rank]]),"ft area.")))
  if(type=="enhancement")return(list(name=paste(damage_type,"Enhancement"),description=paste0("Adds ",ENHANCEMENT_RULES[[rank]]$damage," ",damage_type," damage to attacks made with ",as.character(target_name%||%"the enchanted weapon")," while replenished.")))
  list(name=paste(rank,"Ward"),description=paste(rank,"ward"))
}

normalize_weapon_enchantments <- function(char,default_days=3L) {
  char<-validate_character(char);inv<-inventory_normalize(char$inventory$items);day<-as.integer(char$meta$day%||%1L)
  for(i in seq_len(nrow(inv))){if(inv$type[[i]]!="weapon")next;m<-inv$meta[[i]]%||%list();extra<-as.character(m$damage2%||%"");extra_type<-tools::toTitleCase(tolower(as.character(m$dmg_type2%||%"")));if(!nzchar(as.character(m$glyph_damage%||%""))&&nzchar(extra)&&extra_type%in%GLYPH_DAMAGE_TYPES){m$glyph_damage<-extra;m$glyph_damage_type<-extra_type;m$glyph_name<-as.character(m$magical_name%||%paste(extra_type,"Enchantment"));m$glyph_rank<-tools::toTitleCase(tolower(as.character(m$magic_tier%||%"")));if(!m$glyph_rank%in%names(ENHANCEMENT_RULES)){sides<-suppressWarnings(as.integer(sub(".*d([0-9]+).*","\\1",extra)));m$glyph_rank<-if(is.na(sides)||sides<=4)"Minor"else if(sides<=6)"Major"else if(sides<=8)"Arcane"else"Cursed"};m$glyph_replenish_days<-as.integer(default_days);m$glyph_active_until_day<-day+as.integer(default_days);m$damage2<-"";m$dmg_type2<-"Other";inv$meta[[i]]<-m}}
  char$inventory$items<-inv;char
}

replenishable_weapon_choices <- function(char) {
  char<-normalize_weapon_enchantments(char);inv<-inventory_normalize(char$inventory$items);keep<-vapply(inv$meta,function(m)nzchar(as.character((m%||%list())$glyph_damage%||%"")),logical(1))&inv$type=="weapon";inv[keep,,drop=FALSE]
}

glyph_inventory_index <- function(inv,name) {
  if(!nrow(inv)||!nzchar(as.character(name%||%"")))return(NA_integer_)
  idx<-which(tolower(trimws(inv$name))==tolower(trimws(name))&as.numeric(inv$qty)>0)
  if(length(idx))idx[[1L]]else NA_integer_
}

glyph_consume_inventory_item <- function(inv,name) {
  idx<-glyph_inventory_index(inv,name);if(is.na(idx))return(NULL)
  if(as.numeric(inv$qty[[idx]])>1)inv$qty[[idx]]<-as.numeric(inv$qty[[idx]])-1 else inv<-inv[-idx,,drop=FALSE]
  inventory_normalize(inv)
}

glyph_project_spec <- function(char,glyph_type,rank,material=NULL,size_ft=NULL,enhancement_days=NULL) {
  glyph_type<-tolower(as.character(glyph_type));rank<-tools::toTitleCase(tolower(as.character(rank)))
  if(!rank%in%glyph_unlocked_ranks(char))stop(paste(rank,"glyphs require Glyph Level",match(rank,RUNE_TYPES),". Your Glyph Level is",glyph_mastery_level(char),"."))
  arcana<-glyph_arcana_bonus(char);rule<-get_glyph_rule(glyph_type,rank,arcana,material,size_ft,enhancement_days);if(is.null(rule))stop("That glyph combination is not valid.")
  if(glyph_type=="rune"){
    hours<-glyph_roll_total(rule$crafting_time);cost_rule<-glyph_resource_cost(rule$cost);cost<-glyph_roll_total(cost_rule$dice)
    list(rule=rule,hours=hours,resource=cost_rule$resource,cost=cost,material=glyph_material_requirement(glyph_type,material),tool=rule$required_tools,duration_rounds=as.integer(rule$active_time_rounds),instability=rule$instability_damage,replenishment_dice=NA_character_,replenishment_multiplier=NA_integer_)
  }else if(glyph_type=="ward"){
    cost_rule<-glyph_resource_cost(rule$cost);list(rule=rule,hours=as.numeric(rule$crafting_hours),resource=cost_rule$resource,cost=glyph_roll_total(cost_rule$dice),material=as.character(material),tool="None",duration_rounds=NA_integer_,instability=NA_character_,replenishment_dice=NA_character_,replenishment_multiplier=NA_integer_)
  }else{
    list(rule=rule,hours=as.numeric(rule$crafting_hours),resource="sindre",cost=as.integer(rule$cost_multiplier)*glyph_roll_total(rule$cost_die),material="",tool="None",duration_rounds=NA_integer_,instability=NA_character_,replenishment_dice=rule$cost_die,replenishment_multiplier=as.integer(rule$cost_multiplier))
  }
}

glyph_spend_resource <- function(char,resource,amount) {
  amount<-max(0L,as.integer(amount));char<-validate_character(char)
  if(resource=="hp"){
    current<-as.integer(char$resources$hp$cur%||%0L);if(current<=amount)stop("That HP cost would reduce you to 0 HP. Glyph crafting cannot be started.");char$resources$hp$cur<-current-amount
  }else{
    current<-as.integer(char$resources$sindre$cur%||%0L);if(current<amount)stop(paste0("You need ",amount," Sindre but have ",current,"."));char$resources$sindre$cur<-current-amount
  }
  char
}

start_glyph_project <- function(character_id,glyph_type,rank,name,effect_description="",material=NULL,size_ft=NULL,enhancement_days=NULL,target_item_instance_id=NULL,damage_type=NULL) {
  glyph_type<-tolower(as.character(glyph_type));if(glyph_type=="enhancement")material<-NULL
  con<-get_db_connection();if(is.null(con))return(structure(list(),error="Database unavailable."));on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{
    blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(as.character(character_id)));if(!nrow(blob))stop("Character not found.")
    char<-validate_character(unserialize(blob$state_blob[[1L]]));char<-hydrate_character_inventory_relational(con,char,as.character(character_id));spec<-glyph_project_spec(char,glyph_type,rank,material,size_ft,enhancement_days);inv<-inventory_normalize(char$inventory$items)
    if(nzchar(spec$material)){if(is.na(glyph_inventory_index(inv,spec$material)))stop(paste0("Required material missing: ",spec$material,"."));inv<-glyph_consume_inventory_item(inv,spec$material)}
    if(!identical(tolower(spec$tool),"none")&&is.na(glyph_inventory_index(inv,spec$tool)))stop(paste0("Required tool missing: ",spec$tool," (tools are not consumed)."))
    if(tolower(glyph_type)=="enhancement"&&!nzchar(as.character(target_item_instance_id%||%"")))stop("Choose an owned item to enhance.")
    target_index<-match(as.character(target_item_instance_id),inv$id)
    if(tolower(glyph_type)=="enhancement"&&is.na(target_index))stop("That enhancement target is no longer in your inventory.")
    if(tolower(glyph_type)=="enhancement"&&!identical(as.character(inv$type[[target_index]]),"weapon"))stop("Weapon enhancements must target a weapon.")
    damage_type<-tools::toTitleCase(tolower(as.character(damage_type%||%"Fire")));if(glyph_type%in%c("rune","enhancement")&&!damage_type%in%GLYPH_DAMAGE_TYPES)stop("Choose a valid magical damage type.")
    identity<-glyph_default_identity(glyph_type,rank,damage_type,material,if(!is.na(target_index))inv$name[[target_index]]else NULL);if(!nzchar(trimws(as.character(name%||%""))))name<-identity$name;if(!nzchar(trimws(as.character(effect_description%||%""))))effect_description<-identity$description
    char$inventory$items<-inv;char<-glyph_spend_resource(char,spec$resource,spec$cost);rule<-spec$rule
    row<-DBI::dbGetQuery(con,paste(
      "INSERT INTO character_glyphs(character_id,glyph_type,rank,name,effect_description,material,target_item_instance_id,size_ft,enhancement_days,crafting_hours_required,resource_type,resource_cost,arcane_score,active_duration_rounds,instability_damage,instability_radius_ft,replenishment_dice,replenishment_multiplier,metadata)",
      "VALUES($1,$2,$3,$4,$5,NULLIF($6,''),NULLIF($7,''),$8,$9,$10,$11,$12,$13,$14,NULLIF($15,''),$16,NULLIF($17,''),$18,$19::jsonb) RETURNING *"
    ),params=list(as.character(character_id),tolower(glyph_type),as.character(rank),trimws(as.character(name)),as.character(effect_description),as.character(material%||%""),as.character(target_item_instance_id%||%""),suppressWarnings(as.numeric(size_ft%||%NA)),suppressWarnings(as.integer(enhancement_days%||%NA)),as.numeric(spec$hours),spec$resource,as.integer(spec$cost),as.integer(rule$arcane_score),spec$duration_rounds,as.character(spec$instability%||%""),as.integer(rule$instability_radius_ft%||%NA),as.character(spec$replenishment_dice%||%""),spec$replenishment_multiplier,enemy_json(list(rule=rule,required_material=spec$material,required_tool=spec$tool,damage_type=damage_type,damage=if(glyph_type=="rune")unname(RUNE_DAMAGE_BY_RANK[[rank]])else if(glyph_type=="enhancement")rule$damage else NULL,area_ft=if(glyph_type=="rune")unname(RUNE_RADIUS_BY_RANK[[rank]])else NULL))))
    DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,char_name=$2,updated_at=now() WHERE id::text=$3",params=list(list(serialize(char,NULL)),char$meta$name,as.character(character_id)));sync_character_inventory_relational(con,char,as.character(character_id));list(glyph=row[1,,drop=FALSE],character=char,cost=spec$cost,resource=spec$resource,consumed_material=spec$material)
  }),error=function(e){message("start_glyph_project failed: ",e$message);structure(list(),error=e$message)})
}

get_character_glyphs <- function(character_id) {
  con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbGetQuery(con,"SELECT * FROM character_glyphs WHERE character_id::text=$1 ORDER BY CASE status WHEN 'crafting' THEN 0 WHEN 'ready' THEN 1 WHEN 'active' THEN 2 ELSE 3 END,created_at DESC",params=list(as.character(character_id))),error=function(e){message("get_character_glyphs failed: ",e$message);data.frame()})
}

sync_active_weapon_enhancements <- function(character_id) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{
    glyphs<-DBI::dbGetQuery(con,"SELECT * FROM character_glyphs WHERE character_id::text=$1 AND glyph_type='enhancement' AND status='active'",params=list(as.character(character_id)))
    blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(as.character(character_id)));if(!nrow(blob))return(NULL);char<-validate_character(unserialize(blob$state_blob[[1L]]));char<-hydrate_character_inventory_relational(con,char,as.character(character_id));before<-serialize(char$inventory$items,NULL);char<-normalize_weapon_enchantments(char);inv<-inventory_normalize(char$inventory$items);changed<-!identical(before,serialize(inv,NULL))
    for(i in seq_len(nrow(glyphs))){g<-glyphs[i,,drop=FALSE];idx<-match(as.character(g$target_item_instance_id[[1L]]),inv$id);if(is.na(idx))next;m<-inv$meta[[idx]]%||%list();details<-enemy_db_json(g$metadata[[1L]],list());damage<-as.character(details$damage%||%ENHANCEMENT_RULES[[as.character(g$rank[[1L]])]]$damage%||%"1d4");damage_type<-as.character(details$damage_type%||%if(grepl("fire|flame",paste(g$name[[1L]],g$effect_description[[1L]]),ignore.case=TRUE))"Fire"else"Fire");until<-as.integer(g$active_until_day[[1L]]%||%char$meta$day%||%1L);rank<-as.character(g$rank[[1L]]);days<-as.integer(g$enhancement_days[[1L]]%||%3L);if(!identical(m$glyph_id,as.integer(g$id[[1L]]))||!identical(m$glyph_damage,damage)||!identical(m$glyph_damage_type,damage_type)||!identical(m$glyph_active_until_day,until)||!identical(m$glyph_rank,rank)||!identical(m$glyph_replenish_days,days)){m$glyph_id<-as.integer(g$id[[1L]]);m$glyph_name<-as.character(g$name[[1L]]);m$glyph_rank<-rank;m$glyph_damage<-damage;m$glyph_damage_type<-damage_type;m$glyph_replenish_days<-days;m$glyph_active_until_day<-until;inv$meta[[idx]]<-m;changed<-TRUE}}
    if(!changed)return(NULL);char$inventory$items<-inv;DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,updated_at=now() WHERE id::text=$2",params=list(list(serialize(char,NULL)),as.character(character_id)));sync_character_inventory_relational(con,char,as.character(character_id));char
  }),error=function(e){message("sync_active_weapon_enhancements failed: ",e$message);NULL})
}

weapon_enchantment_quote <- function(char,weapon_id) {
  char<-normalize_weapon_enchantments(char);inv<-inventory_normalize(char$inventory$items);idx<-match(as.character(weapon_id),inv$id);if(is.na(idx))stop("That enchanted weapon is no longer in inventory.");m<-inv$meta[[idx]]%||%list();rank<-as.character(m$glyph_rank%||%"Minor");if(!rank%in%names(ENHANCEMENT_RULES))rank<-"Minor";days<-max(1L,as.integer(m$glyph_replenish_days%||%3L));dice<-ENHANCEMENT_RULES[[rank]]$cost_die;list(weapon_id=as.character(weapon_id),weapon_name=inv$name[[idx]],rank=rank,days=days,dice=dice,cost=days*glyph_roll_total(dice),damage=as.character(m$glyph_damage),damage_type=as.character(m$glyph_damage_type),current_sindre=as.integer(char$resources$sindre$cur%||%0L))
}

replenish_weapon_enchantment <- function(character_id,weapon_id,cost_override=NULL) {
  con<-get_db_connection();if(is.null(con))return(structure(list(),error="Database unavailable."));on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(as.character(character_id)));char<-validate_character(unserialize(blob$state_blob[[1L]]));char<-hydrate_character_inventory_relational(con,char,as.character(character_id));char<-normalize_weapon_enchantments(char);inv<-inventory_normalize(char$inventory$items);idx<-match(as.character(weapon_id),inv$id);if(is.na(idx))stop("That enchanted weapon is no longer in inventory.");m<-inv$meta[[idx]]%||%list();if(!nzchar(as.character(m$glyph_damage%||%"")))stop("That weapon has no magical damage enhancement.");rank<-as.character(m$glyph_rank%||%"Minor");if(!rank%in%names(ENHANCEMENT_RULES))rank<-"Minor";days<-max(1L,as.integer(m$glyph_replenish_days%||%3L));cost<-if(is.null(cost_override))days*glyph_roll_total(ENHANCEMENT_RULES[[rank]]$cost_die)else max(0L,as.integer(cost_override));char<-glyph_spend_resource(char,"sindre",cost);m$glyph_active_until_day<-as.integer(char$meta$day%||%1L)+days;inv$meta[[idx]]<-m;char$inventory$items<-inv;DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,updated_at=now() WHERE id::text=$2",params=list(list(serialize(char,NULL)),as.character(character_id)));sync_character_inventory_relational(con,char,as.character(character_id));list(character=char,cost=cost,weapon_name=inv$name[[idx]],active_until_day=m$glyph_active_until_day)}),error=function(e)structure(list(),error=e$message))
}

work_glyph_project <- function(character_id,glyph_id,hours) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE);hours<-max(0,as.numeric(hours%||%0))
  tryCatch(DBI::dbWithTransaction(con,{
    glyph<-DBI::dbGetQuery(con,"SELECT * FROM character_glyphs WHERE id=$1 AND character_id::text=$2 AND status='crafting' FOR UPDATE",params=list(as.integer(glyph_id),as.character(character_id)));if(!nrow(glyph))stop("That crafting project is no longer available.")
    complete<-as.numeric(glyph$crafting_hours_completed[[1L]])+hours>=as.numeric(glyph$crafting_hours_required[[1L]]);status<-if(complete)if(glyph$glyph_type[[1L]]=="rune")"ready"else"active"else"crafting";until_day<-NA_integer_
    char<-NULL
    if(complete&&glyph$glyph_type[[1L]]=="enhancement"){
      blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(as.character(character_id)));char<-validate_character(unserialize(blob$state_blob[[1L]]));char<-hydrate_character_inventory_relational(con,char,as.character(character_id));until_day<-as.integer(char$meta$day%||%1L)+as.integer(glyph$enhancement_days[[1L]]);inv<-inventory_normalize(char$inventory$items);idx<-match(as.character(glyph$target_item_instance_id[[1L]]),inv$id);if(is.na(idx))stop("The weapon being enhanced is no longer in your inventory.");m<-inv$meta[[idx]]%||%list();details<-enemy_db_json(glyph$metadata[[1L]],list());m$glyph_id<-as.integer(glyph$id[[1L]]);m$glyph_name<-as.character(glyph$name[[1L]]);m$glyph_rank<-as.character(glyph$rank[[1L]]);m$glyph_damage<-as.character(details$damage%||%"1d4");m$glyph_damage_type<-as.character(details$damage_type%||%"Fire");m$glyph_replenish_days<-as.integer(glyph$enhancement_days[[1L]]);m$glyph_active_until_day<-until_day;inv$meta[[idx]]<-m;char$inventory$items<-inv
    }
    row<-DBI::dbGetQuery(con,"UPDATE character_glyphs SET crafting_hours_completed=LEAST(crafting_hours_required,crafting_hours_completed+$3),status=$4,completed_at=CASE WHEN $5 THEN now() ELSE completed_at END,activated_at=CASE WHEN $4='active' THEN now() ELSE activated_at END,active_until_day=COALESCE($6::integer,active_until_day),updated_at=now() WHERE id=$1 AND character_id::text=$2 RETURNING *",params=list(as.integer(glyph_id),as.character(character_id),hours,status,complete,until_day))[1,,drop=FALSE]
    if(complete&&glyph$glyph_type[[1L]]=="rune"){
      blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(as.character(character_id)));char<-validate_character(unserialize(blob$state_blob[[1L]]));char<-hydrate_character_inventory_relational(con,char,as.character(character_id));details<-enemy_db_json(glyph$metadata[[1L]],list());rune_item<-enemy_loot_to_inventory_row(list(name=as.character(glyph$name[[1L]]),type="glyph",desc=as.character(glyph$effect_description[[1L]]),value=0,weight=.1,qty=1,meta=list(glyph_id=as.integer(glyph$id[[1L]]),glyph_type="rune",rank=as.character(glyph$rank[[1L]]),damage=details$damage,damage_type=details$damage_type,area_ft=details$area_ft,duration_rounds=as.integer(glyph$active_duration_rounds[[1L]]),arcane_score=as.integer(glyph$arcane_score[[1L]]),status="ready")),paste0("crafted_rune_",glyph$id[[1L]]));if(!rune_item$id[[1L]]%in%char$inventory$items$id)char$inventory$items<-inventory_normalize(rbind(char$inventory$items,rune_item))
    }
    if(!is.null(char)){DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,updated_at=now() WHERE id::text=$2",params=list(list(serialize(char,NULL)),as.character(character_id)));sync_character_inventory_relational(con,char,as.character(character_id))}
    list(glyph=row,character=char)
  }),error=function(e){message("work_glyph_project failed: ",e$message);NULL})
}

replenish_enhancement <- function(character_id,glyph_id) {
  con<-get_db_connection();if(is.null(con))return(structure(list(),error="Database unavailable."));on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{
    glyph<-DBI::dbGetQuery(con,"SELECT * FROM character_glyphs WHERE id=$1 AND character_id::text=$2 AND glyph_type='enhancement' AND status IN ('active','depleted') FOR UPDATE",params=list(as.integer(glyph_id),as.character(character_id)));if(!nrow(glyph))stop("That enhancement cannot be replenished.");cost<-as.integer(glyph$replenishment_multiplier[[1L]])*glyph_roll_total(glyph$replenishment_dice[[1L]])
    blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(as.character(character_id)));char<-validate_character(unserialize(blob$state_blob[[1L]]));char<-glyph_spend_resource(char,"sindre",cost);until<-as.integer(char$meta$day%||%1L)+as.integer(glyph$enhancement_days[[1L]])
    inv<-inventory_normalize(char$inventory$items);idx<-match(as.character(glyph$target_item_instance_id[[1L]]),inv$id);if(is.na(idx))stop("The enhanced weapon is no longer in your inventory.");m<-inv$meta[[idx]]%||%list();m$glyph_active_until_day<-until;inv$meta[[idx]]<-m;char$inventory$items<-inv
    row<-DBI::dbGetQuery(con,"UPDATE character_glyphs SET status='active',active_until_day=$3,activated_at=now(),updated_at=now() WHERE id=$1 AND character_id::text=$2 RETURNING *",params=list(as.integer(glyph_id),as.character(character_id),until));DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,updated_at=now() WHERE id::text=$2",params=list(list(serialize(char,NULL)),as.character(character_id)));sync_character_inventory_relational(con,char,as.character(character_id));list(glyph=row[1,,drop=FALSE],character=char,cost=cost)
  }),error=function(e){message("replenish_enhancement failed: ",e$message);structure(list(),error=e$message)})
}

use_crafted_rune <- function(character_id,glyph_id,encounter_id=NULL,target_actor_id=NULL,natural_roll_override=NULL) {
  con<-get_db_connection();if(is.null(con))return(structure(list(),error="Database unavailable."));on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{
    glyph<-DBI::dbGetQuery(con,"SELECT * FROM character_glyphs WHERE id=$1 AND character_id::text=$2 AND glyph_type='rune' AND status='ready' FOR UPDATE",params=list(as.integer(glyph_id),as.character(character_id)));if(!nrow(glyph))stop("That rune is not ready to use.");roll<-as.integer(natural_roll_override%||%sample.int(20L,1L));unstable<-!is.null(encounter_id)&&roll==1L;round_now<-0L
    if(!is.null(encounter_id)){combat<-DBI::dbGetQuery(con,"SELECT round_number FROM combat_state WHERE encounter_id=$1",params=list(as.integer(encounter_id)));if(nrow(combat))round_now<-as.integer(combat$round_number[[1L]]%||%0L)}
    status<-if(unstable)"expended"else"active";until<-if(unstable)NA_integer_ else round_now+max(1L,as.integer(glyph$active_duration_rounds[[1L]]%||%1L))-1L
    details<-enemy_db_json(glyph$metadata[[1L]],list());rank<-as.character(glyph$rank[[1L]]);row<-DBI::dbGetQuery(con,"UPDATE character_glyphs SET status=$3,active_until_round=$4,activated_at=now(),updated_at=now(),metadata=metadata||$5::jsonb WHERE id=$1 AND character_id::text=$2 RETURNING *",params=list(as.integer(glyph_id),as.character(character_id),status,until,enemy_json(list(last_use_roll=roll,target_actor_id=target_actor_id,encounter_id=encounter_id,unstable=unstable))));blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(as.character(character_id)));char<-validate_character(unserialize(blob$state_blob[[1L]]));char<-hydrate_character_inventory_relational(con,char,as.character(character_id));inv<-inventory_normalize(char$inventory$items);inv<-inv[inv$id!=paste0("crafted_rune_",glyph_id),,drop=FALSE];char$inventory$items<-inv;DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,updated_at=now() WHERE id::text=$2",params=list(list(serialize(char,NULL)),as.character(character_id)));sync_character_inventory_relational(con,char,as.character(character_id));list(glyph=row[1,,drop=FALSE],character=char,roll=roll,unstable=unstable,instability_damage=as.character(glyph$instability_damage[[1L]]),radius_ft=as.integer(glyph$instability_radius_ft[[1L]]),damage=as.character(details$damage%||%unname(RUNE_DAMAGE_BY_RANK[[rank]])%||%"1d6"),damage_type=as.character(details$damage_type%||%"Fire"),area_ft=as.integer(details$area_ft%||%unname(RUNE_RADIUS_BY_RANK[[rank]])%||%5L))
  }),error=function(e){message("use_crafted_rune failed: ",e$message);structure(list(),error=e$message)})
}

release_crafted_rune <- function(character_id,glyph_id,encounter_id,target_actor_id,natural_roll_override=NULL) {
  actors<-get_encounter_actors(encounter_id);target<-actors[as.character(actors$actor_id)==as.character(target_actor_id),,drop=FALSE];if(!nrow(target))return(structure(list(),error="The selected rune target is no longer present."));release_crafted_rune_at(character_id,glyph_id,encounter_id,target$x[[1L]],target$y[[1L]],target_actor_id,natural_roll_override)
}

apply_rune_area_damage <- function(encounter_id,center_x,center_y,area_ft,damage,damage_type,source_character_id="") {
  actors<-get_encounter_actors(encounter_id);squares<-as.numeric(area_ft)/5;distance<-sqrt((as.numeric(actors$x)-as.numeric(center_x))^2+(as.numeric(actors$y)-as.numeric(center_y))^2);affected<-actors[!is.na(distance)&distance<=squares,,drop=FALSE];amount<-glyph_roll_total(damage);enc<-get_encounter(encounter_id);session_id<-as.integer(enc$session_id[[1L]]);applied<-character()
  for(i in seq_len(nrow(affected))){a<-affected[i,,drop=FALSE];ok<-if(a$actor_type[[1L]]=="enemy")!is.null(damage_encounter_enemy(encounter_id,a$actor_id[[1L]],amount))else if(a$actor_type[[1L]]=="player")!is.null(damage_session_player(session_id,a$actor_id[[1L]],amount))else FALSE;if(ok)applied<-c(applied,as.character(a$display_name[[1L]]))}
  list(damage_total=amount,affected=applied)
}

release_crafted_rune_at <- function(character_id,glyph_id,encounter_id,center_x,center_y,target_actor_id=NULL,natural_roll_override=NULL) {
  result<-use_crafted_rune(character_id,glyph_id,encounter_id,target_actor_id,natural_roll_override);if(length(result$error%||%character())||isTRUE(result$unstable))return(result);combat<-get_combat_state(encounter_id);round_now<-if(nrow(combat))as.integer(combat$round_number[[1L]]%||%1L)else 1L;con<-get_db_connection();if(!is.null(con)){on.exit(release_db_connection(con),add=TRUE);DBI::dbExecute(con,"UPDATE character_glyphs SET metadata=metadata||$2::jsonb,updated_at=now() WHERE id=$1",params=list(as.integer(glyph_id),enemy_json(list(center_x=as.integer(center_x),center_y=as.integer(center_y),last_damage_round=round_now))))};applied<-apply_rune_area_damage(encounter_id,center_x,center_y,result$area_ft,result$damage,result$damage_type,character_id);log_game_event(encounter_id,"rune_released","player",as.character(character_id),as.character(target_actor_id%||%""),list(damage=applied$damage_total,damage_expression=result$damage,damage_type=result$damage_type,area_ft=result$area_ft,center_x=center_x,center_y=center_y,affected=applied$affected));result$damage_total<-applied$damage_total;result$affected<-applied$affected;result
}

tick_active_rune_zones <- function(encounter_id,round_number) {
  con<-get_db_connection();if(is.null(con))return(list());on.exit(release_db_connection(con),add=TRUE);ticks<-tryCatch(DBI::dbWithTransaction(con,{DBI::dbExecute(con,"UPDATE character_glyphs SET status='expended',updated_at=now() WHERE glyph_type='rune' AND status='active' AND (metadata->>'encounter_id')::integer=$1 AND active_until_round<$2",params=list(as.integer(encounter_id),as.integer(round_number)));rows<-DBI::dbGetQuery(con,"SELECT * FROM character_glyphs WHERE glyph_type='rune' AND status='active' AND (metadata->>'encounter_id')::integer=$1 AND active_until_round>=$2 FOR UPDATE",params=list(as.integer(encounter_id),as.integer(round_number)));out<-list();for(i in seq_len(nrow(rows))){m<-enemy_db_json(rows$metadata[[i]],list());if(as.integer(m$last_damage_round%||%-1L)>=as.integer(round_number)||is.null(m$center_x)||is.null(m$center_y))next;DBI::dbExecute(con,"UPDATE character_glyphs SET metadata=metadata||$2::jsonb,updated_at=now() WHERE id=$1",params=list(as.integer(rows$id[[i]]),enemy_json(list(last_damage_round=as.integer(round_number)))));out[[length(out)+1L]]<-list(row=rows[i,,drop=FALSE],meta=m)};out}),error=function(e){message("tick_active_rune_zones failed: ",e$message);list()});for(x in ticks){m<-x$meta;applied<-apply_rune_area_damage(encounter_id,m$center_x,m$center_y,m$area_ft,m$damage,m$damage_type,as.character(x$row$character_id[[1L]]));log_game_event(encounter_id,"rune_zone_tick","player",as.character(x$row$character_id[[1L]]),payload=list(glyph_id=x$row$id[[1L]],round=round_number,damage=applied$damage_total,damage_type=m$damage_type,affected=applied$affected))};ticks
}

glyph_counter_outcome <- function(glyph_type,arcane_score,roll) {
  glyph_type<-tolower(as.character(glyph_type));score<-as.integer(arcane_score);roll<-as.integer(roll);success<-roll>=score
  list(success=success,unstable=glyph_type=="rune"&&success&&(roll-score)%in%c(1L,2L),outcome=if(!success)"holds"else if(glyph_type=="rune"&&(roll-score)%in%c(1L,2L))"unstable"else"broken")
}
