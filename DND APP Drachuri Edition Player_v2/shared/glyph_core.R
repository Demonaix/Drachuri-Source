glyph_character_level <- function(char) {
  classes<-char$build$classes%||%list()
  if(length(classes))sum(vapply(classes,function(x)as.integer(x$level%||%0L),integer(1)))else as.integer(char$build$level%||%1L)
}

glyph_arcana_bonus <- function(char) character_skill_modifier(char,"Arcana")

glyph_unlocked_ranks <- function(char) get_available_rune_types(glyph_character_level(char))

glyph_roll_total <- function(expr) as.integer(roll_dice_expr(as.character(expr))$total)

glyph_material_requirement <- function(glyph_type,material) {
  if(tolower(glyph_type)=="rune")paste(tools::toTitleCase(tolower(material)),"Rune Blank")else as.character(material%||%"")
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
  if(!rank%in%glyph_unlocked_ranks(char))stop(paste(rank,"glyphs unlock at level",RUNE_LEVEL_UNLOCKS[[rank]],"."))
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

start_glyph_project <- function(character_id,glyph_type,rank,name,effect_description="",material=NULL,size_ft=NULL,enhancement_days=NULL,target_item_instance_id=NULL) {
  if(!nzchar(trimws(as.character(name%||%""))))return(structure(list(),error="Give the glyph a name."))
  glyph_type<-tolower(as.character(glyph_type));if(glyph_type=="enhancement")material<-NULL
  con<-get_db_connection();if(is.null(con))return(structure(list(),error="Database unavailable."));on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{
    blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(as.character(character_id)));if(!nrow(blob))stop("Character not found.")
    char<-validate_character(unserialize(blob$state_blob[[1L]]));char<-hydrate_character_inventory_relational(con,char,as.character(character_id));spec<-glyph_project_spec(char,glyph_type,rank,material,size_ft,enhancement_days);inv<-inventory_normalize(char$inventory$items)
    if(nzchar(spec$material)){if(is.na(glyph_inventory_index(inv,spec$material)))stop(paste0("Required material missing: ",spec$material,"."));inv<-glyph_consume_inventory_item(inv,spec$material)}
    if(!identical(tolower(spec$tool),"none")&&is.na(glyph_inventory_index(inv,spec$tool)))stop(paste0("Required tool missing: ",spec$tool," (tools are not consumed)."))
    if(tolower(glyph_type)=="enhancement"&&!nzchar(as.character(target_item_instance_id%||%"")))stop("Choose an owned item to enhance.")
    if(tolower(glyph_type)=="enhancement"&&is.na(match(as.character(target_item_instance_id),inv$id)))stop("That enhancement target is no longer in your inventory.")
    char$inventory$items<-inv;char<-glyph_spend_resource(char,spec$resource,spec$cost);rule<-spec$rule
    row<-DBI::dbGetQuery(con,paste(
      "INSERT INTO character_glyphs(character_id,glyph_type,rank,name,effect_description,material,target_item_instance_id,size_ft,enhancement_days,crafting_hours_required,resource_type,resource_cost,arcane_score,active_duration_rounds,instability_damage,instability_radius_ft,replenishment_dice,replenishment_multiplier,metadata)",
      "VALUES($1,$2,$3,$4,$5,NULLIF($6,''),NULLIF($7,''),$8,$9,$10,$11,$12,$13,$14,NULLIF($15,''),$16,NULLIF($17,''),$18,$19::jsonb) RETURNING *"
    ),params=list(as.character(character_id),tolower(glyph_type),as.character(rank),trimws(as.character(name)),as.character(effect_description),as.character(material%||%""),as.character(target_item_instance_id%||%""),suppressWarnings(as.numeric(size_ft%||%NA)),suppressWarnings(as.integer(enhancement_days%||%NA)),as.numeric(spec$hours),spec$resource,as.integer(spec$cost),as.integer(rule$arcane_score),spec$duration_rounds,as.character(spec$instability%||%""),as.integer(rule$instability_radius_ft%||%NA),as.character(spec$replenishment_dice%||%""),spec$replenishment_multiplier,enemy_json(list(rule=rule,required_material=spec$material,required_tool=spec$tool))))
    DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,char_name=$2,updated_at=now() WHERE id::text=$3",params=list(list(serialize(char,NULL)),char$meta$name,as.character(character_id)));sync_character_inventory_relational(con,char,as.character(character_id));list(glyph=row[1,,drop=FALSE],character=char,cost=spec$cost,resource=spec$resource)
  }),error=function(e){message("start_glyph_project failed: ",e$message);structure(list(),error=e$message)})
}

get_character_glyphs <- function(character_id) {
  con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbGetQuery(con,"SELECT * FROM character_glyphs WHERE character_id=$1 ORDER BY CASE status WHEN 'crafting' THEN 0 WHEN 'ready' THEN 1 WHEN 'active' THEN 2 ELSE 3 END,created_at DESC",params=list(as.character(character_id))),error=function(e)data.frame())
}

work_glyph_project <- function(character_id,glyph_id,hours) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE);hours<-max(0,as.numeric(hours%||%0))
  tryCatch(DBI::dbWithTransaction(con,{
    glyph<-DBI::dbGetQuery(con,"SELECT * FROM character_glyphs WHERE id=$1 AND character_id=$2 AND status='crafting' FOR UPDATE",params=list(as.integer(glyph_id),as.character(character_id)));if(!nrow(glyph))stop("That crafting project is no longer available.")
    complete<-as.numeric(glyph$crafting_hours_completed[[1L]])+hours>=as.numeric(glyph$crafting_hours_required[[1L]]);status<-if(complete)if(glyph$glyph_type[[1L]]=="rune")"ready"else"active"else"crafting";until_day<-NA_integer_
    if(complete&&glyph$glyph_type[[1L]]=="enhancement"){blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1",params=list(as.character(character_id)));char<-validate_character(unserialize(blob$state_blob[[1L]]));until_day<-as.integer(char$meta$day%||%1L)+as.integer(glyph$enhancement_days[[1L]])}
    DBI::dbGetQuery(con,"UPDATE character_glyphs SET crafting_hours_completed=LEAST(crafting_hours_required,crafting_hours_completed+$3),status=$4,completed_at=CASE WHEN $5 THEN now() ELSE completed_at END,activated_at=CASE WHEN $4='active' THEN now() ELSE activated_at END,active_until_day=COALESCE($6::integer,active_until_day),updated_at=now() WHERE id=$1 AND character_id=$2 RETURNING *",params=list(as.integer(glyph_id),as.character(character_id),hours,status,complete,until_day))[1,,drop=FALSE]
  }),error=function(e){message("work_glyph_project failed: ",e$message);NULL})
}

replenish_enhancement <- function(character_id,glyph_id) {
  con<-get_db_connection();if(is.null(con))return(structure(list(),error="Database unavailable."));on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{
    glyph<-DBI::dbGetQuery(con,"SELECT * FROM character_glyphs WHERE id=$1 AND character_id=$2 AND glyph_type='enhancement' AND status IN ('active','depleted') FOR UPDATE",params=list(as.integer(glyph_id),as.character(character_id)));if(!nrow(glyph))stop("That enhancement cannot be replenished.");cost<-as.integer(glyph$replenishment_multiplier[[1L]])*glyph_roll_total(glyph$replenishment_dice[[1L]])
    blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(as.character(character_id)));char<-validate_character(unserialize(blob$state_blob[[1L]]));char<-glyph_spend_resource(char,"sindre",cost);until<-as.integer(char$meta$day%||%1L)+as.integer(glyph$enhancement_days[[1L]])
    row<-DBI::dbGetQuery(con,"UPDATE character_glyphs SET status='active',active_until_day=$3,activated_at=now(),updated_at=now() WHERE id=$1 AND character_id=$2 RETURNING *",params=list(as.integer(glyph_id),as.character(character_id),until));DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,updated_at=now() WHERE id::text=$2",params=list(list(serialize(char,NULL)),as.character(character_id)));list(glyph=row[1,,drop=FALSE],character=char,cost=cost)
  }),error=function(e){message("replenish_enhancement failed: ",e$message);structure(list(),error=e$message)})
}

use_crafted_rune <- function(character_id,glyph_id,encounter_id=NULL,target_actor_id=NULL,natural_roll_override=NULL) {
  con<-get_db_connection();if(is.null(con))return(structure(list(),error="Database unavailable."));on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{
    glyph<-DBI::dbGetQuery(con,"SELECT * FROM character_glyphs WHERE id=$1 AND character_id=$2 AND glyph_type='rune' AND status='ready' FOR UPDATE",params=list(as.integer(glyph_id),as.character(character_id)));if(!nrow(glyph))stop("That rune is not ready to use.");roll<-as.integer(natural_roll_override%||%sample.int(20L,1L));unstable<-!is.null(encounter_id)&&roll==1L;round_now<-0L
    if(!is.null(encounter_id)){combat<-DBI::dbGetQuery(con,"SELECT round_number FROM combat_state WHERE encounter_id=$1",params=list(as.integer(encounter_id)));if(nrow(combat))round_now<-as.integer(combat$round_number[[1L]]%||%0L)}
    status<-if(unstable)"expended"else"active";until<-if(unstable)NA_integer_ else round_now+as.integer(glyph$active_duration_rounds[[1L]]%||%1L)
    row<-DBI::dbGetQuery(con,"UPDATE character_glyphs SET status=$3,active_until_round=$4,activated_at=now(),updated_at=now(),metadata=metadata||$5::jsonb WHERE id=$1 AND character_id=$2 RETURNING *",params=list(as.integer(glyph_id),as.character(character_id),status,until,enemy_json(list(last_use_roll=roll,target_actor_id=target_actor_id,encounter_id=encounter_id,unstable=unstable))));list(glyph=row[1,,drop=FALSE],roll=roll,unstable=unstable,instability_damage=as.character(glyph$instability_damage[[1L]]),radius_ft=as.integer(glyph$instability_radius_ft[[1L]]))
  }),error=function(e){message("use_crafted_rune failed: ",e$message);structure(list(),error=e$message)})
}

glyph_counter_outcome <- function(glyph_type,arcane_score,roll) {
  glyph_type<-tolower(as.character(glyph_type));score<-as.integer(arcane_score);roll<-as.integer(roll);success<-roll>=score
  list(success=success,unstable=glyph_type=="rune"&&success&&(roll-score)%in%c(1L,2L),outcome=if(!success)"holds"else if(glyph_type=="rune"&&(roll-score)%in%c(1L,2L))"unstable"else"broken")
}
