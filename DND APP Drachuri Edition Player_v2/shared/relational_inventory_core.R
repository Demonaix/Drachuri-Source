# Compatibility layer for relational inventory rollout. Character blobs remain
# the UI-shaped rollback copy while every save is mirrored into typed tables.

relational_inventory_ready <- function(con) {
  isTRUE(tryCatch(DBI::dbGetQuery(
    con,
    "SELECT to_regclass('public.character_inventory_items') IS NOT NULL AS ready"
  )$ready[[1L]], error = function(e) FALSE))
}

inventory_json <- function(x) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return("{}")
  as.character(jsonlite::toJSON(x %||% list(), auto_unbox = TRUE, null = "null", na = "null"))
}

inventory_json_list <- function(x) {
  if (is.list(x) && !is.data.frame(x)) return(x)
  if (is.null(x) || !length(x) || is.na(x[[1L]]) || !nzchar(as.character(x[[1L]]))) return(list())
  tryCatch(jsonlite::fromJSON(as.character(x[[1L]]), simplifyVector = FALSE), error = function(e) list())
}

lookup_equipment_provenance <- function(material, build_quality, armour_type = NULL) {
  con <- get_db_connection(); if (is.null(con)) return(list()); on.exit(release_db_connection(con), add=TRUE)
  tryCatch({
    row <- DBI::dbGetQuery(con, paste(
      "SELECT m.id AS material_id,q.id AS condition_id,m.attack_bonus AS material_attack_bonus,",
      "m.damage_modifier AS material_damage_modifier,q.attack_bonus AS quality_attack_bonus,",
      "q.damage_modifier AS quality_damage_modifier,q.armour_modifier AS quality_armour_modifier,",
      "CASE lower($3) WHEN 'light' THEN m.light_armour_modifier WHEN 'medium' THEN m.medium_armour_modifier WHEN 'heavy' THEN m.heavy_armour_modifier ELSE 0 END AS material_armour_modifier",
      "FROM item_materials m CROSS JOIN item_conditions q WHERE lower(m.name)=lower($1) AND lower(q.name)=lower($2)"
    ),params=list(as.character(material),as.character(build_quality),as.character(armour_type%||%"")))
    if(!nrow(row))list() else as.list(row[1,,drop=FALSE])
  },error=function(e)list())
}

inventory_definition_kind <- function(type) {
  type <- tolower(trimws(as.character(type %||% "item")))
  if (type %in% c("weapon", "weapons")) return("weapon")
  if (type %in% c("armor", "armour", "shield")) return("armour")
  "item"
}

inventory_item_category <- function(item) {
  type <- tolower(trimws(as.character(item$type[[1L]] %||% "item")))
  meta <- if (is.list(item$meta[[1L]])) item$meta[[1L]] else list()
  explicit <- tolower(as.character(meta$category %||% ""))
  allowed <- c("mundane_loot", "food", "magical_item", "consumable", "crafting", "tool", "treasure", "quest")
  if (explicit %in% allowed) return(explicit)
  if (type %in% c("consumable", "potion", "food", "drink")) return("consumable")
  if (type %in% c("crafting", "material", "ingredient")) return("crafting")
  if (type %in% c("tool", "tools")) return("tool")
  if (type %in% c("treasure", "valuable")) return("treasure")
  if (type %in% c("quest", "quest_item")) return("quest")
  "mundane_loot"
}

inventory_definition_id <- function(con, kind, item) {
  table <- c(weapon = "weapons", armour = "armour", item = "items")[[kind]]
  name <- trimws(as.character(item$name[[1L]] %||% "Unnamed item"))
  found <- DBI::dbGetQuery(
    con, paste0("SELECT id FROM ", table, " WHERE lower(trim(name))=lower(trim($1)) LIMIT 1"),
    params = list(name)
  )
  if (nrow(found)) return(as.character(found$id[[1L]]))
  slug <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(iconv(name, to = "ASCII//TRANSLIT", sub = ""))))
  id <- paste0("party_", if (nzchar(slug)) substr(slug, 1L, 70L) else "unnamed")
  meta <- if (is.list(item$meta[[1L]])) item$meta[[1L]] else list()
  value <- suppressWarnings(as.numeric(item$value[[1L]] %||% 0)); if (is.na(value)) value <- 0
  weight <- suppressWarnings(as.numeric(item$weight[[1L]] %||% 0)); if (is.na(weight)) weight <- 0
  desc <- as.character(item$desc[[1L]] %||% "")
  if (kind == "weapon") {
    damage_type <- tolower(as.character(meta$dmg_type1 %||% meta$damage_type_1 %||% "other"))
    if (!damage_type %in% enemy_damage_types() && damage_type != "other") damage_type <- "other"
    damage_type_2 <- tolower(as.character(meta$dmg_type2 %||% meta$damage_type_2 %||% "other"))
    if (!damage_type_2 %in% enemy_damage_types() && damage_type_2 != "other") damage_type_2 <- "other"
    DBI::dbExecute(con, paste(
      "INSERT INTO weapons(id,name,description,value,weight,stat,advantage,to_hit_bonus,damage_1,damage_type_1,damage_2,damage_type_2,proficient)",
      "VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13) ON CONFLICT(id) DO NOTHING"
    ), params = list(id, name, desc, value, weight,
      tolower(as.character(meta$stat %||% "str")), as.character(meta$adv %||% "Normal"),
      as.integer(meta$to_hit_bonus %||% 0L), as.character(meta$damage1 %||% "1d4"), damage_type,
      as.character(meta$damage2 %||% ""), damage_type_2,
      isTRUE(meta$proficient %||% TRUE)))
  } else if (kind == "armour") {
    armour_type <- as.character(meta$type %||% meta$armour_type %||% "Light")
    if (!armour_type %in% c("Light", "Medium", "Heavy", "Shield", "Unarmoured")) armour_type <- "Light"
    DBI::dbExecute(con, paste(
      "INSERT INTO armour(id,name,description,value,weight,base_ac,armour_type,max_dex_bonus,proficient)",
      "VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9) ON CONFLICT(id) DO NOTHING"
    ), params = list(id, name, desc, value, weight, as.integer(meta$base_ac %||% 10L),
      armour_type, as.integer(meta$custom_max_dex %||% meta$max_dex_bonus %||% 0L),
      isTRUE(meta$proficient %||% TRUE)))
  } else {
    DBI::dbExecute(con, paste(
      "INSERT INTO items(id,name,item_type,description,value,weight,effect,effect_amount,category,ration_value,shelf_life_days)",
      "VALUES($1,$2,'item',$3,$4,$5,$6,$7,$8,$9,$10) ON CONFLICT(id) DO NOTHING"
    ), params = list(id, name, desc, value, weight, as.character(meta$effect %||% ""),
      as.character(meta$effect_amount %||% ""), inventory_item_category(item),as.integer(meta$ration_value%||%0L),as.integer(meta$shelf_life_days%||%0L)))
  }
  id
}

save_control_catalogue_definition <- function(entry) {
  con <- get_db_connection(); if (is.null(con)) return(FALSE); on.exit(release_db_connection(con), add = TRUE)
  tryCatch({
    kind <- inventory_definition_kind(entry$type %||% "item"); meta <- entry$meta %||% list()
    id <- as.character(entry$id); name <- as.character(entry$name); desc <- as.character(entry$desc %||% "")
    value <- as.numeric(entry$value %||% 0); weight <- as.numeric(entry$weight %||% 0)
    pools <- as.character(entry$pools %||% character())
    if (kind == "weapon") {
      type1 <- tolower(as.character(meta$dmg_type1 %||% "other")); type2 <- tolower(as.character(meta$dmg_type2 %||% "other"))
      valid <- c(enemy_damage_types(), "other"); if (!type1 %in% valid) type1 <- "other"; if (!type2 %in% valid) type2 <- "other"
      DBI::dbExecute(con, paste(
        "INSERT INTO weapons(id,name,description,value,weight,stat,advantage,to_hit_bonus,damage_1,damage_type_1,damage_2,damage_type_2,proficient,pools,updated_at)",
        "VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14::text[],now()) ON CONFLICT(id) DO UPDATE SET name=EXCLUDED.name,description=EXCLUDED.description,value=EXCLUDED.value,weight=EXCLUDED.weight,stat=EXCLUDED.stat,advantage=EXCLUDED.advantage,to_hit_bonus=EXCLUDED.to_hit_bonus,damage_1=EXCLUDED.damage_1,damage_type_1=EXCLUDED.damage_type_1,damage_2=EXCLUDED.damage_2,damage_type_2=EXCLUDED.damage_type_2,proficient=EXCLUDED.proficient,pools=EXCLUDED.pools,updated_at=now()"
      ), params=list(id,name,desc,value,weight,tolower(as.character(meta$stat%||%"str")),as.character(meta$adv%||%"Normal"),as.integer(meta$to_hit_bonus%||%0L),as.character(meta$damage1%||%"1d4"),type1,as.character(meta$damage2%||%""),type2,isTRUE(meta$proficient%||%TRUE),enemy_pg_array(pools)))
    } else if (kind == "armour") {
      armour_type <- as.character(meta$type %||% "Light"); if (!armour_type %in% c("Light","Medium","Heavy","Shield","Unarmoured")) armour_type <- "Light"
      DBI::dbExecute(con, paste(
        "INSERT INTO armour(id,name,description,value,weight,base_ac,armour_type,max_dex_bonus,proficient,pools,updated_at)",
        "VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10::text[],now()) ON CONFLICT(id) DO UPDATE SET name=EXCLUDED.name,description=EXCLUDED.description,value=EXCLUDED.value,weight=EXCLUDED.weight,base_ac=EXCLUDED.base_ac,armour_type=EXCLUDED.armour_type,max_dex_bonus=EXCLUDED.max_dex_bonus,proficient=EXCLUDED.proficient,pools=EXCLUDED.pools,updated_at=now()"
      ), params=list(id,name,desc,value,weight,as.integer(meta$base_ac%||%10L),armour_type,as.integer(meta$custom_max_dex%||%0L),isTRUE(meta$proficient%||%TRUE),enemy_pg_array(pools)))
    } else {
      DBI::dbExecute(con, paste(
        "INSERT INTO items(id,name,item_type,description,value,weight,effect,effect_amount,pools,category,ration_value,shelf_life_days,updated_at)",
        "VALUES($1,$2,'item',$3,$4,$5,$6,$7,$8::text[],$9,$10,$11,now()) ON CONFLICT(id) DO UPDATE SET name=EXCLUDED.name,description=EXCLUDED.description,value=EXCLUDED.value,weight=EXCLUDED.weight,effect=EXCLUDED.effect,effect_amount=EXCLUDED.effect_amount,pools=EXCLUDED.pools,category=EXCLUDED.category,ration_value=EXCLUDED.ration_value,shelf_life_days=EXCLUDED.shelf_life_days,updated_at=now()"
      ), params=list(id,name,desc,value,weight,as.character(meta$effect%||%""),as.character(meta$effect_amount%||%""),enemy_pg_array(pools),inventory_item_category(data.frame(type=entry$type%||%"item",meta=I(list(meta)))),as.integer(meta$ration_value%||%0L),as.integer(meta$shelf_life_days%||%0L)))
    }
    TRUE
  }, error=function(e){message("save_control_catalogue_definition failed: ",e$message);FALSE})
}

get_control_catalogue_definitions <- function() {
  con<-get_db_connection();if(is.null(con))return(list());on.exit(release_db_connection(con),add=TRUE)
  tryCatch({
    weapons<-DBI::dbGetQuery(con,"SELECT * FROM weapons ORDER BY lower(name)");armour<-DBI::dbGetQuery(con,"SELECT * FROM armour ORDER BY lower(name)");items<-DBI::dbGetQuery(con,"SELECT * FROM items ORDER BY lower(name)")
    out<-list()
    if(nrow(weapons))for(i in seq_len(nrow(weapons))){x<-weapons[i,,drop=FALSE];magic<-enemy_db_json(x$magical_properties[[1L]]%||%NULL,list());meta<-c(list(stat=as.character(x$stat[[1L]]%||%"str"),adv=as.character(x$advantage[[1L]]%||%"Normal"),to_hit_bonus=as.integer(x$to_hit_bonus[[1L]]%||%0L),damage1=as.character(x$damage_1[[1L]]%||%"1d4"),dmg_type1=as.character(x$damage_type_1[[1L]]%||%"other"),damage2=as.character(x$damage_2[[1L]]%||%""),dmg_type2=as.character(x$damage_type_2[[1L]]%||%"other"),proficient=isTRUE(x$proficient[[1L]])),magic);out[[length(out)+1L]]<-list(id=as.character(x$id[[1L]]),name=as.character(x$name[[1L]]),type="weapon",desc=as.character(x$description[[1L]]%||%""),value=as.numeric(x$value[[1L]]%||%0),weight=as.numeric(x$weight[[1L]]%||%0),qty=1,pools=enemy_db_values(x$pools[[1L]]%||%character()),meta=meta)}
    if(nrow(armour))for(i in seq_len(nrow(armour))){x<-armour[i,,drop=FALSE];magic<-enemy_db_json(x$magical_properties[[1L]]%||%NULL,list());meta<-c(list(base_ac=as.integer(x$base_ac[[1L]]%||%10L),type=as.character(x$armour_type[[1L]]%||%"Light"),custom_max_dex=as.integer(x$max_dex_bonus[[1L]]%||%0L),proficient=isTRUE(x$proficient[[1L]])),magic);out[[length(out)+1L]]<-list(id=as.character(x$id[[1L]]),name=as.character(x$name[[1L]]),type="armor",desc=as.character(x$description[[1L]]%||%""),value=as.numeric(x$value[[1L]]%||%0),weight=as.numeric(x$weight[[1L]]%||%0),qty=1,pools=enemy_db_values(x$pools[[1L]]%||%character()),meta=meta)}
    if(nrow(items))for(i in seq_len(nrow(items))){x<-items[i,,drop=FALSE];category<-as.character(x$category[[1L]]%||%"mundane_loot");magic<-enemy_db_json(x$magical_properties[[1L]]%||%NULL,list());meta<-c(list(category=category,effect=as.character(x$effect[[1L]]%||%""),effect_amount=as.character(x$effect_amount[[1L]]%||%""),ration_value=as.integer(x$ration_value[[1L]]%||%0L),shelf_life_days=as.integer(x$shelf_life_days[[1L]]%||%0L)),magic);out[[length(out)+1L]]<-list(id=as.character(x$id[[1L]]),name=as.character(x$name[[1L]]),type=if(category%in%c("food","consumable"))"consumable"else"item",desc=as.character(x$description[[1L]]%||%""),value=as.numeric(x$value[[1L]]%||%0),weight=as.numeric(x$weight[[1L]]%||%0),qty=1,pools=enemy_db_values(x$pools[[1L]]%||%character()),meta=meta)}
    out
  },error=function(e){message("get_control_catalogue_definitions failed: ",e$message);list()})
}

sync_character_inventory_relational <- function(con, char, character_id) {
  if (!relational_inventory_ready(con)) return(invisible(FALSE))
  char <- validate_character(char)
  inv <- inventory_normalize(char$inventory$items)
  DBI::dbExecute(con, paste(
    "INSERT INTO character_wallets(character_id,gold,updated_at) VALUES($1,$2,now())",
    "ON CONFLICT(character_id) DO UPDATE SET gold=EXCLUDED.gold,updated_at=now()"
  ), params = list(as.character(character_id), as.numeric(char$inventory$gold %||% 0)))
  keep <- character()
  for (i in seq_len(nrow(inv))) {
    item <- inv[i, , drop = FALSE]
    instance_id <- as.character(item$id[[1L]])
    keep <- c(keep, instance_id)
    existing <- DBI::dbGetQuery(con, paste(
      "SELECT weapon_id,armour_id,item_id FROM character_inventory_items",
      "WHERE character_id=$1 AND instance_id=$2"
    ), params = list(as.character(character_id), instance_id))
    kind <- inventory_definition_kind(item$type[[1L]])
    definition_id <- if (nrow(existing)) {
      current <- existing[[paste0(kind, "_id")]][[1L]]
      if (!is.na(current) && nzchar(as.character(current))) as.character(current) else inventory_definition_id(con, kind, item)
    } else inventory_definition_id(con, kind, item)
    refs <- list(
      if (kind == "weapon") definition_id else NA_character_,
      if (kind == "armour") definition_id else NA_character_,
      if (kind == "item") definition_id else NA_character_
    )
    meta <- item$meta[[1L]]; if (!is.list(meta)) meta <- list()
    meta$legacy_type <- as.character(item$type[[1L]])
    material_id <- suppressWarnings(as.numeric(meta$material_id %||% NA))
    if (is.na(material_id) && nzchar(as.character(meta$material %||% ""))) {
      found <- DBI::dbGetQuery(con,"SELECT id FROM item_materials WHERE lower(name)=lower($1) LIMIT 1",params=list(as.character(meta$material)))
      if(nrow(found)) material_id <- as.numeric(found$id[[1L]])
    }
    condition_id <- suppressWarnings(as.numeric(meta$condition_id %||% NA))
    if (is.na(condition_id) && nzchar(as.character(meta$build_quality %||% ""))) {
      found <- DBI::dbGetQuery(con,"SELECT id FROM item_conditions WHERE lower(name)=lower($1) LIMIT 1",params=list(as.character(meta$build_quality)))
      if(nrow(found)) condition_id <- as.numeric(found$id[[1L]])
    }
    DBI::dbExecute(con, paste(
      "INSERT INTO character_inventory_items(character_id,instance_id,weapon_id,armour_id,item_id,quantity,equipped,in_bag,material_id,condition_id,custom_name,custom_description,custom_value,custom_weight,properties,source,updated_at)",
      "VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15::jsonb,'dual_write',now())",
      "ON CONFLICT(character_id,instance_id) DO UPDATE SET weapon_id=EXCLUDED.weapon_id,armour_id=EXCLUDED.armour_id,item_id=EXCLUDED.item_id,quantity=EXCLUDED.quantity,equipped=EXCLUDED.equipped,in_bag=EXCLUDED.in_bag,material_id=COALESCE(EXCLUDED.material_id,character_inventory_items.material_id),condition_id=COALESCE(EXCLUDED.condition_id,character_inventory_items.condition_id),custom_name=EXCLUDED.custom_name,custom_description=EXCLUDED.custom_description,custom_value=EXCLUDED.custom_value,custom_weight=EXCLUDED.custom_weight,properties=EXCLUDED.properties,source='dual_write',updated_at=now()"
    ), params = c(list(as.character(character_id), instance_id), refs, list(
      as.numeric(item$qty[[1L]]), isTRUE(item$equipped[[1L]]), isTRUE(item$in_bag[[1L]]),
      material_id, condition_id,
      as.character(item$name[[1L]]), as.character(item$desc[[1L]]), as.numeric(item$value[[1L]]),
      as.numeric(item$weight[[1L]]), inventory_json(meta)
    )))
  }
  existing_ids <- DBI::dbGetQuery(con,
    "SELECT instance_id FROM character_inventory_items WHERE character_id=$1",
    params = list(as.character(character_id)))$instance_id
  remove_ids <- setdiff(as.character(existing_ids), keep)
  for (id in remove_ids) DBI::dbExecute(con,
    "DELETE FROM character_inventory_items WHERE character_id=$1 AND instance_id=$2",
    params = list(as.character(character_id), id))
  invisible(TRUE)
}

hydrate_character_inventory_relational <- function(con, char, character_id) {
  if (!relational_inventory_ready(con)) return(char)
  wallet <- DBI::dbGetQuery(con, "SELECT gold FROM character_wallets WHERE character_id=$1", params = list(as.character(character_id)))
  if (nrow(wallet)) char$inventory$gold <- as.numeric(wallet$gold[[1L]])
  rows <- DBI::dbGetQuery(con, paste(
    "SELECT ci.instance_id,ci.material_id,ci.condition_id,ci.properties,",
    "m.name AS material_name,m.is_iron,m.is_wood,m.attack_bonus AS material_attack_bonus,m.damage_modifier AS material_damage_modifier,",
    "CASE lower(a.armour_type) WHEN 'light' THEN m.light_armour_modifier WHEN 'medium' THEN m.medium_armour_modifier WHEN 'heavy' THEN m.heavy_armour_modifier ELSE 0 END AS material_armour_modifier,",
    "q.name AS quality_name,q.attack_bonus AS quality_attack_bonus,q.damage_modifier AS quality_damage_modifier,q.armour_modifier AS quality_armour_modifier",
    "FROM character_inventory_items ci LEFT JOIN item_materials m ON m.id=ci.material_id LEFT JOIN armour a ON a.id=ci.armour_id",
    "LEFT JOIN item_conditions q ON q.id=ci.condition_id WHERE ci.character_id=$1"
  ), params = list(as.character(character_id)))
  inv <- inventory_normalize(char$inventory$items)
  for (i in seq_len(nrow(rows))) {
    index <- match(as.character(rows$instance_id[[i]]), inv$id)
    if (is.na(index)) next
    meta <- inv$meta[[index]]; if (!is.list(meta)) meta <- list()
    if (!is.na(rows$material_id[[i]])) {
      meta$material_id <- as.numeric(rows$material_id[[i]])
      meta$material <- as.character(rows$material_name[[i]])
      meta$is_iron <- isTRUE(rows$is_iron[[i]]); meta$is_wood <- isTRUE(rows$is_wood[[i]])
      meta$material_attack_bonus <- as.numeric(rows$material_attack_bonus[[i]] %||% 0)
      meta$material_damage_modifier <- as.numeric(rows$material_damage_modifier[[i]] %||% 0)
      meta$material_armour_modifier <- as.numeric(rows$material_armour_modifier[[i]] %||% 0)
    }
    if (!is.na(rows$condition_id[[i]])) {
      meta$condition_id <- as.numeric(rows$condition_id[[i]])
      meta$build_quality <- as.character(rows$quality_name[[i]])
      meta$quality_attack_bonus <- as.numeric(rows$quality_attack_bonus[[i]] %||% 0)
      meta$quality_damage_modifier <- as.numeric(rows$quality_damage_modifier[[i]] %||% 0)
      meta$quality_armour_modifier <- as.numeric(rows$quality_armour_modifier[[i]] %||% 0)
    }
    inv$meta[[index]] <- meta
  }
  char$inventory$items <- inv
  char
}

pending_equipment_assignments <- function(character_id) {
  con <- get_db_connection(); if (is.null(con)) return(data.frame()); on.exit(release_db_connection(con), add = TRUE)
  if (!relational_inventory_ready(con)) return(data.frame())
  tryCatch(DBI::dbGetQuery(con, paste(
    "SELECT ci.instance_id,COALESCE(ci.custom_name,w.name,a.name) AS equipment_name,",
    "CASE WHEN ci.weapon_id IS NOT NULL THEN 'weapon' ELSE 'armour' END AS equipment_kind,",
    "ci.material_id,ci.condition_id,COALESCE(w.default_material_id,a.default_material_id) AS default_material_id,",
    "m.name AS default_material FROM character_inventory_items ci LEFT JOIN weapons w ON w.id=ci.weapon_id",
    "LEFT JOIN armour a ON a.id=ci.armour_id LEFT JOIN item_materials m ON m.id=COALESCE(w.default_material_id,a.default_material_id)",
    "WHERE ci.character_id=$1 AND (ci.weapon_id IS NOT NULL OR ci.armour_id IS NOT NULL)",
    "AND ci.needs_provenance_roll AND (ci.material_id IS NULL OR ci.condition_id IS NULL) ORDER BY equipment_name,ci.instance_id"
  ), params = list(as.character(character_id))), error = function(e) data.frame())
}

weighted_equipment_choice <- function(rows) {
  weights <- suppressWarnings(as.numeric(rows$drop_rate)); weights[is.na(weights) | weights < 0] <- 0
  if (!sum(weights)) weights[] <- 1
  index <- sample.int(nrow(rows), 1L, prob = weights)
  list(row = rows[index, , drop = FALSE], roll = stats::runif(1), weights = stats::setNames(weights, rows$name))
}

equipment_material_is_eligible <- function(row, enemy_type = "", characteristics = character()) {
  rule_values <- function(x) {
    x <- as.character(x %||% character())
    if (length(x) == 1L && grepl("^\\{.*\\}$", x)) {
      x <- sub("^\\{", "", sub("\\}$", "", x))
      if (!nzchar(x)) return(character())
      x <- strsplit(x, ",", fixed=TRUE)[[1L]]
    }
    trimws(gsub('^"|"$', '', x))
  }
  excluded <- tolower(rule_values(row$excluded_enemy_types[[1L]] %||% character()))
  required <- tolower(rule_values(row$required_characteristics[[1L]] %||% character()))
  type_ok <- !tolower(as.character(enemy_type %||% "")) %in% excluded
  characteristic_ok <- !length(required) || all(required %in% tolower(as.character(characteristics)))
  type_ok && characteristic_ok
}

roll_loot_equipment_provenance <- function(loot, enemy_type = "", characteristics = character()) {
  if (!length(loot)) return(loot)
  con <- get_db_connection(); if (is.null(con)) return(loot); on.exit(release_db_connection(con), add = TRUE)
  if (!relational_inventory_ready(con)) return(loot)
  materials <- DBI::dbGetQuery(con, "SELECT * FROM item_materials ORDER BY id")
  qualities <- DBI::dbGetQuery(con, "SELECT * FROM item_conditions WHERE COALESCE(drop_rate,0)>0 ORDER BY id")
  for (i in seq_along(loot)) {
    entry <- loot[[i]]
    kind <- inventory_definition_kind(entry$type %||% "item")
    if (!kind %in% c("weapon", "armour")) next
    meta <- entry$meta %||% list(); if (!is.list(meta)) meta <- list()
    if (isTRUE(meta$provenance_assigned) && !is.null(meta$material_id) && !is.null(meta$condition_id)) next
    name <- tolower(as.character(entry$name %||% ""))
    forced_wood <- grepl("shortbow|longbow|crossbow|quarterstaff|wooden club", name)
    locked <- isTRUE(meta$lock_provenance)
    requested <- if (locked) tolower(as.character(meta$material %||% "")) else ""
    candidates <- materials[vapply(seq_len(nrow(materials)), function(j) {
      equipment_material_is_eligible(materials[j, , drop=FALSE], enemy_type, characteristics)
    }, logical(1)), , drop=FALSE]
    material <- if (forced_wood) {
      candidates[tolower(candidates$name) == "wood", , drop=FALSE]
    } else if (nzchar(requested)) {
      candidates[tolower(candidates$name) == requested, , drop=FALSE]
    } else data.frame()
    if (!nrow(material)) {
      weighted <- candidates[!candidates$is_wood & suppressWarnings(as.numeric(candidates$drop_rate)) > 0, , drop=FALSE]
      if (!nrow(weighted)) weighted <- candidates
      material <- weighted_equipment_choice(weighted)$row
    }
    requested_quality <- if (locked) tolower(as.character(meta$build_quality %||% "")) else ""
    quality <- qualities[tolower(qualities$name) == requested_quality, , drop=FALSE]
    if (!nrow(quality)) quality <- weighted_equipment_choice(qualities)$row
    meta$material_id <- as.numeric(material$id[[1L]]); meta$material <- as.character(material$name[[1L]])
    meta$is_iron <- isTRUE(material$is_iron[[1L]]); meta$is_wood <- isTRUE(material$is_wood[[1L]])
    meta$material_attack_bonus <- as.numeric(material$attack_bonus[[1L]] %||% 0)
    meta$material_damage_modifier <- as.numeric(material$damage_modifier[[1L]] %||% 0)
    meta$condition_id <- as.numeric(quality$id[[1L]]); meta$build_quality <- as.character(quality$name[[1L]])
    meta$quality_attack_bonus <- as.numeric(quality$attack_bonus[[1L]] %||% 0)
    meta$quality_damage_modifier <- as.numeric(quality$damage_modifier[[1L]] %||% 0)
    meta$quality_armour_modifier <- as.numeric(quality$armour_modifier[[1L]] %||% 0)
    meta$provenance_assigned <- TRUE
    entry$meta <- meta; loot[[i]] <- entry
  }
  loot
}

roll_equipment_assignment <- function(character_id, instance_id, assignment_source = "player_roll") {
  assignment_source <- match.arg(assignment_source, c("player_roll", "control", "loot_roll"))
  con <- get_db_connection(); if (is.null(con)) return(NULL); on.exit(release_db_connection(con), add = TRUE)
  tryCatch(DBI::dbWithTransaction(con, {
    item <- DBI::dbGetQuery(con, paste(
      "SELECT ci.*,COALESCE(ci.custom_name,w.name,a.name) AS equipment_name,COALESCE(w.default_material_id,a.default_material_id) AS default_material_id",
      "FROM character_inventory_items ci LEFT JOIN weapons w ON w.id=ci.weapon_id LEFT JOIN armour a ON a.id=ci.armour_id",
      "WHERE ci.character_id=$1 AND ci.instance_id=$2 FOR UPDATE OF ci"
    ), params = list(as.character(character_id), as.character(instance_id)))
    if (!nrow(item)) stop("Weapon is no longer available.")
    material <- NULL; quality <- NULL
    if (is.na(item$material_id[[1L]])) {
      if (!is.na(item$default_material_id[[1L]])) {
        material <- DBI::dbGetQuery(con, "SELECT * FROM item_materials WHERE id=$1", params = list(item$default_material_id[[1L]]))
        source <- if (isTRUE(material$is_wood[[1L]])) "forced_wood" else assignment_source
        roll <- NA_real_; weights <- list()
      } else {
        choices <- DBI::dbGetQuery(con, "SELECT * FROM item_materials WHERE NOT is_wood AND COALESCE(drop_rate,0)>0 ORDER BY id")
        picked <- weighted_equipment_choice(choices); material <- picked$row; source <- assignment_source; roll <- picked$roll; weights <- as.list(picked$weights)
      }
      DBI::dbExecute(con, "UPDATE character_inventory_items SET material_id=$3,material_assignment=$4,updated_at=now() WHERE character_id=$1 AND instance_id=$2", params = list(as.character(character_id),as.character(instance_id),material$id[[1L]],source))
      DBI::dbExecute(con, "INSERT INTO equipment_assignment_log(character_id,instance_id,assignment_type,definition_id,roll_value,eligible_weights,assignment_source) VALUES($1,$2,'material',$3,$4,$5::jsonb,$6)", params = list(as.character(character_id),as.character(instance_id),material$id[[1L]],roll,inventory_json(weights),source))
    }
    if (is.na(item$condition_id[[1L]])) {
      choices <- DBI::dbGetQuery(con, "SELECT * FROM item_conditions WHERE COALESCE(drop_rate,0)>0 ORDER BY id")
      picked <- weighted_equipment_choice(choices); quality <- picked$row
      DBI::dbExecute(con, "UPDATE character_inventory_items SET condition_id=$3,condition_assignment=$4,updated_at=now() WHERE character_id=$1 AND instance_id=$2", params = list(as.character(character_id),as.character(instance_id),quality$id[[1L]],assignment_source))
      DBI::dbExecute(con, "INSERT INTO equipment_assignment_log(character_id,instance_id,assignment_type,definition_id,roll_value,eligible_weights,assignment_source) VALUES($1,$2,'build_quality',$3,$4,$5::jsonb,$6)", params = list(as.character(character_id),as.character(instance_id),quality$id[[1L]],picked$roll,inventory_json(as.list(picked$weights)),assignment_source))
    }
    final <- DBI::dbGetQuery(con, paste(
      "SELECT m.name AS material,q.name AS build_quality,m.attack_bonus+q.attack_bonus AS attack_bonus,",
      "m.damage_modifier+q.damage_modifier AS damage_modifier FROM character_inventory_items ci",
      "JOIN item_materials m ON m.id=ci.material_id JOIN item_conditions q ON q.id=ci.condition_id",
      "WHERE ci.character_id=$1 AND ci.instance_id=$2"
    ), params = list(as.character(character_id),as.character(instance_id)))
    DBI::dbExecute(con, "UPDATE character_inventory_items SET needs_provenance_roll=FALSE,updated_at=now() WHERE character_id=$1 AND instance_id=$2", params=list(as.character(character_id),as.character(instance_id)))
    if (nrow(final)) as.list(final[1, , drop = FALSE]) else NULL
  }), error = function(e) { message("roll_equipment_assignment failed: ", e$message); NULL })
}
