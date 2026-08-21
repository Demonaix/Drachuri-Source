#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
apply_changes <- identical(args, "--apply")
if (length(args) > 1L || (length(args) == 1L && !apply_changes)) {
  stop("Usage: Rscript scripts/backfill_relational_inventory.R [--apply]", call. = FALSE)
}

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- gsub("~\\+~", " ", sub("^--file=", "", file_arg[[1L]]))
project_dir <- normalizePath(file.path(dirname(script_path), ".."))
player_dir <- file.path(project_dir, "DND APP Drachuri Edition Player_v2")
launcher_library <- file.path(player_dir, "launcher", "bootstrap-library")
if (dir.exists(launcher_library)) .libPaths(c(launcher_library, .libPaths()))
sys.source(file.path(player_dir, "env.R"), envir = globalenv())

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(jsonlite)
})

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
scalar <- function(x, fallback = "") {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) fallback else x[[1L]]
}
number <- function(x, fallback = 0) {
  value <- suppressWarnings(as.numeric(scalar(x, fallback)))
  if (is.na(value) || !is.finite(value)) fallback else value
}
flag <- function(x, fallback = FALSE) {
  value <- as.logical(scalar(x, fallback))
  if (is.na(value)) fallback else value
}
slug <- function(x) {
  value <- tolower(iconv(as.character(x), to = "ASCII//TRANSLIT", sub = ""))
  value <- gsub("[^a-z0-9]+", "_", value)
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value)) "unnamed" else substr(value, 1L, 70L)
}
normalise_type <- function(x) {
  type <- tolower(trimws(as.character(x %||% "item")))
  if (type %in% c("weapon", "weapons")) return("weapon")
  if (type %in% c("armor", "armour", "shield")) return("armour")
  "item"
}
safe_meta <- function(x) if (is.list(x)) x else list()
json <- function(x) as.character(toJSON(x, auto_unbox = TRUE, null = "null", na = "null"))

con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("SUPABASE_HOST"),
  port = as.integer(Sys.getenv("SUPABASE_PORT")),
  dbname = Sys.getenv("SUPABASE_DBNAME"),
  user = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_DB_PASSWORD"),
  sslmode = Sys.getenv("SUPABASE_SSLMODE", unset = "require")
)
on.exit(dbDisconnect(con), add = TRUE)

required_tables <- c(
  "character_inventory_items", "character_wallets", "weapons", "armour", "items"
)
missing_tables <- required_tables[!vapply(
  required_tables, function(table) dbExistsTable(con, table), logical(1)
)]
if (length(missing_tables)) {
  stop("Apply migration 007 first. Missing: ", paste(missing_tables, collapse = ", "), call. = FALSE)
}

characters <- dbGetQuery(
  con,
  "SELECT id, char_name, state_blob FROM character_blobs ORDER BY char_name, id"
)
catalogues <- list(
  weapon = dbGetQuery(con, "SELECT id, name FROM weapons"),
  armour = dbGetQuery(con, "SELECT id, name FROM armour"),
  item = dbGetQuery(con, "SELECT id, name FROM items")
)

definition_id <- function(kind, name) {
  catalogue <- catalogues[[kind]]
  match_index <- match(tolower(trimws(name)), tolower(trimws(catalogue$name)))
  if (!is.na(match_index)) return(catalogue$id[[match_index]])
  paste0("party_", slug(name))
}

plans <- list()
for (row_index in seq_len(nrow(characters))) {
  character <- tryCatch(unserialize(characters$state_blob[[row_index]]), error = function(e) NULL)
  if (is.null(character)) {
    warning("Could not decode character ", characters$id[[row_index]])
    next
  }
  inventory <- character$inventory$items
  if (is.null(inventory) || !is.data.frame(inventory)) inventory <- data.frame()
  item_plans <- vector("list", nrow(inventory))
  for (item_index in seq_len(nrow(inventory))) {
    item <- inventory[item_index, , drop = FALSE]
    kind <- normalise_type(scalar(item$type, "item"))
    name <- trimws(as.character(scalar(item$name, "Unnamed item")))
    meta <- safe_meta(if ("meta" %in% names(item)) item$meta[[1L]] else list())
    id <- definition_id(kind, name)
    item_plans[[item_index]] <- list(
      character_id = as.character(characters$id[[row_index]]),
      instance_id = as.character(scalar(item$id, paste0("legacy_", item_index))),
      kind = kind,
      definition_id = id,
      name = name,
      description = as.character(scalar(item$desc, "")),
      value = number(item$value, 0),
      weight = number(item$weight, 0),
      quantity = number(item$qty, 1),
      equipped = flag(item$equipped, FALSE),
      in_bag = flag(item$in_bag, FALSE),
      original_type = as.character(scalar(item$type, "item")),
      meta = meta
    )
  }
  plans[[length(plans) + 1L]] <- list(
    character_id = as.character(characters$id[[row_index]]),
    character_name = as.character(characters$char_name[[row_index]] %||% ""),
    gold = number(character$inventory$gold, 0),
    items = item_plans
  )
}

flat_items <- unlist(lapply(plans, `[[`, "items"), recursive = FALSE)
cat(sprintf(
  "%s: %d character(s), %d owned item row(s), %d gold total.\n",
  if (apply_changes) "Applying" else "Dry run",
  length(plans), length(flat_items), sum(vapply(plans, `[[`, numeric(1), "gold"))
))
by_kind <- table(vapply(flat_items, `[[`, character(1), "kind"))
if (length(by_kind)) print(by_kind)
new_defs <- unique(vapply(flat_items, function(x) paste(x$kind, x$definition_id), character(1)))
cat(length(new_defs), "distinct catalogue definition(s) referenced.\n")
if (!apply_changes) {
  cat("No database changes made. Re-run with --apply after reviewing this summary.\n")
  quit(save = "no", status = 0L)
}

upsert_definition <- function(item) {
  if (item$kind == "weapon") {
    dbExecute(con, paste(
      "INSERT INTO weapons (id,name,description,value,weight,default_quantity,stat,advantage,",
      "to_hit_bonus,damage_1,damage_type_1,damage_2,damage_type_2,proficient,pools)",
      "VALUES ($1,$2,$3,$4,$5,1,$6,$7,$8,$9,$10,$11,$12,$13,'{}')",
      "ON CONFLICT (id) DO NOTHING"
    ), params = list(
      item$definition_id, item$name, item$description, item$value, item$weight,
      tolower(as.character(scalar(item$meta$stat, "str"))),
      as.character(scalar(item$meta$adv %||% item$meta$advantage, "Normal")),
      as.integer(number(item$meta$to_hit_bonus, 0)),
      as.character(scalar(item$meta$damage1 %||% item$meta$damage_1, "1d4")),
      as.character(scalar(item$meta$dmg_type1 %||% item$meta$damage_type_1, "Other")),
      as.character(scalar(item$meta$damage2 %||% item$meta$damage_2, "")),
      as.character(scalar(item$meta$dmg_type2 %||% item$meta$damage_type_2, "Other")),
      flag(item$meta$proficient, TRUE)
    ))
  } else if (item$kind == "armour") {
    armour_type <- as.character(scalar(item$meta$type %||% item$meta$armour_type, "Light"))
    if (!armour_type %in% c("Light", "Medium", "Heavy", "Shield", "Unarmoured")) armour_type <- "Light"
    dbExecute(con, paste(
      "INSERT INTO armour (id,name,description,value,weight,default_quantity,base_ac,",
      "armour_type,max_dex_bonus,proficient,pools)",
      "VALUES ($1,$2,$3,$4,$5,1,$6,$7,$8,$9,'{}') ON CONFLICT (id) DO NOTHING"
    ), params = list(
      item$definition_id, item$name, item$description, item$value, item$weight,
      as.integer(number(item$meta$base_ac, 10)), armour_type,
      as.integer(number(item$meta$custom_max_dex %||% item$meta$max_dex_bonus, 0)),
      flag(item$meta$proficient, TRUE)
    ))
  } else {
    dbExecute(con, paste(
      "INSERT INTO items (id,name,item_type,description,value,weight,default_quantity,effect,effect_amount,pools)",
      "VALUES ($1,$2,'item',$3,$4,$5,1,$6,$7,'{}') ON CONFLICT (id) DO NOTHING"
    ), params = list(
      item$definition_id, item$name, item$description, item$value, item$weight,
      as.character(scalar(item$meta$effect, "")),
      as.character(scalar(item$meta$effect_amount, ""))
    ))
  }
}

dbWithTransaction(con, {
  for (plan in plans) {
    dbExecute(con, paste(
      "INSERT INTO character_wallets (character_id,gold,updated_at)",
      "VALUES ($1,$2,now()) ON CONFLICT (character_id) DO UPDATE",
      "SET gold=EXCLUDED.gold,updated_at=now()"
    ), params = list(plan$character_id, plan$gold))
    for (item in plan$items) {
      upsert_definition(item)
      refs <- list(
        weapon = if (item$kind == "weapon") item$definition_id else NA_character_,
        armour = if (item$kind == "armour") item$definition_id else NA_character_,
        item = if (item$kind == "item") item$definition_id else NA_character_
      )
      properties <- item$meta
      properties$legacy_type <- item$original_type
      dbExecute(con, paste(
        "INSERT INTO character_inventory_items (character_id,instance_id,weapon_id,armour_id,item_id,",
        "quantity,equipped,in_bag,custom_name,custom_description,custom_value,custom_weight,properties,source,updated_at)",
        "VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13::jsonb,'character_backfill',now())",
        "ON CONFLICT (character_id,instance_id) DO UPDATE SET",
        "weapon_id=EXCLUDED.weapon_id,armour_id=EXCLUDED.armour_id,item_id=EXCLUDED.item_id,",
        "quantity=EXCLUDED.quantity,equipped=EXCLUDED.equipped,in_bag=EXCLUDED.in_bag,",
        "custom_name=EXCLUDED.custom_name,custom_description=EXCLUDED.custom_description,",
        "custom_value=EXCLUDED.custom_value,custom_weight=EXCLUDED.custom_weight,",
        "properties=EXCLUDED.properties,updated_at=now()"
      ), params = list(
        item$character_id, item$instance_id, refs$weapon, refs$armour, refs$item,
        item$quantity, item$equipped, item$in_bag, item$name, item$description,
        item$value, item$weight, json(properties)
      ))
    }
  }
})

counts <- dbGetQuery(con, paste(
  "SELECT (SELECT count(*) FROM character_wallets) AS wallets,",
  "(SELECT count(*) FROM character_inventory_items) AS owned_items"
))
cat(sprintf(
  "Backfill complete: %s wallet row(s), %s owned item row(s). Character blobs were not changed.\n",
  as.character(counts$wallets[[1L]]), as.character(counts$owned_items[[1L]])
))
