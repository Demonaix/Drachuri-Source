encounter_damage_average <- function(expr) {
  expr <- gsub("\\s+", "", tolower(as.character(expr %||% "")))
  if (!nzchar(expr)) return(0)
  parts <- regmatches(expr, gregexpr("[+-]?[^+-]+", expr, perl = TRUE))[[1L]]
  total <- 0
  for (part in parts) {
    sign <- if (startsWith(part, "-")) -1 else 1
    token <- sub("^[+-]", "", part)
    if (grepl("^[0-9]+d[0-9]+$", token)) {
      dice <- as.numeric(strsplit(token, "d", fixed = TRUE)[[1L]])
      total <- total + sign * dice[[1L]] * (dice[[2L]] + 1) / 2
    } else {
      value <- suppressWarnings(as.numeric(token))
      if (!is.na(value)) total <- total + sign * value
    }
  }
  max(0, total)
}

encounter_template_threat <- function(template) {
  hp <- suppressWarnings(as.numeric(template$hp_max[[1L]] %||% 10))
  ac <- suppressWarnings(as.numeric(template$ac[[1L]] %||% 12))
  hit <- suppressWarnings(as.numeric(template$attack_bonus[[1L]] %||% 2))
  damage <- encounter_damage_average(template$damage_expr[[1L]] %||% "1d4")
  if (is.na(hp)) hp <- 10
  if (is.na(ac)) ac <- 12
  if (is.na(hit)) hit <- 2
  max(5, hp * max(.65, 1 + (ac - 12) * .055) + damage * 3 + max(0, hit - 2) * 1.5)
}

encounter_party_budget <- function(players, danger = "standard") {
  if ("is_active" %in% names(players)) players <- players[is.na(players$is_active) | as.logical(players$is_active), , drop = FALSE]
  if (!nrow(players)) return(0)
  hp <- suppressWarnings(as.numeric(players$max_hp %||% rep(12, nrow(players))))
  hp[is.na(hp) | hp < 1] <- 12
  multiplier <- c(low=.42, standard=.68, dangerous=.95, overwhelming=1.25)[[danger]] %||% .68
  max(10, sum(hp) * multiplier)
}

encounter_roll_pool_rules <- function(rules) {
  rules <- rules %||% list()
  independent <- Filter(function(x) !nzchar(as.character(x$group %||% "")), rules)
  selected <- vapply(Filter(function(x) runif(1) * 100 <= as.numeric(x$chance %||% 0), independent), function(x) as.character(x$item_id), character(1))
  grouped <- Filter(function(x) nzchar(as.character(x$group %||% "")), rules)
  if (length(grouped)) for (set in split(grouped, vapply(grouped, function(x) as.character(x$group), character(1)))) {
    weights <- vapply(set, function(x) as.numeric(x$chance %||% 0), numeric(1))
    required <- any(vapply(set, function(x) isTRUE(x$required), logical(1)))
    if (sum(weights) > 0 && (required || runif(1) * 100 <= min(100, sum(weights))))
      selected <- c(selected, sample(vapply(set, function(x) as.character(x$item_id), character(1)), 1L, prob=weights))
  }
  unique(selected)
}

# Produce disposable encounter candidates directly from a pool. Saved templates are
# deliberately not consulted: authored creatures must never leak into random encounters.
generate_encounter_pool_candidates <- function(pool, count=18L, seed=1L) {
  if (is.null(pool) || !nzchar(as.character(pool$id %||% ""))) stop("Choose an NPC pool.")
  if (identical(as.character(pool$id), "custom")) stop("The Custom Enemy pool is for authored creatures and cannot be randomly generated.")
  set.seed(as.integer(seed %||% 1L))
  attacks_catalog <- enemy_attack_catalog()
  loot_catalog <- enemy_loot_catalog()
  feature_catalog <- if (exists("merge_npc_feature_catalogue", mode="function")) merge_npc_feature_catalogue(list()) else list()
  feature_names <- setNames(vapply(feature_catalog, function(x) as.character(x$name %||% x$id %||% ""), character(1)), vapply(feature_catalog, function(x) as.character(x$id %||% ""), character(1)))
  make_one <- function(i) {
    b <- resolve_enemy_blueprint(as.character(pool$base_type %||% "Custom"), character())
    if (length(pool$abilities %||% list())) b$abilities <- pool$abilities
    for (field in c("hp_max","ac","movement_speed")) if (!is.null(pool[[field]])) b[[field]] <- as.integer(pool[[field]])
    if (!is.null(pool$gold)) b$gold <- as.integer(pool$gold)
    features <- unique(c(as.character(pool$features %||% character()), encounter_roll_pool_rules(pool$feature_rules %||% list())))
    loot_ids <- unique(c(as.character(b$loot_ids %||% character()), encounter_roll_pool_rules(pool$rules %||% list())))
    attack_ids <- unique(c(as.character(pool$attack_ids %||% b$attack_ids %||% character()), encounter_roll_pool_rules(pool$attack_rules %||% list())))
    attacks <- unname(attacks_catalog[intersect(attack_ids, names(attacks_catalog))])
    weapon_attacks <- Filter(function(a) nzchar(as.character(a$loot_id %||% "")) && as.character(a$loot_id) %in% loot_ids, attacks_catalog)
    attacks <- c(attacks, weapon_attacks)
    if (!length(attacks)) attacks <- list(list(name="Attack",hit=2L,dmg="1d4",type="bludgeoning"))
    attacks <- attacks[!duplicated(vapply(attacks, function(a) paste(a$name %||% "", a$dmg %||% "", sep="|"), character(1)))]
    loot <- unname(loot_catalog[intersect(loot_ids, names(loot_catalog))])
    if (exists("roll_loot_equipment_provenance", mode="function") && length(loot)) loot <- roll_loot_equipment_provenance(loot, as.character(pool$base_type %||% "Custom"), features)
    primary <- attacks[[1L]]
    descriptors <- unname(feature_names[intersect(features, names(feature_names))]); descriptors <- descriptors[nzchar(descriptors)]
    display <- paste(c(as.character(pool$name %||% "Enemy"), head(descriptors, 1L)), collapse=" ")
    gold <- as.integer(b$gold %||% c(0L,0L)); if (!length(gold)) gold <- c(0L,0L); if (length(gold)==1L) gold <- rep(gold,2L)
    data.frame(npc_id=paste0("generated_",pool$id,"_",seed,"_",i),name=display,enemy_type=as.character(pool$name %||% pool$id),hp_max=as.integer(b$hp_max %||% 10L),ac=as.integer(b$ac %||% 12L),movement_speed=as.integer(b$movement_speed %||% 30L),attack_name=as.character(primary$name %||% "Attack"),attack_bonus=as.integer(primary$hit %||% 2L),damage_expr=as.character(primary$dmg %||% "1d4"),damage_type=as.character(primary$type %||% "bludgeoning"),attacks_json=as.character(enemy_json(attacks)),gold_min=min(gold),gold_max=max(gold),stringsAsFactors=FALSE,
      characteristics=I(list(as.list(features))),abilities=I(list(b$abilities %||% list())),attacks=I(list(attacks)),loot=I(list(loot)),resistances=I(list(unique(c(b$resistances %||% character(),pool$resistances %||% character())))),immunities=I(list(unique(c(b$immunities %||% character(),pool$immunities %||% character())))),vulnerabilities=I(list(unique(c(b$vulnerabilities %||% character(),pool$vulnerabilities %||% character())))),condition_immunities=I(list(unique(c(b$condition_immunities %||% character(),pool$condition_immunities %||% character())))))
  }
  do.call(rbind, lapply(seq_len(max(1L,as.integer(count))), make_one))
}

compose_encounter_draft <- function(players, templates, danger = "standard", seed = 1L, max_enemies = 12L) {
  if (!is.data.frame(players) || !nrow(players)) stop("The selected session has no active players.")
  if (!is.data.frame(templates) || !nrow(templates)) stop("This NPC pool has no saved templates.")
  set.seed(as.integer(seed %||% 1L))
  scores <- vapply(seq_len(nrow(templates)), function(i) encounter_template_threat(templates[i,,drop=FALSE]), numeric(1))
  budget <- encounter_party_budget(players, danger)
  chosen <- integer()
  remaining <- budget
  while (length(chosen) < max_enemies) {
    affordable <- which(scores <= remaining * 1.18)
    if (!length(affordable)) {
      if (!length(chosen)) chosen <- which.min(scores)
      break
    }
    closeness <- 1 / pmax(1, abs(remaining - scores[affordable]))
    pick <- sample(affordable, 1L, prob = closeness)
    chosen <- c(chosen, pick)
    remaining <- remaining - scores[[pick]]
    if (remaining < min(scores) * .55) break
  }
  selected <- templates[chosen,,drop=FALSE]
  selected$threat_score <- round(scores[chosen], 1)
  list(
    enemies = selected,
    party_budget = round(budget, 1),
    enemy_threat = round(sum(scores[chosen]), 1),
    danger = danger,
    seed = as.integer(seed),
    party_size = nrow(players)
  )
}

encounter_draft_dimensions <- function(party_size, enemy_count) {
  actors <- max(2L, as.integer(party_size) + as.integer(enemy_count))
  c(width = max(12L, min(24L, 10L + ceiling(sqrt(actors) * 2.5))),
    height = max(10L, min(20L, 8L + ceiling(sqrt(actors) * 2))))
}

encounter_draft_positions <- function(tiles, player_ids, enemy_ids, seed = 1L) {
  clutter <- c("table","bar","chair","bench","crate","barrel","bed","shelf","rubble","campfire","torch","brazier")
  open <- tiles[!as.logical(tiles$blocks_movement) & !tolower(as.character(tiles$terrain)) %in% clutter,,drop=FALSE]
  needed <- length(player_ids) + length(enemy_ids)
  if (nrow(open) < needed) stop("The generated map does not contain enough open deployment tiles.")
  set.seed(as.integer(seed %||% 1L) + 7919L)
  player_zone <- open[order(open$y, open$x),,drop=FALSE]
  enemy_zone <- open[order(-open$y, open$x),,drop=FALSE]
  take_spread <- function(zone, n, used = character(), edge = "low") {
    zone <- zone[!paste(zone$x,zone$y,sep=",") %in% used,,drop=FALSE]
    if (!n) return(zone[FALSE,,drop=FALSE])
    ys <- sort(unique(zone$y))
    cutoff <- if (edge == "high") ys[max(1L,length(ys)-2L)] else ys[min(3L,length(ys))]
    band <- zone[if (edge == "high") zone$y >= cutoff else zone$y <= cutoff,,drop=FALSE]
    if (nrow(band) < n) band <- zone
    band[sample(seq_len(nrow(band)), n),,drop=FALSE]
  }
  pp <- take_spread(player_zone, length(player_ids));used <- paste(pp$x,pp$y,sep=",")
  ep <- take_spread(enemy_zone, length(enemy_ids), used, "high")
  rbind(
    data.frame(actor_type="player",actor_id=as.character(player_ids),x=as.integer(pp$x),y=as.integer(pp$y),stringsAsFactors=FALSE),
    data.frame(actor_type="enemy",actor_id=as.character(enemy_ids),x=as.integer(ep$x),y=as.integer(ep$y),stringsAsFactors=FALSE)
  )
}
