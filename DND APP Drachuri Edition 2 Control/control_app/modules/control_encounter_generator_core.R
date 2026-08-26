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
  clutter <- c("table","bar","chair","bench","crate","barrel","bed","shelf","rubble","campfire")
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
