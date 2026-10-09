# server/combat_map_logic.R

`%||%` <- get("%||%", inherits = TRUE)

# ============================================================
# MAP SCHEMA
# ============================================================

empty_map_tiles <- function() {
  data.frame(
    map_id = integer(),
    x = integer(),
    y = integer(),
    terrain = character(),          # grass / stone / forest / wall / etc
    fog = integer(),                # 1 hidden, 0 visible
    light = character(),            # full / dim / dark
    move_cost = numeric(),          # 1 normal, 2 hard, Inf/NA impassable
    blocks_movement = logical(),
    blocks_vision = logical(),
    object_type = character(), object_id = integer(), object_chest_id = integer(), object_locked = logical(), object_name = character(),
    stringsAsFactors = FALSE
  )
}

empty_map_occupants <- function() {
  data.frame(
    map_id = integer(),
    encounter_id = integer(),
    actor_type = character(),
    actor_id = character(),
    x = integer(),
    y = integer(),
    stringsAsFactors = FALSE
  )
}

normalize_map_occupants <- function(df = NULL) {
  tmpl <- empty_map_occupants()
  
  if (is.null(df) || !is.data.frame(df)) {
    return(tmpl)
  }
  
  for (nm in names(tmpl)) {
    if (!nm %in% names(df)) {
      df[[nm]] <- tmpl[[nm]]
    }
  }
  
  df <- df[, names(tmpl), drop = FALSE]
  
  df$map_id <- suppressWarnings(as.integer(df$map_id))
  df$encounter_id <- suppressWarnings(as.integer(df$encounter_id))
  df$actor_type <- as.character(df$actor_type)
  df$actor_id <- as.character(df$actor_id)
  df$x <- suppressWarnings(as.integer(df$x))
  df$y <- suppressWarnings(as.integer(df$y))
  
  df$map_id[is.na(df$map_id)] <- 1L
  df$encounter_id[is.na(df$encounter_id)] <- 1L
  df$actor_type[is.na(df$actor_type) | df$actor_type == ""] <- "player"
  df$actor_id[is.na(df$actor_id)] <- ""
  df$x[is.na(df$x)] <- 1L
  df$y[is.na(df$y)] <- 1L
  
  df
}

# ============================================================
# MAP BUILDERS
# ============================================================

create_square_map_tiles <- function(map_id, width, height,
                                    default_terrain = "grass",
                                    default_fog = 0L,
                                    default_light = "full",
                                    default_move_cost = 1,
                                    default_blocks_movement = FALSE,
                                    default_blocks_vision = FALSE) {
  width <- as.integer(width %||% 0)
  height <- as.integer(height %||% 0)
  map_id <- as.integer(map_id %||% 1)
  
  if (is.na(width) || is.na(height) || width < 1 || height < 1) {
    return(empty_map_tiles())
  }
  
  grid <- expand.grid(
    x = seq_len(width),
    y = seq_len(height),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  data.frame(
    map_id = map_id,
    x = as.integer(grid$x),
    y = as.integer(grid$y),
    terrain = as.character(default_terrain %||% "grass"),
    fog = as.integer(default_fog %||% 0L),
    light = as.character(default_light %||% "full"),
    move_cost = as.numeric(default_move_cost %||% 1),
    blocks_movement = as.logical(default_blocks_movement %||% FALSE),
    blocks_vision = as.logical(default_blocks_vision %||% FALSE),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# VALIDATION / NORMALISATION
# ============================================================

normalize_map_tiles <- function(df = NULL) {
  tmpl <- empty_map_tiles()
  
  if (is.null(df) || !is.data.frame(df)) {
    return(tmpl)
  }
  
  for (nm in names(tmpl)) {
    if (!nm %in% names(df)) {
      df[[nm]] <- switch(nm,
        object_type = rep("", nrow(df)), object_name = rep("", nrow(df)),
        object_id = rep(NA_integer_, nrow(df)), object_chest_id = rep(NA_integer_, nrow(df)),
        object_locked = rep(FALSE, nrow(df)), tmpl[[nm]])
    }
  }
  
  df <- df[, names(tmpl), drop = FALSE]
  
  df$map_id <- suppressWarnings(as.integer(df$map_id))
  df$x <- suppressWarnings(as.integer(df$x))
  df$y <- suppressWarnings(as.integer(df$y))
  df$terrain <- as.character(df$terrain)
  df$fog <- suppressWarnings(as.integer(df$fog))
  df$light <- as.character(df$light)
  df$move_cost <- suppressWarnings(as.numeric(df$move_cost))
  df$blocks_movement <- as.logical(df$blocks_movement)
  df$blocks_vision <- as.logical(df$blocks_vision)
  
  df$map_id[is.na(df$map_id)] <- 1L
  df$x[is.na(df$x)] <- 1L
  df$y[is.na(df$y)] <- 1L
  df$terrain[is.na(df$terrain) | df$terrain == ""] <- "grass"
  df$fog[is.na(df$fog)] <- 0L
  df$light[is.na(df$light) | df$light == ""] <- "full"
  df$move_cost[is.na(df$move_cost)] <- 1
  df$blocks_movement[is.na(df$blocks_movement)] <- FALSE
  df$blocks_vision[is.na(df$blocks_vision)] <- FALSE
  
  df
}

normalize_map_occupants <- function(df = NULL) {
  tmpl <- empty_map_occupants()
  
  if (is.null(df) || !is.data.frame(df)) {
    return(tmpl)
  }
  
  for (nm in names(tmpl)) {
    if (!nm %in% names(df)) {
      df[[nm]] <- tmpl[[nm]]
    }
  }
  
  df <- df[, names(tmpl), drop = FALSE]
  
  df$map_id <- suppressWarnings(as.integer(df$map_id))
  df$encounter_id <- suppressWarnings(as.integer(df$encounter_id))
  df$actor_type <- as.character(df$actor_type)
  df$actor_id <- as.character(df$actor_id)
  df$x <- suppressWarnings(as.integer(df$x))
  df$y <- suppressWarnings(as.integer(df$y))
  
  df$map_id[is.na(df$map_id)] <- 1L
  df$encounter_id[is.na(df$encounter_id)] <- 1L
  df$actor_type[is.na(df$actor_type) | df$actor_type == ""] <- "player"
  df$actor_id[is.na(df$actor_id)] <- ""
  df$x[is.na(df$x)] <- 1L
  df$y[is.na(df$y)] <- 1L
  
  df
}

# ============================================================
# TILE HELPERS
# ============================================================

get_map_bounds <- function(tiles) {
  tiles <- normalize_map_tiles(tiles)
  
  if (!nrow(tiles)) {
    return(list(
      min_x = 1L, max_x = 0L,
      min_y = 1L, max_y = 0L,
      width = 0L, height = 0L
    ))
  }
  
  min_x <- min(tiles$x, na.rm = TRUE)
  max_x <- max(tiles$x, na.rm = TRUE)
  min_y <- min(tiles$y, na.rm = TRUE)
  max_y <- max(tiles$y, na.rm = TRUE)
  
  list(
    min_x = as.integer(min_x),
    max_x = as.integer(max_x),
    min_y = as.integer(min_y),
    max_y = as.integer(max_y),
    width = as.integer(max_x - min_x + 1L),
    height = as.integer(max_y - min_y + 1L)
  )
}

get_tile_row <- function(tiles, x, y, map_id = NULL) {
  tiles <- normalize_map_tiles(tiles)
  x <- as.integer(x %||% NA)
  y <- as.integer(y %||% NA)
  
  if (is.na(x) || is.na(y) || !nrow(tiles)) {
    return(tiles[0, , drop = FALSE])
  }
  
  out <- tiles[tiles$x == x & tiles$y == y, , drop = FALSE]
  
  if (!is.null(map_id)) {
    out <- out[out$map_id == as.integer(map_id), , drop = FALSE]
  }
  
  out
}

set_tile_values <- function(tiles, x, y, map_id = NULL, updates = list()) {
  tiles <- normalize_map_tiles(tiles)
  row <- get_tile_row(tiles, x, y, map_id = map_id)
  if (!nrow(row)) return(tiles)
  
  idx <- which(tiles$x == as.integer(x) & tiles$y == as.integer(y))
  if (!is.null(map_id)) {
    idx <- idx[tiles$map_id[idx] == as.integer(map_id)]
  }
  if (!length(idx)) return(tiles)
  
  for (nm in names(updates)) {
    if (nm %in% names(tiles)) {
      tiles[idx[1], nm] <- updates[[nm]]
    }
  }
  
  normalize_map_tiles(tiles)
}

# ============================================================
# OCCUPANT HELPERS
# ============================================================

get_actor_position <- function(occupants, actor_id, map_id = NULL, encounter_id = NULL) {
  occupants <- normalize_map_occupants(occupants)
  actor_id <- as.character(actor_id %||% "")
  
  if (!nzchar(actor_id) || !nrow(occupants)) {
    return(occupants[0, , drop = FALSE])
  }
  
  out <- occupants[occupants$actor_id == actor_id, , drop = FALSE]
  
  if (!is.null(map_id)) {
    out <- out[out$map_id == as.integer(map_id), , drop = FALSE]
  }
  
  if (!is.null(encounter_id)) {
    out <- out[out$encounter_id == as.integer(encounter_id), , drop = FALSE]
  }
  
  out
}

set_actor_position_local <- function(occupants, actor_id, x, y,
                                     map_id = 1L, encounter_id = 1L,
                                     actor_type = "player") {
  occupants <- normalize_map_occupants(occupants)
  
  actor_id <- as.character(actor_id %||% "")
  if (!nzchar(actor_id)) return(occupants)
  
  x <- as.integer(x %||% 1L)
  y <- as.integer(y %||% 1L)
  map_id <- as.integer(map_id %||% 1L)
  encounter_id <- as.integer(encounter_id %||% 1L)
  actor_type <- as.character(actor_type %||% "player")
  
  idx <- which(
    occupants$actor_id == actor_id &
      occupants$map_id == map_id &
      occupants$encounter_id == encounter_id
  )
  
  if (length(idx) >= 1) {
    occupants$x[idx[1]] <- x
    occupants$y[idx[1]] <- y
    occupants$actor_type[idx[1]] <- actor_type
  } else {
    occupants <- rbind(
      occupants,
      data.frame(
        map_id = map_id,
        encounter_id = encounter_id,
        actor_type = actor_type,
        actor_id = actor_id,
        x = x,
        y = y,
        stringsAsFactors = FALSE
      )
    )
  }
  
  normalize_map_occupants(occupants)
}

remove_actor_position_local <- function(occupants, actor_id, map_id = NULL, encounter_id = NULL) {
  occupants <- normalize_map_occupants(occupants)
  actor_id <- as.character(actor_id %||% "")
  
  if (!nzchar(actor_id) || !nrow(occupants)) return(occupants)
  
  keep <- occupants$actor_id != actor_id
  if (!is.null(map_id)) {
    keep <- keep | occupants$map_id != as.integer(map_id)
  }
  if (!is.null(encounter_id)) {
    keep <- keep | occupants$encounter_id != as.integer(encounter_id)
  }
  
  occupants[keep, , drop = FALSE]
}


get_encounter <- function(encounter_id) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  if (is.na(encounter_id) || encounter_id < 1) return(data.frame())
  
  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM encounters
      WHERE id = $1
      ",
      params = list(encounter_id)
    ),
    error = function(e) {
      message("get_encounter failed: ", e$message)
      data.frame()
    }
  )
}

get_encounter_events <- function(encounter_id, limit = 50) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  limit <- suppressWarnings(as.integer(limit))
  
  if (is.na(encounter_id) || encounter_id < 1) return(data.frame())
  if (is.na(limit) || limit < 1) limit <- 50L
  
  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM game_events
      WHERE encounter_id = $1
      ORDER BY created_at DESC
      LIMIT $2
      ",
      params = list(encounter_id, limit)
    ),
    error = function(e) {
      message("get_encounter_events failed: ", e$message)
      data.frame()
    }
  )
}

get_encounter_overview <- function(encounter_id) {
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  
  if (is.na(encounter_id) || encounter_id < 1) {
    return(list(
      encounter = data.frame(),
      players = data.frame(),
      positions = data.frame(),
      combat = data.frame(),
      events = data.frame()
    ))
  }
  
  enc <- tryCatch(get_encounter(encounter_id), error = function(e) data.frame())
  pos <- tryCatch(get_encounter_positions(encounter_id), error = function(e) data.frame())
  combat <- tryCatch(get_combat_state(encounter_id), error = function(e) data.frame())
  events <- tryCatch(get_encounter_events(encounter_id, limit = 20), error = function(e) data.frame())
  
  players <- data.frame()
  if (is.data.frame(enc) && nrow(enc) > 0 && "session_id" %in% names(enc)) {
    sid <- suppressWarnings(as.integer(enc$session_id[1]))
    if (!is.na(sid) && sid > 0) {
      players <- tryCatch(get_session_players(sid), error = function(e) data.frame())
    }
  }
  
  list(
    encounter = enc,
    players = players,
    positions = pos,
    combat = combat,
    events = events
  )
}
tile_is_occupied <- function(occupants, x, y, map_id = NULL, exclude_actor_id = NULL) {
  occupants <- normalize_map_occupants(occupants)
  if (!nrow(occupants)) return(FALSE)
  
  out <- occupants[
    occupants$x == as.integer(x) &
      occupants$y == as.integer(y),
    ,
    drop = FALSE
  ]
  
  if (!is.null(map_id)) {
    out <- out[out$map_id == as.integer(map_id), , drop = FALSE]
  }
  
  if (!is.null(exclude_actor_id)) {
    out <- out[as.character(out$actor_id) != as.character(exclude_actor_id), , drop = FALSE]
  }
  
  nrow(out) > 0
}

# ============================================================
# MOVEMENT / VISIBILITY
# ============================================================

can_enter_tile <- function(tiles, occupants, x, y, map_id = NULL, exclude_actor_id = NULL) {
  row <- get_tile_row(tiles, x, y, map_id = map_id)
  if (!nrow(row)) {
    return(list(ok = FALSE, reason = "out_of_bounds", move_cost = Inf))
  }
  
  blocks_move <- isTRUE(row$blocks_movement[1] %||% FALSE)
  cost <- suppressWarnings(as.numeric(row$move_cost[1] %||% 1))
  if (is.na(cost)) cost <- 1
  
  if (blocks_move || is.infinite(cost)) {
    return(list(ok = FALSE, reason = "blocked", move_cost = cost))
  }
  
  if (tile_is_occupied(occupants, x, y, map_id = map_id, exclude_actor_id = exclude_actor_id)) {
    return(list(ok = FALSE, reason = "occupied", move_cost = cost))
  }
  
  list(ok = TRUE, reason = "ok", move_cost = cost)
}

get_square_neighbors <- function(x, y, diagonal = FALSE) {
  dirs <- list(
    c(-1L, 0L),
    c(1L, 0L),
    c(0L, -1L),
    c(0L, 1L)
  )
  
  if (isTRUE(diagonal)) {
    dirs <- c(dirs, list(
      c(-1L, -1L),
      c(-1L, 1L),
      c(1L, -1L),
      c(1L, 1L)
    ))
  }
  
  data.frame(
    x = vapply(dirs, function(d) as.integer(x + d[1]), integer(1)),
    y = vapply(dirs, function(d) as.integer(y + d[2]), integer(1)),
    stringsAsFactors = FALSE
  )
}

can_view_tile_for_actor <- function(tile_row, actor_flags = list()) {
  if (!is.data.frame(tile_row) || nrow(tile_row) == 0) return(FALSE)
  
  fog <- as.integer(tile_row$fog[1] %||% 0)
  light <- tolower(as.character(tile_row$light[1] %||% "full"))
  
  has_torch <- isTRUE(actor_flags$has_torch %||% FALSE)
  darkvision <- isTRUE(actor_flags$darkvision %||% FALSE)
  
  if (fog == 1L) return(FALSE)
  if (light == "full") return(TRUE)
  if (light == "dim") return(TRUE)
  if (light == "dark") {
    return(has_torch || darkvision)
  }
  
  FALSE
}

# ============================================================
# SIMPLE RENDERER DATA
# ============================================================

build_map_render_df <- function(tiles, occupants = NULL, map_id = NULL) {
  tiles <- normalize_map_tiles(tiles)
  occupants <- normalize_map_occupants(occupants)
  
  if (!is.null(map_id)) {
    tiles <- tiles[tiles$map_id == as.integer(map_id), , drop = FALSE]
    occupants <- occupants[occupants$map_id == as.integer(map_id), , drop = FALSE]
  }
  
  if (!nrow(tiles)) return(tiles)
  
  occ_key <- paste0(occupants$x, "::", occupants$y)
  tile_key <- paste0(tiles$x, "::", tiles$y)
  
  tiles$occupant_type <- NA_character_
  tiles$occupant_id <- NA_character_
  
  if (nrow(occupants)) {
    match_idx <- match(tile_key, occ_key)
    hit <- which(!is.na(match_idx))
    
    if (length(hit)) {
      tiles$occupant_type[hit] <- occupants$actor_type[match_idx[hit]]
      tiles$occupant_id[hit] <- occupants$actor_id[match_idx[hit]]
    }
  }
  
  tiles
}


can_phase_through_objects <- function(char) {
  char <- validate_character(char)
  
  race_ok <- identical(tolower(as.character(char$meta$race %||% "")), "tylwyth teg")
  
  subclass_candidates <- c(
    as.character(char$build$path %||% ""),
    as.character(char$build$subclass %||% ""),
    as.character(char$build$archetype %||% "")
  )
  subclass_candidates <- tolower(trimws(subclass_candidates))
  
  subclass_ok <- any(subclass_candidates == "heart eater")
  
  isTRUE(race_ok && subclass_ok)
}

# ============================================================
# DB HELPERS (OPTIONAL NEXT STEP)
# ============================================================

`%||%` <- get("%||%", inherits = TRUE)

ensure_map_store <- function(ctrl) {
  if (is.null(ctrl$map_tiles_store)) {
    ctrl$map_tiles_store <- list()
  }
  invisible(TRUE)
}

get_shared_map_tiles <- function(ctrl, map_id) {
  ensure_map_store(ctrl)
  
  key <- as.character(as.integer(map_id %||% 1L))
  store <- ctrl$map_tiles_store %||% list()
  out <- store[[key]] %||% NULL
  
  if (is.null(out) || !is.data.frame(out)) {
    return(empty_map_tiles())
  }
  
  normalize_map_tiles(out)
}

set_shared_map_tiles <- function(ctrl, map_id, tiles) {
  ensure_map_store(ctrl)
  
  key <- as.character(as.integer(map_id %||% 1L))
  store <- ctrl$map_tiles_store %||% list()
  store[[key]] <- normalize_map_tiles(tiles)
  ctrl$map_tiles_store <- store
  
  invisible(TRUE)
}

get_or_create_shared_map_tiles <- function(ctrl, map_id, width = 10L, height = 10L) {
  tiles <- get_shared_map_tiles(ctrl, map_id)
  
  if (!is.data.frame(tiles) || nrow(tiles) == 0) {
    tiles <- create_square_map_tiles(
      map_id = as.integer(map_id %||% 1L),
      width = as.integer(width %||% 10L),
      height = as.integer(height %||% 10L),
      default_terrain = "grass",
      default_fog = 0L,
      default_light = "full",
      default_move_cost = 1,
      default_blocks_movement = FALSE,
      default_blocks_vision = FALSE
    )
    set_shared_map_tiles(ctrl, map_id, tiles)
  }
  
  tiles
}

# ============================================================
# MAP TILE DB HELPERS
# ============================================================

get_map_tiles <- function(map_id) {
  con <- get_db_connection()
  if (is.null(con)) return(empty_map_tiles())
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  map_id <- suppressWarnings(as.integer(map_id %||% NA))
  if (is.na(map_id) || map_id < 1) return(empty_map_tiles())
  
  out <- tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        t.map_id,
        t.x,
        t.y,
        t.terrain,
        t.fog,
        t.light,
        t.move_cost,
        t.blocks_movement,
        t.blocks_movement AS terrain_blocks_movement,
        CASE WHEN o.object_type IN ('door','gate') AND COALESCE(c.locked,FALSE)=FALSE THEN FALSE ELSE (t.blocks_vision OR (o.object_type='door' AND COALESCE(c.locked,TRUE))) END AS blocks_vision,
        COALESCE(o.object_type,'') AS object_type,o.id AS object_id,o.chest_id AS object_chest_id,COALESCE(c.locked,FALSE) AS object_locked,COALESCE(c.name,'') AS object_name,
        CASE WHEN o.object_type IN ('door','gate') THEN COALESCE(c.locked,TRUE) ELSE (t.blocks_movement OR o.object_type='chest') END AS effective_blocks_movement
      FROM public.map_tiles t LEFT JOIN map_objects o ON o.map_id=t.map_id AND o.x=t.x AND o.y=t.y LEFT JOIN chests c ON c.id=o.chest_id
      WHERE t.map_id = $1
      ORDER BY y, x
      ",
      params = list(map_id)
    ),
    error = function(e) {
      message("get_map_tiles failed: ", e$message)
      empty_map_tiles()
    }
  )
  
  if(is.data.frame(out)&&"effective_blocks_movement"%in%names(out))out$blocks_movement<-as.logical(out$effective_blocks_movement)
  normalize_map_tiles(out)
}

save_map_tiles <- function(map_id, tiles_df) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  
  
  map_id <- suppressWarnings(as.integer(map_id %||% NA))
  if (is.na(map_id) || map_id < 1) return(FALSE)
  
  tiles <- normalize_map_tiles(tiles_df)
  if (!is.data.frame(tiles) || nrow(tiles) == 0) return(FALSE)
  
  tiles$map_id <- map_id
  
  bounds <- get_map_bounds(tiles)
  ok_map <- ensure_map_record(
    map_id = map_id,
    width = bounds$width %||% 10L,
    height = bounds$height %||% 10L,
    name = paste0("Map ", map_id)
  )
  if (!isTRUE(ok_map)) return(FALSE)
  
  ok <- tryCatch({
    DBI::dbBegin(con)
    
    # Remove tiles that are no longer present in the incoming map snapshot
    DBI::dbExecute(
      con,
      "DELETE FROM public.map_tiles WHERE map_id = $1",
      params = list(map_id)
    )
    
    sql <- "
      INSERT INTO public.map_tiles (
        map_id,
        x,
        y,
        terrain,
        fog,
        light,
        move_cost,
        blocks_movement,
        blocks_vision
      )
      VALUES (
        $1, $2, $3, $4, $5, $6, $7, $8, $9
      )
    "
    
    for (i in seq_len(nrow(tiles))) {
      DBI::dbExecute(
        con,
        sql,
        params = list(
          suppressWarnings(as.integer(tiles$map_id[i] %||% map_id)),
          suppressWarnings(as.integer(tiles$x[i] %||% 1L)),
          suppressWarnings(as.integer(tiles$y[i] %||% 1L)),
          as.character(tiles$terrain[i] %||% "grass"),
          suppressWarnings(as.integer(tiles$fog[i] %||% 0L)),
          as.character(tiles$light[i] %||% "full"),
          suppressWarnings(as.numeric(tiles$move_cost[i] %||% 1)),
          isTRUE(tiles$blocks_movement[i]),
          isTRUE(tiles$blocks_vision[i])
        )
      )
    }
    
    DBI::dbCommit(con)
    TRUE
  }, error = function(e) {
    message("save_map_tiles failed: ", e$message)
    try(DBI::dbRollback(con), silent = TRUE)
    FALSE
  })
  
  isTRUE(ok)
}

upsert_map_tile <- function(map_id, x, y, updates = list()) {
  map_id <- suppressWarnings(as.integer(map_id %||% NA))
  x <- suppressWarnings(as.integer(x %||% NA))
  y <- suppressWarnings(as.integer(y %||% NA))
  
  if (is.na(map_id) || map_id < 1 || is.na(x) || x < 1 || is.na(y) || y < 1) {
    return(FALSE)
  }
  
  current <- get_map_tiles(map_id)
  
  if (!is.data.frame(current) || nrow(current) == 0) {
    width <- max(10L, x)
    height <- max(10L, y)
    current <- create_square_map_tiles(
      map_id = map_id,
      width = width,
      height = height,
      default_terrain = "grass",
      default_fog = 0L,
      default_light = "full",
      default_move_cost = 1,
      default_blocks_movement = FALSE,
      default_blocks_vision = FALSE
    )
  }
  
  updated <- set_tile_values(
    tiles = current,
    x = x,
    y = y,
    map_id = map_id,
    updates = updates
  )
  
  save_map_tiles(map_id, updated)
}

get_or_create_map_tiles <- function(map_id, width = 10L, height = 10L) {
  map_id <- suppressWarnings(as.integer(map_id %||% NA))
  width <- suppressWarnings(as.integer(width %||% 10L))
  height <- suppressWarnings(as.integer(height %||% 10L))
  
  if (is.na(map_id) || map_id < 1) map_id <- 1L
  if (is.na(width) || width < 1) width <- 10L
  if (is.na(height) || height < 1) height <- 10L
  
  tiles <- get_map_tiles(map_id)
  
  if (!is.data.frame(tiles) || nrow(tiles) == 0) {
    tiles <- create_square_map_tiles(
      map_id = map_id,
      width = width,
      height = height,
      default_terrain = "grass",
      default_fog = 0L,
      default_light = "full",
      default_move_cost = 1,
      default_blocks_movement = FALSE,
      default_blocks_vision = FALSE
    )
    ok <- save_map_tiles(map_id, tiles)
    if (!isTRUE(ok)) {
      message("get_or_create_map_tiles: failed to persist new map_id ", map_id)
    }
  }
  
  normalize_map_tiles(tiles)
}

ensure_map_store <- function(ctrl) {
  isolate({
    if (is.null(ctrl$map_tiles_store)) {
      ctrl$map_tiles_store <- list()
    }
  })
  invisible(TRUE)
}

get_shared_map_tiles <- function(ctrl, map_id) {
  ensure_map_store(ctrl)
  
  key <- as.character(as.integer(map_id %||% 1L))
  store <- isolate(ctrl$map_tiles_store %||% list())
  
  out <- store[[key]] %||% NULL
  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) {
    out <- get_map_tiles(map_id)
    if (is.data.frame(out) && nrow(out) > 0) {
      store[[key]] <- normalize_map_tiles(out)
      isolate({
        ctrl$map_tiles_store <- store
      })
    }
  }
  
  if (is.null(out) || !is.data.frame(out)) {
    return(empty_map_tiles())
  }
  
  normalize_map_tiles(out)
}

set_shared_map_tiles <- function(ctrl, map_id, tiles) {
  ensure_map_store(ctrl)
  
  key <- as.character(as.integer(map_id %||% 1L))
  norm_tiles <- normalize_map_tiles(tiles)
  
  ok <- save_map_tiles(map_id, norm_tiles)
  if (!isTRUE(ok)) return(FALSE)
  
  store <- isolate(ctrl$map_tiles_store %||% list())
  store[[key]] <- norm_tiles
  
  isolate({
    ctrl$map_tiles_store <- store
  })
  
  TRUE
}

get_or_create_shared_map_tiles <- function(ctrl, map_id, width = 10L, height = 10L) {
  ensure_map_store(ctrl)
  
  map_id <- suppressWarnings(as.integer(map_id %||% 1L))
  if (is.na(map_id) || map_id < 1) map_id <- 1L
  
  key <- as.character(map_id)
  store <- isolate(ctrl$map_tiles_store %||% list())
  out <- store[[key]] %||% NULL
  
  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) {
    out <- get_or_create_map_tiles(map_id = map_id, width = width, height = height)
    store[[key]] <- normalize_map_tiles(out)
    
    isolate({
      ctrl$map_tiles_store <- store
    })
  }
  
  normalize_map_tiles(out)
}

add_encounter_enemy <- function(
    encounter_id,
    name,
    hp_max,
    ac,
    movement_speed,
    attack_bonus = 2L,
    damage_expr = "1d6",
    damage_type = "slashing",
    template_key = NULL, enemy_type = "Custom", characteristics = list(),
    abilities = list(), attacks = list(), loot = list(), resistances = character(),
    immunities = character(), vulnerabilities = character(), condition_immunities = character(), gold_min = 0L, gold_max = 0L
) {
  `%||%` <- get("%||%", inherits = TRUE)
  loot <- roll_loot_equipment_provenance(loot, enemy_type, unlist(characteristics %||% list()))
  for (i in seq_along(attacks)) {
    loot_id <- as.character(attacks[[i]]$loot_id %||% "")
    loot_name <- if (nzchar(loot_id) && loot_id %in% names(enemy_loot_catalog())) enemy_loot_catalog()[[loot_id]]$name %||% "" else ""
    matched <- Filter(function(x) identical(as.character(x$name %||% ""), as.character(loot_name)), loot)
    if (length(matched)) attacks[[i]]$material <- as.character(matched[[1]]$meta$material %||% "")
  }
  
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  encounter_id   <- suppressWarnings(as.integer(encounter_id))
  hp_max         <- suppressWarnings(as.integer(hp_max))
  ac             <- suppressWarnings(as.integer(ac))
  movement_speed <- suppressWarnings(as.integer(movement_speed))
  attack_bonus   <- suppressWarnings(as.integer(attack_bonus))
  
  name        <- as.character(name %||% "")
  damage_expr <- as.character(damage_expr %||% "")
  damage_type <- as.character(damage_type %||% "")
  
  if (is.na(encounter_id) || encounter_id < 1) return(NULL)
  if (!nzchar(name)) name <- "Enemy"
  if (is.na(hp_max) || hp_max < 1L) hp_max <- 1L
  if (is.na(ac) || ac < 1L) ac <- 10L
  if (is.na(movement_speed) || movement_speed < 0L) movement_speed <- 30L
  if (is.na(attack_bonus)) attack_bonus <- 2L
  if (!nzchar(damage_expr)) damage_expr <- "1d6"
  if (!nzchar(damage_type)) damage_type <- "slashing"
  
  roll <- sample(1:20, 1)
  init_total <- as.integer(roll + attack_bonus)
  
  out <- tryCatch(
    DBI::dbGetQuery(
      con,
      "
      INSERT INTO encounter_enemies (
        encounter_id,
        name,
        hp_max,
        hp_current,
        temp_hp,
        ac,
        initiative,
        turn_order,
        is_active,
        movement_speed,
        attack_bonus,
        damage_expr,
        damage_type,
        template_key, enemy_type, characteristics, abilities, attacks, loot,
        resistances, immunities, vulnerabilities, condition_immunities, gold_min, gold_max,
        created_at,
        updated_at
      )
      VALUES (
        $1,   -- encounter_id
        $2,   -- name
        $3,   -- hp_max
        $3,   -- hp_current
        0,    -- temp_hp
        $4,   -- ac
        $5,   -- initiative
        NULL, -- turn_order
        TRUE, -- is_active
        $6,   -- movement_speed
        $7,   -- attack_bonus
        $8,   -- damage_expr
        $9,   -- damage_type
        $10, $11, $12::jsonb, $13::jsonb, $14::jsonb, $15::jsonb,
        $16::text[], $17::text[], $18::text[], $19::text[], $20, $21,
        NOW(),
        NOW()
      )
      RETURNING enemy_uuid
      ",
      params = list(
        encounter_id,
        name,
        hp_max,
        ac,
        init_total,
        movement_speed,
        attack_bonus,
        damage_expr,
        damage_type,
        as.character(template_key %||% ""), as.character(enemy_type %||% "Custom"),
        enemy_json(characteristics), enemy_json(abilities), enemy_json(attacks), enemy_json(loot),
        enemy_pg_array(resistances), enemy_pg_array(immunities), enemy_pg_array(vulnerabilities), enemy_pg_array(condition_immunities),
        as.integer(gold_min %||% 0L), as.integer(gold_max %||% 0L)
      )
    ),
    error = function(e) {
      message("add_encounter_enemy DB insert failed: ", e$message)
      NULL
    }
  )
  
  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) return(NULL)
  
  enemy_uuid <- as.character(out$enemy_uuid[1] %||% "")
  if (!nzchar(enemy_uuid)) return(NULL)
  
  enemy_uuid
}

ensure_map_record <- function(map_id, width = 10L, height = 10L, name = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  map_id <- suppressWarnings(as.integer(map_id %||% NA))
  width <- suppressWarnings(as.integer(width %||% 10L))
  height <- suppressWarnings(as.integer(height %||% 10L))
  
  if (is.na(map_id) || map_id < 1) return(FALSE)
  if (is.na(width) || width < 1) width <- 10L
  if (is.na(height) || height < 1) height <- 10L
  if (is.null(name) || !nzchar(as.character(name))) {
    name <- paste0("Map ", map_id)
  }
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      INSERT INTO public.maps (id, name, width, height)
      VALUES ($1, $2, $3, $4)
      ON CONFLICT (id) DO UPDATE
      SET
        name = COALESCE(public.maps.name, EXCLUDED.name),
        width = GREATEST(public.maps.width, EXCLUDED.width),
        height = GREATEST(public.maps.height, EXCLUDED.height),
        updated_at = now()
      ",
      params = list(
        map_id,
        as.character(name),
        width,
        height
      )
    )
    TRUE
  }, error = function(e) {
    message("ensure_map_record failed: ", e$message)
    FALSE
  })
}
