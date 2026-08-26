# global.R
library(shiny)

APP_SAVE_VERSION <- 2

`%||%` <- function(a, b) if (!is.null(a)) a else b

#Database connection
library(DBI)
library(RPostgres)

get_all_sessions <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        id AS session_id,
        name,
        mode,
        status,
        round_number,
        current_turn_order,
        active_actor_type,
        active_actor_id,
        active_map_id,
        created_at,
        updated_at
      FROM game_sessions
      ORDER BY id DESC
      "
    ),
    error = function(e) {
      message("get_all_sessions failed: ", e$message)
      data.frame()
    }
  )
}

get_session_enemies <- function(session_id) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  if (is.na(session_id) || session_id < 1) return(data.frame())
  
  res <- tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        id,
        session_id,
        enemy_uuid,
        name,
        template_key,
        hp_max,
        hp_current,
        temp_hp,
        ac,
        initiative,
        turn_order,
        is_active,
        movement_speed,
        notes,
        created_at,
        updated_at
      FROM encounter_enemies
      WHERE session_id = $1
      ORDER BY created_at ASC, id ASC
      ",
      params = list(session_id)
    ),
    error = function(e) {
      message("get_session_enemies failed: ", e$message)
      data.frame()
    }
  )
  
  if (!is.data.frame(res) || nrow(res) == 0) return(data.frame())
  
  int_cols <- c("id", "session_id", "hp_max", "hp_current", "temp_hp",
                "ac", "initiative", "turn_order", "movement_speed")
  for (nm in intersect(int_cols, names(res))) {
    res[[nm]] <- suppressWarnings(as.integer(res[[nm]]))
  }
  
  if ("is_active" %in% names(res)) {
    res$is_active <- as.logical(res$is_active)
  }
  
  res$enemy_uuid <- as.character(res$enemy_uuid %||% "")
  res$name <- as.character(res$name %||% "")
  res$template_key <- as.character(res$template_key %||% "")
  res$notes <- as.character(res$notes %||% "")
  
  res
}

add_session_enemy <- function(session_id,
                              name,
                              hp_max = 10,
                              ac = 12,
                              template_key = NULL,
                              movement_speed = 30,
                              notes = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  `%||%` <- get("%||%", inherits = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  hp_max <- suppressWarnings(as.integer(hp_max))
  ac <- suppressWarnings(as.integer(ac))
  movement_speed <- suppressWarnings(as.integer(movement_speed))
  
  name <- as.character(name %||% "")
  template_key <- as.character(template_key %||% "")
  notes <- as.character(notes %||% "")
  
  if (is.na(session_id) || session_id < 1 || !nzchar(name)) return(NULL)
  if (is.na(hp_max) || hp_max < 1) hp_max <- 10L
  if (is.na(ac) || ac < 1) ac <- 12L
  if (is.na(movement_speed) || movement_speed < 0) movement_speed <- 30L
  
  template_key_db <- if (nzchar(template_key)) template_key else NA_character_
  notes_db <- if (nzchar(notes)) notes else NA_character_
  
  res <- tryCatch(
    DBI::dbGetQuery(
      con,
      "
      INSERT INTO encounter_enemies (
        session_id,
        name,
        template_key,
        hp_max,
        hp_current,
        temp_hp,
        ac,
        initiative,
        turn_order,
        is_active,
        movement_speed,
        notes,
        created_at,
        updated_at
      )
      VALUES (
        $1, $2, $3,
        $4, $4, 0,
        $5, NULL, NULL, TRUE, $6, $7,
        NOW(), NOW()
      )
      RETURNING enemy_uuid
      ",
      params = list(
        session_id,
        name,
        template_key_db,
        hp_max,
        ac,
        movement_speed,
        notes_db
      )
    ),
    error = function(e) {
      message("add_session_enemy failed: ", e$message)
      NULL
    }
  )
  
  if (is.null(res) || nrow(res) == 0) return(NULL)
  as.character(res$enemy_uuid[1])
}

get_session_actors <- function(session_id) {
  `%||%` <- get("%||%", inherits = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  if (is.na(session_id) || session_id < 1) return(data.frame())
  
  players <- get_session_players(session_id)
  enemies <- get_session_enemies(session_id)
  
  if (!is.data.frame(players)) players <- data.frame()
  if (!is.data.frame(enemies)) enemies <- data.frame()
  
  p <- data.frame(
    actor_id = character(),
    actor_type = character(),
    display_name = character(),
    initiative = integer(),
    turn_order = integer(),
    is_active = logical(),
    current_hp = integer(),
    temp_hp = integer(),
    ac = integer(),
    stringsAsFactors = FALSE
  )
  
  e <- p[0, , drop = FALSE]
  
  if (nrow(players) > 0) {
    p <- data.frame(
      actor_id = as.character(players$character_id %||% ""),
      actor_type = "player",
      display_name = as.character(players$display_name %||% "Unknown"),
      initiative = suppressWarnings(as.integer(players$initiative %||% NA)),
      turn_order = suppressWarnings(as.integer(players$turn_order %||% NA)),
      is_active = as.logical(players$is_active %||% TRUE),
      current_hp = suppressWarnings(as.integer(players$current_hp %||% 0)),
      temp_hp = suppressWarnings(as.integer(players$temp_hp %||% 0)),
      ac = NA_integer_,
      stringsAsFactors = FALSE
    )
  }
  
  if (nrow(enemies) > 0) {
    e <- data.frame(
      actor_id = as.character(enemies$enemy_uuid %||% ""),
      actor_type = "enemy",
      display_name = as.character(enemies$name %||% "Enemy"),
      initiative = suppressWarnings(as.integer(enemies$initiative %||% NA)),
      turn_order = suppressWarnings(as.integer(enemies$turn_order %||% NA)),
      is_active = as.logical(enemies$is_active %||% TRUE),
      current_hp = suppressWarnings(as.integer(enemies$hp_current %||% 0)),
      temp_hp = suppressWarnings(as.integer(enemies$temp_hp %||% 0)),
      ac = suppressWarnings(as.integer(enemies$ac %||% NA)),
      stringsAsFactors = FALSE
    )
  }
  
  out <- rbind(p, e)
  
  if (!nrow(out)) return(out)
  
  out$initiative[is.na(out$initiative)] <- NA_integer_
  out$turn_order[is.na(out$turn_order)] <- NA_integer_
  out$is_active[is.na(out$is_active)] <- TRUE
  out$current_hp[is.na(out$current_hp)] <- 0L
  out$temp_hp[is.na(out$temp_hp)] <- 0L
  
  out
}
remove_session_enemy <- function(enemy_uuid) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  enemy_uuid <- as.character(enemy_uuid %||% "")
  if (!nzchar(enemy_uuid)) return(FALSE)
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      DELETE FROM encounter_enemies
      WHERE enemy_uuid = $1
      ",
      params = list(enemy_uuid)
    )
    TRUE
  }, error = function(e) {
    message("remove_session_enemy failed: ", e$message)
    FALSE
  })
}

upsert_enemy_position <- function(session_id, enemy_uuid, x, y) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  enemy_uuid <- as.character(enemy_uuid %||% "")
  x <- suppressWarnings(as.integer(x))
  y <- suppressWarnings(as.integer(y))
  
  if (is.na(session_id) || session_id < 1 || !nzchar(enemy_uuid) || is.na(x) || is.na(y)) {
    return(FALSE)
  }
  
  tryCatch({
    existing <- DBI::dbGetQuery(
      con,
      "
      SELECT id
      FROM enemy_positions
      WHERE session_id = $1
        AND enemy_uuid = $2
      ",
      params = list(session_id, enemy_uuid)
    )
    
    if (nrow(existing) > 0) {
      DBI::dbExecute(
        con,
        "
        UPDATE enemy_positions
        SET x = $1,
            y = $2,
            updated_at = NOW()
        WHERE session_id = $3
          AND enemy_uuid = $4
        ",
        params = list(x, y, session_id, enemy_uuid)
      )
    } else {
      DBI::dbExecute(
        con,
        "
        INSERT INTO enemy_positions (
          session_id,
          enemy_uuid,
          x,
          y,
          updated_at
        )
        VALUES ($1, $2, $3, $4, NOW())
        ",
        params = list(session_id, enemy_uuid, x, y)
      )
    }
    
    TRUE
  }, error = function(e) {
    message("upsert_enemy_position failed: ", e$message)
    FALSE
  })
}
get_enemy_positions <- function(session_id) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  if (is.na(session_id) || session_id < 1) return(data.frame())
  
  res <- tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        id,
        session_id,
        enemy_uuid,
        x,
        y,
        updated_at
      FROM enemy_positions
      WHERE session_id = $1
      ORDER BY id ASC
      ",
      params = list(session_id)
    ),
    error = function(e) {
      message("get_enemy_positions failed: ", e$message)
      data.frame()
    }
  )
  
  if (!is.data.frame(res) || nrow(res) == 0) return(data.frame())
  
  for (nm in intersect(c("id", "session_id", "x", "y"), names(res))) {
    res[[nm]] <- suppressWarnings(as.integer(res[[nm]]))
  }
  res$enemy_uuid <- as.character(res$enemy_uuid %||% "")
  
  res
}

create_session <- function(name, mode = "exploration", status = "active") {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  name <- as.character(name %||% "")
  mode <- as.character(mode %||% "exploration")
  status <- as.character(status %||% "active")
  
  if (!nzchar(name)) return(NULL)
  
  res <- tryCatch(
    DBI::dbGetQuery(
      con,
      "
      INSERT INTO game_sessions (
        name,
        mode,
        status,
        round_number,
        current_turn_order,
        active_actor_type,
        active_actor_id,
        active_map_id,
        created_at,
        updated_at
      )
      VALUES (
        $1, $2, $3,
        1, NULL, NULL, NULL, NULL,
        NOW(), NOW()
      )
      RETURNING id
      ",
      params = list(name, mode, status)
    ),
    error = function(e) {
      message("create_session failed: ", e$message)
      NULL
    }
  )
  
  if (is.null(res) || nrow(res) == 0) return(NULL)
  
  as.integer(res$id[1])
}

Sys.setenv(
  SUPABASE_HOST = "aws-1-eu-west-2.pooler.supabase.com",
  SUPABASE_PORT = "5432",
  SUPABASE_DBNAME = "postgres",
  SUPABASE_USER = "postgres.kymncomirjlvjhcsnfnl",
  SUPABASE_DB_PASSWORD = "defvEf-sufru5-suwnac"
)

get_db_connection <- function() {
  tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("SUPABASE_HOST"),
      port = as.integer(Sys.getenv("SUPABASE_PORT")),
      dbname = Sys.getenv("SUPABASE_DBNAME"),
      user = Sys.getenv("SUPABASE_USER"),
      password = Sys.getenv("SUPABASE_DB_PASSWORD"),
      sslmode = "require"
    ),
    error = function(e) {
      message("DB connection failed: ", e$message)
      NULL
    }
  )
}

restore_sindre <- function(x, hours = 0, full = FALSE, add_log = NULL) {
  x <- validate_character(x)
  `%||%` <- get("%||%", inherits = TRUE)
  
  log_safe <- function(msg, toast = TRUE, flash = "none") {
    if (is.function(add_log)) {
      try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
    } else {
      message(msg)
    }
  }
  
  x$resources <- x$resources %||% list()
  x$resources$sindre <- x$resources$sindre %||% list()
  
  cur   <- suppressWarnings(as.numeric(x$resources$sindre$cur %||% 0))
  total <- suppressWarnings(as.numeric(x$resources$sindre$total %||% 0))
  regen <- suppressWarnings(as.numeric(x$resources$sindre$regen %||% 0))
  
  if (is.na(cur)) cur <- 0
  if (is.na(total)) total <- 0
  if (is.na(regen)) regen <- 0
  
  cur <- max(0, cur)
  total <- max(0, total)
  regen <- max(0, regen)
  
  race <- tolower(trimws(x$meta$race %||% ""))
  
  # Tylwyth Teg do not naturally regenerate Sindre this way
  if (race == "tylwyth teg") {
    return(x)
  }
  
  if (total <= 0) {
    x$resources$sindre$cur <- 0
    return(x)
  }
  
  old_cur <- cur
  
  if (isTRUE(full)) {
    cur <- total
  } else {
    hours <- suppressWarnings(as.numeric(hours %||% 0))
    if (is.na(hours) || hours < 0) hours <- 0
    cur <- min(total, cur + (regen * hours))
  }
  
  x$resources$sindre$cur <- as.integer(round(cur))
  
  gained <- x$resources$sindre$cur - as.integer(round(old_cur))
  if (gained > 0) {
    log_safe(
      paste0("✨ Sindre restored by ", gained, " (", old_cur, " → ", x$resources$sindre$cur, ")."),
      flash = "gold"
    )
  }
  
  x
}

COMBAT_ARMOR_TYPES <- c("Light", "Medium", "Heavy", "Custom")
COMBAT_WEAPON_STATS <- c("str", "dex", "con", "int", "bld_str", "cha")

#For multiplayer HUD
get_session_players <- function(session_id) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        id,
        session_id,
        character_id,
        display_name,
        initiative,
        turn_order,
        is_active,
        joined_at,
        created_at,
        updated_at,
        current_hp,
        temp_hp
      FROM session_players
      WHERE session_id = $1
      ORDER BY turn_order NULLS LAST, joined_at NULLS LAST, id
      ",
      params = list(as.integer(session_id))
    ),
    error = function(e) {
      message('get_session_players failed: ', e$message)
      data.frame()
    }
  )
}

get_character_initiative_bonus <- function(character_id, fallback_char = NULL) {
  ch <- fallback_char
  
  if (is.null(ch)) {
    ch <- load_character_from_db(character_id)
  }
  
  if (is.null(ch)) return(0L)
  
  ch <- validate_character(ch)
  
  dex <- suppressWarnings(as.integer(ch$abilities$dex %||% 10))
  if (is.na(dex)) dex <- 10L
  
  bonus <- mod_calc(dex)
  
  # optional future hook:
  # bonus <- bonus + as.integer(ch$combat$initiative_bonus %||% 0)
  
  as.integer(bonus)
}

set_session_player_initiative <- function(session_id, character_id, initiative, turn_order) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      UPDATE session_players
      SET initiative = $1,
          turn_order = $2,
          updated_at = NOW()
      WHERE session_id = $3
        AND character_id = $4
      ",
      params = list(
        as.integer(initiative),
        as.integer(turn_order),
        as.integer(session_id),
        as.character(character_id)
      )
    )
    TRUE
  }, error = function(e) {
    message('set_session_player_initiative failed: ', e$message)
    FALSE
  })
}

roll_session_initiative <- function(session_id, core_state = NULL) {
  session_id <- suppressWarnings(as.integer(session_id))
  if (is.na(session_id) || session_id < 1) return(data.frame())
  
  players <- get_session_players(session_id)
  enemies <- get_session_enemies(session_id)
  
  if (is.data.frame(players) && nrow(players) > 0 && "is_active" %in% names(players)) {
    players <- players[players$is_active %in% TRUE, , drop = FALSE]
  }
  if (is.data.frame(enemies) && nrow(enemies) > 0 && "is_active" %in% names(enemies)) {
    enemies <- enemies[enemies$is_active %in% TRUE, , drop = FALSE]
  }
  
  actor_rows <- list()
  
  # ----------------------------
  # Players
  # ----------------------------
  if (is.data.frame(players) && nrow(players) > 0) {
    player_out <- lapply(seq_len(nrow(players)), function(i) {
      row <- players[i, , drop = FALSE]
      
      cid <- as.character(row$character_id[1] %||% "")
      nm  <- as.character(row$display_name[1] %||% "Unknown")
      
      fallback_char <- NULL
      if (!is.null(core_state)) {
        if (identical(as.character(core_state$char_id %||% ""), cid)) {
          fallback_char <- core_state$char
        }
      }
      
      dex_bonus <- get_character_initiative_bonus(cid, fallback_char = fallback_char)
      roll <- sample(1:20, 1)
      total <- as.integer(roll + dex_bonus)
      
      data.frame(
        actor_id = cid,
        actor_type = "player",
        display_name = nm,
        initiative_roll = as.integer(roll),
        initiative_bonus = as.integer(dex_bonus),
        initiative_total = as.integer(total),
        stringsAsFactors = FALSE
      )
    })
    
    actor_rows <- c(actor_rows, player_out)
  }
  
  # ----------------------------
  # Enemies
  # ----------------------------
  if (is.data.frame(enemies) && nrow(enemies) > 0) {
    enemy_out <- lapply(seq_len(nrow(enemies)), function(i) {
      row <- enemies[i, , drop = FALSE]
      
      eid <- as.character(row$enemy_uuid[1] %||% "")
      nm  <- as.character(row$name[1] %||% "Enemy")
      
      bonus <- 0L
      roll <- sample(1:20, 1)
      total <- as.integer(roll + bonus)
      
      data.frame(
        actor_id = eid,
        actor_type = "enemy",
        display_name = nm,
        initiative_roll = as.integer(roll),
        initiative_bonus = as.integer(bonus),
        initiative_total = as.integer(total),
        stringsAsFactors = FALSE
      )
    })
    
    actor_rows <- c(actor_rows, enemy_out)
  }
  
  if (!length(actor_rows)) return(data.frame())
  
  init_df <- do.call(rbind, actor_rows)
  
  init_df <- init_df[order(
    -init_df$initiative_total,
    -init_df$initiative_bonus,
    init_df$display_name,
    init_df$actor_id
  ), , drop = FALSE]
  
  init_df$turn_order <- seq_len(nrow(init_df))
  
  # ----------------------------
  # Write back
  # ----------------------------
  init_df$write_ok <- vapply(seq_len(nrow(init_df)), function(i) {
    row <- init_df[i, , drop = FALSE]
    
    if (identical(as.character(row$actor_type[1]), "player")) {
      isTRUE(set_session_player_initiative(
        session_id = session_id,
        character_id = as.character(row$actor_id[1]),
        initiative = as.integer(row$initiative_total[1]),
        turn_order = as.integer(row$turn_order[1])
      ))
    } else {
      isTRUE(set_session_enemy_initiative(
        session_id = session_id,
        enemy_uuid = as.character(row$actor_id[1]),
        initiative = as.integer(row$initiative_total[1]),
        turn_order = as.integer(row$turn_order[1])
      ))
    }
  }, logical(1))
  
  init_df
}

set_session_enemy_initiative <- function(session_id, enemy_uuid, initiative, turn_order) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  `%||%` <- get("%||%", inherits = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  enemy_uuid <- as.character(enemy_uuid %||% "")
  initiative <- suppressWarnings(as.integer(initiative))
  turn_order <- suppressWarnings(as.integer(turn_order))
  
  if (is.na(session_id) || session_id < 1) return(FALSE)
  if (!nzchar(enemy_uuid)) return(FALSE)
  if (is.na(initiative)) initiative <- 0L
  if (is.na(turn_order) || turn_order < 1) turn_order <- 1L
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      UPDATE encounter_enemies
      SET initiative = $1,
          turn_order = $2,
          updated_at = NOW()
      WHERE session_id = $3
        AND enemy_uuid = $4
      ",
      params = list(
        initiative,
        turn_order,
        session_id,
        enemy_uuid
      )
    )
    
    TRUE
  }, error = function(e) {
    message("set_session_enemy_initiative failed: ", e$message)
    FALSE
  })
}

add_character_to_session <- function(session_id, character_id, display_name = NULL, is_active = TRUE) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  `%||%` <- get("%||%", inherits = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  character_id <- as.character(character_id %||% "")
  display_name <- as.character(display_name %||% "")
  is_active <- isTRUE(is_active)
  
  if (is.na(session_id) || session_id < 1) return(FALSE)
  if (!nzchar(character_id)) return(FALSE)
  
  # Load character for defaults
  ch <- tryCatch(
    load_character_from_db(character_id),
    error = function(e) NULL
  )
  
  if (is.null(ch)) return(FALSE)
  
  ch <- tryCatch(validate_character(ch), error = function(e) NULL)
  if (is.null(ch)) return(FALSE)
  
  if (!nzchar(display_name)) {
    display_name <- as.character(ch$meta$name %||% "Unknown")
  }
  
  hp_cur <- suppressWarnings(as.integer(ch$resources$hp$cur %||% 0))
  hp_temp <- suppressWarnings(as.integer(ch$resources$hp$temp %||% 0))
  
  if (is.na(hp_cur)) hp_cur <- 0L
  if (is.na(hp_temp)) hp_temp <- 0L
  
  tryCatch({
    existing <- DBI::dbGetQuery(
      con,
      "
      SELECT id
      FROM session_players
      WHERE session_id = $1
        AND character_id = $2
      ",
      params = list(session_id, character_id)
    )
    
    if (nrow(existing) > 0) {
      DBI::dbExecute(
        con,
        "
        UPDATE session_players
        SET display_name = $1,
            is_active = $2,
            current_hp = $3,
            temp_hp = $4,
            updated_at = NOW()
        WHERE session_id = $5
          AND character_id = $6
        ",
        params = list(
          display_name,
          is_active,
          hp_cur,
          hp_temp,
          session_id,
          character_id
        )
      )
    } else {
      next_turn_order <- DBI::dbGetQuery(
        con,
        "
        SELECT COALESCE(MAX(turn_order), 0) + 1 AS next_turn_order
        FROM session_players
        WHERE session_id = $1
        ",
        params = list(session_id)
      )
      
      turn_order <- suppressWarnings(as.integer(next_turn_order$next_turn_order[1] %||% 1))
      if (is.na(turn_order) || turn_order < 1) turn_order <- 1L
      
      DBI::dbExecute(
        con,
        "
        INSERT INTO session_players (
          session_id,
          character_id,
          display_name,
          initiative,
          turn_order,
          is_active,
          joined_at,
          created_at,
          updated_at,
          current_hp,
          temp_hp
        )
        VALUES (
          $1, $2, $3,
          NULL, $4, $5,
          NOW(), NOW(), NOW(),
          $6, $7
        )
        ",
        params = list(
          session_id,
          character_id,
          display_name,
          turn_order,
          is_active,
          hp_cur,
          hp_temp
        )
      )
    }
    
    TRUE
  }, error = function(e) {
    message("add_character_to_session failed: ", e$message)
    FALSE
  })
}

remove_character_from_session <- function(session_id, character_id) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  character_id <- as.character(character_id %||% "")
  
  if (is.na(session_id) || session_id < 1 || !nzchar(character_id)) return(FALSE)
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      DELETE FROM session_players
      WHERE session_id = $1
        AND character_id = $2
      ",
      params = list(session_id, character_id)
    )
    
    TRUE
  }, error = function(e) {
    message("remove_character_from_session failed: ", e$message)
    FALSE
  })
}

damage_session_enemy <- function(session_id, enemy_uuid, amount) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  enemy_uuid <- as.character(enemy_uuid %||% "")
  dmg <- suppressWarnings(as.integer(amount %||% 0))
  
  if (is.na(session_id) || session_id < 1 || !nzchar(enemy_uuid)) return(NULL)
  if (is.na(dmg) || dmg < 0) dmg <- 0L
  
  tryCatch({
    row <- DBI::dbGetQuery(
      con,
      "
      SELECT hp_current, temp_hp
      FROM encounter_enemies
      WHERE session_id = $1
        AND enemy_uuid = $2
      ",
      params = list(session_id, enemy_uuid)
    )
    
    if (!is.data.frame(row) || nrow(row) == 0) return(NULL)
    
    old_hp <- as.integer(row$hp_current[1] %||% 0)
    old_temp <- as.integer(row$temp_hp[1] %||% 0)
    
    if (is.na(old_hp)) old_hp <- 0L
    if (is.na(old_temp)) old_temp <- 0L
    
    temp_absorb <- min(old_temp, dmg)
    new_temp <- old_temp - temp_absorb
    remaining <- dmg - temp_absorb
    new_hp <- max(0L, old_hp - remaining)
    
    DBI::dbExecute(
      con,
      "
      UPDATE encounter_enemies
      SET hp_current = $1,
          temp_hp = $2,
          updated_at = NOW()
      WHERE session_id = $3
        AND enemy_uuid = $4
      ",
      params = list(new_hp, new_temp, session_id, enemy_uuid)
    )
    
    list(
      hp_before = old_hp,
      hp_after = new_hp,
      temp_before = old_temp,
      temp_after = new_temp,
      amount = dmg
    )
  }, error = function(e) {
    message("damage_session_enemy failed: ", e$message)
    NULL
  })
}

start_session_combat <- function(session_id, core_state = NULL) {
  init_df <- roll_session_initiative(session_id, core_state = core_state)
  if (!is.data.frame(init_df) || nrow(init_df) == 0) return(NULL)
  
  first <- init_df[1, , drop = FALSE]
  
  ok_state <- set_combat_state(
    session_id = session_id,
    round_number = 1,
    current_turn_order = as.integer(first$turn_order[1]),
    active_actor_type = "player",
    active_actor_id = as.character(first$character_id[1]),
    phase = "combat"
  )
  
  if (!isTRUE(ok_state)) return(NULL)
  
  log_game_event(
    session_id = session_id,
    event_type = "enter_combat",
    actor_type = "system",
    payload = list(
      round_number = 1,
      active_actor_id = as.character(first$character_id[1]),
      initiative = apply(init_df[, c("display_name", "initiative_roll", "initiative_bonus", "initiative_total", "turn_order"), drop = FALSE], 1, as.list)
    )
  )
  
  init_df
}

get_equipped_weapons_for_combat <- function(char) {
  char <- validate_character(char)
  items <- inventory_get(char)
  
  if (!is.data.frame(items) || nrow(items) == 0) return(data.frame())
  
  is_weapon <- tolower(as.character(items$type %||% "")) == "weapon"
  is_equipped <- as.logical(items$equipped %||% FALSE)
  in_bag <- as.logical(items$in_bag %||% FALSE)
  
  is_equipped[is.na(is_equipped)] <- FALSE
  in_bag[is.na(in_bag)] <- FALSE
  
  items <- items[
    is_weapon & is_equipped & !in_bag,
    ,
    drop = FALSE
  ]
  
  if (nrow(items) == 0) return(data.frame())
  
  rows <- lapply(seq_len(nrow(items)), function(i) {
    row <- items[i, , drop = FALSE]
    meta <- row$meta[[1]] %||% list()
    
    data.frame(
      id = as.character(row$id[1] %||% paste0("weapon_", i)),
      name = as.character(row$name[1] %||% "Weapon"),
      stat = as.character(meta$stat %||% "str"),
      adv = as.character(meta$adv %||% "Normal"),
      to_hit_bonus = as.numeric(meta$to_hit_bonus %||% 0),
      damage1 = as.character(meta$damage1 %||% "1d4"),
      dmg_type1 = as.character(meta$dmg_type1 %||% ""),
      damage2 = as.character(meta$damage2 %||% ""),
      dmg_type2 = as.character(meta$dmg_type2 %||% ""),
      proficient = isTRUE(meta$proficient %||% FALSE),
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
ensure_session_player_hp <- function(session_id, character_id, fallback_char = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch({
    row <- DBI::dbGetQuery(
      con,
      "
      SELECT current_hp, temp_hp
      FROM session_players
      WHERE session_id = $1
        AND character_id = $2
      ",
      params = list(as.integer(session_id), as.character(character_id))
    )
    
    if (nrow(row) == 0) return(FALSE)
    
    needs_hp <- is.na(row$current_hp[1])
    
    if (!needs_hp) return(TRUE)
    
    if (is.null(fallback_char)) {
      fallback_char <- load_character_from_db(character_id)
    }
    
    if (is.null(fallback_char)) return(FALSE)
    
    fallback_char <- validate_character(fallback_char)
    
    hp_cur <- as.integer(fallback_char$resources$hp$cur %||% 0)
    hp_tmp <- as.integer(fallback_char$resources$hp$temp %||% 0)
    
    DBI::dbExecute(
      con,
      "
      UPDATE session_players
      SET current_hp = $1,
          temp_hp = $2,
          updated_at = NOW()
      WHERE session_id = $3
        AND character_id = $4
      ",
      params = list(
        hp_cur,
        hp_tmp,
        as.integer(session_id),
        as.character(character_id)
      )
    )
    
    TRUE
  }, error = function(e) {
    message("ensure_session_player_hp failed: ", e$message)
    FALSE
  })
}

is_session_active_for_character <- function(state) {
  !is.null(state$active_session_id) && !is.null(state$char_id)
}

get_effective_hp_state <- function(state) {
  x <- validate_character(state$char)
  
  hp <- x$resources$hp %||% list(max = 0, cur = 0, temp = 0)
  
  out <- list(
    max = as.integer(hp$max %||% 0),
    cur = as.integer(hp$cur %||% 0),
    temp = as.integer(hp$temp %||% 0),
    source = "character"
  )
  
  if (is_session_active_for_character(state)) {
    row <- get_session_player_row(state$active_session_id, state$char_id)
    
    if (nrow(row) > 0) {
      if ("current_hp" %in% names(row) && !is.na(row$current_hp[1])) {
        out$cur <- as.integer(row$current_hp[1])
        out$source <- "session"
      }
      if ("temp_hp" %in% names(row) && !is.na(row$temp_hp[1])) {
        out$temp <- as.integer(row$temp_hp[1])
        out$source <- "session"
      }
    }
  }
  
  out$max  <- max(0L, out$max)
  out$cur  <- max(0L, out$cur)
  out$temp <- max(0L, out$temp)
  
  # ✅ clamp current HP to max HP
  if (out$cur > out$max) {
    out$cur <- out$max
  }
  
  out
}

clamp_session_hp_to_max <- function(state) {
  if (!is_session_active_for_character(state)) return(FALSE)
  
  x <- validate_character(state$char)
  max_hp <- as.integer(x$resources$hp$max %||% 0)
  
  row <- get_session_player_row(state$active_session_id, state$char_id)
  if (nrow(row) == 0) return(FALSE)
  
  cur_hp  <- as.integer(row$current_hp[1] %||% 0)
  temp_hp <- as.integer(row$temp_hp[1] %||% 0)
  
  if (cur_hp <= max_hp) return(TRUE)
  
  ok <- set_session_hp(
    session_id = state$active_session_id,
    character_id = state$char_id,
    current_hp = max_hp,
    temp_hp = temp_hp
  )
  
  isTRUE(ok)
}

set_effective_hp_state <- function(state, cur = NULL, temp = NULL) {
  x <- validate_character(state$char)
  hp <- get_effective_hp_state(state)
  
  new_cur <- if (is.null(cur)) hp$cur else as.integer(cur)
  new_temp <- if (is.null(temp)) hp$temp else as.integer(temp)
  
  new_cur <- max(0L, new_cur)
  new_temp <- max(0L, new_temp)
  
  if (is_session_active_for_character(state)) {
    ok <- set_session_hp(
      session_id = state$active_session_id,
      character_id = state$char_id,
      current_hp = new_cur,
      temp_hp = new_temp
    )
    return(isTRUE(ok))
  }
  
  x$resources$hp$cur <- new_cur
  x$resources$hp$temp <- new_temp
  state$char <- x
  TRUE
}

get_session_encounters <- function(session_id) {
  session_id <- suppressWarnings(as.integer(session_id))
  if (is.na(session_id) || session_id < 1) {
    return(data.frame())
  }
  
  con <- get_db_connection()
  if (is.null(con)) {
    return(data.frame())
  }
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch({
    DBI::dbGetQuery(
      con,
      "
      SELECT
        id AS encounter_id,
        session_id,
        name,
        map_id,
        status,
        created_at,
        updated_at
      FROM encounters
      WHERE session_id = $1
      ORDER BY id DESC
      ",
      params = list(session_id)
    )
  }, error = function(e) {
    message("get_session_encounters failed: ", e$message)
    data.frame()
  })
}
apply_damage_to_state <- function(state, amount) {
  hp <- get_effective_hp_state(state)
  
  dmg <- max(0L, as.integer(amount %||% 0))
  old_hp <- hp$cur
  old_temp <- hp$temp
  
  temp_absorb <- min(old_temp, dmg)
  new_temp <- old_temp - temp_absorb
  remaining <- dmg - temp_absorb
  new_hp <- max(0L, old_hp - remaining)
  
  ok <- set_effective_hp_state(state, cur = new_hp, temp = new_temp)
  if (!isTRUE(ok)) return(NULL)
  
  list(
    hp_before = old_hp,
    hp_after = new_hp,
    temp_before = old_temp,
    temp_after = new_temp,
    amount = dmg,
    source = hp$source
  )
}

apply_healing_to_state <- function(state, amount) {
  hp <- get_effective_hp_state(state)
  
  heal <- max(0L, as.integer(amount %||% 0))
  old_hp <- hp$cur
  new_hp <- min(as.integer(hp$max %||% 0), old_hp + heal)
  
  ok <- set_effective_hp_state(state, cur = new_hp, temp = hp$temp)
  if (!isTRUE(ok)) return(NULL)
  
  list(
    hp_before = old_hp,
    hp_after = new_hp,
    temp_before = hp$temp,
    temp_after = hp$temp,
    amount = heal,
    source = hp$source
  )
}
get_session_player_row <- function(session_id, character_id) {
  players <- get_session_players(session_id)
  if (nrow(players) == 0) return(data.frame())
  
  row <- players[players$character_id == as.character(character_id), , drop = FALSE]
  row
}

is_db_available <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  
  ok <- tryCatch({
    DBI::dbGetQuery(con, "SELECT 1 AS ok")
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  try(DBI::dbDisconnect(con), silent = TRUE)
  ok
}

#Inventory
inventory_normalize <- function(df = NULL) {
  tmpl <- inventory_empty()
  
  if (is.null(df) || !is.data.frame(df)) {
    return(tmpl)
  }
  
  # add missing columns
  for (nm in names(tmpl)) {
    if (!nm %in% names(df)) {
      if (nm == "meta") {
        df[[nm]] <- vector("list", nrow(df))
      } else {
        df[[nm]] <- tmpl[[nm]]
      }
    }
  }
  
  df <- df[, names(tmpl), drop = FALSE]
  
  df$id       <- as.character(df$id)
  df$name     <- as.character(df$name)
  df$type     <- as.character(df$type)
  df$desc     <- as.character(df$desc)
  df$value    <- suppressWarnings(as.numeric(df$value))
  df$weight   <- suppressWarnings(as.numeric(df$weight))
  df$qty      <- suppressWarnings(as.numeric(df$qty))
  df$equipped <- as.logical(df$equipped)
  df$in_bag   <- as.logical(df$in_bag)
  df$edit     <- as.logical(df$edit)
  
  df$value[is.na(df$value)] <- 0
  df$weight[is.na(df$weight)] <- 0
  df$qty[is.na(df$qty)] <- 1
  df$equipped[is.na(df$equipped)] <- FALSE
  df$in_bag[is.na(df$in_bag)] <- FALSE
  df$edit[is.na(df$edit)] <- FALSE
  
  if (!"meta" %in% names(df)) {
    df$meta <- vector("list", nrow(df))
  }
  
  for (i in seq_len(nrow(df))) {
    if (!is.list(df$meta[[i]])) {
      df$meta[[i]] <- list()
    }
  }
  
  df
}

inventory_get <- function(x) {
  x <- validate_character(x)
  inventory_normalize(x$inventory$items)
}

load_character_from_db <- function(char_id) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  res <- tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT state_blob
      FROM character_blobs
      WHERE id = $1
      ",
      params = list(char_id)
    ),
    error = function(e) {
      message("load_character_from_db failed: ", e$message)
      NULL
    }
  )
  
  if (is.null(res) || nrow(res) == 0) return(NULL)
  
  tryCatch(
    unserialize(res$state_blob[[1]]),
    error = function(e) {
      message("unserialize failed: ", e$message)
      NULL
    }
  )
}

list_characters_in_db <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  df <- tryCatch({
    DBI::dbGetQuery(con, "
      SELECT 
        id AS character_id,
        char_name AS name
      FROM character_blobs
      ORDER BY updated_at DESC
    ")
  }, error = function(e) {
    message("list_characters_in_db failed: ", e$message)
    return(data.frame())
  })
  
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
  
  df$character_id <- as.character(df$character_id)
  df$name <- as.character(df$name)
  
  df
}

ctrl <- reactiveValues(
  session_id = NULL,
  encounter_id = NULL,   # 🔥 NEW
  map_id = NULL,
  selected_actor_id = NULL,
  selected_tile = NULL,
  refresh_key = 0L
)

save_character_to_db <- function(char, char_id = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  raw <- serialize(char, NULL)
  name <- char$meta$name %||% "Unnamed"
  
  tryCatch({
    if (is.null(char_id)) {
      res <- DBI::dbGetQuery(
        con,
        "
        INSERT INTO character_blobs (player_id, char_name, state_blob)
        VALUES ($1, $2, $3)
        RETURNING id
        ",
        params = list("shared", name, list(raw))
      )
      as.character(res$id[[1]])
    } else {
      DBI::dbExecute(
        con,
        "
        UPDATE character_blobs
        SET state_blob = $1,
            char_name = $2,
            updated_at = NOW()
        WHERE id = $3
        ",
        params = list(list(raw), name, char_id)
      )
      as.character(char_id)
    }
  }, error = function(e) {
    message("save_character_to_db failed: ", e$message)
    NULL
  })
}

inventory_empty <- function() data.frame(
  id = character(),
  name = character(),
  type = character(),   # "weapon", "armor", "consumable", etc.
  desc = character(),
  value = numeric(),
  weight = numeric(),
  qty = numeric(),
  equipped = logical(),
  in_bag = logical(),
  meta = I(list()),     # IMPORTANT: list column
  edit = logical(),
  stringsAsFactors = FALSE
)

mod_calc <- function(score) {
  if (is.null(score) || is.na(score) || score == "") return(0)
  score <- suppressWarnings(as.numeric(score))
  if (is.na(score)) return(0)
  floor((score - 10) / 2)
}

# ----------------------------
# DATE
# ----------------------------
get_celtic_date <- function(day) {
  day <- day %||% 1
  day_in_year <- ((day - 1) %% 360) + 1
  year <- ((day - 1) %/% 360) + 1
  
  festivals <- c("Samhain","Midwinter","Imbolc","Spring","Beltane","Midsummer","Lughnasadh","Autumn")
  index <- ((day_in_year - 1) %/% 45) + 1
  
  list(
    year = year,
    season = festivals[index],
    day_of_season = ((day_in_year - 1) %% 45) + 1
  )
}

get_items <- function(x, type = NULL, active_only = FALSE) {
  x <- validate_character(x)
  df <- inventory_normalize(x$inventory$items)
  
  if (!is.null(type)) {
    df <- df[df$type == type, , drop = FALSE]
  }
  
  if (active_only) {
    in_bag <- as.logical(df$in_bag)
    in_bag[is.na(in_bag)] <- FALSE
    df <- df[!in_bag, , drop = FALSE]
  }
  
  df
}




weapon_meta_defaults_global <- function(meta = NULL) {
  meta <- meta %||% list()
  if (!is.list(meta)) meta <- list()
  
  meta$stat <- as.character(meta$stat %||% "str")
  if (!meta$stat %in% COMBAT_WEAPON_STATS) meta$stat <- "str"
  
  meta$adv <- as.character(meta$adv %||% "Normal")
  if (!meta$adv %in% c("Normal", "Adv", "Disadv")) meta$adv <- "Normal"
  
  meta$to_hit_bonus <- suppressWarnings(as.numeric(meta$to_hit_bonus %||% 0))
  if (is.na(meta$to_hit_bonus)) meta$to_hit_bonus <- 0
  
  meta$damage1 <- as.character(meta$damage1 %||% "1d6")
  meta$dmg_type1 <- as.character(meta$dmg_type1 %||% "Slashing")
  meta$damage2 <- as.character(meta$damage2 %||% "")
  meta$dmg_type2 <- as.character(meta$dmg_type2 %||% "Other")
  meta$proficient <- isTRUE(meta$proficient)
  
  meta
}

get_character_prof_bonus <- function(char) {
  char <- validate_character(char)
  lvl <- suppressWarnings(as.integer(char$build$level %||% 1))
  if (is.na(lvl) || lvl < 1) lvl <- 1L
  ceiling(lvl / 4) + 1L
}

get_character_ability_mod <- function(char, stat) {
  char <- validate_character(char)
  mod_calc(char$abilities[[stat]] %||% 10)
}

get_equipped_weapons_for_combat <- function(char) {
  char <- validate_character(char)
  df <- inventory_normalize(char$inventory$items)
  
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
  
  in_bag <- as.logical(df$in_bag)
  in_bag[is.na(in_bag)] <- FALSE
  df$in_bag <- in_bag
  
  equipped <- as.logical(df$equipped)
  equipped[is.na(equipped)] <- FALSE
  df$equipped <- equipped
  
  df <- df[
    df$type == "weapon" &
      df$equipped &
      !df$in_bag,
    ,
    drop = FALSE
  ]
  
  if (nrow(df) == 0) return(data.frame())
  
  rows <- lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = FALSE]
    meta <- weapon_meta_defaults_global(row$meta[[1]])
    
    data.frame(
      id = as.character(row$id[1] %||% paste0("weapon_", i)),
      name = as.character(row$name[1] %||% "Weapon"),
      stat = as.character(meta$stat),
      adv = as.character(meta$adv),
      to_hit_bonus = as.numeric(meta$to_hit_bonus),
      damage1 = as.character(meta$damage1),
      dmg_type1 = as.character(meta$dmg_type1),
      damage2 = as.character(meta$damage2),
      dmg_type2 = as.character(meta$dmg_type2),
      proficient = isTRUE(meta$proficient),
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

get_weapon_hit_bonus <- function(char, weapon_row) {
  char <- validate_character(char)
  
  stat <- as.character(weapon_row$stat[1] %||% "str")
  stat_mod <- get_character_ability_mod(char, stat)
  prof_bonus <- if (isTRUE(weapon_row$proficient[1] %||% FALSE)) get_character_prof_bonus(char) else 0L
  flat_bonus <- suppressWarnings(as.integer(weapon_row$to_hit_bonus[1] %||% 0))
  if (is.na(flat_bonus)) flat_bonus <- 0L
  
  as.integer(stat_mod + prof_bonus + flat_bonus)
}

armor_meta_defaults_global <- function(meta = NULL) {
  meta <- meta %||% list()
  if (!is.list(meta)) meta <- list()
  
  meta$base_ac <- suppressWarnings(as.numeric(meta$base_ac %||% 11))
  if (is.na(meta$base_ac)) meta$base_ac <- 11
  
  meta$type <- as.character(meta$type %||% "Light")
  if (!meta$type %in% COMBAT_ARMOR_TYPES) meta$type <- "Light"
  
  meta$custom_max_dex <- suppressWarnings(as.numeric(meta$custom_max_dex %||% 0))
  if (is.na(meta$custom_max_dex)) meta$custom_max_dex <- 0
  
  meta$proficient <- isTRUE(meta$proficient)
  
  meta
}

calc_auto_ac_for_char <- function(char) {
  char <- validate_character(char)
  df <- inventory_normalize(char$inventory$items)
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(10 + get_character_ability_mod(char, "dex"))
  }
  
  equipped <- as.logical(df$equipped)
  equipped[is.na(equipped)] <- FALSE
  in_bag <- as.logical(df$in_bag)
  in_bag[is.na(in_bag)] <- FALSE
  
  arm <- df[
    df$type == "armor" &
      equipped &
      !in_bag,
    ,
    drop = FALSE
  ]
  
  dex_mod <- get_character_ability_mod(char, "dex")
  pb <- get_character_prof_bonus(char)
  
  if (nrow(arm) == 0) {
    return(10 + dex_mod)
  }
  
  row <- arm[1, , drop = FALSE]
  meta <- armor_meta_defaults_global(row$meta[[1]])
  
  type <- meta$type %||% "Light"
  base_ac <- as.numeric(meta$base_ac %||% 10)
  prof <- isTRUE(meta$proficient)
  
  max_dex <- switch(
    type,
    "Light" = Inf,
    "Medium" = 2,
    "Heavy" = 0,
    "Custom" = as.numeric(meta$custom_max_dex %||% 0),
    Inf
  )
  
  dex_add <- min(dex_mod, max_dex)
  as.integer(base_ac + dex_add + if (prof) pb else 0)
}

roll_dice_expr <- function(expr) {
  expr <- gsub("\\s+", "", as.character(expr %||% ""))
  if (!nzchar(expr)) {
    return(list(total = 0L, rolls = integer(0), mod = 0L, expr = expr))
  }
  
  m <- regexec("^([0-9]+)d([0-9]+)([+-][0-9]+)?$", expr)
  parts <- regmatches(expr, m)[[1]]
  
  if (length(parts) == 0) {
    return(list(total = 0L, rolls = integer(0), mod = 0L, expr = expr))
  }
  
  n <- as.integer(parts[2])
  d <- as.integer(parts[3])
  mod <- if (length(parts) >= 4 && nzchar(parts[4])) as.integer(parts[4]) else 0L
  
  if (is.na(n) || is.na(d) || n <= 0 || d <= 0) {
    return(list(total = 0L, rolls = integer(0), mod = 0L, expr = expr))
  }
  
  rolls <- sample.int(d, n, replace = TRUE)
  total <- sum(rolls) + mod
  
  list(
    total = as.integer(total),
    rolls = as.integer(rolls),
    mod = as.integer(mod),
    expr = expr
  )
}

roll_attack_d20 <- function(adv = "Normal") {
  adv <- as.character(adv %||% "Normal")
  if (!adv %in% c("Normal", "Adv", "Disadv")) adv <- "Normal"
  
  if (adv == "Normal") {
    r <- sample(1:20, 1)
    return(list(roll = as.integer(r), rolls = c(as.integer(r))))
  }
  
  r1 <- sample(1:20, 1)
  r2 <- sample(1:20, 1)
  
  final <- if (adv == "Adv") max(r1, r2) else min(r1, r2)
  
  list(
    roll = as.integer(final),
    rolls = c(as.integer(r1), as.integer(r2))
  )
}



required_intake <- function(a) {
  stage <- as.integer(a$stage %||% 1)
  prev  <- as.numeric(a$previous_day_intake %||% 0)
  
  if (stage == 1) {
    return(1)
  } else if (stage == 2) {
    return(prev)
  } else if (stage == 3) {
    return(prev + 1)
  } else {
    return(max(5, prev))  # tweak later if needed
  }
}

advance_blood_day <- function(x, add_log = NULL, state = NULL) {
  x <- validate_character(x)
  
  `%||%` <- get("%||%", inherits = TRUE)
  
  log_safe <- function(msg, toast = TRUE, flash = "none") {
    if (is.function(add_log)) {
      try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
    } else {
      message(msg)
    }
  }
  
  # ----------------------------
  # Ensure schema
  # ----------------------------
  x$status <- x$status %||% list()
  x$status$exhaustion <- as.integer(x$status$exhaustion %||% 0)
  if (is.na(x$status$exhaustion)) x$status$exhaustion <- 0
  x$status$bloodlust <- FALSE
  
  x$resources <- x$resources %||% list()
  x$resources$blood <- x$resources$blood %||% list()
  x$resources$blood$addiction <- x$resources$blood$addiction %||% list()
  
  a <- x$resources$blood$addiction
  
  stage <- as.integer(a$stage %||% 1)
  prev  <- as.numeric(a$previous_day_intake %||% 0)
  today <- as.numeric(a$current_day_intake %||% 0)
  days  <- as.integer(a$days_at_stage %||% 0)
  
  if (is.na(stage)) stage <- 1
  if (is.na(prev))  prev  <- 0
  if (is.na(today)) today <- 0
  if (is.na(days))  days  <- 0
  
  req <- as.numeric(required_intake(a))
  if (is.na(req) || req < 0) req <- 1
  
  deficit <- req - today
  new_stage <- stage
  
  log_safe(
    paste0(
      "DEBUG blood day: stage=", stage,
      ", prev=", round(prev, 2),
      ", today=", round(today, 2),
      ", req=", round(req, 2)
    ),
    toast = FALSE
  )
  
  # ----------------------------
  # WITHDRAWAL
  # ----------------------------
  if (today < req) {
    
    if (stage == 1) {
      hp_loss <- sample(1:6, 1)
      
      if (!is.null(state)) {
        res <- apply_damage_to_state(state, hp_loss)
        
        if (!is.null(res)) {
          log_safe(paste0("💢 Withdrawal: Lost ", res$hp_before - res$hp_after, " HP"))
          x <- validate_character(state$char)
        } else {
          x$resources$hp$cur <- max(0, (x$resources$hp$cur %||% 0) - hp_loss)
          log_safe(paste0("💢 Withdrawal: Lost ", hp_loss, " HP"))
        }
      } else {
        x$resources$hp$cur <- max(0, (x$resources$hp$cur %||% 0) - hp_loss)
        log_safe(paste0("💢 Withdrawal: Lost ", hp_loss, " HP"))
      }
      
    } else if (stage == 2) {
      gain <- if (deficit >= 1) 1 else 0
      x$status$exhaustion <- min(6, x$status$exhaustion + gain)
      log_safe("😵 Blood imbalance causes exhaustion.", flash = "red")
      
    } else if (stage >= 3) {
      x$status$exhaustion <- min(6, x$status$exhaustion + 2)
      x$status$bloodlust <- TRUE
      log_safe("🩸 Blood starvation overwhelms you...", flash = "red")
    }
  }
  
  # ----------------------------
  # PROGRESSION
  # ----------------------------
  if (today > req * 1.5) {
    new_stage <- min(4, new_stage + 1)
    log_safe("🩸 Your dependence on blood deepens...", flash = "gold")
  }
  
  # ----------------------------
  # RECOVERY
  # ----------------------------
  if (today >= req && today <= prev) {
    roll <- sample(1:20, 1)
    log_safe(paste0("🧠 Recovery roll: ", roll))
    
    if (roll >= 18 && new_stage > 1) {
      new_stage <- new_stage - 1
      log_safe(paste0("✨ Addiction lowered to stage ", new_stage))
      
    } else if (roll == 1 && new_stage < 4) {
      new_stage <- new_stage + 1
      log_safe(paste0("🩸 Addiction worsened to stage ", new_stage))
    }
  }
  
  # ----------------------------
  # COMMIT
  # ----------------------------
  a$stage <- new_stage
  a$previous_day_intake <- today
  a$current_day_intake <- 0
  a$days_at_stage <- if (new_stage == stage) (days + 1) else 0
  
  x$resources$blood$addiction <- a
  
  log_safe(paste0(
    "📅 Blood check → Intake: ", round(today, 2),
    " / ", round(req, 2),
    " | Stage: ", new_stage
  ))
  
  x
}

advance_day_all <- function(x, add_log = NULL, state = NULL) {
  x <- validate_character(x)
  
  `%||%` <- get("%||%", inherits = TRUE)
  
  log_safe <- function(msg, toast = TRUE, flash = "none") {
    if (is.function(add_log)) {
      try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
    } else {
      message(msg)
    }
  }
  
  # ============================================================
  # INITIALISE
  # ============================================================
  x$resources   <- x$resources   %||% list()
  x$status      <- x$status      %||% list()
  x$environment <- x$environment %||% list(temperature = "temperate")
  
  if (isTRUE(x$status$resting)) {
    x$status$exhaustion <- max(0, x$status$exhaustion - 1)
    log_safe("😌 Rest reduces exhaustion.", flash = "green")
  }
  
  x$status$exhaustion <- as.integer(x$status$exhaustion %||% 0)
  if (is.na(x$status$exhaustion) || x$status$exhaustion < 0) {
    x$status$exhaustion <- 0
  }
  
  # ============================================================
  # 🍖 RATIONS
  # ============================================================
  x$resources$rations <- x$resources$rations %||% list(cur = 3, max = 5)
  x$resources$rations$cur <- as.numeric(x$resources$rations$cur %||% 0)
  x$resources$rations$max <- as.numeric(x$resources$rations$max %||% 5)
  
  if (is.na(x$resources$rations$cur) || x$resources$rations$cur < 0)
    x$resources$rations$cur <- 0
  
  ate_today <- isTRUE(x$status$ate_today %||% FALSE)
  
  # ============================================================
  # 💧 WATER
  # ============================================================
  x$resources$water <- x$resources$water %||% list(cur = 0, max = 5)
  x$resources$water$cur <- as.numeric(x$resources$water$cur %||% 0)
  x$resources$water$max <- as.numeric(x$resources$water$max %||% 5)
  
  if (is.na(x$resources$water$cur) || x$resources$water$cur < 0)
    x$resources$water$cur <- 0
  
  drank_today <- isTRUE(x$status$drank_today %||% FALSE)
  
  # ============================================================
  # 😵 HUNGER
  # ============================================================
  hunger_days <- as.integer(x$status$hunger_days %||% 0)
  
  if (!ate_today) {
    hunger_days <- hunger_days + 1
    
    if (hunger_days == 1) {
      log_safe("🍽️ You feel hungry.")
    }
    
    if (hunger_days >= 3) {
      x$status$exhaustion <- min(6, x$status$exhaustion + 1)
      log_safe("😵 Starvation causes exhaustion.", flash = "red")
    }
    
  } else {
    hunger_days <- max(0, hunger_days - 1)
    log_safe("🍖 You feel nourished.", flash = "green")
  }
  
  x$status$hunger_days <- hunger_days
  
  # ============================================================
  # 🪵 WOOD
  # ============================================================
  x$resources$wood <- x$resources$wood %||% list(cur = 3, max = 10)
  x$resources$wood$cur <- as.numeric(x$resources$wood$cur %||% 0)
  x$resources$wood$max <- as.numeric(x$resources$wood$max %||% 10)
  
  if (is.na(x$resources$wood$cur) || x$resources$wood$cur < 0)
    x$resources$wood$cur <- 0
  
  # ============================================================
  # 🥵 DEHYDRATION
  # ============================================================
  dehydration_days <- as.integer(x$status$dehydration_days %||% 0)
  
  if (!drank_today) {
    dehydration_days <- dehydration_days + 1
    
    if (dehydration_days == 1) {
      log_safe("🥵 You feel thirsty.", flash = "gold")
    }
    
    if (dehydration_days >= 2) {
      x$status$exhaustion <- min(6, x$status$exhaustion + 1)
      log_safe("💀 Dehydration causes exhaustion.", flash = "red")
    }
    
  } else {
    dehydration_days <- max(0, dehydration_days - 1)
    log_safe("💧 You feel refreshed.", flash = "blue")
  }
  
  x$status$dehydration_days <- dehydration_days
  
  # ============================================================
  # ❄️ TEMPERATURE
  # ============================================================
  temp <- tolower(x$environment$temperature %||% "temperate")
  has_fire <- isTRUE(x$status$has_fire %||% FALSE)
  
  race <- tolower(trimws(x$meta$race %||% ""))
  
  if (race == "tylwyth teg") {
    if (temp == "extreme_cold") {
      temp <- "cold"
    } else if (temp == "cold") {
      temp <- "temperate"
    }
  }
  
  if (race == "tylwyth teg" && temp != tolower(x$environment$temperature %||% "temperate")) {
    log_safe("🌿 Your fey nature softens the cold.", flash = "green")
  }
  
  cold_days <- as.integer(x$status$cold_days %||% 0)
  
  if (temp %in% c("cold", "extreme_cold")) {
    
    if (has_fire) {
      log_safe("🔥 Your fire keeps you warm.", flash = "green")
      cold_days <- max(0, cold_days - 1)
      
    } else {
      cold_days <- cold_days + 1
      
      if (cold_days == 1) {
        log_safe("❄️ The cold bites at you.", flash = "blue")
      }
      
      if (temp == "extreme_cold") {
        x$status$exhaustion <- min(6, x$status$exhaustion + 2)
        log_safe("🧊 Extreme cold drains you rapidly!", flash = "blue")
        
      } else if (cold_days >= 2) {
        x$status$exhaustion <- min(6, x$status$exhaustion + 1)
        log_safe("🧊 Cold exposure causes exhaustion.", flash = "blue")
      }
    }
    
  } else {
    cold_days <- max(0, cold_days - 1)
  }
  
  x$status$cold_days <- cold_days
  
  # ============================================================
  # 🩸 BLOOD SYSTEM
  # ============================================================
  race <- tolower(trimws(x$meta$race %||% ""))
  
  if (race == "tylwyth teg") {
    x <- advance_blood_day(x, add_log = add_log, state = state)
  }
  
  # ============================================================
  # APPLY EFFECTS
  # ============================================================
  x$status$exhaustion <- min(6, x$status$exhaustion)
  x <- sync_exhaustion_effects(x)
  
  # ============================================================
  # ADVANCE DAY
  # ============================================================
  x$meta <- x$meta %||% list()
  day <- as.integer(x$meta$day %||% 1)
  x$meta$day <- day + 1
  
  log_safe(paste0("📅 Day advanced to ", x$meta$day), toast = TRUE)
  
  # ============================================================
  # RESET FLAGS
  # ============================================================
  x$status$ate_today <- FALSE
  x$status$drank_today <- FALSE
  x$status$foraged_today <- FALSE
  x$status$resting <- FALSE
  x$status$has_fire <- FALSE
  x$status$gathered_wood_today <- FALSE
  
  x$resources$sindre <- x$resources$sindre %||% list()
  x$resources$sindre$temp <- 0
  
  x
}
sync_exhaustion_effects <- function(x) {
  x$status <- x$status %||% list()
  x$status$effects <- x$status$effects %||% character()
  
  if (!is.list(x$status)) x$status <- list()
  x$status$exhaustion <- as.integer(x$status$exhaustion %||% 0)
  
  ex <- as.integer(x$status$exhaustion %||% 0)
  # Remove old exhaustion labels
  x$status$effects <- x$status$effects[!grepl("^Exhaustion", x$status$effects)]
  
  # Add updated label
  if (ex > 0) {
    x$status$effects <- c(x$status$effects, paste0("Exhaustion (", ex, ")"))
  }
  
  x
}

#Status engine
get_status_modifiers <- function(x) {
  x <- validate_character(x)
  
  effects <- x$status$effects %||% character(0)
  exhaustion <- as.integer(x$status$exhaustion %||% 0)
  
  # --- Inject exhaustion into effects ---
  if (exhaustion > 0) {
    effects <- c(effects, paste0("Exhaustion (", exhaustion, ")"))
  }
  
  list(
    ability_disadv =
      "Poisoned" %in% effects ||
      exhaustion >= 1,
    
    save_disadv =
      exhaustion >= 3 ||
      "Restrained" %in% effects,
    
    auto_fail_str_dex =
      "Stunned" %in% effects ||
      "Unconscious" %in% effects,
    
    hp_max_halved =
      exhaustion >= 4,
    
    speed_zero =
      exhaustion >= 5 ||
      "Grappled" %in% effects,
    
    dead =
      exhaustion >= 6,
    
    attack_disadv =
      exhaustion >= 3 ||
      "Poisoned" %in% effects ||
      "Blinded" %in% effects ||
      "Restrained" %in% effects ||
      "Prone" %in% effects
  )
}

# ----------------------------
# Combat schema helpers
# ----------------------------
combat_empty_weapons <- function() data.frame(
  id = character(),
  name = character(),
  stat = character(),
  adv = character(),            # Normal / Adv / Disadv
  to_hit_bonus = numeric(),
  damage1 = character(),
  dmg_type1 = character(),
  damage2 = character(),
  dmg_type2 = character(),
  proficient = logical(),
  in_bag = logical(),
  edit = logical(),
  stringsAsFactors = FALSE
)

combat_empty_armors <- function() data.frame(
  id = character(),
  name = character(),
  base_ac = numeric(),
  type = character(),           # Light / Medium / Heavy / Custom
  custom_max_dex = numeric(),   # used only for Custom
  proficient = logical(),
  worn = logical(),
  in_bag = logical(),
  edit = logical(),
  stringsAsFactors = FALSE
)

# ----------------------------
# New character
# ----------------------------
new_character <- function() {
  list(
    save_version = APP_SAVE_VERSION,
    
    meta = list(
      race = "",
      name = "",
      created_at = Sys.time(),
      updated_at = Sys.time()
    ),
    
    build = list(
      class = "",
      path  = "",
      level = 1
    ),
    
    abilities = list(
      str = 10, dex = 10, con = 10, int = 10, cha = 10,
      bld_str = 10
    ),
    
    prof = list(
      saves  = list(),
      skills = list(),
      skill_cards = list()
    ),
    
    resources = list(
      hp = list(max = 10, cur = 10, temp = 0),
      
      # ✅ BLOOD NOW IN THE RIGHT PLACE
      blood = list(
        inventory = data.frame(
          id     = character(),
          pints  = numeric(),
          sindre = numeric(),
          source = character(),
          stringsAsFactors = FALSE
        ),
        addiction = list(
          stage = 1,
          days_at_stage = 0,
          previous_day_intake = 0,
          current_day_intake = 0
        )
      ),
      
      sindre = list(
        cur = 0,
        total = 0,
        regen = 0,
        flow = 0,
        locked = 0,
        bound = 0,
        class = 1,
        tiers = list(refill = 1, total = 1, flow = 1, locked = 1, bound = 1)
      )
    ),
    
    status = list(
      conditions = character(0),
      effects    = character(0),
      exhaustion = 0,
      bloodlust  = FALSE
    ),
    
    character_3d = list(
      label = "",
      
      base_model = "models/universal_base/Base Characters/Godot - UE/Superhero_Female_FullBody.gltf",
      hair_model = "",
      
      body_model = "",
      arms_model = "",
      legs_model = "",
      feet_model = "",
      
      headgear_model = "",
      accessory_model = "",
      
      outfit_model = ""
    ),
    
    journal = list(
      log = character(0)
    ),
    
    inventory = list(
      gold = 0,
      items = inventory_empty()
    ),
    
    combat = list(
      weapons = combat_empty_weapons(),
      armors  = combat_empty_armors()
    ),
    
    diary = list(
      imported_old_journal = FALSE,
      entries = data.frame(
        id = character(),
        ts = as.POSIXct(character()),
        title = character(),
        mood = character(),
        tags = character(),
        body = character(),
        hp_cur = integer(),
        hp_max = integer(),
        hp_temp = integer(),
        effects = character(),
        stringsAsFactors = FALSE
      )
    )
  )
}

# ----------------------------
# Validate / upgrade character
# ----------------------------
validate_character <- function(x) {
  if (is.null(x) || !is.list(x)) return(new_character())
  
  x$save_version <- x$save_version %||% 0
  
  # meta
  x$meta <- x$meta %||% list()
  if (!is.list(x$meta)) x$meta <- list()
  x$meta$name       <- x$meta$name %||% ""
  x$meta$race       <- x$meta$race %||% ""
  x$meta$created_at <- x$meta$created_at %||% Sys.time()
  x$meta$updated_at <- x$meta$updated_at %||% Sys.time()
  
  # build
  x$build <- x$build %||% list()
  if (!is.list(x$build)) x$build <- list()
  x$build$class <- x$build$class %||% ""
  x$build$path  <- x$build$path %||% ""
  x$build$level <- x$build$level %||% 1
  
  # character 3D appearance
  # character 3D appearance
  x$character_3d <- x$character_3d %||% list()
  if (!is.list(x$character_3d)) x$character_3d <- list()
  
  x$character_3d$label <- x$character_3d$label %||% ""
  
  x$character_3d$base_model <- x$character_3d$base_model %||%
    "models/universal_base/Base Characters/Godot - UE/Superhero_Female_FullBody.gltf"
  
  x$character_3d$hair_model <- x$character_3d$hair_model %||% ""
  
  x$character_3d$body_model      <- x$character_3d$body_model %||% ""
  x$character_3d$arms_model      <- x$character_3d$arms_model %||% ""
  x$character_3d$legs_model      <- x$character_3d$legs_model %||% ""
  x$character_3d$feet_model      <- x$character_3d$feet_model %||% ""
  x$character_3d$headgear_model  <- x$character_3d$headgear_model %||% ""
  x$character_3d$accessory_model <- x$character_3d$accessory_model %||% ""
  x$character_3d$outfit_model    <- x$character_3d$outfit_model %||% ""
  # resources MUST exist before x$resources$...
  x$resources <- x$resources %||% list()
  if (!is.list(x$resources)) x$resources <- list()
  
  # status MUST exist before x$status$...
  x$status <- x$status %||% list()
  if (!is.list(x$status)) x$status <- list()
  x$status$conditions <- x$status$conditions %||% character(0)
  x$status$effects    <- x$status$effects %||% character(0)
  x$status$exhaustion <- as.integer(x$status$exhaustion %||% 0)
  x$status$bloodlust  <- isTRUE(x$status$bloodlust %||% FALSE)
  
  # wood
  x$resources$wood <- x$resources$wood %||% list(cur = 3, max = 10)
  if (!is.list(x$resources$wood)) x$resources$wood <- list(cur = 3, max = 10)
  x$resources$wood$cur <- as.numeric(x$resources$wood$cur %||% 0)
  x$resources$wood$max <- as.numeric(x$resources$wood$max %||% 10)
  
  # blood
  x$resources$blood <- x$resources$blood %||% list()
  if (!is.list(x$resources$blood)) x$resources$blood <- list()
  
  if (!is.data.frame(x$resources$blood$inventory)) {
    x$resources$blood$inventory <- data.frame(
      id = character(),
      pints = numeric(),
      sindre = numeric(),
      source = character(),
      stringsAsFactors = FALSE
    )
  }
  
  x$resources$blood$addiction <- x$resources$blood$addiction %||% list()
  if (!is.list(x$resources$blood$addiction)) {
    x$resources$blood$addiction <- list()
  }
  
  a <- x$resources$blood$addiction
  a$stage <- as.integer(a$stage %||% 1)
  a$days_at_stage <- as.integer(a$days_at_stage %||% 0)
  a$previous_day_intake <- as.numeric(a$previous_day_intake %||% 0)
  a$current_day_intake  <- as.numeric(a$current_day_intake %||% 0)
  x$resources$blood$addiction <- a
  
  # abilities
  x$abilities <- x$abilities %||% list()
  if (!is.list(x$abilities)) x$abilities <- list()
  for (nm in c("str","dex","con","int","cha","bld_str")) {
    x$abilities[[nm]] <- x$abilities[[nm]] %||% 10
  }
  
  # prof
  x$prof <- x$prof %||% list()
  if (!is.list(x$prof)) x$prof <- list()
  x$prof$saves  <- x$prof$saves  %||% list()
  x$prof$skills <- x$prof$skills %||% list()
  x$prof$skill_cards <- x$prof$skill_cards %||% list()
  if (!is.list(x$prof$saves))  x$prof$saves  <- list()
  if (!is.list(x$prof$skills)) x$prof$skills <- list()
  if (!is.list(x$prof$skill_cards)) x$prof$skill_cards <- list()
  
  # resources
  x$resources <- x$resources %||% list()
  if (!is.list(x$resources)) x$resources <- list()
  
  # hp
  x$resources$hp <- x$resources$hp %||% list()
  if (!is.list(x$resources$hp)) x$resources$hp <- list()
  
  x$resources$hp$max  <- suppressWarnings(as.integer(x$resources$hp$max %||% 0))
  x$resources$hp$cur  <- suppressWarnings(as.integer(x$resources$hp$cur %||% x$resources$hp$max %||% 0))
  x$resources$hp$temp <- suppressWarnings(as.integer(x$resources$hp$temp %||% 0))
  
  # clamp HP sanity
  x$resources$hp$max  <- max(0, x$resources$hp$max)
  x$resources$hp$cur  <- max(0, min(x$resources$hp$max, x$resources$hp$cur))
  x$resources$hp$temp <- max(0, x$resources$hp$temp)
  
  #Exhaustion HP
  ex <- as.integer(x$status$exhaustion %||% 0)
  
  if (ex >= 4) {
    x$resources$hp$max <- floor(x$resources$hp$max / 2)
    x$resources$hp$cur <- min(x$resources$hp$cur, x$resources$hp$max)
  }
  
  
  #rations
  x$resources$rations <- x$resources$rations %||% list()
  if (!is.list(x$resources$rations)) x$resources$rations <- list()
  
  
  x$resources$rations$cur <- as.numeric(x$resources$rations$cur %||% 3)
  
  #Water
  
  x$resources$water <- x$resources$water %||% list(cur = 3, max = 5)
  
  # ---- SINDRE schema (IMPORTANT FIX) ----
  # Old saves may have resources$sindre as a number (0) instead of a list.
  if (is.null(x$resources$sindre) || !is.list(x$resources$sindre)) {
    # if it was numeric, treat it as "cur" (best-effort), otherwise 0
    cur_guess <- suppressWarnings(as.integer(x$resources$sindre %||% 0))
    if (is.na(cur_guess)) cur_guess <- 0
    
    x$resources$sindre <- list(
      cur = cur_guess,
      total = 0,
      regen = 0,
      flow = 0,
      locked = 0,
      bound = 0,
      class = 1
    )
  }
  
  # ensure all sindre fields exist + coerce
  x$resources$sindre$cur    <- as.integer(x$resources$sindre$cur    %||% 0)
  x$resources$sindre$total  <- as.integer(x$resources$sindre$total  %||% 0)
  x$resources$sindre$regen  <- as.integer(x$resources$sindre$regen  %||% 0)
  x$resources$sindre$flow   <- as.integer(x$resources$sindre$flow   %||% 0)
  x$resources$sindre$locked <- as.integer(x$resources$sindre$locked %||% 0)
  x$resources$sindre$bound  <- as.integer(x$resources$sindre$bound  %||% 0)
  x$resources$sindre$class  <- as.integer(x$resources$sindre$class  %||% 1)
  
  # clamp sane
  x$resources$sindre$total <- max(0, x$resources$sindre$total)
  x$resources$sindre$cur   <- max(0, min(x$resources$sindre$total, x$resources$sindre$cur))
  x$resources$sindre$temp <- as.integer(x$resources$sindre$temp %||% 0)
  
  
  # tiers (persist magic calibration)
  x$resources$sindre$tiers <- x$resources$sindre$tiers %||% list()
  if (!is.list(x$resources$sindre$tiers)) x$resources$sindre$tiers <- list()
  
  for (nm in c("refill","total","flow","locked","bound")) {
    v <- suppressWarnings(as.integer(x$resources$sindre$tiers[[nm]] %||% 1))
    if (is.na(v)) v <- 1
    x$resources$sindre$tiers[[nm]] <- v
  }
  
  #Environment
  x$environment <- x$environment %||% list(
    temperature = "temperate"  # normal | cold | extreme_cold
  )
  
  #magic types
  x$magic <- x$magic %||% list()
  x$magic$types <- x$magic$types %||% list()
  
  x$inventory <- x$inventory %||% list()
  if (!is.list(x$inventory)) x$inventory <- list()
  
  x$inventory$gold <- suppressWarnings(as.numeric(x$inventory$gold %||% 0))
  if (is.na(x$inventory$gold) || x$inventory$gold < 0) {
    x$inventory$gold <- 0
  }
  
  x$inventory$items <- inventory_normalize(x$inventory$items)
  
  # ----------------------------
  # Migrate old combat inventory
  # ----------------------------
  if (!is.null(x$combat) && is.list(x$combat)) {
    inv <- x$inventory$items
    
    if (is.data.frame(x$combat$weapons) && nrow(x$combat$weapons) > 0) {
      for (i in seq_len(nrow(x$combat$weapons))) {
        w <- x$combat$weapons[i, , drop = FALSE]
        
        inv <- rbind(inv, data.frame(
          id = paste0("legacy_w_", i, "_", sample(1000:9999, 1)),
          name = as.character(w$name[[1]] %||% "Weapon"),
          type = "weapon",
          desc = "",
          value = 0,
          weight = 1,
          qty = 1,
          equipped = TRUE,
          in_bag = FALSE,
          meta = I(list(as.list(w))),
          edit = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    if (is.data.frame(x$combat$armors) && nrow(x$combat$armors) > 0) {
      for (i in seq_len(nrow(x$combat$armors))) {
        a <- x$combat$armors[i, , drop = FALSE]
        
        inv <- rbind(inv, data.frame(
          id = paste0("legacy_a_", i, "_", sample(1000:9999, 1)),
          name = as.character(a$name[[1]] %||% "Armor"),
          type = "armor",
          desc = "",
          value = 0,
          weight = 5,
          qty = 1,
          equipped = isTRUE(a$worn[[1]]),
          in_bag = FALSE,
          meta = I(list(as.list(a))),
          edit = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    x$inventory$items <- inventory_normalize(inv)
    x$combat <- NULL
  }
  
  

  
  # journal
  x$journal <- x$journal %||% list()
  if (!is.list(x$journal)) x$journal <- list()
  x$journal$log <- x$journal$log %||% character(0)
  
  # diary
  x$diary <- x$diary %||% list()
  if (!is.list(x$diary)) x$diary <- list()
  x$diary$imported_old_journal <- x$diary$imported_old_journal %||% FALSE
  
  if (!is.data.frame(x$diary$entries)) {
    x$diary$entries <- data.frame(
      ts = as.POSIXct(character()),
      title = character(),
      mood = character(),
      tags = character(),
      body = character(),
      stringsAsFactors = FALSE
    )
  }
  
  df <- x$diary$entries
  n  <- nrow(df)
  
  ensure_col <- function(name, vec) {
    if (!name %in% names(df)) df[[name]] <<- vec
  }
  
  ensure_col("id",      character(n))
  ensure_col("ts",      as.POSIXct(rep(NA, n), origin="1970-01-01"))
  ensure_col("title",   character(n))
  ensure_col("mood",    character(n))
  ensure_col("tags",    character(n))
  ensure_col("body",    character(n))
  ensure_col("hp_cur",  rep(NA_integer_, n))
  ensure_col("hp_max",  rep(NA_integer_, n))
  ensure_col("hp_temp", rep(NA_integer_, n))
  ensure_col("effects", character(n))
  
  if (!inherits(df$ts, "POSIXct")) suppressWarnings(df$ts <- as.POSIXct(df$ts, tz="UTC"))
  df$mood[is.na(df$mood) | df$mood == ""] <- "Neutral"
  df$title[is.na(df$title)] <- ""
  df$tags[is.na(df$tags)] <- ""
  df$body[is.na(df$body)] <- ""
  df$effects[is.na(df$effects)] <- ""
  df$id[is.na(df$id)] <- ""
  
  for (nm in c("hp_cur","hp_max","hp_temp")) {
    suppressWarnings(df[[nm]] <- as.integer(df[[nm]]))
  }
  
  x$diary$entries <- df
  
  # Combat system fully migrated to inventory (legacy removed)
  x$combat <- NULL
  
  # (future) migrations:
  # if (x$save_version < 2) { ...; x$save_version <- 2 }
  
  x$save_version <- APP_SAVE_VERSION
  x
}
