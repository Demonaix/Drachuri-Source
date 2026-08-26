# global.R
# Emoji and Welsh names must be decoded as UTF-8 even when R is launched from
# a minimal shell which falls back to the byte-oriented C locale.
if (identical(Sys.getlocale("LC_CTYPE"), "C")) {
  try(Sys.setlocale("LC_CTYPE", "en_GB.UTF-8"), silent = TRUE)
}
options(encoding = "UTF-8")

library(shiny)

APP_SAVE_VERSION <- 2

`%||%` <- function(a, b) if (!is.null(a)) a else b

#Database connection
library(DBI)
library(RPostgres)

get_all_sessions <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  on.exit(release_db_connection(con), add = TRUE)
  
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
  
  on.exit(release_db_connection(con), add = TRUE)
  
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
  
  on.exit(release_db_connection(con), add = TRUE)
  
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
  
  on.exit(release_db_connection(con), add = TRUE)
  
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
  
  on.exit(release_db_connection(con), add = TRUE)
  
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
  
  on.exit(release_db_connection(con), add = TRUE)
  
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
  
  on.exit(release_db_connection(con), add = TRUE)
  
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

.drachuri_db <- new.env(parent = emptyenv())
.drachuri_db$pool <- NULL
.drachuri_db$connection <- NULL
.drachuri_db$backend <- NULL
.drachuri_db$checked_out <- list()
.drachuri_db$shutdown_registered <- FALSE
.drachuri_db$config <- list(
  host = Sys.getenv("SUPABASE_HOST"),
  port = Sys.getenv("SUPABASE_PORT", unset = "5432"),
  dbname = Sys.getenv("SUPABASE_DBNAME"),
  user = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_DB_PASSWORD"),
  sslmode = Sys.getenv("SUPABASE_SSLMODE", unset = "require")
)

db_connection_args <- function() {
  config <- .drachuri_db$config
  db_port <- suppressWarnings(as.integer(config$port))
  if (is.na(db_port) || db_port < 1L || db_port > 65535L) db_port <- 5432L

  list(
    drv = RPostgres::Postgres(),
    host = config$host,
    port = db_port,
    dbname = config$dbname,
    user = config$user,
    password = config$password,
    sslmode = config$sslmode
  )
}

get_db_connection <- function() {
  tryCatch({
    # pool is optional so existing player installations continue to work. Once
    # installed, each query checks out a managed connection and returns it via
    # release_db_connection().
    if (requireNamespace("pool", quietly = TRUE)) {
      if (is.null(.drachuri_db$pool)) {
        .drachuri_db$pool <- do.call(
          pool::dbPool,
          c(db_connection_args(), list(minSize = 1L, maxSize = 3L, idleTimeout = 60L))
        )
      }
      .drachuri_db$backend <- "pool"
      con <- pool::poolCheckout(.drachuri_db$pool)
      .drachuri_db$checked_out[[length(.drachuri_db$checked_out) + 1L]] <- con
      return(con)
    }

    # Safe fallback for friends who have not restored the new dependency yet:
    # reuse one connection instead of reconnecting on every polling query.
    if (
      is.null(.drachuri_db$connection) ||
      !isTRUE(tryCatch(DBI::dbIsValid(.drachuri_db$connection), error = function(e) FALSE))
    ) {
      .drachuri_db$connection <- do.call(DBI::dbConnect, db_connection_args())
    }
    .drachuri_db$backend <- "persistent"
    .drachuri_db$connection
  }, error = function(e) {
    message("DB connection failed: ", e$message)
    NULL
  })
}

release_db_connection <- function(con) {
  if (is.null(con)) return(invisible(FALSE))
  if (identical(.drachuri_db$backend, "pool") && requireNamespace("pool", quietly = TRUE)) {
    matches <- which(vapply(
      .drachuri_db$checked_out,
      function(candidate) identical(candidate, con),
      logical(1)
    ))
    # Only return handles that this wrapper still considers checked out. This
    # prevents a delayed on.exit handler from returning the same connection a
    # second time after application shutdown has already drained the pool.
    if (length(matches)) {
      .drachuri_db$checked_out <- .drachuri_db$checked_out[-matches[1L]]
      try(pool::poolReturn(con), silent = TRUE)
      return(invisible(TRUE))
    }
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

close_db_pool <- function() {
  if (!is.null(.drachuri_db$pool) && requireNamespace("pool", quietly = TRUE)) {
    # Shiny can invoke its app stop callback before an interrupted reactive has
    # finished unwinding. Drain tracked checkouts first so poolClose() never
    # leaves orphaned handles that later trigger pool's finalizer warning.
    outstanding <- .drachuri_db$checked_out
    .drachuri_db$checked_out <- list()
    for (con in outstanding) {
      try(pool::poolReturn(con), silent = TRUE)
    }
    try(pool::poolClose(.drachuri_db$pool), silent = TRUE)
    .drachuri_db$pool <- NULL
  }
  if (!is.null(.drachuri_db$connection)) {
    try(DBI::dbDisconnect(.drachuri_db$connection), silent = TRUE)
    .drachuri_db$connection <- NULL
  }
  .drachuri_db$backend <- NULL
  .drachuri_db$checked_out <- list()
  invisible(TRUE)
}

register_db_pool_shutdown <- function() {
  if (!isTRUE(.drachuri_db$shutdown_registered)) {
    shiny::onStop(close_db_pool)
    .drachuri_db$shutdown_registered <- TRUE
  }
  invisible(TRUE)
}

# global_core.R is sourced from global.R in both the app.R and legacy
# ui.R/server.R layouts, so this registration cannot be skipped by runApp().
register_db_pool_shutdown()

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
  class_entries <- x$build$classes %||% list()
  class_names <- if (is.list(class_entries) && length(class_entries)) {
    vapply(class_entries, function(entry) {
      tolower(trimws(as.character(entry$class %||% "")))
    }, character(1))
  } else {
    tolower(trimws(as.character(x$build$class %||% "")))
  }
  uses_blood_magic <- "hanianol sorcerer" %in% class_names
  
  # Hanianol Blood Magic cannot regenerate Sindre naturally. Preserve the
  # original Tylwyth Teg restriction for legacy characters as well.
  if (uses_blood_magic || race == "tylwyth teg") {
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

# Resolve the magical resource gained from consuming one heart. Heart Eaters
# restore their normal reserve completely and keep the heart's listed value as
# temporary Sindre. Other characters only keep the portion above their normal
# maximum as temporary Sindre.
consume_heart_sindre <- function(current, maximum, temporary = 0,
                                 heart_value = 0, heart_eater = FALSE) {
  current <- suppressWarnings(as.integer(current %||% 0L))
  maximum <- suppressWarnings(as.integer(maximum %||% 0L))
  temporary <- suppressWarnings(as.integer(temporary %||% 0L))
  heart_value <- suppressWarnings(as.integer(heart_value %||% 0L))
  if (is.na(current)) current <- 0L
  if (is.na(maximum)) maximum <- 0L
  if (is.na(temporary)) temporary <- 0L
  if (is.na(heart_value)) heart_value <- 0L
  maximum <- max(0L, maximum)
  current <- max(0L, min(current, maximum))
  temporary <- max(0L, temporary)
  heart_value <- max(0L, heart_value)

  if (isTRUE(heart_eater)) {
    return(list(
      current = maximum,
      temporary = temporary + heart_value,
      temporary_gained = heart_value
    ))
  }

  combined <- current + heart_value
  overflow <- max(0L, combined - maximum)
  list(
    current = min(combined, maximum),
    temporary = temporary + overflow,
    temporary_gained = overflow
  )
}

character_subclass_names <- function(x) {
  build <- x$build %||% list()
  classes <- build$classes %||% list()
  from_classes <- unlist(lapply(classes, function(entry) {
    as.character(entry$subclass %||% entry$path %||% "")
  }), use.names = FALSE)
  unique(Filter(nzchar, trimws(c(
    as.character(build$subclass %||% ""),
    as.character(build$path %||% ""),
    from_classes
  ))))
}

magical_identity_labels <- function(total, flow, regen, locked = 0, bound = 0,
                                    subclasses = character()) {
  total <- max(1, suppressWarnings(as.numeric(total %||% 0)))
  flow <- max(0, suppressWarnings(as.numeric(flow %||% 0)))
  regen <- max(0, suppressWarnings(as.numeric(regen %||% 0)))
  locked <- max(0, suppressWarnings(as.numeric(locked %||% 0)))
  bound <- max(0, suppressWarnings(as.numeric(bound %||% 0)))
  values <- c(total, flow, regen, locked, bound)
  values[is.na(values)] <- 0
  total <- max(1, values[[1L]]); flow <- values[[2L]]; regen <- values[[3L]]
  locked <- values[[4L]]; bound <- values[[5L]]
  ratio <- flow / total

  core <- if (total > 100) "Abyss" else if (total > 80) "Deepwell" else if (total > 60) "Reservoir" else if (total < 30) "Ember" else "Well"
  expression <- if (ratio > .8) "Tempest" else if (ratio > .6) "Storm" else if (ratio > .4) "Current" else if (ratio < .2) "Stillwater" else "Tide"
  subclass <- tolower(paste(subclasses, collapse = " "))
  aspect <- if (grepl("heart eater", subclass)) "Devourer" else if (grepl("ancestor", subclass)) "Warden" else if (grepl("prophet", subclass)) "Seer" else if (grepl("warrior", subclass)) "Spellblade" else "Channeler"
  modifier <- if (bound > 0) "Bound" else if (locked > 0) "Sealed" else if (regen == 0 && ratio > .5) "Starved" else if (ratio > .75) "Unbound" else if (regen > 10) "Everflowing" else NULL
  list(core = core, expression = expression, aspect = aspect, modifier = modifier,
       title = paste(Filter(nzchar, c(modifier, core, expression, aspect)), collapse = " "))
}

skill_identity_labels <- function(top_skills, top_scores = numeric()) {
  skills <- tolower(trimws(as.character(top_skills %||% character())))
  core_map <- c(
    athletics="Brute", clutch="Grasp", wrestling="Grappler", throwing="Hurler", `dead lift`="Titan",
    acrobatics="Acrobat", `sleight of hand`="Quickhand", stealth="Shadow", precision="Marksman",
    endurance="Bulwark", tolerance="Ironblood", fortitude="Stalwart",
    arcana="Arcanist", history="Chronicler", investigation="Investigator", nature="Naturalist", religion="Theologian", analysis="Strategist",
    perception="Watcher", survival="Stalker", insight="Reader", medicine="Healer", `animal handling`="Beastfriend", `mandred connection`="Mandred-Touched",
    deception="Trickster", intimidation="Menace", persuasion="Orator", performance="Virtuoso", presence="Luminary"
  )
  aspect_map <- c(
    athletics="Enforcer", clutch="Binder", wrestling="Wrestler", throwing="Artillerist", `dead lift`="Bearer",
    acrobatics="Daredevil", `sleight of hand`="Pilferer", stealth="Ghost", precision="Deadeye",
    endurance="Survivor", tolerance="Resistant", fortitude="Guardian",
    arcana="Seer", history="Lorekeeper", investigation="Inquisitor", nature="Warden", religion="Devotee", analysis="Tactician",
    perception="Observer", survival="Hunter", insight="Empath", medicine="Physician", `animal handling`="Handler", `mandred connection`="Conduit",
    deception="Liar", intimidation="Dread", persuasion="Diplomat", performance="Muse", presence="Leader"
  )
  first <- if (length(skills) >= 1L) skills[[1L]] else ""
  second <- if (length(skills) >= 2L) skills[[2L]] else ""
  core <- unname(core_map[[first]] %||% "Wanderer")
  aspect <- unname(aspect_map[[second]] %||% "Operative")
  scores <- suppressWarnings(as.numeric(top_scores))
  scores <- scores[!is.na(scores)]
  modifier <- NULL
  if (length(scores)) {
    avg <- mean(scores[seq_len(min(5L, length(scores)))])
    third <- if(length(skills)>=3L)skills[[3L]]else""
    third_modifier <- c(athletics="Mighty",clutch="Tenacious",wrestling="Relentless",throwing="Keen",`dead lift`="Mighty",acrobatics="Nimble",`sleight of hand`="Cunning",stealth="Elusive",precision="Keen",endurance="Hardy",tolerance="Hardened",fortitude="Resolute",arcana="Mystic",history="Learned",investigation="Shrewd",nature="Wildwise",religion="Devout",analysis="Calculating",perception="Watchful",survival="Seasoned",insight="Intuitive",medicine="Practised",`animal handling`="Beastwise",`mandred connection`="Touched",deception="Cunning",intimidation="Fearsome",persuasion="Silver-Tongued",performance="Mesmeric",presence="Commanding")
    third_label<-if(third%in%names(third_modifier))unname(third_modifier[[third]])else"Cunning"
    if (scores[[1L]] > avg + 3) modifier <- paste("Elite",third_label) else if (scores[[1L]] > avg + 1) modifier <- third_label else if (scores[[1L]] < 1) modifier <- "Unproven"
  }
  list(core = core, aspect = aspect, modifier = modifier,
       title = paste(Filter(nzchar, c(modifier, core, aspect)), collapse = " "))
}

character_magic_types <- function(x) {
  build <- x$build %||% list()
  classes <- build$classes %||% list()
  if (!length(classes) && nzchar(as.character(build$class %||% ""))) {
    classes <- list(list(class = build$class, level = build$level %||% 1L))
  }
  class_names <- vapply(classes, function(entry) as.character(entry$class %||% ""), character(1))
  levels <- vapply(classes, function(entry) as.integer(entry$level %||% 1L), integer(1))
  out <- if (any(grepl("Sorcerer", class_names, fixed = TRUE))) "Mechanical" else character()
  if (sum(levels[class_names == "Hanianol Sorcerer"], na.rm = TRUE) >= 2L) out <- c(out, "Natural")
  choices <- build$level_choices %||% list()
  thermal <- unique(unlist(lapply(names(choices), function(class_name) {
    if (!grepl("Sorcerer", class_name, fixed = TRUE)) return(character())
    unlist(lapply(choices[[class_name]] %||% list(), function(level_choice) {
      as.character(level_choice$thermal_path %||% "")
    }), use.names = FALSE)
  }), use.names = FALSE))
  if ("Exothermic" %in% thermal) out <- c(out, "Fire")
  if ("Endothermic" %in% thermal) out <- c(out, "Cold")
  manual <- as.character((x$magic %||% list())$types %||% character())
  aliases <- c(Chemical = "Poison", Nuclear = "Radiant", Radiation = "Radiant", Light = "Radiant")
  manual <- ifelse(manual %in% names(aliases), unname(aliases[manual]), manual)
  allowed <- c("Mechanical", "Natural", if (exists("GLYPH_DAMAGE_TYPES", inherits = TRUE)) get("GLYPH_DAMAGE_TYPES", inherits = TRUE) else c("Fire","Cold","Lightning","Acid","Poison","Force","Necrotic","Radiant","Psychic","Thunder"))
  unique(c(out, manual)[c(out, manual) %in% allowed])
}

bloodlust_bite_required <- function(x, attack_roll = NA_integer_, start_of_turn = FALSE) {
  x <- validate_character(x)
  addiction <- (x$resources$blood %||% list())$addiction %||% list()
  stage <- suppressWarnings(as.integer(addiction$stage %||% 1L))
  if (is.na(stage)) stage <- 1L
  if (!isTRUE(x$status$bloodlust %||% FALSE) || stage < 3L) return(FALSE)
  if (stage >= 4L && isTRUE(start_of_turn)) return(TRUE)
  roll <- suppressWarnings(as.integer(attack_roll))
  stage == 3L && !is.na(roll) && roll == 1L
}

reset_class_uses_for_rest <- function(char, rest_type = c("short_rest", "long_rest")) {
  rest_type <- match.arg(rest_type)
  char <- validate_character(char)
  uses <- char$resources$class_uses %||% list()
  for (key in names(uses)) {
    recharge <- as.character(uses[[key]]$recharge %||% "long_rest")
    if (identical(rest_type, "long_rest") || identical(recharge, "short_rest")) {
      uses[[key]]$used <- FALSE
    }
  }
  char$resources$class_uses <- uses
  pools <- char$resources$class_pools %||% list()
  for (key in names(pools)) {
    recharge <- as.character(pools[[key]]$recharge %||% "long_rest")
    if (identical(rest_type, "long_rest") || identical(recharge, "short_rest")) {
      pools[[key]]$remaining <- as.integer(pools[[key]]$maximum %||% 0L)
    }
  }
  char$resources$class_pools <- pools
  char$status <- char$status %||% list()
  char$status$raging <- FALSE
  if (identical(rest_type, "long_rest")) {
    boosts <- char$status$balance_boosts %||% character()
    for (stat in as.character(boosts)) {
      current <- suppressWarnings(as.integer(char$abilities[[stat]] %||% 10L))
      if (!is.na(current)) char$abilities[[stat]] <- current - 1L
    }
    char$status$balance_boosts <- character()
  }
  char
}

COMBAT_ARMOR_TYPES <- c("Light", "Medium", "Heavy", "Custom")
COMBAT_WEAPON_STATS <- c("str", "dex", "con", "int", "bld_str", "cha")

#For multiplayer HUD

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
  on.exit(release_db_connection(con), add = TRUE)
  
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
  
  on.exit(release_db_connection(con), add = TRUE)
  
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
  
  on.exit(release_db_connection(con), add = TRUE)
  
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
  
  on.exit(release_db_connection(con), add = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  character_id <- as.character(character_id %||% "")
  
  if (is.na(session_id) || session_id < 1 || !nzchar(character_id)) return(FALSE)
  
  tryCatch({
    changed <- DBI::dbExecute(
      con,
      "
      DELETE FROM session_players
      WHERE session_id = $1
        AND character_id = $2
      ",
      params = list(session_id, character_id)
    )
    
    changed > 0L
  }, error = function(e) {
    message("remove_character_from_session failed: ", e$message)
    FALSE
  })
}

damage_session_enemy <- function(session_id, enemy_uuid, amount) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  
  on.exit(release_db_connection(con), add = TRUE)
  
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

get_effective_max_hp <- function(char) {
  char <- validate_character(char)
  base_max <- suppressWarnings(as.integer(char$resources$hp$max %||% 0L))
  if (is.na(base_max) || base_max < 1L) return(0L)
  exhaustion <- suppressWarnings(as.integer(char$status$exhaustion %||% 0L))
  if (is.na(exhaustion)) exhaustion <- 0L
  if (exhaustion >= 4L) max(1L, floor(base_max / 2L)) else base_max
}

get_effective_hp_state <- function(state) {
  x <- validate_character(state$char)
  
  hp <- x$resources$hp %||% list(max = 0, cur = 0, temp = 0)
  
  out <- list(
    max = get_effective_max_hp(x),
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
  max_hp <- get_effective_max_hp(x)
  
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

food_item_meta <- function(meta, qty=1, current_day=1L) {
  meta<-if(is.list(meta))meta else list();ration_value<-max(1L,as.integer(meta$ration_value%||%1L));shelf_life<-max(0L,as.integer(meta$shelf_life_days%||%3L))
  if(is.null(meta$food_acquired_day))meta$food_acquired_day<-as.integer(current_day)
  if(is.null(meta$fresh_until_day))meta$fresh_until_day<-as.integer(meta$food_acquired_day)+shelf_life
  if(is.null(meta$food_rations_remaining))meta$food_rations_remaining<-max(0,as.numeric(qty%||%1)*ration_value)
  meta$ration_value<-ration_value;meta$shelf_life_days<-shelf_life;meta
}

food_rations_available <- function(x, current_day=NULL) {
  x<-validate_character(x);day<-as.integer(current_day%||%x$meta$day%||%1L);inv<-inventory_normalize(x$inventory$items);total<-0
  if(nrow(inv))for(i in seq_len(nrow(inv))){m<-inv$meta[[i]]%||%list();if(identical(as.character(m$category%||%""),"food")){m<-food_item_meta(m,inv$qty[[i]],day);if(day<=as.integer(m$fresh_until_day))total<-total+as.numeric(m$food_rations_remaining)}}
  as.integer(floor(total))
}

consume_food_ration <- function(x, current_day=NULL) {
  x<-validate_character(x);day<-as.integer(current_day%||%x$meta$day%||%1L);inv<-inventory_normalize(x$inventory$items);candidates<-list()
  if(nrow(inv))for(i in seq_len(nrow(inv))){m<-inv$meta[[i]]%||%list();if(identical(as.character(m$category%||%""),"food")){m<-food_item_meta(m,inv$qty[[i]],day);inv$meta[[i]]<-m;if(day<=as.integer(m$fresh_until_day)&&as.numeric(m$food_rations_remaining)>0)candidates[[length(candidates)+1L]]<-c(i=i,expiry=as.integer(m$fresh_until_day))}}
  if(!length(candidates))return(list(char=x,applied=FALSE,item_name="",remaining=0L))
  pick<-candidates[[which.min(vapply(candidates,function(z)z[["expiry"]],numeric(1)))]];i<-as.integer(pick[["i"]]);m<-inv$meta[[i]];m$food_rations_remaining<-as.numeric(m$food_rations_remaining)-1;item_name<-inv$name[[i]]
  if(m$food_rations_remaining<=0)inv<-inv[-i,,drop=FALSE]else{inv$meta[[i]]<-m;inv$qty[[i]]<-ceiling(m$food_rations_remaining/max(1,m$ration_value))}
  x$inventory$items<-inventory_normalize(inv);list(char=x,applied=TRUE,item_name=item_name,remaining=food_rations_available(x,day))
}

spoil_character_food <- function(x, current_day=NULL) {
  x<-validate_character(x);day<-as.integer(current_day%||%x$meta$day%||%1L);inv<-inventory_normalize(x$inventory$items);spoiled<-character();keep<-rep(TRUE,nrow(inv))
  if(nrow(inv))for(i in seq_len(nrow(inv))){m<-inv$meta[[i]]%||%list();if(identical(as.character(m$category%||%""),"food")){m<-food_item_meta(m,inv$qty[[i]],max(1L,day-1L));inv$meta[[i]]<-m;if(day>as.integer(m$fresh_until_day)){keep[[i]]<-FALSE;spoiled<-c(spoiled,inv$name[[i]])}}}
  x$inventory$items<-inventory_normalize(inv[keep,,drop=FALSE]);list(char=x,spoiled=spoiled)
}

load_character_from_db <- function(char_id) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(release_db_connection(con), add = TRUE)
  
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
  
  char <- tryCatch(
    unserialize(res$state_blob[[1]]),
    error = function(e) {
      message("unserialize failed: ", e$message)
      NULL
    }
  )
  if (is.null(char)) return(NULL)
  tryCatch(
    hydrate_character_inventory_relational(con, char, char_id),
    error = function(e) {
      message("relational inventory hydrate failed; using blob fallback: ", e$message)
      char
    }
  )
}

list_characters_in_db <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  on.exit(release_db_connection(con), add = TRUE)
  
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

character_save_payload <- function(char) {
  list(
    name = as.character(char$meta$name %||% "Unnamed"),
    state_blob = serialize(char, NULL)
  )
}

save_character_to_db <- function(char, char_id = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(release_db_connection(con), add = TRUE)
  
  payload <- character_save_payload(char)
  raw <- payload$state_blob
  name <- payload$name
  
  tryCatch(DBI::dbWithTransaction(con, {
    saved_id <- if (is.null(char_id)) {
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
    sync_character_inventory_relational(con, char, saved_id)
    saved_id
  }), error = function(e) {
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

  modes <- meta$attack_modes %||% list()
  if (is.data.frame(modes)) modes <- lapply(seq_len(nrow(modes)), function(i) as.list(modes[i, , drop = FALSE]))
  if (!is.list(modes)) modes <- list()
  meta$attack_modes <- modes

  meta$equipment_slot <- tolower(as.character(meta$equipment_slot %||%
    if (identical(meta$type, "Shield")) "shield" else "body"))
  if (!meta$equipment_slot %in% c("body", "shield", "head", "accessory")) {
    meta$equipment_slot <- "body"
  }

  default_bonus <- if (identical(meta$equipment_slot, "shield")) meta$base_ac else 0
  meta$ac_bonus <- suppressWarnings(as.numeric(meta$ac_bonus %||% default_bonus))
  if (is.na(meta$ac_bonus)) meta$ac_bonus <- 0
  
  meta
}

upgrade_weapon_damage_die <- function(damage) {
  damage <- as.character(damage %||% "1d4")
  replacements <- c("d4" = "d6", "d6" = "d8", "d8" = "d10", "d10" = "d12")
  for (needle in names(replacements)) {
    if (grepl(needle, damage, fixed = TRUE)) return(sub(needle, replacements[[needle]], damage, fixed = TRUE))
  }
  damage
}

standard_weapon_attack_modes <- function(name, meta = list()) {
  key <- tolower(trimws(as.character(name %||% "")))
  damage <- as.character(meta$damage1 %||% "1d4")
  damage_type <- as.character(meta$dmg_type1 %||% "Other")
  family <- function(pattern) grepl(pattern, key, perl = TRUE)
  mode <- function(id, label, stat = "str", die = damage, hands = 1L,
                   range_ft = 5L, long_range_ft = range_ft) {
    list(id = id, name = label, damage = die, damage_type = damage_type,
         stat = stat, hands = hands, range_ft = range_ft, long_range_ft = long_range_ft)
  }

  finesse <- family("\\b(dagger|rapier|shortsword|scimitar|whip|sabre)\\b")
  if (finesse) meta$finesse <- TRUE

  if (family("\\b(spear|trident)\\b")) {
    return(list(
      mode("one_handed", "One-handed", "str"),
      mode("two_handed", "Two-handed", "str", upgrade_weapon_damage_die(damage), 2L),
      mode("thrown", "Thrown (20/60 ft)", "str", damage, 1L, 20L, 60L)
    ))
  }
  if (family("\\b(battleaxe|longsword|quarterstaff|warhammer)\\b") ||
      family("\\bstaff\\b")) {
    return(list(
      mode("one_handed", "One-handed", "str"),
      mode("two_handed", "Two-handed", "str", upgrade_weapon_damage_die(damage), 2L)
    ))
  }
  if (family("\\bdagger\\b")) {
    return(list(
      mode("melee", "Melee (finesse)", "finesse"),
      mode("thrown", "Thrown (20/60 ft, finesse)", "finesse", damage, 1L, 20L, 60L)
    ))
  }
  if (family("\\b(handaxe|light hammer)\\b")) {
    return(list(
      mode("melee", "Melee", "str"),
      mode("thrown", "Thrown (20/60 ft)", "str", damage, 1L, 20L, 60L)
    ))
  }
  if (family("\\bjavelin\\b")) {
    return(list(
      mode("melee", "Melee", "str"),
      mode("thrown", "Thrown (30/120 ft)", "str", damage, 1L, 30L, 120L)
    ))
  }
  if (finesse) return(list(mode("finesse", "Finesse (STR or DEX)", "finesse")))
  if (family("\\blongbow\\b")) return(list(mode("ranged", "Ranged (150/600 ft)", "dex", damage, 2L, 150L, 600L)))
  if (family("\\bshortbow\\b")) return(list(mode("ranged", "Ranged (80/320 ft)", "dex", damage, 2L, 80L, 320L)))
  if (family("\\bbow\\b")) return(list(mode("ranged", "Ranged (80/320 ft)", "dex", damage, 2L, 80L, 320L)))
  list()
}

standard_spear_attack_modes <- function() standard_weapon_attack_modes(
  "Spear", list(damage1 = "1d6", dmg_type1 = "Piercing")
)

normalise_weapon_attack_modes <- function(name, meta=list()) {
  meta <- weapon_meta_defaults_global(meta)
  inferred <- standard_weapon_attack_modes(name, meta)
  if (!length(meta$attack_modes) && length(inferred)) meta$attack_modes <- inferred
  if (grepl("\\b(dagger|rapier|shortsword|scimitar|whip|sabre)\\b", tolower(as.character(name %||% "")), perl = TRUE)) {
    meta$finesse <- TRUE
  }
  meta
}

merge_legacy_weapon_mode_items <- function(items) {
  inv <- inventory_normalize(items); if (!nrow(inv)) return(inv)
  names_low <- tolower(trimws(inv$name)); spear <- grepl("^spear(?:\\s*\\((?:thrown|two hands|two handed|two-handed)\\))?$", names_low, perl=TRUE)
  variants <- spear & grepl("(", names_low, fixed=TRUE)
  if (sum(spear) > 1L && any(variants)) {
    idx <- which(spear); exact <- idx[names_low[idx]=="spear"]; thrown <- idx[grepl("thrown",names_low[idx],fixed=TRUE)]
    keep <- if(length(exact))exact[[1L]] else if(length(thrown))thrown[[1L]] else idx[[1L]]
    meta <- normalise_weapon_attack_modes("Spear",inv$meta[[keep]]%||%list())
    meta$merged_mode_instance_ids <- unique(c(as.character(meta$merged_mode_instance_ids%||%character()),as.character(inv$id[setdiff(idx,keep)])))
    inv$name[[keep]] <- "Spear"; inv$desc[[keep]] <- if(nzchar(inv$desc[[keep]]))inv$desc[[keep]] else "A versatile spear usable one-handed, two-handed, or thrown."
    inv$equipped[[keep]] <- any(inv$equipped[idx]%in%TRUE); inv$in_bag[[keep]] <- all(inv$in_bag[idx]%in%TRUE); inv$qty[[keep]] <- 1; inv$meta[[keep]] <- meta
    inv <- inv[-setdiff(idx,keep),,drop=FALSE]
  }
  if(nrow(inv))for(i in which(inv$type=="weapon"))inv$meta[[i]]<-normalise_weapon_attack_modes(inv$name[[i]],inv$meta[[i]])
  inventory_normalize(inv)
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
    meta <- normalise_weapon_attack_modes(row$name[[1]],row$meta[[1]])
    glyph_until <- suppressWarnings(as.integer(meta$glyph_active_until_day %||% NA_integer_))
    current_day <- suppressWarnings(as.integer(char$meta$day %||% 1L))
    glyph_active <- !is.na(glyph_until) && current_day <= glyph_until && nzchar(as.character(meta$glyph_damage %||% ""))
    if (glyph_active) {
      if (nzchar(meta$damage2) && !identical(meta$damage2, as.character(meta$glyph_damage))) {
        meta$damage2 <- paste(meta$damage2, as.character(meta$glyph_damage), sep = "+")
      } else meta$damage2 <- as.character(meta$glyph_damage)
      meta$dmg_type2 <- as.character(meta$glyph_damage_type %||% "Other")
    }
    
    base <- data.frame(
      id = as.character(row$id[1] %||% paste0("weapon_", i)),
      physical_id = as.character(row$id[1] %||% paste0("weapon_", i)),
      mode_id = "default",
      range_ft = NA_integer_,
      long_range_ft = NA_integer_,
      name = as.character(row$name[1] %||% "Weapon"),
      stat = as.character(meta$stat),
      adv = as.character(meta$adv),
      to_hit_bonus = as.numeric(meta$to_hit_bonus),
      damage1 = as.character(meta$damage1),
      dmg_type1 = as.character(meta$dmg_type1),
      damage2 = as.character(meta$damage2),
      dmg_type2 = as.character(meta$dmg_type2),
      material = as.character(meta$material %||% ""),
      material_attack_bonus = as.numeric(meta$material_attack_bonus %||% 0),
      material_damage_modifier = as.numeric(meta$material_damage_modifier %||% 0),
      quality_attack_bonus = as.numeric(meta$quality_attack_bonus %||% 0),
      quality_damage_modifier = as.numeric(meta$quality_damage_modifier %||% 0),
      proficient = isTRUE(meta$proficient),
      stringsAsFactors = FALSE
    )
    modes<-meta$attack_modes%||%list();if(!length(modes))return(base)
    do.call(rbind,lapply(modes,function(mode){out<-base;mid<-as.character(mode$id%||%"mode");out$id<-paste0(base$physical_id,"::",mid);out$mode_id<-mid;out$name<-paste0(base$name," — ",as.character(mode$name%||%mid));out$damage1<-as.character(mode$damage%||%base$damage1);out$dmg_type1<-as.character(mode$damage_type%||%base$dmg_type1);out$stat<-as.character(mode$stat%||%base$stat);out$range_ft<-as.integer(mode$range_ft%||%NA_integer_);out$long_range_ft<-as.integer(mode$long_range_ft%||%out$range_ft);out}))
  })
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

get_weapon_hit_bonus <- function(char, weapon_row) {
  char <- validate_character(char)
  
  stat <- as.character(weapon_row$stat[1] %||% "str")
  if (identical(tolower(stat), "finesse")) {
    str_mod <- get_character_ability_mod(char, "str")
    dex_mod <- get_character_ability_mod(char, "dex")
    stat <- if (dex_mod > str_mod) "dex" else "str"
  }
  stat_mod <- get_character_ability_mod(char, stat)
  prof_bonus <- if (isTRUE(weapon_row$proficient[1] %||% FALSE)) get_character_prof_bonus(char) else 0L
  flat_bonus <- suppressWarnings(as.integer(weapon_row$to_hit_bonus[1] %||% 0))
  if (is.na(flat_bonus)) flat_bonus <- 0L
  equipment_bonus <- suppressWarnings(as.numeric(
    weapon_row$material_attack_bonus[1] %||% 0
  ) + as.numeric(weapon_row$quality_attack_bonus[1] %||% 0))
  if (is.na(equipment_bonus)) equipment_bonus <- 0
  
  as.integer(stat_mod + prof_bonus + flat_bonus + equipment_bonus)
}

combat_grid_distance_ft <- function(x1, y1, x2, y2) {
  as.integer(max(abs(as.integer(x2) - as.integer(x1)), abs(as.integer(y2) - as.integer(y1))) * 5L)
}

combat_grid_shortest_path <- function(tiles, occupants, start_x, start_y, target_x, target_y,
                                      map_id = NULL, exclude_actor_id = NULL,
                                      allow_blocked = FALSE, max_cost_ft = Inf) {
  empty_path <- data.frame(x = integer(), y = integer(), step = integer(), stringsAsFactors = FALSE)
  max_cost_ft <- suppressWarnings(as.numeric(max_cost_ft))
  if (is.na(max_cost_ft) || max_cost_ft < 0) max_cost_ft <- Inf
  required <- c("x", "y")
  if (!is.data.frame(tiles) || !all(required %in% names(tiles)) || !nrow(tiles)) {
    return(list(ok = FALSE, reason = "no_map", cost_ft = Inf, path = empty_path))
  }
  tiles <- tiles
  if (!is.null(map_id) && "map_id" %in% names(tiles)) {
    tiles <- tiles[suppressWarnings(as.integer(tiles$map_id)) == as.integer(map_id), , drop = FALSE]
  }
  tiles$x <- suppressWarnings(as.integer(tiles$x)); tiles$y <- suppressWarnings(as.integer(tiles$y))
  tiles <- tiles[!is.na(tiles$x) & !is.na(tiles$y), , drop = FALSE]
  if (!"blocks_movement" %in% names(tiles)) tiles$blocks_movement <- FALSE
  if (!"move_cost" %in% names(tiles)) tiles$move_cost <- 1
  tile_keys <- paste(tiles$x, tiles$y, sep = ",")
  start_key <- paste(as.integer(start_x), as.integer(start_y), sep = ",")
  target_key <- paste(as.integer(target_x), as.integer(target_y), sep = ",")
  if (!start_key %in% tile_keys || !target_key %in% tile_keys) {
    return(list(ok = FALSE, reason = "out_of_bounds", cost_ft = Inf, path = empty_path))
  }

  occupied_keys <- character()
  if (is.data.frame(occupants) && nrow(occupants) && all(c("x", "y") %in% names(occupants))) {
    occ <- occupants
    if (!is.null(map_id) && "map_id" %in% names(occ)) occ <- occ[as.integer(occ$map_id) == as.integer(map_id), , drop = FALSE]
    if (!is.null(exclude_actor_id) && "actor_id" %in% names(occ)) occ <- occ[as.character(occ$actor_id) != as.character(exclude_actor_id), , drop = FALSE]
    occupied_keys <- paste(as.integer(occ$x), as.integer(occ$y), sep = ",")
  }
  if (target_key %in% occupied_keys) {
    return(list(ok = FALSE, reason = "occupied", cost_ft = Inf, path = empty_path))
  }

  tile_info <- function(x, y) {
    index <- match(paste(x, y, sep = ","), tile_keys)
    if (is.na(index)) return(NULL)
    raw_cost <- suppressWarnings(as.numeric(tiles$move_cost[[index]] %||% 1))
    blocked <- isTRUE(tiles$blocks_movement[[index]]) || is.infinite(raw_cost)
    if (is.na(raw_cost) || !is.finite(raw_cost) || raw_cost < 1) raw_cost <- 1
    list(blocked = blocked, cost = raw_cost)
  }
  frontier <- data.frame(x = as.integer(start_x), y = as.integer(start_y), cost = 0, stringsAsFactors = FALSE)
  paths <- list(); paths[[start_key]] <- data.frame(x = as.integer(start_x), y = as.integer(start_y), step = 0L)
  best <- stats::setNames(0, start_key)
  directions <- expand.grid(dx = -1:1, dy = -1:1)
  directions <- directions[!(directions$dx == 0 & directions$dy == 0), , drop = FALSE]

  while (nrow(frontier)) {
    current_index <- which.min(frontier$cost)
    current <- frontier[current_index, , drop = FALSE]
    frontier <- frontier[-current_index, , drop = FALSE]
    current_key <- paste(current$x, current$y, sep = ",")
    if (identical(current_key, target_key)) break
    for (i in seq_len(nrow(directions))) {
      nx <- as.integer(current$x + directions$dx[[i]]); ny <- as.integer(current$y + directions$dy[[i]])
      next_key <- paste(nx, ny, sep = ","); info <- tile_info(nx, ny)
      if (is.null(info) || (!isTRUE(allow_blocked) && info$blocked)) next
      if (!isTRUE(allow_blocked) && directions$dx[[i]] != 0L && directions$dy[[i]] != 0L) {
        side_a <- tile_info(as.integer(current$x + directions$dx[[i]]), as.integer(current$y))
        side_b <- tile_info(as.integer(current$x), as.integer(current$y + directions$dy[[i]]))
        if (is.null(side_a) || is.null(side_b) || side_a$blocked || side_b$blocked) next
      }
      if (next_key %in% occupied_keys && !identical(next_key, target_key) && !isTRUE(allow_blocked)) next
      step_cost <- as.numeric(info$cost) * 5
      new_cost <- as.numeric(current$cost) + step_cost
      if (!is.finite(new_cost) || new_cost > as.numeric(max_cost_ft)) next
      previous <- unname(best[next_key])
      if (length(previous) && !is.na(previous) && previous <= new_cost) next
      best[next_key] <- new_cost
      previous_path <- paths[[current_key]]
      paths[[next_key]] <- rbind(previous_path, data.frame(x = nx, y = ny, step = nrow(previous_path)))
      frontier <- rbind(frontier, data.frame(x = nx, y = ny, cost = new_cost))
    }
  }
  if (is.null(paths[[target_key]])) return(list(ok = FALSE, reason = "unreachable", cost_ft = Inf, path = empty_path))
  list(ok = TRUE, reason = "ok", cost_ft = as.integer(round(best[[target_key]])), path = paths[[target_key]])
}

combat_line_tiles <- function(x1, y1, x2, y2) {
  x1 <- as.integer(x1); y1 <- as.integer(y1); x2 <- as.integer(x2); y2 <- as.integer(y2)
  samples <- max(abs(x2 - x1), abs(y2 - y1)) * 4L
  if (samples < 1L) return(data.frame(x = x1, y = y1))
  progress <- seq(0, 1, length.out = samples + 1L)
  out <- unique(data.frame(
    x = floor(x1 + (x2 - x1) * progress + 0.5),
    y = floor(y1 + (y2 - y1) * progress + 0.5)
  ))
  rownames(out) <- NULL
  out
}

combat_attack_geometry <- function(tiles, attacker_x, attacker_y, target_x, target_y,
                                   range_ft = 5L, long_range_ft = range_ft, map_id = NULL) {
  range_ft <- suppressWarnings(as.integer(range_ft)); long_range_ft <- suppressWarnings(as.integer(long_range_ft))
  if (is.na(range_ft) || range_ft < 1L) range_ft <- 5L
  if (is.na(long_range_ft) || long_range_ft < range_ft) long_range_ft <- range_ft
  distance_ft <- combat_grid_distance_ft(attacker_x, attacker_y, target_x, target_y)
  line <- combat_line_tiles(attacker_x, attacker_y, target_x, target_y)
  middle <- if (nrow(line) > 2L) line[2:(nrow(line) - 1L), , drop = FALSE] else line[0, , drop = FALSE]
  blockers <- middle[0, , drop = FALSE]
  if (nrow(middle) && is.data.frame(tiles) && nrow(tiles)) {
    view <- tiles
    if (!is.null(map_id) && "map_id" %in% names(view)) view <- view[as.integer(view$map_id) == as.integer(map_id), , drop = FALSE]
    if (!"blocks_vision" %in% names(view)) view$blocks_vision <- FALSE
    if (!"terrain" %in% names(view)) view$terrain <- ""
    keys <- paste(view$x, view$y, sep = ",")
    indices <- match(paste(middle$x, middle$y, sep = ","), keys)
    blocked <- !is.na(indices) & vapply(indices, function(index) {
      if (is.na(index)) return(FALSE)
      isTRUE(view$blocks_vision[[index]]) || identical(tolower(as.character(view$terrain[[index]])), "wall")
    }, logical(1))
    blockers <- middle[blocked, , drop = FALSE]
  }
  list(
    ok = distance_ft <= long_range_ft && !nrow(blockers),
    distance_ft = distance_ft,
    normal_range = distance_ft <= range_ft,
    in_range = distance_ft <= long_range_ft,
    line_clear = !nrow(blockers),
    blockers = blockers,
    line = line
  )
}

combat_hide_dc <- function(enemy_passive = 10L, terrain = "grass", light = "full",
                           adjacent_wall = FALSE, enemy_has_los = TRUE) {
  enemy_passive <- suppressWarnings(as.integer(enemy_passive))
  if (is.na(enemy_passive)) enemy_passive <- 10L
  terrain <- tolower(trimws(as.character(terrain %||% "grass")))
  light <- tolower(trimws(as.character(light %||% "full")))
  terrain_adjustment <- switch(
    terrain,
    forest = -3L, woodland = -3L, swamp = -1L, rubble = -1L,
    grass = 5L, road = 6L, stone = 5L, water = 6L, 2L
  )
  light_adjustment <- switch(light, dark = -5L, dim = -2L, 0L)
  cover_adjustment <- if (isTRUE(adjacent_wall) && !isTRUE(enemy_has_los)) -5L else
    if (isTRUE(adjacent_wall)) -1L else if (isTRUE(enemy_has_los)) 2L else 0L
  max(5L, min(30L, enemy_passive + terrain_adjustment + light_adjustment + cover_adjustment))
}

armor_meta_defaults_global <- function(meta = NULL) {
  meta <- meta %||% list()
  if (!is.list(meta)) meta <- list()
  
  meta$base_ac <- suppressWarnings(as.numeric(meta$base_ac %||% 11))
  if (is.na(meta$base_ac)) meta$base_ac <- 11
  
  meta$type <- as.character(meta$type %||% "Light")
  if (!meta$type %in% c("Light", "Medium", "Heavy", "Custom", "Shield", "Unarmoured")) meta$type <- "Light"
  
  meta$custom_max_dex <- suppressWarnings(as.numeric(meta$custom_max_dex %||% 0))
  if (is.na(meta$custom_max_dex)) meta$custom_max_dex <- 0
  
  meta$proficient <- isTRUE(meta$proficient)

  meta$equipment_slot <- tolower(as.character(meta$equipment_slot %||%
    if (identical(meta$type, "Shield")) "shield" else "body"))
  if (!meta$equipment_slot %in% c("body", "shield", "head", "accessory")) {
    meta$equipment_slot <- "body"
  }

  default_bonus <- if (identical(meta$equipment_slot, "shield")) meta$base_ac else 0
  meta$ac_bonus <- suppressWarnings(as.numeric(meta$ac_bonus %||% default_bonus))
  if (is.na(meta$ac_bonus)) meta$ac_bonus <- 0
  
  meta
}

calc_auto_ac_for_char <- function(char) {
  char <- validate_character(char)
  df <- inventory_normalize(char$inventory$items)

  class_entries <- char$build$classes %||% list()
  class_names <- if (is.list(class_entries) && length(class_entries)) {
    vapply(class_entries, function(entry) {
      tolower(trimws(as.character(entry$class %||% "")))
    }, character(1))
  } else {
    tolower(trimws(as.character(char$build$class %||% "")))
  }
  has_unarmoured_defence <- "barbarian" %in% class_names
  fighting_style <- as.character(
    char$build$level_choices$Fighter[["1"]]$fighting_style %||% ""
  )
  unarmoured_ac <- function() {
    ac <- 10L + get_character_ability_mod(char, "dex")
    if (has_unarmoured_defence) ac <- ac + get_character_ability_mod(char, "con")
    as.integer(ac)
  }
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(unarmoured_ac())
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
  
  if (nrow(arm) == 0) return(unarmoured_ac())

  metas <- lapply(arm$meta, function(meta) armor_meta_defaults_global(meta))
  slots <- vapply(metas, function(meta) meta$equipment_slot, character(1))
  body_index <- which(slots == "body")
  body_worn <- length(body_index) > 0L
  ac <- unarmoured_ac()

  if (body_worn) {
    meta <- metas[[body_index[[1L]]]]
    type <- meta$type %||% "Light"
    max_dex <- switch(type, Light = Inf, Medium = 2, Heavy = 0,
      Custom = as.numeric(meta$custom_max_dex %||% 0), Inf)
    equipment_bonus <- suppressWarnings(as.numeric(meta$material_armour_modifier %||% 0) +
      as.numeric(meta$quality_armour_modifier %||% 0))
    if (is.na(equipment_bonus)) equipment_bonus <- 0
    ac <- as.integer(as.numeric(meta$base_ac %||% 10) + min(dex_mod, max_dex) +
      (if (isTRUE(meta$proficient)) pb else 0L) + equipment_bonus)
  }

  # Add only the best equipped item in each supplementary slot. This prevents
  # two shields or several helms stacking while allowing body + shield + helm.
  for (slot in c("shield", "head", "accessory")) {
    bonuses <- vapply(metas[slots == slot], function(meta) as.numeric(meta$ac_bonus %||% 0), numeric(1))
    if (length(bonuses)) ac <- ac + max(bonuses, na.rm = TRUE)
  }
  if (body_worn && identical(fighting_style, "Defence")) ac <- ac + 1L
  as.integer(ac)
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
    return(max(1, prev))
  } else if (stage == 3) {
    return(prev + 1)
  } else {
    return(max(5, prev))  # tweak later if needed
  }
}

warmth_requirement_hours <- function(environment = list()) {
  climate <- tolower(trimws(as.character(environment$climate %||% "temperate")))
  weather <- tolower(trimws(as.character(environment$weather %||% "")))
  if (grepl("blizzard|whiteout|extreme cold|deep freeze|freezing",weather)) return(6)
  if (climate %in% c("alpine")) return(6)
  if (climate %in% c("cold")) return(12)
  if (climate %in% c("hot","tropical")) return(Inf)
  24
}

character_rest_status_cards <- function(x, has_fire = NULL, planned_actions = character(), environment = list(), phase_hours = 0) {
  x <- validate_character(x); status <- x$status %||% list(); cards <- list()
  add <- function(key,label,image,reason,tone="neutral") cards[[length(cards)+1L]] <<- list(key=key,label=label,image=paste0("assets/status-cards/",image),reason=reason,tone=tone)
  needs<-status$needs_hours%||%list();fire <- if(is.null(has_fire))isTRUE(status$has_fire%||%FALSE)else isTRUE(has_fire);warmth_limit<-warmth_requirement_hours(environment);warmth_hours<-as.numeric(needs$warmth%||%0)
  warm<-fire||is.infinite(warmth_limit)||warmth_hours<warmth_limit
  if(warm)add("warm","Warm","rest/warm.png",if(is.infinite(warmth_limit))"This card remains active in this climate; no additional warmth is required."else paste0("This card will last ",round(max(0,warmth_limit-warmth_hours),1)," more hours. Lighting a fire resets it for the whole party."),"positive") else add("cold","Cold","rest/cold.png",paste0("This card remains until the party lights a fire. In ",tolower(as.character(environment$climate%||%"temperate"))," conditions, each ",warmth_limit," hours without warmth can add exhaustion."),"risk")
  food_hours<-as.numeric(needs$food%||%0);water_hours<-as.numeric(needs$water%||%0)
  if(food_hours<24)add("well_fed","Well Fed","rest/well_fed.png",paste0("This card will last ",round(max(0,24-food_hours),1)," more hours. Eating resets it to 24 hours."),"positive") else add("hungry","Hungry","rest/hungry.png","This card remains until you eat. Each further 24 hours without food can add exhaustion.","risk")
  if(water_hours<24)add("hydrated","Hydrated","rest/hydrated.png",paste0("This card will last ",round(max(0,24-water_hours),1)," more hours. Drinking water resets it to 24 hours."),"positive") else add("parched","Parched","rest/parched.png","This card remains until you drink water. Each further 24 hours without water can add exhaustion.","risk")
  race <- tolower(trimws(as.character(x$meta$race%||%"")))
  if(identical(race,"tylwyth teg")){
    addiction <- (x$resources$blood%||%list())$addiction%||%list();stage<-suppressWarnings(as.integer(addiction$stage%||%1L));if(is.na(stage))stage<-1L;stage<-max(1L,min(4L,stage))
    intake<-suppressWarnings(as.numeric(addiction$current_day_intake%||%0));if(is.na(intake))intake<-0;needed<-suppressWarnings(as.numeric(required_intake(addiction)));if(is.na(needed))needed<-1;enough<-intake>=needed;blood_hours<-as.numeric(needs$blood%||%0);check_in<-max(0,24-(blood_hours%%24));if(check_in==0)check_in<-24
    crosses_phase<-as.numeric(phase_hours%||%0)>=check_in
    add(if(enough)"sufficient_blood"else"insufficient_blood",if(enough)"Sufficient Blood"else"Insufficient Blood",if(enough)"rest/sufficient_blood.png"else"rest/insufficient_blood.png",if(enough)paste0("Requirement met: ",round(intake,2)," of ",round(needed,2)," pints. This card lasts ",round(check_in,1)," more hours.",if(crosses_phase)" The current phase crosses that check; a new blood-intake cycle begins before the phase ends."else"")else paste0("Drink ",round(max(0,needed-intake),2)," more pints before the blood check in ",round(check_in,1)," hours. If still short, Stage ",stage," withdrawal will apply.",if(crosses_phase)" This phase crosses the threshold."else""),if(enough)"positive"else"risk")
    files<-c("blood_addiction_1_thirsting.png","blood_addiction_2_craving.png","blood_addiction_3_bloodbound.png","blood_addiction_4_bloodstarved.png");labels<-c("Thirsting","Craving","Bloodbound","Bloodstarved")
    add(paste0("blood_addiction_",stage),paste("Blood Addiction",stage,"—",labels[[stage]]),paste0("blood-addiction/",files[[stage]]),paste("Persistent blood-addiction stage",stage,"."),"persistent")
  }
  exhaustion<-suppressWarnings(as.integer(status$exhaustion%||%0L));if(is.na(exhaustion))exhaustion<-0L
  if(exhaustion>0L){exhaustion<-max(1L,min(6L,exhaustion));files<-c("exhaustion_1_weary.png","exhaustion_2_fatigued.png","exhaustion_3_spent.png","exhaustion_4_haggard.png","exhaustion_5_wretched.png","exhaustion_6_collapsed.png");labels<-c("Weary","Fatigued","Spent","Haggard","Wretched","Collapsed");add(paste0("exhaustion_",exhaustion),paste("Exhaustion",exhaustion,"—",labels[[exhaustion]]),paste0("conditions/",files[[exhaustion]]),paste("Current exhaustion level:",exhaustion),"persistent")}
  actions<-unique(as.character(planned_actions%||%character()))
  sleep_hours<-as.numeric(needs$sleep%||%0);if(sleep_hours>=24&&!"long_rest"%in%actions)add("long_rest_overdue","Long Rest Overdue","rest/long_rest.png",paste0("No Long Rest for ",round(sleep_hours,1)," hours; each 24-hour threshold adds exhaustion."),"risk")
  if("long_rest"%in%actions)add("rested","Rest Planned","rest/rested.png","Six hours of sleep are allocated; you will be Rested when the DM resolves the phase.","planned") else if(sleep_hours<24)add("rested","Rested","rest/rested.png",paste0("Your last Long Rest remains valid for ",round(max(0,24-sleep_hours),1)," more hours."),"positive") else add("not_rested","Not Rested","rest/not_rested.png","You have gone 24 hours without a Long Rest. Resolve a planned Long Rest to recover.","risk")
  if("short_rest"%in%actions)add("short_rest","Short Rest","rest/short_rest.png","One hour allocated. Recovery is applied when the phase resolves.","planned")
  cards
}

character_condition_status_cards <- function(x) {
  x<-validate_character(x);status<-x$status%||%list();values<-unique(tolower(trimws(as.character(c(status$conditions%||%character(),status$effects%||%character())))))
  booleans<-c("blinded","charmed","deafened","frightened","grappled","incapacitated","invisible","paralysed","petrified","poisoned","prone","restrained","stunned","unconscious")
  for(key in booleans)if(isTRUE(status[[key]]%||%FALSE))values<-unique(c(values,key))
  values<-gsub("paralyzed","paralysed",values,fixed=TRUE);values<-intersect(values,booleans)
  descriptions<-c(blinded="You cannot see and automatically fail checks requiring sight.",charmed="You are under a charm effect.",deafened="You cannot hear and automatically fail checks requiring hearing.",frightened="You are affected by fear.",grappled="Your movement is restricted by a grapple.",incapacitated="You cannot take actions or reactions.",invisible="You cannot be seen without special senses or magic.",paralysed="You are paralysed and unable to move or act.",petrified="You have been transformed into an inert solid substance.",poisoned="You have disadvantage on attacks and ability checks.",prone="You are on the ground until you stand.",restrained="Your movement is restricted and attacks are affected.",stunned="You are incapacitated and unable to move.",unconscious="You are unconscious and unable to act.")
  lapply(values,function(key)list(key=paste0("condition_",key),label=tools::toTitleCase(key),image=paste0("assets/status-cards/conditions/",key,".png"),reason=unname(descriptions[[key]]),tone="persistent"))
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
      gain <- if (deficit > 0) 1 else 0
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

advance_day_all_legacy <- function(x, add_log = NULL, state = NULL) {
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

  spoilage<-spoil_character_food(x,x$meta$day);x<-spoilage$char
  if(length(spoilage$spoiled))log_safe(paste0("🥀 Spoiled food discarded: ",paste(spoilage$spoiled,collapse=", "),"."),flash="gold")
  
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

# Offline/manual day advancement uses the same rolling-hour rules as the shared
# phase resolver. The former calendar-day implementation is retained above only
# for save-history reference and is no longer called by the application.
advance_day_all <- function(x, add_log = NULL, state = NULL) {
  x<-validate_character(x);x$status<-x$status%||%list();x$status$needs_hours<-x$status$needs_hours%||%list()
  names<-c("food","water","warmth","blood","sleep");old<-vapply(names,function(k)as.numeric(x$status$needs_hours[[k]]%||%0),numeric(1));next_hours<-old+24
  if(isTRUE(x$status$ate_today))next_hours[["food"]]<-0;if(isTRUE(x$status$drank_today))next_hours[["water"]]<-0
  warmth_limit<-warmth_requirement_hours(list(climate=x$environment$temperature%||%"temperate",weather=""));if(isTRUE(x$status$has_fire)||is.infinite(warmth_limit))next_hours[["warmth"]]<-0
  if(isTRUE(x$status$resting)){next_hours[["sleep"]]<-0;x$status$exhaustion<-max(0L,as.integer(x$status$exhaustion%||%0L)-1L)}
  thresholds<-c(food=24,water=24,warmth=warmth_limit,blood=24,sleep=24);penalties<-vapply(names,function(k)if(is.infinite(thresholds[[k]]))0 else floor(next_hours[[k]]/thresholds[[k]])-floor(old[[k]]/thresholds[[k]]),numeric(1))
  x$status$exhaustion<-min(6L,as.integer(x$status$exhaustion%||%0L)+sum(pmax(0,penalties[c("food","water","warmth","sleep")])))
  if(tolower(trimws(as.character(x$meta$race%||%"")))=="tylwyth teg"&&penalties[["blood"]]>0)for(i in seq_len(penalties[["blood"]]))x<-advance_blood_day(x,add_log,state)
  x$status$needs_hours<-as.list(next_hours);x$meta$day<-as.integer(x$meta$day%||%1L)+1L;spoilage<-spoil_character_food(x,x$meta$day);x<-spoilage$char
  x$status$ate_today<-FALSE;x$status$drank_today<-FALSE;x$status$foraged_today<-FALSE;x$status$resting<-FALSE;x$status$has_fire<-FALSE;x$status$gathered_wood_today<-FALSE
  sync_exhaustion_effects(x)
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

equipped_magical_traits <- function(char) {
  char<-validate_character(char);inv<-inventory_normalize(char$inventory$items);out<-list(resistances=character(),immunities=character(),vulnerabilities=character(),condition_immunities=character())
  if(!nrow(inv))return(out);active<-as.logical(inv$equipped)&!as.logical(inv$in_bag);active[is.na(active)]<-FALSE
  for(i in which(active)){meta<-inv$meta[[i]]%||%list();if(!isTRUE(meta$is_magical))next;for(field in names(out))out[[field]]<-unique(c(out[[field]],tolower(as.character(meta[[field]]%||%character()))))}
  out
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
starting_character_hp <- function(class_name, level = 1L, constitution = 10L,
                                  class_defs = NULL) {
  level <- suppressWarnings(as.integer(level)); if (is.na(level)) level <- 1L
  level <- max(1L, min(20L, level))
  constitution <- suppressWarnings(as.integer(constitution)); if (is.na(constitution)) constitution <- 10L
  con_modifier <- floor((constitution - 10L) / 2L)
  if (is.null(class_defs)) class_defs <- if (exists("CLASSES", inherits=TRUE)) get("CLASSES", inherits=TRUE) else list()
  class_name <- as.character(class_name %||% "")
  hit_die <- suppressWarnings(as.integer((class_defs[[class_name]] %||% list())$hit_die %||% 10L))
  if (is.na(hit_die) || hit_die < 1L) hit_die <- 10L
  first_level <- max(1L, hit_die + con_modifier)
  later_level <- max(1L, floor(hit_die / 2L) + 1L + con_modifier)
  as.integer(first_level + (level - 1L) * later_level)
}

camp_gathering_bonus <- function(char) {
  char<-validate_character(char);score<-suppressWarnings(as.integer(char$abilities$bld_str%||%10L));if(is.na(score))score<-10L
  skills<-char$prof$skills%||%list();idx<-which(tolower(names(skills))=="survival");rank<-if(length(idx))as.character(skills[[idx[[1L]]]]%||%"None")else"None"
  proficiency<-if(exists("character_proficiency_bonus",mode="function",inherits=TRUE))character_proficiency_bonus(char)else{level<-as.integer(char$build$level%||%1L);2L+max(0L,floor((level-1L)/4L))}
  multiplier<-if(tolower(rank)=="expertise")2L else if(tolower(rank)=="proficient")1L else 0L
  as.integer(floor((score-10L)/2L)+multiplier*proficiency)
}

camp_gathering_yield <- function(total) {
  total<-suppressWarnings(as.integer(total));if(is.na(total)||total<10L)0L else if(total<15L)1L else if(total<20L)2L else if(total<25L)3L else 4L
}

camp_foraging_reward <- function(total, choice_override=NULL) {
  portions<-camp_gathering_yield(total);if(portions<1L)return(NULL)
  tiers<-list(
    `1`=list(c("Bluecap Mushrooms",2L,.4),c("Wild Blackberries",2L,.5),c("Wood Sorrel Bundle",1L,.3)),
    `2`=list(c("Chanterelle Basket",3L,1),c("Wild Apple Bundle",6L,1.5),c("River Mussels",1L,2)),
    `3`=list(c("Fresh River Trout",1L,2),c("Snared Grouse",2L,2.5),c("Wild Honeycomb",30L,1)),
    `4`=list(c("Trapped Rabbit",2L,4),c("Venison Haunch",2L,6),c("Forager's Bounty",4L,5))
  )
  choices<-tiers[[as.character(portions)]];index<-if(is.null(choice_override))sample(seq_along(choices),1L)else max(1L,min(length(choices),as.integer(choice_override)))
  picked<-choices[[index]];list(id=paste0("forage_",gsub("_+$","",gsub("[^a-z0-9]+","_",tolower(picked[[1L]])))),name=picked[[1L]],type="consumable",desc=paste0("Fresh food gathered on a Survival check of ",as.integer(total),"."),value=0,weight=as.numeric(picked[[3L]]),qty=1,meta=list(category="food",ration_value=portions,shelf_life_days=as.integer(picked[[2L]]),foraged=TRUE))
}

add_foraged_food <- function(x,reward,current_day=NULL) {
  x<-validate_character(x);if(is.null(reward))return(x);day<-as.integer(current_day%||%x$meta$day%||%1L);reward$meta<-food_item_meta(reward$meta%||%list(),reward$qty%||%1,day);reward$id<-paste0(reward$id%||%"forage_food","_",day,"_",sample(1000:9999,1));row<-enemy_loot_to_inventory_row(reward,reward$id);x$inventory$items<-inventory_normalize(rbind(inventory_normalize(x$inventory$items),row));x
}

character_skill_modifier <- function(char,skill,skill_defs=NULL) {
  char<-validate_character(char);skill<-as.character(skill%||%"");key<-gsub(" ","_",tolower(skill))
  if(is.null(skill_defs))skill_defs<-if(exists("SKILLS_LIST",inherits=TRUE))get("SKILLS_LIST",inherits=TRUE)else data.frame()
  row<-if(is.data.frame(skill_defs)&&nrow(skill_defs))skill_defs[tolower(skill_defs$Skill)==tolower(skill),,drop=FALSE]else data.frame();ability<-if(nrow(row))as.character(row$Ability[[1L]])else"int"
  score<-suppressWarnings(as.integer(char$abilities[[ability]]%||%10L));if(is.na(score))score<-10L
  rank<-as.character(char$prof$skills[[key]]%||%"None");multiplier<-if(tolower(rank)=="expertise")2L else if(tolower(rank)=="proficient")1L else 0L
  proficiency<-if(exists("character_proficiency_bonus",mode="function",inherits=TRUE))character_proficiency_bonus(char)else{level<-as.integer(char$build$level%||%1L);2L+max(0L,floor((level-1L)/4L))}
  as.integer(floor((score-10L)/2L)+multiplier*proficiency)
}

skill_card_count_for_rank <- function(rank) {
  rank <- tolower(as.character(rank %||% "none"))
  if (identical(rank, "expertise")) 2L else if (identical(rank, "proficient")) 1L else 0L
}

character_skill_cards <- function(char, skill) {
  char <- validate_character(char)
  key <- gsub(" ", "_", tolower(as.character(skill %||% "")))
  allowed <- c("reliable", "wild_card", "inspired")
  cards <- unique(as.character(char$prof$skill_cards[[key]] %||% character()))
  cards <- cards[cards %in% allowed]
  needed <- skill_card_count_for_rank(char$prof$skills[[key]] %||% "None")
  if (needed < 1L) character() else cards[seq_len(min(length(cards), needed))]
}

resolve_skill_card_roll <- function(natural_roll, modifier, proficiency_bonus, cards = character(), wild_roll = NULL) {
  natural_roll <- suppressWarnings(as.integer(natural_roll))
  modifier <- suppressWarnings(as.integer(modifier))
  proficiency_bonus <- suppressWarnings(as.integer(proficiency_bonus))
  if (is.na(natural_roll) || natural_roll < 1L || natural_roll > 20L) stop("Natural skill roll must be between 1 and 20.")
  if (is.na(modifier)) modifier <- 0L
  if (is.na(proficiency_bonus) || proficiency_bonus < 0L) proficiency_bonus <- 0L
  cards <- unique(as.character(cards %||% character()))
  card_bonus <- 0L
  effects <- character()
  if ("reliable" %in% cards && natural_roll <= 5L) {
    card_bonus <- card_bonus + proficiency_bonus
    effects <- c(effects, paste0("Reliable +", proficiency_bonus))
  }
  if ("inspired" %in% cards && natural_roll >= 16L) {
    card_bonus <- card_bonus + proficiency_bonus
    effects <- c(effects, paste0("Inspired +", proficiency_bonus))
  }
  wild_die <- NA_integer_
  if ("wild_card" %in% cards) {
    wild_die <- if (is.null(wild_roll)) sample.int(6L, 1L) else suppressWarnings(as.integer(wild_roll))
    if (is.na(wild_die) || wild_die < 1L || wild_die > 6L) stop("Wild Card roll must be between 1 and 6.")
    if (wild_die == 1L) {
      card_bonus <- card_bonus - proficiency_bonus
      effects <- c(effects, paste0("Wild Card 1: -", proficiency_bonus))
    } else if (wild_die == 6L) {
      card_bonus <- card_bonus + proficiency_bonus
      effects <- c(effects, paste0("Wild Card 6: +", proficiency_bonus))
    } else {
      effects <- c(effects, paste0("Wild Card ", wild_die, ": no change"))
    }
  }
  list(
    natural_roll = natural_roll,
    modifier = modifier,
    proficiency_bonus = proficiency_bonus,
    cards = cards,
    wild_roll = wild_die,
    card_bonus = as.integer(card_bonus),
    total = as.integer(natural_roll + modifier + card_bonus),
    effects = effects
  )
}

party_skill_support_result <- function(leader_total, helper_totals = numeric(), support_cap = 2L) {
  leader_total <- suppressWarnings(as.integer(leader_total))
  if (is.na(leader_total)) return(list(total=NA_integer_,support=0L,contributions=integer()))
  helper_totals <- suppressWarnings(as.numeric(helper_totals));helper_totals<-helper_totals[is.finite(helper_totals)]
  contributions<-ifelse(helper_totals<10,-1L,ifelse(helper_totals>=15,1L,0L));cap<-max(0L,suppressWarnings(as.integer(support_cap%||%0L)));support<-max(-cap,min(cap,sum(contributions)))
  list(total=as.integer(leader_total+support),support=as.integer(support),contributions=as.integer(contributions))
}

merchant_pricing_multiplier <- function(pricing_style=c("standard","cheap","expensive","very_expensive")) {
  pricing_style<-match.arg(pricing_style)
  c(cheap=.75,standard=1,expensive=1.35,very_expensive=1.75)[[pricing_style]]
}

merchant_item_stock_weight <- function(item) {
  rarity<-tolower(as.character((item$meta%||%list())$rarity%||%"common"))
  c(common=1,uncommon=.55,rare=.20,very_rare=.07,legendary=.015)[[rarity]]%||%1
}

merchant_haggle_terms <- function(base_value,direction=c("buy","sell"),temperament=c("fair","hard","generous"),roll,dc_penalty=0L,pricing_style="standard") {
  direction<-match.arg(direction);temperament<-match.arg(temperament)
  base<-max(0,as.numeric(base_value%||%0));roll<-as.integer(roll%||%0L)
  dc<-c(generous=10L,fair=13L,hard=16L)[[temperament]]+max(0L,as.integer(dc_penalty%||%0L));success<-roll>=dc
  normal<-if(direction=="buy")c(generous=.90,fair=1,hard=1.15)[[temperament]]*merchant_pricing_multiplier(pricing_style) else c(generous=.65,fair=.50,hard=.35)[[temperament]]
  margin<-if(success)min(.25,.10+.05*floor(max(0,roll-dc)/5))else 0
  multiplier<-if(success){if(direction=="buy")normal-margin else normal+margin}else{if(direction=="buy")normal+.10 else max(.10,normal-.10)}
  list(base=base,dc=dc,roll=roll,success=success,multiplier=multiplier,price=max(if(base>0)1 else 0,round(base*multiplier)))
}

merchant_stock_category <- function(item) {
  type<-tolower(as.character(item$type%||%"item"));category<-tolower(as.character((item$meta%||%list())$category%||%""))
  if(type=="weapon")"weapon"else if(type%in%c("armor","armour"))"armour"else if(category=="food")"food"else if(category%in%c("mundane_loot","tool"))"general"else if(category%in%c("consumable","crafting","magical_item","treasure","quest"))category else"general"
}

merchant_select_stock <- function(items,n,specialty="general") {
  if(!length(items)||n<1L)return(list());n<-min(as.integer(n),length(items));weights<-vapply(items,merchant_item_stock_weight,numeric(1));if(specialty!="general")return(sample(items,n,replace=FALSE,prob=weights))
  groups<-split(items,vapply(items,merchant_stock_category,character(1)));targets<-c(general=.35,food=.25,consumable=.10,weapon=.10,armour=.10,crafting=.05,magical_item=.03,treasure=.02);chosen<-list()
  for(category in names(targets)){pool<-groups[[category]]%||%list();take<-min(length(pool),max(if(category%in%c("general","food"))1L else 0L,as.integer(round(n*targets[[category]]))));if(take>0L)chosen<-c(chosen,sample(pool,take,replace=FALSE))}
  used<-vapply(chosen,function(x)as.character(x$id%||%""),character(1));remaining<-Filter(function(x)!as.character(x$id%||%"")%in%used,items);needed<-n-length(chosen);if(needed>0L&&length(remaining)){staples<-Filter(function(x)merchant_stock_category(x)%in%c("general","food"),remaining);fill<-if(length(staples)>=needed)staples else remaining;fill_weights<-vapply(fill,merchant_item_stock_weight,numeric(1));chosen<-c(chosen,sample(fill,min(needed,length(fill)),replace=FALSE,prob=fill_weights))};chosen[seq_len(min(n,length(chosen)))]
}

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
  x$prof$tools  <- x$prof$tools  %||% list()
  if (!is.list(x$prof$saves))  x$prof$saves  <- list()
  if (!is.list(x$prof$skills)) x$prof$skills <- list()
  if (!is.list(x$prof$skill_cards)) x$prof$skill_cards <- list()
  if (!is.list(x$prof$tools))  x$prof$tools  <- list()

  # Canonical damage traits. Combat reads these directly; legacy locations are
  # still read for backwards compatibility.
  x$combat_profile <- x$combat_profile %||% list()
  if (!is.list(x$combat_profile)) x$combat_profile <- list()
  for (field in c("resistances", "immunities", "vulnerabilities")) {
    x$combat_profile[[field]] <- unique(tolower(as.character(
      x$combat_profile[[field]] %||% character()
    )))
  }

  if (exists("apply_unlocked_class_effects", mode = "function", inherits = TRUE) &&
      exists("CLASSES", inherits = TRUE)) {
    x <- tryCatch(
      apply_unlocked_class_effects(x, get("CLASSES", inherits = TRUE)),
      error = function(e) x
    )
  }
  
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
  
  # Exhaustion changes effective maximum HP at read/use time. Never mutate the
  # canonical stored maximum here: validate_character() is called repeatedly.
  
  
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
  
  x$inventory$items <- merge_legacy_weapon_mode_items(x$inventory$items)
  
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
