library(shiny)
cat("SOURCED server.R\n")
server_player <- function(input, output, session) {
  # runApp() restores its caller's working directory after the initial app
  # load. Every browser connection invokes this function afterwards, so an
  # installed build must re-anchor relative module paths for each session.
  drachuri_app_root <- Sys.getenv("DRACHURI_APP_DIR", "")
  if (nzchar(drachuri_app_root) && dir.exists(drachuri_app_root)) {
    setwd(normalizePath(drachuri_app_root, mustWork = TRUE))
  }
  
  # ------------------------------------------------------------
  # Shared control state
  # ------------------------------------------------------------
  ctrl <- reactiveValues(
    session_id = NULL,
    map_id = NULL,
    selected_actor_id = NULL,
    selected_tile = NULL,
    refresh_key = 0L
  )
  
  bump_refresh <- function() {
    ctrl$refresh_key <- as.integer(ctrl$refresh_key %||% 0L) + 1L
    invisible(ctrl$refresh_key)
  }
  
  current_session_id <- reactive({
    sid <- ctrl$session_id %||% input$ctrl_session_id %||% 1
    suppressWarnings(as.integer(sid))
  })
  
  current_map_id <- reactive({
    mid <- ctrl$map_id %||% 1
    suppressWarnings(as.integer(mid))
  })
  
  # ------------------------------------------------------------
  # Shared session snapshot
  # ------------------------------------------------------------
  session_data <- reactive({
    ctrl$refresh_key
    sid <- current_session_id()
    
    if (is.null(sid) || is.na(sid) || sid < 1) {
      return(list(
        session = data.frame(),
        players = data.frame(),
        positions = data.frame(),
        combat = data.frame(),
        events = data.frame()
      ))
    }
    
    out <- tryCatch(
      get_session_overview(sid),
      error = function(e) {
        list(
          session = data.frame(),
          players = data.frame(),
          positions = data.frame(),
          combat = data.frame(),
          events = data.frame(),
          error = e$message
        )
      }
    )
    
    out
  })
  
  session_tbl <- reactive({
    session_data()$session %||% data.frame()
  })
  
  players_tbl <- reactive({
    session_data()$players %||% data.frame()
  })
  
  positions_tbl <- reactive({
    session_data()$positions %||% data.frame()
  })
  
  combat_tbl <- reactive({
    session_data()$combat %||% data.frame()
  })
  
  events_tbl <- reactive({
    session_data()$events %||% data.frame()
  })
  
  # ------------------------------------------------------------
  # Top-level refresh control
  # ------------------------------------------------------------
  observeEvent(input$ctrl_refresh, {
    bump_refresh()
  }, ignoreInit = TRUE)
  
  # ------------------------------------------------------------
  # Optional fallback session selector in main control shell
  # ------------------------------------------------------------
  observe({
    sid <- suppressWarnings(as.integer(input$ctrl_session_id %||% NA))
    if (!is.na(sid) && sid > 0) {
      ctrl$session_id <- sid
    }
  })
  
  
  #Toast!
  source("server/core_character.R")
  core <- characterCoreServer(input, output, session)

  # Modules use this to pause expensive database polling while their tab is
  # hidden. Changing tabs immediately invalidates the relevant observers, so
  # the newly opened screen still refreshes without waiting for its timer.
  observeEvent(input$main_tabs, {
    core$state$active_tab <- as.character(input$main_tabs %||% "camp")
  }, ignoreInit = FALSE)

  observe({
    invalidateLater(1800,session)
    cid<-as.character(core$state$char_id%||%"");if(!nzchar(cid)||isTRUE(core$state$offline_mode))return()
    update<-consume_character_refresh(cid);if(is.null(update)||!is.list(update$character))return()
    core$state$char<-update$character;if(is.function(core$bump_char_rev))core$bump_char_rev()
    phase_messages<-as.character(update$messages[as.character(update$kinds)=="phase_resolution"]%||%character())
    other_messages<-as.character(update$messages[as.character(update$kinds)!="phase_resolution"]%||%character())
    if(length(other_messages))showNotification(paste(unique(other_messages),collapse="\n"),type="message",duration=8)
    if(length(phase_messages))showModal(modalDialog(title="Phase Resolved",div(class="phase-resolution-screen",h4("What happened"),lapply(unique(phase_messages),p),p("Your character sheet, status cards, health and Sindre have been refreshed.")),footer=modalButton("Continue"),easyClose=FALSE,size="m"))
  })
  
  source("session_db.R") #Helpers for sessions (overflow from global basically)
  source("plug/skills_data.R")
  source("plug/combat_data.R")
  source("plug/game_data.R")
  source("shared/class_feature_core.R")
  
  
  source("server/stats_module.R")
  source("server/inventory_module.R")
  source("server/skills_module.R")
  source("server/armoury_module.R")
  source("server/diary_module.R")
  source("server/camp_module.R")
  source("server/sidebar_module.R")
  source("server/landing_module.R")
  source("server/private_notes_module.R")
  source("server/merchant_module.R")
  source("server/chest_module.R")
  source("server/story_module.R")
  source("server/dice_module.R")
  source("server/magic_module.R")
  source("server/blood_module.R")
  source("server/rest_module.R")
  source("server/level_module.R")
  source("server/HUD.R")
  source("server/debug_combat_module.R")
  source("server/party_hud_module.R")
  source("server/rune_crafting_module.R")
  
  
  
  # One compact live snapshot feeds the HUD, party HUD, and combat module.
  # It uses one checked-out database connection per refresh and only invalidates
  # downstream reactives when the returned data actually changed.
  live_snapshot <- reactiveVal(empty_player_live_snapshot())
  live_snapshot_signature <- reactiveVal(NULL)
  live_snapshot_trigger <- reactiveVal(0L)
  last_notified_hp <- reactiveVal(NA_integer_)
  last_notified_character <- reactiveVal("")

  refresh_live_snapshot <- function() {
    live_snapshot_trigger(isolate(live_snapshot_trigger()) + 1L)
    invisible(TRUE)
  }

  observe({
    invalidateLater(3000, session)
    live_snapshot_trigger()

    sid <- suppressWarnings(as.integer(core$state$active_session_id %||% NA))
    cid <- as.character(core$state$char_id %||% "")
    if (isTRUE(core$state$offline_mode) || is.na(sid) || sid < 1 || !nzchar(cid)) {
      empty <- empty_player_live_snapshot()
      empty_signature <- serialize(empty[names(empty) != "fetched_at"], NULL, version = 2)
      if (!identical(empty_signature, isolate(live_snapshot_signature()))) {
        live_snapshot_signature(empty_signature)
        live_snapshot(empty)
      }
      return()
    }

    snapshot_started <- proc.time()[["elapsed"]]
    snapshot <- tryCatch(
      # The control app owns game_sessions.active_encounter_id. Always let the
      # session row select the encounter so players follow DM changes.
      get_player_live_snapshot(sid, cid, encounter_id = NULL, event_limit = 20L),
      error = function(e) {
        message("Live snapshot refresh failed: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(snapshot)) return()

    if (identical(tolower(Sys.getenv("DND_PROFILE", "false")), "true")) {
      elapsed_ms <- round((proc.time()[["elapsed"]] - snapshot_started) * 1000)
      payload_kb <- round(length(serialize(snapshot, NULL, version = 2)) / 1024, 1)
      message("[profile] live snapshot: ", elapsed_ms, " ms, ", payload_kb, " KiB")
    }

    signature_payload <- snapshot[names(snapshot) != "fetched_at"]
    signature <- serialize(signature_payload, NULL, version = 2)
    if (!identical(signature, isolate(live_snapshot_signature()))) {
      live_snapshot_signature(signature)
      live_snapshot(snapshot)

      session_row <- snapshot$session %||% data.frame()
      if (is.data.frame(session_row) && nrow(session_row) > 0 &&
          "active_encounter_id" %in% names(session_row)) {
        active_eid <- suppressWarnings(as.integer(session_row$active_encounter_id[1] %||% NA))
        core$state$active_encounter_id <- if (is.na(active_eid)) NULL else active_eid
      }
    }
  })

  observe({
    self<-live_snapshot()$self_player%||%data.frame();if(!is.data.frame(self)||!nrow(self))return();current_cid<-as.character(core$state$char_id%||%"");if(!identical(current_cid,isolate(last_notified_character()))){last_notified_character(current_cid);last_notified_hp(NA_integer_)};hp<-suppressWarnings(as.integer(self$current_hp[1]%||%NA));old<-isolate(last_notified_hp())
    if(!is.na(old)&&!is.na(hp)&&hp<old)showNotification(paste0("You took ",old-hp," damage. HP: ",hp," / ",as.integer(self$max_hp[1]%||%hp)),type="error",duration=7)
    if(!is.na(hp))last_notified_hp(hp)
  })

  hudServer("hud", core$state, live_snapshot = live_snapshot)
  # One database heartbeat handles both offline and recovery transitions.
  # Keeping this in one observer avoids every player opening two redundant
  # Supabase connections on each heartbeat.
  observe({
    invalidateLater(15000, session)
    
    was_offline <- isTRUE(core$state$offline_mode)
    db_ok <- is_db_available()
    
    core$state$offline_mode <- !db_ok
    
    if (!db_ok && !isTRUE(core$state$offline_modal_shown)) {
      core$state$offline_modal_shown <- TRUE
      
      showModal(
        modalDialog(
          title = "Offline mode launched",
          "Unable to connect to the database. The app will continue in offline mode.",
          tags$p(
            style = "margin-top:10px;",
            "You can still load from RDS, edit your character, and download a backup."
          ),
          tags$p(
            "Server-backed features like Continue Adventure and live session sync will be unavailable until the connection returns."
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
    }
    
    if (was_offline && db_ok && is.function(core$add_log)) {
      try(core$add_log("✅ Database connection restored.", toast = TRUE), silent = TRUE)
    }
  })
  
  observe({
    cat("char_id:", core$state$char_id, "\n")
    cat("active_session_id:", core$state$active_session_id, "\n")
  })

  # ============================================================
  # Debounced autosave from canonical core state
  # ============================================================
  debounced_char <- debounce(
    reactive(core$state$char),
    2000
  )

  # A reactive character can invalidate even when its serialized content has
  # not changed. Remember the last successful payload so those invalidations do
  # not produce unnecessary database writes.
  last_saved_character <- reactiveVal(NULL)
  
  observe({
    char <- debounced_char()
    offline_mode <- isTRUE(core$state$offline_mode)
    sync_enabled <- isTRUE(core$state$sync_enabled)
    
    req(char)
    
    if (isTRUE(core$restoring())) return()
    if (offline_mode) return()
    if (!sync_enabled) return()
    
    char <- validate_character(char)

    payload_signature <- jsonlite::toJSON(
      char,
      auto_unbox = TRUE,
      null = "null",
      dataframe = "rows",
      digits = NA
    )
    if (identical(payload_signature, isolate(last_saved_character()))) return()
    
    # extra protection: do not sync obvious blank starter characters
    has_identity <- nzchar(trimws(char$meta$name %||% "")) ||
      nzchar(trimws(char$build$class %||% "")) ||
      nzchar(trimws(char$meta$race %||% ""))
    
    if (!has_identity) return()
    
    char_id <- isolate(core$state$char_id)
    
    saved_id <- tryCatch(
      save_character_to_db(char, char_id = char_id),
      error = function(e) {
        message("Autosave failed: ", e$message)
        NULL
      }
    )
    
    if (!is.null(saved_id) && is.null(char_id)) {
      core$state$char_id <- saved_id
      
      if (is.function(core$add_log)) {
        try(core$add_log("☁️ Character synced to server.", toast = TRUE), silent = TRUE)
      }
    }

    if (!is.null(saved_id)) {
      last_saved_character(payload_signature)
    }
  })
  
  observeEvent(core$state$offline_mode, {
    if (isTRUE(core$state$offline_mode)) return()
    
    char <- isolate(core$state$char)
    char_id <- isolate(core$state$char_id)
    
    req(char)
    if (!is.null(char_id)) return()
    if (isTRUE(core$restoring())) return()
    
    char <- validate_character(char)
    
    saved_id <- tryCatch(
      save_character_to_db(char, char_id = NULL),
      error = function(e) {
        message("Reconnect sync failed: ", e$message)
        NULL
      }
    )
    
    if (!is.null(saved_id)) {
      core$state$char_id <- saved_id
      
      if (is.function(core$add_log)) {
        try(core$add_log("☁️ Offline character synced after reconnect.", toast = TRUE), silent = TRUE)
      }
    }
  }, ignoreInit = TRUE)
  
  #Return to camp
  observeEvent(input$return_to_camp, {
    updateTabsetPanel(session, "main_tabs", selected = "camp")
  }, ignoreInit = TRUE)
  observeEvent(input$shortcut_armoury, updateTabsetPanel(session, "main_tabs", selected = "armoury"), ignoreInit = TRUE)
  observeEvent(input$shortcut_magic, updateTabsetPanel(session, "main_tabs", selected = "magic"), ignoreInit = TRUE)
  observeEvent(input$shortcut_combat, updateTabsetPanel(session, "main_tabs", selected = "debug_combat"), ignoreInit = TRUE)
  
  
  # Now wire tabs via calls (Demand.R pattern)
  source("calls/skills_calls.R", local = TRUE)
  source("calls/stats_calls.R", local=TRUE)
  source("calls/armoury_call.R", local=TRUE)
  source("calls/diary_call.R", local = TRUE)
  source("calls/camp_call.R", local =TRUE)
  source("calls/sidebar_calls.R", local = TRUE)
  source("calls/landing_call.R", local = TRUE)
  source("calls/dice_call.R", local=TRUE)
  source("calls/magic_call.R", local=TRUE)
  source("calls/blood_call.R", local=TRUE)
  source("calls/rest_call.R", local=TRUE)
  source("calls/level_call.R", local=TRUE)
  source("calls/inventory_call.R", local=TRUE)
  source("calls/debug_combat_call.R", local=TRUE)
  source("calls/party_hud_call.R", local=TRUE)
  privateNotesServer("notes", core$state)
  merchantServer("merchants",core$state)
  chestServer("chests",core$state)
  storyPlayerServer("story",core$state)
 source("calls/character_3d_call.R", local=TRUE)
  source("calls/rune_call.R", local=TRUE)
  
}
