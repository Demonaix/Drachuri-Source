library(shiny)

debugCombatUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    
    title = "Combat",
    
    value = "debug_combat",
    
    tagList(
      
      tags$link(rel = "stylesheet", type = "text/css", href = "css/combat.css"),
      
      tags$script(src = paste0("js/combat2d_simple.js?v=", as.integer(Sys.time()))),
      
      div(
        
        class = "combat-wrap",
        
        div(
          class = "combat-card",
          div(
            class = "combat-compact-header",
            uiOutput(ns("header_ui")),
            div(
            class = "combat-compact-actions",
              uiOutput(ns("class_actions_ui")),
              actionButton(ns("end_turn"), "End Turn", class = "btn btn-warning"),
              checkboxInput(ns("dash_move"), "Dash / Sprint", value = FALSE),
              uiOutput(ns("phase_move_ui")),
              div(
                class = "combat-turn-box",
                uiOutput(ns("turn_notice_ui"))
              )
            )
          )
        ),
        
        uiOutput(ns("combat_layout_ui"))
      )
    )
  )
}

debugCombatServer <- function(id, core, ctrl, add_log = NULL,
                              live_snapshot = NULL,
                              refresh_live_snapshot = NULL) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    source("server/combat_map_logic.R", local = FALSE)
    
    refresh_key <- reactiveVal(0)
    movement_paths <- reactiveVal(list())
    initiative_key <- reactiveVal(0L)
    bump_initiative <- function() initiative_key(isolate(initiative_key()) + 1L)
    
    positions_key <- reactiveVal(0)
    combat_key <- reactiveVal(0)
    events_key <- reactiveVal(0)
    
    map_ui_ready <- reactiveVal(FALSE)
    
    map_visual_key <- reactiveVal(0L)
    selected_target_id <- reactiveVal("")
    current_attack_is_opp <- reactiveVal(FALSE)
    
    bump_map_visual <- function() {
      map_visual_key(isolate(map_visual_key()) + 1L)
    }

    
    bump_positions <- function() positions_key(isolate(positions_key()) + 1L)
    bump_combat    <- function() combat_key(isolate(combat_key()) + 1L)
    bump_events    <- function() events_key(isolate(events_key()) + 1L)
 
    
    bump_refresh <- function() {
      refresh_key(isolate(refresh_key()) + 1L)
      bump_positions()
      bump_combat()
      bump_events()
      bump_initiative()
      bump_map_visual()
      if (is.function(refresh_live_snapshot)) refresh_live_snapshot()
    }

    snapshot_data <- reactive({
      if (!is.function(live_snapshot)) return(empty_player_live_snapshot())
      live_snapshot()
    })
    
    pending_attack <- reactiveVal(NULL)
    turn_move_ft <- reactiveVal(0L)
    pending_move <- reactiveVal(NULL)
    movement_dash <- reactiveVal(FALSE)
    movement_phase <- reactiveVal(FALSE)
    
    
   
    
    is_heart_eater <- reactive({
      char <- isolate(core$state$char)
      
      if (is.null(char)) return(FALSE)
      
      path_txt <- tryCatch(
        tolower(as.character(char$build$path %||% "")),
        error = function(e) ""
      )
      
      grepl("heart eater", path_txt, fixed = TRUE)
    })
    
    can_pass_through_tile <- function(tile, phase = FALSE) {
      if (isTRUE(phase) && isTRUE(is_heart_eater())) return(TRUE)
      !isTRUE(tile$blocks_movement)
    }
    
    can_land_on_tile <- function(tile) {
      !nzchar(as.character(tile$occupant_id %||% ""))
    }
    
    base_speed_ft <- function() {
      
      speed_ft <- suppressWarnings(as.integer(
        
        core$speed %||%
          
          core$state$speed %||%
          
          core$state$char$combat$speed_ft %||%
          
          core$state$char$combat_profile$speed_ft %||%
          
          30L
        
      ))
      
      if (length(speed_ft) < 1 || is.na(speed_ft) || speed_ft < 0L) {
        
        30L
        
      } else {
        
        speed_ft
        
      }
      
    }
    
    movement_allowance_ft <- function() {
      
      base <- base_speed_ft()
      
      if (isTRUE(movement_dash())) {
        
        base * 2L
        
      } else {
        
        base
        
      }
      
    }
    
    can_use_phase <- function() {
      !is.null(core$state$char) && isTRUE(is_heart_eater())
    }
    
    known_ac_rv <- reactiveVal(data.frame(
      target_id = character(),
      lower = integer(),
      upper = integer(),
      stringsAsFactors = FALSE
    ))
    
    map_id <- reactiveVal(1L)
    
    map_fullscreen <- reactiveVal(FALSE)
    map_true3d_mode <- reactiveVal(TRUE)
    
    map_tiles_rv <- reactive({
      mid <- map_id()
      if (is.null(mid) || is.na(mid) || mid < 1) {
        return(data.frame())
      }
      
      tiles <- tryCatch(
        get_map_tiles(mid),
        error = function(e) data.frame()
      )
      
      if (!is.data.frame(tiles)) data.frame() else tiles
    })
    
    add_3d_models_to_render_df <- function(render_df) {
      if (!is.data.frame(render_df) || nrow(render_df) == 0) return(render_df)
      
      model_cols <- c(
        "model_base",
        "model_hair",
        "model_body",
        "model_arms",
        "model_legs",
        "model_feet",
        "model_headgear",
        "model_accessory",
        "hair_color"
      )
      
      for (col in model_cols) {
        if (!col %in% names(render_df)) {
          render_df[[col]] <- rep("", nrow(render_df))
        }
      }
      
      safe_chr1 <- function(x, default = "") {
        if (is.null(x)) return(default)
        x <- unlist(x, use.names = FALSE)
        if (length(x) < 1) return(default)
        x <- as.character(x[1])
        if (is.na(x)) default else x
      }
      
      player_rows <- which(
        !is.na(render_df$occupant_id) &
          nzchar(as.character(render_df$occupant_id)) &
          as.character(render_df$occupant_type) == "player"
      )
      
      for (i in player_rows) {
        actor_id <- as.character(render_df$occupant_id[i])
        
        char_obj <- NULL
        
        if (identical(actor_id, as.character(core$state$char_id %||% ""))) {
          char_obj <- core$state$char
        } else {
          char_obj <- tryCatch(load_character_from_db(actor_id), error = function(e) NULL)
        }
        
        if (is.null(char_obj)) next
        
        char_obj <- validate_character(char_obj)
        char3d <- char_obj$character_3d %||% list()
        
        render_df$model_base[i]      <- safe_chr1(char3d$base_model)
        render_df$model_hair[i]      <- safe_chr1(char3d$hair_model)
        render_df$model_body[i]      <- safe_chr1(char3d$body_model)
        render_df$model_arms[i]      <- safe_chr1(char3d$arms_model)
        render_df$model_legs[i]      <- safe_chr1(char3d$legs_model)
        render_df$model_feet[i]      <- safe_chr1(char3d$feet_model)
        render_df$model_headgear[i]  <- safe_chr1(char3d$headgear_model)
        render_df$model_accessory[i] <- safe_chr1(char3d$accessory_model)
        render_df$hair_color[i]      <- safe_chr1(char3d$hair_color, "#3b2416")
      }
      
      render_df
    }
    
   
    
    map_occupants_rv <- reactive({
      pos <- positions_tbl()
      eid <- current_encounter_id()
      mid <- map_id()
      
      new_occ <- empty_map_occupants()
      
      if (is.na(eid) || !is.data.frame(pos) || nrow(pos) == 0) {
        return(new_occ)
      }
      
      for (i in seq_len(nrow(pos))) {
        actor_id <- as.character(pos$actor_id[i] %||% "")
        actor_type <- as.character(pos$actor_type[i] %||% "player")
        x <- suppressWarnings(as.integer(pos$x[i] %||% NA))
        y <- suppressWarnings(as.integer(pos$y[i] %||% NA))
        
        if (!nzchar(actor_id) || is.na(x) || is.na(y)) next
        
        new_occ <- set_actor_position_local(
          occupants = new_occ,
          actor_id = actor_id,
          x = x,
          y = y,
          map_id = mid,
          encounter_id = eid,
          actor_type = actor_type
        )
      }
      
      new_occ
    })
    

    
    log_safe <- function(msg) {
      if (is.function(add_log)) {
        try(add_log(msg, toast = TRUE), silent = TRUE)
      } else {
        message(msg)
      }
    }
    
    is_players_turn <- reactive({
      cid <- as.character(core$state$char_id %||% "")
      if (!nzchar(cid)) return(FALSE)
      
      combat <- combat_tbl()
      if (!is.data.frame(combat) || nrow(combat) == 0) return(FALSE)
      
      active_id <- as.character(combat$active_actor_id[1] %||% "")
      active_type <- as.character(combat$active_actor_type[1] %||% "")
      
      identical(active_type, "player") && identical(active_id, cid)
    })
    
    output$combat_3d_static <- renderUI({
      
      if (!isTRUE(map_true3d_mode())) return(NULL)
      
      div(
        class = "combat-section-title",
        "3D Map",
        
        tags$div(
          id = session$ns("combat_3d_canvas"),
          style = "
        width:100%;
        height:620px;
        border-radius:14px;
        overflow:hidden;
        background:#111;
      "
        )
      )
    })
    
    observeEvent(input$dash_move, {
      movement_dash(isTRUE(input$dash_move))
    }, ignoreInit = FALSE)
    
    output$phase_move_ui <- renderUI({
      if (!isTRUE(can_use_phase())) return(NULL)
      
      checkboxInput(
        session$ns("phase_move"),
        "Phase through objects",
        value = FALSE
      )
    })
    
    observeEvent(input$phase_move, {
      movement_phase(isTRUE(input$phase_move) && isTRUE(can_use_phase()))
    }, ignoreInit = FALSE)
    
  
    
    output$turn_notice_ui <- renderUI({
      cid <- as.character(core$state$char_id %||% "")
      if (!nzchar(cid)) return(NULL)
      
      if (isTRUE(is_players_turn())) {
        div(
          class = "combat-pill",
          style = "background: rgba(225,245,225,0.96); border-color: rgba(90,150,90,0.75);",
          "✅ It is your turn"
        )
      } else {
        div(
          class = "combat-pill",
          style = "background: rgba(255,245,230,0.96); border-color: rgba(191,167,111,0.75);",
          "⏳ Not your turn — only opportunity attacks allowed"
        )
      }
    })
    
    # --------------------------------------------------
    # Current encounter
    # --------------------------------------------------
    current_encounter_id <- reactive({
      snapshot <- snapshot_data()
      session_row <- snapshot$session %||% data.frame()
      eid <- NA_integer_

      if (is.data.frame(session_row) && nrow(session_row) > 0 &&
          "active_encounter_id" %in% names(session_row)) {
        eid <- suppressWarnings(as.integer(session_row$active_encounter_id[1] %||% NA))
      }

      if (is.na(eid)) {
        eid <- suppressWarnings(as.integer(core$state$active_encounter_id %||% NA))
      }
      if (is.na(eid) || eid < 1) return(NA_integer_)
      eid
    })
    
    

    


    observeEvent(input$map_fullscreen_exit, {
      map_fullscreen(FALSE)
    }, ignoreInit = TRUE)
    
    encounter_tbl <- reactive({
      refresh_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())

      cached <- snapshot_data()$encounter %||% data.frame()
      if (is.data.frame(cached) && nrow(cached) > 0 &&
          identical(as.integer(cached$id[1] %||% NA), as.integer(eid))) {
        return(cached)
      }
      
      df <- tryCatch(get_encounter(eid), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    
    current_session_id <- reactive({
      enc <- encounter_tbl()
      if (!is.data.frame(enc) || nrow(enc) == 0) return(NA_integer_)
      
      sid <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
      if (is.na(sid) || sid < 1) return(NA_integer_)
      sid
    })
    
    players_tbl <- reactive({
      refresh_key()
      sid <- current_session_id()
      if (is.na(sid)) return(data.frame())
      
      df <- snapshot_data()$players %||% data.frame()
      if (!is.data.frame(df)) data.frame() else df
    })
    
    positions_tbl <- reactive({
      positions_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- snapshot_data()$positions %||% data.frame()
      if (!is.data.frame(df)) data.frame() else df
    })
    
    combat_tbl <- reactive({
      combat_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- snapshot_data()$combat %||% data.frame()
      if (!is.data.frame(df)) data.frame() else df
    })
    
    events_tbl <- reactive({
      events_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- snapshot_data()$events %||% data.frame()
      if (!is.data.frame(df)) data.frame() else df
    })
    
    encounter_actors_tbl <- reactive({
      initiative_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- tryCatch(
        build_snapshot_encounter_actors(snapshot_data()),
        error = function(e) data.frame()
      )
      if (!is.data.frame(df)) data.frame() else df
    })
    
    # --------------------------------------------------
    # Map hydration
    # --------------------------------------------------
    observeEvent(encounter_tbl(), {
      enc <- encounter_tbl()
      if (!is.data.frame(enc) || nrow(enc) == 0) return()
      
      mid <- suppressWarnings(as.integer(enc$map_id[1] %||% 1L))
      if (is.na(mid) || mid < 1) mid <- 1L
      map_id(mid)
    }, ignoreInit = FALSE)
    
    

    
    observeEvent(input$apply_admin, {
      eid <- current_encounter_id()
      combat <- combat_tbl()
      
      if (is.na(eid) || nrow(combat) == 0) {
        log_safe("No active combat.")
        return()
      }
      
      actor_id <- as.character(combat$active_actor_id[1] %||% "")
      actor_type <- as.character(combat$active_actor_type[1] %||% "")
      
      amt <- suppressWarnings(as.integer(input$admin_amount %||% 0))
      if (is.na(amt) || amt <= 0) {
        log_safe("Invalid amount.")
        return()
      }
      
      mode <- as.character(input$admin_type %||% "damage")
      
      result <- NULL
      
      if (mode == "damage") {
        result <- if (actor_type == "player") {
          damage_player_in_encounter(eid, actor_id, amt)
        } else {
          tryCatch(
            damage_encounter_enemy(eid, actor_id, amt),
            error = function(e) NULL
          )
        }
      } else if (mode == "heal") {
        result <- if (actor_type == "player") {
          heal_session_player(
            session_id = current_session_id(),
            character_id = actor_id,
            amount = amt
          )
        } else {
          tryCatch(
            heal_encounter_enemy(eid, actor_id, amt),
            error = function(e) NULL
          )
        }
      }
      
      if (is.null(result)) {
        log_safe("Action failed.")
        return()
      }
      
      # Log it
      try(
        log_game_event(
          encounter_id = eid,
          event_type = ifelse(mode == "heal", "heal", "damage"),
          actor_type = "control",
          target_id = actor_id,
          payload = list(
            amount = amt,
            hp_before = result$hp_before,
            hp_after = result$hp_after
          )
        ),
        silent = TRUE
      )
      
      log_safe(paste0(
        ifelse(mode == "heal", "Healed ", "Damaged "),
        get_actor_display_name(actor_id),
        " for ", amt
      ))
      
      bump_positions()
      bump_events()
      bump_initiative()
      bump_map_visual()
    })
    
    

    
    # --------------------------------------------------
    # Actor helpers
    # --------------------------------------------------
    get_actor_row <- function(actor_id, actor_type = NULL) {
      actor_id <- as.character(actor_id %||% "")
      if (!nzchar(actor_id)) return(data.frame())
      
      actors <- encounter_actors_tbl()
      if (!is.data.frame(actors) || nrow(actors) == 0) return(data.frame())
      
      out <- actors[as.character(actors$actor_id) == actor_id, , drop = FALSE]
      
      if (!is.null(actor_type)) {
        out <- out[as.character(out$actor_type) == as.character(actor_type), , drop = FALSE]
      }
      
      out
    }
    
    get_actor_display_name <- function(actor_id, actor_type = NULL) {
      row <- get_actor_row(actor_id, actor_type)
      if (!is.data.frame(row) || nrow(row) == 0) return("Unknown")
      as.character(row$display_name[1] %||% row$name[1] %||% "Unknown")
    }
    
    get_actor_type_by_id <- function(actor_id) {
      row <- get_actor_row(actor_id)
      if (!is.data.frame(row) || nrow(row) == 0) return(NA_character_)
      as.character(row$actor_type[1] %||% NA_character_)
    }
    
    active_actor_id <- reactive({
      combat <- combat_tbl()
      if (!is.data.frame(combat) || nrow(combat) == 0) return(NULL)
      
      aid <- as.character(combat$active_actor_id[1] %||% "")
      if (!nzchar(aid)) return(NULL)
      aid
    })
    
    active_actor_row <- reactive({
      aid <- active_actor_id()
      if (is.null(aid)) return(data.frame())
      get_actor_row(aid)
    })
    
    active_position_row <- reactive({
      aid <- active_actor_id()
      pos <- positions_tbl()
      
      if (is.null(aid) || !is.data.frame(pos) || nrow(pos) == 0) {
        return(data.frame())
      }
      
      pos[as.character(pos$actor_id) == as.character(aid), , drop = FALSE]
    })
    
    load_actor_for_combat <- function(actor_id, actor_type = NULL) {
      actor_id <- as.character(actor_id %||% "")
      actor_type <- as.character(actor_type %||% get_actor_type_by_id(actor_id) %||% "")
      
      if (!nzchar(actor_id) || !nzchar(actor_type)) return(NULL)
      
      if (identical(actor_type, "player")) {
        db_char <- tryCatch(load_character_from_db(actor_id), error = function(e) NULL)
        
        if (!is.null(db_char)) {
          return(db_char)
        }
        
        if (identical(as.character(core$state$char_id %||% ""), actor_id)) {
          return(core$state$char)
        }
        
        return(NULL)
      }
      
      if (identical(actor_type, "enemy")) {
        enemies <- tryCatch(get_encounter_enemies(current_encounter_id()), error = function(e) data.frame())
        if (!is.data.frame(enemies) || nrow(enemies) == 0) return(NULL)
        
        row <- enemies[as.character(enemies$enemy_uuid) == actor_id, , drop = FALSE]
        if (nrow(row) == 0) return(NULL)
        
        return(list(
          meta = list(name = as.character(row$name[1] %||% "Enemy"), race = "Enemy"),
          build = list(class = "Enemy", level = 1),
          abilities = list(str = 10, dex = 10, con = 10, int = 10, cha = 10, bld_str = 10),
          resources = list(
            hp = list(
              max = as.integer(row$hp_max[1] %||% 1),
              cur = as.integer(row$hp_current[1] %||% 0),
              temp = as.integer(row$temp_hp[1] %||% 0)
            ),
            sindre = list(cur = 0, total = 0, temp = 0)
          ),
          status = list(
            effects = character(0),
            exhaustion = 0,
            bloodlust = FALSE
          ),
          combat_profile = list(
            resistances = character(0),
            immunities = character(0),
            vulnerabilities = character(0),
            ac_override = as.integer(row$ac[1] %||% 10),
            speed_ft = as.integer(row$movement_speed[1] %||% row$speed_ft[1] %||% 30L),
            initiative_mod = as.integer(row$initiative_mod[1] %||% 0L),
            attack_bonus = as.integer(row$attack_bonus[1] %||% 2L),
            damage_expr = as.character(row$damage_expr[1] %||% "1d6"),
            damage_type = as.character(row$damage_type[1] %||% "slashing")
          )
        ))
      }
      
      NULL
    }
    
 
    get_effective_actor_ac <- function(actor_id, actor_type = NULL, actor_obj = NULL) {
      actor_type <- as.character(actor_type %||% get_actor_type_by_id(actor_id) %||% "")
      
      if (identical(actor_type, "enemy")) {
        enemies <- tryCatch(get_encounter_enemies(current_encounter_id()), error = function(e) data.frame())
        if (is.data.frame(enemies) && nrow(enemies) > 0) {
          row <- enemies[as.character(enemies$enemy_uuid) == as.character(actor_id), , drop = FALSE]
          if (nrow(row) > 0) {
            ac <- suppressWarnings(as.integer(row$ac[1] %||% NA))
            if (!is.na(ac)) return(ac)
          }
        }
      }
      
      if (!is.null(actor_obj)) {
        ac <- tryCatch(calc_auto_ac_for_char(actor_obj), error = function(e) NA_integer_)
        if (!is.na(ac)) return(ac)
      }
      
      10L
    }
    
    # --------------------------------------------------
    # HP / AC helpers
    # --------------------------------------------------
    damage_player_in_encounter <- function(encounter_id, character_id, amount) {
      enc <- tryCatch(get_encounter(encounter_id), error = function(e) data.frame())
      if (!is.data.frame(enc) || nrow(enc) == 0) return(NULL)
      
      session_id <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
      if (is.na(session_id) || session_id < 1) return(NULL)
      
      damage_session_player(
        session_id = session_id,
        character_id = character_id,
        amount = amount
      )
    }
    
    update_known_ac <- function(target_id, attack_total, is_hit) {
      df <- known_ac_rv()
      target_id <- as.character(target_id %||% "")
      if (!nzchar(target_id)) return(invisible(FALSE))
      
      idx <- which(df$target_id == target_id)
      
      if (length(idx) == 0) {
        df <- rbind(df, data.frame(
          target_id = target_id,
          lower = 1L,
          upper = 99L,
          stringsAsFactors = FALSE
        ))
        idx <- nrow(df)
      }
      
      if (isTRUE(is_hit)) {
        df$upper[idx] <- min(as.integer(df$upper[idx]), as.integer(attack_total))
      } else {
        df$lower[idx] <- max(as.integer(df$lower[idx]), as.integer(attack_total) + 1L)
      }
      
      known_ac_rv(df)
      invisible(TRUE)
    }
    
    format_known_ac <- function(target_id) {
      df <- known_ac_rv()
      row <- df[df$target_id == as.character(target_id), , drop = FALSE]
      if (nrow(row) == 0) return("AC ?")
      
      lo <- as.integer(row$lower[1] %||% 1L)
      hi <- as.integer(row$upper[1] %||% 99L)
      
      if (lo <= 1L && hi >= 99L) return("AC ?")
      if (lo > 1L && hi < 99L && lo <= hi) return(paste0("AC ", lo, "–", hi))
      if (lo > 1L && hi >= 99L) return(paste0("AC ≥ ", lo))
      if (lo <= 1L && hi < 99L) return(paste0("AC ≤ ", hi))
      "AC ?"
    }
    
    hp_bar_ui <- function(cur, temp = 0, maxv = NULL) {
      cur  <- suppressWarnings(as.integer(cur %||% 0))
      temp <- suppressWarnings(as.integer(temp %||% 0))
      maxv <- suppressWarnings(as.integer(maxv %||% max(cur, 1)))
      
      if (is.na(cur)) cur <- 0L
      if (is.na(temp)) temp <- 0L
      if (is.na(maxv) || maxv <= 0) maxv <- max(cur, 1L)
      
      cur <- max(0L, cur)
      temp <- max(0L, temp)
      
      base_pct <- max(0, min(100, round((cur / maxv) * 100)))
      temp_pct <- max(0, min(100, round(((cur + temp) / maxv) * 100)))
      
      tags$div(
        class = "mini-bar-wrap",
        tags$div(
          class = "mini-bar-label",
          sprintf(
            "HP %s/%s%s",
            cur,
            maxv,
            if (temp > 0) paste0(" +", temp, " temp") else ""
          )
        ),
        tags$div(
          class = "mini-bar",
          tags$div(
            class = "mini-fill hp",
            style = paste0("width:", base_pct, "%;")
          ),
          if (temp > 0) {
            tags$div(
              class = "mini-fill",
              style = paste0(
                "width:", temp_pct, "%;",
                "background: linear-gradient(90deg, rgba(220,235,255,0.42), rgba(145,205,255,0.82));"
              )
            )
          }
        )
      )
    }
    
    # --------------------------------------------------
    # Damage trait helpers
    # --------------------------------------------------
    normalize_damage_type <- function(x) {
      x <- tolower(trimws(as.character(x %||% "")))
      x[nzchar(x)]
    }
    
    get_character_damage_traits <- function(char) {
      char <- validate_character(char)
      
      prof1 <- char$combat_profile %||% list()
      prof2 <- char$combat %||% list()
      prof3 <- char$meta$combat_profile %||% list()
      
      get_vec <- function(name) {
        out <- c(
          prof1[[name]] %||% character(0),
          prof2[[name]] %||% character(0),
          prof3[[name]] %||% character(0)
        )
        unique(normalize_damage_type(out))
      }
      
      list(
        resistances = get_vec("resistances"),
        immunities = get_vec("immunities"),
        vulnerabilities = get_vec("vulnerabilities")
      )
    }
    
    get_class_level <- function(char, class_name) {
      char <- validate_character(char)
      class_name <- tolower(trimws(as.character(class_name %||% "")))
      if (!nzchar(class_name)) return(0L)
      
      clv <- char$build$class_levels %||% NULL
      if (is.list(clv) || is.vector(clv)) {
        nms <- names(clv) %||% character(0)
        idx <- which(tolower(nms) == class_name)
        if (length(idx) >= 1) {
          val <- suppressWarnings(as.integer(clv[[idx[1]]]))
          if (!is.na(val)) return(max(0L, val))
        }
      }
      
      cls <- char$build$classes %||% NULL
      if (is.list(cls)) {
        nms <- names(cls) %||% character(0)
        idx <- which(tolower(nms) == class_name)
        if (length(idx) >= 1) {
          ent <- cls[[idx[1]]]
          val <- suppressWarnings(as.integer(ent$level %||% ent$levels %||% ent))
          if (!is.na(val)) return(max(0L, val))
        }
      }
      
      mc <- char$build$multiclass %||% NULL
      if (is.list(mc)) {
        nms <- names(mc) %||% character(0)
        idx <- which(tolower(nms) == class_name)
        if (length(idx) >= 1) {
          ent <- mc[[idx[1]]]
          val <- suppressWarnings(as.integer(ent$level %||% ent$levels %||% ent))
          if (!is.na(val)) return(max(0L, val))
        }
      }
      
      main_class <- tolower(as.character(char$build$class %||% ""))
      lvl <- suppressWarnings(as.integer(char$build$level %||% 1))
      if (!is.na(lvl) && nzchar(main_class) && identical(main_class, class_name)) {
        return(max(0L, lvl))
      }
      
      0L
    }
    
    get_sneak_attack_expr <- function(char) {
      rogue_level <- get_class_level(char, "rogue")
      if (rogue_level < 1) return("")
      
      dice_n <- floor((rogue_level + 1) / 2)
      if (dice_n < 1) return("")
      paste0(dice_n, "d6")
    }
    
    apply_damage_traits_to_parts <- function(parts, traits) {
      if (length(parts) == 0) return(list(parts = list(), total = 0L))
      
      res <- lapply(parts, function(part) {
        typ <- normalize_damage_type(part$type %||% "")
        raw <- as.integer(part$total %||% 0)
        adj <- raw
        rule <- "normal"
        
        if (length(typ) > 0 && typ %in% normalize_damage_type(traits$immunities)) {
          adj <- 0L
          rule <- "immune"
        } else if (length(typ) > 0 && typ %in% normalize_damage_type(traits$resistances)) {
          adj <- floor(raw / 2)
          rule <- "resistant"
        } else if (length(typ) > 0 && typ %in% normalize_damage_type(traits$vulnerabilities)) {
          adj <- raw * 2L
          rule <- "vulnerable"
        }
        
        c(part, list(adjusted_total = as.integer(adj), rule = rule))
      })
      
      total <- sum(vapply(res, function(x) as.integer(x$adjusted_total %||% 0L), integer(1)))
      list(parts = res, total = as.integer(total))
    }
    
    build_attack_preview <- function(attacker_char, target_char, weapon_row, attacker_name, target_name,
                                     attacker_id, target_id, attacker_type = "player", target_type = NULL,
                                     adv_override = NULL) {
      attacker_char <- validate_character(attacker_char)
      target_type <- as.character(target_type %||% get_actor_type_by_id(target_id) %||% "player")
      
      adv <- as.character(adv_override %||% weapon_row$adv[1] %||% "Normal")
      adv_norm <- tolower(as.character(adv %||% "normal"))
      
      if (adv_norm %in% c("advantage", "adv")) {
        attack_rolls <- sample.int(20, 2)
        attack_roll <- max(attack_rolls)
      } else if (adv_norm %in% c("disadvantage", "dis")) {
        attack_rolls <- sample.int(20, 2)
        attack_roll <- min(attack_rolls)
      } else {
        attack_rolls <- sample.int(20, 1)
        attack_roll <- attack_rolls[1]
      }
      

      
      attack_bonus <- get_weapon_hit_bonus(attacker_char, weapon_row)
      attack_total <- as.integer(attack_roll + attack_bonus)
      target_ac <- get_effective_actor_ac(target_id, target_type, target_char)
      
      is_crit <- identical(attack_roll, 20L)
      is_hit <- is_crit || (attack_total >= target_ac)
      
      damage_parts <- list()
      
      if (isTRUE(is_hit)) {
        dmg1_expr <- as.character(weapon_row$damage1[1] %||% "")
        if (nzchar(dmg1_expr)) {
          d1 <- roll_dice_expr(dmg1_expr)
          damage_parts <- c(damage_parts, list(list(
            source = "Weapon",
            expr = dmg1_expr,
            type = as.character(weapon_row$dmg_type1[1] %||% ""),
            rolls = d1$rolls,
            total = as.integer(d1$total)
          )))
        }
        
        dmg2_expr <- as.character(weapon_row$damage2[1] %||% "")
        if (nzchar(dmg2_expr)) {
          d2 <- roll_dice_expr(dmg2_expr)
          damage_parts <- c(damage_parts, list(list(
            source = "Weapon Extra",
            expr = dmg2_expr,
            type = as.character(weapon_row$dmg_type2[1] %||% ""),
            rolls = d2$rolls,
            total = as.integer(d2$total)
          )))
        }
      }
      
      sneak_expr <- get_sneak_attack_expr(attacker_char)
      sneak_part <- NULL
      if (nzchar(sneak_expr) && isTRUE(is_hit)) {
        sa <- roll_dice_expr(sneak_expr)
        sneak_part <- list(
          source = "Sneak Attack",
          expr = sneak_expr,
          type = as.character(weapon_row$dmg_type1[1] %||% "piercing"),
          rolls = sa$rolls,
          total = as.integer(sa$total)
        )
      }
      
      list(
        attacker_id = as.character(attacker_id),
        attacker_type = as.character(attacker_type %||% "player"),
        target_id = as.character(target_id),
        target_type = as.character(target_type),
        attacker_name = as.character(attacker_name),
        target_name = as.character(target_name),
        weapon_id = as.character(weapon_row$id[1] %||% ""),
        weapon_name = as.character(weapon_row$name[1] %||% "Weapon"),
        attack_roll = as.integer(attack_roll),
        attack_rolls = as.integer(attack_rolls),
        attack_adv_mode = adv,
        attack_bonus = as.integer(attack_bonus),
        attack_total = as.integer(attack_total),
        target_ac = as.integer(target_ac),
        is_hit = isTRUE(is_hit),
        is_crit = isTRUE(is_crit),
        base_parts = damage_parts,
        sneak_available = !is.null(sneak_part),
        sneak_part = sneak_part,
        target_traits = get_character_damage_traits(target_char),
        primary_damage_type = as.character(weapon_row$dmg_type1[1] %||% "")
      )
    }
    
    compute_final_attack <- function(preview, apply_sneak = FALSE, manual_bonus = 0L, manual_type = "") {
      if (is.null(preview)) {
        return(list(parts = list(), adjusted = list(parts = list(), total = 0L), raw_total = 0L))
      }
      
      parts <- preview$base_parts %||% list()
      
      if (isTRUE(apply_sneak) && isTRUE(preview$sneak_available) && !is.null(preview$sneak_part)) {
        parts <- c(parts, list(preview$sneak_part))
      }
      
      manual_bonus <- suppressWarnings(as.integer(manual_bonus %||% 0))
      if (is.na(manual_bonus) || manual_bonus < 0) manual_bonus <- 0L
      
      if (manual_bonus > 0L) {
        use_type <- as.character(manual_type %||% "")
        if (!nzchar(use_type) || identical(use_type, "same_as_primary")) {
          use_type <- preview$primary_damage_type %||% ""
        }
        if (identical(use_type, "untyped")) {
          use_type <- ""
        }
        
        parts <- c(parts, list(list(
          source = "Manual Bonus",
          expr = paste0(manual_bonus),
          type = use_type,
          rolls = integer(0),
          total = as.integer(manual_bonus)
        )))
      }
      
      raw_total <- sum(vapply(parts, function(x) as.integer(x$total %||% 0L), integer(1)))
      adjusted <- apply_damage_traits_to_parts(parts, preview$target_traits)
      
      list(
        parts = parts,
        adjusted = adjusted,
        raw_total = as.integer(raw_total)
      )
    }
    
    # --------------------------------------------------
    # Attack confirm modal
    # --------------------------------------------------
    output$attack_confirm_ui <- renderUI({
      preview <- pending_attack()
      if (is.null(preview)) return(NULL)
      
      apply_sneak <- isTRUE(input$final_apply_sneak %||% FALSE)
      manual_bonus <- suppressWarnings(as.integer(input$final_bonus_damage %||% 0))
      manual_type <- as.character(input$final_bonus_type %||% "same_as_primary")
      
      calc <- compute_final_attack(
        preview = preview,
        apply_sneak = apply_sneak,
        manual_bonus = manual_bonus,
        manual_type = manual_type
      )
      
      traits <- preview$target_traits %||% list(
        resistances = character(0),
        immunities = character(0),
        vulnerabilities = character(0)
      )
      
      part_ui <- if (length(calc$adjusted$parts) == 0) {
        div(class = "damage-part", span("No damage parts"), span("0"))
      } else {
        lapply(calc$adjusted$parts, function(part) {
          raw <- as.integer(part$total %||% 0L)
          adj <- as.integer(part$adjusted_total %||% 0L)
          rule <- as.character(part$rule %||% "normal")
          typ <- as.character(part$type %||% "")
          lbl <- paste0(
            part$source %||% "Damage",
            if (nzchar(typ)) paste0(" (", typ, ")") else ""
          )
          rhs <- if (identical(rule, "normal")) {
            as.character(adj)
          } else {
            paste0(raw, " → ", adj, " [", rule, "]")
          }
          
          div(class = "damage-part", span(lbl), span(rhs))
        })
      }
      
      tagList(
        div(
          class = "confirm-box",
          div(class = "combat-section-title", "Attack Preview"),
          div(
            class = "confirm-kv",
            div(class = "confirm-k", "Attacker"), div(preview$attacker_name),
            div(class = "confirm-k", "Target"), div(preview$target_name),
            div(class = "confirm-k", "Weapon"), div(preview$weapon_name),
            div(class = "confirm-k", "To Hit"), div(paste0(
              paste(preview$attack_rolls, collapse = "/"),
              if (!identical(preview$attack_adv_mode %||% "Normal", "Normal")) {
                paste0(" [", preview$attack_adv_mode %||% "Normal", "]")
              } else {
                ""
              },
              " → ", preview$attack_roll,
              " + ", preview$attack_bonus,
              " = ", preview$attack_total,
              " vs AC ", preview$target_ac
            )),
            div(class = "confirm-k", "Result"), div(
              if (isTRUE(preview$is_hit)) {
                if (isTRUE(preview$is_crit)) "Critical Hit" else "Hit"
              } else {
                "Miss"
              }
            )
          )
        ),
        
        if (isTRUE(preview$is_hit)) {
          tagList(
            div(
              class = "confirm-box",
              div(class = "combat-section-title", "Modify Damage"),
              if (isTRUE(preview$sneak_available)) {
                checkboxInput(
                  session$ns("final_apply_sneak"),
                  paste0("Apply Sneak Attack (", preview$sneak_part$expr %||% "", ")"),
                  value = FALSE
                )
              },
              fluidRow(
                column(
                  6,
                  numericInput(
                    session$ns("final_bonus_damage"),
                    "Manual bonus damage",
                    value = 0,
                    min = 0,
                    step = 1
                  )
                ),
                column(
                  6,
                  selectInput(
                    session$ns("final_bonus_type"),
                    "Bonus damage type",
                    choices = c(
                      "Same as primary" = "same_as_primary",
                      "Untyped" = "untyped",
                      unique(c(
                        preview$primary_damage_type %||% "",
                        normalize_damage_type(traits$resistances),
                        normalize_damage_type(traits$immunities),
                        normalize_damage_type(traits$vulnerabilities)
                      ))
                    ),
                    selected = "same_as_primary"
                  )
                )
              )
            ),
            
            div(
              class = "confirm-box",
              div(class = "combat-section-title", "Target Defences"),
              div(
                class = "confirm-kv",
                div(class = "confirm-k", "Resistances"),
                div(if (length(traits$resistances)) paste(traits$resistances, collapse = ", ") else "—"),
                div(class = "confirm-k", "Immunities"),
                div(if (length(traits$immunities)) paste(traits$immunities, collapse = ", ") else "—"),
                div(class = "confirm-k", "Vulnerabilities"),
                div(if (length(traits$vulnerabilities)) paste(traits$vulnerabilities, collapse = ", ") else "—")
              )
            ),
            
            div(
              class = "confirm-box",
              div(class = "combat-section-title", "Damage Breakdown"),
              part_ui,
              tags$hr(),
              div(class = "damage-part", span(tags$strong("Raw total")), span(tags$strong(calc$raw_total))),
              div(class = "damage-part", span(tags$strong("Adjusted total")), span(tags$strong(calc$adjusted$total))),
              div(class = "confirm-note", "You can still override the final number before applying.")
            )
          )
        } else {
          div(
            class = "confirm-box",
            div(class = "combat-section-title", "Damage"),
            div("Missed attacks default to 0 damage, but you can override if needed.")
          )
        }
      )
    })
    
    # --------------------------------------------------
    # Event formatting
    # --------------------------------------------------
    
    safe_event_payload <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.list(x)) return(x)
      
      if (is.character(x) && length(x) == 1 && nzchar(x)) {
        return(tryCatch(
          jsonlite::fromJSON(x, simplifyVector = FALSE),
          error = function(e) NULL
        ))
      }
      
      NULL
    }
    

    
    # --------------------------------------------------
    # Header
    # --------------------------------------------------
    output$header_ui <- renderUI({
      enc <- encounter_tbl()
      combat <- combat_tbl()
      active <- active_actor_row()
      
      enc_name <- if (is.data.frame(enc) && nrow(enc) > 0) {
        as.character(enc$name[1] %||% paste("encounter", current_encounter_id()))
      } else {
        paste("encounter", current_encounter_id())
      }
      
      round_txt <- if (is.data.frame(combat) && nrow(combat) > 0) {
        as.character(combat$round_number[1] %||% "—")
      } else {
        "—"
      }
      
      active_name <- if (is.data.frame(active) && nrow(active) > 0) {
        as.character(active$display_name[1] %||% "Unknown")
      } else {
        "No active actor"
      }
      
      div(
        class = "combat-compact-summary",
        tags$strong("⚔️ ", enc_name),
        tags$span(paste("Round", round_txt)),
        tags$span(paste("Turn:", active_name)),
        tags$span(paste("Moved:", turn_move_ft(), "ft"))
      )
    })
    
    actor_short_label <- function(actor_id, actor_type, occ_name) {
      actors <- encounter_actors_tbl()
      if (!is.data.frame(actors) || nrow(actors) == 0) {
        return(substr(occ_name, 1, 2))
      }
      
      row <- actors[
        as.character(actors$actor_id) == as.character(actor_id),
        ,
        drop = FALSE
      ]
      
      ord <- suppressWarnings(as.integer(row$turn_order[1] %||% NA))
      prefix <- if (identical(actor_type, "player")) "P" else "E"
      
      if (!is.na(ord)) return(paste0(prefix, ord))
      paste0(prefix, substr(occ_name, 1, 1))
    }
    
    # --------------------------------------------------
    # Map UI
    # --------------------------------------------------
    output$map_ui <- renderUI({
      map_ui_ready(TRUE)
      
      tags$div(
        id = session$ns("combat_3d_shell"),
        class = "combat-2d-shell",
        
        tags$div(
          class = "combat-map-toolbar",
          div(class = "combat-section-title", "2D Combat Map"),
          actionButton(
            session$ns("map_3d_fullscreen"),
            "Fullscreen Map",
            class = "btn btn-default"
          )
        ),
        
        tags$div(
          id = session$ns("combat_3d_canvas"),
          class = "combat-2d-canvas"
        )
      )
    })
    
    
    observe({
      
      req(map_true3d_mode())
      req(map_ui_ready())
      map_visual_key()
      tiles_now <- map_tiles_rv()
      
      if (!is.data.frame(tiles_now) || nrow(tiles_now) == 0) {
        return()
      }
      
      occ <- tryCatch(
        map_occupants_rv(),
        error = function(e) empty_map_occupants()
      )
      
      render_df <- tryCatch(
        build_map_render_df(
          tiles = tiles_now,
          occupants = occ,
          map_id = map_id()
        ),
        error = function(e) {
          log_safe(paste("⚠️ Map render failed:", conditionMessage(e)))
          data.frame()
        }
      )
      
      
     # render_df <- add_3d_models_to_render_df(render_df)
      
      actors_lookup <- encounter_actors_tbl()
      
      if (!"occupant_name" %in% names(render_df)) {
        render_df$occupant_name <- ""
      }
      
      if (is.data.frame(actors_lookup) && nrow(actors_lookup) > 0) {
        for (i in seq_len(nrow(render_df))) {
          oid <- as.character(render_df$occupant_id[i] %||% "")
          if (!nzchar(oid)) next
          
          row <- actors_lookup[
            as.character(actors_lookup$actor_id) == oid,
            ,
            drop = FALSE
          ]
          
          if (nrow(row) > 0) {
            render_df$occupant_name[i] <- as.character(
              row$display_name[1] %||% row$name[1] %||% oid
            )
          }
        }
      }
      
      render_df$is_reachable <- FALSE
      render_df$is_pending_move <- FALSE
      render_df$is_active_actor <- FALSE
      
      combat <- tryCatch(combat_tbl(), error = function(e) data.frame())
      
      if (is.data.frame(combat) && nrow(combat) > 0) {
        
        active_id <- active_actor_id()
        
     
        
        render_df$is_reachable <- FALSE
        
        pm <- pending_move()
        
        if (!is.null(pm)) {
          render_df$is_pending_move <-
            render_df$x == pm$x &
            render_df$y == pm$y
        }
        
        render_df$is_active_actor <-
          !is.na(render_df$occupant_id) &
          as.character(render_df$occupant_id) == as.character(active_id %||% "")
      }
      
    
      
      later::later(function() {
        session$sendCustomMessage(
          "combat3d-init",
          list(
            containerId = session$ns("combat_3d_canvas"),
            mapData = jsonlite::toJSON(
              render_df,
              dataframe = "rows",
              auto_unbox = TRUE,
              null = "null"
            ),
            inputIds = list(
              move = session$ns("move_to_tile"),
              target = session$ns("map_target_click")
            )
          )
        )
      }, delay = 0.1)
    })
    
  
    
    empty_reachable <- function() {
      data.frame(
        x = integer(),
        y = integer(),
        move_cost_ft = integer(),
        stringsAsFactors = FALSE
      )
    }
    
    movement_range_tiles <- reactive({
      
      if (!isTRUE(is_players_turn())) {
        return(empty_reachable())
      }
      
      speed_ft <- suppressWarnings(as.integer(movement_allowance_ft()))
      if (length(speed_ft) < 1 || is.na(speed_ft) || speed_ft < 0L) {
        speed_ft <- 30L
      }
      
      used_ft <- suppressWarnings(as.integer(turn_move_ft() %||% 0L))
      if (length(used_ft) < 1 || is.na(used_ft) || used_ft < 0L) {
        used_ft <- 0L
      }
      
      remaining_ft <- max(0L, speed_ft - used_ft)
      
      if (remaining_ft < 5L) {
        return(empty_reachable())
      }
      
      pos <- active_position_row()
      
      if (!is.data.frame(pos) || nrow(pos) < 1) {
        return(empty_reachable())
      }
      
      start_x <- suppressWarnings(as.integer(pos$x[1] %||% NA))
      start_y <- suppressWarnings(as.integer(pos$y[1] %||% NA))
      
      if (length(start_x) < 1 || length(start_y) < 1 || is.na(start_x) || is.na(start_y)) {
        return(empty_reachable())
      }
      
      combat <- combat_tbl()
      
      if (!is.data.frame(combat) || nrow(combat) < 1) {
        return(empty_reachable())
      }
      
      actor_id <- as.character(combat$active_actor_id[1] %||% "")
      actor_type <- as.character(combat$active_actor_type[1] %||% "player")
      
      if (!nzchar(actor_id)) {
        return(empty_reachable())
      }
      
      occ <- map_occupants_rv()
      tiles <- map_tiles_rv()
      
      if (!is.data.frame(tiles) || nrow(tiles) < 1) {
        return(empty_reachable())
      }
      
      can_phase <- identical(actor_type, "player") &&
        isTRUE(input$phase_move) &&
        isTRUE(is_heart_eater())
      
      dirs <- expand.grid(dx = -1:1, dy = -1:1)
      dirs <- dirs[!(dirs$dx == 0 & dirs$dy == 0), , drop = FALSE]
      
      start_key <- paste(start_x, start_y, sep = ",")
      
      frontier <- data.frame(
        x = start_x,
        y = start_y,
        cost = 0L,
        path = I(list(data.frame(x = start_x, y = start_y))),
        stringsAsFactors = FALSE
      )
      
      best <- data.frame(
        key = start_key,
        cost = 0L,
        stringsAsFactors = FALSE
      )
      
      paths <- list()
      
      while (is.data.frame(frontier) && nrow(frontier) > 0) {
        
        idx <- which.min(frontier$cost)
        if (length(idx) < 1 || is.na(idx)) break
        
        cur <- frontier[idx, , drop = FALSE]
        frontier <- frontier[-idx, , drop = FALSE]
        
        cur_x <- suppressWarnings(as.integer(cur$x[1] %||% NA))
        cur_y <- suppressWarnings(as.integer(cur$y[1] %||% NA))
        cur_cost <- suppressWarnings(as.integer(cur$cost[1] %||% 0L))
        
        if (
          length(cur_x) < 1 || length(cur_y) < 1 || length(cur_cost) < 1 ||
          is.na(cur_x) || is.na(cur_y) || is.na(cur_cost)
        ) {
          next
        }
        
        for (i in seq_len(nrow(dirs))) {
          
          nx <- suppressWarnings(as.integer(cur_x + dirs$dx[i]))
          ny <- suppressWarnings(as.integer(cur_y + dirs$dy[i]))
          
          if (length(nx) < 1 || length(ny) < 1 || is.na(nx) || is.na(ny)) {
            next
          }
          
          nkey <- paste(nx, ny, sep = ",")
          
          # First reject anything not actually on the map.
          # This prevents can_enter_tile() from receiving missing/off-map tiles.
          tile_row <- tryCatch(
            get_tile_row(
              tiles = tiles,
              x = nx,
              y = ny,
              map_id = map_id()
            ),
            error = function(e) data.frame()
          )
          
          if (!is.data.frame(tile_row) || nrow(tile_row) < 1) {
            next
          }
          
          move_check <- tryCatch(
            can_enter_tile(
              tiles = tiles,
              occupants = occ,
              x = nx,
              y = ny,
              map_id = map_id(),
              exclude_actor_id = actor_id
            ),
            error = function(e) {
              list(
                ok = FALSE,
                reason = "error",
                move_cost = 1
              )
            }
          )
          
          ok <- isTRUE(move_check$ok)
          
          if (!ok) {
            
            if (isTRUE(can_phase)) {
              
              move_check$ok <- TRUE
              move_check$reason <- "phase"
              move_check$move_cost <- suppressWarnings(
                as.numeric(tile_row$move_cost[1] %||% 1)
              )
              
            } else {
              next
            }
          }
          
          move_cost <- suppressWarnings(as.numeric(move_check$move_cost %||% 1))
          
          if (length(move_cost) < 1 || is.na(move_cost) || move_cost <= 0) {
            move_cost <- 1
          }
          
          step_ft <- as.integer(round(move_cost * 5))
          
          if (length(step_ft) < 1 || is.na(step_ft) || step_ft < 5L) {
            step_ft <- 5L
          }
          
          new_cost <- as.integer(cur_cost + step_ft)
          
          if (length(new_cost) < 1 || is.na(new_cost)) {
            next
          }
          
          if (new_cost > remaining_ft) {
            next
          }
          
          old_best <- best[best$key == nkey, , drop = FALSE]
          
          if (
            is.data.frame(old_best) &&
            nrow(old_best) > 0 &&
            length(old_best$cost[1]) > 0 &&
            !is.na(old_best$cost[1]) &&
            old_best$cost[1] <= new_cost
          ) {
            next
          }
          
          cur_path <- cur$path[[1]]
          
          if (!is.data.frame(cur_path) || nrow(cur_path) < 1) {
            cur_path <- data.frame(x = cur_x, y = cur_y)
          }
          
          new_path <- rbind(
            cur_path,
            data.frame(x = nx, y = ny)
          )
          
          best <- best[best$key != nkey, , drop = FALSE]
          
          best <- rbind(
            best,
            data.frame(
              key = nkey,
              cost = new_cost,
              stringsAsFactors = FALSE
            )
          )
          
          paths[[nkey]] <- list(
            cost = new_cost,
            path = new_path
          )
          
          frontier <- rbind(
            frontier,
            data.frame(
              x = nx,
              y = ny,
              cost = new_cost,
              path = I(list(new_path)),
              stringsAsFactors = FALSE
            )
          )
        }
      }
      
      out <- best[best$key != start_key, , drop = FALSE]
      
      if (!is.data.frame(out) || nrow(out) < 1) {
        movement_paths(list())
        return(empty_reachable())
      }
      
      split_keys <- strsplit(out$key, ",", fixed = TRUE)
      valid <- vapply(split_keys, length, integer(1)) == 2L
      
      if (!any(valid)) {
        movement_paths(list())
        return(empty_reachable())
      }
      
      out <- out[valid, , drop = FALSE]
      split_keys <- split_keys[valid]
      
      xy <- do.call(rbind, split_keys)
      
      result <- data.frame(
        x = suppressWarnings(as.integer(xy[, 1])),
        y = suppressWarnings(as.integer(xy[, 2])),
        move_cost_ft = suppressWarnings(as.integer(out$cost)),
        stringsAsFactors = FALSE
      )
      
      result <- result[
        !is.na(result$x) &
          !is.na(result$y) &
          !is.na(result$move_cost_ft),
        ,
        drop = FALSE
      ]
      
      if (!is.data.frame(result) || nrow(result) < 1) {
        movement_paths(list())
        return(empty_reachable())
      }
      
      # Heart Eater phase may pass through occupied spaces,
      # but no actor may end movement on an occupied space.
      if (
        is.data.frame(occ) &&
        nrow(occ) > 0 &&
        all(c("x", "y") %in% names(occ))
      ) {
        occupied_keys <- paste(occ$x, occ$y, sep = ",")
        
        result <- result[
          !paste(result$x, result$y, sep = ",") %in% occupied_keys,
          ,
          drop = FALSE
        ]
      }
      
      valid_keys <- paste(result$x, result$y, sep = ",")
      paths <- paths[names(paths) %in% valid_keys]
      
      movement_paths(paths)
      
      result
    })
    
    attack_range_tiles <- reactive({
      if (!isTRUE(is_players_turn())) {
        return(data.frame(x = integer(), y = integer()))
      }
      
      pos <- active_position_row()
      if (!is.data.frame(pos) || nrow(pos) == 0) {
        return(data.frame(x = integer(), y = integer()))
      }
      
      start_x <- as.integer(pos$x[1] %||% NA)
      start_y <- as.integer(pos$y[1] %||% NA)
      if (is.na(start_x) || is.na(start_y)) {
        return(data.frame(x = integer(), y = integer()))
      }
      
      # Pass 1: melee range only.
      # Later we can pull real weapon range from selected weapon/equipped weapon.
      range_ft <- 5L
      steps <- max(1L, floor(range_ft / 5L))
      
      out <- expand.grid(dx = -steps:steps, dy = -steps:steps)
      out <- out[!(out$dx == 0 & out$dy == 0), , drop = FALSE]
      out <- out[pmax(abs(out$dx), abs(out$dy)) <= steps, , drop = FALSE]
      
      data.frame(
        x = start_x + out$dx,
        y = start_y + out$dy
      )
    })
    
    
    move_actor_to_tile <- function(target_x, target_y, forced_cost_ft = NULL) {
      
      eid <- current_encounter_id()
      combat <- combat_tbl()
      cid <- as.character(core$state$char_id %||% "")
      
      if (!nzchar(cid) || !isTRUE(is_players_turn())) {
        log_safe("⚠️ You can only move your own character on your turn.")
        return(FALSE)
      }
      
      if (is.na(eid) || !is.data.frame(combat) || nrow(combat) < 1) {
        log_safe("⚠️ No active combat actor.")
        return(FALSE)
      }
      
      actor_id <- as.character(combat$active_actor_id[1] %||% "")
      actor_type <- as.character(combat$active_actor_type[1] %||% "player")
      
      if (!identical(actor_id, cid)) {
        log_safe("⚠️ You can only move your own character.")
        return(FALSE)
      }
      
      pos <- active_position_row()
      
      if (!is.data.frame(pos) || nrow(pos) < 1) {
        log_safe("⚠️ Active actor has no map position.")
        return(FALSE)
      }
      
      old_x <- suppressWarnings(as.integer(pos$x[1] %||% NA))
      old_y <- suppressWarnings(as.integer(pos$y[1] %||% NA))
      
      if (is.na(old_x) || is.na(old_y)) {
        return(FALSE)
      }
      
      target_x <- suppressWarnings(as.integer(target_x))
      target_y <- suppressWarnings(as.integer(target_y))
      
      if (is.na(target_x) || is.na(target_y)) {
        return(FALSE)
      }
      
      if (identical(old_x, target_x) && identical(old_y, target_y)) {
        return(FALSE)
      }
      
      if (tile_is_occupied(
        occupants = map_occupants_rv(),
        x = target_x,
        y = target_y,
        map_id = map_id(),
        exclude_actor_id = actor_id
      )) {
        log_safe("⚠️ You cannot end movement in an occupied space.")
        return(FALSE)
      }
      
      if (!is.null(forced_cost_ft)) {
        
        total_ft <- suppressWarnings(as.integer(forced_cost_ft))
        
      } else {
        
        reachable <- movement_range_tiles()
        
        if (!is.data.frame(reachable) || nrow(reachable) < 1) {
          log_safe("⚠️ No reachable movement tiles.")
          return(FALSE)
        }
        
        target_row <- reachable[
          reachable$x == target_x &
            reachable$y == target_y,
          ,
          drop = FALSE
        ]
        
        if (!is.data.frame(target_row) || nrow(target_row) < 1) {
          log_safe("⚠️ Destination unreachable.")
          return(FALSE)
        }
        
        total_ft <- suppressWarnings(as.integer(target_row$move_cost_ft[1] %||% NA))
      }
      
      if (length(total_ft) < 1 || is.na(total_ft) || total_ft < 0L) {
        log_safe("⚠️ Could not calculate movement cost.")
        return(FALSE)
      }
      
      remaining_ft <- suppressWarnings(
        as.integer(movement_allowance_ft() - as.integer(turn_move_ft() %||% 0L))
      )
      
      if (length(remaining_ft) < 1 || is.na(remaining_ft)) {
        remaining_ft <- 0L
      }
      
      if (total_ft > remaining_ft) {
        log_safe(paste0(
          "⚠️ Not enough movement. Needs ",
          total_ft,
          " ft; you have ",
          remaining_ft,
          " ft."
        ))
        return(FALSE)
      }
      
      phased_any <- isTRUE(movement_phase()) && isTRUE(can_use_phase())
      
      ok <- upsert_encounter_actor_position(
        encounter_id = eid,
        actor_type = actor_type,
        actor_id = actor_id,
        x = target_x,
        y = target_y
      )
      
      if (!isTRUE(ok)) {
        log_safe("⚠️ Could not move active actor.")
        return(FALSE)
      }
      
      log_game_event(
        encounter_id = eid,
        event_type = "move",
        actor_type = actor_type,
        actor_id = actor_id,
        payload = list(
          from = list(x = old_x, y = old_y),
          to = list(x = target_x, y = target_y),
          move_cost_ft = total_ft,
          phased = phased_any,
          dash = isTRUE(movement_dash()),
          atomic_click_move = TRUE
        )
      )
      
      turn_move_ft(as.integer(turn_move_ft() + total_ft))
      
      pending_move(NULL)
      
      log_safe(paste0(
        if (phased_any) "👻 " else "🧭 ",
        "Moved to (", target_x, ", ", target_y, "). Cost: ", total_ft, " ft."
      ))
      
      bump_positions()
      bump_map_visual()
      
      TRUE
    }
    # --------------------------------------------------
    # Initiative
    # --------------------------------------------------
    
    observeEvent(input$move_to_tile, {
      
      target_x <- suppressWarnings(as.integer(input$move_to_tile$x %||% NA))
      target_y <- suppressWarnings(as.integer(input$move_to_tile$y %||% NA))
      
      if (is.na(target_x) || is.na(target_y)) return()
      
      pm <- pending_move()
      
      # Second click on same tile confirms
      if (
        !is.null(pm) &&
        identical(as.integer(pm$x), target_x) &&
        identical(as.integer(pm$y), target_y)
      ) {
        move_actor_to_tile(
          target_x,
          target_y,
          forced_cost_ft = pm$cost_ft
        )
        
        pending_move(NULL)
        return()
      }
      
      pos <- active_position_row()
      
      if (!is.data.frame(pos) || nrow(pos) < 1) {
        log_safe("⚠️ Active actor has no map position.")
        return()
      }
      
      old_x <- suppressWarnings(as.integer(pos$x[1] %||% NA))
      old_y <- suppressWarnings(as.integer(pos$y[1] %||% NA))
      
      if (is.na(old_x) || is.na(old_y)) return()
      
      dx <- target_x - old_x
      dy <- target_y - old_y
      steps <- max(abs(dx), abs(dy))
      
      if (steps < 1) return()
      
      cost_ft <- as.integer(steps * 5L)
      
      speed_ft <- movement_allowance_ft()
      used_ft <- as.integer(turn_move_ft() %||% 0L)
      remaining_ft <- max(0L, speed_ft - used_ft)
      
      if (cost_ft > remaining_ft) {
        log_safe(paste0(
          "⚠️ Destination too far. Needs ",
          cost_ft,
          " ft; you have ",
          remaining_ft,
          " ft."
        ))
        return()
      }
      
      if (tile_is_occupied(
        occupants = map_occupants_rv(),
        x = target_x,
        y = target_y,
        map_id = map_id(),
        exclude_actor_id = active_actor_id()
      )) {
        log_safe("⚠️ You cannot end movement in an occupied space.")
        return()
      }
      
      pending_move(list(
        x = target_x,
        y = target_y,
        cost_ft = cost_ft,
        dash = isTRUE(movement_dash()),
        phase = isTRUE(movement_phase()) && isTRUE(can_use_phase())
      ))
      
      log_safe(paste0(
        "🟨 Move preview: (",
        target_x,
        ", ",
        target_y,
        ") — ",
        cost_ft,
        " ft. Click again to confirm."
      ))
      
      bump_map_visual()
      
    }, ignoreInit = TRUE)
    

    

    
    class_combat_actions <- reactive({
      char <- core$state$char
      if (is.null(char)) return(list())
      get_unlocked_combat_actions(char)
    })

    output$class_actions_ui <- renderUI({
      actions <- class_combat_actions()
      if (!length(actions)) return(NULL)
      actionButton(
        session$ns("open_class_action"),
        paste0("Abilities (", length(actions), ")"),
        class = "btn btn-primary"
      )
    })

    observeEvent(input$open_class_action, {
      if (!isTRUE(is_players_turn())) {
        log_safe("⚠️ Combat abilities can only be used on your turn.")
        return()
      }

      actions <- class_combat_actions()
      if (!length(actions)) return()
      actors <- encounter_actors_tbl()
      targets <- actors[as.character(actors$actor_type %||% "") == "enemy", , drop = FALSE]
      needs_enemy <- any(vapply(actions, function(feature) {
        identical(as.character(feature$action$target %||% "enemy"), "enemy")
      }, logical(1)))
      has_self_action <- any(vapply(actions, function(feature) {
        identical(as.character(feature$action$target %||% "enemy"), "self")
      }, logical(1)))
      if (needs_enemy && !has_self_action && (!is.data.frame(targets) || nrow(targets) == 0L)) {
        log_safe("⚠️ There are no enemy targets in this encounter.")
        return()
      }

      action_choices <- stats::setNames(
        as.character(seq_along(actions)),
        vapply(actions, function(feature) as.character(feature$action$name %||% feature$name), character(1))
      )
      showModal(modalDialog(
        title = "Use Combat Ability",
        selectInput(session$ns("class_action_index"), "Ability", choices = action_choices),
        uiOutput(session$ns("class_action_target_ui")),
        uiOutput(session$ns("class_action_preview_ui")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_class_action"), "Use Ability", class = "btn btn-danger")
        ),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)

    output$class_action_target_ui <- renderUI({
      actions <- class_combat_actions()
      idx <- suppressWarnings(as.integer(input$class_action_index %||% 1L))
      if (is.na(idx) || idx < 1L || idx > length(actions)) return(NULL)
      action <- actions[[idx]]$action
      if (identical(as.character(action$target %||% "enemy"), "self")) {
        return(tags$p(class = "confirm-note", "Target: Self"))
      }
      actors <- encounter_actors_tbl()
      targets <- actors[as.character(actors$actor_type %||% "") == "enemy", , drop = FALSE]
      if (!is.data.frame(targets) || nrow(targets) == 0L) {
        return(tags$p(class = "confirm-note", "No enemy targets are available."))
      }
      selectInput(
        session$ns("class_action_target"), "Target",
        choices = stats::setNames(as.character(targets$actor_id), as.character(targets$display_name %||% targets$actor_id))
      )
    })

    output$class_action_preview_ui <- renderUI({
      actions <- class_combat_actions()
      idx <- suppressWarnings(as.integer(input$class_action_index %||% 1L))
      if (is.na(idx) || idx < 1L || idx > length(actions)) return(NULL)
      feature <- actions[[idx]]
      action <- feature$action
      resource <- action$resource %||% list()
      div(
        class = "confirm-box",
        tags$strong(action$name %||% feature$name),
        tags$p(feature$desc),
        if (length(resource)) tags$p(
          tags$strong("Cost: "), resource$cost %||% 0L, " ", tools::toTitleCase(resource$name %||% "resource")
        ),
        if (nzchar(action$note %||% "")) tags$p(class = "confirm-note", action$note)
      )
    })

    observeEvent(input$confirm_class_action, {
      req(isTRUE(is_players_turn()))
      actions <- class_combat_actions()
      idx <- suppressWarnings(as.integer(input$class_action_index %||% NA))
      if (is.na(idx) || idx < 1L || idx > length(actions)) return()

      feature <- actions[[idx]]
      action <- feature$action
      target_mode <- as.character(action$target %||% "enemy")
      target_id <- if (identical(target_mode, "self")) {
        as.character(core$state$char_id %||% "self")
      } else as.character(input$class_action_target %||% "")
      if (!nzchar(target_id)) return()
      damage <- action$damage %||% list()

      char <- validate_character(core$state$char)
      if (!class_action_use_available(char, action)) {
        log_safe(paste0("⚠️ ", action$name %||% feature$name, " has already been used and needs a rest."))
        return()
      }
      resource <- action$resource %||% list()
      if (length(resource) && identical(as.character(resource$name %||% ""), "sindre")) {
        cost <- suppressWarnings(as.integer(resource$cost %||% 0L))
        available <- suppressWarnings(as.integer(char$resources$sindre$cur %||% 0L))
        if (is.na(cost)) cost <- 0L
        if (is.na(available)) available <- 0L
        if (available < cost) {
          log_safe("⚠️ Not enough Sindre for that ability.")
          return()
        }
        char$resources$sindre$cur <- available - cost
      }

      if (identical(target_mode, "self") && is.list(action$healing)) {
        healing <- resolve_class_action_healing(action, char)
        char <- mark_class_action_used(char, action)
        core$state$char <- char
        result <- apply_healing_to_state(core$state, healing)
        if (is.null(result)) {
          log_safe("⚠️ The healing ability could not be applied.")
          return()
        }
        ability_name <- as.character(action$name %||% feature$name)
        gained <- as.integer(result$hp_after %||% 0L) - as.integer(result$hp_before %||% 0L)
        log_game_event(
          encounter_id = current_encounter_id(), event_type = "ability",
          actor_type = "player", actor_id = as.character(core$state$char_id %||% ""),
          target_id = target_id,
          payload = list(ability_name = ability_name, healing_rolled = healing, healing = gained)
        )
        removeModal()
        log_safe(paste0("✨ ", ability_name, " restores ", gained, " HP."))
        bump_refresh()
        return()
      }

      target_row <- get_actor_row(target_id, "enemy")
      if (!is.data.frame(target_row) || nrow(target_row) == 0L) {
        log_safe("⚠️ That target is no longer available.")
        removeModal()
        return()
      }

      max_hp <- suppressWarnings(as.integer(target_row$hp_max[1] %||% target_row$max_hp[1] %||% 1L))
      resolved_damage <- resolve_class_action_damage(action, max_hp, char)
      raw_damage <- resolved_damage$amount

      target_char <- load_actor_for_combat(target_id, "enemy")
      traits <- get_damage_traits(target_char)
      adjusted <- apply_damage_traits_to_parts(
        list(list(total = raw_damage, type = resolved_damage$damage_type)),
        traits
      )
      final_damage <- as.integer(adjusted$total %||% raw_damage)
      eid <- current_encounter_id()
      result <- tryCatch(
        damage_encounter_enemy(eid, target_id, final_damage),
        error = function(e) NULL
      )
      if (is.null(result)) {
        log_safe("⚠️ The ability could not be applied.")
        return()
      }

      core$state$char <- mark_class_action_used(char, action)
      ability_name <- as.character(action$name %||% feature$name)
      target_name <- get_actor_display_name(target_id, "enemy")
      log_game_event(
        encounter_id = eid,
        event_type = "ability",
        actor_type = "player",
        actor_id = as.character(core$state$char_id %||% ""),
        target_id = target_id,
        payload = list(
          ability_name = ability_name,
          target_name = target_name,
          raw_damage = raw_damage,
          damage = final_damage,
          damage_type = resolved_damage$damage_type,
          resource_cost = resource$cost %||% 0L
        )
      )
      removeModal()
      log_safe(paste0("✨ ", ability_name, " deals ", final_damage, " damage to ", target_name, "."))
      bump_refresh()
    }, ignoreInit = TRUE)

    output$initiative_ui <- renderUI({
      actors <- encounter_actors_tbl()
      aid <- active_actor_id()
      
      if (!is.data.frame(actors) || nrow(actors) == 0) {
        return(tagList(
          div(class = "combat-section-title", "Initiative"),
          div("No actors in this encounter.")
        ))
      }
      
      if ("turn_order" %in% names(actors)) {
        actors <- actors[order(actors$turn_order, na.last = TRUE), , drop = FALSE]
      }
      
      rows <- lapply(seq_len(nrow(actors)), function(i) {
        row <- actors[i, , drop = FALSE]
        
        cid <- as.character(row$actor_id[1] %||% "")
        ctype <- as.character(row$actor_type[1] %||% "actor")
        nm <- as.character(row$display_name[1] %||% "Unknown")
        turn_order <- as.character(row$turn_order[1] %||% "—")
        initiative <- as.character(row$initiative[1] %||% "—")
        cur_hp  <- suppressWarnings(as.integer(row$current_hp[1] %||% row$hp_current[1] %||% 0))
        temp_hp <- suppressWarnings(as.integer(row$temp_hp[1] %||% 0))
        max_hp  <- suppressWarnings(as.integer(row$hp_max[1] %||% row$max_hp[1] %||% NA))
        
        if (is.na(cur_hp)) cur_hp <- 0L
        if (is.na(temp_hp)) temp_hp <- 0L
        
        if (is.na(max_hp) || max_hp < 1L) {
          max_hp <- max(cur_hp, 1L)
        }
        is_active <- isTRUE(row$is_active[1] %||% FALSE)
        
        ac_txt <- if (identical(ctype, "player")) {
          char_obj <- load_actor_for_combat(cid, "player")
          ac_val <- tryCatch(calc_auto_ac_for_char(char_obj), error = function(e) NA_integer_)
          if (!is.na(ac_val)) paste0("AC ", ac_val) else "AC ?"
        } else {
          format_known_ac(cid)
        }
        
        div(
          class = paste("initiative-row", if (!is.null(aid) && identical(cid, aid)) "active"),
          div(
            class = "initiative-top",
            div(
              div(class = "initiative-name", nm),
              div(class = "initiative-sub", paste("Order", turn_order, "• Initiative", initiative, "•", ctype))
            )
          ),
          div(
            class = "initiative-tags",
            div(class = "initiative-tag", if (is_active) "Active" else "Inactive"),
            div(class = "initiative-tag", ac_txt)
          ),
          hp_bar_ui(cur_hp, temp_hp, maxv = max_hp)
        )
      })
      
      tagList(
        div(class = "combat-section-title", "Initiative"),
        div(class = "initiative-list", rows)
      )
    })
    
    

    
    observeEvent(input$map_fullscreen_exit, {
      map_fullscreen(FALSE)
      
      later::later(function() {
        session$sendCustomMessage(
          "combat3d-resize",
          list(containerId = session$ns("combat_3d_canvas"))
        )
      }, delay = 0.15)
    }, ignoreInit = TRUE)
    
    
    output$combat_layout_ui <- renderUI({
      div(
        class = "combat-play-layout",
        
        div(
          class = "combat-card combat-map-card combat-map-card-large",
          uiOutput(session$ns("map_ui"))
        ),
        
        div(
          class = "combat-card combat-log-card combat-log-card-bottom",
          uiOutput(session$ns("log_ui"))
        )
      )
    })
    
    
    format_event_text_fast <- function(ev_row, actor_name_lookup) {
      typ <- as.character(ev_row$event_type[1] %||% "event")
      actor_id <- as.character(ev_row$actor_id[1] %||% "")
      target_id <- as.character(ev_row$target_id[1] %||% "")
      
      payload <- NULL
      if ("payload" %in% names(ev_row)) {
        payload <- safe_event_payload(ev_row$payload[[1]])
      }
      
      actor_name <- if (nzchar(actor_id)) actor_name_lookup(actor_id) else "Actor"
      target_name <- if (nzchar(target_id)) actor_name_lookup(target_id) else "Target"
      
      if (typ == "enter_combat") {
        return("Combat begins.")
      }
      
      if (typ == "initiative") {
        if (is.list(payload)) {
          return(paste0(
            "Initiative rolled. ",
            payload$top_actor %||% "Someone",
            " leads on ",
            payload$top_score %||% "?",
            "."
          ))
        }
        
        return("Initiative rolled.")
      }

      if (typ == "ability") {
        if (is.list(payload)) {
          return(paste0(
            actor_name, " uses ", payload$ability_name %||% "an ability",
            " on ", payload$target_name %||% target_name,
            " for ", payload$damage %||% 0, " ",
            payload$damage_type %||% "", " damage."
          ))
        }
        return(paste0(actor_name, " uses an ability."))
      }
      
      if (typ == "move") {
        if (is.list(payload) && !is.null(payload$to)) {
          tx <- payload$to$x %||% "?"
          ty <- payload$to$y %||% "?"
          cost <- payload$move_cost_ft %||% "?"
          
          prefix <- if (isTRUE(payload$phased %||% FALSE)) {
            paste0(actor_name, " phases")
          } else if (isTRUE(payload$dash %||% FALSE)) {
            paste0(actor_name, " dashes")
          } else {
            paste0(actor_name, " moves")
          }
          
          return(paste0(
            prefix,
            " to (", tx, ", ", ty, ").",
            " Cost: ", cost, " ft."
          ))
        }
        
        return(paste0(actor_name, " moves."))
      }
      
      if (typ == "heal") {
        if (is.list(payload) && !is.null(payload$amount)) {
          amt <- payload$amount %||% "?"
          before <- payload$hp_before %||% "?"
          after <- payload$hp_after %||% "?"
          
          return(paste0(
            target_name,
            " heals ",
            amt,
            " HP (",
            before,
            " → ",
            after,
            ")."
          ))
        }
        
        return(paste0(target_name, " is healed."))
      }
      
      if (typ == "damage") {
        if (is.list(payload) && !is.null(payload$amount)) {
          amt <- payload$amount %||% "?"
          before <- payload$hp_before %||% "?"
          after <- payload$hp_after %||% "?"
          
          return(paste0(
            target_name,
            " takes ",
            amt,
            " damage (HP ",
            before,
            " → ",
            after,
            ")."
          ))
        }
        
        return("Damage was dealt.")
      }
      
      if (typ == "defeated") {
        return(paste0(target_name, " is defeated."))
      }
      
      if (typ == "attack") {
        if (is.list(payload)) {
          wpn <- payload$weapon_name %||% "Weapon"
          
          opp_txt <- if (isTRUE(payload$is_opportunity_attack %||% FALSE)) {
            " makes an opportunity attack and"
          } else {
            ""
          }
          
          crit_txt <- if (isTRUE(payload$is_crit %||% FALSE)) {
            " critically hits "
          } else {
            " hits "
          }
          
          if (isTRUE(payload$is_hit %||% FALSE)) {
            dmg <- payload$final_damage %||% payload$damage_total %||% "?"
            extra <- if (isTRUE(payload$used_sneak_attack %||% FALSE)) " + Sneak Attack" else ""
            
            return(paste0(
              actor_name,
              opp_txt,
              crit_txt,
              target_name,
              " with ",
              wpn,
              extra,
              " for ",
              dmg,
              " damage."
            ))
          }
          
          return(paste0(
            actor_name,
            opp_txt,
            " misses ",
            target_name,
            " with ",
            wpn,
            " (",
            payload$attack_total %||% "?",
            " vs AC ",
            payload$target_ac %||% "?",
            ")."
          ))
        }
        
        return(paste0(actor_name, " attacks ", target_name, "."))
      }
      
      if (typ == "end_turn") {
        if (is.list(payload)) {
          from_name <- if (nzchar(as.character(payload$from_actor_id %||% ""))) {
            actor_name_lookup(payload$from_actor_id)
          } else {
            actor_name
          }
          
          to_name <- if (nzchar(as.character(payload$to_actor_id %||% ""))) {
            actor_name_lookup(payload$to_actor_id)
          } else {
            "Next actor"
          }
          
          return(paste0(from_name, " ends turn. ", to_name, " acts next."))
        }
        
        return("Turn advanced.")
      }
      
      typ
    }
    
    # --------------------------------------------------
    # Attack flow
    # --------------------------------------------------
    
    resolve_attack_adv_mode <- function(mode = "auto", attacker_id = "", target_id = "") {
      mode <- as.character(mode %||% "auto")
      
      if (identical(mode, "advantage")) return("Advantage")
      if (identical(mode, "disadvantage")) return("Disadvantage")
      if (identical(mode, "normal")) return("Normal")
      
      # Future automatic rules go here:
      # - target has cover -> "Disadvantage"
      # - attacker invisible -> "Advantage"
      # - prone/ranged/etc.
      "Normal"
    }
    
    open_attack_flow <- function(target_id) {
      
      is_opp <- !isTRUE(is_players_turn())
      current_attack_is_opp(isTRUE(is_opp))
      
      if (isTRUE(is_opp)) {
        log_safe("⚠️ This is not your turn. This will be treated as an opportunity attack.")
      }
      selected_target_id(as.character(target_id))
      
      attacker_id <- active_actor_id()
      attacker_type <- as.character(combat_tbl()$active_actor_type[1] %||% "")
      
      
      if (is.null(attacker_id) || !nzchar(as.character(attacker_id))) {
        log_safe("⚠️ No active attacker.")
        return()
      }
      
      if (!nzchar(target_id)) {
        log_safe("⚠️ Choose a target first.")
        return()
      }
      
      attacker_name <- get_actor_display_name(attacker_id)
      if (!nzchar(attacker_name)) attacker_name <- "Attacker"
      
      if (identical(attacker_type, "player")) {
        attacker_char <- load_actor_for_combat(attacker_id, "player")
        if (is.null(attacker_char)) {
          log_safe("⚠️ Could not load attacker.")
          return()
        }
        
        weapons <- get_equipped_weapons_for_combat(attacker_char)
        if (!is.data.frame(weapons) || nrow(weapons) == 0) {
          log_safe("⚠️ No equipped weapons available.")
          return()
        }
        
        labels <- vapply(seq_len(nrow(weapons)), function(i) {
          w <- weapons[i, , drop = FALSE]
          hit_bonus <- get_weapon_hit_bonus(attacker_char, w)
          
          paste0(
            w$name[1],
            " • Hit ",
            ifelse(hit_bonus >= 0, "+", ""),
            hit_bonus,
            " • ",
            w$damage1[1],
            if (nzchar(w$damage2[1])) paste0(" + ", w$damage2[1]) else ""
          )
        }, character(1))
        
        weapon_choices <- stats::setNames(as.character(weapons$id), labels)
        
        showModal(modalDialog(
          title = paste0("Choose weapon — ", attacker_name),
          radioButtons(session$ns("attack_weapon_id"), "Equipped weapons", choices = weapon_choices),
          radioButtons(
            session$ns("attack_adv_mode"),
            "Attack roll",
            choices = c(
              "Auto" = "auto",
              "Normal" = "normal",
              "Advantage" = "advantage",
              "Disadvantage" = "disadvantage"
            ),
            selected = "auto",
            inline = TRUE
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_attack"), "Roll Attack", class = "btn btn-danger")
          ),
          easyClose = TRUE
        ))
        
      } else if (identical(attacker_type, "enemy")) {
        attacker_char <- load_actor_for_combat(attacker_id, "enemy")
        target_type <- get_actor_type_by_id(target_id)
        target_char <- load_actor_for_combat(target_id, target_type)
        
        if (is.null(attacker_char) || is.null(target_char)) {
          log_safe("⚠️ Could not load combatants.")
          return()
        }
        
        preview <- build_enemy_attack_preview(
          attacker_char = attacker_char,
          target_char = target_char,
          attacker_name = attacker_name,
          target_name = get_actor_display_name(target_id),
          attacker_id = attacker_id,
          target_id = target_id,
          attacker_type = "enemy",
          target_type = target_type
        )
        
        pending_attack(preview)
        
        base_proposed <- if (isTRUE(preview$is_hit)) {
          compute_final_attack(preview)$adjusted$total
        } else {
          0L
        }
        
        showModal(modalDialog(
          title = "Confirm Attack",
          uiOutput(session$ns("attack_confirm_ui")),
          numericInput(
            session$ns("final_damage_override"),
            "Final damage to apply",
            value = base_proposed,
            min = 0,
            step = 1
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("apply_attack_final"), "Apply Result", class = "btn btn-danger")
          ),
          easyClose = TRUE,
          size = "m"
        ))
        
      } else {
        log_safe("⚠️ Unsupported attacker type.")
      }
    }
    
    pending_target <- reactiveVal(NULL)
    
    observeEvent(input$map_target_click, {
      
      actor_id <- as.character(input$map_target_click$actor_id %||% "")
      actor_type <- as.character(input$map_target_click$actor_type %||% "")
      
      if (!nzchar(actor_id)) return()
      
      combat <- combat_tbl()
      active_id <- as.character(combat$active_actor_id[1] %||% "")
      
      if (identical(actor_id, active_id)) return()
      
      old <- pending_target()
      
      if (!is.null(old) && identical(as.character(old$actor_id), actor_id)) {
        pending_target(NULL)
        open_attack_flow(actor_id)
        return()
      }
      
      pending_target(list(
        actor_id = actor_id,
        actor_type = actor_type
      ))
      
      selected_target_id(actor_id)
      
      log_safe(paste0(
        "🎯 Target selected: ",
        get_actor_display_name(actor_id),
        ". Click again to attack."
      ))
      
    }, ignoreInit = TRUE)
    
    build_enemy_attack_preview <- function(attacker_char, target_char, attacker_name, target_name,
                                           attacker_id, target_id, attacker_type = "enemy", target_type = NULL) {
      target_type <- as.character(target_type %||% get_actor_type_by_id(target_id) %||% "player")
      
      roll_obj <- roll_attack_d20(adv = "Normal")
      attack_roll <- as.integer(roll_obj$roll)
      attack_bonus <- as.integer(attacker_char$combat_profile$attack_bonus %||% 2L)
      attack_total <- as.integer(attack_roll + attack_bonus)
      target_ac <- get_effective_actor_ac(target_id, target_type, target_char)
      
      is_crit <- identical(attack_roll, 20L)
      is_hit <- is_crit || (attack_total >= target_ac)
      
      damage_parts <- list()
      if (isTRUE(is_hit)) {
        dmg_expr <- as.character(attacker_char$combat_profile$damage_expr %||% "1d6")
        dmg_type <- as.character(attacker_char$combat_profile$damage_type %||% "slashing")
        dr <- roll_dice_expr(dmg_expr)
        
        damage_parts <- list(list(
          source = "Enemy Attack",
          expr = dmg_expr,
          type = dmg_type,
          rolls = dr$rolls,
          total = as.integer(dr$total)
        ))
      }
      
      list(
        attacker_id = as.character(attacker_id),
        attacker_type = as.character(attacker_type),
        target_id = as.character(target_id),
        target_type = as.character(target_type),
        attacker_name = as.character(attacker_name),
        target_name = as.character(target_name),
        weapon_id = "",
        weapon_name = "Natural / Simple Attack",
        attack_roll = attack_roll,
        attack_rolls = attack_rolls,
        attack_bonus = as.integer(attack_bonus),
        attack_total = as.integer(attack_total),
        target_ac = as.integer(target_ac),
        is_hit = isTRUE(is_hit),
        is_crit = isTRUE(is_crit),
        base_parts = damage_parts,
        sneak_available = FALSE,
        sneak_part = NULL,
        target_traits = get_character_damage_traits(target_char),
        primary_damage_type = as.character(attacker_char$combat_profile$damage_type %||% "")
      )
    }
    
    
    
    observeEvent(input$confirm_attack, {
      removeModal()
      
      attacker_id <- active_actor_id()
      target_id <- as.character(selected_target_id() %||% "")
      weapon_id <- as.character(input$attack_weapon_id %||% "")
      
      if (is.null(attacker_id) || !nzchar(as.character(attacker_id))) {
        log_safe("⚠️ No active attacker.")
        return()
      }
      
      if (!nzchar(target_id)) {
        log_safe("⚠️ No target selected.")
        return()
      }
      
      if (!nzchar(weapon_id)) {
        log_safe("⚠️ No weapon selected.")
        return()
      }
      
      target_type <- get_actor_type_by_id(target_id)
      attacker_char <- load_actor_for_combat(attacker_id, "player")
      target_char <- load_actor_for_combat(target_id, target_type)
      
      if (is.null(attacker_char) || is.null(target_char)) {
        log_safe("⚠️ Could not load combatants.")
        return()
      }
      
      weapons <- get_equipped_weapons_for_combat(attacker_char)
      weapon_row <- weapons[as.character(weapons$id) == weapon_id, , drop = FALSE]
      
      if (!is.data.frame(weapon_row) || nrow(weapon_row) == 0) {
        log_safe("⚠️ Could not find selected weapon.")
        return()
      }
      
      attacker_name <- get_actor_display_name(attacker_id)
      target_name <- get_actor_display_name(target_id)
      
      if (!nzchar(attacker_name)) attacker_name <- "Attacker"
      if (!nzchar(target_name)) target_name <- "Target"
      
      adv_mode <- resolve_attack_adv_mode(
        mode = input$attack_adv_mode %||% "auto",
        attacker_id = attacker_id,
        target_id = target_id
      )
      
      preview <- build_attack_preview(
        attacker_char = attacker_char,
        target_char = target_char,
        weapon_row = weapon_row,
        attacker_name = attacker_name,
        target_name = target_name,
        attacker_id = attacker_id,
        target_id = target_id,
        adv_override = adv_mode
      )
      
      preview$is_opportunity_attack <- isTRUE(current_attack_is_opp())
      
      pending_attack(preview)
      
      base_proposed <- if (isTRUE(preview$is_hit)) {
        compute_final_attack(preview)$adjusted$total
      } else {
        0L
      }
      
      showModal(modalDialog(
        title = "Confirm Attack",
        uiOutput(session$ns("attack_confirm_ui")),
        numericInput(
          session$ns("final_damage_override"),
          "Final damage to apply",
          value = base_proposed,
          min = 0,
          step = 1
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("apply_attack_final"), "Apply Result", class = "btn btn-danger")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$apply_attack_final, {
      preview <- pending_attack()
      if (is.null(preview)) {
        removeModal()
        return()
      }
      
      removeModal()
      
      apply_sneak <- isTRUE(input$final_apply_sneak %||% FALSE)
      manual_bonus <- suppressWarnings(as.integer(input$final_bonus_damage %||% 0))
      manual_type <- as.character(input$final_bonus_type %||% "same_as_primary")
      final_damage <- suppressWarnings(as.integer(input$final_damage_override %||% 0))
      if (is.na(final_damage) || final_damage < 0) final_damage <- 0L
      
      calc <- compute_final_attack(
        preview = preview,
        apply_sneak = apply_sneak,
        manual_bonus = manual_bonus,
        manual_type = manual_type
      )
      
      eid <- current_encounter_id()
      
      update_known_ac(
        target_id = preview$target_id,
        attack_total = preview$attack_total,
        is_hit = preview$is_hit
      )
      
      log_game_event(
        encounter_id = eid,
        event_type = "attack",
        actor_type = as.character(preview$attacker_type %||% "player"),
        actor_id = as.character(preview$attacker_id),
        target_id = as.character(preview$target_id),
        payload = list(
          attacker_name = preview$attacker_name,
          target_name = preview$target_name,
          target_type = preview$target_type,
          weapon_name = preview$weapon_name,
          is_opportunity_attack = isTRUE(preview$is_opportunity_attack),
          attack_roll = preview$attack_roll,
          attack_rolls = as.list(preview$attack_rolls),
          attack_bonus = preview$attack_bonus,
          attack_total = preview$attack_total,
          target_ac = preview$target_ac,
          is_hit = preview$is_hit,
          is_crit = preview$is_crit,
          damage_total = calc$adjusted$total,
          final_damage = final_damage,
          used_sneak_attack = isTRUE(apply_sneak),
          raw_damage_total = calc$raw_total,
          damage_parts = lapply(calc$adjusted$parts, function(x) {
            list(
              source = x$source %||% "",
              type = x$type %||% "",
              total = x$total %||% 0,
              adjusted_total = x$adjusted_total %||% 0,
              rule = x$rule %||% "normal"
            )
          })
        )
      )
      
      if (isTRUE(preview$is_hit) && final_damage > 0L) {
        res <- if (identical(preview$target_type, "player")) {
          damage_player_in_encounter(
            encounter_id = eid,
            character_id = preview$target_id,
            amount = final_damage
          )
        } else {
          tryCatch(
            damage_encounter_enemy(
              encounter_id = eid,
              enemy_uuid = preview$target_id,
              amount = final_damage
            ),
            error = function(e) NULL
          )
        }
        
        if (!is.null(res)) {
          log_game_event(
            encounter_id = eid,
            event_type = "damage",
            actor_type = as.character(preview$attacker_type %||% "player"),
            actor_id = as.character(preview$attacker_id),
            target_id = as.character(preview$target_id),
            payload = list(
              amount = final_damage,
              hp_before = res$hp_before %||% NA,
              hp_after = res$hp_after %||% NA,
              temp_before = res$temp_before %||% NA,
              temp_after = res$temp_after %||% NA
            )
          )
        } else {
          log_safe("⚠️ Attack landed, but damage could not be applied.")
        }
      }
      
      ac_hint <- if (identical(preview$target_type, "enemy")) {
        format_known_ac(preview$target_id)
      } else {
        paste0("AC ", preview$target_ac)
      }
      
      if (isTRUE(preview$is_hit)) {
        log_safe(paste0(
          "🗡️ ", preview$attacker_name, " hits ", preview$target_name,
          " with ", preview$weapon_name,
          if (isTRUE(apply_sneak)) " + Sneak Attack" else "",
          " for ", final_damage, " damage. ",
          "(", preview$attack_total, " vs ", ac_hint, ")"
        ))
      } else {
        log_safe(paste0(
          "🛡️ ", preview$attacker_name, " misses ", preview$target_name,
          " with ", preview$weapon_name,
          " (", preview$attack_total, " vs ", ac_hint, ")."
        ))
      }
      
      pending_attack(NULL)
      bump_positions()
      bump_events()
      bump_initiative()
      bump_map_visual()
    }, ignoreInit = TRUE)
    
  
    # --------------------------------------------------
    # Combat log
    # --------------------------------------------------
    output$log_ui <- renderUI({
      ev <- events_tbl()
      
      if (!is.data.frame(ev) || nrow(ev) == 0) {
        return(tagList(
          div(class = "combat-section-title", "Combat Log"),
          div("No recent events.")
        ))
      }
      
      if ("created_at" %in% names(ev)) {
        ord <- order(ev$created_at, decreasing = TRUE, na.last = TRUE)
        ev <- ev[ord, , drop = FALSE]
      }
      
      actors_for_log <- isolate(encounter_actors_tbl())
      
      actor_name_lookup <- function(actor_id) {
        actor_id <- as.character(actor_id %||% "")
        if (!nzchar(actor_id)) return("Actor")
        
        if (is.data.frame(actors_for_log) && nrow(actors_for_log) > 0) {
          row <- actors_for_log[
            as.character(actors_for_log$actor_id) == actor_id,
            ,
            drop = FALSE
          ]
          
          if (nrow(row) > 0) {
            return(as.character(row$display_name[1] %||% row$name[1] %||% "Unknown"))
          }
        }
        
        "Unknown"
      }
      
      items <- lapply(seq_len(nrow(ev)), function(i) {
        row <- ev[i, , drop = FALSE]
        
        typ <- as.character(row$event_type[1] %||% "event")
        body <- format_event_text_fast(row, actor_name_lookup)
        ts <- if ("created_at" %in% names(row)) as.character(row$created_at[1] %||% "") else ""
        
        div(
          class = "combat-log-item",
          div(class = "combat-log-type", typ),
          div(class = "combat-log-body", body),
          if (nzchar(ts)) div(class = "combat-log-time", ts)
        )
      })
      
      tagList(
        div(class = "combat-section-title", "Combat Log"),
        div(class = "combat-log", items)
      )
    })
    
    # --------------------------------------------------
    # Refresh / bind / combat flow
    # --------------------------------------------------
    observeEvent(input$refresh, {
      bump_refresh()
    }, ignoreInit = TRUE)
    
    observeEvent(input$end_turn, {
      eid <- current_encounter_id()
      if (is.na(eid)) {
        log_safe("⚠️ Choose an encounter first.")
        return()
      }
      
      ok <- advance_turn(eid)
      turn_move_ft(0L)
      
      if (isTRUE(ok)) {
        log_safe("⏭️ Turn advanced.")
      } else {
        log_safe("⚠️ Could not advance turn.")
      }
      
      bump_combat()
      bump_positions()
      bump_events()
      bump_initiative()
      bump_map_visual()
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------
    # Movement
    # --------------------------------------------------
 
    
 
    
    #Thing thing
    last_positions_sig <- reactiveVal("")
    last_combat_sig <- reactiveVal("")
    last_events_sig <- reactiveVal("")
    
    df_sig <- function(df, cols = NULL) {
      if (!is.data.frame(df) || nrow(df) == 0) return("empty")
      
      if (!is.null(cols)) {
        cols <- intersect(cols, names(df))
        df <- df[, cols, drop = FALSE]
      }
      
      paste(utils::capture.output(str(df)), collapse = "|")
    }
    
    polling_busy <- reactiveVal(FALSE)
    
    observe({
      snapshot <- snapshot_data()
      
      if (isTRUE(isolate(polling_busy()))) return()
      
      polling_busy(TRUE)
      
      tryCatch({
        
        eid <- isolate(current_encounter_id())
        if (is.na(eid)) return()
        
        pos_now <- snapshot$positions %||% data.frame()
        pos_sig <- df_sig(pos_now, c("actor_type", "actor_id", "x", "y", "updated_at"))
        
        if (!identical(pos_sig, isolate(last_positions_sig()))) {
          last_positions_sig(pos_sig)
          bump_positions()
          bump_map_visual()
        }
        
        combat_now <- snapshot$combat %||% data.frame()
        combat_sig <- df_sig(
          combat_now,
          c("round_number", "current_turn_order", "active_actor_type", "active_actor_id", "phase", "updated_at")
        )
        
        if (!identical(combat_sig, isolate(last_combat_sig()))) {
          last_combat_sig(combat_sig)
          bump_combat()
          bump_initiative()
          bump_map_visual()
        }
        
        events_now <- snapshot$events %||% data.frame()
        
        if (is.data.frame(events_now) && nrow(events_now) > 0 && "event_type" %in% names(events_now)) {
          events_now <- events_now[
            !as.character(events_now$event_type %||% "") %in% c("move"),
            ,
            drop = FALSE
          ]
        }
        
        events_sig <- df_sig(events_now, c("id", "created_at", "event_type", "actor_id", "target_id"))
        
        if (!identical(events_sig, isolate(last_events_sig()))) {
          last_events_sig(events_sig)
          bump_events()
        }
        
      }, error = function(e) {
        message("Combat polling failed: ", conditionMessage(e))
      }, finally = {
        polling_busy(FALSE)
      })
    })
    # --------------------------------------------------
    # Damage test
    # --------------------------------------------------
    
    get_player_attackers_for_reaction <- function() {
      actors <- encounter_actors_tbl()
      if (!is.data.frame(actors) || nrow(actors) == 0) return(data.frame())
      
      actors[
        as.character(actors$actor_type %||% "") == "player" &
          as.logical(actors$is_active %||% TRUE),
        ,
        drop = FALSE
      ]
    }

  })
}
