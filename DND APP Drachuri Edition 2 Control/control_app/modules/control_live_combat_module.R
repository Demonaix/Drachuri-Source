library(shiny)

controlLiveCombatUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
  .live-combat-wrap{
    display:flex;
    flex-direction:column;
    gap:12px;
    width:100%;
    max-width:100%;
    min-width:0;
    box-sizing:border-box;
  }

  .live-combat-grid{
    display:grid;
    grid-template-columns:minmax(260px, .7fr) minmax(0, 2fr);
    gap:12px;
    width:100%;
    max-width:100%;
    min-width:0;
    box-sizing:border-box;
  }

  .live-combat-card{
    border:1px solid rgba(191,167,111,0.55);
    background:rgba(255,255,245,0.94);
    border-radius:14px;
    padding:14px;
    box-shadow:0 8px 22px rgba(0,0,0,0.10);
    min-width:0;
    max-width:100%;
    overflow:visible;
    box-sizing:border-box;
  }

  .live-combat-log-wide{ grid-column:1 / -1; min-height:320px; }
  .live-combat-log-wide .combat-log{ max-height:420px; overflow-y:auto; }
  .live-combat-card .selectize-dropdown{ z-index:10050 !important; }

  .combat-3d-shell{
    width:100%;
    max-width:100%;
    height:620px;
    min-height:620px;
    overflow:hidden;
    box-sizing:border-box;
  }

  .combat-3d-canvas{
    width:100%;
    max-width:100%;
    height:560px;
    min-height:560px;
    overflow:hidden;
    box-sizing:border-box;
  }

  .combat-3d-canvas canvas{
    display:block;
    width:100% !important;
    max-width:100% !important;
    height:100% !important;
  }

  .combat-map-toolbar{
    display:flex;
    flex-wrap:wrap;
    gap:8px;
    align-items:center;
    width:100%;
    max-width:100%;
    min-width:0;
    overflow:hidden;
    box-sizing:border-box;
    margin-bottom:8px;
  }

  .live-combat-title{
    font-size:20px;
    font-weight:900;
    margin-bottom:4px;
  }

  .live-combat-sub{
    font-size:12px;
    opacity:.82;
  }

  .live-combat-section{
    font-size:15px;
    font-weight:900;
    margin-bottom:8px;
  }

  .live-combat-pills{
    display:flex;
    flex-wrap:wrap;
    gap:8px;
    margin-top:10px;
    min-width:0;
  }

  .live-combat-pill{
    display:inline-flex;
    align-items:center;
    gap:6px;
    padding:6px 10px;
    border-radius:999px;
    border:1px solid rgba(191,167,111,0.75);
    background:rgba(255,255,245,0.9);
    font-size:12px;
    font-weight:800;
    max-width:100%;
  }

  .live-combat-actions{
    display:flex;
    flex-wrap:wrap;
    gap:8px;
    align-items:flex-end;
    margin-top:10px;
    max-width:100%;
    min-width:0;
  }

  .live-combat-actions .form-group{
    margin-bottom:0;
  }

  .initiative-list{
    display:flex;
    flex-direction:column;
    gap:8px;
    min-width:0;
  }

  .initiative-row{
    border:1px solid rgba(191,167,111,0.45);
    background:rgba(255,255,245,0.84);
    border-radius:12px;
    padding:8px 10px;
    min-width:0;
  }

  .initiative-row.active{
    border-color:rgba(180,90,40,0.95);
    box-shadow:0 0 0 2px rgba(220,140,60,0.18);
    background:rgba(255,248,235,0.96);
  }

  .initiative-top{
    display:flex;
    justify-content:space-between;
    gap:8px;
    align-items:center;
    min-width:0;
  }

  .initiative-name{
    font-weight:900;
    font-size:13px;
  }

  .initiative-sub{
    font-size:11px;
    opacity:.78;
    margin-top:2px;
  }

  .initiative-tags{
    display:flex;
    gap:6px;
    flex-wrap:wrap;
    margin-top:6px;
  }

  .initiative-tag{
    font-size:10px;
    font-weight:800;
    padding:3px 7px;
    border-radius:999px;
    background:rgba(62,47,28,0.08);
    border:1px solid rgba(191,167,111,0.45);
  }

  .mini-bar-wrap{
    margin-top:8px;
  }

  .mini-bar-label{
    font-size:10px;
    font-weight:800;
    margin-bottom:4px;
    opacity:.82;
  }

  .mini-bar{
    position:relative;
    height:10px;
    border-radius:999px;
    overflow:hidden;
    background:rgba(40,40,40,0.12);
    border:1px solid rgba(191,167,111,0.35);
  }

  .mini-fill{
    position:absolute;
    top:0;
    left:0;
    height:100%;
    border-radius:999px;
  }

  .mini-fill.hp{
    background:linear-gradient(90deg, rgba(170,35,35,0.92), rgba(240,120,60,0.90));
  }

  .battlefield-kv{
    display:grid;
    grid-template-columns:110px minmax(0, 1fr);
    gap:8px;
    row-gap:8px;
    font-size:12px;
    min-width:0;
  }

  .battlefield-k{
    font-weight:900;
    opacity:.9;
  }

  .combat-log{
    display:flex;
    flex-direction:column;
    gap:8px;
    max-height:440px;
    overflow-y:auto;
    overflow-x:hidden;
    padding-right:4px;
    min-width:0;
  }

  .combat-log-item{
    border:1px solid rgba(191,167,111,0.38);
    border-radius:10px;
    padding:8px 10px;
    background:rgba(255,255,245,0.82);
    min-width:0;
    word-break:break-word;
  }

  .combat-log-type{
    font-size:10px;
    font-weight:900;
    text-transform:uppercase;
    opacity:.75;
    margin-bottom:4px;
  }

  .combat-log-body{
    font-size:12px;
    line-height:1.25;
  }

  .combat-log-time{
    margin-top:4px;
    font-size:10px;
    opacity:.65;
    font-family:monospace;
  }

  .confirm-box{
    border:1px solid rgba(191,167,111,0.45);
    border-radius:10px;
    padding:10px;
    background:rgba(255,255,245,0.82);
    margin-bottom:10px;
  }

  .confirm-kv{
    display:grid;
    grid-template-columns:130px minmax(0, 1fr);
    gap:8px;
    row-gap:6px;
    font-size:12px;
    margin-bottom:10px;
  }

  .confirm-k{
    font-weight:900;
    opacity:.9;
  }

  .damage-part{
    display:flex;
    justify-content:space-between;
    gap:10px;
    font-size:12px;
    padding:4px 0;
    border-bottom:1px dashed rgba(191,167,111,0.3);
  }

  .damage-part:last-child{
    border-bottom:none;
  }

  .confirm-note{
    font-size:11px;
    opacity:.8;
    margin-top:8px;
  }

  @media (max-width: 1000px){
    .live-combat-grid{
      grid-template-columns:1fr;
    }

    .combat-3d-shell{
      height:520px;
      min-height:520px;
    }

    .combat-3d-canvas{
      height:460px;
      min-height:460px;
    }
  }
")),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "css/combat.css"),
    tags$script(src = paste0("js/combat2d_simple.js?v=", as.integer(Sys.time()))),
    
    div(
      class = "control-card",
      div(class = "control-section-title", "⚔️ Live Combat"),
      
      div(
        class = "live-combat-wrap",

        div(
          class = "live-combat-card",
          uiOutput(ns("header_ui")),
          
          div(
            class = "live-combat-actions",
            actionButton(ns("bind_encounter"), "Set Active Encounter", class = "btn btn-default"),
            actionButton(ns("start_combat"), "Start Combat", class = "btn btn-primary"),
            actionButton(ns("end_turn"), "End Turn", class = "btn btn-warning"),
            actionButton(ns("end_combat"), "End Combat", class = "btn btn-danger"),
            selectInput(ns("target_id"), "Target", choices = c(), width = "220px"),
            actionButton(ns("attack_btn"), "Attack", class = "btn btn-danger")
            ,checkboxInput(ns("movement_disengage"), "Disengage before moving", value = FALSE)
          )
        ),
        div(
          class = "live-combat-card",
          div(class = "live-combat-section", "Enemy Reinforcements"),
          
          div(
            class = "live-combat-actions",
            selectInput(ns("reinforce_npc_template_id"), "NPC / Enemy Template", choices = c(), width = "280px"),
            numericInput(ns("reinforce_count"), "Count", value = 1, min = 1, max = 20, width = "90px"),
            numericInput(ns("reinforce_x"), "X", value = 5, min = 1, width = "80px"),
            numericInput(ns("reinforce_y"), "Y", value = 5, min = 1, width = "80px"),
            actionButton(ns("add_reinforcement"), "Add Enemy", class = "btn btn-danger")
          ),
          tags$hr(),
          
          div(
            class = "live-combat-actions",
            selectInput(ns("selected_enemy_id"), "Enemy", choices = c(), width = "240px"),
            actionButton(ns("remove_selected_enemy"), "Remove Enemy", class = "btn btn-default")
          )
        ),
        div(
          class = "live-combat-card",
          div(class = "live-combat-section", "Admin Override"),
          div(
            class = "live-combat-actions",
            selectInput(ns("admin_target_id"), "Target", choices = c(), width = "240px"),
            numericInput(ns("admin_amount"), "Amount", value = 5, min = 0, width = "100px"),
            selectInput(
              ns("admin_mode"),
              "Action",
              choices = c(
                "Damage" = "damage",
                "Heal" = "heal"
              ),
              width = "120px"
            ),
            actionButton(ns("apply_admin"), "Apply Override", class = "btn btn-danger")
          )
        ),
        div(
          class = "live-combat-grid",
          div(class = "live-combat-card", uiOutput(ns("initiative_ui"))),
          div(
            class = "live-combat-card",
            uiOutput(ns("battlefield_ui")),
            uiOutput(ns("map_ui"))
          ),
          div(class = "live-combat-card live-combat-log-wide", uiOutput(ns("log_ui")))
        )
      ),
      
      
    )
  )
}

controlLiveCombatServer <- function(
    id,
    ctrl,
    session_tbl = NULL,
    players_tbl = NULL,
    positions_tbl = NULL,
    combat_tbl = NULL,
    events_tbl = NULL,
    bump_refresh
) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    pending_attack <- reactiveVal(NULL)
    turn_move_ft <- reactiveVal(0L)
    reinforce_templates_rv <- reactiveVal(data.frame())
    
    list_npc_templates <- function() {
      con <- get_db_connection()
      if (is.null(con)) return(data.frame())
      on.exit(release_db_connection(con), add = TRUE)
      
      tryCatch(
        DBI::dbGetQuery(
          con,
          "select * from npc_templates order by lower(name)"
        ),
        error = function(e) {
          message("list_npc_templates failed: ", e$message)
          data.frame()
        }
      )
    }
    
    get_npc_template <- function(npc_id) {
      con <- get_db_connection()
      if (is.null(con)) return(data.frame())
      on.exit(release_db_connection(con), add = TRUE)
      
      tryCatch(
        DBI::dbGetQuery(
          con,
          "
select *
from npc_templates
where npc_id = $1
limit 1
",
          params = list(as.character(npc_id))
        ),
        error = function(e) {
          message("get_npc_template failed: ", e$message)
          data.frame()
        }
      )
    }
    
    load_reinforce_templates <- function() {
      df <- list_npc_templates()
      if (!is.data.frame(df)) df <- data.frame()
      reinforce_templates_rv(df)
    }
    
    observeEvent(TRUE, {
      load_reinforce_templates()
    }, once = TRUE)
    
    observeEvent(input$refresh_reinforce_templates, {
      load_reinforce_templates()
    }, ignoreInit = TRUE)
    
    observeEvent(ctrl$refresh_key, {
      load_reinforce_templates()
    }, ignoreInit = TRUE)
    
    observe({
      df <- reinforce_templates_rv()
      
      if (!is.data.frame(df) || nrow(df) == 0 || !"npc_id" %in% names(df) || !"name" %in% names(df)) {
        updateSelectInput(session, "reinforce_npc_template_id", choices = c("No NPC templates found" = ""))
        return()
      }
      
      ids <- as.character(df$npc_id)
      labels <- paste0(df$name, " • HP ", df$hp_max, " • AC ", df$ac)
      
      selected_id <- as.character(input$reinforce_npc_template_id %||% "")
      if (!nzchar(selected_id) || !selected_id %in% ids) {
        selected_id <- ids[1]
      }
      
      updateSelectInput(
        session,
        "reinforce_npc_template_id",
        choices = stats::setNames(ids, labels),
        selected = selected_id
      )
    })
    
    known_ac_rv <- reactiveVal(data.frame(
      target_id = character(),
      lower = integer(),
      upper = integer(),
      stringsAsFactors = FALSE
    ))
    
    log_safe <- function(msg, type = "message") {
      showNotification(msg, type = type)
    }
    
    positions_key <- reactiveVal(0L)
    combat_key <- reactiveVal(0L)
    events_key <- reactiveVal(0L)
    actors_key <- reactiveVal(0L)
    enemies_key <- reactiveVal(0L)
    map_visual_key <- reactiveVal(0L)
    map_ui_ready <- reactiveVal(FALSE)
    pending_move <- reactiveVal(NULL)
    
    last_positions_sig <- reactiveVal("")
    last_combat_sig <- reactiveVal("")
    last_events_sig <- reactiveVal("")
    last_actors_sig <- reactiveVal("")
    last_enemies_sig <- reactiveVal("")
    
    df_sig <- function(df, cols = NULL) {
      if (!is.data.frame(df) || nrow(df) == 0) return("empty")
      if (!is.null(cols)) {
        cols <- intersect(cols, names(df))
        df <- df[, cols, drop = FALSE]
      }
      paste(utils::capture.output(str(df)), collapse = "|")
    }
    
    observe({
      invalidateLater(5000, session)
      
      eid <- current_encounter_id()
      if (is.na(eid)) return()
      
      pos_now <- tryCatch(get_encounter_positions(eid), error = function(e) data.frame())
      pos_sig <- df_sig(pos_now, c("actor_type", "actor_id", "x", "y", "updated_at"))
      
      if (!identical(pos_sig, last_positions_sig())) {
        last_positions_sig(pos_sig)
        bump_positions()
        bump_map_visual()
      }
      
      combat_now <- tryCatch(get_combat_state(eid), error = function(e) data.frame())
      combat_sig <- df_sig(combat_now, c(
        "round_number", "current_turn_order",
        "active_actor_type", "active_actor_id",
        "phase", "updated_at"
      ))
      
      if (!identical(combat_sig, last_combat_sig())) {
        last_combat_sig(combat_sig)
        bump_combat()
        bump_actors()
        bump_map_visual()
      }
      
      events_now <- tryCatch(get_encounter_events(eid, limit = 1), error = function(e) data.frame())
      events_sig <- df_sig(events_now, c("id", "created_at", "event_type", "actor_id", "target_id"))
      
      if (!identical(events_sig, last_events_sig())) {
        last_events_sig(events_sig)
        bump_events()
      }
      
      actors_now <- tryCatch(get_encounter_actors(eid), error = function(e) data.frame())
      actors_sig <- df_sig(actors_now, c(
        "actor_id", "actor_type", "display_name",
        "initiative", "turn_order", "is_active",
        "current_hp", "temp_hp", "hp_current", "hp_max", "max_hp"
      ))
      
      if (!identical(actors_sig, last_actors_sig())) {
        last_actors_sig(actors_sig)
        bump_actors()
      }
      
      enemies_now <- tryCatch(get_encounter_enemies(eid), error = function(e) data.frame())
      enemies_sig <- df_sig(enemies_now, c(
        "enemy_uuid", "name", "hp_current", "temp_hp",
        "hp_max", "ac", "movement_speed", "updated_at"
      ))
      
      if (!identical(enemies_sig, last_enemies_sig())) {
        last_enemies_sig(enemies_sig)
        bump_enemies()
        bump_actors()
      }
    })
    
    bump_positions <- function() positions_key(isolate(positions_key()) + 1L)
    bump_combat    <- function() combat_key(isolate(combat_key()) + 1L)
    bump_events    <- function() events_key(isolate(events_key()) + 1L)
    bump_actors    <- function() actors_key(isolate(actors_key()) + 1L)
    bump_enemies   <- function() enemies_key(isolate(enemies_key()) + 1L)
    bump_map_visual <- function() map_visual_key(isolate(map_visual_key()) + 1L)
    
    bump_live <- function() {
      bump_positions()
      bump_combat()
      bump_events()
      bump_actors()
      bump_enemies()
      bump_map_visual()
    }
    # --------------------------------------------------
    # Current IDs
    # --------------------------------------------------
    current_session_id <- reactive({
      sid <- suppressWarnings(as.integer(ctrl$active_session_id %||% ctrl$session_id %||% NA))
      if (is.na(sid) || sid < 1) return(NA_integer_)
      sid
    })
    
    current_encounter_id <- reactive({
      eid <- suppressWarnings(as.integer(ctrl$active_encounter_id %||% ctrl$encounter_id %||% NA))
      if (is.na(eid) || eid < 1) return(NA_integer_)
      eid
    })
    
    current_map_id <- reactive({
      enc <- encounter_tbl_r()
      mid <- suppressWarnings(as.integer(enc$map_id[1] %||% ctrl$map_id %||% 1L))
      if (is.na(mid) || mid < 1) mid <- 1L
      mid
    })
    

    
    add_3d_models_to_render_df <- function(render_df) {
      if (!is.data.frame(render_df) || nrow(render_df) == 0) return(render_df)
      
      model_cols <- c(
        "model_base", "model_hair", "model_body", "model_arms",
        "model_legs", "model_feet", "model_headgear",
        "model_accessory", "hair_color"
      )
      
      for (col in model_cols) {
        if (!col %in% names(render_df)) render_df[[col]] <- rep("", nrow(render_df))
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
        char_obj <- tryCatch(load_character_from_db(actor_id), error = function(e) NULL)
        if (is.null(char_obj)) next
        char_obj <- tryCatch(validate_character(char_obj), error = function(e) char_obj)
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
    
    map_occupants_r <- reactive({
      eid <- current_encounter_id()
      mid <- current_map_id()
      pos <- encounter_positions_r()
      
      occ <- empty_map_occupants()
      
      if (is.na(eid) || !is.data.frame(pos) || nrow(pos) == 0) {
        return(occ)
      }
      
      for (i in seq_len(nrow(pos))) {
        actor_id_i <- as.character(pos$actor_id[i] %||% "")
        actor_type_i <- as.character(pos$actor_type[i] %||% "player")
        x_i <- suppressWarnings(as.integer(pos$x[i] %||% NA))
        y_i <- suppressWarnings(as.integer(pos$y[i] %||% NA))
        
        if (!nzchar(actor_id_i) || is.na(x_i) || is.na(y_i)) next
        
        occ <- set_actor_position_local(
          occupants = occ,
          actor_id = actor_id_i,
          x = x_i,
          y = y_i,
          map_id = mid,
          encounter_id = eid,
          actor_type = actor_type_i
        )
      }
      
      occ
    })
    
    map_tiles_r <- reactive({
      mid <- current_map_id()
      get_or_create_shared_map_tiles(ctrl, mid, width = 10L, height = 10L)
    })
    
    observe({
      actors <- encounter_actors_r()
      
      choices <- c()
      
      if (is.data.frame(actors) && nrow(actors) > 0) {
        labels <- vapply(seq_len(nrow(actors)), function(i) {
          row <- actors[i, , drop = FALSE]
          nm <- as.character(row$display_name[1] %||% row$name[1] %||% "Unknown")
          tp <- as.character(row$actor_type[1] %||% "actor")
          paste0(nm, " (", tp, ")")
        }, character(1))
        
        ids <- as.character(actors$actor_id %||% "")
        choices <- stats::setNames(ids, labels)
      }
      
      current_target <- as.character(input$admin_target_id %||% "")
      choice_values <- unname(choices)
      
      selected_target <- if (nzchar(current_target) && current_target %in% choice_values) {
        current_target
      } else if (length(choice_values) > 0) {
        choice_values[1]
      } else {
        character(0)
      }
      
      updateSelectInput(session, "admin_target_id", choices = choices, selected = selected_target)
    })
    
    heal_player_in_encounter <- function(encounter_id, character_id, amount) {
      enc <- tryCatch(get_encounter(encounter_id), error = function(e) data.frame())
      if (!is.data.frame(enc) || nrow(enc) == 0) return(NULL)
      
      session_id <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
      if (is.na(session_id) || session_id < 1) return(NULL)
      
      tryCatch(
        heal_session_player(
          session_id = session_id,
          character_id = character_id,
          amount = amount
        ),
        error = function(e) NULL
      )
    }
    
    observeEvent(input$apply_admin, {
      eid <- current_encounter_id()
      target_id <- as.character(input$admin_target_id %||% "")
      amount <- suppressWarnings(as.integer(input$admin_amount %||% 0L))
      mode <- as.character(input$admin_mode %||% "damage")
      
      if (is.na(eid)) {
        log_safe("Choose an encounter first.", type = "error")
        return()
      }
      
      if (!nzchar(target_id)) {
        log_safe("Choose a target first.", type = "error")
        return()
      }
      
      if (is.na(amount) || amount <= 0L) {
        log_safe("Enter a valid amount.", type = "error")
        return()
      }
      
      target_type <- get_actor_type_by_id(target_id)
      if (!nzchar(target_type)) {
        log_safe("Could not determine target type.", type = "error")
        return()
      }
      
      res <- NULL
      
      if (identical(mode, "damage")) {
        res <- if (identical(target_type, "player")) {
          damage_player_in_encounter(
            encounter_id = eid,
            character_id = target_id,
            amount = amount
          )
        } else {
          tryCatch(
            damage_encounter_enemy(
              encounter_id = eid,
              enemy_uuid = target_id,
              amount = amount
            ),
            error = function(e) NULL
          )
        }
      } else if (identical(mode, "heal")) {
        res <- if (identical(target_type, "player")) {
          heal_player_in_encounter(
            encounter_id = eid,
            character_id = target_id,
            amount = amount
          )
        } else {
          tryCatch(
            heal_encounter_enemy(
              encounter_id = eid,
              enemy_uuid = target_id,
              amount = amount
            ),
            error = function(e) NULL
          )
        }
      }
      
      if (is.null(res)) {
        log_safe("Override failed.", type = "error")
        return()
      }
      
      try(
        log_game_event(
          encounter_id = eid,
          event_type = if (identical(mode, "heal")) "heal" else "damage",
          actor_type = "control",
          target_id = target_id,
          payload = list(
            amount = amount,
            hp_before = res$hp_before %||% NA,
            hp_after = res$hp_after %||% NA,
            temp_before = res$temp_before %||% NA,
            temp_after = res$temp_after %||% NA,
            override = TRUE
          )
        ),
        silent = TRUE
      )
      
      target_name <- get_actor_display_name(target_id)
      
      if (identical(mode, "heal")) {
        log_safe(paste0("Healed ", target_name, " for ", amount, "."), type = "message")
      } else {
        log_safe(paste0("Damaged ", target_name, " for ", amount, "."), type = "message")
      }
      
      bump_live()
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------
    # Data readers
    # --------------------------------------------------
    encounter_tbl_r <- reactive({
      ctrl$refresh_key
      
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- tryCatch(get_encounter(eid), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    
    session_players_r <- reactive({
      ctrl$refresh_key
      
      sid <- current_session_id()
      cat("\n[DM session_players_r] session:", sid, "\n")
      
      if (is.na(sid)) return(data.frame())
      
      df <- tryCatch(get_session_players(sid), error = function(e) {
        cat("[DM session_players_r] failed:", e$message, "\n")
        data.frame()
      })
      
      print(df)
      if (!is.data.frame(df)) data.frame() else df
    })
    
    encounter_positions_r <- reactive({
      positions_key()
      ctrl$refresh_key
      
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- tryCatch(get_encounter_positions(eid), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    
    combat_state_r <- reactive({
      combat_key()
      ctrl$refresh_key
      
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      if (!is.null(combat_tbl) && is.reactive(combat_tbl)) {
        df <- tryCatch(combat_tbl(), error = function(e) data.frame())
        if (is.data.frame(df) && nrow(df) > 0) return(df)
      }
      
      df <- tryCatch(get_combat_state(eid), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    
    encounter_events_r <- reactive({
      events_key()
      ctrl$refresh_key
      
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      if (!is.null(events_tbl) && is.reactive(events_tbl)) {
        df <- tryCatch(events_tbl(), error = function(e) data.frame())
        if (is.data.frame(df) && nrow(df) > 0) return(df)
      }
      
      df <- tryCatch(get_encounter_events(eid, limit = 20), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    
    encounter_actors_r <- reactive({
      actors_key()
      ctrl$refresh_key
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- tryCatch(get_encounter_actors(eid), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    
    encounter_enemies_r <- reactive({
      enemies_key()
      ctrl$refresh_key
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- tryCatch(get_encounter_enemies(eid), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    

    
    # --------------------------------------------------
    # Small helpers
    # --------------------------------------------------
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
    
    normalize_damage_type <- function(x) {
      x <- tolower(trimws(as.character(x %||% "")))
      x[nzchar(x)]
    }
    
    get_actor_row <- function(actor_id, actor_type = NULL) {
      actor_id <- as.character(actor_id %||% "")
      if (!nzchar(actor_id)) return(data.frame())
      
      actors <- encounter_actors_r()
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
      combat <- combat_state_r()
      cat("\n[active_actor_id] combat rows:", if (is.data.frame(combat)) nrow(combat) else "not df", "\n")
      print(combat)
      
      if (!is.data.frame(combat) || nrow(combat) == 0) return(NULL)
      
      aid <- as.character(combat$active_actor_id[1] %||% "")
      cat("[active_actor_id] active_actor_id =", aid, "\n")
      
      if (!nzchar(aid)) return(NULL)
      aid
    })
    
    active_actor_type <- reactive({
      combat <- combat_state_r()
      if (!is.data.frame(combat) || nrow(combat) == 0) return(NA_character_)
      as.character(combat$active_actor_type[1] %||% NA_character_)
    })
    
    active_actor_row <- reactive({
      aid <- active_actor_id()
      if (is.null(aid)) return(data.frame())
      get_actor_row(aid)
    })
    
    active_position_row <- reactive({
      aid <- active_actor_id()
      pos <- encounter_positions_r()
      
      if (is.null(aid) || !is.data.frame(pos) || nrow(pos) == 0) return(data.frame())
      pos[as.character(pos$actor_id) == as.character(aid), , drop = FALSE]
    })
    
    # --------------------------------------------------
    # Character / enemy loaders
    # --------------------------------------------------
    load_actor_for_combat <- function(actor_id, actor_type = NULL) {
      actor_id <- as.character(actor_id %||% "")
      actor_type <- as.character(actor_type %||% get_actor_type_by_id(actor_id) %||% "")
      
      if (!nzchar(actor_id) || !nzchar(actor_type)) return(NULL)
      
      if (identical(actor_type, "player")) {
        db_char <- tryCatch(load_character_from_db(actor_id), error = function(e) NULL)
        if (!is.null(db_char)) return(db_char)
        return(NULL)
      }
      
      if (identical(actor_type, "enemy")) {
        enemies <- encounter_enemies_r()
        if (!is.data.frame(enemies) || nrow(enemies) == 0) return(NULL)
        
        row <- enemies[as.character(enemies$enemy_uuid %||% enemies$actor_id %||% "") == actor_id, , drop = FALSE]
        if (nrow(row) == 0) return(NULL)
        
        return(list(
          meta = list(
            name = as.character(row$name[1] %||% "Enemy"),
            race = "Enemy"
          ),
          build = list(
            class = "Enemy",
            level = 1
          ),
          abilities = enemy_db_json(row$abilities[[1]] %||% NULL, list(str=10,dex=10,con=10,int=10,cha=10,bld_str=10)),
          resources = list(
            hp = list(
              max = as.integer(row$hp_max[1] %||% 1L),
              cur = as.integer(row$hp_current[1] %||% 0L),
              temp = as.integer(row$temp_hp[1] %||% 0L)
            ),
            sindre = list(cur = 0, total = 0, temp = 0)
          ),
          status = list(
            effects = character(0),
            exhaustion = 0,
            bloodlust = FALSE
          ),
          combat_profile = list(
            resistances = enemy_db_values(row$resistances[[1]] %||% NULL),
            immunities = enemy_db_values(row$immunities[[1]] %||% NULL),
            vulnerabilities = enemy_db_values(row$vulnerabilities[[1]] %||% NULL),
            condition_immunities = enemy_db_values(row$condition_immunities[[1]] %||% NULL),
            ac_override = as.integer(row$ac[1] %||% 10L),
            speed_ft = as.integer(row$movement_speed[1] %||% row$speed_ft[1] %||% 30L),
            initiative_mod = as.integer(row$initiative_mod[1] %||% 0L),
            
            # legacy single-attack fallback
            attack_bonus = as.integer(row$attack_bonus[1] %||% 2L),
            damage_expr = as.character(row$damage_expr[1] %||% "1d6"),
            damage_type = as.character(row$damage_type[1] %||% "slashing"),
            
            # new structured attack list
            attacks = parse_enemy_attacks(row)
          )
        ))
      }
      
      NULL
    }
    
    get_effective_actor_ac <- function(actor_id, actor_type = NULL, actor_obj = NULL) {
      actor_type <- as.character(actor_type %||% get_actor_type_by_id(actor_id) %||% "")
      
      if (identical(actor_type, "enemy")) {
        enemies <- encounter_enemies_r()
        if (is.data.frame(enemies) && nrow(enemies) > 0) {
          row <- enemies[as.character(enemies$enemy_uuid %||% enemies$actor_id %||% "") == as.character(actor_id), , drop = FALSE]
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
    
    get_actor_speed_ft <- function(actor_id, actor_type = NULL) {
      actor_type <- as.character(actor_type %||% get_actor_type_by_id(actor_id) %||% "")
      obj <- load_actor_for_combat(actor_id, actor_type)
      
      if (is.null(obj)) return(30L)
      
      if (identical(actor_type, "enemy")) {
        spd <- suppressWarnings(as.integer(obj$combat_profile$speed_ft %||% 30L))
        if (!is.na(spd)) return(spd)
      }
      
      if (identical(actor_type, "player")) {
        spd <- suppressWarnings(as.integer(obj$combat$speed_ft %||% obj$combat_profile$speed_ft %||% 30L))
        if (!is.na(spd)) return(spd)
      }
      
      30L
    }
    
    # --------------------------------------------------
    # Damage / AC helpers
    # --------------------------------------------------
    damage_player_in_encounter <- function(encounter_id, character_id, amount) {
      enc <- tryCatch(get_encounter(encounter_id), error = function(e) data.frame())
      if (!is.data.frame(enc) || nrow(enc) == 0) return(NULL)
      
      session_id <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
      if (is.na(session_id) || session_id < 1) return(NULL)
      
      tryCatch(
        damage_session_player(
          session_id = session_id,
          character_id = character_id,
          amount = amount
        ),
        error = function(e) NULL
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
    
    # --------------------------------------------------
    # Damage traits
    # --------------------------------------------------
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
    
    # --------------------------------------------------
    # Attack builders
    # --------------------------------------------------
    build_attack_preview <- function(attacker_char, target_char, weapon_row, attacker_name, target_name,
                                     attacker_id, target_id, attacker_type = "player", target_type = NULL) {
      attacker_char <- validate_character(attacker_char)
      target_type <- as.character(target_type %||% get_actor_type_by_id(target_id) %||% "player")
      
      adv <- as.character(weapon_row$adv[1] %||% "Normal")
      attack_roll_obj <- roll_attack_d20(adv = adv)
      attack_roll <- as.integer(attack_roll_obj$roll)
      
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
        attack_roll = attack_roll,
        attack_rolls = as.integer(attack_roll_obj$rolls),
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
    
    build_enemy_attacks_json <- function(attacks) {
      jsonlite::toJSON(attacks, auto_unbox = TRUE, pretty = TRUE)
    }
    
    build_enemy_attack_preview <- function(attacker_char, target_char, attacker_name, target_name,
                                           attacker_id, target_id, attacker_type = "enemy",
                                           target_type = NULL, chosen_attack = NULL) {
      `%||%` <- get("%||%", inherits = TRUE)
      
      target_type <- as.character(target_type %||% get_actor_type_by_id(target_id) %||% "player")
      
      attacks <- attacker_char$combat_profile$attacks %||% list()
      
      # fallback if missing for any reason
      if (!is.list(attacks) || length(attacks) == 0) {
        attacks <- list(list(
          id = "default",
          name = "Natural / Simple Attack",
          attack_bonus = as.integer(attacker_char$combat_profile$attack_bonus %||% 0L),
          damage_expr = as.character(attacker_char$combat_profile$damage_expr %||% "1d4"),
          damage_type = as.character(attacker_char$combat_profile$damage_type %||% "bludgeoning")
        ))
      }
      
      atk <- NULL
      
      if (!is.null(chosen_attack) && nzchar(as.character(chosen_attack))) {
        idx <- which(vapply(attacks, function(x) identical(as.character(x$id %||% ""), as.character(chosen_attack)), logical(1)))
        if (length(idx) >= 1) atk <- attacks[[idx[1]]]
      }
      
      if (is.null(atk)) atk <- attacks[[1]]
      
      roll_obj <- roll_attack_d20(adv = "Normal")
      attack_roll <- as.integer(roll_obj$roll)
      attack_bonus <- as.integer(atk$attack_bonus %||% 0L)
      attack_total <- as.integer(attack_roll + attack_bonus)
      target_ac <- get_effective_actor_ac(target_id, target_type, target_char)
      
      is_crit <- identical(attack_roll, 20L)
      is_hit <- is_crit || (attack_total >= target_ac)
      
      damage_parts <- list()
      if (isTRUE(is_hit)) {
        dmg_expr <- as.character(atk$damage_expr %||% "1d4")
        dmg_type <- as.character(atk$damage_type %||% "bludgeoning")
        dr <- roll_dice_expr(dmg_expr)
        
        damage_parts <- list(list(
          source = as.character(atk$name %||% "Enemy Attack"),
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
        
        weapon_id = as.character(atk$id %||% "default"),
        weapon_name = as.character(atk$name %||% "Natural / Simple Attack"),
        
        attack_roll = attack_roll,
        attack_rolls = as.integer(roll_obj$rolls),
        attack_bonus = as.integer(attack_bonus),
        attack_total = as.integer(attack_total),
        target_ac = as.integer(target_ac),
        is_hit = isTRUE(is_hit),
        is_crit = isTRUE(is_crit),
        
        base_parts = damage_parts,
        sneak_available = FALSE,
        sneak_part = NULL,
        target_traits = get_character_damage_traits(target_char),
        primary_damage_type = as.character(atk$damage_type %||% "")
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
        if (identical(use_type, "untyped")) use_type <- ""
        
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
    # Spawn helper
    # --------------------------------------------------
    spawn_encounter_enemy <- function(
    encounter_id,
    name = "Enemy",
    hp_max = 10L,
    ac = 12L,
    movement_speed = 30L,
    x = 5L,
    y = 5L,
    attack_name = "Natural / Simple Attack",
    attack_bonus = 2L,
    damage_expr = "1d6",
    damage_type = "slashing",
    attacks_json = NULL, template_key = NULL, enemy_type = "Custom", characteristics = list(),
    abilities = list(), attacks = list(), loot = list(), resistances = character(), immunities = character(),
    vulnerabilities = character(), condition_immunities = character(), gold_min = 0L, gold_max = 0L
    ) {
      attack_bonus <- suppressWarnings(as.integer(attack_bonus))
      if (is.na(attack_bonus)) attack_bonus <- 0L
      
      enemy_uuid <- tryCatch(
        add_encounter_enemy(
          encounter_id = encounter_id,
          name = name,
          hp_max = hp_max,
          ac = ac,
          movement_speed = movement_speed,
          attack_bonus = attack_bonus,
          damage_expr = damage_expr,
          damage_type = damage_type, attacks_json = attacks_json, template_key = template_key,
          enemy_type = enemy_type, characteristics = characteristics, abilities = abilities, attacks = attacks, loot = loot,
          resistances = resistances, immunities = immunities, vulnerabilities = vulnerabilities,
          condition_immunities = condition_immunities, gold_min = gold_min, gold_max = gold_max
        ),
        error = function(e) {
          message("add_encounter_enemy failed: ", e$message)
          NULL
        }
      )
      
      if (is.null(enemy_uuid) || !nzchar(enemy_uuid)) return(NULL)
      
      ok_pos <- tryCatch(
        upsert_encounter_actor_position(
          encounter_id = encounter_id,
          actor_type = "enemy",
          actor_id = enemy_uuid,
          x = as.integer(x),
          y = as.integer(y)
        ),
        error = function(e) {
          message("upsert_encounter_actor_position failed: ", e$message)
          FALSE
        }
      )
      
      if (!isTRUE(ok_pos)) return(NULL)
      enemy_uuid
    }
    
    # --------------------------------------------------
    # Confirm attack UI
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
          lbl <- paste0(part$source %||% "Damage", if (nzchar(typ)) paste0(" (", typ, ")") else "")
          rhs <- if (identical(rule, "normal")) as.character(adj) else paste0(raw, " → ", adj, " [", rule, "]")
          div(class = "damage-part", span(lbl), span(rhs))
        })
      }
      
      tagList(
        div(
          class = "confirm-box",
          div(class = "live-combat-section", "Attack Preview"),
          div(
            class = "confirm-kv",
            div(class = "confirm-k", "Attacker"), div(preview$attacker_name),
            div(class = "confirm-k", "Target"), div(preview$target_name),
            div(class = "confirm-k", "Attack"), div(preview$weapon_name),
            div(class = "confirm-k", "To Hit"), div(paste0(
              paste(preview$attack_rolls, collapse = "/"),
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
              div(class = "live-combat-section", "Modify Damage"),
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
              div(class = "live-combat-section", "Target Defences"),
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
              div(class = "live-combat-section", "Damage Breakdown"),
              part_ui,
              tags$hr(),
              div(class = "damage-part", span(tags$strong("Raw total")), span(tags$strong(calc$raw_total))),
              div(class = "damage-part", span(tags$strong("Adjusted total")), span(tags$strong(calc$adjusted$total))),
              div(class = "confirm-note", "You can still override the final number before applying.")
            ),
            
            numericInput(
              session$ns("final_damage_override"),
              "Final damage to apply",
              value = calc$adjusted$total,
              min = 0,
              step = 1
            ),
            
            div(
              class = "live-combat-actions",
              actionButton(session$ns("apply_attack_final"), "Apply Result", class = "btn btn-danger"),
              actionButton(session$ns("cancel_attack"), "Cancel", class = "btn btn-default")
            )
          )
        } else {
          tagList(
            div(
              class = "confirm-box",
              div(class = "live-combat-section", "Damage"),
              div("Missed attacks default to 0 damage, but you can override if needed.")
            ),
            numericInput(
              session$ns("final_damage_override"),
              "Final damage to apply",
              value = 0,
              min = 0,
              step = 1
            ),
            div(
              class = "live-combat-actions",
              actionButton(session$ns("apply_attack_final"), "Apply Result", class = "btn btn-danger"),
              actionButton(session$ns("cancel_attack"), "Cancel", class = "btn btn-default")
            )
          )
        }
      )
    })
    
    observeEvent(input$cancel_attack, {
      pending_attack(NULL)
      removeModal()
    }, ignoreInit = TRUE)
    
    observeEvent(pending_attack(), {
      preview <- pending_attack()
      if (is.null(preview)) return()
      
      showModal(modalDialog(
        title = "Confirm Attack",
        uiOutput(session$ns("attack_confirm_ui")),
        footer = NULL,
        easyClose = TRUE,
        size = "m"
      ))
    })
    
    # --------------------------------------------------
    # Event formatting
    # --------------------------------------------------
    
    safe_event_payload <- function(x) {
      if (is.null(x)) return(NULL)
      
      if (is.list(x)) return(x)
      
      if (is.character(x) && length(x) == 1 && nzchar(x)) {
        out <- tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
        return(out)
      }
      
      NULL
    }
    
    format_event_text <- function(ev_row) {
      if (!is.data.frame(ev_row) || nrow(ev_row) == 0) return("No details.")
      
      typ <- as.character(ev_row$event_type[1] %||% "event")
      actor_id <- as.character(ev_row$actor_id[1] %||% "")
      target_id <- as.character(ev_row$target_id[1] %||% "")
      
      payload <- NULL
      if ("payload" %in% names(ev_row)) {
        payload <- safe_event_payload(ev_row$payload[[1]])
      }
      
      actor_name <- if (nzchar(actor_id)) get_actor_display_name(actor_id) else "Actor"
      target_name <- if (nzchar(target_id)) get_actor_display_name(target_id) else "Target"
      
      if (typ == "enter_combat") return("Combat begins.")
      
      if (typ == "spawn_enemy") {
        nm <- if (is.list(payload)) as.character(payload$name %||% "Enemy") else "Enemy"
        return(paste0(nm, " joins the battle."))
      }
      
      if (typ == "remove_enemy") return("Enemy removed from combat.")
      
      if (typ == "move") {
        if (is.list(payload) && !is.null(payload$to)) {
          tx <- payload$to$x %||% "?"
          ty <- payload$to$y %||% "?"
          return(paste0(actor_name, " moves to (", tx, ", ", ty, ")."))
        }
        return(paste0(actor_name, " moves."))
      }
      
      if (typ == "damage") {
        if (is.list(payload) && !is.null(payload$amount)) {
          amt <- payload$amount %||% "?"
          before <- payload$hp_before %||% "?"
          after <- payload$hp_after %||% "?"
          return(paste0(target_name, " takes ", amt, " damage (HP ", before, " → ", after, ")."))
        }
        return("Damage was dealt.")
      }
      
      if (typ == "attack") {
        if (is.list(payload)) {
          wpn <- payload$weapon_name %||% "Attack"
          
          trait_note <- ""
          parts <- payload$damage_parts %||% list()
          
          if (is.list(parts) && length(parts) > 0) {
            trait_rules <- unique(vapply(parts, function(p) {
              as.character(p$rule %||% "normal")
            }, character(1)))
            
            trait_rules <- setdiff(trait_rules, "normal")
            
            if (length(trait_rules) > 0) {
              trait_note <- paste0(" Damage adjusted: ", paste(trait_rules, collapse = ", "), ".")
            }
          }
          
          if (isTRUE(payload$is_hit %||% FALSE)) {
            dmg <- payload$final_damage %||% payload$damage_total %||% "?"
            extra <- if (isTRUE(payload$used_sneak_attack %||% FALSE)) " + Sneak Attack" else ""
            
            return(paste0(
              actor_name, " hits ", target_name,
              " with ", wpn, extra,
              " for ", dmg, " damage.",
              trait_note
            ))
          } else {
            atk <- payload$attack_total %||% "?"
            ac <- payload$target_ac %||% "?"
            return(paste0(actor_name, " misses ", target_name, " with ", wpn, " (", atk, " vs AC ", ac, ")."))
          }
        }
        
        return(paste0(actor_name, " attacks ", target_name, "."))
      }
      
      if (typ == "heal") {
        if (is.list(payload) && !is.null(payload$amount)) {
          amt <- payload$amount %||% "?"
          before <- payload$hp_before %||% "?"
          after <- payload$hp_after %||% "?"
          return(paste0(target_name, " heals ", amt, " HP (", before, " → ", after, ")."))
        }
        return("Healing was applied.")
      }
      
      if (typ == "end_turn") return("Turn advanced.")
      if (nzchar(actor_id) && nzchar(target_id)) return(paste(actor_name, "→", target_name))
      if (nzchar(actor_id)) return(paste("Actor:", actor_name))
      
      typ
    }
    
    parse_enemy_attacks <- function(row) {
      `%||%` <- get("%||%", inherits = TRUE)
      
      out <- list()
      
      # -----------------------------------------
      # Preferred: JSON column from DB
      # attacks_json example:
      # [
      #   {"name":"Bite","hit":5,"dmg":"1d8+3","type":"piercing"},
      #   {"name":"Claw","hit":5,"dmg":"1d6+2","type":"slashing"}
      # ]
      # -----------------------------------------
      attacks_json <- NULL
      if ("attacks_json" %in% names(row)) {
        attacks_json <- row$attacks_json[1] %||% NULL
      } else if ("attacks" %in% names(row)) {
        attacks_json <- row$attacks[1] %||% NULL
      }
      
      parsed <- NULL
      if (!is.null(attacks_json) && length(attacks_json) == 1) {
        if (is.list(attacks_json)) {
          parsed <- attacks_json
        } else if (is.character(attacks_json) && nzchar(attacks_json)) {
          parsed <- tryCatch(
            jsonlite::fromJSON(attacks_json, simplifyVector = FALSE),
            error = function(e) NULL
          )
        }
      }
      
      if (is.list(parsed) && length(parsed) > 0) {
        for (i in seq_along(parsed)) {
          a <- parsed[[i]]
          if (!is.list(a)) next
          
          nm  <- as.character(a$name %||% paste0("Attack ", i))
          hit <- suppressWarnings(as.integer(a$hit %||% a$attack_bonus %||% NA))
          dmg <- as.character(a$dmg %||% a$damage_expr %||% "")
          typ <- as.character(a$type %||% a$damage_type %||% "")
          
          if (is.na(hit)) hit <- 0L
          if (!nzchar(dmg)) next
          
          out[[length(out) + 1L]] <- list(
            id = paste0("atk_", i),
            name = nm,
            attack_bonus = hit,
            damage_expr = dmg,
            damage_type = typ
          )
        }
      }
      
      # -----------------------------------------
      # Fallback: single default attack
      # -----------------------------------------
      if (length(out) == 0) {
        out[[1]] <- list(
          id = "default",
          name = as.character(row$attack_name[1] %||% "Natural / Simple Attack"),
          attack_bonus = suppressWarnings(as.integer(row$attack_bonus[1] %||% 2L)),
          damage_expr = as.character(row$damage_expr[1] %||% "1d6"),
          damage_type = as.character(row$damage_type[1] %||% "slashing")
        )
      }
      
      out
    }
    
    # --------------------------------------------------
    # Header
    # --------------------------------------------------
    output$header_ui <- renderUI({
      enc <- encounter_tbl_r()
      combat <- combat_state_r()
      active <- active_actor_row()
      
      enc_name <- if (is.data.frame(enc) && nrow(enc) > 0) {
        as.character(enc$name[1] %||% paste("Encounter", current_encounter_id()))
      } else {
        paste("Encounter", current_encounter_id())
      }
      
      enc_status <- if (is.data.frame(enc) && nrow(enc) > 0) {
        as.character(enc$status[1] %||% "—")
      } else {
        "—"
      }
      
      round_txt <- if (is.data.frame(combat) && nrow(combat) > 0) {
        as.character(combat$round_number[1] %||% "—")
      } else {
        "—"
      }
      
      phase_txt <- if (is.data.frame(combat) && nrow(combat) > 0) {
        as.character(combat$phase[1] %||% "—")
      } else {
        "—"
      }
      
      active_name <- if (is.data.frame(active) && nrow(active) > 0) {
        as.character(active$display_name[1] %||% active$name[1] %||% "Unknown")
      } else {
        "No active actor"
      }
      
      tagList(
        div(class = "live-combat-title", "⚔️ Live Combat"),
        div(class = "live-combat-sub", paste(enc_name, "•", enc_status)),
        div(
          class = "live-combat-pills",
          div(class = "live-combat-pill", paste("Session", current_session_id() %||% "—")),
          div(class = "live-combat-pill", paste("Encounter", current_encounter_id() %||% "—")),
          div(class = "live-combat-pill", paste("Round", round_txt)),
          div(class = "live-combat-pill", paste("Phase", phase_txt)),
          div(class = "live-combat-pill", paste("Active", active_name)),
          div(class = "live-combat-pill", paste("Moved", turn_move_ft(), "ft"))
        )
      )
    })
    
    # --------------------------------------------------
    # Initiative UI
    # --------------------------------------------------
    
    output$initiative_ui <- renderUI({
      actors <- encounter_actors_r()
      aid <- active_actor_id()
      
      if (!is.data.frame(actors) || nrow(actors) == 0) {
        return(tagList(
          div(class = "live-combat-section", "Initiative"),
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
        nm <- as.character(row$display_name[1] %||% row$name[1] %||% "Unknown")
        turn_order <- as.character(row$turn_order[1] %||% "—")
        initiative <- as.character(row$initiative[1] %||% row$initiative_total[1] %||% "—")
        
        cur_hp  <- suppressWarnings(as.integer(row$current_hp[1] %||% row$hp_current[1] %||% 0))
        temp_hp <- suppressWarnings(as.integer(row$temp_hp[1] %||% 0))
        max_hp <- suppressWarnings(as.integer(row$hp_max[1] %||% row$max_hp[1] %||% NA))
        
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
          enemy_obj <- load_actor_for_combat(cid, "enemy")
          ac_val <- suppressWarnings(as.integer(enemy_obj$combat_profile$ac_override %||% NA))
          if (!is.na(ac_val)) paste0("AC ", ac_val) else "AC ?"
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
        div(class = "live-combat-section", "Initiative"),
        div(class = "initiative-list", rows)
      )
    })
    # --------------------------------------------------
    # Battlefield summary
    # --------------------------------------------------
    output$battlefield_ui <- renderUI({
      active <- active_actor_row()
      pos <- active_position_row()
      combat <- combat_state_r()
      enc <- encounter_tbl_r()
      
      encounter_name <- if (is.data.frame(enc) && nrow(enc) > 0) {
        as.character(enc$name[1] %||% "—")
      } else {
        "—"
      }
      
      active_name <- if (is.data.frame(active) && nrow(active) > 0) {
        as.character(active$display_name[1] %||% active$name[1] %||% "Unknown")
      } else {
        "No active actor"
      }
      
      actor_type <- if (is.data.frame(combat) && nrow(combat) > 0) {
        as.character(combat$active_actor_type[1] %||% "—")
      } else {
        "—"
      }
      
      pos_x <- if (is.data.frame(pos) && nrow(pos) > 0) as.character(pos$x[1] %||% "—") else "—"
      pos_y <- if (is.data.frame(pos) && nrow(pos) > 0) as.character(pos$y[1] %||% "—") else "—"
      
      target_name <- ""
      if (nzchar(as.character(input$target_id %||% ""))) {
        target_name <- get_actor_display_name(input$target_id)
      }
      
      tagList(
        div(class = "live-combat-section", "Battlefield"),
        div(
          class = "battlefield-kv",
          div(class = "battlefield-k", "Encounter"), div(encounter_name),
          div(class = "battlefield-k", "Actor"), div(active_name),
          div(class = "battlefield-k", "Actor Type"), div(actor_type),
          div(class = "battlefield-k", "Position"), div(paste0("(", pos_x, ", ", pos_y, ")")),
          div(class = "battlefield-k", "Target"), div(if (nzchar(target_name)) target_name else "—"),
          div(class = "battlefield-k", "Encounter ID"), div(as.character(current_encounter_id()))
        ),
        tags$hr(),
        div(
          class = "live-combat-sub",
          "This pass uses the encounter positions table as the runtime battlefield state."
        )
      )
    })
    
    
    output$map_ui <- renderUI({
      session$onFlushed(function() {
        map_ui_ready(TRUE)
        bump_map_visual()
      }, once = TRUE)
      
      tags$div(
        id = session$ns("combat_3d_shell"),
        class = "combat-2d-shell",
        
        tags$div(
          class = "combat-map-toolbar combat-2d-toolbar",
          actionButton(session$ns("map_3d_fullscreen"), "Fullscreen Map", class = "btn btn-default")
        ),
        
        tags$div(
          id = session$ns("combat_3d_canvas"),
          class = "combat-2d-canvas"
        )
      )
    })
    
    observe({
      req(map_ui_ready())
      req(!is.na(current_encounter_id()))
      req(!is.na(current_map_id()))
      map_visual_key()
      
      tiles_now <- map_tiles_r()
      if (!is.data.frame(tiles_now) || nrow(tiles_now) == 0) return()
      occ <- tryCatch(map_occupants_r(), error = function(e) empty_map_occupants())
      
      render_df <- tryCatch(
        build_map_render_df(tiles = tiles_now, occupants = occ, map_id = current_map_id()),
        error = function(e) {
          log_safe(paste("Map render failed:", conditionMessage(e)), type = "error")
          data.frame()
        }
      )
      if (!is.data.frame(render_df) || nrow(render_df) == 0) return()
      
     # render_df <- add_3d_models_to_render_df(render_df)
      
      actors_lookup <- encounter_actors_r()
      
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
      
      pm <- pending_move()
      if (!is.null(pm)) {
        render_df$is_pending_move <- render_df$x == as.integer(pm$x) & render_df$y == as.integer(pm$y)
      }
      
      active_id <- active_actor_id()
      render_df$is_active_actor <- !is.na(render_df$occupant_id) & as.character(render_df$occupant_id) == as.character(active_id %||% "")
      cat("\n[LIVE MAP INIT]\n")
      cat("eid:", current_encounter_id(), "\n")
      cat("mid:", current_map_id(), "\n")
      cat("tiles:", nrow(tiles_now), "\n")
      cat("render rows:", nrow(render_df), "\n")
      cat("container:", session$ns("combat_3d_canvas"), "\n")
      later::later(function() {
        session$sendCustomMessage(
          "combat3d-init",
          list(
            containerId = session$ns("combat_3d_canvas"),
            mapData = jsonlite::toJSON(render_df, dataframe = "rows", auto_unbox = TRUE, null = "null"),
            inputIds = list(move = session$ns("move_to_tile"), target = session$ns("map_target_click"))
          )
        )
      }, delay = 0.1)
    })
    
    # --------------------------------------------------
    # Combat log
    # --------------------------------------------------
    output$log_ui <- renderUI({
      ev <- encounter_events_r()
      
      if (!is.data.frame(ev) || nrow(ev) == 0) {
        return(tagList(
          div(class = "live-combat-section", "Combat Log"),
          div("No recent events.")
        ))
      }
      
      if ("created_at" %in% names(ev)) {
        ord <- order(ev$created_at, decreasing = TRUE, na.last = TRUE)
        ev <- ev[ord, , drop = FALSE]
      }
      
      items <- lapply(seq_len(nrow(ev)), function(i) {
        row <- ev[i, , drop = FALSE]
        
        typ <- as.character(row$event_type[1] %||% "event")
        body <- format_event_text(row)
        ts <- if ("created_at" %in% names(row)) as.character(row$created_at[1] %||% "") else ""
        
        div(
          class = "combat-log-item",
          div(class = "combat-log-type", typ),
          div(class = "combat-log-body", body),
          if (nzchar(ts)) div(class = "combat-log-time", ts)
        )
      })
      
      tagList(
        div(class = "live-combat-section", "Combat Log"),
        div(class = "combat-log", items)
      )
    })
    
    # --------------------------------------------------
    # Target dropdown
    # --------------------------------------------------
    observe({
      actors <- encounter_actors_r()
      attacker_id <- active_actor_id()
      
      choices <- c()
      
      if (is.data.frame(actors) && nrow(actors) > 0) {
        target_rows <- actors[
          as.character(actors$actor_id) != as.character(attacker_id %||% ""),
          ,
          drop = FALSE
        ]
        
        if (nrow(target_rows) > 0) {
          labels <- vapply(seq_len(nrow(target_rows)), function(i) {
            row <- target_rows[i, , drop = FALSE]
            nm <- as.character(row$display_name[1] %||% row$name[1] %||% "Unknown")
            tp <- as.character(row$actor_type[1] %||% "actor")
            paste0(nm, " (", tp, ")")
          }, character(1))
          
          ids <- as.character(target_rows$actor_id %||% "")
          choices <- stats::setNames(ids, labels)
        }
      }
      
      current_target <- as.character(input$target_id %||% "")
      
      choice_values <- unname(choices)
      
      selected_target <- if (nzchar(current_target) && current_target %in% choice_values) {
        current_target
      } else if (length(choice_values) > 0) {
        choice_values[1]
      } else {
        character(0)
      }
      
      updateSelectInput(session, "target_id", choices = choices, selected = selected_target)
    })
    
    # --------------------------------------------------
    # Reinforcement enemy dropdown
    # --------------------------------------------------
    observe({
      enemies <- encounter_enemies_r()
      
      choices <- c()
      if (is.data.frame(enemies) && nrow(enemies) > 0) {
        ids <- as.character(enemies$enemy_uuid %||% enemies$actor_id %||% "")
        labels <- paste0(
          as.character(enemies$name %||% enemies$display_name %||% "Enemy"),
          " • HP ",
          as.integer(enemies$hp_current %||% 0),
          "/",
          as.integer(enemies$hp_max %||% 0)
        )
        choices <- stats::setNames(ids, labels)
      }
      
      current <- as.character(input$selected_enemy_id %||% "")
      choice_values <- unname(choices)
      
      selected <- if (nzchar(current) && current %in% choice_values) {
        current
      } else if (length(choice_values)) {
        choice_values[1]
      } else {
        character(0)
      }
      updateSelectInput(session, "selected_enemy_id", choices = choices, selected = selected)
    })
    
    # --------------------------------------------------
    # Refresh / bind / combat flow
    # --------------------------------------------------
    observeEvent(input$refresh, {
      bump_live()
    }, ignoreInit = TRUE)
    
    observeEvent(input$bind_encounter, {
      eid <- current_encounter_id()
      
      if (is.na(eid)) {
        log_safe("Choose an encounter first.", type = "error")
        return()
      }
      
      enc <- tryCatch(get_encounter(eid), error = function(e) data.frame())
      sid <- NA_integer_
      
      if (is.data.frame(enc) && nrow(enc) > 0) {
        sid <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
      }
      
      ctrl$encounter_id <- eid
      ctrl$active_encounter_id <- eid
      
      if (!is.na(sid) && sid > 0L) {
        ctrl$session_id <- sid
        ctrl$active_session_id <- sid

        if (!isTRUE(tryCatch(set_active_encounter(sid, eid), error = function(e) FALSE))) {
          log_safe("Could not set the session's active encounter.", type = "error")
          return()
        }
      }
      
      bump_live()
      log_safe(paste0("Encounter ", eid, " is now active for all players."))
    }, ignoreInit = TRUE)
    
    output$live_debug <- renderPrint({
      list(
        ctrl_session_id = ctrl$session_id %||% NULL,
        ctrl_encounter_id = ctrl$encounter_id %||% NULL,
        current_map_id = current_map_id(),
        encounter_rows = nrow(encounter_tbl_r()),
        positions = encounter_positions_r()
      )
    })
    
    observeEvent(input$start_combat, {
      eid <- current_encounter_id()
      
      cat("\n================ START COMBAT CLICK ================\n")
      cat("current encounter_id:", eid, "\n")
      
      if (is.na(eid)) {
        log_safe("Choose an encounter first.", type = "error")
        cat("[start_combat] encounter_id is NA\n")
        return()
      }

      enc <- tryCatch(get_encounter(eid), error = function(e) data.frame())
      sid <- if (is.data.frame(enc) && nrow(enc) > 0) {
        suppressWarnings(as.integer(enc$session_id[1] %||% NA))
      } else {
        NA_integer_
      }

      if (is.na(sid) || sid < 1L ||
          !isTRUE(tryCatch(set_active_encounter(sid, eid), error = function(e) FALSE))) {
        log_safe("Could not make this encounter active for the session.", type = "error")
        return()
      }

      try(set_encounter_status(eid, status = "active"), silent = TRUE)
      
      known_ac_rv(data.frame(
        target_id = character(),
        lower = integer(),
        upper = integer(),
        stringsAsFactors = FALSE
      ))
      
      actors_before <- tryCatch(get_encounter_actors(eid), error = function(e) {
        cat("[start_combat] get_encounter_actors failed:", e$message, "\n")
        data.frame()
      })
      
      cat("[start_combat] encounter actors before start:\n")
      print(actors_before)
      
      init_df <- tryCatch(
        start_encounter_combat(
          encounter_id = eid,
          core_state = NULL
        ),
        error = function(e) {
          cat("[start_combat] start_encounter_combat failed:", e$message, "\n")
          data.frame()
        }
      )
      
      cat("[start_combat] returned init_df:\n")
      print(init_df)
      
      combat_after <- tryCatch(get_combat_state(eid), error = function(e) {
        cat("[start_combat] get_combat_state after start failed:", e$message, "\n")
        data.frame()
      })
      
      cat("[start_combat] combat_state after start:\n")
      print(combat_after)
      
      if (!is.data.frame(init_df) || nrow(init_df) == 0) {
        log_safe("Could not start combat.", type = "error")
        cat("[start_combat] init_df empty\n")
        return()
      }
      
      turn_move_ft(0L)
      ctrl$active_encounter_id <- eid
      
      first_name <- as.character(init_df$display_name[1] %||% "Unknown")
      log_safe(paste0("Combat started and initiative rolled. ", first_name, " acts first."))
      
      cat("====================================================\n")
      
      bump_live()
    }, ignoreInit = TRUE)
    
    observeEvent(input$end_turn, {
      eid <- current_encounter_id()
      
      if (is.na(eid)) {
        log_safe("Choose an encounter first.", type = "error")
        return()
      }
      
      ok <- tryCatch(advance_turn(eid), error = function(e) FALSE)
      turn_move_ft(0L)
      
      if (isTRUE(ok)) {
        log_safe("Turn advanced.")
      } else {
        log_safe("Could not advance turn.", type = "error")
      }
      
      bump_live()
    }, ignoreInit = TRUE)

    observeEvent(input$end_combat, {
      showModal(modalDialog(
        title = "End combat?",
        p("This closes the active encounter for every player and returns the session to exploration."),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_end_combat"), "End Combat", class = "btn btn-danger"))
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_end_combat, {
      eid <- current_encounter_id()
      removeModal()
      if (is.na(eid) || !isTRUE(end_encounter_combat(eid))) {
        log_safe("Could not end combat.", type = "error")
        return()
      }
      log_safe("Combat ended. Players returned to exploration.")
      bump_live()
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------
    # Reinforcements
    # --------------------------------------------------
    observeEvent(input$add_reinforcement, {
      eid <- current_encounter_id()
      if (is.na(eid)) {
        log_safe("Choose an encounter first.", type = "error")
        return()
      }
      
      npc_id <- as.character(input$reinforce_npc_template_id %||% "")
      if (!nzchar(npc_id)) {
        log_safe("Choose an NPC template first.", type = "error")
        return()
      }
      
      npc <- get_npc_template(npc_id)
      if (!is.data.frame(npc) || nrow(npc) == 0) {
        log_safe("NPC template not found.", type = "error")
        return()
      }
      
      count <- suppressWarnings(as.integer(input$reinforce_count %||% 1L))
      x <- suppressWarnings(as.integer(input$reinforce_x %||% 5L))
      y <- suppressWarnings(as.integer(input$reinforce_y %||% 5L))
      
      if (is.na(count) || count < 1L) count <- 1L
      if (is.na(x) || x < 1L) x <- 5L
      if (is.na(y) || y < 1L) y <- 5L
      
      added_ids <- character()
      base_name <- as.character(npc$name[1] %||% "Enemy")
      
      for (i in seq_len(count)) {
        enemy_name <- if (count > 1L) paste0(base_name, " ", i) else base_name
        
        new_enemy_id <- tryCatch(
          spawn_encounter_enemy(
            encounter_id = eid,
            name = enemy_name,
            hp_max = as.integer(npc$hp_max[1] %||% 10L),
            ac = as.integer(npc$ac[1] %||% 12L),
            movement_speed = as.integer(npc$movement_speed[1] %||% 30L),
            x = x + i - 1L,
            y = y,
            attack_name = as.character(npc$attack_name[1] %||% "Attack"),
            attack_bonus = as.integer(npc$attack_bonus[1] %||% 0L),
            damage_expr = as.character(npc$damage_expr[1] %||% "1d4"),
            damage_type = as.character(npc$damage_type[1] %||% "bludgeoning"),
            attacks_json = as.character(npc$attacks_json[1] %||% ""), template_key = npc_id,
            enemy_type = as.character(npc$enemy_type[1] %||% "Custom"), characteristics = enemy_db_json(npc$characteristics[[1]],list()),
            abilities = enemy_db_json(npc$abilities[[1]],list()), attacks = enemy_db_json(npc$attacks[[1]],list()), loot = enemy_db_json(npc$loot[[1]],list()),
            resistances = enemy_db_values(npc$resistances[[1]]), immunities = enemy_db_values(npc$immunities[[1]]),
            vulnerabilities = enemy_db_values(npc$vulnerabilities[[1]]), condition_immunities = enemy_db_values(npc$condition_immunities[[1]]),
            gold_min = as.integer(npc$gold_min[1] %||% 0L), gold_max = as.integer(npc$gold_max[1] %||% 0L)
          ),
          error = function(e) {
            message("spawn_encounter_enemy from template failed: ", e$message)
            NULL
          }
        )
        
        if (!is.null(new_enemy_id) && nzchar(new_enemy_id)) {
          added_ids <- c(added_ids, new_enemy_id)
          
          try(
            log_game_event(
              encounter_id = eid,
              event_type = "spawn_enemy",
              actor_type = "control",
              actor_id = new_enemy_id,
              payload = list(
                name = enemy_name,
                template_id = npc_id,
                hp = as.integer(npc$hp_max[1] %||% 10L),
                ac = as.integer(npc$ac[1] %||% 12L),
                x = x + i - 1L,
                y = y
              )
            ),
            silent = TRUE
          )
        }
      }
      
      if (!length(added_ids)) {
        log_safe("Failed to add reinforcement.", type = "error")
        return()
      }
      
      combat_now <- combat_state_r()
      if (is.data.frame(combat_now) && nrow(combat_now) > 0) {
        try(
          roll_encounter_initiative(
            encounter_id = eid,
            core_state = NULL
          ),
          silent = TRUE
        )
      }
      
      log_safe(paste0("Added ", length(added_ids), " reinforcement(s): ", base_name, "."))
      bump_live()
    }, ignoreInit = TRUE)
    
    
    
    
    observeEvent(input$remove_selected_enemy, {
      eid <- current_encounter_id()
      enemy_id <- as.character(input$selected_enemy_id %||% "")
      
      if (is.na(eid) || !nzchar(enemy_id)) {
        log_safe("Choose an enemy first.", type = "error")
        return()
      }
      
      ok_enemy <- tryCatch(remove_encounter_enemy(enemy_id), error = function(e) FALSE)
      
      ok_pos <- TRUE
      if (exists("remove_encounter_actor_position", mode = "function")) {
        ok_pos <- tryCatch(
          remove_encounter_actor_position(
            encounter_id = eid,
            actor_type = "enemy",
            actor_id = enemy_id
          ),
          error = function(e) FALSE
        )
      }
      
      if (!isTRUE(ok_enemy) || !isTRUE(ok_pos)) {
        log_safe("Failed to remove enemy.", type = "error")
        return()
      }
      
      try(
        log_game_event(
          encounter_id = eid,
          event_type = "remove_enemy",
          actor_type = "control",
          actor_id = enemy_id,
          payload = list(reason = "removed_live")
        ),
        silent = TRUE
      )
      
      log_safe("Enemy removed from combat.")
      bump_live()
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------
    # Movement
    # --------------------------------------------------
    move_active_actor <- function(dx = 0L, dy = 0L) {
      steps <- suppressWarnings(as.integer(input$move_steps %||% 1L))
      if (is.na(steps) || steps < 1L) steps <- 1L
      steps <- min(3L, steps)
      
      eid <- current_encounter_id()
      combat <- combat_state_r()
      mid <- current_map_id()
      
      if (is.na(eid) || !is.data.frame(combat) || nrow(combat) == 0 || is.na(combat$active_actor_id[1])) {
        log_safe("No active combat actor.", type = "error")
        return(invisible(FALSE))
      }
      
      actor_id <- as.character(combat$active_actor_id[1] %||% "")
      actor_type <- as.character(combat$active_actor_type[1] %||% "player")
      
      pos_tbl <- encounter_positions_r()
      pos <- pos_tbl[as.character(pos_tbl$actor_id) == actor_id, , drop = FALSE]
      
      if (!is.data.frame(pos) || nrow(pos) == 0) {
        log_safe("Active actor has no position.", type = "error")
        return(invisible(FALSE))
      }
      
      old_x <- as.integer(pos$x[1] %||% 0)
      old_y <- as.integer(pos$y[1] %||% 0)
      
      cur_x <- old_x
      cur_y <- old_y
      total_ft <- 0L
      
      tiles <- map_tiles_r()
      occ <- map_occupants_r()
      
      speed_ft <- get_actor_speed_ft(actor_id, actor_type)
      
      for (step_i in seq_len(steps)) {
        next_x <- cur_x + as.integer(dx)
        next_y <- cur_y + as.integer(dy)
        
        move_check <- can_enter_tile(
          tiles = tiles,
          occupants = occ,
          x = next_x,
          y = next_y,
          map_id = mid,
          exclude_actor_id = actor_id
        )
        
        if (!isTRUE(move_check$ok)) break
        
        step_mult <- if (abs(as.integer(dx)) == 1L && abs(as.integer(dy)) == 1L) 2L else 1L
        step_ft <- as.integer(round((move_check$move_cost %||% 1) * 5 * step_mult))
        
        projected_ft <- as.integer(turn_move_ft() + total_ft + step_ft)
        
        if (!is.na(speed_ft) && projected_ft > speed_ft) break
        
        total_ft <- total_ft + step_ft
        cur_x <- next_x
        cur_y <- next_y
      }
      
      if (identical(cur_x, old_x) && identical(cur_y, old_y)) {
        log_safe("Cannot move there, or move exceeds speed.", type = "error")
        return(invisible(FALSE))
      }
      
      ok1 <- tryCatch(
        upsert_encounter_actor_position(
          encounter_id = eid,
          actor_type = actor_type,
          actor_id = actor_id,
          x = cur_x,
          y = cur_y
        ),
        error = function(e) FALSE
      )
      
      if (!isTRUE(ok1)) {
        log_safe("Could not move active actor.", type = "error")
        return(invisible(FALSE))
      }

      if (identical(actor_type,"enemy") && !isTRUE(input$movement_disengage)) {
        attackers<-tryCatch(get_opportunity_attackers(eid,actor_id,actor_type,old_x,old_y,cur_x,cur_y),error=function(e)data.frame())
        if(is.data.frame(attackers)&&nrow(attackers)) log_game_event(eid,"opportunity_available",actor_type,actor_id,payload=list(attacker_ids=as.list(as.character(attackers$actor_id)),attacker_names=as.list(as.character(attackers$display_name%||%attackers$name%||%"Player"))))
      }
      
      try(
        log_game_event(
          encounter_id = eid,
          event_type = "move",
          actor_type = actor_type,
          actor_id = actor_id,
          payload = list(
            from = list(x = old_x, y = old_y),
            to = list(x = cur_x, y = cur_y),
            steps_requested = steps,
            move_cost_ft = total_ft
          )
        ),
        silent = TRUE
      )
      
      turn_move_ft(as.integer(turn_move_ft() + total_ft))
      if(isTRUE(input$movement_disengage))updateCheckboxInput(session,"movement_disengage",value=FALSE)
      
      log_safe(paste0("Moved to (", cur_x, ", ", cur_y, "). Cost: ", total_ft, " ft."))
      
      bump_positions()
      bump_events()
      bump_map_visual()
      
      invisible(TRUE)
    }
    
    observeEvent(input$move_w, { move_active_actor(dx = -1L, dy = 0L) }, ignoreInit = TRUE)
    observeEvent(input$move_e, { move_active_actor(dx = 1L, dy = 0L) }, ignoreInit = TRUE)
    observeEvent(input$move_n, { move_active_actor(dx = 0L, dy = -1L) }, ignoreInit = TRUE)
    observeEvent(input$move_s, { move_active_actor(dx = 0L, dy = 1L) }, ignoreInit = TRUE)
    observeEvent(input$move_nw, { move_active_actor(-1L, -1L) }, ignoreInit = TRUE)
    observeEvent(input$move_ne, { move_active_actor( 1L, -1L) }, ignoreInit = TRUE)
    observeEvent(input$move_sw, { move_active_actor(-1L,  1L) }, ignoreInit = TRUE)
    observeEvent(input$move_se, { move_active_actor( 1L,  1L) }, ignoreInit = TRUE)
    
    observeEvent(input$map_target_click, {
      actor_id <- as.character(input$map_target_click$actor_id %||% "")
      if (!nzchar(actor_id)) return()
      if (identical(actor_id, as.character(active_actor_id() %||% ""))) return()
      updateSelectInput(session, "target_id", selected = actor_id)
      log_safe(paste0("Target selected: ", get_actor_display_name(actor_id)))
    }, ignoreInit = TRUE)
    
    observeEvent(input$move_to_tile, {
      target_x <- suppressWarnings(as.integer(input$move_to_tile$x %||% NA))
      target_y <- suppressWarnings(as.integer(input$move_to_tile$y %||% NA))
      if (is.na(target_x) || is.na(target_y)) return()
      pos <- active_position_row()
      if (!is.data.frame(pos) || nrow(pos) == 0) {
        log_safe("Active actor has no position.", type = "error")
        return()
      }
      old_x <- suppressWarnings(as.integer(pos$x[1] %||% NA))
      old_y <- suppressWarnings(as.integer(pos$y[1] %||% NA))
      if (is.na(old_x) || is.na(old_y)) return()
      dx_total <- target_x - old_x
      dy_total <- target_y - old_y
      steps <- max(abs(dx_total), abs(dy_total))
      if (steps < 1L) return()
      if (steps > 3L) {
        log_safe("Use the 1x/2x/3x movement controls or click a nearer tile.", type = "error")
        return()
      }
      if (!(dx_total == 0L || dy_total == 0L || abs(dx_total) == abs(dy_total))) {
        log_safe("Click movement currently supports straight or diagonal movement only.", type = "error")
        return()
      }
      pending_move(list(x = target_x, y = target_y))
      updateRadioButtons(session, "move_steps", selected = steps)
      ok <- move_active_actor(dx = sign(dx_total), dy = sign(dy_total))
      pending_move(NULL)
      bump_map_visual()
      invisible(ok)
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------
    # Attack flow
    # --------------------------------------------------
    observe({
      active <- active_actor_row()
      
      lbl <- "Attack"
      if (is.data.frame(active) && nrow(active) > 0) {
        nm <- as.character(active$display_name[1] %||% active$name[1] %||% "Actor")
        lbl <- paste0("Attack as ", nm)
      }
      
      updateActionButton(session, "attack_btn", label = lbl)
    })
    
    observeEvent(input$attack_btn, {
      attacker_id <- active_actor_id()
      attacker_type <- active_actor_type()
      target_id <- as.character(input$target_id %||% "")
      
      if (is.null(attacker_id) || !nzchar(as.character(attacker_id))) {
        log_safe("No active attacker.", type = "error")
        return()
      }
      
      if (!nzchar(target_id)) {
        log_safe("Choose a target first.", type = "error")
        return()
      }
      
      attacker_name <- get_actor_display_name(attacker_id)
      if (!nzchar(attacker_name)) attacker_name <- "Attacker"
      
      if (identical(attacker_type, "player")) {
        attacker_char <- load_actor_for_combat(attacker_id, attacker_type)
        if (is.null(attacker_char)) {
          log_safe("Could not load attacker.", type = "error")
          return()
        }
        
        weapons <- get_equipped_weapons_for_combat(attacker_char)
        if (!is.data.frame(weapons) || nrow(weapons) == 0) {
          log_safe("No equipped weapons available.", type = "error")
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
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_attack"), "Roll Attack", class = "btn btn-danger")
          ),
          easyClose = TRUE
        ))
        
      } else if (identical(attacker_type, "enemy")) {
        attacker_char <- load_actor_for_combat(attacker_id, attacker_type)
        
        if (is.null(attacker_char)) {
          log_safe("Could not load attacker.", type = "error")
          return()
        }
        
        attacks <- attacker_char$combat_profile$attacks %||% list()
        if (!is.list(attacks) || length(attacks) == 0) {
          attacks <- list(list(
            id = "default",
            name = "Natural / Simple Attack",
            attack_bonus = as.integer(attacker_char$combat_profile$attack_bonus %||% 0L),
            damage_expr = as.character(attacker_char$combat_profile$damage_expr %||% "1d4"),
            damage_type = as.character(attacker_char$combat_profile$damage_type %||% "bludgeoning")
          ))
        }
        
        attack_choices <- stats::setNames(
          vapply(attacks, function(a) as.character(a$id %||% "default"), character(1)),
          vapply(attacks, function(a) {
            paste0(
              as.character(a$name %||% "Attack"),
              " • Hit ",
              ifelse(as.integer(a$attack_bonus %||% 0L) >= 0, "+", ""),
              as.integer(a$attack_bonus %||% 0L),
              " • ",
              as.character(a$damage_expr %||% "1d4"),
              if (nzchar(as.character(a$damage_type %||% ""))) {
                paste0(" ", as.character(a$damage_type %||% ""))
              } else {
                ""
              }
            )
          }, character(1))
        )
        
        showModal(modalDialog(
          title = paste0("Choose enemy attack — ", attacker_name),
          radioButtons(session$ns("enemy_attack_id"), "Available attacks", choices = attack_choices),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_enemy_attack"), "Roll Attack", class = "btn btn-danger")
          ),
          easyClose = TRUE
        ))
        
      } else {
        log_safe("Unsupported attacker type.", type = "error")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$confirm_attack, {
      removeModal()
      
      attacker_id <- active_actor_id()
      target_id <- as.character(input$target_id %||% "")
      weapon_id <- as.character(input$attack_weapon_id %||% "")
      
      if (is.null(attacker_id) || !nzchar(as.character(attacker_id))) {
        log_safe("No active attacker.", type = "error")
        return()
      }
      
      if (!nzchar(target_id)) {
        log_safe("No target selected.", type = "error")
        return()
      }
      
      if (!nzchar(weapon_id)) {
        log_safe("No weapon selected.", type = "error")
        return()
      }
      
      target_type <- get_actor_type_by_id(target_id)
      attacker_char <- load_actor_for_combat(attacker_id, "player")
      target_char <- load_actor_for_combat(target_id, target_type)
      
      if (is.null(attacker_char) || is.null(target_char)) {
        log_safe("Could not load combatants.", type = "error")
        return()
      }
      
      weapons <- get_equipped_weapons_for_combat(attacker_char)
      weapon_row <- weapons[as.character(weapons$id) == weapon_id, , drop = FALSE]
      
      if (!is.data.frame(weapon_row) || nrow(weapon_row) == 0) {
        log_safe("Could not find selected weapon.", type = "error")
        return()
      }
      
      attacker_name <- get_actor_display_name(attacker_id)
      target_name <- get_actor_display_name(target_id)
      
      preview <- build_attack_preview(
        attacker_char = attacker_char,
        target_char = target_char,
        weapon_row = weapon_row,
        attacker_name = attacker_name,
        target_name = target_name,
        attacker_id = attacker_id,
        target_id = target_id
      )
      
      pending_attack(preview)
    }, ignoreInit = TRUE)
    
    
    observeEvent(input$confirm_enemy_attack, {
      removeModal()
      
      attacker_id <- active_actor_id()
      target_id <- as.character(input$target_id %||% "")
      chosen_attack <- as.character(input$enemy_attack_id %||% "")
      
      if (is.null(attacker_id) || !nzchar(as.character(attacker_id))) {
        log_safe("No active attacker.", type = "error")
        return()
      }
      
      if (!nzchar(target_id)) {
        log_safe("No target selected.", type = "error")
        return()
      }
      
      target_type <- get_actor_type_by_id(target_id)
      attacker_char <- load_actor_for_combat(attacker_id, "enemy")
      target_char <- load_actor_for_combat(target_id, target_type)
      
      if (is.null(attacker_char) || is.null(target_char)) {
        log_safe("Could not load combatants.", type = "error")
        return()
      }
      
      preview <- build_enemy_attack_preview(
        attacker_char = attacker_char,
        target_char = target_char,
        attacker_name = get_actor_display_name(attacker_id),
        target_name = get_actor_display_name(target_id),
        attacker_id = attacker_id,
        target_id = target_id,
        attacker_type = "enemy",
        target_type = target_type,
        chosen_attack = chosen_attack
      )
      
      pending_attack(preview)
    }, ignoreInit = TRUE)
    
    observeEvent(input$apply_attack_final, {
      preview <- pending_attack()
      if (is.null(preview)) return()
      
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
      
      trait_note <- ""
      parts <- calc$adjusted$parts %||% list()
      
      trait_rules <- unique(vapply(parts, function(p) {
        as.character(p$rule %||% "normal")
      }, character(1)))
      
      trait_rules <- setdiff(trait_rules, "normal")
      
      if (length(trait_rules) > 0) {
        trait_note <- paste0(" Damage adjusted: ", paste(trait_rules, collapse = ", "), ".")
      }
      
      eid <- current_encounter_id()
      
      update_known_ac(
        target_id = preview$target_id,
        attack_total = preview$attack_total,
        is_hit = preview$is_hit
      )
      
      try(
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
        ),
        silent = TRUE
      )
      
      if (final_damage > 0L) {
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
          try(
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
            ),
            silent = TRUE
          )
        } else {
          log_safe("Attack landed, but damage could not be applied.", type = "error")
        }
      }
      
      ac_hint <- if (identical(preview$target_type, "enemy")) {
        format_known_ac(preview$target_id)
      } else {
        paste0("AC ", preview$target_ac)
      }
      
      if (isTRUE(preview$is_hit)) {
        log_safe(paste0(
          preview$attacker_name, " hits ", preview$target_name,
          " with ", preview$weapon_name,
          if (isTRUE(apply_sneak)) " + Sneak Attack" else "",
          " for ", final_damage, " damage.",
          trait_note,
          " (", preview$attack_total, " vs ", ac_hint, ")"
        ))
      } else {
        log_safe(paste0(
          preview$attacker_name, " misses ", preview$target_name,
          " with ", preview$weapon_name,
          " (", preview$attack_total, " vs ", ac_hint, ")."
        ))
      }
      
      pending_attack(NULL)
      removeModal()
      bump_live()
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------
    # Damage test
    # --------------------------------------------------
    observeEvent(input$damage_test, {
      eid <- current_encounter_id()
      combat <- combat_state_r()
      
      if (is.na(eid) || !is.data.frame(combat) || nrow(combat) == 0 || is.na(combat$active_actor_id[1])) {
        log_safe("No active combat actor.", type = "error")
        return()
      }
      
      actor_id <- as.character(combat$active_actor_id[1] %||% "")
      actor_type <- as.character(combat$active_actor_type[1] %||% "player")
      
      res <- if (identical(actor_type, "player")) {
        damage_player_in_encounter(
          encounter_id = eid,
          character_id = actor_id,
          amount = 2
        )
      } else {
        tryCatch(
          damage_encounter_enemy(
            encounter_id = eid,
            enemy_uuid = actor_id,
            amount = 2
          ),
          error = function(e) NULL
        )
      }
      
      if (is.null(res)) {
        log_safe("Could not damage active actor.", type = "error")
        return()
      }
      
      try(
        log_game_event(
          encounter_id = eid,
          event_type = "damage",
          actor_type = "control",
          target_id = actor_id,
          payload = list(
            amount = res$amount %||% 2,
            hp_before = res$hp_before,
            hp_after = res$hp_after,
            temp_before = res$temp_before,
            temp_after = res$temp_after
          )
        ),
        silent = TRUE
      )
      
      log_safe(paste0(
        "Damaged active actor for 2 HP (",
        res$hp_before, " → ", res$hp_after, ")."
      ))
      
      bump_live()
    }, ignoreInit = TRUE)
  })
}
