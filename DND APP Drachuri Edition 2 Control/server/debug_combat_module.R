library(shiny)

debugCombatUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Combat",
    value = "debug_combat",
    
    tagList(
      tags$style(HTML("
        .combat-wrap{
          display:flex;
          flex-direction:column;
          gap:12px;
          min-width:0;
        }

        .combat-card{
          border:1px solid rgba(191,167,111,0.6);
          background:rgba(255,255,245,0.92);
          border-radius:14px;
          padding:14px;
          box-shadow:0 8px 22px rgba(0,0,0,0.12);
          min-width:0;
          overflow:hidden;
        }

        .combat-title{
          font-size:20px;
          font-weight:900;
          margin-bottom:4px;
        }

        .combat-sub{
          font-size:12px;
          opacity:0.8;
        }

        .combat-section-title{
          font-size:15px;
          font-weight:900;
          margin-bottom:8px;
        }

        .combat-pills{
          display:flex;
          gap:8px;
          flex-wrap:wrap;
          margin-top:10px;
        }

        .combat-pill{
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

        .combat-top-grid{
          display:grid;
          grid-template-columns: minmax(0, 1fr) minmax(260px, 320px);
          gap:12px;
          margin-top:12px;
          align-items:start;
        }

        .combat-top-left{
          display:flex;
          flex-direction:column;
          gap:12px;
          min-width:0;
        }

        .combat-control-row{
          display:flex;
          flex-wrap:wrap;
          gap:8px;
          align-items:flex-end;
          min-width:0;
        }

        .combat-control-row .form-group{
          margin-bottom:0;
        }

        .combat-control-row .btn{
          border-radius:10px;
          white-space:normal;
        }

        .combat-control-row .shiny-input-container{
          min-width:120px;
        }

        .combat-action-buttons{
          display:flex;
          flex-wrap:wrap;
          gap:8px;
          min-width:0;
        }

        .combat-action-buttons .btn{
          border-radius:10px;
          min-width:140px;
          flex:0 1 auto;
        }

        .combat-lower-actions{
          display:grid;
          grid-template-columns: auto minmax(0, 1fr);
          gap:16px;
          align-items:start;
        }

        .combat-movement-block{
          display:flex;
          flex-direction:column;
          gap:6px;
          min-width:max-content;
        }

        .combat-move-grid{
          display:grid;
          grid-template-columns:repeat(3, 44px);
          gap:6px;
          width:max-content;
        }

        .combat-turn-box{
          min-width:0;
          display:flex;
          align-items:flex-start;
          flex-wrap:wrap;
          gap:8px;
        }

        .combat-grid{
          display:grid;
          grid-template-columns: minmax(260px, 0.9fr) minmax(320px, 1.2fr) minmax(260px, 0.9fr);
          gap:12px;
          align-items:start;
        }

        .initiative-list{
          display:flex;
          flex-direction:column;
          gap:8px;
        }

        .initiative-row{
          border:1px solid rgba(191,167,111,0.45);
          background:rgba(255,255,245,0.82);
          border-radius:12px;
          padding:8px 10px;
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
        }

        .initiative-name{
          font-weight:900;
          font-size:13px;
        }

        .initiative-sub{
          font-size:11px;
          opacity:0.78;
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
          opacity:0.82;
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
          grid-template-columns:110px 1fr;
          gap:8px;
          row-gap:8px;
          font-size:12px;
        }

        .battlefield-k{
          font-weight:900;
          opacity:0.9;
        }

        .combat-log{
          display:flex;
          flex-direction:column;
          gap:8px;
          max-height:420px;
          overflow-y:auto;
          padding-right:4px;
        }

        .combat-log-item{
          border:1px solid rgba(191,167,111,0.38);
          border-radius:10px;
          padding:8px 10px;
          background:rgba(255,255,245,0.82);
        }

        .combat-log-type{
          font-size:10px;
          font-weight:900;
          text-transform:uppercase;
          opacity:0.75;
          margin-bottom:4px;
        }

        .combat-log-body{
          font-size:12px;
          line-height:1.25;
        }

        .combat-log-time{
          margin-top:4px;
          font-size:10px;
          opacity:0.65;
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
          grid-template-columns:130px 1fr;
          gap:8px;
          row-gap:6px;
          font-size:12px;
          margin-bottom:10px;
        }

        .confirm-k{
          font-weight:900;
          opacity:0.9;
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
          opacity:0.8;
          margin-top:8px;
        }

        .map-tile{
          cursor:help;
        }

        .map-tile::after{
          content:attr(data-tip);
          position:absolute;
          left:50%;
          bottom:calc(100% + 8px);
          transform:translateX(-50%);
          min-width:170px;
          max-width:240px;
          white-space:pre-line;
          padding:8px 10px;
          border-radius:10px;
          background:rgba(20,18,16,0.96);
          color:#f5e6c8;
          border:1px solid rgba(191,167,111,0.55);
          box-shadow:0 8px 22px rgba(0,0,0,0.28);
          font-size:11px;
          line-height:1.3;
          pointer-events:none;
          opacity:0;
          z-index:99999;
        }

        .map-tile:hover::after{
          opacity:1;
        }

        .map-tile::before{
          content:'';
          position:absolute;
          left:50%;
          bottom:calc(100% + 2px);
          transform:translateX(-50%);
          border-width:6px;
          border-style:solid;
          border-color:rgba(20,18,16,0.96) transparent transparent transparent;
          opacity:0;
          pointer-events:none;
          z-index:99999;
        }

        .map-tile:hover::before{
          opacity:1;
        }

        @media (max-width: 1200px){
          .combat-grid{
            grid-template-columns: 1fr 1fr;
          }

          .combat-grid > :nth-child(3){
            grid-column: 1 / -1;
          }
        }

        @media (max-width: 900px){
          .combat-top-grid{
            grid-template-columns:1fr;
          }

          .combat-lower-actions{
            grid-template-columns:1fr;
          }

          .combat-movement-block{
            min-width:0;
          }

          .combat-grid{
            grid-template-columns:1fr;
          }

          .combat-grid > :nth-child(3){
            grid-column:auto;
          }
        }

        @media (max-width: 700px){
          .combat-control-row{
            flex-direction:column;
            align-items:stretch;
          }

          .combat-control-row .shiny-input-container,
          .combat-control-row .btn,
          .combat-action-buttons .btn{
            width:100% !important;
            min-width:0;
          }

          .combat-action-buttons{
            flex-direction:column;
          }

          .battlefield-kv{
            grid-template-columns:1fr;
          }
        }
      ")),
      
      div(
        class = "combat-wrap",
        
        div(
          class = "combat-card",
          uiOutput(ns("header_ui")),
          
          div(
            class = "combat-top-grid",
            
            div(
              class = "combat-top-left",
              
              div(
                class = "combat-control-row",
                numericInput(ns("encounter_id"), "Encounter ID", value = 1, min = 1, width = "130px"),
                actionButton(ns("refresh"), "Refresh", class = "btn btn-default"),
                actionButton(ns("bind_encounter"), "Use Encounter", class = "btn btn-default"),
                actionButton(ns("roll_init"), "Roll Initiative", class = "btn btn-primary"),
                actionButton(ns("init_combat"), "Start Combat", class = "btn btn-primary"),
                actionButton(ns("end_turn"), "End Turn", class = "btn btn-warning")
              ),
              
              div(
                class = "combat-action-buttons",
                actionButton(ns("attack_btn"), "Attack", class = "btn btn-danger"),
                actionButton(ns("opp_attack_btn"), "Opportunity Attack", class = "btn btn-warning")
              ),
              
              div(
                class = "combat-lower-actions",
                
                div(
                  class = "combat-movement-block",
                  div(class = "combat-sub", tags$strong("Movement")),
                  tags$div(
                    class = "combat-move-grid",
                    actionButton(ns("move_nw"), "↖", class = "btn btn-info"),
                    actionButton(ns("move_n"),  "↑", class = "btn btn-info"),
                    actionButton(ns("move_ne"), "↗", class = "btn btn-info"),
                    
                    actionButton(ns("move_w"),  "←", class = "btn btn-info"),
                    tags$div(style = "width:44px; height:38px;"),
                    actionButton(ns("move_e"),  "→", class = "btn btn-info"),
                    
                    actionButton(ns("move_sw"), "↙", class = "btn btn-info"),
                    actionButton(ns("move_s"),  "↓", class = "btn btn-info"),
                    actionButton(ns("move_se"), "↘", class = "btn btn-info")
                  )
                ),
                
                div(
                  class = "combat-turn-box",
                  uiOutput(ns("turn_notice_ui"))
                )
              )
            ),
            
            div(
              class = "combat-card",
              style = "padding:12px;",
              selectInput(ns("target_id"), "Target", choices = NULL, width = "100%")
            )
          )
        ),
       
        
        div(
          class = "combat-grid",
          div(class = "combat-card", uiOutput(ns("initiative_ui"))),
          div(
            class = "combat-card",
            uiOutput(ns("battlefield_ui")),
            uiOutput(ns("map_ui"))
          ),
          div(class = "combat-card", uiOutput(ns("log_ui")))
        )
      )
    )
  )
}
debugCombatServer <- function(id, core, ctrl, add_log = NULL) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    source("server/combat_map_logic.R", local = FALSE)
    
    refresh_key <- reactiveVal(0)
    pending_attack <- reactiveVal(NULL)
    turn_move_ft <- reactiveVal(0L)
    
    known_ac_rv <- reactiveVal(data.frame(
      target_id = character(),
      lower = integer(),
      upper = integer(),
      stringsAsFactors = FALSE
    ))
    
    map_id <- reactiveVal(1L)
    
    map_tiles_rv <- reactive({
      mid <- map_id()
      get_or_create_shared_map_tiles(ctrl, mid, width = 10L, height = 10L)
    })
    
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
    
    bump_refresh <- function() {
      refresh_key(refresh_key() + 1L)
    }
    
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
      eid <- suppressWarnings(as.integer(input$encounter_id %||% NA))
      if (is.na(eid) || eid < 1) return(NA_integer_)
      eid
    })
    
    encounter_tbl <- reactive({
      refresh_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
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
      
      df <- tryCatch(get_session_players(sid), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    
    positions_tbl <- reactive({
      refresh_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- tryCatch(get_encounter_positions(eid), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    
    combat_tbl <- reactive({
      refresh_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- tryCatch(get_combat_state(eid), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    
    events_tbl <- reactive({
      refresh_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- tryCatch(get_encounter_events(eid, limit = 50), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    
    encounter_actors_tbl <- reactive({
      refresh_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- tryCatch(get_encounter_actors(eid), error = function(e) data.frame())
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
    

  
    observeEvent(input$opp_attack_btn, {
      actors <- encounter_actors_tbl()
      if (!is.data.frame(actors) || nrow(actors) == 0) {
        log_safe("⚠️ No actors in encounter.")
        return()
      }
      
      cid <- as.character(core$state$char_id %||% "")
      attackers <- actors[
        as.character(actors$actor_type %||% "") == "player" &
          as.character(actors$actor_id %||% "") == cid,
        ,
        drop = FALSE
      ]
      
      targets <- actors
      
      if (!is.data.frame(attackers) || nrow(attackers) == 0) {
        log_safe("⚠️ No player attackers available.")
        return()
      }
      
      attacker_choices <- stats::setNames(
        as.character(attackers$actor_id),
        as.character(attackers$display_name %||% attackers$actor_id)
      )
      
      target_choices <- stats::setNames(
        as.character(targets$actor_id),
        paste0(
          as.character(targets$display_name %||% targets$actor_id),
          " (", as.character(targets$actor_type %||% "actor"), ")"
        )
      )
      
      showModal(modalDialog(
        title = "Opportunity Attack",
        selectInput(session$ns("opp_attacker_id"), "Attacker", choices = attacker_choices),
        selectInput(session$ns("opp_target_id"), "Target", choices = target_choices),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("opp_choose_weapon"), "Choose Weapon", class = "btn btn-warning")
        ),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$opp_choose_weapon, {
      attacker_id <- as.character(input$opp_attacker_id %||% "")
      target_id <- as.character(input$opp_target_id %||% "")
      
      if (!nzchar(attacker_id) || !nzchar(target_id)) {
        log_safe("⚠️ Choose attacker and target first.")
        return()
      }
      
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
      
      removeModal()
      showModal(modalDialog(
        title = paste0("Opportunity Attack — ", get_actor_display_name(attacker_id)),
        radioButtons(session$ns("opp_weapon_id"), "Equipped weapons", choices = weapon_choices),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("opp_confirm_attack"), "Roll Opportunity Attack", class = "btn btn-danger")
        ),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)
    
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
      
      bump_refresh()
    })
    
    
    observeEvent(input$opp_confirm_attack, {
      removeModal()
      
      attacker_id <- as.character(input$opp_attacker_id %||% "")
      target_id <- as.character(input$opp_target_id %||% "")
      weapon_id <- as.character(input$opp_weapon_id %||% "")
      
      if (!nzchar(attacker_id) || !nzchar(target_id) || !nzchar(weapon_id)) {
        log_safe("⚠️ Opportunity attack setup incomplete.")
        return()
      }
      
      attacker_char <- load_actor_for_combat(attacker_id, "player")
      target_type <- get_actor_type_by_id(target_id)
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
      
      preview <- build_attack_preview(
        attacker_char = attacker_char,
        target_char = target_char,
        weapon_row = weapon_row,
        attacker_name = get_actor_display_name(attacker_id),
        target_name = get_actor_display_name(target_id),
        attacker_id = attacker_id,
        target_id = target_id,
        attacker_type = "player",
        target_type = target_type
      )
      
      preview$is_opportunity_attack <- TRUE
      pending_attack(preview)
      
      base_proposed <- if (isTRUE(preview$is_hit)) {
        compute_final_attack(preview)$adjusted$total
      } else {
        0L
      }
      
      showModal(modalDialog(
        title = "Confirm Opportunity Attack",
        uiOutput(session$ns("attack_confirm_ui")),
        numericInput(session$ns("final_damage_override"), "Final damage to apply", value = base_proposed, min = 0, step = 1),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("apply_attack_final"), "Apply Result", class = "btn btn-danger")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    }, ignoreInit = TRUE)
    
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
    
    observe({
      active <- active_actor_row()
      lbl <- if (isTRUE(is_players_turn())) "Attack" else "Attack (wait for turn)"
      
      if (is.data.frame(active) && nrow(active) > 0 && isTRUE(is_players_turn())) {
        nm <- as.character(active$display_name[1] %||% "Actor")
        lbl <- paste0("Attack as ", nm)
      }
      
      updateActionButton(session, "attack_btn", label = lbl)
    })
    
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
          wpn <- payload$weapon_name %||% "Weapon"
          if (isTRUE(payload$is_hit %||% FALSE)) {
            dmg <- payload$final_damage %||% payload$damage_total %||% "?"
            extra <- if (isTRUE(payload$used_sneak_attack %||% FALSE)) " + Sneak Attack" else ""
            return(paste0(actor_name, " hits ", target_name, " with ", wpn, extra, " for ", dmg, " damage."))
          } else {
            atk <- payload$attack_total %||% "?"
            ac  <- payload$target_ac %||% "?"
            return(paste0(actor_name, " misses ", target_name, " with ", wpn, " (", atk, " vs AC ", ac, ")."))
          }
        }
        return(paste0(actor_name, " attacks ", target_name, "."))
      }
      
      if (typ == "end_turn") return("Turn advanced.")
      
      if (nzchar(actor_id) && nzchar(target_id)) return(paste(actor_name, "→", target_name))
      if (nzchar(actor_id)) return(paste("Actor:", actor_name))
      
      typ
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
        as.character(active$display_name[1] %||% "Unknown")
      } else {
        "No active actor"
      }
      
      tagList(
        div(class = "combat-title", "⚔️ Combat"),
        div(class = "combat-sub", paste(enc_name, "•", enc_status)),
        div(
          class = "combat-pills",
          div(class = "combat-pill", paste("Round", round_txt)),
          div(class = "combat-pill", paste("Phase", phase_txt)),
          div(class = "combat-pill", paste("Active", active_name)),
          div(class = "combat-pill", paste("Movement", turn_move_ft(), "ft"))
        )
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
      occ <- map_occupants_rv()
      
      render_df <- build_map_render_df(
        tiles = map_tiles_rv(),
        occupants = occ,
        map_id = map_id()
      )
      
      if (!is.data.frame(render_df) || nrow(render_df) == 0) {
        return(tags$em("No map loaded."))
      }
      
      bounds <- get_map_bounds(render_df)
      
      terrain_col <- function(terrain) {
        terrain <- tolower(as.character(terrain %||% "grass"))
        switch(
          terrain,
          grass  = "#8fbf7a",
          stone  = "#b8b8b8",
          forest = "#5d8a4f",
          swamp  = "#6d8a57",
          water  = "#6da7d9",
          wall   = "#555555",
          road   = "#c8b58a",
          "#d9d4c7"
        )
      }
      
      light_overlay <- function(light, fog) {
        light <- tolower(as.character(light %||% "full"))
        fog <- as.integer(fog %||% 0L)
        
        if (fog == 1L) return("background: rgba(20,20,20,0.88);")
        if (light == "dark") return("background: rgba(20,20,20,0.55);")
        if (light == "dim") return("background: rgba(80,80,80,0.18);")
        "background: transparent;"
      }
      
      token_labels <- list()
      
      if (is.data.frame(occ) && nrow(occ) > 0) {
        names_vec <- vapply(seq_len(nrow(occ)), function(i) {
          get_actor_display_name(as.character(occ$actor_id[i]))
        }, character(1))
        
        first_letters <- toupper(substr(names_vec, 1, 1))
        counts <- list()
        
        for (i in seq_along(first_letters)) {
          key <- first_letters[i]
          
          if (is.null(counts[[key]])) {
            counts[[key]] <- 1
            token_labels[[occ$actor_id[i]]] <- key
          } else {
            counts[[key]] <- counts[[key]] + 1
            token_labels[[occ$actor_id[i]]] <- paste0(key, counts[[key]])
          }
        }
      }
      
      rows_ui <- lapply(seq(bounds$min_y, bounds$max_y), function(yv) {
        row_tiles <- render_df[render_df$y == yv, , drop = FALSE]
        row_tiles <- row_tiles[order(row_tiles$x), , drop = FALSE]
        
        cols_ui <- lapply(seq_len(nrow(row_tiles)), function(i) {
          t <- row_tiles[i, , drop = FALSE]
          
          occ_lbl <- ""
          occ_bg <- "transparent"
          occ_name <- "Empty"
          occ_type_txt <- "None"
          hp_pct <- 0
          is_active_actor <- FALSE
          is_dead <- FALSE
          
          has_occupant <- !is.na(t$occupant_id[1]) && nzchar(as.character(t$occupant_id[1]))
          
          if (has_occupant) {
            occ_id <- as.character(t$occupant_id[1])
            occ_type_txt <- as.character(t$occupant_type[1] %||% "unknown")
            occ_name <- get_actor_display_name(occ_id)
            occ_lbl <- token_labels[[occ_id]] %||% "?"
            
            if (identical(occ_type_txt, "player")) {
              occ_bg <- "rgba(30,80,180,0.88)"
            } else {
              occ_bg <- "rgba(160,40,40,0.88)"
            }
            
            actors <- encounter_actors_tbl()
            row <- actors[as.character(actors$actor_id) == occ_id, , drop = FALSE]
            
            if (nrow(row) > 0) {
              cur_hp <- suppressWarnings(as.integer(row$current_hp[1] %||% row$hp_current[1] %||% 0))
              max_hp <- suppressWarnings(as.integer(row$hp_max[1] %||% NA))
              
              if (is.na(cur_hp)) cur_hp <- 0L
              if (is.na(max_hp) || max_hp <= 0L) {
                max_hp <- max(cur_hp, 1L)
              }
              
              hp_pct <- max(0, min(100, round((cur_hp / max_hp) * 100)))
              is_dead <- isTRUE(cur_hp <= 0L)
            }
            
            is_active_actor <- identical(occ_id, active_actor_id())
          }
          
          tip_txt <- paste(
            paste0("Square: (", t$x[1], ", ", t$y[1], ")"),
            paste0("Terrain: ", t$terrain[1]),
            paste0("Occupant: ", occ_name),
            paste0("Occupant Type: ", occ_type_txt),
            paste0("Fog: ", ifelse(as.integer(t$fog[1] %||% 0L) == 1L, "Hidden", "Visible")),
            paste0("Light: ", t$light[1]),
            paste0("Move Cost: ", t$move_cost[1], " (", t$move_cost[1] * 5, " ft)"),
            paste0("Blocks Movement: ", ifelse(isTRUE(t$blocks_movement[1]), "Yes", "No")),
            paste0("Blocks Vision: ", ifelse(isTRUE(t$blocks_vision[1]), "Yes", "No")),
            sep = "\n"
          )
          
          tags$div(
            class = "map-tile",
            `data-tip` = tip_txt,
            style = paste0(
              "position:relative;",
              "width:38px;height:38px;",
              "border:1px solid rgba(80,60,30,0.22);",
              "background:", terrain_col(t$terrain[1]), ";",
              "box-sizing:border-box;",
              "overflow:visible;"
            ),
            
            tags$div(
              style = paste0(
                "position:absolute;inset:0;",
                light_overlay(t$light[1], t$fog[1])
              )
            ),
            
            if (isTRUE(t$blocks_movement[1])) {
              tags$div(
                style = "position:absolute; inset:8px; border:2px solid rgba(60,20,20,0.65); border-radius:4px;"
              )
            },
            
            if (has_occupant) {
              tags$div(
                style = paste0(
                  "position:absolute;",
                  "left:6px; top:6px;",
                  "width:24px; height:24px;",
                  "border-radius:999px;",
                  "display:flex; align-items:center; justify-content:center;",
                  "font-size:11px; font-weight:900; color:white;",
                  "background:", occ_bg, ";",
                  if (is_active_actor) {
                    "box-shadow: 0 0 0 2px rgba(255,180,60,0.9), 0 0 8px rgba(255,140,40,0.9);"
                  } else "",
                  if (is_dead) {
                    "filter: grayscale(0.8) brightness(0.7);"
                  } else ""
                ),
                
                tags$div(
                  style = paste0(
                    "position:absolute;",
                    "inset:-3px;",
                    "border-radius:999px;",
                    "background:conic-gradient(",
                    "rgba(200,40,40,0.95) 0% ", hp_pct, "%, ",
                    "rgba(60,60,60,0.2) ", hp_pct, "% 100%);",
                    "z-index:0;"
                  )
                ),
                
                tags$div(
                  style = paste0(
                    "position:absolute;",
                    "inset:2px;",
                    "border-radius:999px;",
                    "background:", occ_bg, ";",
                    "z-index:1;"
                  )
                ),
                
                tags$div(
                  style = "position:relative; z-index:2;",
                  occ_lbl
                ),
                
                if (is_dead) {
                  tags$div(
                    style = paste0(
                      "position:absolute;",
                      "inset:0;",
                      "display:flex; align-items:center; justify-content:center;",
                      "font-size:16px; font-weight:900;",
                      "color:rgba(255,255,255,0.95);",
                      "text-shadow:0 0 6px rgba(0,0,0,0.9);",
                      "z-index:3;"
                    ),
                    "☠"
                  )
                }
              )
            }
          )
        })
        
        tags$div(
          style = "display:flex; gap:0; overflow:visible;",
          cols_ui
        )
      })
      
      tagList(
        tags$div(
          style = "margin-top:12px;",
          tags$div(class = "combat-section-title", "Map"),
          tags$div(
            style = paste0(
              "display:inline-flex; flex-direction:column; ",
              "border:1px solid rgba(191,167,111,0.5); ",
              "background:rgba(255,255,245,0.75); padding:6px;",
              "overflow:visible;"
            ),
            rows_ui
          ),
          tags$div(
            class = "combat-sub",
            style = "margin-top:8px;",
            "Each square = 5 ft. Blue tokens are players."
          )
        )
      )
    })
    
    # --------------------------------------------------
    # Initiative
    # --------------------------------------------------
    observeEvent(input$roll_init, {
      eid <- current_encounter_id()
      if (is.na(eid)) {
        log_safe("⚠️ Choose an encounter first.")
        return()
      }
      
      init_df <- roll_encounter_initiative(
        encounter_id = eid,
        core_state = core$state
      )
      
      if (!is.data.frame(init_df) || nrow(init_df) == 0) {
        log_safe("⚠️ Could not roll initiative.")
        return()
      }
      
      top_name <- as.character(init_df$display_name[1] %||% "Unknown")
      top_score <- as.integer(init_df$initiative_total[1] %||% 0)
      
      log_safe(paste0("🎲 Initiative rolled. ", top_name, " leads on ", top_score, "."))
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
    
    # --------------------------------------------------
    # Target choices
    # --------------------------------------------------
    observe({
      actors <- encounter_actors_tbl()
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
            nm <- as.character(row$display_name[1] %||% "Unknown")
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
      
      updateSelectInput(
        session,
        "target_id",
        choices = choices,
        selected = selected_target
      )
    })
    
    # --------------------------------------------------
    # Attack flow
    # --------------------------------------------------
    observeEvent(input$attack_btn, {
      attacker_id <- active_actor_id()
      attacker_type <- as.character(combat_tbl()$active_actor_type[1] %||% "")
      target_id <- as.character(input$target_id %||% "")
      
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
        primary_damage_type = as.character(attacker_char$combat_profile$damage_type %||% "")
      )
    }
    
    
    observe({
      cid <- as.character(core$state$char_id %||% "")
      has_char <- nzchar(cid)
      on_turn <- isTRUE(is_players_turn())
      
      shinyjs::toggleState(id = session$ns("attack_btn"), condition = has_char && on_turn)
    })
    
    observeEvent(input$confirm_attack, {
      removeModal()
      
      attacker_id <- active_actor_id()
      target_id <- as.character(input$target_id %||% "")
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
      bump_refresh()
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------
    # Battlefield summary
    # --------------------------------------------------
    output$battlefield_ui <- renderUI({
      active <- active_actor_row()
      pos <- active_position_row()
      combat <- combat_tbl()
      enc <- encounter_tbl()
      
      encounter_name <- if (is.data.frame(enc) && nrow(enc) > 0) {
        as.character(enc$name[1] %||% "—")
      } else {
        "—"
      }
      
      active_name <- if (is.data.frame(active) && nrow(active) > 0) {
        as.character(active$display_name[1] %||% "Unknown")
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
        div(class = "combat-section-title", "Battlefield"),
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
          class = "combat-sub",
          "This is the pass 1 battlefield summary. Next pass can turn this into a proper visual grid/map."
        )
      )
    })
    
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
    
    observeEvent(input$bind_encounter, {
      eid <- current_encounter_id()
      cid <- as.character(core$state$char_id %||% "")
      
      if (is.na(eid)) {
        log_safe("⚠️ Choose an encounter first.")
        return()
      }
      
      if (!nzchar(cid)) {
        log_safe("⚠️ No character is currently loaded in player state.")
        return()
      }
      
      actors <- encounter_actors_tbl()
      row <- actors[
        as.character(actors$actor_type %||% "") == "player" &
          as.character(actors$actor_id %||% "") == cid,
        ,
        drop = FALSE
      ]
      
      if (!is.data.frame(row) || nrow(row) == 0) {
        log_safe("⚠️ Current loaded character is not part of this encounter.")
        return()
      }
      
      enc <- tryCatch(get_encounter(eid), error = function(e) data.frame())
      sid <- NA_integer_
      if (is.data.frame(enc) && nrow(enc) > 0) {
        sid <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
      }
      
      core$state$active_encounter_id <- eid
      if (!is.na(sid) && sid > 0) {
        core$state$active_session_id <- sid
      }
      
      log_safe(paste0("🔗 Bound combat to encounter ", eid, "."))
      bump_refresh()
    }, ignoreInit = TRUE)
    
    observeEvent(input$init_combat, {
      eid <- current_encounter_id()
      if (is.na(eid)) {
        log_safe("⚠️ Choose an encounter first.")
        return()
      }
      
      known_ac_rv(data.frame(
        target_id = character(),
        lower = integer(),
        upper = integer(),
        stringsAsFactors = FALSE
      ))
      
      init_df <- start_encounter_combat(
        encounter_id = eid,
        core_state = core$state
      )
      
      if (!is.data.frame(init_df) || nrow(init_df) == 0) {
        log_safe("⚠️ Could not initialise combat.")
        return()
      }
      
      enc <- tryCatch(get_encounter(eid), error = function(e) data.frame())
      sid <- NA_integer_
      if (is.data.frame(enc) && nrow(enc) > 0) {
        sid <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
      }
      
      turn_move_ft(0L)
      core$state$active_encounter_id <- eid
      if (!is.na(sid) && sid > 0) {
        core$state$active_session_id <- sid
      }
      
      first_name <- as.character(init_df$display_name[1] %||% "Unknown")
      log_safe(paste0("⚔️ Combat initialised. ", first_name, " acts first."))
      
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
      
      bump_refresh()
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------
    # Movement
    # --------------------------------------------------
    move_active_actor <- function(dx = 0L, dy = 0L) {
      eid <- current_encounter_id()
      combat <- combat_tbl()
      
      cid <- as.character(core$state$char_id %||% "")
      if (!nzchar(cid)) {
        log_safe("⚠️ No player character is currently loaded.")
        return(invisible(FALSE))
      }
      
      if (!isTRUE(is_players_turn())) {
        log_safe("⚠️ You can only move on your own turn.")
        return(invisible(FALSE))
      }
      
      if (!identical(as.character(combat$active_actor_id[1] %||% ""), cid)) {
        log_safe("⚠️ You can only move your own character.")
        return(invisible(FALSE))
      }
      
      if (is.na(eid) || !is.data.frame(combat) || nrow(combat) == 0 || is.na(combat$active_actor_id[1])) {
        log_safe("⚠️ No active combat actor.")
        return(invisible(FALSE))
      }
      
      actor_id <- as.character(combat$active_actor_id[1] %||% "")
      actor_type <- as.character(combat$active_actor_type[1] %||% "player")
      
      occ <- map_occupants_rv()
      pos <- get_actor_position(
        occupants = occ,
        actor_id = actor_id,
        map_id = map_id(),
        encounter_id = eid
      )
      
      if (!is.data.frame(pos) || nrow(pos) == 0) {
        log_safe("⚠️ Active actor has no map position.")
        return(invisible(FALSE))
      }
      
      actor_char <- NULL
      if (identical(actor_type, "player")) {
        actor_char <- if (identical(as.character(core$state$char_id %||% ""), actor_id)) {
          core$state$char
        } else {
          tryCatch(load_character_from_db(actor_id), error = function(e) NULL)
        }
      }
      
      can_phase <- identical(actor_type, "player") &&
        !is.null(actor_char) &&
        isTRUE(can_phase_through_objects(actor_char))
      
      old_x <- as.integer(pos$x[1] %||% 0)
      old_y <- as.integer(pos$y[1] %||% 0)
      new_x <- old_x + as.integer(dx)
      new_y <- old_y + as.integer(dy)
      
      move_check <- can_enter_tile(
        tiles = map_tiles_rv(),
        occupants = occ,
        x = new_x,
        y = new_y,
        map_id = map_id(),
        exclude_actor_id = actor_id
      )
      
      if (!isTRUE(move_check$ok)) {
        if (identical(move_check$reason, "blocked") && isTRUE(can_phase)) {
          tile_row <- get_tile_row(
            tiles = map_tiles_rv(),
            x = new_x,
            y = new_y,
            map_id = map_id()
          )
          
          if (!nrow(tile_row)) {
            log_safe("⚠️ That tile is outside the map.")
            return(invisible(FALSE))
          }
          
          if (tile_is_occupied(
            occupants = occ,
            x = new_x,
            y = new_y,
            map_id = map_id(),
            exclude_actor_id = actor_id
          )) {
            log_safe("⚠️ That tile is occupied.")
            return(invisible(FALSE))
          }
          
          move_check$ok <- TRUE
          move_check$reason <- "phase"
          move_check$move_cost <- as.numeric(tile_row$move_cost[1] %||% 1)
          if (is.na(move_check$move_cost)) move_check$move_cost <- 1
        } else {
          msg <- switch(
            move_check$reason,
            out_of_bounds = "⚠️ That tile is outside the map.",
            blocked = "⚠️ That tile blocks movement.",
            occupied = "⚠️ That tile is occupied.",
            "⚠️ Cannot move there."
          )
          log_safe(msg)
          return(invisible(FALSE))
        }
      }
      
      ok1 <- upsert_encounter_actor_position(
        encounter_id = eid,
        actor_type = actor_type,
        actor_id = actor_id,
        x = new_x,
        y = new_y
      )
      
      if (!isTRUE(ok1)) {
        log_safe("⚠️ Could not move active actor.")
        return(invisible(FALSE))
      }
      

      
      log_game_event(
        encounter_id = eid,
        event_type = "move",
        actor_type = actor_type,
        actor_id = actor_id,
        payload = list(
          from = list(x = old_x, y = old_y),
          to = list(x = new_x, y = new_y),
          move_cost = move_check$move_cost,
          phased = identical(move_check$reason, "phase")
        )
      )
      
      step_mult <- if (abs(as.integer(dx)) == 1L && abs(as.integer(dy)) == 1L) 2L else 1L
      move_ft <- as.integer(round((move_check$move_cost %||% 1) * 5 * step_mult))
      turn_move_ft(as.integer(turn_move_ft() + move_ft))
      
      opp_attackers <- tryCatch(
        get_opportunity_attackers(
          encounter_id = eid,
          mover_id = actor_id,
          mover_type = actor_type,
          old_x = old_x,
          old_y = old_y,
          new_x = new_x,
          new_y = new_y
        ),
        error = function(e) {
          message("get_opportunity_attackers failed: ", e$message)
          data.frame()
        }
      )
      
      
      if (identical(move_check$reason, "phase")) {
        log_safe(paste0(
          "👻 Active actor phases to (", new_x, ", ", new_y, "). Cost: ", move_ft, " ft."
        ))
      } else {
        log_safe(paste0(
          "🧭 Active actor moved to (", new_x, ", ", new_y, "). Cost: ", move_ft, " ft."
        ))
      }
      
      bump_refresh()
      invisible(TRUE)
    }
    
    observeEvent(input$move_w, { move_active_actor(dx = -1L, dy =  0L) }, ignoreInit = TRUE)
    observeEvent(input$move_e, { move_active_actor(dx =  1L, dy =  0L) }, ignoreInit = TRUE)
    observeEvent(input$move_n, { move_active_actor(dx =  0L, dy = -1L) }, ignoreInit = TRUE)
    observeEvent(input$move_s, { move_active_actor(dx =  0L, dy =  1L) }, ignoreInit = TRUE)
    
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
    
    observeEvent(input$move_nw, { move_active_actor(dx = -1L, dy = -1L) }, ignoreInit = TRUE)
    observeEvent(input$move_ne, { move_active_actor(dx =  1L, dy = -1L) }, ignoreInit = TRUE)
    observeEvent(input$move_sw, { move_active_actor(dx = -1L, dy =  1L) }, ignoreInit = TRUE)
    observeEvent(input$move_se, { move_active_actor(dx =  1L, dy =  1L) }, ignoreInit = TRUE)
  })
}