library(shiny)

debugCombatUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Debug Combat",
    value = "debug_combat",
    
    div(
      class = "card",
      h3("⚔️ Debug Combat"),
      
      numericInput(ns("session_id"), "Session ID", value = 1, min = 1),
      
      div(
        style = "display:flex; gap:10px; flex-wrap:wrap; margin-bottom:12px;",
        actionButton(ns("refresh"), "Refresh", class = "btn btn-default"),
        actionButton(ns("bind_session"), "Use This Session", class = "btn btn-default"),
        actionButton(ns("init_combat"), "Init Combat", class = "btn btn-primary"),
        actionButton(ns("end_turn"), "End Turn", class = "btn btn-warning"),
        actionButton(ns("move_test"), "Move Test Player +1 X", class = "btn btn-info"),
        actionButton(ns("damage_test"), "Damage Active Player -2 HP", class = "btn btn-danger"),
        actionButton(ns("opp_attack_btn"), "Opportunity Attack", class = "btn btn-warning")
      ),
      
      tags$hr(),
      
      h4("Session"),
      tableOutput(ns("session_tbl")),
      
      h4("Players"),
      tableOutput(ns("players_tbl")),
      
      h4("Positions"),
      tableOutput(ns("positions_tbl")),
      
      h4("Combat State"),
      tableOutput(ns("combat_tbl")),
      
      h4("Recent Events"),
      tableOutput(ns("events_tbl"))
    )
  )
}

debugCombatServer <- function(id, core, add_log = NULL) {
  moduleServer(id, function(input, output, session) {
    
    log_safe <- function(msg) {
      if (is.function(add_log)) {
        try(add_log(msg, toast = TRUE), silent = TRUE)
      } else {
        message(msg)
      }
    }
    
    current_session_id <- reactive({
      as.integer(input$session_id %||% 1)
    })
    
    refresh_key <- reactiveVal(0)
    
    bump_refresh <- function() {
      refresh_key(refresh_key() + 1)
    }
    
    session_data <- reactive({
      refresh_key()
      get_session_overview(current_session_id())
    })
    
    output$session_tbl <- renderTable({
      session_data()$session
    }, striped = TRUE, bordered = TRUE)
    
    output$players_tbl <- renderTable({
      session_data()$players
    }, striped = TRUE, bordered = TRUE)
    
    output$positions_tbl <- renderTable({
      session_data()$positions
    }, striped = TRUE, bordered = TRUE)
    
    output$combat_tbl <- renderTable({
      session_data()$combat
    }, striped = TRUE, bordered = TRUE)
    
    output$events_tbl <- renderTable({
      session_data()$events
    }, striped = TRUE, bordered = TRUE)
    
    observeEvent(input$refresh, {
      bump_refresh()
    }, ignoreInit = TRUE)
    
    observeEvent(input$bind_session, {
      sid <- current_session_id()
      cid <- core$state$char_id
      
      if (is.null(cid)) {
        log_safe("⚠️ No character is currently loaded in player state.")
        return()
      }
      
      row <- get_session_player_row(sid, cid)
      if (nrow(row) == 0) {
        log_safe("⚠️ Current loaded character is not part of this session.")
        return()
      }
      
      ok_hp <- ensure_session_player_hp(
        session_id = sid,
        character_id = cid,
        fallback_char = core$state$char
      )
      
      core$state$active_session_id <- sid
      
      cat("DEBUG BIND SESSION\n")
      cat("char_id =", cid, "\n")
      cat("active_session_id =", core$state$active_session_id, "\n")
      
      if (!isTRUE(ok_hp)) {
        log_safe("⚠️ Session bound, but could not initialise session HP.")
      } else {
        log_safe(paste0("🔗 Bound HUD to session ", sid, "."))
      }
      
      bump_refresh()
    }, ignoreInit = TRUE)
    
    observeEvent(input$init_combat, {
      sid <- current_session_id()
      players <- get_session_players(sid)
      
      if (nrow(players) == 0) {
        log_safe("⚠️ No players found for this session.")
        return()
      }
      
      players <- players[order(players$turn_order, na.last = TRUE), , drop = FALSE]
      first <- players[1, , drop = FALSE]
      
      ok1 <- set_combat_state(
        session_id = sid,
        round_number = 1,
        current_turn_order = as.integer(first$turn_order[1] %||% 1),
        active_actor_type = "player",
        active_actor_id = as.character(first$character_id[1]),
        phase = "combat"
      )
      
      ok2 <- log_game_event(
        session_id = sid,
        event_type = "enter_combat",
        actor_type = "system",
        payload = list(
          active_actor_id = as.character(first$character_id[1]),
          turn_order = as.integer(first$turn_order[1] %||% 1),
          round_number = 1
        )
      )
      
      if (!isTRUE(ok1)) {
        log_safe("⚠️ Could not initialise combat.")
        return()
      }
      
      core$state$active_session_id <- sid
      log_safe("⚔️ Combat initialised.")
      bump_refresh()
    }, ignoreInit = TRUE)
    
    observeEvent(input$end_turn, {
      ok <- advance_turn(current_session_id())
      if (ok) {
        log_safe("⏭️ Turn advanced.")
      } else {
        log_safe("⚠️ Could not advance turn.")
      }
      bump_refresh()
    }, ignoreInit = TRUE)
    
    observeEvent(input$move_test, {
      sid <- current_session_id()
      combat <- get_combat_state(sid)
      
      if (nrow(combat) == 0 || is.na(combat$active_actor_id[1])) {
        log_safe("⚠️ No active combat actor.")
        return()
      }
      
      actor_id <- as.character(combat$active_actor_id[1])
      pos <- get_player_positions(sid)
      row <- pos[pos$character_id == actor_id, , drop = FALSE]
      
      if (nrow(row) == 0) {
        log_safe("⚠️ Active actor has no position row.")
        return()
      }
      
      old_x <- as.integer(row$x[1])
      old_y <- as.integer(row$y[1])
      
      ok1 <- set_player_position(
        session_id = sid,
        character_id = actor_id,
        x = old_x + 1L,
        y = old_y
      )
      
      ok2 <- log_game_event(
        session_id = sid,
        event_type = "move",
        actor_type = "player",
        actor_id = actor_id,
        payload = list(
          from = list(x = old_x, y = old_y),
          to = list(x = old_x + 1L, y = old_y)
        )
      )
      
      if (!isTRUE(ok1)) {
        log_safe("⚠️ Could not move active player.")
        return()
      }
      
      log_safe("🧭 Active player moved.")
      bump_refresh()
    }, ignoreInit = TRUE)
    
    observeEvent(input$damage_test, {
      sid <- current_session_id()
      combat <- get_combat_state(sid)
      
      if (nrow(combat) == 0 || is.na(combat$active_actor_id[1])) {
        log_safe("⚠️ No active combat actor.")
        return()
      }
      
      actor_id <- as.character(combat$active_actor_id[1])
      
      cat("DAMAGE TEST\n")
      cat("session_id =", sid, "\n")
      cat("active_actor_id =", actor_id, "\n")
      cat("core char_id =", core$state$char_id, "\n")
      
      res <- damage_session_player(
        session_id = sid,
        character_id = actor_id,
        amount = 2
      )
      
      if (is.null(res)) {
        log_safe("⚠️ Could not damage active player.")
        return()
      }
      
      log_game_event(
        session_id = sid,
        event_type = "damage",
        actor_type = "control",
        target_id = actor_id,
        payload = list(
          amount = res$amount,
          hp_before = res$hp_before,
          hp_after = res$hp_after,
          temp_before = res$temp_before,
          temp_after = res$temp_after
        )
      )
      
      log_safe(paste0(
        "💥 Damaged active player for 2 HP (",
        res$hp_before, " → ", res$hp_after, ")."
      ))
      
      bump_refresh()
    }, ignoreInit = TRUE)
  })
}