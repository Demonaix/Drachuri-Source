library(shiny)


controlEncounterSetupUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .enc-map-wrap{
        overflow:auto;
        max-width:100%;
        width:100%;
        max-height:60vh;
        padding:8px;
        border:1px solid rgba(191,167,111,0.45);
        border-radius:12px;
        background:rgba(255,255,245,0.8);
        box-sizing:border-box;
      }

      .enc-map-grid{
        display:grid;
        gap:0;
        width:max-content;
      }

      .enc-map-cell{
        position:relative;
        width:24px;
        height:24px;
        min-width:24px;
        min-height:24px;
        border:1px solid rgba(80,60,30,0.22);
        box-sizing:border-box;
      }

      .enc-map-token{
        position:absolute;
        left:4px;
        top:4px;
        width:14px;
        height:14px;
        border-radius:999px;
        display:flex;
        align-items:center;
        justify-content:center;
        font-size:9px;
        font-weight:900;
        color:white;
        z-index:3;
      }
    ")),
    
    div(
      class = "control-card",
      div(class = "control-section-title", "⚔️ Encounter Setup"),
      
      div(
        class = "control-card",
        h4("Session"),
        selectInput(ns("session_id"), "Session", choices = c())
      ),
      
      div(
        class = "control-card",
        h4("Encounter"),
        fluidRow(
          column(
            6,
            selectInput(ns("encounter_id"), "Encounter", choices = c())
          ),
          column(
            2,
            br(),
            actionButton(ns("delete_encounter"), "Delete Encounter", class = "btn btn-danger")
          )
        ),
        fluidRow(
          column(
            5,
            textInput(ns("new_encounter_name"), "New Encounter Name", placeholder = "Roadside Ambush")
          ),
          column(
            3,
            selectInput(ns("map_id"), "Map", choices = c(), width = "100%")
          ),
          column(
            3,
            br(),
            actionButton(ns("create_encounter"), "Create Encounter", class = "btn btn-success")
          )
        )
      ),
      
      div(
        class = "control-card",
        h4("Players in Session"),
        tableOutput(ns("players_tbl"))
      ),
      
      div(
        class = "control-card",
        h4("Enemies in Encounter"),
        
        fluidRow(
          column(
            6,
            selectInput(ns("npc_template_id"), "NPC / Enemy Template", choices = c())
          ),
          column(
            2,
            numericInput(ns("enemy_count"), "Count", value = 1, min = 1, max = 20)
          ),
          column(
            2,
            br(),
            actionButton(ns("refresh_npc_templates"), "Refresh", class = "btn btn-default")
          ),
          column(
            2,
            br(),
            actionButton(ns("add_enemy_from_template"), "Add", class = "btn btn-danger")
          )
        ),
        tags$br(),
        tags$br(),
        uiOutput(ns("enemies_ui"))
      ),
      div(
        class = "control-card",
        h4("Encounter Positions"),
        uiOutput(ns("positions_ui"))
      ),
      
      div(
        class = "control-card",
        h4("Map Preview"),
        uiOutput(ns("map_preview_ui"))
      ),
      
      div(
        class = "control-card",
        actionButton(ns("save_encounter"), "Save Encounter", class = "btn btn-primary")
      )
    )
  )
}

controlEncounterSetupServer <- function(
    id,
    ctrl,
    session_tbl = NULL,
    players_tbl = NULL,
    positions_tbl = NULL,
    combat_tbl = NULL,
    bump_refresh
) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    enemy_obs_ids <- reactiveVal(character())
    npc_templates_rv <- reactiveVal(data.frame())

    list_encounter_maps <- function() {
      con <- get_db_connection()
      if (is.null(con)) return(data.frame())
      on.exit(release_db_connection(con), add = TRUE)
      tryCatch(
        DBI::dbGetQuery(con, "select id as map_id, name as map_name, width, height from maps order by lower(name), id"),
        error = function(e) {
          message("list_encounter_maps failed: ", e$message)
          data.frame()
        }
      )
    }
    
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
    
    save_current_position_inputs <- function() {
      eid <- current_encounter_id()
      if (is.na(eid)) return(TRUE)
      
      pos <- preview_positions()
      if (!is.data.frame(pos) || nrow(pos) == 0) return(TRUE)
      
      ok_all <- TRUE
      
      for (i in seq_len(nrow(pos))) {
        ok <- tryCatch(
          upsert_encounter_actor_position(
            encounter_id = eid,
            actor_type = as.character(pos$actor_type[i]),
            actor_id = as.character(pos$actor_id[i]),
            x = as.integer(pos$x[i]),
            y = as.integer(pos$y[i])
          ),
          error = function(e) {
            message("save_current_position_inputs failed: ", e$message)
            FALSE
          }
        )
        
        if (!isTRUE(ok)) ok_all <- FALSE
      }
      
      ok_all
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
    
    load_npc_templates <- function() {
      df <- list_npc_templates()
      if (!is.data.frame(df)) df <- data.frame()
      npc_templates_rv(df)
    }
    
    observeEvent(TRUE, {
      load_npc_templates()
    }, once = TRUE)
    
    observeEvent(input$refresh_npc_templates, {
      load_npc_templates()
    }, ignoreInit = TRUE)
    
    observe({
      df <- npc_templates_rv()
      
      if (!is.data.frame(df) || nrow(df) == 0 || !"npc_id" %in% names(df) || !"name" %in% names(df)) {
        updateSelectInput(session, "npc_template_id", choices = c("No NPC templates found" = ""))
        return()
      }
      
      labels <- paste0(
        df$name,
        " • HP ", df$hp_max,
        " • AC ", df$ac
      )
      
      ids <- as.character(df$npc_id)
      selected_id <- as.character(input$npc_template_id %||% "")
      
      if (!nzchar(selected_id) || !selected_id %in% ids) {
        selected_id <- ids[1]
      }
      
      updateSelectInput(
        session,
        "npc_template_id",
        choices = stats::setNames(ids, labels),
        selected = selected_id
      )
    })
    

    
    current_session_id <- reactive({
      sid <- suppressWarnings(as.integer(ctrl$session_id %||% input$session_id %||% NA))
      if (is.na(sid) || sid < 1) return(NA_integer_)
      sid
    })
    
    current_encounter_id <- reactive({
      eid <- suppressWarnings(as.integer(ctrl$encounter_id %||% input$encounter_id %||% NA))
      if (is.na(eid) || eid < 1) return(NA_integer_)
      eid
    })
    
    current_encounters <- reactive({
      ctrl$refresh_key
      
      sid <- current_session_id()
      if (is.na(sid)) return(data.frame())
      
      df <- tryCatch(
        get_session_encounters(sid),
        error = function(e) {
          message("get_session_encounters failed: ", e$message)
          data.frame()
        }
      )
      
      if (!is.data.frame(df)) data.frame() else df
    })

    encounter_maps <- reactive({
      ctrl$refresh_key
      maps <- list_encounter_maps()
      if (!is.data.frame(maps)) data.frame() else maps
    })

    observe({
      maps <- encounter_maps()
      if (!nrow(maps)) {
        updateSelectInput(session, "map_id", choices = c())
        return()
      }
      ids <- as.character(maps$map_id)
      dimensions <- if (all(c("width", "height") %in% names(maps))) paste0(" • ", maps$width, "×", maps$height) else ""
      labels <- paste0(maps$map_name, " (#", ids, ")", dimensions)
      selected <- as.character(input$map_id %||% ctrl$map_id %||% "")
      if (!selected %in% ids) selected <- ids[[1L]]
      updateSelectInput(session, "map_id", choices = stats::setNames(ids, labels), selected = selected)
    })
    
    current_players <- reactive({
      ctrl$refresh_key
      
      sid <- current_session_id()
      if (is.na(sid)) return(data.frame())
      
      df <- NULL
      
      if (!is.null(players_tbl) && is.reactive(players_tbl)) {
        df <- tryCatch(players_tbl(), error = function(e) data.frame())
      } else {
        df <- tryCatch(
          get_session_players(sid),
          error = function(e) {
            message("get_session_players failed: ", e$message)
            data.frame()
          }
        )
      }
      
      if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
      
      if ("session_id" %in% names(df)) {
        df <- df[suppressWarnings(as.integer(df$session_id)) == sid, , drop = FALSE]
      }
      
      if (!is.data.frame(df)) data.frame() else df
    })
    
    current_enemies <- reactive({
      ctrl$refresh_key
      
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- tryCatch(
        get_encounter_enemies(eid),
        error = function(e) {
          message("get_encounter_enemies failed: ", e$message)
          data.frame()
        }
      )
      
      if (!is.data.frame(df)) data.frame() else df
    })
    
  
    
    observeEvent(input$delete_encounter, {
      sid <- current_session_id()
      eid <- current_encounter_id()
      
      if (is.na(sid) || sid < 1 || is.na(eid) || eid < 1) {
        showNotification("Choose an encounter to delete.", type = "error")
        return()
      }
      
      ok <- tryCatch(
        {
          if (exists("delete_encounter", mode = "function")) {
            delete_encounter(eid)
          } else if (exists("remove_encounter", mode = "function")) {
            remove_encounter(eid)
          } else {
            stop("No delete_encounter/remove_encounter helper exists.")
          }
        },
        error = function(e) {
          message("delete encounter failed: ", e$message)
          FALSE
        }
      )
      
      if (!isTRUE(ok)) {
        showNotification("Failed to delete encounter. Missing helper or DB error.", type = "error")
        return()
      }
      
      ctrl$encounter_id <- NULL
      ctrl$map_id <- NULL
      enemy_obs_ids(character())
      bump_refresh()
      
      showNotification("Encounter deleted.", type = "message")
    })
    
  
    
    current_positions <- reactive({
      ctrl$refresh_key
      
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- NULL
      
      if (!is.null(positions_tbl) && is.reactive(positions_tbl)) {
        df <- tryCatch(positions_tbl(), error = function(e) data.frame())
      } else {
        df <- tryCatch(
          get_encounter_positions(eid),
          error = function(e) {
            message("get_encounter_positions failed: ", e$message)
            data.frame()
          }
        )
      }
      
      if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
      
      if ("encounter_id" %in% names(df)) {
        df <- df[suppressWarnings(as.integer(df$encounter_id)) == eid, , drop = FALSE]
      }
      
      if (!is.data.frame(df)) data.frame() else df
    })
    
    current_encounter_row <- reactive({
      eid <- current_encounter_id()
      sid <- current_session_id()
      
      if (is.na(eid) || is.na(sid)) return(data.frame())
      
      df <- tryCatch(get_encounter(eid), error = function(e) data.frame())
      if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
      
      row_sid <- suppressWarnings(as.integer(df$session_id[1] %||% NA))
      if (is.na(row_sid) || !identical(row_sid, sid)) return(data.frame())
      
      df
    })
    
    preview_positions <- reactive({
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      players <- current_players()
      enemies <- current_enemies()
      saved_pos <- current_positions()
      
      out <- data.frame(
        encounter_id = integer(),
        actor_type = character(),
        actor_id = character(),
        x = integer(),
        y = integer(),
        stringsAsFactors = FALSE
      )
      
      if (is.data.frame(players) && nrow(players) > 0) {
        for (i in seq_len(nrow(players))) {
          row <- players[i, , drop = FALSE]
          cid <- as.character(row$character_id[1] %||% "")
          if (!nzchar(cid)) next
          
          default_x <- i
          default_y <- 1L
          
          if (is.data.frame(saved_pos) && nrow(saved_pos) > 0) {
            prow <- saved_pos[
              as.character(saved_pos$actor_type) == "player" &
                as.character(saved_pos$actor_id) == cid,
              ,
              drop = FALSE
            ]
            
            if (nrow(prow) > 0) {
              default_x <- suppressWarnings(as.integer(prow$x[1] %||% default_x))
              default_y <- suppressWarnings(as.integer(prow$y[1] %||% default_y))
            }
          }
          
          x_key <- paste0("player_pos_x_", cid)
          y_key <- paste0("player_pos_y_", cid)
          
          x_val <- suppressWarnings(as.integer(input[[x_key]] %||% default_x))
          y_val <- suppressWarnings(as.integer(input[[y_key]] %||% default_y))
          
          if (is.na(x_val) || x_val < 1) x_val <- default_x
          if (is.na(y_val) || y_val < 1) y_val <- default_y
          
          out <- rbind(
            out,
            data.frame(
              encounter_id = as.integer(eid),
              actor_type = "player",
              actor_id = cid,
              x = as.integer(x_val),
              y = as.integer(y_val),
              stringsAsFactors = FALSE
            )
          )
        }
      }
      
      if (is.data.frame(enemies) && nrow(enemies) > 0) {
        for (i in seq_len(nrow(enemies))) {
          row <- enemies[i, , drop = FALSE]
          enemy_id <- as.character(row$enemy_uuid[1] %||% row$actor_id[1] %||% "")
          if (!nzchar(enemy_id)) next
          
          default_x <- i
          default_y <- 5L
          
          if (is.data.frame(saved_pos) && nrow(saved_pos) > 0) {
            erow <- saved_pos[
              as.character(saved_pos$actor_type) == "enemy" &
                as.character(saved_pos$actor_id) == enemy_id,
              ,
              drop = FALSE
            ]
            
            if (nrow(erow) > 0) {
              default_x <- suppressWarnings(as.integer(erow$x[1] %||% default_x))
              default_y <- suppressWarnings(as.integer(erow$y[1] %||% default_y))
            }
          }
          
          x_key <- paste0("enemy_pos_x_", enemy_id)
          y_key <- paste0("enemy_pos_y_", enemy_id)
          
          x_val <- suppressWarnings(as.integer(input[[x_key]] %||% default_x))
          y_val <- suppressWarnings(as.integer(input[[y_key]] %||% default_y))
          
          if (is.na(x_val) || x_val < 1) x_val <- default_x
          if (is.na(y_val) || y_val < 1) y_val <- default_y
          
          out <- rbind(
            out,
            data.frame(
              encounter_id = as.integer(eid),
              actor_type = "enemy",
              actor_id = enemy_id,
              x = as.integer(x_val),
              y = as.integer(y_val),
              stringsAsFactors = FALSE
            )
          )
        }
      }
      
      out
    })
    
    observe({
      ctrl$refresh_key
      
      sess <- tryCatch(
        get_all_sessions(),
        error = function(e) {
          message("get_all_sessions failed: ", e$message)
          data.frame()
        }
      )
      
      if (!is.data.frame(sess) || nrow(sess) == 0) {
        updateSelectInput(session, "session_id", choices = c())
        return()
      }
      
      id_col <- if ("session_id" %in% names(sess)) "session_id" else if ("id" %in% names(sess)) "id" else NULL
      if (is.null(id_col) || !"name" %in% names(sess)) {
        updateSelectInput(session, "session_id", choices = c())
        return()
      }
      
      ids <- as.character(sess[[id_col]])
      labels <- paste0(sess$name, " (#", ids, ")")
      choices <- stats::setNames(ids, labels)
      
      selected_id <- as.character(ctrl$session_id %||% input$session_id %||% "")
      if (!nzchar(selected_id) || !selected_id %in% ids) {
        selected_id <- ids[1]
      }
      
      updateSelectInput(session, "session_id", choices = choices, selected = selected_id)
    })
    
    observeEvent(input$session_id, {
      sid <- suppressWarnings(as.integer(input$session_id %||% NA))
      if (is.na(sid) || sid < 1) return()
      
      ctrl$session_id <- sid
      ctrl$encounter_id <- NULL
      ctrl$map_id <- NULL
      enemy_obs_ids(character())
      bump_refresh()
    }, ignoreInit = TRUE)
    
    observe({
      sid <- current_session_id()
      
      if (is.na(sid)) {
        updateSelectInput(session, "encounter_id", choices = c())
        return()
      }
      
      enc <- current_encounters()
      if (!is.data.frame(enc) || nrow(enc) == 0) {
        updateSelectInput(session, "encounter_id", choices = c())
        return()
      }
      
      id_col <- if ("encounter_id" %in% names(enc)) "encounter_id" else if ("id" %in% names(enc)) "id" else NULL
      name_col <- if ("name" %in% names(enc)) "name" else NULL
      
      if (is.null(id_col) || is.null(name_col)) {
        updateSelectInput(session, "encounter_id", choices = c())
        return()
      }
      
      ids <- as.character(enc[[id_col]])
      labels <- paste0(enc[[name_col]], " (#", ids, ")")
      choices <- stats::setNames(ids, labels)
      
      selected_id <- as.character(input$encounter_id %||% ctrl$encounter_id %||% "")
      
      if (!nzchar(selected_id) || !selected_id %in% ids) {
        selected_id <- ids[1]
      }
      
      updateSelectInput(session, "encounter_id", choices = choices, selected = selected_id)
    })
    
    observeEvent(input$encounter_id, {
      eid <- suppressWarnings(as.integer(input$encounter_id %||% NA))
      if (is.na(eid) || eid < 1) return()
      
      sid <- current_session_id()
      
      ctrl$encounter_id <- eid
      
      if (!is.na(sid) && sid > 0L) {
        ctrl$session_id <- sid
        ctrl$active_session_id <- sid
      }
      
      enemy_obs_ids(character())
      
      if (is.function(bump_refresh)) bump_refresh()
    }, ignoreInit = TRUE)
    
    observeEvent(current_encounter_id(), {
      eid <- current_encounter_id()
      if (is.na(eid)) return()
      
      enc <- current_encounter_row()
      if (!is.data.frame(enc) || nrow(enc) == 0) return()
      
      map_val <- suppressWarnings(as.integer(enc$map_id[1] %||% 1L))
      if (is.na(map_val) || map_val < 1) map_val <- 1L
      
      ctrl$map_id <- map_val
      updateSelectInput(session, "map_id", selected = as.character(map_val))
    }, ignoreInit = FALSE)
    
    observeEvent(input$create_encounter, {
      sid <- current_session_id()
      if (is.na(sid)) {
        showNotification("Choose a session first.", type = "error")
        return()
      }
      
      nm <- as.character(input$new_encounter_name %||% "")
      if (!nzchar(nm)) {
        nm <- paste0("Encounter ", format(Sys.time(), "%Y-%m-%d %H:%M"))
      }
      
      map_id <- suppressWarnings(as.integer(input$map_id %||% NA))
      if (is.na(map_id) || map_id < 1) {
        showNotification("Choose a map first.", type = "error")
        return()
      }
      
      eid <- tryCatch(
        create_encounter(
          session_id = sid,
          name = nm,
          map_id = map_id,
          status = "setup"
        ),
        error = function(e) {
          message("create_encounter failed: ", e$message)
          NULL
        }
      )
      
      if (is.null(eid)) {
        showNotification("Failed to create encounter.", type = "error")
        return()
      }
      
      ctrl$encounter_id <- as.integer(eid)
      ctrl$map_id <- map_id
      ctrl$active_session_id <- sid
      enemy_obs_ids(character())
      bump_refresh()
      
      updateSelectInput(session, "encounter_id", selected = as.character(eid))
      showNotification(paste0("Encounter created: ", nm), type = "message")
    }, ignoreInit = TRUE)
    
    output$players_tbl <- renderTable({
      df <- current_players()
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
      
      keep <- intersect(
        c("character_id", "display_name", "current_hp", "temp_hp", "initiative", "turn_order", "is_active"),
        names(df)
      )
      
      df[, keep, drop = FALSE]
    }, striped = TRUE, bordered = TRUE)
    
    observeEvent(input$add_enemy_from_template, {
      
 
      eid <- current_encounter_id()
      if (is.na(eid)) {
        showNotification("Create or choose an encounter first.", type = "error")
        return()
      }
      save_current_position_inputs()
      npc_id <- as.character(input$npc_template_id %||% "")
      if (!nzchar(npc_id)) {
        showNotification("Choose an NPC template first.", type = "error")
        return()
      }
      
      npc <- get_npc_template(npc_id)
      if (!is.data.frame(npc) || nrow(npc) == 0) {
        showNotification("NPC template not found.", type = "error")
        return()
      }
      
      count <- suppressWarnings(as.integer(input$enemy_count %||% 1L))
      if (is.na(count) || count < 1L) count <- 1L
      
      existing_pos <- preview_positions()
      next_x <- 5L
      
      if (is.data.frame(existing_pos) && nrow(existing_pos) > 0 && "x" %in% names(existing_pos)) {
        x_vals <- suppressWarnings(as.integer(existing_pos$x))
        x_vals <- x_vals[!is.na(x_vals)]
        if (length(x_vals)) next_x <- max(x_vals) + 1L
      }
      
      added <- 0L
      
      for (i in seq_len(count)) {
        base_name <- as.character(npc$name[1] %||% "Enemy")
        enemy_name <- if (count > 1L) paste0(base_name, " ", i) else base_name
        
        enemy_uuid <- tryCatch(
          add_encounter_enemy(
            encounter_id = eid,
            name = enemy_name,
            hp_max = as.integer(npc$hp_max[1] %||% 10L),
            ac = as.integer(npc$ac[1] %||% 12L),
            movement_speed = as.integer(npc$movement_speed[1] %||% 30L),
            attack_name = as.character(npc$attack_name[1] %||% "Attack"),
            attack_bonus = as.integer(npc$attack_bonus[1] %||% 0L),
            damage_expr = as.character(npc$damage_expr[1] %||% "1d4"),
            damage_type = as.character(npc$damage_type[1] %||% "bludgeoning"),
            attacks_json = as.character(npc$attacks_json[1] %||% ""),
            template_key = as.character(npc$npc_id[1]), enemy_type = as.character(npc$enemy_type[1] %||% "Custom"),
            characteristics = enemy_db_json(npc$characteristics[[1]], list()), abilities = enemy_db_json(npc$abilities[[1]], list()),
            attacks = enemy_db_json(npc$attacks[[1]], list()), loot = enemy_db_json(npc$loot[[1]], list()),
            resistances = enemy_db_values(npc$resistances[[1]]), immunities = enemy_db_values(npc$immunities[[1]]),
            vulnerabilities = enemy_db_values(npc$vulnerabilities[[1]]), condition_immunities = enemy_db_values(npc$condition_immunities[[1]]),
            gold_min = as.integer(npc$gold_min[1] %||% 0L), gold_max = as.integer(npc$gold_max[1] %||% 0L)
          ),
          error = function(e) {
            message("add_encounter_enemy from NPC template failed: ", e$message)
            NULL
          }
        )
        
        if (is.null(enemy_uuid) || !nzchar(enemy_uuid)) next
        
        ok_pos <- tryCatch(
          upsert_encounter_actor_position(
            encounter_id = eid,
            actor_type = "enemy",
            actor_id = enemy_uuid,
            x = next_x + i - 1L,
            y = 5L
          ),
          error = function(e) {
            message("upsert encounter enemy position failed: ", e$message)
            FALSE
          }
        )
        
        if (isTRUE(ok_pos)) added <- added + 1L
      }
      
      bump_refresh()
      showNotification(paste0("Added ", added, " enemy/enemies."), type = "message")
    }, ignoreInit = TRUE)
    
    output$enemies_ui <- renderUI({
      df <- current_enemies()
      
      if (!is.data.frame(df) || nrow(df) == 0) {
        return(tags$em("No enemies added yet."))
      }
      
      cards <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, , drop = FALSE]
        
        enemy_uuid <- as.character(row$enemy_uuid[1] %||% row$actor_id[1] %||% "")
        nm         <- as.character(row$name[1] %||% row$display_name[1] %||% "Enemy")
        hp_cur     <- suppressWarnings(as.integer(row$hp_current[1] %||% row$current_hp[1] %||% 0))
        hp_max     <- suppressWarnings(as.integer(row$hp_max[1] %||% 0))
        ac         <- suppressWarnings(as.integer(row$ac[1] %||% 0))
        spd        <- suppressWarnings(as.integer(row$movement_speed[1] %||% 30))
        atk_name   <- as.character(row$attack_name[1] %||% "Attack")
        atk_hit    <- suppressWarnings(as.integer(row$attack_bonus[1] %||% 0))
        atk_dmg    <- as.character(row$damage_expr[1] %||% "—")
        atk_type   <- as.character(row$damage_type[1] %||% "—")
        
        if (is.na(hp_cur)) hp_cur <- 0L
        if (is.na(hp_max)) hp_max <- 0L
        if (is.na(ac)) ac <- 0L
        if (is.na(spd)) spd <- 30L
        if (is.na(atk_hit)) atk_hit <- 0L
        
        div(
          class = "control-card",
          div(
            style = "display:flex; justify-content:space-between; align-items:flex-start; gap:10px;",
            div(
              tags$strong(nm),
              tags$div(
                style = "font-size:12px; opacity:.8;",
                paste0("HP ", hp_cur, "/", hp_max, " • AC ", ac, " • Speed ", spd)
              ),
              tags$div(
                style = "font-size:12px; opacity:.8; margin-top:2px;",
                paste0(atk_name, " • Hit ", ifelse(atk_hit >= 0, "+", ""), atk_hit, " • ", atk_dmg, " ", atk_type)
              )
            ),
            actionButton(
              session$ns(paste0("remove_enemy_", enemy_uuid)),
              "Remove",
              class = "btn btn-danger btn-sm"
            )
          )
        )
      })
      
      tagList(cards)
    })
    
    observeEvent(current_encounter_id(), {
      enemy_obs_ids(character())
    }, ignoreInit = TRUE)
    
    observe({
      df <- current_enemies()
      if (!is.data.frame(df) || nrow(df) == 0) return()
      
      ids <- as.character(df$enemy_uuid %||% df$actor_id %||% "")
      ids <- ids[nzchar(ids)]
      
      new_ids <- setdiff(ids, enemy_obs_ids())
      if (!length(new_ids)) return()
      
      for (enemy_id in new_ids) {
        local({
          this_enemy_id <- enemy_id
          
          observeEvent(input[[paste0("remove_enemy_", this_enemy_id)]], {
            enc_id <- current_encounter_id()
            if (is.na(enc_id)) {
              showNotification("No active encounter selected.", type = "error")
              return()
            }
            
            ok_enemy <- tryCatch(
              remove_encounter_enemy(this_enemy_id),
              error = function(e) {
                message("remove_encounter_enemy failed: ", e$message)
                FALSE
              }
            )
            
            ok_pos <- TRUE
            if (exists("remove_encounter_actor_position", mode = "function")) {
              ok_pos <- tryCatch(
                remove_encounter_actor_position(
                  encounter_id = enc_id,
                  actor_type = "enemy",
                  actor_id = this_enemy_id
                ),
                error = function(e) {
                  message("remove_encounter_actor_position failed: ", e$message)
                  FALSE
                }
              )
            }
            
            if (isTRUE(ok_enemy) && isTRUE(ok_pos)) {
              bump_refresh()
              showNotification("Enemy removed.", type = "message")
            } else {
              showNotification("Failed to remove enemy.", type = "error")
            }
          }, ignoreInit = TRUE)
        })
      }
      
      enemy_obs_ids(unique(c(enemy_obs_ids(), new_ids)))
    })
    
    output$positions_ui <- renderUI({
      eid <- current_encounter_id()
      if (is.na(eid)) {
        return(tags$em("Create or choose an encounter first."))
      }
      
      players <- current_players()
      enemies <- current_enemies()
      pos <- current_positions()
      
      ui_parts <- list()
      
      if (is.data.frame(players) && nrow(players) > 0) {
        player_rows <- lapply(seq_len(nrow(players)), function(i) {
          row <- players[i, , drop = FALSE]
          
          cid <- as.character(row$character_id[1] %||% "")
          nm  <- as.character(row$display_name[1] %||% "Player")
          
          x_val <- i
          y_val <- 1L
          
          if (is.data.frame(pos) && nrow(pos) > 0) {
            prow <- pos[
              as.character(pos$actor_type) == "player" &
                as.character(pos$actor_id) == cid,
              ,
              drop = FALSE
            ]
            
            if (nrow(prow) > 0) {
              x_val <- suppressWarnings(as.integer(prow$x[1] %||% x_val))
              y_val <- suppressWarnings(as.integer(prow$y[1] %||% y_val))
            }
          }
          
          if (is.na(x_val) || x_val < 1) x_val <- i
          if (is.na(y_val) || y_val < 1) y_val <- 1L
          
          fluidRow(
            column(4, tags$strong(paste0(nm, " (Player)"))),
            column(3, numericInput(session$ns(paste0("player_pos_x_", cid)), "X", value = x_val, min = 1)),
            column(3, numericInput(session$ns(paste0("player_pos_y_", cid)), "Y", value = y_val, min = 1))
          )
        })
        
        ui_parts <- c(ui_parts, list(tags$h5("Players")), player_rows)
      }
      
      if (is.data.frame(enemies) && nrow(enemies) > 0) {
        enemy_rows <- lapply(seq_len(nrow(enemies)), function(i) {
          row <- enemies[i, , drop = FALSE]
          
          enemy_id <- as.character(row$enemy_uuid[1] %||% row$actor_id[1] %||% "")
          nm       <- as.character(row$name[1] %||% row$display_name[1] %||% "Enemy")
          
          x_val <- i
          y_val <- 5L
          
          if (is.data.frame(pos) && nrow(pos) > 0) {
            erow <- pos[
              as.character(pos$actor_type) == "enemy" &
                as.character(pos$actor_id) == enemy_id,
              ,
              drop = FALSE
            ]
            
            if (nrow(erow) > 0) {
              x_val <- suppressWarnings(as.integer(erow$x[1] %||% x_val))
              y_val <- suppressWarnings(as.integer(erow$y[1] %||% y_val))
            }
          }
          
          if (is.na(x_val) || x_val < 1) x_val <- i
          if (is.na(y_val) || y_val < 1) y_val <- 5L
          
          fluidRow(
            column(4, tags$strong(paste0(nm, " (Enemy)"))),
            column(3, numericInput(session$ns(paste0("enemy_pos_x_", enemy_id)), "X", value = x_val, min = 1)),
            column(3, numericInput(session$ns(paste0("enemy_pos_y_", enemy_id)), "Y", value = y_val, min = 1))
          )
        })
        
        ui_parts <- c(ui_parts, list(tags$h5("Enemies")), enemy_rows)
      }
      
      if (!length(ui_parts)) {
        return(tags$em("No actors to position."))
      }
      
      tagList(ui_parts)
    })
    
    observeEvent(input$save_encounter, {
      sid <- current_session_id()
      eid <- current_encounter_id()
      map_id <- suppressWarnings(as.integer(input$map_id %||% NA))
      
      if (is.na(sid) || sid < 1) {
        showNotification("Choose a valid session first.", type = "error")
        return()
      }
      
      if (is.na(eid) || eid < 1) {
        showNotification("Create or choose an encounter first.", type = "error")
        return()
      }
      
      if (is.na(map_id) || map_id < 1) {
        showNotification("Choose a valid map first.", type = "error")
        return()
      }
      
      players <- current_players()
      enemies <- current_enemies()
      ok_all <- TRUE
      
      if (is.data.frame(players) && nrow(players) > 0) {
        for (i in seq_len(nrow(players))) {
          row <- players[i, , drop = FALSE]
          cid <- as.character(row$character_id[1] %||% "")
          
          x_key <- paste0("player_pos_x_", cid)
          y_key <- paste0("player_pos_y_", cid)
          
          x_val <- suppressWarnings(as.integer(input[[x_key]] %||% i))
          y_val <- suppressWarnings(as.integer(input[[y_key]] %||% 1L))
          
          if (!nzchar(cid) || is.na(x_val) || is.na(y_val)) {
            ok_all <- FALSE
            next
          }
          
          ok_pos <- tryCatch(
            upsert_encounter_actor_position(
              encounter_id = eid,
              actor_type = "player",
              actor_id = cid,
              x = x_val,
              y = y_val
            ),
            error = function(e) {
              message("player upsert_encounter_actor_position failed: ", e$message)
              FALSE
            }
          )
          
          if (!isTRUE(ok_pos)) ok_all <- FALSE
        }
      }
      
      if (is.data.frame(enemies) && nrow(enemies) > 0) {
        for (i in seq_len(nrow(enemies))) {
          row <- enemies[i, , drop = FALSE]
          enemy_id <- as.character(row$enemy_uuid[1] %||% row$actor_id[1] %||% "")
          
          x_key <- paste0("enemy_pos_x_", enemy_id)
          y_key <- paste0("enemy_pos_y_", enemy_id)
          
          x_val <- suppressWarnings(as.integer(input[[x_key]] %||% i))
          y_val <- suppressWarnings(as.integer(input[[y_key]] %||% 5L))
          
          if (!nzchar(enemy_id) || is.na(x_val) || is.na(y_val)) {
            ok_all <- FALSE
            next
          }
          
          ok_epos <- tryCatch(
            upsert_encounter_actor_position(
              encounter_id = eid,
              actor_type = "enemy",
              actor_id = enemy_id,
              x = x_val,
              y = y_val
            ),
            error = function(e) {
              message("enemy upsert_encounter_actor_position failed: ", e$message)
              FALSE
            }
          )
          
          if (!isTRUE(ok_epos)) ok_all <- FALSE
        }
      }
      
      ok_map <- tryCatch(set_encounter_map(eid, map_id), error = function(e) FALSE)
      ok_status <- tryCatch(set_encounter_status(eid, status = "setup"), error = function(e) FALSE)
      ok_active <- TRUE
      
      if (!isTRUE(ok_all) || !isTRUE(ok_map) || !isTRUE(ok_status) || !isTRUE(ok_active)) {
        showNotification("Encounter save partly failed. Check helper logs.", type = "error")
        return()
      }
      
      ctrl$session_id <- sid
      ctrl$encounter_id <- eid
      ctrl$map_id <- map_id
      bump_refresh()
      
      showNotification("⚔️ Encounter saved. Make it active from Live Combat when ready.", type = "message")
    }, ignoreInit = TRUE)
    
    observeEvent(ctrl$refresh_key, {
      load_npc_templates()
    }, ignoreInit = TRUE)
    
    output$map_preview_ui <- renderUI({
      eid <- current_encounter_id()
      mid <- suppressWarnings(as.integer(input$map_id %||% ctrl$map_id %||% NA))
      if (is.na(mid) || mid < 1) return(tags$em("Choose a map to preview it."))
      
      tiles <- tryCatch(get_map_tiles(mid), error = function(e) data.frame())
      if (!is.data.frame(tiles) || nrow(tiles) == 0) {
        tiles <- create_square_map_tiles(
          map_id = mid,
          width = 10L,
          height = 10L,
          default_terrain = "grass",
          default_fog = 0L,
          default_light = "full",
          default_move_cost = 1,
          default_blocks_movement = FALSE,
          default_blocks_vision = FALSE
        )
      }
      
      pos <- preview_positions()
      occ <- empty_map_occupants()
      
      if (!is.na(eid) && is.data.frame(pos) && nrow(pos) > 0) {
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
      }
      
      render_df <- build_map_render_df(
        tiles = tiles,
        occupants = occ,
        map_id = mid
      )
      
      if (!is.data.frame(render_df) || nrow(render_df) == 0) {
        return(tags$em("No map loaded."))
      }
      
      render_df <- render_df[order(render_df$y, render_df$x), , drop = FALSE]
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
        
        if (fog == 1L) return("position:absolute; inset:0; background: rgba(20,20,20,0.82); z-index:1;")
        if (light == "dark") return("position:absolute; inset:0; background: rgba(20,20,20,0.52); z-index:1;")
        if (light == "dim") return("position:absolute; inset:0; background: rgba(80,80,80,0.16); z-index:1;")
        "position:absolute; inset:0; background: transparent; z-index:1;"
      }
      
      cells_ui <- lapply(seq_len(nrow(render_df)), function(i) {
        t <- render_df[i, , drop = FALSE]
        
        occ_lbl <- ""
        occ_bg <- "transparent"
        
        if (!is.na(t$occupant_id[1]) && nzchar(as.character(t$occupant_id[1]))) {
          occ_type <- as.character(t$occupant_type[1] %||% "player")
          if (identical(occ_type, "enemy")) {
            occ_lbl <- "E"
            occ_bg <- "rgba(160,40,40,0.88)"
          } else {
            occ_lbl <- "P"
            occ_bg <- "rgba(30,80,180,0.88)"
          }
        }
        
        tags$div(
          class = "enc-map-cell",
          style = paste0(
            "background:", terrain_col(t$terrain[1]), ";",
            if (isTRUE(t$blocks_movement[1])) "box-shadow: inset 0 0 0 2px rgba(60,20,20,0.65);" else ""
          ),
          tags$div(style = light_overlay(t$light[1], t$fog[1])),
          tags$div(
            style = "
    position:absolute;
    left:2px;
    top:1px;
    font-size:7px;
    color:rgba(0,0,0,0.55);
    z-index:2;
    pointer-events:none;
  ",
            paste0(t$x[1], ",", t$y[1])
          ),
          if (nzchar(occ_lbl)) {
            tags$div(
              class = "enc-map-token",
              style = paste0("background:", occ_bg, ";"),
              occ_lbl
            )
          }
        )
      })
      
      tagList(
        div(
          class = "enc-map-wrap",
          tags$div(
            class = "enc-map-grid",
            style = paste0("grid-template-columns: repeat(", bounds$width, ", 24px);"),
            cells_ui
          )
        ),
        tags$div(
          style = "margin-top:8px; font-size:12px; opacity:.8;",
          paste0("Map ID ", mid, " • P = player • E = enemy • preview updates from current position inputs")
        )
      )
    })
  })
}
