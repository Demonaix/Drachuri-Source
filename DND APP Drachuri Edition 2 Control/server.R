server_control <- function(input, output, session) {
  `%||%` <- get("%||%", inherits = TRUE)
 

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
    sid <- ctrl$session_id %||% input$ctrl_session_id %||% NA
    sid <- suppressWarnings(as.integer(sid))
    if (is.na(sid) || sid < 1) return(NULL)
    sid
  })
  
  current_map_id <- reactive({
    mid <- ctrl$map_id %||% NA
    mid <- suppressWarnings(as.integer(mid))
    if (is.na(mid) || mid < 1) return(NULL)
    mid
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
    
    t0 <- Sys.time()
    
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
    
    message("get_session_overview took: ", round(as.numeric(Sys.time() - t0, units = "secs"), 3), " sec")
    
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
  observeEvent(input$ctrl_session_id, {
    sid <- suppressWarnings(as.integer(input$ctrl_session_id %||% NA))
    if (!is.na(sid) && sid > 0 && !identical(ctrl$session_id, sid)) {
      ctrl$session_id <- sid
    }
  }, ignoreInit = TRUE)
  
  # ------------------------------------------------------------
  # Source control modules
  # ------------------------------------------------------------
  source("control_app/modules/control_sessions_module.R", local = FALSE)
  source("control_app/modules/control_players_module.R", local = FALSE)
  source("control_app/modules/control_map_builder_module.R", local = FALSE)
  source("control_app/modules/control_encounter_setup_module.R", local = FALSE)
  source("control_app/modules/control_live_combat_module.R", local = FALSE)
  source("control_app/modules/control_npc_creator_module.R", local = FALSE)
  source("control_app/modules/control_inventory_module.R", local = FALSE)
  source("control_app/modules/control_npc_pools_module.R", local = FALSE)
  source("control_app/modules/control_npc_features_module.R", local = FALSE)
  
 
  # ------------------------------------------------------------
  # Wire modules
  # ------------------------------------------------------------
  controlSessionsServer(
    id = "sessions",
    ctrl = ctrl,
    session_tbl = session_tbl,
    players_tbl = players_tbl,
    bump_refresh = bump_refresh
  )
  
  controlPlayersServer(
    id = "players",
    ctrl = ctrl,
    session_tbl = session_tbl,
    players_tbl = players_tbl,
    positions_tbl = positions_tbl,
    bump_refresh = bump_refresh
  )
  
  controlMapBuilderServer(
    id = "map_builder",
    ctrl = ctrl,
    session_tbl = session_tbl,
    players_tbl = players_tbl,
    positions_tbl = positions_tbl,
    bump_refresh = bump_refresh
  )
  
  controlEncounterSetupServer(
    id = "encounter_setup",
    ctrl = ctrl,
    session_tbl = session_tbl,
    players_tbl = players_tbl,
    positions_tbl = positions_tbl,
    combat_tbl = combat_tbl,
    bump_refresh = bump_refresh
  )
  
  controlLiveCombatServer(
    id = "live_combat",
    ctrl = ctrl,
    session_tbl = session_tbl,
    players_tbl = players_tbl,
    positions_tbl = positions_tbl,
    combat_tbl = combat_tbl,
    events_tbl = events_tbl,
    bump_refresh = bump_refresh
  )
  
  
  controlNpcCreatorServer(
    id = "npc_creator",
    ctrl = ctrl,
    bump_refresh = bump_refresh
  )

  controlInventoryServer(
    id = "control_inventory",
    ctrl = ctrl,
    players_tbl = players_tbl,
    bump_refresh = bump_refresh
  )

  controlNpcPoolsServer("npc_pools")
  controlNpcFeaturesServer("npc_features")
  
}
