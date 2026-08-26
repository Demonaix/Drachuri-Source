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
    sid <- ctrl$session_id %||% NA
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
  
  control_live_snapshot<-reactive({
    ctrl$refresh_key
    invalidateLater(3000, session)
    sid<-current_session_id()
    if(is.null(sid))return(empty_player_live_snapshot())
    tryCatch(get_player_live_snapshot(sid,"__control__",encounter_id=NULL,event_limit=20L),error=function(e){message("Control live snapshot failed: ",e$message);empty_player_live_snapshot()})
  })
  players_tbl <- reactive({
    control_live_snapshot()$players%||%data.frame()
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
  control_hud_state<-reactiveValues(active_session_id=NULL,active_encounter_id=NULL,char_id="__control__",offline_mode=FALSE)
  observe({control_hud_state$active_session_id<-current_session_id();s<-control_live_snapshot()$session%||%data.frame();eid<-if(nrow(s))suppressWarnings(as.integer(s$active_encounter_id[[1L]]%||%NA))else NA_integer_;control_hud_state$active_encounter_id<-if(is.na(eid))NULL else eid})
  partyHudServer("control_partyhud",control_hud_state,live_snapshot=control_live_snapshot,
                 portrait_base="player-assets/assets/player-posters")
  output$ctrl_active_session<-renderUI({
    sid<-current_session_id();span(class="control-kpi",paste0("Active session: ",sid%||%"none"))
  })
  control_notification_session<-reactiveVal(NA_integer_);control_notification_last_id<-reactiveVal(0L)
  observe({
    invalidateLater(2000,session);sid<-current_session_id();if(is.null(sid))return()
    if(!identical(as.integer(control_notification_session()),as.integer(sid))){existing<-get_session_notifications_after(sid,0L,500L);control_notification_session(as.integer(sid));control_notification_last_id(if(nrow(existing))max(as.integer(existing$id),na.rm=TRUE)else 0L);return()}
    rows<-get_session_notifications_after(sid,control_notification_last_id(),50L);if(!nrow(rows))return();control_notification_last_id(max(as.integer(rows$id),na.rm=TRUE))
    for(i in seq_len(nrow(rows))){r<-rows[i,,drop=FALSE];showNotification(paste0(as.character(r$source_name[[1L]]%||%"Player"),": ",as.character(r$message[[1L]])),type=as.character(r$notification_type[[1L]]%||%"message"),duration=8)}
  })
  # ------------------------------------------------------------
  # Top-level refresh control
  # ------------------------------------------------------------
  observeEvent(input$ctrl_refresh, {
    bump_refresh()
  }, ignoreInit = TRUE)
  
  # ------------------------------------------------------------
  # Source control modules
  # ------------------------------------------------------------
  source("control_app/modules/control_sessions_module.R", local = FALSE)
  source("control_app/modules/control_players_module.R", local = FALSE)
  source("control_app/modules/control_map_builder_module.R", local = FALSE)
  source("control_app/modules/control_encounter_generator_core.R", local = FALSE)
  source("control_app/modules/control_encounter_setup_module.R", local = FALSE)
  source("control_app/modules/control_live_combat_module.R", local = FALSE)
  source("control_app/modules/control_npc_creator_module.R", local = FALSE)
  source("control_app/modules/control_inventory_module.R", local = FALSE)
  source("control_app/modules/control_npc_pools_module.R", local = FALSE)
  source("control_app/modules/control_npc_features_module.R", local = FALSE)
  source("control_app/modules/control_npc_attacks_module.R", local = FALSE)
  source("control_app/modules/control_merchants_module.R", local = FALSE)
  source("control_app/modules/control_story_module.R", local = FALSE)
  source("control_app/modules/control_issue_reports_module.R", local = FALSE)
  source("control_app/modules/control_geography_climate_module.R", local = FALSE)
  
 
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
  controlGeographyClimateServer("geography_climate",ctrl,bump_refresh)
  
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
  controlNpcAttacksServer("npc_attacks")
  controlMerchantsServer("merchants",ctrl,players_tbl,bump_refresh)
  controlStoryServer("story",ctrl)
  controlIssueReportsServer("issue_reports")
  
}
