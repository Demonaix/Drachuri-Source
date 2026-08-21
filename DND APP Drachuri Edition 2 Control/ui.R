library(shiny)
library(shinyjs)

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
source("control_app/modules/control_npc_attacks_module.R", local = FALSE)
source("control_app/modules/control_merchants_module.R", local = FALSE)

ui_control <- fluidPage(
  useShinyjs(),
  
  tags$script(type = "importmap", HTML('
{
  "imports": {
    "three": "https://cdn.jsdelivr.net/npm/three@0.160.0/build/three.module.js",
    "three/addons/": "https://cdn.jsdelivr.net/npm/three@0.160.0/examples/jsm/"
  }
}
')),
  
  tags$head(
    tags$title("DND Control Dashboard"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Cinzel&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      html, body {
        min-height: 100%;
        margin: 0;
        background: #efe6d3;
        color: #3e2f1c;
        font-family: 'Cinzel', serif;
      }

      .control-shell {
        max-width: 1500px;
        margin: 18px auto;
        padding: 0 14px 20px 14px;
      }

      .control-hero {
        border: 1px solid rgba(191,167,111,0.78);
        background: rgba(255,255,245,0.95);
        border-radius: 16px;
        padding: 16px 18px;
        margin-bottom: 14px;
        box-shadow: 0 10px 26px rgba(0,0,0,0.10);
      }

      .control-title {
        font-size: 26px;
        font-weight: 900;
        margin-bottom: 4px;
      }

      .control-sub {
        font-size: 13px;
        opacity: 0.82;
      }

      .control-toolbar {
        display: flex;
        gap: 10px;
        align-items: flex-end;
        flex-wrap: wrap;
        margin-top: 14px;
      }

      .control-card {
        border: 1px solid rgba(191,167,111,0.72);
        background: rgba(255,255,245,0.94);
        border-radius: 14px;
        padding: 14px;
        margin-bottom: 14px;
        box-shadow: 0 8px 22px rgba(0,0,0,0.08);
      }

      .control-section-title {
        font-size: 18px;
        font-weight: 900;
        margin-bottom: 8px;
      }

      .nav-tabs {
        margin-bottom: 12px;
        border-bottom: 1px solid rgba(191,167,111,0.55);
      }

      .nav-tabs > li > a {
        font-weight: 800;
        color: #4a3822;
        border-radius: 10px 10px 0 0;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background: rgba(255,255,245,0.96);
        border: 1px solid rgba(191,167,111,0.72);
        border-bottom-color: transparent;
        color: #2f2315;
      }

      .tab-content > .tab-pane {
        padding-top: 4px;
      }

      .control-kpi-row {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        margin-top: 12px;
      }

      .control-kpi {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 7px 11px;
        border-radius: 999px;
        border: 1px solid rgba(191,167,111,0.72);
        background: rgba(255,255,245,0.92);
        font-size: 12px;
        font-weight: 800;
      }

      .control-mini {
        font-size: 12px;
        opacity: 0.82;
      }

      .control-party-hud { position:fixed; left:10px; top:120px; width:190px; max-height:calc(100vh - 145px); overflow-y:auto; z-index:900; padding:10px; border:1px solid rgba(191,167,111,.72); border-radius:12px; background:rgba(24,18,16,.94); color:#fff7df; box-shadow:0 6px 20px rgba(0,0,0,.35); }
      .control-party-hud h4 { margin:0 0 8px; font-size:14px; }
      .control-party-member { padding:7px; margin-bottom:6px; border-radius:8px; background:rgba(255,255,255,.08); font-size:12px; }
      .control-party-member.inactive { opacity:.5; }

      .shiny-input-container {
        margin-bottom: 0;
      }

      table {
        background: rgba(255,255,245,0.94);
      }
    "))
  ),
  
  div(
    class = "control-shell",
    uiOutput("control_party_hud"),
    
    # --------------------------------------------------------
    # Header / global toolbar
    # --------------------------------------------------------
    div(
      class = "control-hero",
      div(class = "control-title", "🎛️ DND Control Dashboard"),
      div(class = "control-sub", "GM controls for sessions, players, maps, encounter setup, and live combat."),
      
      div(
        class = "control-toolbar",
        numericInput("ctrl_session_id", "Session ID", value = 1, min = 1, width = "140px"),
        actionButton("ctrl_refresh", "Refresh", class = "btn btn-default")
      ),
      
      div(
        class = "control-kpi-row",
        div(class = "control-kpi", "Shared DB State"),
        div(class = "control-kpi", "Control App"),
        div(class = "control-kpi", "Modular UI")
      )
    ),

    
    # --------------------------------------------------------
    # Modular tabs
    # --------------------------------------------------------
    tabsetPanel(
      id = "control_tabs",
      
      tabPanel(
        title = "Sessions",
        controlSessionsUI("sessions")
      ),
      
      tabPanel(
        title = "Players",
        controlPlayersUI("players")
      ),
      
      tabPanel(
        title = "Map Builder",
        controlMapBuilderUI("map_builder")
      ),
      tabPanel(
        title = "NPC Creator",
        controlNpcCreatorUI("npc_creator")
      ),
      tabPanel(
        title = "Inventory",
        controlInventoryUI("control_inventory")
      ),
      tabPanel(
        title = "Merchants",
        controlMerchantsUI("merchants")
      ),
      tabPanel(
        title = "NPC Pools",
        controlNpcPoolsUI("npc_pools")
      ),
      tabPanel(
        title = "NPC Features",
        controlNpcFeaturesUI("npc_features")
      ),
      tabPanel(
        title = "NPC Attacks",
        controlNpcAttacksUI("npc_attacks")
      ),
      
      tabPanel(
        title = "Encounter Setup",
        controlEncounterSetupUI("encounter_setup")
      ),
      
      tabPanel(
        title = "Live Combat",
        controlLiveCombatUI("live_combat")
      )
    )
  )
)
