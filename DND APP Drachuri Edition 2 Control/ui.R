library(shiny)
library(shinyjs)

# Reuse the player's lean combat renderer instead of maintaining two divergent
# copies. The control app is launched with this directory as its working folder.
player_www <- normalizePath(
  file.path("..", "DND APP Drachuri Edition Player_v2", "www"),
  mustWork = TRUE
)
if (!"player-assets" %in% names(shiny::resourcePaths())) {
  shiny::addResourcePath("player-assets", player_www)
}

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
source("control_app/modules/control_story_module.R", local = FALSE)
source("../DND APP Drachuri Edition Player_v2/server/party_hud_module.R", local = FALSE)

ui_control <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$title("DND Control Dashboard"),
    tags$link(rel = "icon", type = "image/png", href = "drachuri-control-logo.png"),
    # Import maps must precede the module which imports `three`. Keeping both
    # in the head avoids browsers executing the module before a body importmap.
    tags$script(type = "importmap", HTML('
{
  "imports": {
    "three": "https://cdn.jsdelivr.net/npm/three@0.160.0/build/three.module.js",
    "three/addons/": "https://cdn.jsdelivr.net/npm/three@0.160.0/examples/jsm/"
  }
}
')),
    tags$script(type = "module", src = paste0("player-assets/js/combat3d_lean.js?v=", as.integer(Sys.time()))),
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
        max-width: none;
        margin: 18px 18px 18px 170px;
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
      .merchant-invite-panel { clear:both; margin-top:16px; padding-top:12px; border-top:1px solid rgba(191,167,111,.5); }
      .merchant-invite-actions { display:flex; gap:8px; flex-wrap:wrap; margin-top:10px; }
      @media (max-width: 900px) {
        .control-shell { margin:10px; padding:0 8px 18px; }
        .control-party-hud { position:relative; left:auto; top:auto; width:auto; max-height:220px; margin-bottom:12px; }
        #control_partyhud-partyhud_root { position:relative; top:auto; left:auto; width:156px; max-height:220px; margin-bottom:12px; }
      }

      .shiny-input-container {
        margin-bottom: 0;
      }

      table {
        background: rgba(255,255,245,0.94);
      }

      /* --------------------------------------------------
         DM desk theme
         -------------------------------------------------- */
      html, body {
        background: #17100b url('control-desk-bg.jpg') center top / cover fixed no-repeat;
      }
      body::before {
        content: '';
        position: fixed;
        inset: 0;
        pointer-events: none;
        background: linear-gradient(180deg, rgba(8,5,3,.08), rgba(8,5,3,.28));
        z-index: 0;
      }
      .control-shell {
        position: relative;
        z-index: 1;
        max-width: 1500px;
        margin: 24px 28px 40px 220px;
        padding: 0 18px 30px;
      }
      .control-hero {
        display: flex;
        align-items: center;
        gap: 18px;
        min-height: 104px;
        padding: 14px 20px;
        overflow: hidden;
        border: 1px solid rgba(219,181,104,.72);
        border-radius: 18px;
        background:
          linear-gradient(105deg, rgba(35,13,13,.97), rgba(67,20,23,.94) 54%, rgba(24,18,14,.96));
        box-shadow: 0 14px 38px rgba(0,0,0,.48), inset 0 0 0 2px rgba(255,235,183,.06);
        color: #fff5dc;
      }
      .control-brand-mark {
        width: 88px;
        height: 88px;
        flex: 0 0 88px;
        object-fit: cover;
        border-radius: 50%;
        border: 2px solid rgba(226,188,106,.8);
        box-shadow: 0 7px 20px rgba(0,0,0,.45), 0 0 22px rgba(147,25,28,.38);
      }
      .control-brand-copy { min-width: 260px; }
      .control-title {
        color: #fff2cc;
        font-size: clamp(22px, 2.2vw, 34px);
        letter-spacing: .055em;
        line-height: 1.08;
        text-shadow: 0 2px 3px rgba(0,0,0,.55);
      }
      .control-sub {
        margin-top: 7px;
        color: rgba(255,241,211,.74);
        font-family: Georgia, serif;
        font-size: 14px;
        letter-spacing: .035em;
      }
      .control-toolbar {
        margin: 0 0 0 auto;
        padding-left: 16px;
        align-items: center;
        justify-content: flex-end;
      }
      .control-toolbar .shiny-input-container { color: #fff4d8; }
      .control-toolbar .control-kpi {
        border-color: rgba(225,190,113,.62);
        background: rgba(15,9,8,.44);
        color: #f5dfac;
        box-shadow: inset 0 0 12px rgba(0,0,0,.2);
      }
      .control-toolbar .btn {
        border-color: rgba(225,190,113,.78);
        background: linear-gradient(#f4dfad, #c79a4d);
        color: #30180e;
        font-weight: 900;
        box-shadow: 0 5px 14px rgba(0,0,0,.32);
      }
      .control-shell > .tabbable > .nav-tabs {
        display: flex;
        gap: 5px;
        overflow-x: auto;
        margin: 14px 7px 0;
        padding: 7px 9px 0;
        border: 1px solid rgba(205,166,91,.55);
        border-bottom: 0;
        border-radius: 14px 14px 0 0;
        background: rgba(23,13,9,.92);
        box-shadow: 0 7px 22px rgba(0,0,0,.35);
        scrollbar-width: none;
      }
      .control-shell > .tabbable > .nav-tabs::-webkit-scrollbar { display: none; }
      .control-shell > .tabbable > .nav-tabs > li { flex: 0 0 auto; }
      .control-shell > .tabbable > .nav-tabs > li > a {
        margin: 0;
        padding: 11px 12px;
        border: 1px solid transparent;
        border-radius: 10px 10px 0 0;
        color: #ead6ab;
        font-size: 12px;
        letter-spacing: .025em;
      }
      .control-shell > .tabbable > .nav-tabs > li > a:hover {
        background: rgba(139,45,44,.38);
        border-color: rgba(217,177,99,.25);
        color: #fff1cc;
      }
      .control-shell > .tabbable > .nav-tabs > li.active > a {
        border-color: rgba(196,151,68,.72);
        border-bottom-color: #f4ecd9;
        background: #f4ecd9;
        color: #4a2319;
        box-shadow: inset 0 3px 0 #8e2d2d;
      }
      .control-shell > .tabbable > .tab-content {
        min-height: 520px;
        padding: 18px;
        border: 1px solid rgba(205,166,91,.68);
        border-radius: 0 0 18px 18px;
        background:
          radial-gradient(circle at 50% 0, rgba(255,255,255,.56), transparent 42%),
          linear-gradient(rgba(249,242,220,.97), rgba(232,216,179,.97));
        box-shadow: 0 18px 46px rgba(0,0,0,.48), inset 0 0 44px rgba(107,71,32,.08);
      }
      .control-card,
      .control-shell .well,
      .control-shell .panel,
      .control-shell .dataTables_wrapper {
        border-color: rgba(128,87,38,.27);
        background: rgba(255,252,239,.76);
        box-shadow: 0 5px 15px rgba(83,55,25,.09);
      }
      .control-shell h3, .control-shell h4, .control-section-title {
        color: #51251e;
        letter-spacing: .025em;
      }
      .control-shell hr { border-top-color: rgba(109,70,31,.2); }
      .control-shell .btn-primary { background:#386a79; border-color:#2d5865; }
      .control-shell .btn-success { background:#597d45; border-color:#496a38; }
      .control-shell .btn-danger { background:#8e3434; border-color:#702727; }
      .control-shell .form-control {
        border-color: rgba(112,74,33,.32);
        background: rgba(255,253,244,.92);
        box-shadow: inset 0 1px 2px rgba(62,38,17,.08);
      }
      #control_partyhud-partyhud_root {
        border-color: rgba(219,181,104,.62) !important;
        background: linear-gradient(180deg, rgba(38,16,16,.97), rgba(20,14,12,.97)) !important;
        box-shadow: 0 12px 30px rgba(0,0,0,.5) !important;
      }
      @media (max-width: 1050px) {
        .control-shell { margin: 14px; padding: 0 8px 22px; }
        .control-hero { flex-wrap: wrap; }
        .control-toolbar { width: 100%; margin-left: 104px; justify-content: flex-start; }
      }
      @media (max-width: 650px) {
        .control-brand-mark { width: 64px; height: 64px; flex-basis: 64px; }
        .control-brand-copy { min-width: 0; }
        .control-toolbar { margin-left: 0; }
        .control-shell > .tabbable > .tab-content { padding: 10px; }
      }
    "))
  ),
  
  div(
    class = "control-shell",
    partyHudUI("control_partyhud"),
    
    # --------------------------------------------------------
    # Header / global toolbar
    # --------------------------------------------------------
    div(
      class = "control-hero",
      tags$img(class = "control-brand-mark", src = "drachuri-control-logo.png", alt = "Drachuri Control"),
      div(
        class = "control-brand-copy",
        div(class = "control-title", "Drachuri Control"),
        div(class = "control-sub", "The Game Master's table — shape the world, direct the encounter.")
      ),
      
      div(
        class = "control-toolbar",
        uiOutput("ctrl_active_session"),
        actionButton("ctrl_refresh", "Refresh", class = "btn btn-default")
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
        title = "NPCs",
        tabsetPanel(
          id = "npc_tabs",
          tabPanel("Creator", controlNpcCreatorUI("npc_creator")),
          tabPanel("Pools", controlNpcPoolsUI("npc_pools")),
          tabPanel("Features", controlNpcFeaturesUI("npc_features")),
          tabPanel("Attacks", controlNpcAttacksUI("npc_attacks"))
        )
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
        title = "Story",
        controlStoryUI("story")
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
