# ui.R
source("library.R")
source("plug/game_data.R")
source("shared/class_feature_core.R")

source("server/skills_module.R")
source("server/inventory_module.R")
source("server/armoury_module.R")
source("server/diary_module.R")
source("server/sidebar_module.R")
source("server/camp_module.R")
source("server/landing_module.R")
source("server/private_notes_module.R")
source("server/merchant_module.R")
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

dnd_3d_enabled <- identical(tolower(Sys.getenv("DND_ENABLE_3D", "false")), "true")
if (dnd_3d_enabled) {
  source("server/character_3d_module.R", local = FALSE)
} else {
  # Keep the navigation contract stable while avoiding the large model and
  # colourpicker dependency chain until the 3D module is deliberately enabled.
  source("server/character_3d_disabled_module.R", local = FALSE)
}

ui_player <- fluidPage(
  responsive = TRUE,
  useShinyjs(),
  tags$script(type = "importmap", HTML('
{
  "imports": {
    "three": "https://cdn.jsdelivr.net/npm/three@0.160.0/build/three.module.js",
    "three/addons/": "https://cdn.jsdelivr.net/npm/three@0.160.0/examples/jsm/"
  }
}
')),
  
  tags$script(
    type = "module",
    src = "js/character-preview.js"
  ),
  tags$head(
    tags$script(HTML("
(function(){
  try {
    var el = document.getElementById('returnUrl');
    if(!el){
      el = document.createElement('a');
      el.id = 'returnUrl';
      el.href = '#';
      el.style.display = 'none';
      (document.documentElement || document).appendChild(el);
    }
  } catch(e) {}
})();
")),

    tags$link(rel = "icon", href = "favicon.ico"),

    tags$script(HTML("
document.addEventListener('click', function(e){
  var el = e.target;
  console.log('[CLICK HIT]', el.tagName, el.id || '(no id)', el.className || '(no class)');
}, false);
")),

    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Cinzel&display=swap",
      rel = "stylesheet"
    ),

    # Music controller
    tags$script(src = "music.js"),

    # -----------------------------
    # CSS
    # -----------------------------
    tags$style(HTML("
.magic-cards{
  display: grid;
  grid-template-columns: repeat(5, minmax(140px, 1fr));
  gap: 10px;
  margin-top: 8px;
}
@media (max-width: 1000px){
  .magic-cards{ grid-template-columns: repeat(2, minmax(160px, 1fr)); }
}
.magic-card{
  border: 1px solid rgba(191,167,111,0.55);
  background: rgba(255,255,245,0.85);
  border-radius: 14px;
  padding: 10px 12px;
  box-shadow: 0 6px 16px rgba(0,0,0,0.08);
}
.magic-card .k{ font-weight: 900; opacity: .9; }
.magic-card .tier{ margin-top: 6px; font-family: monospace; opacity: .8; }
.magic-card .val{
  margin-top: 6px;
  font-family: monospace;
  font-size: 18px;
  font-weight: 900;
}
.magic-card .hint{ margin-top: 6px; font-size: 12px; opacity: .75; }
.magic-calib-actions{
  display:flex;
  gap:10px;
  margin-top:10px;
  flex-wrap:wrap;
}

#landing-stage .landing-bg,
#landing-stage .landing-shade {
  pointer-events: none;
}

.modal-dialog {
  margin-top: 120px !important;
}

/* Toast container (top layer, always visible) */
#toast-container{
  position: fixed;
  bottom: 20px;
  right: 20px;
  z-index: 1000000;
  display: flex;
  flex-direction: column;
  gap: 10px;
  width: 320px;
  max-width: 80vw;
  pointer-events: none !important;
}

#toast-container .toast{
  pointer-events: auto !important;
  background: rgba(255, 255, 245, 0.92);
  border: 1px solid #bfa76f;
  box-shadow: 0 4px 14px rgba(0,0,0,0.25);
  border-radius: 8px;
  padding: 10px 12px;
  font-family: 'Cinzel', serif;
  color: #3e2f1c;
}

#toast-container .toast small{
  opacity: 0.75;
  font-family: monospace;
}

html, body {
  height: 100%;
  margin: 0;
}

body {
  font-family: 'Cinzel', serif;
  color: #3e2f1c;
  background: #000;
  overflow: hidden;
}

/* Hide tabs */
.nav-tabs {
  display: none !important;
}

/* Kill default Shiny padding */
.tab-content {
  padding: 0 !important;
}
.container-fluid {
  padding-left: 0 !important;
  padding-right: 0 !important;
}

/* Toast container shouldn't block clicks */
#toast-container {
  pointer-events: none !important;
}
#toast-container .toast {
  pointer-events: auto !important;
}

/* Optional flash overlay */
#screen-flash {
  position: fixed;
  inset: 0;
  pointer-events: none;
  opacity: 0;
  z-index: 9998;
  transition: opacity 120ms ease;
}
#screen-flash.flash-red  { background: rgba(200, 30, 30, 0.28); }
#screen-flash.flash-gold { background: rgba(240, 200, 40, 0.22); }

/* -----------------------------
   MAIN APP WRAP
   ----------------------------- */
#app-wrap{
  position: fixed;
  inset: 0;
  overflow: hidden;
  z-index: 1;
}

#app-wrap::before{
  content: '';
  position: absolute;
  inset: 0;
  background-image: url('camp.png');
  background-size: cover;
  background-position: center;
  background-repeat: no-repeat;
  filter: saturate(1.03) contrast(1.03);
  transform: scale(1.01);
  z-index: 0;
}

#app-wrap::after{
  pointer-events: none !important;
  content: '';
  position: absolute;
  inset: 0;
  background: rgba(0,0,0,0.22);
  z-index: 0;
}

#app-panel{
  position: relative;
  z-index: 1;
  width: 100%;
  height: 100%;
}

/* -----------------------------
   CAMP vs PARCHMENT THEMES
   ----------------------------- */
body.camp { background: #000 !important; }
body.parchment { background: #000; }

/* -----------------------------
   CAMP TAB
   ----------------------------- */
body.camp #app-panel .tab-content{
  height: 100vh;
  overflow: hidden;
  padding: 0 !important;
}

/* -----------------------------
   NON-CAMP TABS
   ----------------------------- */
body.parchment #app-panel .tab-content{
  height: 100vh;
  overflow: hidden;
  padding: 0 !important;
}

body.parchment #app-panel .tab-content > .tab-pane.active{
  position: absolute;
  left: 50%;
  top: 90px;
  transform: translateX(-50%);
  width: min(1050px, calc(100vw - 54px));
  max-height: calc(100vh - 92px);
  overflow-y: auto;
  overflow-x: hidden;
  padding: 18px 18px 22px 18px;

  background: rgba(255,255,245,0.92);
  border: 1px solid rgba(191,167,111,0.88);
  border-radius: 16px;
  box-shadow: 0 14px 40px rgba(0,0,0,0.45);
}

body.parchment #app-panel .tab-content > .tab-pane.active .container-fluid{
  padding-left: 10px !important;
  padding-right: 10px !important;
}

/* -----------------------------
   RETURN TO CAMP
   ----------------------------- */
#return-camp-wrap{
  position: fixed;
  right: 14px;
  top: 90px;
  z-index: 999950;
  display: none;
}
#return-camp-wrap{display:none;flex-direction:column;align-items:stretch;gap:6px}
#return-camp-wrap .btn{
  border-radius: 999px;
  padding: 10px 14px;
  box-shadow: 0 8px 20px rgba(0,0,0,0.35);
  border: 1px solid rgba(191,167,111,0.9);
  background: rgba(255,255,245,0.92);
  font-family: 'Cinzel', serif;
}

/* -----------------------------
   LANDING FULLSCREEN STAGE
   ----------------------------- */
#landing-stage{
  position: fixed;
  inset: 0;
  z-index: 999900;
  overflow: hidden;
  pointer-events: none;
}
#landing-stage.hidden{
  display: none !important;
}

#landing-module-wrap {
  pointer-events: auto;
}

#landing-stage .landing-bg{
  position: absolute;
  inset: 0;
  background-image: url('loading_screen.png');
  background-size: cover;
  background-position: center;
  background-repeat: no-repeat;
  filter: saturate(1.05) contrast(1.05);
  transform: scale(1.02);
}
#landing-stage .landing-shade{
  position: absolute;
  inset: 0;
  background: linear-gradient(
    to top,
    rgba(0,0,0,0.78) 0%,
    rgba(0,0,0,0.30) 38%,
    rgba(0,0,0,0.00) 75%
  );
}

#landing-stage .panel{
  position: absolute;
  left: 50%;
  bottom: 26px;
  transform: translateX(-50%);
  width: min(920px, 92vw);
  padding: 16px 16px 14px 16px;
  border-radius: 14px;
  background: rgba(255,255,245,0.90);
  border: 1px solid rgba(191,167,111,0.88);
  box-shadow: 0 10px 30px rgba(0,0,0,0.35);
}

#landing-stage .title{
  font-weight: 800;
  font-size: 16px;
  margin-bottom: 8px;
  display:flex;
  justify-content:space-between;
  gap: 10px;
  align-items:center;
}

#landing-stage .tip{
  font-size: 14px;
  opacity: 0.95;
  line-height: 1.25;
  min-height: 2.3em;
}

.global-hud{
  position: fixed;
  top: 12px;
  left: 12px;
  right: 12px;
  z-index: 9999;
  pointer-events: none;
}

.global-hud .hudbar-inner{
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 10px 12px;
  border-radius: 14px;
  background: rgba(255,255,245,0.90);
  border: 1px solid rgba(191,167,111,0.92);
  box-shadow: 0 12px 34px rgba(0,0,0,0.35);
  backdrop-filter: blur(2px);
  color: #3e2f1c;
  pointer-events: auto;
}

.hud-name{
  font-weight: 900;
  font-size: 16px;
  max-width: 220px;
}

.hud-level{
  font-weight: 800;
  font-size: 13px;
}

.hud-group{
  display: flex;
  align-items: center;
  gap: 10px;
  position: relative;
}

.hud-group:not(:last-child)::after{
  content: '';
  position: absolute;
  right: -8px;
  top: 20%;
  height: 60%;
  width: 1px;
  background: rgba(191,167,111,0.25);
}

.hud-left{
  gap: 10px;
  padding-right: 12px;
  border-right: 1px solid rgba(191,167,111,0.4);
}

.hud-name{
  font-size: 17px;
  letter-spacing: 0.5px;
}

.hud-level{
  opacity: 0.85;
}

.hud-center{
  gap: 16px;
  padding: 0 14px;
  border-right: 1px solid rgba(191,167,111,0.4);
}

.bar-wrap{
  display: flex;
  align-items: center;
  gap: 8px;
  min-width: 200px;
  max-width: 260px;
}

.hud-status-wrap{
  gap: 10px;
  padding: 0 14px;
  border-right: 1px solid rgba(191,167,111,0.4);
}

.hud-time{
  font-size: 12px;
  opacity: 0.8;
  white-space: nowrap;
}

.hud-right{
  gap: 6px;
}

.hud-right .pill{
  padding: 5px 9px;
}

.hud-status-wrap div{
  font-size: 12px;
  opacity: 0.85;
  white-space: nowrap;
}

.identity-block{
  margin-left: 4px;
}

.bar-wrap{
  min-width: 200px;
}

.hud-center{
  padding-left: 10px;
  border-left: 1px solid rgba(191,167,111,0.35);
}

.hud-right{
  padding-left: 10px;
  border-left: 1px solid rgba(191,167,111,0.35);
}

.pill{
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 6px 10px;
  border-radius: 999px;
  border: 1px solid rgba(191,167,111,0.82);
  background: rgba(255,255,245,0.78);
  font-size: 13px;
  font-weight: 800;
}

.bar-wrap{
  display: flex;
  align-items: center;
  gap: 8px;
  min-width: 180px;
  max-width: 360px;
  flex: 1;
}

.bar-label{
  font-size: 12px;
  font-weight: 900;
}

.bar{
  position: relative;
  flex: 1;
  height: 14px;
  border-radius: 999px;
  overflow: hidden;
  background: rgba(40,40,40,0.18);
  border: 1px solid rgba(191,167,111,0.62);
}

.fill{
  height: 100%;
  width: 0%;
  transition: width 180ms ease;
}

.fill.hp{
  background: linear-gradient(90deg, rgba(170,35,35,0.92), rgba(240,120,60,0.90));
}

.fill.magic{
  background: linear-gradient(90deg, rgba(40,80,190,0.92), rgba(100,190,255,0.92));
}

.fill.temp{
  position: absolute;
  left: 0;
  top: 0;
  opacity: 0.5;
}

.spacer{
  flex: 1;
}

#landing-stage .bar{
  margin-top: 10px;
  height: 14px;
  border-radius: 999px;
  background: rgba(120,120,120,0.18);
  border: 1px solid rgba(191,167,111,0.55);
  overflow: hidden;
}
#landing-stage .bar > div{
  height: 100%;
  width: 0%;
  background: linear-gradient(90deg, rgba(180,40,40,0.80), rgba(240,120,60,0.88));
  transition: width 120ms linear;
}

#landing-module-wrap{
  display: none;
}
")),

    # -----------------------------
    # JS: flash + toast
    # -----------------------------
    tags$script(HTML("
window.flashScreen = function(type, ms=280) {
  var el = document.getElementById('screen-flash');
  if (!el) return;
  el.className = '';
  el.classList.add(type === 'gold' ? 'flash-gold' : 'flash-red');
  el.style.opacity = '1';
  setTimeout(function(){ el.style.opacity = '0'; }, ms);
};

window.showToast = function(message, timeoutMs = 2500) {
  var container = document.getElementById('toast-container');
  if (!container) return;

  var toast = document.createElement('div');
  toast.className = 'toast';

  var time = new Date().toLocaleTimeString();
  toast.innerHTML = '<div>' + message + '</div><small>' + time + '</small>';

  container.appendChild(toast);

  setTimeout(function() {
    toast.style.transition = 'opacity 400ms ease';
    toast.style.opacity = '0';
    setTimeout(function() {
      if (toast && toast.parentNode) toast.parentNode.removeChild(toast);
    }, 450);
  }, timeoutMs);
};
")),

    # Landing loader
    tags$script(src = "landing.js"),

    # -----------------------------
    # JS: camp/parchment theme + return button
    # -----------------------------
    tags$script(HTML("
(function(){
  function setThemeFor(value){
    var isCamp = (value === 'camp');
    document.body.classList.toggle('camp', isCamp);
    document.body.classList.toggle('parchment', !isCamp);

    var showReturn = (!isCamp && value !== 'landing');
    var wrap = document.getElementById('return-camp-wrap');
    if (wrap) wrap.style.display = showReturn ? 'flex' : 'none';
  }

  function activePaneValue(){
    var pane = document.querySelector('.tab-content > .tab-pane.active');
    if (!pane) return null;
    return pane.getAttribute('data-value');
  }

  function attach(){
    setThemeFor('camp');

    setTimeout(function(){
      var v = activePaneValue();
      if (v) setThemeFor(v);
    }, 0);

    $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(){
      var v = activePaneValue();
      if (v) setThemeFor(v);
    });
  }

  document.addEventListener('DOMContentLoaded', attach);
})();
"))
  ),

  # Hidden background music player
  tags$audio(
    id = "bgm",
    preload = "auto",
    autoplay = NA,
    style = "display:none;"
  ),

  # Outside tabs
  tags$div(id = "toast-container"),
  tags$div(id = "screen-flash"),
  hudUI("hud"),
 partyHudUI("partyhud"),
 privateNotesUI("notes"),
 merchantUI("merchants"),
 storyPlayerUI("story"),

  # Landing overlay
  div(
    id = "landing-stage",
    div(class = "landing-bg"),
    div(class = "landing-shade"),

    div(
      id = "landing-load-panel",
      class = "panel",
      div(
        class = "title",
        span("Loading the Twin Worlds…"),
        span(id = "landing-pct", "0%")
      ),
      div(id = "landing-tip", class = "tip", ""),
      div(class = "bar", div(id = "landing-fill"))
    ),

    div(
      id = "landing-module-wrap",
      class = "panel",
      landingUI("landing")
    )
  ),

  # Return to camp
  div(
    id = "return-camp-wrap",
    actionButton("return_to_camp", "↩ Return to Camp", class = "btn btn-default"),
    actionButton("shortcut_armoury", "🛡 Armoury", class = "btn btn-default"),
    actionButton("shortcut_magic", "✨ Magic", class = "btn btn-default"),
    actionButton("shortcut_combat", "⚔ Combat", class = "btn btn-default")
  ),

  # Main app
  div(
    id = "app-wrap",
    div(
      id = "app-panel",
      tabsetPanel(
        id = "main_tabs",
        selected = "camp",

        sidebarTabUI("sidebar"),
        tabPanel("Camp", value = "camp", campUI("camp")),
        inventoryTabUI("inventory"),
        skillsTabUI("skills"),
        armouryTabUI("armoury"),
        diceTabUI("dice"),
        diaryTabUI("diary"),
        magicTabUI("magic"),
        bloodTabUI("blood"),
        restTabUI("rest"),
        levelTabUI("level"),
        debugCombatUI("debug_combat"),
        character3DTabUI("character_3d"),
        tabPanel(
          
          title = "Glyphs",
          
          value = "runes",
          
          runeCraftingUI("runes")
          
        )
      )
    )
  )
)
