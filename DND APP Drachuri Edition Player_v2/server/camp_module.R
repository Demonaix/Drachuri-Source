# server/camp_module.R
library(shiny)
library(shinyjs)

campUI <- function(id) {
  ns <- NS(id)
  root <- ns("root")
  
  # Invisible hotspot button + always-visible label tag
  hotspot <- function(inputId, text, style_btn, style_tag = NULL) {
    tagList(
      actionButton(
        ns(inputId),
        label = "",
        class = "hotspot",
        style = style_btn
      ),
      div(
        class = "hotspot-tag",
        style = style_tag %||% style_btn,
        text
      )
    )
  }
  
  tagList(
    tags$audio(
      id = "fire_sfx",
      src = "fire.mp3",
      loop = NA,
      preload = "auto",
      style = "display:none;"
    ),
    tags$style(HTML(paste0("
      /* Camp screen fills viewport */
      #", root, "{
        position: relative;
        width: 100vw;
        height: 100vh;
        overflow: hidden;
        margin: 0;
        padding: 0;
      }

      /* Background */
      #", root, " .camp-bg{
        position: absolute;
        inset: 0;
        background-image: url('camp.png');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
      }

      /* ===== HOTSPOTS ===== */
      #", root, " .hotspot{
        position: absolute;
        border: 0;
        background: rgba(255,255,255,0.00);
        cursor: pointer;
        z-index: 20;
        padding: 0;
        margin: 0;
      }

      #", root, " .hotspot:focus{
        outline: none;
        box-shadow: none;
      }

      #", root, " .hotspot:hover{
        outline: 2px solid rgba(240, 200, 40, 0.22);
        background: rgba(240, 200, 40, 0.06);
      }

      /* Debug mode */
      #", root, ".debug .hotspot{
        outline: 2px dashed rgba(80, 200, 255, 0.85);
        background: rgba(80, 200, 255, 0.12);
      }
      
      #", root, " .bar{
  position: relative;
}

#", root, " .bar .fill.temp{
  position: absolute;
  left: 0;
  top: 0;
  height: 100%;
  opacity: 0.55;
  pointer-events: none;
}

#", root, " .fill.hp.temp{
  background: linear-gradient(90deg, rgba(255,200,200,0.9), rgba(255,160,120,0.9));
}

#", root, " .fill.magic.temp{
  background: linear-gradient(90deg, rgba(160,200,255,0.9), rgba(200,240,255,0.9));
}

#", root, " .identity-block{
  display: flex;
  flex-direction: column;
  justify-content: center;
  padding: 4px 8px;
  border-radius: 10px;
  background: rgba(255,255,245,0.75);
  border: 1px solid rgba(191,167,111,0.7);
  line-height: 1.1;
}

#", root, " .identity-line{
  font-size: 12px;
  font-weight: 800;
}

#", root, " .identity-sub{
  font-size: 11px;
  opacity: 0.8;
}

#", root, " .status-bar{
  display: flex;
  gap: 6px;
  align-items: center;
}

#", root, " .status-icon{
  font-size: 16px;
  padding: 4px 6px;
  border-radius: 6px;
  background: rgba(0,0,0,0.15);
  border: 1px solid rgba(191,167,111,0.5);
}

      /* Always-on discreet labels for hotspots */
      #", root, " .hotspot-tag{
        position: absolute;
        z-index: 21;
        pointer-events: none;
        transform: translate(6px, 6px);
        padding: 4px 8px;
        border-radius: 999px;
        background: rgba(0,0,0,0.45);
        border: 1px solid rgba(255,255,255,0.12);
        color: rgba(255,255,255,0.92);
        font-family: 'Cinzel', serif;
        font-size: 12px;
        letter-spacing: 0.2px;
        box-shadow: 0 8px 18px rgba(0,0,0,0.25);
        opacity: 0.85;
      }

      #", root, " .hotspot:hover + .hotspot-tag{
        opacity: 1;
        background: rgba(0,0,0,0.55);
      }

      /* ===== TOP HUD BAR ===== */
      #", root, " .camp-hudbar{
        position: absolute;
        left: 12px;
        right: 12px;
        top: 12px;
        z-index: 40;
        pointer-events: none; /* click-through */
      }

      #", root, " .hudbar-inner{
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
      }

      #", root, " .hud-name{
        font-weight: 900;
        font-size: 16px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        max-width: 220px;
        color: #3e2f1c;
      }

      #", root, " .hud-level{
        font-weight: 800;
        font-size: 13px;
        opacity: 0.95;
        white-space: nowrap;
        color: #3e2f1c;
      }

      #", root, " .pill{
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 6px 10px;
        border-radius: 999px;
        border: 1px solid rgba(191,167,111,0.82);
        background: rgba(255,255,245,0.78);
        font-size: 13px;
        font-weight: 800;
        white-space: nowrap;
        color: #3e2f1c;
      }

      #", root, " .pill .pill-label{
        opacity: 0.90;
        font-weight: 900;
        color: #3e2f1c;
      }

      #", root, " .bar-wrap{
        display: flex;
        align-items: center;
        gap: 8px;
        min-width: 180px;
        max-width: 360px;
        flex: 1;
      }

      #", root, " .bar-label{
        font-size: 12px;
        font-weight: 900;
        opacity: 0.95;
        white-space: nowrap;
        color: #3e2f1c;
      }

      #", root, " .bar{
        flex: 1;
        height: 14px;
        border-radius: 999px;
        overflow: hidden;
        background: rgba(40,40,40,0.18); /* darker so fill reads */
        border: 1px solid rgba(191,167,111,0.62);
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.22);
      }

      #", root, " .bar .fill{
        height: 100%;
        width: 0%;
        transition: width 180ms ease;
      }

      #", root, " .fill.hp{
        background: linear-gradient(90deg, rgba(170,35,35,0.92), rgba(240,120,60,0.90));
      }

      #", root, " .fill.magic{
        background: linear-gradient(90deg, rgba(40,80,190,0.92), rgba(100,190,255,0.92));
      }

      #", root, " .spacer{ flex: 1; }

      @media (max-width: 820px){
        #", root, " .hudbar-inner{ flex-wrap: wrap; gap: 8px; }
        #", root, " .hud-name{ max-width: 160px; }
        #", root, " .bar-wrap{ min-width: 160px; max-width: 100%; }
      }
    "))),
    
    div(
      id = root,
      
      div(class = "camp-bg"),
      
      # HUD (click-through)
      
      # Armoury
      hotspot("go_armoury", "Armoury",
              "left: 10%; top: 20%; width: 16%; height: 18%;",
              "left: 10%; top: 20%;"),
      

      # Runes / Glyphcrafting
      hotspot("go_runes", "Runes",
              "left: 42%; top: 72%; width: 12%; height: 14%;",
              "left: 42%; top: 72%;"),
      
      # 3D Character Builder
      hotspot("go_character_3d", "3D Character",
              "left: 58%; top: 36%; width: 14%; height: 12%;",
              "left: 58%; top: 36%;"),
      
      # Combat (armour + tent)
      hotspot("go_debug_combat", "Combat",
              "left: 24%; top: 50%; width: 12%; height: 12%;",
              "left: 24%; top: 50%;"),
      
      # Inventory (campfire center)
      hotspot("go_inventory", "Inventory",
              "left: 80%; top: 18%; width: 14%; height: 16%;",
              "left: 80%; top: 18%;"),
      
      # Skills (weapon rack left)
      hotspot("go_skills", "Skills",
              "left: 30%; top: 25%; width: 15%; height: 15%;",
              "left: 30%; top: 25%;"),
      
      # Diary (open book bottom-right table)
      hotspot("go_diary", "Diary",
              "left: 22%; top: 72%; width: 14%; height: 18%;",
              "left: 22%; top: 72%;"),
      
      # Character (top-right near tent)
      hotspot("go_character", "Character",
              "left: 58%; top: 18%; width: 14%; height: 16%;",
              "left: 58%; top: 18%;"),
      
      # Dice (bottom-right table edge)
      hotspot("go_dice", "Dice",
              "left: 62%; top: 82%; width: 12%; height: 14%;",
              "left: 62%; top: 82%;"),
      
      # Magic (glowing candles right table)
      hotspot("go_magic", "Magic",
              "left: 72%; top: 52%; width: 14%; height: 18%;",
              "left: 72%; top: 52%;"),
      
      # Blood (red potions bottom-left)
      hotspot("go_blood", "Blood",
      "left: 80%; top: 80%; width: 12%; height: 12%;",
      "left: 80%; top: 80%;"),
      
      # Rest (campfire core)
      hotspot("go_rest", "Rest",
              "left: 46%; top: 46%; width: 10%; height: 14%;",
              "left: 46%; top: 46%;"),
      
      # Level (far bottom-left corner)
      hotspot("go_level", "Level",
              "left: 10%; top: 50%; width: 12%; height: 14%;",
              "left: 10%; top: 50%;")
    )
  )
}

campServer <- function(
    id,
    state,
    tabset_id = "main_tabs",
    values = list(
      inventory    = "inventory",
      skills       = "skills",
      armoury      = "armoury",
      runes        = "runes",
      diary        = "diary",
      character    = "level",
      dice         = "dice",
      magic        = "magic",
      blood        = "blood",
      rest         = "rest",
      level        = "level",
      debug_combat = "debug_combat",
      character_3d = "character_3d"
    )
) {
  moduleServer(id, function(input, output, session) {
    
    cat("Camp MODULE SERVER STARTED\n")
    cat("Camp ns prefix:", session$ns("test"), "\n")
    
    go_tab <- function(value_or_title) {
      v <- gsub('"', '\\"', as.character(value_or_title))
      
      shinyjs::runjs(sprintf(
        "
        (function(){
          var tabset = $('#%s');
          if (!tabset.length) return;

          var a = tabset.find('a[data-toggle=\"tab\"][data-value=\"%s\"]');
          if (a.length) { a.tab('show'); return; }

          var target = \"%s\".toLowerCase();
          var found = null;

          tabset.find('a[data-toggle=\"tab\"]').each(function(){
            var txt = ($(this).text() || '').trim().toLowerCase();
            if (txt === target) { found = this; return false; }
          });
          if (found) { $(found).tab('show'); return; }

          tabset.find('a[data-toggle=\"tab\"]').each(function(){
            var txt = ($(this).text() || '').trim().toLowerCase();
            if (txt.indexOf(target) !== -1) { found = this; return false; }
          });
          if (found) { $(found).tab('show'); return; }

          console.log('[Camp] could not find tab for:', \"%s\");
        })();
        ",
        tabset_id, v, v, v
      ))
    }
    
    observeEvent(input$go_inventory,    { go_tab(values$inventory) },    ignoreInit = TRUE)
    observeEvent(input$go_skills,       { go_tab(values$skills) },       ignoreInit = TRUE)
    observeEvent(input$go_armoury,      { go_tab(values$armoury) },      ignoreInit = TRUE)
    observeEvent(input$go_debug_combat, { go_tab(values$debug_combat) }, ignoreInit = TRUE)
    observeEvent(input$go_diary,        { go_tab(values$diary) },        ignoreInit = TRUE)
    observeEvent(input$go_character,    { go_tab(values$character) },    ignoreInit = TRUE)
    observeEvent(input$go_dice,         { go_tab(values$dice) },         ignoreInit = TRUE)
    observeEvent(input$go_magic,        { go_tab(values$magic) },        ignoreInit = TRUE)
    observeEvent(input$go_blood,        { go_tab(values$blood) },        ignoreInit = TRUE)
    observeEvent(input$go_rest,         { go_tab(values$rest) },         ignoreInit = TRUE)
    observeEvent(input$go_level,        { go_tab(values$level) },        ignoreInit = TRUE)
    observeEvent(input$go_character_3d, { go_tab(values$character_3d) }, ignoreInit = TRUE)
    observeEvent(input$go_runes, { 
      
      go_tab(values$runes) 
      
    }, ignoreInit = TRUE)
  })
}
