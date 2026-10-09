# server/character_3d_module.R
library(shiny)
library(colourpicker)

character3DTabUI <- function(id) {
  ns <- NS(id)
  
  OUTFIT_WEB_ROOT <- "models/fantasy_outfits/Exports/gltf"
  
  MODULAR_LOCAL <- file.path(
    "www", "models", "fantasy_outfits", "Exports", "gltf", "Modular Parts"
  )
  
  make_gltf_choices <- function(pattern = NULL) {
    if (!dir.exists(MODULAR_LOCAL)) return(c("None" = ""))
    
    files <- list.files(MODULAR_LOCAL, pattern = "\\.gltf$", full.names = FALSE)
    
    if (!is.null(pattern)) {
      files <- files[grepl(pattern, files, ignore.case = TRUE)]
    }
    
    labels <- gsub("\\.gltf$", "", files)
    labels <- gsub("_", " ", labels)
    
    values <- file.path(OUTFIT_WEB_ROOT, "Modular Parts", files)
    values <- gsub("\\\\", "/", values)
    
    c("None" = "", stats::setNames(values, labels))
  }
  
  body_choices <- make_gltf_choices("_Body\\.gltf$")
  arms_choices <- make_gltf_choices("_Arms\\.gltf$")
  legs_choices <- make_gltf_choices("_Legs\\.gltf$")
  feet_choices <- make_gltf_choices("_Feet\\.gltf$")
  headgear_choices <- make_gltf_choices("_Head|_Hood|Helmet|Hat")
  accessory_choices <- make_gltf_choices("_Acc|Pauldrons|Cape|Cloak|Shoulder")
  
  tabPanel(
    "Combat Markers",
    value = "character_3d",
    
    h4("Combat Marker Workshop"),
    p("Choose how your character is recognised on both combat maps. Your existing 3D character pieces remain available below."),
    tags$style(HTML(sprintf("
      #%s{display:flex;align-items:center;justify-content:center;gap:48px;min-height:190px;margin-top:8px;padding:18px;border:1px solid rgba(150,120,70,.45);border-radius:12px;background:radial-gradient(circle,rgba(44,38,31,.88),rgba(15,17,21,.96))}
      #%s .marker-demo{display:flex;flex-direction:column;align-items:center;gap:8px;color:#f2e4be;font-weight:700}
      #%s .marker-token{width:82px;height:82px;display:flex;align-items:center;justify-content:center;border:5px solid #ead38a;box-shadow:0 5px 14px rgba(0,0,0,.5);font:900 28px Cinzel,Georgia,serif;color:#fff;text-shadow:0 2px 4px #000}
      #%s .marker-token.circle{border-radius:50%%} #%s .marker-token.shield{clip-path:polygon(50%% 0,94%% 16%%,86%% 72%%,50%% 100%%,14%% 72%%,6%% 16%%)} #%s .marker-token.diamond{transform:rotate(45deg)} #%s .marker-token.diamond span{transform:rotate(-45deg)}
      #%s .marker-aura{width:90px;height:112px;border-radius:50%%;position:relative;background:radial-gradient(ellipse,var(--aura-core) 0%%,var(--aura-fade) 34%%,transparent 72%%);filter:drop-shadow(0 0 10px var(--aura-core))}
      #%s .marker-aura.beacon{border:4px solid var(--aura-core);box-shadow:0 0 22px var(--aura-core),inset 0 0 18px var(--aura-core)} #%s .marker-aura.subtle{opacity:.48;filter:drop-shadow(0 0 5px var(--aura-core))}
    ", ns("marker_preview"),ns("marker_preview"),ns("marker_preview"),ns("marker_preview"),ns("marker_preview"),ns("marker_preview"),ns("marker_preview"),ns("marker_preview"),ns("marker_preview"),ns("marker_preview")))),
    tags$script(HTML(sprintf("(function(){
      function value(id,fallback){
        if(window.Shiny&&Shiny.shinyapp&&Shiny.shinyapp.$inputValues){var shinyValue=Shiny.shinyapp.$inputValues[id];if(shinyValue!==undefined&&shinyValue!==null&&String(shinyValue)!=='')return String(shinyValue);}
        var el=document.getElementById(id);if(!el)return fallback;
        if(el.value!==undefined&&el.value!==null&&String(el.value)!=='')return String(el.value);
        var child=el.querySelector('input');return child&&child.value?child.value:fallback;
      }
      function syncMarkerPreview(){
        var root=document.getElementById('%s');if(!root)return;
        var token=root.querySelector('.marker-token'),aura=root.querySelector('.marker-aura');
        if(token){token.style.background=value('%s','#4b91b5');token.className='marker-token '+value('%s','circle');var symbol=token.querySelector('span'),raw=value('%s','').trim();if(symbol)symbol.textContent=raw.substring(0,2)||symbol.textContent;}
        if(aura){var colour=value('%s','#77ddff');aura.style.setProperty('--aura-core',colour);aura.style.setProperty('--aura-fade',colour+'66');aura.className='marker-aura '+value('%s','wisps');}
      }
      Shiny.addCustomMessageHandler('%s',function(x){setTimeout(function(){
        var root=document.getElementById('%s');if(!root)return;var token=root.querySelector('.marker-token'),aura=root.querySelector('.marker-aura');
        if(token){token.style.background=x.tokenColour;token.className='marker-token '+x.shape;var symbol=token.querySelector('span');if(symbol)symbol.textContent=x.symbol;}
        if(aura){aura.style.setProperty('--aura-core',x.auraColour);aura.style.setProperty('--aura-fade',x.auraColour+'66');aura.className='marker-aura '+x.aura;}
      },0);});
      document.addEventListener('input',syncMarkerPreview,true);document.addEventListener('change',syncMarkerPreview,true);setInterval(syncMarkerPreview,250);
    })();",ns("marker_preview"),ns("marker_2d_color"),ns("marker_2d_shape"),ns("marker_2d_symbol"),ns("marker_3d_color"),ns("marker_3d_style"),ns("marker_preview_update"),ns("marker_preview")))),
    div(
      class = "magic-card",
      style = "margin-bottom:12px;",
      h4("2D map marker"),
      fluidRow(
        column(4, selectInput(ns("marker_2d_shape"), "Token shape", choices=c("Round"="circle", "Shield"="shield", "Diamond"="diamond"))),
        column(4, colourInput(ns("marker_2d_color"), "Token colour", value="#4b91b5")),
        column(4, textInput(ns("marker_2d_symbol"), "Symbol (up to 2 characters)", value=""))
      ),
      h4("3D map marker"),
      fluidRow(
        column(4, selectInput(ns("marker_3d_style"), "Marker aura", choices=c("Arcane wisps"="wisps", "Steady beacon"="beacon", "Subtle"="subtle"))),
        column(4, colourInput(ns("marker_3d_color"), "Aura colour", value="#77ddff"))
      ),
      uiOutput(ns("marker_preview"))
    ),
    
    div(
      class = "magic-card",
      style = "margin-bottom:12px;",
      h4("3D character pieces"),
      
      fluidRow(
        column(
          4,
          selectInput(
            ns("base_model"),
            "Base character",
            choices = c(
              "Female head" = "models/universal_base/Heads/female_head.glb",
              "Male head" = "models/universal_base/Heads/male_head.glb",
              "None" = "__none__"
            ),
            selected = "models/universal_base/Heads/female_head.glb"
          )
        ),
        column(
          4,
          selectInput(
            ns("hair_model"),
            "Hair",
            choices = c(
              "None" = "",
              "Long" = "models/universal_base/Hairstyles/Rigged to Head Bone/glTF (Godot -Unreal)/Hair_Long.gltf",
              "Buns" = "models/universal_base/Hairstyles/Rigged to Head Bone/glTF (Godot -Unreal)/Hair_Buns.gltf",
              "Buzzed" = "models/universal_base/Hairstyles/Rigged to Head Bone/glTF (Godot -Unreal)/Hair_Buzzed.gltf",
              "Buzzed Female" = "models/universal_base/Hairstyles/Rigged to Head Bone/glTF (Godot -Unreal)/Hair_BuzzedFemale.gltf",
              "Beard" = "models/universal_base/Hairstyles/Rigged to Head Bone/glTF (Godot -Unreal)/Hair_Beard.gltf",
              "Simple Parted" = "models/universal_base/Hairstyles/Rigged to Head Bone/glTF (Godot -Unreal)/Hair_SimpleParted.gltf"
            )
          )
        ),
        column(
          4,
          textInput(ns("character_3d_label"), "3D label", value = "")
        )
      ),
      
      tags$hr(),
      h4("Hair and eyes"),
      fluidRow(
        column(4, colourInput(ns("hair_color"), "Hair colour", value = "#3b2416")),
        column(
          4,
          selectInput(
            ns("eye_texture"),
            "Eye texture",
            choices = c(
              "Brown" = "T_Eye_Brown.png",
              "Red" = "T_Eye_Red.PNG",
              "Blue" = "T_Eye_Blue.PNG",
              "Green" = "T_Eye_Green.PNG",
              "Hazel" = "T_Eye_Hazel.PNG",
              "Yellow" = "T_Eye_Yellow.PNG",
              "Red" = "T_Eye_Red.PNG",
              "Violet" = "T_Eye_Violet.PNG",
              "Gold" = "T_Eye_Gold.PNG"
            ),
            selected = "T_Eye_Brown.png"
          )
        )
      ),
      
      tags$hr(),
      h4("Clothing / Armour"),
      
      fluidRow(
        column(4, selectInput(ns("body_model"), "Body / Torso", choices = body_choices)),
        column(4, selectInput(ns("arms_model"), "Arms", choices = arms_choices)),
        column(4, selectInput(ns("legs_model"), "Legs", choices = legs_choices))
      ),
      
      fluidRow(
        column(4, selectInput(ns("feet_model"), "Feet / Boots", choices = feet_choices)),
        column(4, selectInput(ns("headgear_model"), "Headgear", choices = headgear_choices)),
        column(4, selectInput(ns("accessory_model"), "Accessory", choices = accessory_choices))
      ),
      
      tags$hr(),
      
      fluidRow(
        column(4, actionButton(ns("load_preview"), "🔄 Load Preview", class = "btn btn-primary")),
        column(4, actionButton(ns("save_3d_character"), "Save Combat Markers", class = "btn btn-success")),
        column(4, actionButton(ns("reset_3d_character"), "♻ Reset", class = "btn btn-warning"))
      )
    ),
    
    div(
      class = "magic-card",
      h4("Preview"),
      tags$div(
        id = ns("character_preview"),
        style = "
          width:100%;
          height:520px;
          border:1px solid rgba(255,255,255,0.2);
          border-radius:8px;
          overflow:hidden;
        "
      )
    ),
    
    tags$hr(),
    h4("Saved Combat Marker Config"),
    uiOutput(ns("saved_3d_ui"))
  )
}

character3DTabServer <- function(id, state, restoring, add_log, char_rev = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    normalise_base_for_save <- function(x) {
      if (is.null(x) || identical(x, "__none__")) "" else x
    }
    
    normalise_base_for_ui <- function(x) {
      if (is.null(x) || !nzchar(x)) "__none__" else x
    }
    
    default_3d <- function() {
      list(
        label = "",
        marker_2d_shape = "circle",
        marker_2d_color = "#4b91b5",
        marker_2d_symbol = "",
        marker_3d_style = "wisps",
        marker_3d_color = "#77ddff",
        base_model = "models/universal_base/Heads/female_head.glb",
        hair_model = "",
        body_model = "",
        arms_model = "",
        legs_model = "",
        feet_model = "",
        headgear_model = "",
        accessory_model = "",
        hair_color = "#3b2416",
        eye_texture = "T_Eye_Brown.png"
      )
    }
    
    ensure_3d_state <- function() {
      if (is.null(state$char$character_3d) || !is.list(state$char$character_3d)) {
        state$char$character_3d <- default_3d()
      }
      
      d <- default_3d()
      for (nm in names(d)) {
        state$char$character_3d[[nm]] <- state$char$character_3d[[nm]] %||% d[[nm]]
      }
    }

    output$marker_preview <- renderUI({
      ensure_3d_state()
      name <- as.character(state$char$meta$name %||% "?")
      symbol <- substr(trimws(as.character(input$marker_2d_symbol %||% "")), 1L, 2L)
      if (!nzchar(symbol)) symbol <- substr(toupper(name), 1L, 1L)
      token_colour <- as.character(input$marker_2d_color %||% "#4b91b5")
      aura_colour <- as.character(input$marker_3d_color %||% "#77ddff")
      shape <- as.character(input$marker_2d_shape %||% "circle")
      aura <- as.character(input$marker_3d_style %||% "wisps")
      tags$div(
        class = "marker-live-preview",
        tags$div(class="marker-demo", tags$div(class=paste("marker-token",shape),style=paste0("background:",token_colour,";"),tags$span(symbol)), tags$span("2D token")),
        tags$div(class="marker-demo", tags$div(class=paste("marker-aura",aura),style=paste0("--aura-core:",aura_colour,";--aura-fade:",aura_colour,"66;")), tags$span("3D aura"))
      )
    })
    observe({
      name<-as.character(state$char$meta$name%||%"?");symbol<-substr(trimws(as.character(input$marker_2d_symbol%||%"")),1L,2L);if(!nzchar(symbol))symbol<-substr(toupper(name),1L,1L)
      session$sendCustomMessage(ns("marker_preview_update"),list(tokenColour=as.character(input$marker_2d_color%||%"#4b91b5"),shape=as.character(input$marker_2d_shape%||%"circle"),symbol=symbol,auraColour=as.character(input$marker_3d_color%||%"#77ddff"),aura=as.character(input$marker_3d_style%||%"wisps")))
    })
    
    send_preview <- function(
    base_model = NULL,
    hair_model = NULL,
    body_model = NULL,
    arms_model = NULL,
    legs_model = NULL,
    feet_model = NULL,
    headgear_model = NULL,
    accessory_model = NULL,
    hair_color = "#3b2416",
    eye_texture = "T_Eye_Brown.png"
    ) {
      session$sendCustomMessage(
        "loadCharacterPreview",
        list(
          containerId = ns("character_preview"),
          baseModel = normalise_base_for_save(base_model),
          hairModel = hair_model %||% "",
          bodyModel = body_model %||% "",
          armsModel = arms_model %||% "",
          legsModel = legs_model %||% "",
          feetModel = feet_model %||% "",
          headgearModel = headgear_model %||% "",
          accessoryModel = accessory_model %||% "",
          hairColor = hair_color %||% "#3b2416",
          eyeTexture = eye_texture %||% "T_Eye_Brown.png"
        )
      )
    }
    
    output$saved_3d_ui <- renderUI({
      ensure_3d_state()
      x <- state$char$character_3d
      
      div(
        class = "magic-card",
        tags$div(strong("Label: "), x$label %||% "None"),
        tags$div(strong("2D marker: "), x$marker_2d_shape %||% "circle", " · ", x$marker_2d_color %||% "#4b91b5", if(nzchar(x$marker_2d_symbol %||% "")) paste0(" · ", x$marker_2d_symbol) else ""),
        tags$div(strong("3D aura: "), x$marker_3d_style %||% "wisps", " · ", x$marker_3d_color %||% "#77ddff"),
        tags$div(strong("Base model: "), ifelse(nzchar(x$base_model %||% ""), basename(x$base_model), "None")),
        tags$div(strong("Hair model: "), ifelse(nzchar(x$hair_model %||% ""), basename(x$hair_model), "None")),
        tags$div(strong("Hair colour: "), x$hair_color %||% "#3b2416"),
        tags$div(strong("Eye texture: "), x$eye_texture %||% "T_Eye_Brown.png"),
        tags$div(strong("Body: "), ifelse(nzchar(x$body_model %||% ""), basename(x$body_model), "None")),
        tags$div(strong("Arms: "), ifelse(nzchar(x$arms_model %||% ""), basename(x$arms_model), "None")),
        tags$div(strong("Legs: "), ifelse(nzchar(x$legs_model %||% ""), basename(x$legs_model), "None")),
        tags$div(strong("Feet: "), ifelse(nzchar(x$feet_model %||% ""), basename(x$feet_model), "None")),
        tags$div(strong("Headgear: "), ifelse(nzchar(x$headgear_model %||% ""), basename(x$headgear_model), "None")),
        tags$div(strong("Accessory: "), ifelse(nzchar(x$accessory_model %||% ""), basename(x$accessory_model), "None"))
      )
    })
    
    observeEvent(input$load_preview, {
      if (isTRUE(restoring())) return()
      
      send_preview(
        base_model = input$base_model,
        hair_model = input$hair_model,
        body_model = input$body_model,
        arms_model = input$arms_model,
        legs_model = input$legs_model,
        feet_model = input$feet_model,
        headgear_model = input$headgear_model,
        accessory_model = input$accessory_model,
        hair_color = input$hair_color %||% "#3b2416",
        eye_texture = input$eye_texture %||% "T_Eye_Brown.png"
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$save_3d_character, {
      if (isTRUE(restoring())) return()
      
      state$char$character_3d <- list(
        label = input$character_3d_label %||% "",
        marker_2d_shape = input$marker_2d_shape %||% "circle",
        marker_2d_color = input$marker_2d_color %||% "#4b91b5",
        marker_2d_symbol = substr(trimws(input$marker_2d_symbol %||% ""), 1L, 2L),
        marker_3d_style = input$marker_3d_style %||% "wisps",
        marker_3d_color = input$marker_3d_color %||% "#77ddff",
        base_model = normalise_base_for_save(input$base_model),
        hair_model = input$hair_model %||% "",
        body_model = input$body_model %||% "",
        arms_model = input$arms_model %||% "",
        legs_model = input$legs_model %||% "",
        feet_model = input$feet_model %||% "",
        headgear_model = input$headgear_model %||% "",
        accessory_model = input$accessory_model %||% "",
        hair_color = input$hair_color %||% "#3b2416",
        eye_texture = input$eye_texture %||% "T_Eye_Brown.png"
      )

      if (is.function(char_rev)) char_rev(char_rev() + 1L)
      
      add_log("Combat marker appearance saved.")
      
      send_preview(
        base_model = input$base_model,
        hair_model = input$hair_model,
        body_model = input$body_model,
        arms_model = input$arms_model,
        legs_model = input$legs_model,
        feet_model = input$feet_model,
        headgear_model = input$headgear_model,
        accessory_model = input$accessory_model,
        hair_color = input$hair_color %||% "#3b2416",
        eye_texture = input$eye_texture %||% "T_Eye_Brown.png"
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$reset_3d_character, {
      if (isTRUE(restoring())) return()
      
      d <- default_3d()
      state$char$character_3d <- d
      
      updateTextInput(session, "character_3d_label", value = d$label)
      updateSelectInput(session, "marker_2d_shape", selected = d$marker_2d_shape)
      updateColourInput(session, "marker_2d_color", value = d$marker_2d_color)
      updateTextInput(session, "marker_2d_symbol", value = d$marker_2d_symbol)
      updateSelectInput(session, "marker_3d_style", selected = d$marker_3d_style)
      updateColourInput(session, "marker_3d_color", value = d$marker_3d_color)
      updateSelectInput(session, "base_model", selected = d$base_model)
      updateSelectInput(session, "hair_model", selected = d$hair_model)
      updateColourInput(session, "hair_color", value = d$hair_color)
      updateSelectInput(session, "eye_texture", selected = d$eye_texture)
      updateSelectInput(session, "body_model", selected = d$body_model)
      updateSelectInput(session, "arms_model", selected = d$arms_model)
      updateSelectInput(session, "legs_model", selected = d$legs_model)
      updateSelectInput(session, "feet_model", selected = d$feet_model)
      updateSelectInput(session, "headgear_model", selected = d$headgear_model)
      updateSelectInput(session, "accessory_model", selected = d$accessory_model)
      
      add_log("♻ 3D character appearance reset.")
      
      later::later(function() {
        send_preview(
          base_model = d$base_model,
          hair_model = d$hair_model,
          body_model = d$body_model,
          arms_model = d$arms_model,
          legs_model = d$legs_model,
          feet_model = d$feet_model,
          headgear_model = d$headgear_model,
          accessory_model = d$accessory_model,
          hair_color = d$hair_color,
          eye_texture = d$eye_texture
        )
      }, delay = 0.1)
    }, ignoreInit = TRUE)
    
    if (!is.null(char_rev)) {
      observeEvent(char_rev(), {
        restoring(TRUE)
        on.exit(restoring(FALSE), add = TRUE)
        
        ensure_3d_state()
        x <- state$char$character_3d
        
        updateTextInput(session, "character_3d_label", value = x$label %||% "")
        updateSelectInput(session, "marker_2d_shape", selected = x$marker_2d_shape %||% "circle")
        updateColourInput(session, "marker_2d_color", value = x$marker_2d_color %||% "#4b91b5")
        updateTextInput(session, "marker_2d_symbol", value = x$marker_2d_symbol %||% "")
        updateSelectInput(session, "marker_3d_style", selected = x$marker_3d_style %||% "wisps")
        updateColourInput(session, "marker_3d_color", value = x$marker_3d_color %||% "#77ddff")
        updateSelectInput(session, "base_model", selected = normalise_base_for_ui(x$base_model %||% ""))
        updateSelectInput(session, "hair_model", selected = x$hair_model %||% "")
        updateColourInput(session, "hair_color", value = x$hair_color %||% "#3b2416")
        updateSelectInput(session, "eye_texture", selected = x$eye_texture %||% "T_Eye_Brown.png")
        updateSelectInput(session, "body_model", selected = x$body_model %||% "")
        updateSelectInput(session, "arms_model", selected = x$arms_model %||% "")
        updateSelectInput(session, "legs_model", selected = x$legs_model %||% "")
        updateSelectInput(session, "feet_model", selected = x$feet_model %||% "")
        updateSelectInput(session, "headgear_model", selected = x$headgear_model %||% "")
        updateSelectInput(session, "accessory_model", selected = x$accessory_model %||% "")
        
        later::later(function() {
          send_preview(
            base_model = normalise_base_for_ui(x$base_model %||% ""),
            hair_model = x$hair_model %||% "",
            body_model = x$body_model %||% "",
            arms_model = x$arms_model %||% "",
            legs_model = x$legs_model %||% "",
            feet_model = x$feet_model %||% "",
            headgear_model = x$headgear_model %||% "",
            accessory_model = x$accessory_model %||% "",
            hair_color = x$hair_color %||% "#3b2416",
            eye_texture = x$eye_texture %||% "T_Eye_Brown.png"
          )
        }, delay = 0.2)
      }, ignoreInit = FALSE)
    }
  })
}
