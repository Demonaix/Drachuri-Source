library(shiny)

# =============================
# UI
# =============================
levelTabUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Level & Class",
    value = "level",
    
    tagList(
      
      # -------------------------
      # Styles
      # -------------------------
      tags$style(HTML("
      .card{
        border: 1px solid rgba(191,167,111,0.6);
        background: rgba(255,255,245,0.9);
        border-radius: 14px;
        padding: 14px;
        margin-bottom: 12px;
        box-shadow: 0 8px 22px rgba(0,0,0,0.15);
      }
      .title{ font-size: 18px; font-weight: 900; }
      .desc{ font-size: 13px; opacity: 0.85; margin-bottom: 8px; }
      .pill{
        display:inline-block;
        padding:4px 8px;
        border-radius:999px;
        border:1px solid rgba(191,167,111,0.7);
        margin:2px;
        font-size:12px;
        font-weight:700;
      }
      .grid{
        display:grid;
        grid-template-columns: repeat(auto-fit, minmax(180px,1fr));
        gap:10px;
      }
      .feature{
        border:1px solid rgba(191,167,111,0.5);
        border-radius:10px;
        padding:8px;
        background:rgba(255,255,255,0.6);
      }
      .feature-name{ font-weight:800; font-size:13px; }
      .feature-desc{ font-size:12px; opacity:0.8; }
      .portrait{
        width:120px;
        height:120px;
        object-fit:cover;
        border-radius:12px;
        border:1px solid rgba(191,167,111,0.7);
        margin-bottom:10px;
      }
      "))
      ,
      
      # -------------------------
      # Portrait
      # -------------------------
      h4("Character"),
      div(class="card",
          fileInput(ns("portrait_upload"), "Upload Portrait"),
          uiOutput(ns("portrait_ui"))
      ),
      
      # -------------------------
      
      # Audio
      
      # -------------------------
      
      h4("Audio"),
      
      div(class="card",
          
          sliderInput(
            
            ns("master_volume"),
            
            "Volume",
            
            min = 0,
            
            max = 1,
            
            value = 0.7,
            
            step = 0.05
            
          )
          
      ),
      # -------------------------
      # HP
      # -------------------------
      h4("Health"),
      div(class="card",
          fluidRow(
            column(
              4,
              numericInput(ns("hp_max"), "Max HP", value = 0, min = 0)
            ),
            column(
              4,
              numericInput(ns("hp_cur"), "Current HP", value = 0, min = 0)
            ),
            column(
              4,
              numericInput(ns("hp_temp"), "Temp HP", value = 0, min = 0)
            )
          )
      ),
      # -------------------------
      # Heritage
      # -------------------------
      h4("Heritage"),
      uiOutput(ns("race_ui")),
      
      tags$hr(),
      
      # -------------------------
      # Classes
      # -------------------------
      h4("Classes"),
      uiOutput(ns("classes_ui")),
      actionButton(ns("add_class"), "➕ Add Class"),
      
      tags$hr(),
      
      # -------------------------
      # Features
      # -------------------------
      h4("Features"),
      uiOutput(ns("features_ui"))
    )
  )
}

# =============================
# SERVER
# =============================
levelTabServer <- function(id, state, restoring, add_log, char_rev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------
    # Helpers
    # -------------------------
    
    log_safe <- function(msg, toast = TRUE, flash = "none") {
      if (is.function(add_log)) {
        try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
      }
    }
    
    ensure_classes <- function(char) {
      if (is.null(char$build$classes)) {
        char$build$classes <- list(
          list(
            class = char$build$class %||% "",
            level = char$build$level %||% 1,
            subclass = char$build$path %||% ""
          )
        )
      }
      char
    }
    
    # -------------------------
    # Portrait
    # -------------------------
    
    observeEvent(input$portrait_upload, {
      if (isTRUE(restoring())) return()
      req(input$portrait_upload$datapath)
      
      portrait_dir <- file.path("www", "portraits")
      dir.create(portrait_dir, recursive = TRUE, showWarnings = FALSE)
      
      ext <- tools::file_ext(input$portrait_upload$name)
      if (ext == "") ext <- "png"
      
      filename <- paste0(
        "portrait_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        "_",
        sample(100000:999999, 1),
        ".",
        ext
      )
      
      dest <- file.path(portrait_dir, filename)
      file.copy(input$portrait_upload$datapath, dest, overwrite = TRUE)
      
      # Browser-facing path, because files inside www are served from app root
      state$char$meta$portrait <- file.path("portraits", filename)
      
      log_safe("🖼️ Portrait updated")
    }, ignoreInit = TRUE)
    
    output$portrait_ui <- renderUI({
      path <- state$char$meta$portrait
      
      if (is.null(path) || path == "") {
        return(tags$em("No portrait uploaded."))
      }
      
      tags$img(src = path, class = "portrait")
    })
    
    # -------------------------
    # Add class
    # -------------------------
    
    observeEvent(input$add_class, {
      if (isTRUE(restoring())) return()
      
      char <- ensure_classes(state$char)
      
      char$build$classes <- append(char$build$classes, list(
        list(class = "Rogue", level = 1, subclass = "")
      ))
      
      state$char <- char
      log_safe("➕ Added class")
      
    }, ignoreInit = TRUE)
    
    # -------------------------
    # Classes UI
    # -------------------------
    
    output$classes_ui <- renderUI({
      
      char <- ensure_classes(state$char)
      classes <- char$build$classes
      
      tagList(lapply(seq_along(classes), function(i){
        
        cls <- classes[[i]]
        prefix <- paste0("cls_", i, "_")
        
  subclasses <- if (!is.null(cls$class) && cls$class %in% names(CLASSES)) {
  names(CLASSES[[cls$class]]$subclasses %||% list())
} else {
  character(0)
}
        
        div(class="card",
            
            div(style="display:flex; justify-content:space-between;",
                strong(cls$class %||% "Class"),
                actionButton(ns(paste0(prefix, "remove")), "✖", class="btn-xs btn-danger")
            ),
            
            selectInput(
              ns(paste0(prefix, "class")),
              "Class",
              choices = names(CLASSES),
              selected = if (cls$class %in% names(CLASSES)) cls$class else names(CLASSES)[1]
            ),
            
            numericInput(
              ns(paste0(prefix, "level")),
              "Level",
              value = cls$level %||% 1,
              min = 1,
              max = 20
            ),
            
            selectInput(
              ns(paste0(prefix, "subclass")),
              "Subclass",
              choices = c("None" = "", subclasses),
              selected = cls$subclass %||% ""
            )
        )
      }))
    })
    
    # -------------------------
    # Class observers (SAFE)
    # -------------------------
    
    observe({
      classes <- state$char$build$classes %||% list()
      
      lapply(seq_along(classes), function(i) {
        
        prefix <- paste0("cls_", i, "_")
        
        # CLASS
        observeEvent(input[[paste0(prefix, "class")]], {
          if (isTRUE(restoring())) return()
          
          char <- state$char
          char$build$classes[[i]]$class <- input[[paste0(prefix, "class")]]
          char$build$classes[[i]]$subclass <- ""
          state$char <- char
          
        }, ignoreInit = TRUE)
        
        # REMOVE
        observeEvent(input[[paste0(prefix, "remove")]], {
          if (isTRUE(restoring())) return()
          
          char <- ensure_classes(state$char)
          
          if (length(char$build$classes) <= 1) {
            char$build$classes <- list(
              list(class = "", level = 1, subclass = "")
            )
          } else {
            char$build$classes[[i]] <- NULL
          }
          
          state$char <- char
          log_safe("✖ Removed class")
          
        }, ignoreInit = TRUE)
        
        # LEVEL
        observeEvent(input[[paste0(prefix, "level")]], {
          if (isTRUE(restoring())) return()
          
          char <- state$char
          char$build$classes[[i]]$level <- input[[paste0(prefix, "level")]]
          state$char <- char
          
        }, ignoreInit = TRUE)
        
        # SUBCLASS
        observeEvent(input[[paste0(prefix, "subclass")]], {
          if (isTRUE(restoring())) return()
          
          char <- state$char
          char$build$classes[[i]]$subclass <- input[[paste0(prefix, "subclass")]]
          state$char <- char
          
        }, ignoreInit = TRUE)
        
      })
    })
    
    # -------------------------
    # Dynamic subclass updates
    # -------------------------
    
    observe({
      classes <- state$char$build$classes %||% list()
      
      lapply(seq_along(classes), function(i) {
        local({
          idx <- i
          prefix <- paste0("cls_", idx, "_")
          
          observeEvent(input[[paste0(prefix, "class")]], {
            if (isTRUE(restoring())) return()
            
            char <- ensure_classes(state$char)
            char$build$classes[[idx]]$class <- input[[paste0(prefix, "class")]]
            char$build$classes[[idx]]$subclass <- ""
            state$char <- char
            
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(prefix, "level")]], {
            if (isTRUE(restoring())) return()
            
            char <- ensure_classes(state$char)
            char$build$classes[[idx]]$level <- input[[paste0(prefix, "level")]]
            state$char <- char
            
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(prefix, "subclass")]], {
            if (isTRUE(restoring())) return()
            
            char <- ensure_classes(state$char)
            char$build$classes[[idx]]$subclass <- input[[paste0(prefix, "subclass")]]
            state$char <- char
            
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(prefix, "remove")]], {
            if (isTRUE(restoring())) return()
            
            char <- ensure_classes(state$char)
            
            if (length(char$build$classes) <= 1) {
              char$build$classes <- list(
                list(class = "", level = 1, subclass = "")
              )
            } else {
              char$build$classes[[idx]] <- NULL
            }
            
            state$char <- char
            log_safe("✖ Removed class")
            
          }, ignoreInit = TRUE)
        })
      })
    })
    
   
    
    # -------------------------
    # Race UI
    # -------------------------
    
    output$race_ui <- renderUI({
      
      char <- state$char
      race <- RACES[[char$meta$race]]
      sub  <- race$subraces[[char$build$path]]
      
      if (is.null(race)) return(tags$em("No race data."))
      
      core <- race$core
      
      tagList(
        
        div(class="card",
            div(class="title", char$meta$race),
            div(class="desc", race$desc),
            tags$span(class="pill", paste("Speed:", core$speed)),
            tags$span(class="pill", paste("Size:", core$size))
        ),
        
        if (!is.null(sub))
          div(class="card",
              div(class="title", char$build$path),
              div(class="desc", sub$desc)
          )
      )
    })
    
    # -------------------------
    # Feature aggregation
    # -------------------------
    
    get_all_features <- function(char) {
      
      char <- ensure_classes(char)
      classes <- char$build$classes %||% list()
      
      all_feats <- list()
      
      for (cls in classes) {
        
        # 🛑 Skip invalid class
        if (is.null(cls$class) || cls$class == "" || !cls$class %in% names(CLASSES)) {
          next
        }
        
        class_data <- CLASSES[[cls$class]]
        lvl <- suppressWarnings(as.numeric(cls$level))
        if (is.na(lvl)) lvl <- 1
        
        # 🧱 Base features
        if (!is.null(class_data$levels)) {
          for (lv in names(class_data$levels)) {
            if (as.numeric(lv) <= lvl) {
              all_feats <- c(all_feats, class_data$levels[[lv]]$features %||% list())
            }
          }
        }
        
        # 🧬 Subclass features (SAFE)
        if (!is.null(cls$subclass) &&
            cls$subclass != "" &&
            !is.null(class_data$subclasses) &&
            cls$subclass %in% names(class_data$subclasses)) {
          
          sub_data <- class_data$subclasses[[cls$subclass]]
          
          if (!is.null(sub_data$levels)) {
            for (lv in names(sub_data$levels)) {
              if (as.numeric(lv) <= lvl) {
                all_feats <- c(all_feats, sub_data$levels[[lv]]$features %||% list())
              }
            }
          }
        }
      }
      
      all_feats
    }
    
    # -------------------------
    # HP manual edit
    # -------------------------
    
    observeEvent(input$hp_max, {
      if (isTRUE(restoring())) return()
      
      char <- state$char
      char$resources <- char$resources %||% list()
      char$resources$hp <- char$resources$hp %||% list()
      
      new_max <- suppressWarnings(as.integer(input$hp_max))
      if (is.na(new_max)) new_max <- 0
      new_max <- max(0, new_max)
      
      old_cur <- as.integer(char$resources$hp$cur %||% 0)
      char$resources$hp$max <- new_max
      char$resources$hp$cur <- min(old_cur, new_max)
      
      state$char <- char
    }, ignoreInit = TRUE)
    
    observeEvent(input$hp_cur, {
      if (isTRUE(restoring())) return()
      
      char <- state$char
      char$resources <- char$resources %||% list()
      char$resources$hp <- char$resources$hp %||% list()
      
      hp_max <- as.integer(char$resources$hp$max %||% 0)
      new_cur <- suppressWarnings(as.integer(input$hp_cur))
      if (is.na(new_cur)) new_cur <- 0
      
      char$resources$hp$cur <- max(0, min(hp_max, new_cur))
      state$char <- char
    }, ignoreInit = TRUE)
    
    observeEvent(input$hp_temp, {
      if (isTRUE(restoring())) return()
      
      char <- state$char
      char$resources <- char$resources %||% list()
      char$resources$hp <- char$resources$hp %||% list()
      
      new_temp <- suppressWarnings(as.integer(input$hp_temp))
      if (is.na(new_temp)) new_temp <- 0
      
      char$resources$hp$temp <- max(0, new_temp)
      state$char <- char
    }, ignoreInit = TRUE)
    
    # -------------------------
    # Hydrate HP inputs from state
    # -------------------------
    observe({
      char_rev()
      
      char <- state$char
      hp <- char$resources$hp %||% list(max = 0, cur = 0, temp = 0)
      
      updateNumericInput(session, "hp_max",  value = hp$max  %||% 0)
      updateNumericInput(session, "hp_cur",  value = hp$cur  %||% 0)
      updateNumericInput(session, "hp_temp", value = hp$temp %||% 0)
    })
    
    
    
    # -------------------------
    # Features UI
    # -------------------------
    feats_reactive <- reactive({
      get_all_features(state$char)
    }) %>% debounce(100)
    
    
    output$features_ui <- renderUI({

      feats <- feats_reactive()
      
      if (length(feats) == 0) {
        return(tags$em("No features yet."))
      }
      
      tagList(
        div(class="grid",
            lapply(feats, function(f){
              div(class="feature",
                  div(class="feature-name", f$name),
                  div(class="feature-desc", f$desc)
              )
            })
        )
      )
    })
    
  })
}