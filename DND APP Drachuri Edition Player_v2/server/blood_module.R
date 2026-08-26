library(shiny)

# ============================================================
# UI
# ============================================================
bloodTabUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    tagList(
      tags$audio(
        id = "blood_sfx",
        src = "dark.mp3",
        loop = NA,
        preload = "auto",
        style = "display:none;"
      ),
      
      tags$style(HTML(sprintf("
        .blood-bleed-overlay{
          position: fixed;
          inset: 0;
          pointer-events: none;
          z-index: 999;
          opacity: 0;
          transition: opacity 0.8s ease, filter 0.8s ease;
          mix-blend-mode: multiply;
        }

        .blood-bleed-overlay::before{
          content: '';
          position: absolute;
          inset: 0;
          background:
            radial-gradient(ellipse at left center,
              rgba(120, 0, 0, 0.55) 0%%,
              rgba(120, 0, 0, 0.30) 18%%,
              rgba(120, 0, 0, 0.00) 42%%),
            radial-gradient(ellipse at right center,
              rgba(120, 0, 0, 0.55) 0%%,
              rgba(120, 0, 0, 0.30) 18%%,
              rgba(120, 0, 0, 0.00) 42%%),
            radial-gradient(ellipse at center top,
              rgba(70, 0, 0, 0.22) 0%%,
              rgba(70, 0, 0, 0.10) 16%%,
              rgba(70, 0, 0, 0.00) 36%%),
            radial-gradient(ellipse at center bottom,
              rgba(70, 0, 0, 0.22) 0%%,
              rgba(70, 0, 0, 0.10) 16%%,
              rgba(70, 0, 0, 0.00) 36%%);
        }

        .blood-bleed-overlay.stage-1{
          opacity: 0.08;
          filter: blur(6px);
        }

        .blood-bleed-overlay.stage-2{
          opacity: 0.16;
          filter: blur(8px);
        }

        .blood-bleed-overlay.stage-3{
          opacity: 0.28;
          filter: blur(10px);
        }

        .blood-bleed-overlay.stage-4{
          opacity: 0.42;
          filter: blur(12px);
        }

        #%s .blood-status-hand{
          display:flex;
          gap:14px;
          align-items:flex-start;
          overflow-x:auto;
          padding:6px 4px 14px;
        }

        #%s .blood-status-card{
          flex:0 0 168px;
          text-align:center;
          transition:transform .18s ease;
        }

        #%s .blood-status-card:hover{transform:translateY(-5px)}
        #%s .blood-status-card img{
          display:block;
          width:168px;
          aspect-ratio:4/5;
          object-fit:cover;
          border-radius:11px;
          box-shadow:0 8px 18px rgba(45,8,8,.38);
        }

        #%s .blood-status-card.risk img{box-shadow:0 0 0 2px #8e3e32,0 8px 18px rgba(45,8,8,.38)}
        #%s .blood-status-card.positive img{box-shadow:0 0 0 2px #587b4f,0 8px 18px rgba(45,8,8,.38)}
        #%s .blood-status-reason{margin-top:7px;font-size:12px;line-height:1.35;color:#503024}
      ", ns("root"), ns("root"), ns("root"), ns("root"), ns("root"), ns("root"), ns("root")))),
      
      tags$script(HTML(sprintf("
        Shiny.addCustomMessageHandler('%s', function(stage) {
          var el = document.getElementById('%s');
          if (!el) return;

          el.classList.remove('stage-1', 'stage-2', 'stage-3', 'stage-4');
          el.classList.add('stage-' + stage);
        });
      ", ns("set_bleed_stage"), ns("blood_bleed_overlay"))))
    ),
    
    title = "Blood",
    value = "blood",
    
    tags$div(
      id = ns("blood_bleed_overlay"),
      class = "blood-bleed-overlay stage-1"
    ),
    
    div(
      id = ns("root"),
      class = "card",
      
      h3("🩸 Blood Inventory"),
      tags$p(
        style = "opacity:.9;",
        "Store blood for later. You can drink part of a stash; Sindre gain is proportional."
      ),
      
      tags$hr(),
      
      div(
        class = "card",
        h4("Add Blood"),
        fluidRow(
          column(4, numericInput(ns("blood_pints"), "Pints", value = 1, min = 0.1, step = 0.1)),
          column(4, numericInput(ns("blood_sindre"), "Total Sindre value", value = 5, min = 0, step = 1)),
          column(4, textInput(ns("blood_source"), "Source", value = ""))
        ),
        actionButton(ns("add_blood"), "➕ Store Blood", class = "btn btn-danger")
      ),
      
      div(
        class = "card",
        h4("Stored Blood"),
        uiOutput(ns("blood_inventory_ui"))
      ),
      
      div(
        class = "card",
        h4("🫀 Hearts"),
        
        fluidRow(
          column(4, numericInput(ns("heart_count"), "Hearts", value = 1, min = 1, step = 1)),
          column(4, numericInput(ns("heart_sindre"), "Sindre value", value = 25, min = 0)),
          column(4, textInput(ns("heart_source"), "Source", value = ""))
        ),
        
        actionButton(ns("add_heart"), "➕ Store Heart", class = "btn btn-danger"),
        
        tags$hr(),
        
        uiOutput(ns("hearts_inventory_ui"))
      ),
      
      div(
        class = "card",
        h4("🧠 Blood Addiction"),

        uiOutput(ns("blood_status_cards")),

        uiOutput(ns("blood_stage_card")),
        
        tags$hr(),
        
        fluidRow(
          column(6, strong(textOutput(ns("blood_intake_today")))),
          column(6, strong(textOutput(ns("blood_intake_required"))))
        )
      ),
      div(class = "card", h4("📜 Consumption History"), uiOutput(ns("blood_history_ui")))
    )
  )
}

# ============================================================
# SERVER
# ============================================================
bloodTabServer <- function(
    id,
    state,
    restoring = NULL,
    add_log = NULL,
    char_rev = NULL
) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    if (is.null(state)) stop("bloodTabServer: state is NULL")
    if (is.null(restoring)) restoring <- reactiveVal(FALSE)
    if (is.null(char_rev)) char_rev <- reactiveVal(0)
    
    log_safe <- function(msg, toast = TRUE, flash = "none") {
      if (is.function(add_log)) {
        try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
      } else {
        message(msg)
      }
    }
    
    blood_obs_ids <- reactiveVal(character())
    heart_obs_ids <- reactiveVal(character())
    pending_id <- reactiveVal(NULL)
    
    # ------------------------------------------------------------
    # Core helpers
    # ------------------------------------------------------------
    ensure_blood_state <- function(x) {
      x <- validate_character(x)
      
      x$resources <- x$resources %||% list()
      if (!is.list(x$resources)) x$resources <- list()
      
      x$resources$blood <- x$resources$blood %||% list()
      if (!is.list(x$resources$blood)) x$resources$blood <- list()
      
      x$resources$blood$addiction <- x$resources$blood$addiction %||% list()
      if (!is.list(x$resources$blood$addiction)) x$resources$blood$addiction <- list()
      
      a <- x$resources$blood$addiction
      a$stage <- as.integer(a$stage %||% 1)
      a$days_at_stage <- as.integer(a$days_at_stage %||% 0)
      a$previous_day_intake <- as.numeric(a$previous_day_intake %||% 0)
      a$current_day_intake  <- as.numeric(a$current_day_intake %||% 0)
      if (is.na(a$stage)) a$stage <- 1
      if (is.na(a$days_at_stage)) a$days_at_stage <- 0
      if (is.na(a$previous_day_intake)) a$previous_day_intake <- 0
      if (is.na(a$current_day_intake)) a$current_day_intake <- 0
      
      x$resources$blood$addiction <- a
      x
    }
    
    get_inventory <- reactive({
      x <- ensure_blood_state(state$char)
      inventory_normalize(x$inventory$items)
    })
    
    write_core <- function(x) {
      x <- ensure_blood_state(x)
      x$inventory$items <- inventory_normalize(x$inventory$items)
      
      # preserve session-authoritative HP blob fields
      if (is_session_active_for_character(state)) {
        current_char <- validate_character(state$char)
        x$resources$hp <- current_char$resources$hp
      }
      
      state$char <- x
    }
    
    write_inventory <- function(df) {
      x <- ensure_blood_state(state$char)
      x$status<-x$status%||%list();x$status$needs_hours<-x$status$needs_hours%||%list();x$status$needs_hours$blood<-0
      x$inventory$items <- inventory_normalize(df)
      write_core(x)
      record_blood_consumption(state$char_id, state$active_session_id, x$meta$day,
                               "blood", source, drink_pints, sindre_gain)
    }
    
    get_hp_state <- function() {
      get_effective_hp_state(state)
    }
    
    heal_hp_state <- function(amount) {
      apply_healing_to_state(state, amount)
    }
    
    is_heart_eater <- reactive({
      x <- ensure_blood_state(state$char)
      grepl("heart eater", tolower(x$build$path %||% ""))
    })
    
    # ------------------------------------------------------------
    # Shared inventory views
    # ------------------------------------------------------------
    blood_items <- reactive({
      df <- get_inventory()
      df <- df[df$type == "blood", , drop = FALSE]
      if (!nrow(df)) return(df)
      
      df$pints <- suppressWarnings(as.numeric(df$qty))
      df$pints[is.na(df$pints)] <- 0
      
      df$source <- vapply(
        df$meta,
        function(m) as.character((m %||% list())$source %||% "Unknown source"),
        character(1)
      )
      
      df$sindre_per_unit <- vapply(
        df$meta,
        function(m) as.numeric((m %||% list())$sindre_per_unit %||% 0),
        numeric(1)
      )
      df$sindre_per_unit[is.na(df$sindre_per_unit)] <- 0
      
      df$sindre <- df$pints * df$sindre_per_unit
      df
    })
    
    heart_items <- reactive({
      df <- get_inventory()
      df <- df[df$type == "heart", , drop = FALSE]
      if (!nrow(df)) return(df)
      
      df$hearts <- suppressWarnings(as.numeric(df$qty))
      df$hearts[is.na(df$hearts)] <- 0
      
      df$source <- vapply(
        df$meta,
        function(m) as.character((m %||% list())$source %||% "Unknown source"),
        character(1)
      )
      
      df$sindre <- vapply(
        df$meta,
        function(m) as.numeric((m %||% list())$sindre_per_unit %||% 0),
        numeric(1)
      )
      df$sindre[is.na(df$sindre)] <- 0
      
      df
    })
    
    core_addiction <- reactive({
      x <- ensure_blood_state(state$char)
      x$resources$blood$addiction
    })
    
    observe({
      a <- core_addiction()
      stage <- as.integer(a$stage %||% 1)
      
      if (is.na(stage) || stage < 1) stage <- 1
      if (stage > 4) stage <- 4
      
      session$sendCustomMessage(session$ns("set_bleed_stage"), stage)
    })
    
    # ------------------------------------------------------------
    # Hydrate / migrate legacy blood + hearts into shared inventory
    # ------------------------------------------------------------
    hydrate_blood <- function() {
      restoring(TRUE)
      on.exit(restoring(FALSE), add = TRUE)
      
      x <- ensure_blood_state(state$char)
      inv <- inventory_normalize(x$inventory$items)
      
      # migrate legacy blood inventory
      legacy_blood <- x$resources$blood$inventory
      if (is.data.frame(legacy_blood) && nrow(legacy_blood) > 0) {
        for (i in seq_len(nrow(legacy_blood))) {
          row <- legacy_blood[i, , drop = FALSE]
          
          pints <- suppressWarnings(as.numeric(row$pints[[1]] %||% 0))
          sindre <- suppressWarnings(as.numeric(row$sindre[[1]] %||% 0))
          source <- as.character(row$source[[1]] %||% "Unknown source")
          
          if (is.na(pints) || pints <= 0) next
          if (is.na(sindre) || sindre < 0) sindre <- 0
          
          inv <- rbind(inv, data.frame(
            id = as.character(row$id[[1]] %||% paste0("legacy_b_", i, "_", sample(1000:9999, 1))),
            name = paste0("Stored Blood (", source, ")"),
            type = "blood",
            desc = paste0("Stored blood from ", source),
            value = 0,
            weight = pints,
            qty = pints,
            equipped = FALSE,
            in_bag = FALSE,
            meta = I(list(list(
              source = source,
              sindre_per_unit = if (pints > 0) sindre / pints else 0
            ))),
            edit = FALSE,
            stringsAsFactors = FALSE
          ))
        }
        
        x$resources$blood$inventory <- data.frame(
          id = character(),
          pints = numeric(),
          sindre = numeric(),
          source = character(),
          stringsAsFactors = FALSE
        )
      }
      
      # migrate legacy hearts inventory
      legacy_hearts <- x$resources$hearts
      if (is.data.frame(legacy_hearts) && nrow(legacy_hearts) > 0) {
        for (i in seq_len(nrow(legacy_hearts))) {
          row <- legacy_hearts[i, , drop = FALSE]
          
          hearts <- suppressWarnings(as.numeric(row$hearts[[1]] %||% 0))
          sindre <- suppressWarnings(as.numeric(row$sindre[[1]] %||% 0))
          source <- as.character(row$source[[1]] %||% "Unknown source")
          
          if (is.na(hearts) || hearts <= 0) next
          if (is.na(sindre) || sindre < 0) sindre <- 0
          
          inv <- rbind(inv, data.frame(
            id = as.character(row$id[[1]] %||% paste0("legacy_h_", i, "_", sample(1000:9999, 1))),
            name = paste0("Heart (", source, ")"),
            type = "heart",
            desc = paste0("Preserved heart from ", source),
            value = 0,
            weight = hearts,
            qty = hearts,
            equipped = FALSE,
            in_bag = FALSE,
            meta = I(list(list(
              source = source,
              sindre_per_unit = sindre
            ))),
            edit = FALSE,
            stringsAsFactors = FALSE
          ))
        }
        
        x$resources$hearts <- data.frame(
          id = character(),
          hearts = numeric(),
          sindre = numeric(),
          source = character(),
          stringsAsFactors = FALSE
        )
      }
      
      x$inventory$items <- inventory_normalize(inv)
      write_core(x)
    }
    
    observeEvent(TRUE, hydrate_blood(), once = TRUE)
    
    observeEvent(char_rev(), {
      if (isTRUE(restoring())) return()
      blood_obs_ids(character())
      heart_obs_ids(character())
      hydrate_blood()
    }, ignoreInit = TRUE)
    
    # ------------------------------------------------------------
    # Add blood
    # ------------------------------------------------------------
    observeEvent(input$add_blood, {
      if (isTRUE(restoring())) return()
      
      df <- get_inventory()
      
      pints <- suppressWarnings(as.numeric(input$blood_pints %||% 0))
      sindre <- suppressWarnings(as.numeric(input$blood_sindre %||% 0))
      source <- trimws(as.character(input$blood_source %||% ""))
      
      if (is.na(pints) || pints <= 0) {
        log_safe("❌ Pints must be > 0.", toast = TRUE, flash = "red")
        return()
      }
      if (is.na(sindre) || sindre < 0) sindre <- 0
      if (!nzchar(source)) source <- "Unknown source"
      
      new <- data.frame(
        id = paste0("b", as.integer(Sys.time()), sample(1000:9999, 1)),
        name = paste0("Stored Blood (", source, ")"),
        type = "blood",
        desc = paste0("Stored blood from ", source),
        value = 0,
        weight = pints,
        qty = pints,
        equipped = FALSE,
        in_bag = FALSE,
        meta = I(list(list(
          source = source,
          sindre_per_unit = if (pints > 0) sindre / pints else 0
        ))),
        edit = FALSE,
        stringsAsFactors = FALSE
      )
      
      write_inventory(rbind(df, new))
      
      log_safe(
        paste0("➕ Stored ", pints, " pint(s) from ", source, " (", sindre, " Sindre total)."),
        toast = TRUE
      )
    }, ignoreInit = TRUE)
    
    # ------------------------------------------------------------
    # Add heart
    # ------------------------------------------------------------
    observeEvent(input$add_heart, {
      if (isTRUE(restoring())) return()
      
      df <- get_inventory()
      
      hearts <- suppressWarnings(as.numeric(input$heart_count %||% 0))
      sindre <- suppressWarnings(as.numeric(input$heart_sindre %||% 0))
      source <- trimws(as.character(input$heart_source %||% ""))
      
      if (is.na(hearts) || hearts <= 0) {
        log_safe("❌ Must add at least 1 heart.", TRUE, "red")
        return()
      }
      if (is.na(sindre) || sindre < 0) sindre <- 0
      if (!nzchar(source)) source <- "Unknown source"
      
      new <- data.frame(
        id = paste0("h", as.integer(Sys.time()), sample(1000:9999, 1)),
        name = paste0("Heart (", source, ")"),
        type = "heart",
        desc = paste0("Preserved heart from ", source),
        value = 0,
        weight = hearts,
        qty = hearts,
        equipped = FALSE,
        in_bag = FALSE,
        meta = I(list(list(
          source = source,
          sindre_per_unit = sindre
        ))),
        edit = FALSE,
        stringsAsFactors = FALSE
      )
      
      write_inventory(rbind(df, new))
      
      log_safe(
        paste0("🫀 Stored ", hearts, " heart(s) from ", source, " (", sindre, " Sindre each)."),
        TRUE
      )
    }, ignoreInit = TRUE)
    
    # ------------------------------------------------------------
    # Blood inventory UI
    # ------------------------------------------------------------
    output$blood_inventory_ui <- renderUI({
      blood <- blood_items()
      if (!is.data.frame(blood) || nrow(blood) == 0) {
        return(tags$div(style = "opacity:.85;", "No blood stored."))
      }
      
      tagList(
        fluidRow(
          column(3, strong("Pints (left)")),
          column(3, strong("Sindre (left)")),
          column(4, strong("Source")),
          column(2, strong("Drink"))
        ),
        lapply(seq_len(nrow(blood)), function(i) {
          row <- blood[i, , drop = FALSE]
          btn_id <- paste0("drink_", row$id[[1]])
          
          fluidRow(
            column(3, span(round(as.numeric(row$pints[[1]]), 2))),
            column(3, span(round(as.numeric(row$sindre[[1]]), 2))),
            column(4, span(as.character(row$source[[1]]))),
            column(2, actionButton(session$ns(btn_id), "🩸 Drink", class = "btn-danger btn-sm"))
          )
        })
      )
    })
    
    # ------------------------------------------------------------
    # Hearts inventory UI
    # ------------------------------------------------------------
    output$hearts_inventory_ui <- renderUI({
      hearts <- heart_items()
      
      if (!is.data.frame(hearts) || nrow(hearts) == 0) {
        return(tags$div(style = "opacity:.85;", "No hearts stored."))
      }
      
      tagList(
        fluidRow(
          column(3, strong("Hearts")),
          column(3, strong("Sindre")),
          column(4, strong("Source")),
          column(2, strong("Use"))
        ),
        lapply(seq_len(nrow(hearts)), function(i) {
          row <- hearts[i, , drop = FALSE]
          btn_id <- paste0("use_heart_", row$id[[1]])
          
          fluidRow(
            column(3, span(row$hearts[[1]])),
            column(3, span(row$sindre[[1]])),
            column(4, span(row$source[[1]])),
            column(2, actionButton(session$ns(btn_id), "🫀 Use", class = "btn-danger btn-sm"))
          )
        })
      )
    })
    
    # ------------------------------------------------------------
    # Drink buttons registry
    # ------------------------------------------------------------
    observeEvent(blood_items(), {
      blood <- blood_items()
      if (!nrow(blood)) return()
      
      new_ids <- setdiff(blood$id, blood_obs_ids())
      if (!length(new_ids)) return()
      
      for (id0 in new_ids) {
        local({
          this_id <- id0
          btn_id <- paste0("drink_", this_id)
          
          observeEvent(input[[btn_id]], {
            if (isTRUE(restoring())) return()
            
            blood <- blood_items()
            row <- blood[blood$id == this_id, , drop = FALSE]
            if (nrow(row) != 1) return()
            
            pending_id(this_id)
            
            avail_pints <- as.numeric(row$pints[[1]] %||% 0)
            if (is.na(avail_pints) || avail_pints <= 0) return()
            
            showModal(modalDialog(
              title = paste0("Drink from: ", row$source[[1]]),
              numericInput(
                session$ns("drink_pints"),
                "How many pints?",
                value = min(1, avail_pints),
                min = 0.1,
                max = avail_pints,
                step = 0.1
              ),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(session$ns("confirm_drink"), "Drink", class = "btn-danger")
              )
            ))
          }, ignoreInit = TRUE)
        })
      }
      
      blood_obs_ids(unique(c(blood_obs_ids(), new_ids)))
    }, ignoreInit = TRUE)
    
    # ------------------------------------------------------------
    # Confirm drink
    # ------------------------------------------------------------
    observeEvent(input$confirm_drink, {
      if (isTRUE(restoring())) return()
      removeModal()
      
      this_id <- pending_id()
      pending_id(NULL)
      if (is.null(this_id)) return()
      
      drink_pints <- suppressWarnings(as.numeric(input$drink_pints %||% 0))
      if (is.na(drink_pints) || drink_pints <= 0) return()
      
      x <- ensure_blood_state(state$char)
      df <- inventory_normalize(x$inventory$items)
      
      idx <- which(df$id == this_id & df$type == "blood")
      if (length(idx) != 1) return()
      
      row <- df[idx, , drop = FALSE]
      meta <- row$meta[[1]] %||% list()
      
      avail_pints <- suppressWarnings(as.numeric(row$qty[[1]] %||% 0))
      sindre_per_pint <- suppressWarnings(as.numeric(meta$sindre_per_unit %||% 0))
      source <- as.character(meta$source %||% "Unknown source")
      
      if (is.na(avail_pints) || avail_pints <= 0) return()
      if (is.na(sindre_per_pint) || sindre_per_pint < 0) sindre_per_pint <- 0
      
      drink_pints <- max(0, min(avail_pints, drink_pints))
      sindre_gain <- drink_pints * sindre_per_pint
      
      x <- validate_character(x)
      cur <- suppressWarnings(as.integer(x$resources$sindre$cur %||% 0))
      tot <- suppressWarnings(as.integer(x$resources$sindre$total %||% 0))
      if (is.na(cur)) cur <- 0
      if (is.na(tot)) tot <- 0
      
      new_cur <- cur + sindre_gain
      overflow <- max(0, new_cur - tot)
      
      if (isTRUE(is_heart_eater())) {
        x$resources$sindre$cur <- as.integer(min(new_cur, tot))
        x$resources$sindre$temp <- as.integer((x$resources$sindre$temp %||% 0) + overflow)
      } else {
        x$resources$sindre$cur <- as.integer(min(new_cur, tot))
      }
      
      # reduce blood item proportionally
      remaining_pints <- avail_pints - drink_pints
      if (remaining_pints <= 1e-6) {
        df <- df[-idx, , drop = FALSE]
      } else {
        df$qty[idx] <- remaining_pints
        df$weight[idx] <- remaining_pints
      }
      
      # addiction bookkeeping
      a <- x$resources$blood$addiction
      a$current_day_intake <- as.numeric(a$current_day_intake %||% 0) + drink_pints
      x$resources$blood$addiction <- a
      
      # heart eater heal from blood
      if (isTRUE(is_heart_eater())) {
        heal_amt <- as.integer(round(5 * drink_pints))
        hp_before <- as.integer(x$resources$hp$cur %||% 0L)
        hp_max <- as.integer(x$resources$hp$max %||% hp_before)
        x$resources$hp$cur <- min(hp_max, hp_before + heal_amt)
        gained <- x$resources$hp$cur - hp_before
        log_safe(paste0("❤️ Blood heals you for ", gained, " HP."), toast = TRUE)
      }
      
      x$inventory$items <- inventory_normalize(df)
      write_core(x)
      
      log_safe(
        paste0(
          "🩸 Drank ", round(drink_pints, 2), " pint(s) from ", source,
          ": +", round(sindre_gain, 2), " Sindre",
          if (isTRUE(is_heart_eater())) paste0(" (+", round(overflow, 2), " overflow)") else "",
          " → ", x$resources$sindre$cur, "/", tot
        ),
        toast = TRUE,
        flash = "gold"
      )
    })
    
    # ------------------------------------------------------------
    # Heart button registry
    # ------------------------------------------------------------
    observeEvent(heart_items(), {
      hearts <- heart_items()
      if (!nrow(hearts)) return()
      
      new_ids <- setdiff(hearts$id, heart_obs_ids())
      if (!length(new_ids)) return()
      
      for (id0 in new_ids) {
        local({
          this_id <- id0
          btn_id <- paste0("use_heart_", this_id)
          
          observeEvent(input[[btn_id]], {
            if (isTRUE(restoring())) return()
            
            x <- ensure_blood_state(state$char)
            df <- inventory_normalize(x$inventory$items)
            
            idx <- which(df$id == this_id & df$type == "heart")
            if (length(idx) != 1) return()
            
            row <- df[idx, , drop = FALSE]
            meta <- row$meta[[1]] %||% list()
            
            hearts <- suppressWarnings(as.numeric(row$qty[[1]] %||% 0))
            sindre <- suppressWarnings(as.numeric(meta$sindre_per_unit %||% 0))
            source <- as.character(meta$source %||% "Unknown source")
            
            if (is.na(hearts) || hearts <= 0) return()
            if (is.na(sindre) || sindre < 0) sindre <- 0
            
            cur <- suppressWarnings(as.integer(x$resources$sindre$cur %||% 0))
            tot <- suppressWarnings(as.integer(x$resources$sindre$total %||% 0))
            if (is.na(cur)) cur <- 0
            if (is.na(tot)) tot <- 0
            
            heart_result <- consume_heart_sindre(
              cur, tot, x$resources$sindre$temp, sindre,
              heart_eater = isTRUE(is_heart_eater())
            )
            x$resources$sindre$cur <- heart_result$current
            x$resources$sindre$temp <- heart_result$temporary
            
            # reduce heart count
            if (hearts <= 1) {
              df <- df[-idx, , drop = FALSE]
            } else {
              df$qty[idx] <- hearts - 1
              df$weight[idx] <- hearts - 1
            }
            
            # addiction: one heart = 10 pint equivalent
            a <- x$resources$blood$addiction
            a$current_day_intake <- as.numeric(a$current_day_intake %||% 0) + 10
            x$resources$blood$addiction <- a
            
            x$inventory$items <- inventory_normalize(df)
            x$status<-x$status%||%list();x$status$needs_hours<-x$status$needs_hours%||%list();x$status$needs_hours$blood<-0
            write_core(x)
            record_blood_consumption(state$char_id, state$active_session_id, x$meta$day,
                                     "heart", source, 1, sindre)
            
            log_safe(
              paste0(
                "🫀 Consumed heart from ", source,
                ": +", sindre, " Sindre",
                if (isTRUE(is_heart_eater())) {
                  paste0(" (fully restored; +", heart_result$temporary_gained, " temporary Sindre)")
                } else if (heart_result$temporary_gained > 0) {
                  paste0(" (+", heart_result$temporary_gained, " temporary Sindre)")
                } else "",
                " → ", x$resources$sindre$cur, "/", tot
              ),
              TRUE,
              "gold"
            )
            
            log_safe("🧠 Heart consumption surges your addiction (+10 intake).", TRUE)
          }, ignoreInit = TRUE)
        })
      }
      
      heart_obs_ids(unique(c(heart_obs_ids(), new_ids)))
    }, ignoreInit = TRUE)
    
    # ------------------------------------------------------------
    # Addiction UI
    # ------------------------------------------------------------
    output$blood_stage_text <- renderText({
      a <- core_addiction()
      stage <- as.integer(a$stage %||% 1)
      
      desc <- switch(
        as.character(stage),
        "1" = "Stage 1: Requires ≥1 pint/day or lose 1d6 HP.",
        "2" = "Stage 2: Mismatched intake = exhaustion (hook later).",
        "3" = "Stage 3: Bite risk in combat on nat 1 (hook later).",
        "4" = "Stage 4: Bloodlust overrides free will (hook later).",
        "Stage 1: Requires ≥1 pint/day or lose 1d6 HP."
      )
      
      paste0("Addiction Stage ", stage, " (", as.integer(a$days_at_stage %||% 0), " days): ", desc)
    })

    output$blood_history_ui <- renderUI({
      char_rev()
      rows <- get_blood_consumption_history(state$char_id, 30L)
      if (!is.data.frame(rows) || !nrow(rows)) return(tags$div(style="opacity:.75;", "No recorded consumption yet."))
      tags$div(lapply(seq_len(nrow(rows)), function(i) {
        kind <- if (rows$consumption_type[[i]] == "heart") "🫀 Heart" else "🩸 Blood"
        tags$div(class="confirm-note", paste0("Day ", rows$campaign_day[[i]], " — ", kind,
          " from ", rows$source[[i]], ": ", rows$quantity[[i]],
          if (rows$consumption_type[[i]] == "blood") " pint(s)" else "",
          " · ", rows$sindre_value[[i]], " Sindre"))
      }))
    })
    
    required_intake_local <- function(a) {
      stage <- as.integer(a$stage %||% 1)
      prev <- as.numeric(a$previous_day_intake %||% 0)
      
      if (stage == 1) {
        return(1)
      } else if (stage == 2) {
        return(max(1, prev))
      } else if (stage == 3) {
        return(prev + 1)
      } else {
        return(max(5, prev))
      }
    }
    
    output$blood_intake_today <- renderText({
      a <- core_addiction()
      intake <- round(as.numeric(a$current_day_intake %||% 0), 2)
      paste0("🩸 Today: ", intake, " pint(s)")
    })
    
    output$blood_intake_required <- renderText({
      a <- core_addiction()
      
      intake <- as.numeric(a$current_day_intake %||% 0)
      req <- required_intake_local(a)
      
      status <- if (intake >= req) {
        "✅ Satisfied"
      } else {
        "⚠️ Deficient"
      }
      
      paste0("Required: ", round(req, 2), " pint(s) → ", status)
    })

    output$blood_status_cards <- renderUI({
      char_rev()
      x <- ensure_blood_state(state$char)
      cards <- character_rest_status_cards(x)
      cards <- Filter(function(card) card$key %in% c(
        "sufficient_blood", "insufficient_blood",
        "blood_addiction_1", "blood_addiction_2",
        "blood_addiction_3", "blood_addiction_4"
      ), cards)
      if (!length(cards)) return(NULL)

      div(
        class = "blood-status-hand",
        lapply(cards, function(card) {
          div(
            class = paste("blood-status-card", card$tone),
            title = card$reason,
            tags$img(src = card$image, alt = card$label),
            div(class = "blood-status-reason", card$reason)
          )
        })
      )
    })
    
    output$blood_stage_card <- renderUI({
      a <- core_addiction()
      stage <- as.integer(a$stage %||% 1)
      days <- as.integer(a$days_at_stage %||% 0)
      
      effects <- switch(
        as.character(stage),
        "1" = c("• Lose 1d6 HP if no blood consumed"),
        "2" = c("• Incorrect intake → +1 Exhaustion"),
        "3" = c("• +2 Exhaustion if starving", "• Bloodlust risk"),
        "4" = c("• Bloodlust overrides control", "• Severe exhaustion gain"),
        c("• Unknown stage")
      )
      
      div(
        style = "padding:10px; border:1px solid rgba(191,167,111,0.7); border-radius:10px;",
        tags$div(
          style = "font-weight:900; font-size:16px;",
          paste0("Stage ", stage, " (", days, " days)")
        ),
        tags$div(
          style = "margin-top:6px; opacity:.85;",
          "Effects:"
        ),
        tags$ul(
          style = "margin-top:4px;",
          lapply(effects, tags$li)
        )
      )
    })
  })
}
