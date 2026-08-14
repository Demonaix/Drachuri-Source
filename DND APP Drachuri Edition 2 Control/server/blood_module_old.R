# server/blood_module.R
library(shiny)

# ============================================================
# UI
# ============================================================
bloodTabUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Blood",
    value = "blood",
    
    div(
      id = ns("root"),
      class = "card",
      
      h3("🩸 Blood Inventory"),
      tags$p(style="opacity:.9;",
             "Store blood for later. You can drink part of a stash; Sindre gain is proportional."),
      
      tags$hr(),
      
      div(
        class = "card",
        h4("Add Blood"),
        fluidRow(
          column(4, numericInput(ns("blood_pints"),  "Pints", value = 1, min = 0.1, step = 0.1)),
          column(4, numericInput(ns("blood_sindre"), "Total Sindre value", value = 5, min = 0, step = 1)),
          column(4, textInput(ns("blood_source"),    "Source", value = ""))
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
        
        uiOutput(ns("blood_stage_card")),
        
        tags$hr(),
        
        fluidRow(
          column(6, strong(textOutput(ns("blood_intake_today")))),
          column(6, strong(textOutput(ns("blood_intake_required"))))
        )
      )
    )
  )
}

# ============================================================
# SERVER
# ============================================================
bloodTabServer <- function(
    id,
    state,                 # core reactiveValues(char=...)
    restoring = NULL,       # reactiveVal guard (optional)
    add_log = NULL,         # function(msg, toast=TRUE, flash="none")
    char_rev = NULL         # reactiveVal revision counter (optional)
) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    if (is.null(state)) stop("bloodTabServer: state is NULL")
    if (is.null(restoring)) restoring <- reactiveVal(FALSE)
    if (is.null(char_rev))  char_rev  <- reactiveVal(0)
    
    log_safe <- function(msg, toast = TRUE, flash = "none") {
      if (is.function(add_log)) {
        try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
      } else {
        message(msg)
      }
    }
    
    # ----------------------------
    # Core schema helpers (local)
    # ----------------------------
    empty_blood_df <- function() data.frame(
      id = character(),
      pints = numeric(),
      sindre = numeric(),
      source = character(),
      stringsAsFactors = FALSE
    )
    
    empty_hearts_df <- function() data.frame(
      id = character(),
      hearts = numeric(),
      sindre = numeric(),
      source = character(),
      stringsAsFactors = FALSE
    )
    
    
    ensure_core_blood <- function(x) {
      x <- validate_character(x)
      
      if (is.null(x$resources$blood) || !is.list(x$resources$blood)) {
        x$resources$blood <- x$resources$blood %||% list()
      }
      
      if (is.null(x$resources$hearts) || !is.data.frame(x$resources$hearts)) {
        x$resources$hearts <- empty_hearts_df()
      }
      
      if (is.null(x$resources$blood$inventory) || !is.data.frame(x$resources$blood$inventory)) {
        x$resources$blood$inventory <- empty_blood_df()
      } else {
        # make sure cols exist
        df <- x$resources$blood$inventory
        for (nm in names(empty_blood_df())) if (!nm %in% names(df)) df[[nm]] <- empty_blood_df()[[nm]]
        x$resources$blood$inventory <- df
      }
      
      # addiction
      if (is.null(x$resources$blood$addiction) || !is.list(x$resources$blood$addiction)) {
        x$resources$blood$addiction <- list()
      }
      a <- x$resources$blood$addiction
      a$stage <- as.integer(a$stage %||% 1)
      a$days_at_stage <- as.integer(a$days_at_stage %||% 0)
      a$previous_day_intake <- as.numeric(a$previous_day_intake %||% 0)
      a$current_day_intake  <- as.numeric(a$current_day_intake  %||% 0)
      x$resources$blood$addiction <- a
      
      x
    }
    
    core_blood_inventory <- reactive({
      x <- ensure_core_blood(state$char)
      x$resources$blood$inventory
    })
    
    core_addiction <- reactive({
      x <- ensure_core_blood(state$char)
      x$resources$blood$addiction
    })
    
    write_core <- function(x) {
      x <- validate_character(x)
      
      # Preserve current local blob HP fields if session HP is authoritative.
      # Session HP should stay in SQL; blob HP should not be accidentally reset here.
      if (is_session_active_for_character(state)) {
        current_char <- validate_character(state$char)
        x$resources$hp <- current_char$resources$hp
      }
      
      state$char <- x
    }
    
    get_hp_state <- function() {
      get_effective_hp_state(state)
    }
    
    heal_hp_state <- function(amount) {
      apply_healing_to_state(state, amount)
    }
    # ----------------------------
    # Hydrate (on load / character replace)
    # ----------------------------
    hydrate_blood <- function() {
      restoring(TRUE)
      on.exit(restoring(FALSE), add = TRUE)
      
      state$char <- ensure_core_blood(state$char)
    }
    
    observeEvent(TRUE, hydrate_blood(), once = TRUE)
    observeEvent(char_rev(), {
      if (isTRUE(restoring())) return()
      hydrate_blood()
    }, ignoreInit = TRUE)
    
    # ----------------------------
    # Add Blood
    # ----------------------------
    observeEvent(input$add_blood, {
      if (isTRUE(restoring())) return()
      
      x <- ensure_core_blood(state$char)
      inv <- x$resources$blood$inventory
      
      pints <- suppressWarnings(as.numeric(input$blood_pints %||% 0))
      sindre <- suppressWarnings(as.numeric(input$blood_sindre %||% 0))
      source <- trimws(as.character(input$blood_source %||% ""))
      
      if (is.na(pints) || pints <= 0) {
        log_safe("❌ Pints must be > 0.", toast = TRUE, flash = "red")
        return()
      }
      if (is.na(sindre) || sindre < 0) sindre <- 0
      if (!nzchar(source)) source <- "Unknown source"
      
      new_id <- paste0("b", as.integer(Sys.time()), sample(1000:9999, 1))
      
      new_entry <- data.frame(
        id = new_id,
        pints = pints,
        sindre = sindre,
        source = source,
        stringsAsFactors = FALSE
      )
      
     
      x$resources$blood$inventory <- rbind(inv, new_entry)
      write_core(x)
      
      log_safe(paste0("➕ Stored ", pints, " pint(s) from ", source, " (", sindre, " Sindre total)."),
               toast = TRUE)
    }, ignoreInit = TRUE)
    
    # ----------------------------
    # Inventory UI
    # ----------------------------
    output$blood_inventory_ui <- renderUI({
      blood <- core_blood_inventory()
      if (!is.data.frame(blood) || nrow(blood) == 0) {
        return(tags$div(style="opacity:.85;", "No blood stored."))
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
    

    # ----------------------------
    # Partial-drink modal flow
    # ----------------------------
    pending_id <- reactiveVal(NULL)
    
    observe({
      ids <- core_blood_inventory()$id
      if (length(ids) == 0) return()
      
      lapply(ids, function(id0) {
        local({
          this_id <- id0
          btn_id <- paste0("drink_", this_id)
          
          observeEvent(input[[btn_id]], {
            if (isTRUE(restoring())) return()
            
            x <- ensure_core_blood(state$char)
            inv <- x$resources$blood$inventory
            row <- inv[inv$id == this_id, , drop = FALSE]
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
      })
    })
    
    observeEvent(input$confirm_drink, {
      if (isTRUE(restoring())) return()
      removeModal()
      
      this_id <- pending_id()
      pending_id(NULL)
      if (is.null(this_id)) return()
      
      drink_pints <- suppressWarnings(as.numeric(input$drink_pints %||% 0))
      if (is.na(drink_pints) || drink_pints <= 0) return()
      
      x <- ensure_core_blood(state$char)
      
      inv <- x$resources$blood$inventory
      row <- inv[inv$id == this_id, , drop = FALSE]
      if (nrow(row) != 1) return()
      
      avail_pints  <- suppressWarnings(as.numeric(row$pints[[1]] %||% 0))
      avail_sindre <- suppressWarnings(as.numeric(row$sindre[[1]] %||% 0))
      source <- as.character(row$source[[1]] %||% "Unknown source")
      
      if (is.na(avail_pints) || avail_pints <= 0) return()
      if (is.na(avail_sindre) || avail_sindre < 0) avail_sindre <- 0
      
      drink_pints <- max(0, min(avail_pints, drink_pints))
      
      sindre_per_pint <- if (avail_pints <= 0) 0 else (avail_sindre / avail_pints)
      sindre_gain <- drink_pints * sindre_per_pint
      
      # Apply to CORE sindre (clamped to total)
      x <- validate_character(x)
      cur <- suppressWarnings(as.integer(x$resources$sindre$cur %||% 0))
      tot <- suppressWarnings(as.integer(x$resources$sindre$total %||% 0))
      if (is.na(cur)) cur <- 0
      if (is.na(tot)) tot <- 0
      
      path <- tolower(x$build$path %||% "")
      is_heart_eater <- grepl("heart eater", path)
      
      total_gain <- sindre_gain
      new_cur <- cur + total_gain
      
      overflow <- max(0, new_cur - tot)
      
      if (is_heart_eater) {
        # Allow overflow into temp
     
        
        x$resources$sindre$cur <- as.integer(min(new_cur, tot))
        x$resources$sindre$temp <- as.integer((x$resources$sindre$temp %||% 0) + overflow)
        
      } else {
        # Normal behaviour
        x$resources$sindre$cur <- as.integer(min(new_cur, tot))
      }
      
      # Reduce the blood entry proportionally
      remaining_pints  <- avail_pints - drink_pints
      remaining_sindre <- max(0, avail_sindre - sindre_gain)
      
      if (remaining_pints <= 1e-6) {
        inv <- inv[inv$id != this_id, , drop = FALSE]
      } else {
        inv[inv$id == this_id, "pints"]  <- remaining_pints
        inv[inv$id == this_id, "sindre"] <- remaining_sindre
      }
      x$resources$blood$inventory <- inv
      
      # Addiction intake bookkeeping
      a <- x$resources$blood$addiction
      a$current_day_intake <- as.numeric(a$current_day_intake %||% 0) + drink_pints
      x$resources$blood$addiction <- a
      
      if (is_heart_eater) {
        heal_amt <- as.integer(round(5 * drink_pints))
        heal_res <- heal_hp_state(heal_amt)
        
        if (!is.null(heal_res)) {
          gained <- heal_res$hp_after - heal_res$hp_before
          log_safe(paste0("❤️ Blood heals you for ", gained, " HP."), toast = TRUE)
        } else {
          log_safe("⚠️ Blood healing failed.", toast = TRUE, flash = "red")
        }
      }
      
      write_core(x)
      
      log_safe(paste0(
        "🩸 Drank ", round(drink_pints, 2), " pint(s) from ", source,
        ": +", round(sindre_gain, 2), " Sindre",
        if (is_heart_eater) paste0(" (+", round(overflow, 2), " overflow)") else "",
        " → ", x$resources$sindre$cur, "/", tot
      ), toast = TRUE, flash = "gold")
    
    
    })
    # ----------------------------
    # Daily check (placeholder)
    # ----------------------------
    
    
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
 


#Required intake for blood, note copy of this also in global
    required_intake <- function(a) {
      stage <- as.integer(a$stage %||% 1)
      prev  <- as.numeric(a$previous_day_intake %||% 0)
      
      if (stage == 1) {
        return(1)
      } else if (stage == 2) {
        return(prev)
      } else if (stage == 3) {
        return(prev + 1)
      } else {
        return(max(5, prev))  # tweak later if needed
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
  req <- required_intake(a)
  
  status <- if (intake >= req) {
    "✅ Satisfied"
  } else {
    "⚠️ Deficient"
  }
  
  paste0("Required: ", round(req, 2), " pint(s) → ", status)
})

#Heart logic
core_hearts_inventory <- reactive({
  x <- ensure_core_blood(state$char)
  x$resources$hearts
})

observeEvent(input$add_heart, {
  if (isTRUE(restoring())) return()
  
  x <- ensure_core_blood(state$char)
  inv <- x$resources$hearts
  
  hearts <- as.numeric(input$heart_count %||% 0)
  sindre <- as.numeric(input$heart_sindre %||% 0)
  source <- trimws(input$heart_source %||% "")
  
  if (hearts <= 0) {
    log_safe("❌ Must add at least 1 heart.", TRUE, "red")
    return()
  }
  
  if (!nzchar(source)) source <- "Unknown source"
  
  new_id <- paste0("h", as.integer(Sys.time()), sample(1000:9999, 1))
  
  new_entry <- data.frame(
    id = new_id,
    hearts = hearts,
    sindre = sindre,
    source = source,
    stringsAsFactors = FALSE
  )
  
  x$resources$hearts <- rbind(inv, new_entry)
  write_core(x)
  
  log_safe(paste0("🫀 Stored ", hearts, " heart(s) from ", source,
                  " (", sindre, " Sindre each)."), TRUE)
}, ignoreInit = TRUE)


output$hearts_inventory_ui <- renderUI({
  hearts <- core_hearts_inventory()
  
  if (!is.data.frame(hearts) || nrow(hearts) == 0) {
    return(tags$div(style="opacity:.85;", "No hearts stored."))
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
        column(2, actionButton(session$ns(btn_id), "🫀 Use", class="btn-danger btn-sm"))
      )
    })
  )
})

#New blood stage card
output$blood_stage_card <- renderUI({
  a <- core_addiction()
  stage <- as.integer(a$stage %||% 1)
  days  <- as.integer(a$days_at_stage %||% 0)
  
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

observe({
  ids <- core_hearts_inventory()$id
  if (length(ids) == 0) return()
  
  lapply(ids, function(id0) {
    local({
      this_id <- id0
      btn_id <- paste0("use_heart_", this_id)
      
      observeEvent(input[[btn_id]], {
        if (isTRUE(restoring())) return()
        
        x <- ensure_core_blood(state$char)
        inv <- x$resources$hearts
        
        row <- inv[inv$id == this_id, , drop = FALSE]
        if (nrow(row) != 1) return()
        
        hearts <- row$hearts[[1]]
        sindre <- row$sindre[[1]]
        source <- row$source[[1]]
        
        # Apply sindre (WITH overflow like Heart Eater)
        cur <- x$resources$sindre$cur %||% 0
        tot <- x$resources$sindre$total %||% 0
        
        new_cur <- cur + sindre
        overflow <- max(0, new_cur - tot)
        
        x$resources$sindre$cur <- min(new_cur, tot)
        x$resources$sindre$temp <- (x$resources$sindre$temp %||% 0) + overflow
        
        # Reduce heart count
        if (hearts <= 1) {
          inv <- inv[inv$id != this_id, , drop = FALSE]
        } else {
          inv[inv$id == this_id, "hearts"] <- hearts - 1
        }
        
        x$resources$hearts <- inv
        
        #blood addiction mechanics
        a <- x$resources$blood$addiction
        a$current_day_intake <- as.numeric(a$current_day_intake %||% 0) + 10
        x$resources$blood$addiction <- a
        
        write_core(x)
        
        log_safe(
          paste0(
            "🫀 Consumed heart from ", source,
            ": +", sindre, " Sindre",
            if (overflow > 0) paste0(" (+", overflow, " overflow)") else "",
            " → ", x$resources$sindre$cur, "/", tot
          ),
          TRUE, "gold"
        )
        
        log_safe("🧠 Heart consumption surges your addiction (+10 intake).", TRUE)
        
      }, ignoreInit = TRUE)
    })
  })
})
  })
}