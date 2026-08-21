# server/magic_module.R
library(shiny)

# ============================================================
# UI
# ============================================================
magicTabUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$audio(
      id = "magic_sfx",
      src = "magic.wav",
      loop = NA,
      preload = "auto",
      style = "display:none;"
    )
    )
  
  tabPanel(
    title = "Magic",
    value = "magic",
    
    # local styles for this tab (self-contained)
    tags$style(HTML(paste0("
      /* --- magic layout helpers --- */
      #", ns("root"), " .magic-grid{
        display:grid;
        grid-template-columns: repeat(5, minmax(0, 1fr));
        gap: 10px;
      }
      @media (max-width: 920px){
        #", ns("root"), " .magic-grid{ grid-template-columns: repeat(2, minmax(0, 1fr)); }
      }
      @media (max-width: 520px){
        #", ns("root"), " .magic-grid{ grid-template-columns: 1fr; }
      }

      #", ns("root"), " .magic-card{
        border: 1px solid rgba(191,167,111,0.75);
        background: rgba(255,255,245,0.86);
        border-radius: 14px;
        padding: 10px 12px;
        box-shadow: 0 6px 16px rgba(0,0,0,0.08);
      }
      
      .magic-card .pill.active {
  background: linear-gradient(135deg, #d4af37, #f5e6a3);
  color: black;
  border: 1px solid #bfa76f;
}
      #", ns("root"), " .magic-card .k{
        font-weight: 900;
        opacity: 0.85;
        font-size: 12px;
        letter-spacing: 0.2px;
      }
      #", ns("root"), " .magic-card .tier{
        margin-top: 4px;
        font-family: monospace;
        font-size: 12px;
        opacity: 0.8;
      }
      #", ns("root"), " .magic-card .val{
        margin-top: 6px;
        font-family: monospace;
        font-size: 20px;
        font-weight: 900;
      }
      #", ns("root"), " .magic-card .hint{
        margin-top: 6px;
        opacity: 0.75;
        font-size: 12px;
      }

      #", ns("root"), " .pill-row{
        display:flex;
        flex-wrap: wrap;
        gap: 8px;
        margin-top: 8px;
      }
      #", ns("root"), " .pill{
        display:inline-flex;
        gap: 8px;
        align-items:center;
        padding: 6px 10px;
        border-radius: 999px;
        border: 1px solid rgba(191,167,111,0.7);
        background: rgba(255,255,245,0.75);
        font-weight: 800;
      }
      #", ns("root"), " .pill .muted{ opacity: .85; font-weight: 900; }
      #", ns("root"), " .pill .mono{ font-family: monospace; }

      #", ns("root"), " .cost-box{
        display:inline-flex;
        gap: 10px;
        align-items:center;
        padding: 8px 12px;
        border-radius: 14px;
        border: 1px solid rgba(191,167,111,0.75);
        background: rgba(255,255,245,0.86);
        font-weight: 900;
      }
      #", ns("root"), " .cost-box .mono{ font-family: monospace; font-size: 16px; }
    "))),  # ✅ correct closing
    
    div(
      id = ns("root"),
      class = "card",
      
      h3("✨ Sindre & Casting"),
      tags$p(
        style = "opacity:.9;",
        "Sindre is your magical reserve. Casting pressure depends on cost, flow, your level, and your build strength."
      ),
      tags$hr(),
      
      # -------------------------
      # UNLOCKED MAGIC TYPES (placeholder)
      # -------------------------
      div(
        class = "card",
        h4("Unlocked Magic Types"),
        uiOutput(ns("magic_types_ui"))
      ),
      
      # -------------------------
      # CASTING
      # -------------------------
      uiOutput(ns("spell_casting_ui")),
        
    
      # -------------------------
      # CALIBRATION (Edit/Save)
      # -------------------------
      div(
        class = "card",
        h4("Class Calibration"),
        uiOutput(ns("calib_cards_ui")),
        tags$div(style = "height:10px;"),
        uiOutput(ns("calib_edit_ui"))
      ),
      
      div(
        class = "card",
        h4("Magical Nature"),
        uiOutput(ns("sorcerer_identity_ui"))
      ),
      
      
      # -------------------------
      # DEEP MAGIC SPELLS (placeholder)
      # -------------------------
      div(
        class = "card",
        h4("Deep Magic Spells"),
        tags$p(
          style="opacity:.85;",
          "Fixed-cost spells with descriptions. Unlocks will come from the Level-Up / Classing module later."
        ),
        uiOutput(ns("deep_magic_ui"))
      )
    )
  )
}

# ============================================================
# SERVER
# ============================================================
magicTabServer <- function(
    id,
    state,
    restoring,
    add_log,
    char_rev
) {
  moduleServer(id, function(input, output, session) {
    
    MAGIC_TYPES <- c(
      "Mechanical",
      "Natural",
      "Fire",
      "Cold",
      "Lightning",
      "Radiant",   # better than "Light"
      "Necrotic",  # better than "Radiation"
      "Chemical",
      "Nuclear"
    )
    
    output$magic_types_ui <- renderUI({
      x <- validate_character(state$char)
      
      selected <- character(0)
      if (is.list(x$magic) && !is.null(x$magic$types)) {
        selected <- x$magic$types
      }
      all_types <- MAGIC_TYPES
      
      tags$div(
        class = "pill-row",
        
        lapply(all_types, function(type) {
          
          active <- type %in% selected
          
          tags$div(
            class = paste("pill", if (active) "active" else ""),
            
            style = paste0(
              "cursor:pointer;",
              if (active) "background:#d4af37; color:black;" else ""
            ),
            
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
              session$ns("toggle_magic_type"),
              type
            ),
            
            tags$span(class="mono", type)
          )
        })
      )
    })
    
    observeEvent(input$toggle_magic_type, {
      if (isTRUE(restoring())) return()
      
      type <- input$toggle_magic_type
      
      x <- validate_character(state$char)
      
      current <- x$magic$types %||% character(0)
      
      if (type %in% current) {
        # REMOVE
        current <- setdiff(current, type)
        log_safe(paste0("❌ Removed ", type, " magic"))
      } else {
        # ADD
        current <- c(current, type)
        log_safe(paste0("✨ Learned ", type, " magic"), flash = "gold")
      }
      
      if (is.null(x$magic) || !is.list(x$magic)) {
        x$magic <- list()
      }
      x$magic$types <- current
      state$char <- x
    })
    
    # -------------------------
    # Guards / utilities
    # -------------------------
    if (is.null(state)) stop("magicTabServer: state is NULL")
    if (is.null(restoring)) restoring <- reactiveVal(FALSE)
    if (is.null(char_rev))  char_rev  <- reactiveVal(0)
    
    log_safe <- function(msg, toast = TRUE, flash = "none") {
      if (is.function(add_log)) {
        try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
      } else {
        message(msg)
      }
    }
    
    # -------------------------
    # Spell cost matrix (8x6)
    # -------------------------
    spell_cost_matrix <- matrix(
      c(
        1, 2, 4, 8, 16, 32,
        2, 4, 8, 16, 32, 64,
        4, 8, 16, 32, 64, 128,
        8, 16, 32, 64, 128, 256,
        16, 32, 64, 128, 256, 512,
        32, 64, 128, 256, 512, 1024,
        64, 128, 256, 512, 1024, 2048,
        128, 256, 512, 1024, 2048, 4096
      ),
      nrow = 8, byrow = TRUE,
      dimnames = list(
        opt1 = c(
          "A teeny-tiny grain of sand",
          "A small pebble",
          "A rock",
          "A large rock",
          "A weak wall",
          "A big ass wall",
          "A small castle",
          "A freakin' mountain"
        ),
        opt2 = c(
          "A slight nudge",
          "I want to move it a bit",
          "This bitch is going somewhere",
          "Watch out mothercluckers",
          "Good lord it's a comin'",
          "The face of Annwn will never be the same again"
        )
      )
    )
    
    # Spell DC pick list (7 options)
    spell_dc_levels <- 1:7
    spell_dc_level_names <- c(
      "A baby could do this, I don't care what happens I just want magic",
      "A toddler could think this through a basic action nothing more",
      "Ooh things are getting fancy... oh maybe not really, just a slight flourish",
      "Okay now we can call this respectable magic. Whatever you are doing requires some form of concentration or training",
      "Getting a bit difficult here. Do you really want this rock to move, change shape, sing a song and then melt into a pool of magma?",
      "Dammmnn gurrll you better know what you are doing",
      "Wow just wow. You better be some goddess reborn or there is no chance in hell this is going to plan"
    )
    
    # Map DC "level" -> numeric base DC used by the mechanic.
    # Tweak later if you want different numbers.
    base_dc_from_level <- function(level) {
      level <- suppressWarnings(as.integer(level))
      if (is.na(level)) level <- 1L
      level <- max(1L, min(7L, level))
      8L + (level - 1L) * 2L  # 8,10,12,14,16,18,20
    }
    
    # Hydrate selectInput choices once
    observeEvent(TRUE, {
      updateSelectInput(
        session, "spell_size",
        choices = rownames(spell_cost_matrix),
        selected = rownames(spell_cost_matrix)[1]
      )
      updateSelectInput(
        session, "spell_move",
        choices = colnames(spell_cost_matrix),
        selected = colnames(spell_cost_matrix)[1]
      )
      updateSelectInput(
        session, "spell_dc_level",
        choices = setNames(spell_dc_levels, spell_dc_level_names),
        selected = 1
      )
    }, once = TRUE)
    
    # -------------------------
    # classification_table (safe fallback)
    # -------------------------
    safe_class_table <- reactive({
      if (exists("classification_table", inherits = TRUE)) {
        tbl <- get("classification_table", inherits = TRUE)
        ok <- is.data.frame(tbl) &&
          all(c("ClassLevel", "RefillRate", "SindreLevel", "MaxFlow", "Locked", "Bound") %in% names(tbl))
        if (ok) return(tbl)
      }
      
      data.frame(
        ClassLevel  = 0:20,
        RefillRate  = rep(0L, 21),
        SindreLevel = rep(0L, 21),
        MaxFlow     = rep(0L, 21),
        Locked      = rep(0L, 21),
        Bound       = rep(0L, 21)
      )
    })
    
    tier_min <- reactive({
      tbl <- safe_class_table()
      v <- suppressWarnings(min(tbl$ClassLevel, na.rm = TRUE))
      if (is.na(v)) 0 else as.integer(v)
    })
    tier_max <- reactive({
      tbl <- safe_class_table()
      v <- suppressWarnings(max(tbl$ClassLevel, na.rm = TRUE))
      if (is.na(v)) 20 else as.integer(v)
    })
    
    # Snap tier to nearest available ClassLevel in the table
    row_for <- function(idx) {
      tbl <- safe_class_table()
      idx <- suppressWarnings(as.integer(idx))
      if (is.na(idx)) idx <- tier_min()
      
      levels <- suppressWarnings(as.integer(tbl$ClassLevel))
      levels <- sort(unique(levels[!is.na(levels)]))
      if (length(levels) == 0) levels <- 0:20
      
      nearest <- levels[which.min(abs(levels - idx))]
      out <- tbl[tbl$ClassLevel == nearest, , drop = FALSE]
      if (nrow(out) == 0) out <- tbl[1, , drop = FALSE]
      out
    }
    
    # -------------------------
    # Local UI state
    # -------------------------
    editing_calib <- reactiveVal(FALSE)
    
    # Cache race (never mutate state$char from reactives)
    race_cache <- reactiveVal("")
    observeEvent(state$char, {
      x <- validate_character(state$char)
      r <- x$meta$race %||% ""
      if (!identical(r, race_cache())) race_cache(r)
    }, ignoreInit = FALSE)
    
    is_tylwyth <- reactive({
      grepl("tylwyth", tolower(race_cache() %||% ""))
    })
    
    # -------------------------
    # Core sindre (saved calibration + current)
    # -------------------------
    core_sindre <- reactive({
      x <- validate_character(state$char)
      s <- x$resources$sindre %||% list()
      
      s$cur    <- as.integer(s$cur %||% 0)
      s$total  <- as.integer(s$total %||% 0)
      s$regen  <- as.integer(s$regen %||% 0)
      s$flow   <- as.integer(s$flow %||% 0)
      s$locked <- as.integer(s$locked %||% 0)
      s$bound  <- as.integer(s$bound %||% 0)
      
      if (is.null(s$tiers) || !is.list(s$tiers)) s$tiers <- list()
      s$tiers$refill <- as.integer(s$tiers$refill %||% 1)
      s$tiers$total  <- as.integer(s$tiers$total  %||% 1)
      s$tiers$flow   <- as.integer(s$tiers$flow   %||% 1)
      s$tiers$locked <- as.integer(s$tiers$locked %||% 1)
      s$tiers$bound  <- as.integer(s$tiers$bound  %||% 1)
      
      s
    })
    
    tier_inputs_from_core <- function() {
      s <- core_sindre()
      list(
        refill = s$tiers$refill %||% 1,
        total  = s$tiers$total  %||% 1,
        flow   = s$tiers$flow   %||% 1,
        locked = s$tiers$locked %||% 1,
        bound  = s$tiers$bound  %||% 1
      )
    }
    
    # Preview computed from current tier inputs (NOT applied until Save)
    computed_from_tiers <- reactive({
      ref <- row_for(input$class_refill %||% tier_min())
      tot <- row_for(input$class_total  %||% tier_min())
      flo <- row_for(input$class_flow   %||% tier_min())
      lok <- row_for(input$class_locked %||% tier_min())
      bnd <- row_for(input$class_bound  %||% tier_min())
      
      tyl <- is_tylwyth()
      
      list(
        regen  = if (tyl) 0L else as.integer(ref$RefillRate[[1]] %||% 0),
        total  = as.integer(tot$SindreLevel[[1]] %||% 0),
        flow   = as.integer(flo$MaxFlow[[1]] %||% 0),
        locked = as.integer(lok$Locked[[1]] %||% 0),
        bound  = if (tyl) as.integer(bnd$Bound[[1]] %||% 0) else 0L,
        
        tier_refill = as.integer(ref$ClassLevel[[1]] %||% tier_min()),
        tier_total  = as.integer(tot$ClassLevel[[1]] %||% tier_min()),
        tier_flow   = as.integer(flo$ClassLevel[[1]] %||% tier_min()),
        tier_locked = as.integer(lok$ClassLevel[[1]] %||% tier_min()),
        tier_bound  = as.integer(bnd$ClassLevel[[1]] %||% tier_min())
      )
    })
    
    # -------------------------
    # Hydrate from core (character load / replace)
    # -------------------------
    hydrate_magic_ui <- function() {
      s <- core_sindre()
      t <- tier_inputs_from_core()
      
      restoring(TRUE)
      on.exit(restoring(FALSE), add = TRUE)
      
      updateNumericInput(session, "current_sindre", value = as.integer(s$cur %||% 0))
      
      # These exist only when editing, but safe to update.
      updateNumericInput(session, "class_refill", value = as.integer(t$refill %||% 1))
      updateNumericInput(session, "class_total",  value = as.integer(t$total  %||% 1))
      updateNumericInput(session, "class_flow",   value = as.integer(t$flow   %||% 1))
      updateNumericInput(session, "class_locked", value = as.integer(t$locked %||% 1))
      updateNumericInput(session, "class_bound",  value = as.integer(t$bound  %||% 1))
      
      editing_calib(FALSE)
    }
    
    observeEvent(TRUE, hydrate_magic_ui(), once = TRUE)
    observeEvent(char_rev(), {
      if (isTRUE(restoring())) return()
      hydrate_magic_ui()
    }, ignoreInit = TRUE)
    
    # -------------------------
    # Current Sindre always writes to core (like weapons damage)
    # -------------------------
    observeEvent(input$current_sindre, {
      if (isTRUE(restoring())) return()
      
      x <- validate_character(state$char)
      s <- x$resources$sindre %||% list()
      tot <- suppressWarnings(as.integer(s$total %||% 0))
      cur_in <- suppressWarnings(as.integer(input$current_sindre %||% 0))
      if (is.na(tot)) tot <- 0
      if (is.na(cur_in)) cur_in <- 0
      
      cur <- max(0, min(tot, cur_in))
      
      if (!identical(as.integer(s$cur %||% NA), as.integer(cur))) {
        x$resources$sindre$cur <- cur
        state$char <- x
      }
      
      if (!identical(cur, cur_in)) {
        updateNumericInput(session, "current_sindre", value = cur)
      }
    }, ignoreInit = TRUE)
    
    # -------------------------
    # Apply calibration (SAVE) into core
    # -------------------------
    apply_calibration_to_core <- function() {
      x <- validate_character(state$char)
      c <- computed_from_tiers()
      
      # Persist tiers the user typed (raw), not snapped tier
      x$resources$sindre$tiers <- list(
        refill = as.integer(input$class_refill %||% 1),
        total  = as.integer(input$class_total  %||% 1),
        flow   = as.integer(input$class_flow   %||% 1),
        locked = as.integer(input$class_locked %||% 1),
        bound  = as.integer(input$class_bound  %||% 1)
      )
      
      cur <- suppressWarnings(as.integer(x$resources$sindre$cur %||% 0))
      if (is.na(cur)) cur <- 0
      cur <- max(0, min(c$total, cur))
      
      x$resources$sindre$total  <- c$total
      x$resources$sindre$regen  <- c$regen
      x$resources$sindre$flow   <- c$flow
      x$resources$sindre$locked <- c$locked
      x$resources$sindre$bound  <- c$bound
      x$resources$sindre$cur    <- cur
      
      state$char <- x
      
      if (!isTRUE(restoring())) {
        updateNumericInput(session, "current_sindre", value = cur)
      }
    }
    
    # -------------------------
    # UI: Calibration cards (collapsed)
    # -------------------------
    output$calib_cards_ui <- renderUI({
      if (isTRUE(editing_calib())) return(NULL)
      
      t <- tier_inputs_from_core()
      r_ref <- row_for(t$refill)
      r_tot <- row_for(t$total)
      r_flo <- row_for(t$flow)
      r_lok <- row_for(t$locked)
      r_bnd <- row_for(t$bound)
      
      card <- function(label, tier, value, hint = NULL) {
        tags$div(
          class = "magic-card",
          tags$div(class = "k", label),
          tags$div(class = "tier", paste0("Tier ", tier)),
          tags$div(class = "val", as.character(value)),
          if (!is.null(hint)) tags$div(class = "hint", hint)
        )
      }
      
      bound_val <- if (is_tylwyth()) as.integer(r_bnd$Bound[[1]] %||% 0) else "—"
      bound_hint <- if (is_tylwyth()) "Tylwyth only" else "Only for Tylwyth"
      
      tagList(
        tags$div(
          class = "magic-grid",
          card("Regen / hr",  as.integer(r_ref$ClassLevel[[1]] %||% 0), if (is_tylwyth()) 0 else as.integer(r_ref$RefillRate[[1]] %||% 0)),
          card("Total",       as.integer(r_tot$ClassLevel[[1]] %||% 0), as.integer(r_tot$SindreLevel[[1]] %||% 0)),
          card("Max Flow",    as.integer(r_flo$ClassLevel[[1]] %||% 0), as.integer(r_flo$MaxFlow[[1]] %||% 0)),
          card("Locked",      as.integer(r_lok$ClassLevel[[1]] %||% 0), as.integer(r_lok$Locked[[1]] %||% 0)),
          card("Bound",       as.integer(r_bnd$ClassLevel[[1]] %||% 0), bound_val, bound_hint)
        ),
        tags$div(style="margin-top:10px;",
                 actionButton(session$ns("calib_edit"), "Edit", class = "btn btn-default"))
      )
    })
    
    output$calib_edit_ui <- renderUI({
      if (!isTRUE(editing_calib())) return(NULL)
      
      tagList(
        div(
          class = "card",
          h5("Edit tiers (not applied until Save)"),
          
          fluidRow(
            column(4,
                   numericInput(session$ns("class_refill"), "Refill tier",
                                value = isolate(input$class_refill %||% tier_min()),
                                min = tier_min(), max = tier_max())),
            column(4,
                   numericInput(session$ns("class_total"), "Total tier",
                                value = isolate(input$class_total %||% tier_min()),
                                min = tier_min(), max = tier_max())),
            column(4,
                   numericInput(session$ns("class_flow"), "Flow tier",
                                value = isolate(input$class_flow %||% tier_min()),
                                min = tier_min(), max = tier_max()))
          ),
          fluidRow(
            column(6,
                   numericInput(session$ns("class_locked"), "Locked tier",
                                value = isolate(input$class_locked %||% tier_min()),
                                min = tier_min(), max = tier_max())),
            column(6,
                   numericInput(session$ns("class_bound"), "Bound tier (Tylwyth only)",
                                value = isolate(input$class_bound %||% tier_min()),
                                min = tier_min(), max = tier_max()))
          ),
          
          tags$div(style="opacity:.9; margin-top:8px;", {
            c <- computed_from_tiers()
            sprintf("Preview → Total: %d | Regen/hr: %d | Flow: %d | Locked: %d | Bound: %d",
                    c$total, c$regen, c$flow, c$locked, c$bound)
          }),
          
          tags$div(
            style="margin-top:10px; display:flex; gap:10px;",
            actionButton(session$ns("calib_save"), "Save", class = "btn btn-primary"),
            actionButton(session$ns("calib_cancel"), "Cancel", class = "btn btn-default")
          )
        )
      )
    })
    
    observeEvent(input$calib_edit, {
      if (isTRUE(restoring())) return()
      
      t <- tier_inputs_from_core()
      
      restoring(TRUE)
      on.exit(restoring(FALSE), add = TRUE)
      
      updateNumericInput(session, "class_refill", value = as.integer(t$refill %||% tier_min()))
      updateNumericInput(session, "class_total",  value = as.integer(t$total  %||% tier_min()))
      updateNumericInput(session, "class_flow",   value = as.integer(t$flow   %||% tier_min()))
      updateNumericInput(session, "class_locked", value = as.integer(t$locked %||% tier_min()))
      updateNumericInput(session, "class_bound",  value = as.integer(t$bound  %||% tier_min()))
      
      editing_calib(TRUE)
    }, ignoreInit = TRUE)
    
    observeEvent(input$calib_cancel, {
      if (isTRUE(restoring())) return()
      editing_calib(FALSE)
    }, ignoreInit = TRUE)
    
    observeEvent(input$calib_save, {
      if (isTRUE(restoring())) return()
      apply_calibration_to_core()
      editing_calib(FALSE)
      log_safe("💾 Saved magic calibration to core.", toast = TRUE, flash = "gold")
    }, ignoreInit = TRUE)
    
    # -------------------------
    # UI: Refill controls (placeholders)
    # -------------------------
    output$refill_controls_ui <- renderUI({
      if (is_tylwyth()) {
        tagList(
          tags$p(
            style="opacity:.85;",
            "Sindre is restored via blood consumption (handled in Blood module)."
          )
        )
      } else {
        tagList(
          tags$p(style="opacity:.85;",
                 "Refill will come from Rest options once the Rest module is added."),
          tags$div(
            style="display:flex; gap:10px; flex-wrap:wrap;",
            actionButton(session$ns("short_rest"), "Short Rest (placeholder)", class="btn btn-default"),
            actionButton(session$ns("long_rest"),  "Long Rest (placeholder)",  class="btn btn-default")
          )
        )
      }
    })
    

    observeEvent(input$short_rest, {
      log_safe("🛌 Short Rest is not wired yet (Rest module coming soon).", toast = TRUE)
    }, ignoreInit = TRUE)
    observeEvent(input$long_rest, {
      log_safe("🌙 Long Rest is not wired yet (Rest module coming soon).", toast = TRUE)
    }, ignoreInit = TRUE)
    
    # -------------------------
    # UI: Pool summary
    # -------------------------
    output$pool_summary_ui <- renderUI({
      s <- core_sindre()
      tags$div(
        style="opacity:.9; margin-top:6px;",
        sprintf(
          "Saved → Total: %d | Regen/hr: %d | Flow: %d | Locked: %d | Bound: %d",
          as.integer(s$total %||% 0),
          as.integer(s$regen %||% 0),
          as.integer(s$flow %||% 0),
          as.integer(s$locked %||% 0),
          as.integer(s$bound %||% 0)
        )
      )
    })
    
    # -------------------------
    # Unlocked magic types (placeholder pills)
    # Later module can write: state$char$magic$types (character vector)
    # -------------------------
    magic_types <- reactive({
      x <- validate_character(state$char)
      types <- NULL
      if (is.list(x$magic) && !is.null(x$magic$types)) types <- x$magic$types
      if (is.null(types) && !is.null(x$magic_types)) types <- x$magic_types
      if (is.null(types)) types <- c("Thermal Magic", "Mechanical Magic") # placeholder
      types <- unique(as.character(types))
      types[nzchar(types)]
    })
    

    
    # -------------------------
    # Deep magic spells (placeholder)
    # Later module can write: state$char$magic$deep_spells (df or list)
    # -------------------------
    deep_spells <- reactive({
      x <- validate_character(state$char)
      ds <- NULL
      if (is.list(x$magic) && !is.null(x$magic$deep_spells)) ds <- x$magic$deep_spells
      
      if (is.null(ds)) {
        ds <- data.frame(
          name = c("Ember Oath", "Iron Hymn"),
          cost = c(25, 40),
          desc = c(
            "A binding vow of heat and ash. (placeholder)",
            "A ringing mechanical chant that bends motion. (placeholder)"
          ),
          stringsAsFactors = FALSE
        )
      }
      
      if (is.data.frame(ds)) return(ds)
      
      # list -> data.frame best effort
      tryCatch({
        do.call(rbind, lapply(ds, function(it) {
          data.frame(
            name = as.character(it$name %||% ""),
            cost = as.integer(it$cost %||% 0),
            desc = as.character(it$desc %||% ""),
            stringsAsFactors = FALSE
          )
        }))
      }, error = function(e) {
        data.frame(name=character(), cost=integer(), desc=character(), stringsAsFactors = FALSE)
      })
    })
    
    output$deep_magic_ui <- renderUI({
      ds <- deep_spells()
      if (!is.data.frame(ds) || nrow(ds) == 0) {
        return(tags$div(style="opacity:.75;", "No deep spells unlocked yet."))
      }
      
      tags$div(
        lapply(seq_len(nrow(ds)), function(i) {
          div(
            class="magic-card",
            tags$div(class="k", ds$name[[i]] %||% ""),
            tags$div(class="tier", paste0("Cost: ", as.integer(ds$cost[[i]] %||% 0), " Sindre")),
            tags$div(class="hint", ds$desc[[i]] %||% "")
          )
        })
      )
    })
    
    # -------------------------
    # Casting: compute cost + DC from dropdowns
    # -------------------------
    spell_cost <- reactive({
      size <- input$spell_size %||% rownames(spell_cost_matrix)[1]
      move <- input$spell_move %||% colnames(spell_cost_matrix)[1]
      
      if (!size %in% rownames(spell_cost_matrix)) size <- rownames(spell_cost_matrix)[1]
      if (!move %in% colnames(spell_cost_matrix)) move <- colnames(spell_cost_matrix)[1]
      
      base_cost <- as.integer(spell_cost_matrix[size, move])
      
      if (isTRUE(input$half_cost)) {
        base_cost <- ceiling(base_cost / 2)
      }
      
      base_cost
    })
    
    output$spell_cost_ui <- renderUI({
      cost <- spell_cost()
      tags$div(
        class = "cost-box",
        tags$span(style="opacity:.85;", "Cost"),
        tags$span(class="mono", cost)
      )
    })
    
    # -------------------------
    # SPELL DC + CASTING
    # IMPORTANT: uses *saved core* values for flow/total,
    # and dropdowns for base DC + cost.
    # -------------------------
    output$computed_spell_dc <- renderText({
      s <- core_sindre()
      x <- validate_character(state$char)
      
      cost <- spell_cost()
      
      bld_mod <- mod_calc(x$abilities$bld_str %||% 10)
      level_mod <- floor((x$build$level %||% 1) / 5)
      
      cur <- as.integer((s$cur %||% 0) + (s$temp %||% 0))
      if (is.na(cur)) cur <- 0
      
      total <- as.integer(s$total %||% 0)
      flow  <- as.integer(s$flow  %||% 0)
      
      pool_ratio <- if (total == 0) 0 else (total - cur) / total
      sindre_mod <- round(pool_ratio * 3)
      
      control_mod <- bld_mod + level_mod + sindre_mod
      cost_pressure <- round((cost / max(1, flow)) * 4)
      
      base_dc <- base_dc_from_level(input$spell_dc_level %||% 1)
      if (isTRUE(input$half_dc)) base_dc <- ceiling(base_dc / 2)
      
      target_dc <- base_dc + cost_pressure - control_mod
      target_dc <- max(5, min(25, target_dc))
      
      paste0("🎯 Target Spell DC to beat: ", target_dc, " (base ", base_dc, ")")
    })
    
    #Spell outcome 
    spell_outcome <- reactive({
      s <- core_sindre()
      x <- validate_character(state$char)
      
      cost <- spell_cost()
      
      bld_mod <- mod_calc(x$abilities$bld_str %||% 10)
      level_mod <- floor((x$build$level %||% 1) / 5)
      
      cur <- as.integer((s$cur %||% 0) + (s$temp %||% 0))
      if (is.na(cur)) cur <- 0
      
      total <- as.integer(s$total %||% 0)
      flow  <- as.integer(s$flow  %||% 0)
      
      pool_ratio <- if (total == 0) 0 else (total - cur) / total
      sindre_mod <- round(pool_ratio * 3)
      
      control_mod <- bld_mod + level_mod + sindre_mod
      cost_pressure <- round((cost / max(1, flow)) * 4)
      
      base_dc <- base_dc_from_level(input$spell_dc_level %||% 1)
      if (isTRUE(input$half_dc)) base_dc <- ceiling(base_dc / 2)
      
      target_dc <- base_dc + cost_pressure - control_mod
      target_dc <- max(5, min(25, target_dc))
      
      # 🎯 Success chance (d20 system)
      success_chance <- (21 - target_dc) / 20
      success_chance <- max(0, min(1, success_chance))
      
      fail_chance <- 1 - success_chance
      
      # 💥 Wild magic (only on failure)
      flow_ratio <- if (total == 0) 0 else flow / total
      
      if (flow_ratio > 0.5) {
        wild_trigger_chance <- 4/6
      } else if (flow_ratio < 0.25) {
        wild_trigger_chance <- 2/6
      } else {
        wild_trigger_chance <- 3/6
      }
      wild_chance_total <- fail_chance * wild_trigger_chance
      
      list(
        dc = target_dc,
        success = success_chance,
        fail = fail_chance,
        wild = wild_chance_total,
        wild_minor = wild_chance_total * 0.6,
        wild_major = wild_chance_total * 0.3,
        wild_extreme = wild_chance_total * 0.1
      )
    })
    
    output$spell_outcome_ui <- renderUI({
      o <- spell_outcome()
      
      pct <- function(x) paste0(round(x * 100), "%")
      
      tags$div(
        class = "magic-grid",
        
        # Success
        tags$div(
          class = "magic-card",
          tags$div(class="k", "Success Chance"),
          tags$div(class="val", pct(o$success)),
          tags$div(class="hint", paste0("Beat DC ", o$dc))
        ),
        
        # Failure
        tags$div(
          class = "magic-card",
          tags$div(class="k", "Failure Chance"),
          tags$div(class="val", pct(o$fail))
        ),
        
        # Wild Magic
        tags$div(
          class = "magic-card",
          tags$div(class="k", "Wild Magic Risk"),
          tags$div(class="val", pct(o$wild)),
          tags$div(class="hint", "Only triggers on failure")
        ),
        
        # Severity
        tags$div(
          class = "magic-card",
          tags$div(class="k", "Severity"),
          tags$div(class="hint",
                   paste0(
                     "Minor ", pct(o$wild_minor), " | ",
                     "Major ", pct(o$wild_major), " | ",
                     "Extreme ", pct(o$wild_extreme)
                   )
          )
        )
      )
    })
    
    #Sorcerer identity juicy juicy
    
    magic_identity <- reactive({
      x <- validate_character(state$char)
      s <- core_sindre()
      
      race <- x$meta$race %||% ""
      class <- x$build$class %||% ""
      subclasses <- character_subclass_names(x)
      subclass <- paste(subclasses, collapse = " ")
      
      total  <- as.integer(s$total %||% 0)
      flow   <- as.integer(s$flow  %||% 0)
      regen  <- as.integer(s$regen %||% 0)
      locked <- as.integer(s$locked %||% 0)
      bound  <- as.integer(s$bound %||% 0)
      
      desc <- c()
      
      # =====================
      # 🌊 NATURE (mechanics)
      # =====================
      if (total <= 0) total <- 1
      if (flow > total * 0.5) {
        desc <- c(desc, "Your magic surges violently, difficult to restrain.")
      } else if (flow < total * 0.25) {
        desc <- c(desc, "Your magic moves slowly, shaped with deliberate control.")
      }
      
      if (total > 70) {
        desc <- c(desc, "A deep well of Sindre rests within you.")
      } else if (total < 30) {
        desc <- c(desc, "Your reserves are thin, forcing careful casting.")
      }
      
      if (regen == 0) {
        desc <- c(desc, "Your power does not return naturally — it must be taken.")
      }
      
      
      if (locked > 0) {
        desc <- c(desc, "Part of your power lies sealed, beyond your reach.")
      }
      if (bound > 0) {
        desc <- c(desc, "Your magic is bound to deeper forces, not entirely your own.")
      }
      
      # =====================
      # 🌿 RACE (origin)
      # =====================
      if (grepl("Tylwyth", race, ignore.case = TRUE)) {
        desc <- c(desc, "You are tied to Annwn, your magic echoing an older world.")
      }
      
      if (grepl("Na'Haran", race)) {
        desc <- c(desc, "Your magic is hardened by desert survival and ancient houses.")
      }
      
      if (grepl("Isildur", race)) {
        desc <- c(desc, "Your power carries the weight of a drowned past.")
      }
      
      if (grepl("Rhodrian", race)) {
        desc <- c(desc, "Your magic reflects a structured and divided lineage.")
      }
      
      # =====================
      # 🔥 CLASS (source)
      # =====================
      if (grepl("Hanianol", class)) {
        desc <- c(desc, "Your magic feeds on blood and the living world.")
      }
      
      if (grepl("Na'Haran Sorcerer", class)) {
        desc <- c(desc, "Your power is volatile, yet shaped by discipline.")
      }
      
      # =====================
      # ⚡ SUBCLASS (expression)
      # =====================
      if (grepl("Ancestor", subclass)) {
        desc <- c(desc, "You walk in step with ancestral currents of the Mandred.")
      }
      
      if (grepl("Heart Eater", subclass)) {
        desc <- c(desc, "You consume life to grow stronger — a predator of magic.")
      }
      
      if (grepl("Prophet", subclass)) {
        desc <- c(desc, "You glimpse threads of fate before they unfold.")
      }
      
      if (grepl("Warrior", subclass)) {
        desc <- c(desc, "You blend blade and spell into a single art.")
      }
      
      # =====================
      # 🏷️ TITLE GENERATION
      # =====================
      ## =====================
      # 🏷️ ADVANCED TITLE SYSTEM
      # =====================
      
      # ---------------------
      # CORE (flow / total)
      # ---------------------
      identity <- magical_identity_labels(total, flow, regen, locked, bound, subclasses)
      core <- paste(identity$core, identity$expression)
      
      # ---------------------
      # ASPECT (subclass)
      # ---------------------
      aspect <- identity$aspect
      
      # ---------------------
      # MODIFIER (state)
      # ---------------------
      modifier <- identity$modifier
      
      # ---------------------
      # FINAL TITLE BUILD
      # ---------------------
      title <- paste(core, aspect)
      
      if (!is.null(modifier)) {
        title <- paste(modifier, title)
      }
      
      list(title = title, desc = unique(desc))
    })
    
   
    
    output$sorcerer_identity_ui <- renderUI({
      id <- magic_identity()
      
      tagList(
        tags$div(
          style="font-weight:900; font-size:18px; margin-bottom:6px;",
          paste0("✨ ", id$title)
        ),
        tags$div(
          style="display:flex; flex-direction:column; gap:6px;",
          lapply(id$desc, function(d) {
            tags$div(
              style="padding:6px 10px; border-left:3px solid rgba(191,167,111,0.8);",
              d
            )
          })
        )
      )
    })
    
    output$spell_casting_ui <- renderUI({
      
      div(
        class = "card",
        h4("✨ Spell Casting"),
        
        # -------------------------
        # INTENT
        # -------------------------
        div(
          class = "magic-card",
          tags$div(class="k", "Intent"),
          
          fluidRow(
            column(
              6,
              selectInput(session$ns("spell_size"), "What are you affecting?",
                          choices = rownames(spell_cost_matrix))
            ),
            column(
              6,
              selectInput(session$ns("spell_move"), "What are you trying to do?",
                          choices = colnames(spell_cost_matrix))
            )
          )
        ),
        
        # -------------------------
        # POWER
        # -------------------------
        div(
          class = "magic-card",
          tags$div(class="k", "Power Draw"),
          
          div(
            class = "pill-row",
            
            uiOutput(session$ns("spell_cost_ui")),
            
            div(
              class = "pill",
              checkboxInput(session$ns("half_cost"),
                            "Channel (Half Cost)", value = FALSE)
            )
          )
        ),
        
        # -------------------------
        # CONTROL
        # -------------------------
        div(
          class = "magic-card",
          tags$div(class="k", "Control"),
          
          selectInput(session$ns("spell_dc_level"),
                      "How controlled is the spell?",
                      choices = setNames(spell_dc_levels, spell_dc_level_names)),
          
          div(
            class = "pill-row",
            div(
              class = "pill",
              checkboxInput(session$ns("half_dc"),
                            "Ritual (Half DC)", value = FALSE)
            )
          ),
          
          tags$div(
            style="margin-top:6px;",
            textOutput(session$ns("computed_spell_dc"))
          )
        ),
        
        # -------------------------
        # OUTCOME
        # -------------------------
        div(
          class = "magic-card",
          tags$div(class="k", "Outcome"),
          uiOutput(session$ns("spell_outcome_ui"))
        ),
        
        # -------------------------
        # ACTION
        # -------------------------
        div(
          style="margin-top:12px; display:flex; justify-content:center;",
          actionButton(session$ns("cast_spell"),
                       "Cast Spell",
                       class="btn btn-primary btn-lg")
        )
      )
    })
    
    observeEvent(input$cast_spell, {
      if (isTRUE(restoring())) return()
      
      s <- core_sindre()
      x <- validate_character(state$char)
      
      cost <- spell_cost()
      
      cur <- as.integer((s$cur %||% 0) + (s$temp %||% 0))
      if (is.na(cur)) cur <- 0
      
      total <- as.integer(s$total %||% 0)
      flow  <- as.integer(s$flow  %||% 0)
      
      if (cost > cur) {
        log_safe("❌ Not enough Sindre to cast the spell.", toast = TRUE, flash = "red")
        return()
      }
      if (cost > flow) {
        log_safe(paste0("❌ Spell cost (", cost, ") exceeds max flow (", flow, ")."),
                 toast = TRUE, flash = "red")
        return()
      }
      
      bld_mod <- mod_calc(x$abilities$bld_str %||% 10)
      level_mod <- floor((x$build$level %||% 1) / 5)
      
      pool_ratio <- if (total == 0) 0 else (total - cur) / total
      sindre_mod <- round(pool_ratio * 3)
      
      control_mod <- bld_mod + level_mod + sindre_mod
      cost_pressure <- round((cost / max(1, flow)) * 4)
      
      base_dc <- base_dc_from_level(input$spell_dc_level %||% 1)
      if (isTRUE(input$half_dc)) base_dc <- ceiling(base_dc / 2)
      
      target_dc <- base_dc + cost_pressure - control_mod
      target_dc <- max(5, min(25, target_dc))
      
      player_roll <- sample(1:20, 1)
      success <- player_roll >= target_dc
      
      if (success) {
        new_cur <- cur - cost
        updateNumericInput(session, "current_sindre", value = new_cur) # observer syncs to core
        log_safe(
          paste0("✨ Spell Success! Rolled ", player_roll,
                 " vs DC ", target_dc,
                 ". Used ", cost, " Sindre → ", new_cur, " remaining."),
          toast = TRUE, flash = "gold"
        )
      } else {
        log_safe(
          paste0("❌ Spell Failed (Rolled ", player_roll, " vs DC ", target_dc, "). Checking Wild Magic..."),
          toast = TRUE, flash = "red"
        )
        
        wild_roll <- sample(1:6, 1)
        flow_ratio <- if (total == 0) 0 else flow / total
        
        wild_threshold <- 4
        
        # High flow = more chaos (harder to avoid wild magic)
        if (flow_ratio > 0.5) {
          wild_threshold <- 5
        } else if (flow_ratio < 0.25) {
          wild_threshold <- 3
        }
        
        if (wild_roll >= wild_threshold) {
          log_safe(
            paste0("🎲 Avoided Wild Magic! Rolled ", wild_roll,
                   " vs threshold ", wild_threshold, ". No Sindre lost."),
            toast = TRUE
          )
        } else {
          effect <- sample(c("Minor", "Major", "Extreme"), 1, prob = c(0.6, 0.3, 0.1))
          new_cur <- cur - cost
          updateNumericInput(session, "current_sindre", value = new_cur) # observer syncs to core
          log_safe(
            paste0("💥 Wild Magic! ", effect,
                   " (Rolled ", wild_roll, " < ", wild_threshold, "). ",
                   cost, " Sindre lost → ", new_cur, " remaining."),
            toast = TRUE, flash = "red"
          )
        }
      }
    }, ignoreInit = TRUE)
    
  })
}
