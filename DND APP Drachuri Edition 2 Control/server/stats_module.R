# server/stats_module.R
library(shiny)

statsTabUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Stats",
    h4("Ability Scores & Saving Throws"),
    fluidRow(
      column(3, strong("Ability")),
      column(2, strong("Score")),
      column(2, strong("Modifier")),
      column(2, strong("Save Bonus")),
      column(2, strong("Proficient")),
      column(1, strong("Roll"))
    ),
    uiOutput(ns("stats_table")),
    tags$hr(),
    fluidRow(
      column(4, h5("Proficiency Bonus"), textOutput(ns("prof_bonus"))),
      column(4, h5("Passive Perception"), textOutput(ns("passive_perception")))
    )
  )
}

statsTabServer <- function(id, state, restoring, add_log, char_rev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ABILITIES <- c("str","dex","con","int","bld_str","cha")
    LABELS <- c(
      str="Strength", dex="Dexterity", con="Constitution",
      int="Intelligence", bld_str="Blood Strength", cha="Charisma"
    )
    
    prof_bonus <- reactive({
      lvl <- state$char$build$level %||% 1
      ceiling(lvl / 4) + 1
    })
    
    ability_mod <- function(score) mod_calc(score %||% 10)
    
    # --- UI ---
    output$stats_table <- renderUI({
      pb <- prof_bonus()
      
      rows <- lapply(ABILITIES, function(ab) {
        score <- state$char$abilities[[ab]] %||% 10
        mod <- ability_mod(score)
        
        save_prof <- isTRUE(state$char$prof$saves[[ab]] %||% FALSE)
        save_bonus <- mod + if (save_prof) pb else 0
        
        fluidRow(
          column(3, strong(LABELS[[ab]])),
          column(2, numericInput(ns(ab), NULL, score, min = 1, max = 30)),
          column(2, span(if (mod >= 0) paste0("+", mod) else mod)),
          column(2, span(if (save_bonus >= 0) paste0("+", save_bonus) else save_bonus)),
          column(2, checkboxInput(ns(paste0("save_prof_", ab)), NULL, value = save_prof)),
          column(1, actionButton(ns(paste0("roll_", ab)), "🎲", class = "btn-sm"))
        )
      })
      
      tagList(rows)
    })
    
    # --- Write ability scores ---
    lapply(ABILITIES, function(ab) {
      observeEvent(input[[ab]], {
        if (restoring()) return()
        state$char$abilities[[ab]] <- input[[ab]]
      }, ignoreInit = TRUE)
    })
    
    # --- Write save prof ---
    lapply(ABILITIES, function(ab) {
      id2 <- paste0("save_prof_", ab)
      observeEvent(input[[id2]], {
        if (restoring()) return()
        state$char$prof$saves[[ab]] <- isTRUE(input[[id2]])
      }, ignoreInit = TRUE)
    })
    
    # --- Roll saving throws ---
    lapply(ABILITIES, function(ab) {
      btn <- paste0("roll_", ab)
      
      observeEvent(input[[btn]], {
        if (restoring()) return()
        
        score <- state$char$abilities[[ab]] %||% 10
        mod <- ability_mod(score)
        pb <- prof_bonus()
        
        prof <- isTRUE(state$char$prof$saves[[ab]] %||% FALSE)
        total_mod <- mod + if (prof) pb else 0
        
        mods <- get_status_modifiers(state$char)
        
        # Auto-fail STR / DEX
        if (mods$auto_fail_str_dex && ab %in% c("str", "dex")) {
          add_log(
            paste0(
              "💀 Saving Throw - ", LABELS[[ab]],
              ": AUTO FAIL (", paste(state$char$status$effects, collapse=", "), ")"
            )
          )
          return()
        }
        
        mode <- if (mods$save_disadv) "Disadv" else "Normal"
        
        roll_d20_adv <- function(mode) {
          r1 <- sample(1:20, 1)
          r2 <- sample(1:20, 1)
          
          if (mode == "Normal") return(list(val = r1, txt = paste0("d20(", r1, ")")))
          if (mode == "Disadv") return(list(val = min(r1, r2), txt = paste0("Disadv(", r1, ",", r2, ") → ", min(r1, r2))))
        }
        
        rr <- roll_d20_adv(mode)
        
        total <- rr$val + total_mod
        
        print(get_status_modifiers(state$char))
        
        label <- if (mode == "Disadv") "⚠️ Disadv " else ""
        
        add_log(
          paste0(
            "🎲 ", label, "Saving Throw - ", LABELS[[ab]],
            ": ",
            rr$txt,
            " + ", total_mod,
            " = ", total
          )
        )
     
      }, ignoreInit = TRUE)
    })
    
    # --- Derived displays ---
    output$prof_bonus <- renderText(prof_bonus())
    
    output$passive_perception <- renderText({
      pb <- prof_bonus()
      mod <- ability_mod(state$char$abilities$bld_str %||% 10)
      prof <- state$char$prof$skills$perception %||% "None"
      
      bonus <- if (prof == "Proficient") pb else if (prof == "Expertise") pb * 2 else 0
      10 + mod + bonus
    })
    
    # --- Rehydrate on character replace ---
    observeEvent(char_rev(), {
      restoring(TRUE)
      on.exit(restoring(FALSE), add = TRUE)
      
      x <- validate_character(state$char)
      
      for (ab in ABILITIES) {
        updateNumericInput(session, ab, value = x$abilities[[ab]] %||% 10)
        updateCheckboxInput(session, paste0("save_prof_", ab),
                            value = isTRUE(x$prof$saves[[ab]] %||% FALSE))
      }
    }, ignoreInit = TRUE)
    
  })
}