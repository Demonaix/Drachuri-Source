# server/skills_tab.R
library(shiny)

# ---------- UI ----------
skillsTabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Skills",
    fluidRow(
      column(
        12,
        h4("Abilities & Saving Throws"),
        uiOutput(ns("ability_ui")),
        tags$hr(),
        h4("Skills"),
        uiOutput(ns("skills_ui"))
      )
    )
  )
}

# ---------- Server ----------
skillsTabServer <- function(id, state, restoring, add_log, char_rev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- helpers ---
    prof_bonus <- reactive({
      lvl <- state$char$build$level %||% 1
      ceiling(lvl / 4) + 1
    })
    
    ability_mods <- reactive({
      ab <- state$char$abilities %||% list()
      list(
        str = mod_calc(ab$str %||% 10),
        dex = mod_calc(ab$dex %||% 10),
        con = mod_calc(ab$con %||% 10),
        int = mod_calc(ab$int %||% 10),
        bld_str = mod_calc(ab$bld_str %||% 10),
        cha = mod_calc(ab$cha %||% 10)
      )
    })
    
    skill_key <- function(skill_name) gsub(" ", "_", tolower(skill_name))
    
    # --- Abilities + save prof UI ---
    output$ability_ui <- renderUI({
      pb <- prof_bonus()
      mods <- ability_mods()
      
      rows <- lapply(SKILLS_ABILITIES, function(abbr) {
        label <- SKILLS_ABILITY_NAMES[[abbr]]
        score <- state$char$abilities[[abbr]] %||% 10
        mod <- mods[[abbr]] %||% 0
        
        save_prof <- isTRUE(state$char$prof$saves[[abbr]] %||% FALSE)
        save_mod <- mod + if (save_prof) pb else 0
        
        fluidRow(
          column(3, strong(label)),
          column(2, numericInput(ns(abbr), NULL, value = score, width = "100%")),
          column(2, span(if (mod >= 0) paste0("+", mod) else mod)),
          column(2, span(if (save_mod >= 0) paste0("+", save_mod) else save_mod)),
          column(3, checkboxInput(ns(paste0("save_prof_", abbr)), "Save prof", value = save_prof))
        )
      })
      
      tagList(rows)
    })
    
    # write ability scores into state
    lapply(SKILLS_ABILITIES, function(abbr) {
      observeEvent(input[[abbr]], {
        if (isTRUE(restoring())) return()
        state$char$abilities[[abbr]] <- input[[abbr]]
      }, ignoreInit = TRUE)
    })
    
    # write save prof into state
    lapply(SKILLS_ABILITIES, function(abbr) {
      id2 <- paste0("save_prof_", abbr)
      observeEvent(input[[id2]], {
        if (isTRUE(restoring())) return()
        state$char$prof$saves[[abbr]] <- isTRUE(input[[id2]])
      }, ignoreInit = TRUE)
    })
    
    # --- Skills UI ---
    output$skills_ui <- renderUI({
      pb <- prof_bonus()
      mods <- ability_mods()
      
      rows <- lapply(seq_len(nrow(SKILLS_LIST)), function(i) {
        skill <- SKILLS_LIST$Skill[i]
        ability <- SKILLS_LIST$Ability[i]
        key <- skill_key(skill)
        
        prof_level <- state$char$prof$skills[[key]] %||% "None"
        mult <- switch(prof_level, "None" = 0, "Proficient" = 1, "Expertise" = 2, 0)
        
        total_mod <- (mods[[ability]] %||% 0) + (pb * mult)
        mod_display <- if (total_mod >= 0) paste0("+", total_mod) else as.character(total_mod)
        
        prof_icon <- if (prof_level == "Expertise") "⬤⬤" else if (prof_level == "Proficient") "⬤" else "○"
        
        fluidRow(
          column(3, strong(skill)),
          column(1, span(prof_icon)),
          column(1, span(mod_display)),
          column(3,
                 selectInput(
                   ns(paste0("prof_", key)), NULL,
                   choices = c("None", "Proficient", "Expertise"),
                   selected = prof_level
                 )
          ),
          column(2, actionButton(ns(paste0("roll_", key)), "🎲 Roll", class = "btn-sm")),
          column(2, HTML(paste0("(", toupper(ability), ")")))
        )
      })
      
      do.call(tagList, rows)
    })
    
    # write skill prof selections into state
    lapply(seq_len(nrow(SKILLS_LIST)), function(i) {
      key <- skill_key(SKILLS_LIST$Skill[i])
      input_id <- paste0("prof_", key)
      
      observeEvent(input[[input_id]], {
        if (isTRUE(restoring())) return()
        state$char$prof$skills[[key]] <- input[[input_id]]
      }, ignoreInit = TRUE)
    })
    
    # roll handlers
    lapply(seq_len(nrow(SKILLS_LIST)), function(i) {
      skill <- SKILLS_LIST$Skill[i]
      ability <- SKILLS_LIST$Ability[i]
      key <- skill_key(skill)
      btn <- paste0("roll_", key)
      
      observeEvent(input[[btn]], {
        if (isTRUE(restoring())) return()
        
        pb <- prof_bonus()
        mods <- ability_mods()
        
        prof_level <- state$char$prof$skills[[key]] %||% "None"
        mult <- switch(prof_level, "None" = 0, "Proficient" = 1, "Expertise" = 2, 0)
        total_mod <- (mods[[ability]] %||% 0) + (pb * mult)
        
        roll <- sample(1:20, 1)
        total <- roll + total_mod
        
        add_log(paste0("🎲 Skill Roll - ", skill, ": Rolled ", roll, " + ", total_mod, " = ", total))
      }, ignoreInit = TRUE)
    })
    
    # --- Hydrate module inputs after character replace (upload/new) ---
    # Use a revision counter (char_rev) so this triggers reliably when you replace state$char.
    observeEvent(char_rev(), {
      restoring(TRUE)
      on.exit(restoring(FALSE), add = TRUE)
      
      x <- validate_character(state$char)
      
      # abilities + save prof
      for (abbr in SKILLS_ABILITIES) {
        updateNumericInput(session, abbr, value = x$abilities[[abbr]] %||% 10)
        updateCheckboxInput(session, paste0("save_prof_", abbr), value = isTRUE(x$prof$saves[[abbr]] %||% FALSE))
      }
      
      # skills selects
      for (i in seq_len(nrow(SKILLS_LIST))) {
        key <- skill_key(SKILLS_LIST$Skill[i])
        updateSelectInput(session, paste0("prof_", key), selected = x$prof$skills[[key]] %||% "None")
      }
    }, ignoreInit = TRUE)
    
  })
}
