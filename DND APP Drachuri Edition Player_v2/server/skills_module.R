# server/skills_module.R
library(shiny)

skillsTabUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Skills",
    h4("Abilities & Saving Throws"),
    uiOutput(ns("stats_table")),
    tags$hr(),
    h4("🧭 Skill Identity"),
    uiOutput(ns("skill_identity_ui"))
  )
}

skillsTabServer <- function(id, state, restoring, add_log, char_rev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =============================
    # Constants
    # =============================
    ABILITIES <- c("str","dex","con","int","bld_str","cha")
    
    LABELS <- c(
      str="Strength",
      dex="Dexterity",
      con="Constitution",
      int="Intelligence",
      bld_str="Blood Strength",
      cha="Charisma"
    )
    
    ability_mod <- function(score) mod_calc(score %||% 10)
    
    skill_key <- function(skill_name) {
      gsub(" ", "_", tolower(skill_name))
    }
    
    # =============================
    # Core Reactives
    # =============================
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
    
    # =============================
    # UI
    # =============================
    output$stats_table <- renderUI({
      pb <- prof_bonus()
      mods <- ability_mods()
      
      ability_blocks <- lapply(ABILITIES, function(ab) {
        
        score <- state$char$abilities[[ab]] %||% 10
        mod <- ability_mod(score)
        
        save_prof <- isTRUE(state$char$prof$saves[[ab]] %||% FALSE)
        save_bonus <- mod + if (save_prof) pb else 0
        
        # ---- Skills under ability ----
        skill_rows <- lapply(seq_len(nrow(SKILLS_LIST)), function(i) {
          if (SKILLS_LIST$Ability[i] != ab) return(NULL)
          
          skill <- SKILLS_LIST$Skill[i]
          key <- skill_key(skill)
          
          prof_level <- state$char$prof$skills[[key]] %||% "None"
          mult <- switch(prof_level,
                         "None" = 0,
                         "Proficient" = 1,
                         "Expertise" = 2,
                         0)
          
          total_mod <- (mods[[ab]] %||% 0) + (pb * mult)
          
          prof_icon <- if (prof_level == "Expertise") "⬤⬤"
          else if (prof_level == "Proficient") "⬤"
          else "○"
          
          fluidRow(
            column(4, skill_with_tooltip(skill)),
            column(2, strong(ifelse(total_mod >= 0, paste0("+", total_mod), total_mod))),
            column(2,
                   actionButton(ns(paste0("prof_toggle_", key)),
                                prof_icon,
                                class = "btn btn-xs")
            ),
            column(4,
                   div(style="display:flex; gap:4px;",
                       actionButton(ns(paste0("roll_dis_", key)), "⬇", class="btn btn-xs btn-danger"),
                       actionButton(ns(paste0("roll_", key)), "🎲", class="btn btn-xs"),
                       actionButton(ns(paste0("roll_adv_", key)), "⬆", class="btn btn-xs btn-success")
                   )
            )
          )
        })
        
        skill_rows <- Filter(Negate(is.null), skill_rows)
        
        # ---- Ability block ----
        div(
          class = "magic-card",
          style="margin-bottom:12px;",
          
          h4(LABELS[[ab]]),
          
          # ✅ NEW HEADER ROW
          fluidRow(
            column(2, strong("Score")),
            column(2, strong("Mod")),
            column(2, strong("Save")),
            column(2, strong("Prof")),
            column(4, strong("Roll"))
          ),
          
          fluidRow(
            column(2, numericInput(ns(ab), NULL, score, min=1, max=30)),
            column(2, strong(ifelse(mod>=0,paste0("+",mod),mod))),
            column(2, strong(ifelse(save_bonus>=0,paste0("+",save_bonus),save_bonus))),
            column(2, checkboxInput(ns(paste0("save_prof_", ab)), NULL, value=save_prof)),
            column(4,
                   div(style="display:flex; gap:4px;",
                       actionButton(ns(paste0("save_dis_", ab)), "⬇", class="btn btn-xs btn-danger"),
                       actionButton(ns(paste0("save_", ab)), "🎲", class="btn btn-xs"),
                       actionButton(ns(paste0("save_adv_", ab)), "⬆", class="btn btn-xs btn-success")
                   )
            )
          ),
          
          tags$hr(),
          
          # ✅ SKILL HEADER
          fluidRow(
            column(4, strong("Skill")),
            column(2, strong("Mod")),
            column(2, strong("Prof")),
            column(4, strong("Roll"))
          ),
          
          tagList(skill_rows)
        )
      })
      
      tagList(ability_blocks)
    })
    
    # =============================
    # Dice
    # =============================
    roll_d20 <- function(mode) {
      r1 <- sample(1:20,1)
      r2 <- sample(1:20,1)
      
      if (mode=="Normal") return(list(val=r1, txt=paste0("d20(",r1,")")))
      if (mode=="Adv") return(list(val=max(r1,r2), txt=paste0("Adv(",r1,",",r2,") → ",max(r1,r2))))
      if (mode=="Disadv") return(list(val=min(r1,r2), txt=paste0("Disadv(",r1,",",r2,") → ",min(r1,r2))))
    }
    
    # =============================
    # Rolls
    # =============================
    roll_save <- function(ab, mode) {
      pb <- prof_bonus()
      score <- state$char$abilities[[ab]] %||% 10
      mod <- ability_mod(score)
      
      prof <- isTRUE(state$char$prof$saves[[ab]] %||% FALSE)
      total_mod <- mod + if (prof) pb else 0
      
      rr <- roll_d20(mode)
      total <- rr$val + total_mod
      
      add_log(paste0("🛡 Save - ", LABELS[[ab]], ": ", rr$txt, " + ", total_mod, " = ", total))
    }
    
    # =============================
    # Skill Identity
    # =============================
    skill_identity <- reactive({
      pb <- prof_bonus()
      mods <- ability_mods()
      
      scores <- list()
      
      for (i in seq_len(nrow(SKILLS_LIST))) {
        skill <- SKILLS_LIST$Skill[i]
        ability <- SKILLS_LIST$Ability[i]
        key <- skill_key(skill)
        
        prof_level <- state$char$prof$skills[[key]] %||% "None"
        mult <- switch(prof_level, "None"=0,"Proficient"=1,"Expertise"=2,0)
        
        scores[[skill]] <- (mods[[ability]] %||% 0) + (pb * mult)
      }
      
      sorted <- sort(unlist(scores), decreasing = TRUE)
      top <- names(sorted)[1:3]
      
      s1 <- tolower(top[1] %||% "")
      s2 <- tolower(top[2] %||% "")
      
      desc <- c()
      
      # =====================
      # CORE
      # =====================
      core <- "Wanderer"
      
      if (grepl("stealth", s1)) core <- "Shadow"
      else if (grepl("survival", s1)) core <- "Stalker"
      else if (grepl("arcana", s1)) core <- "Arcanist"
      else if (grepl("athletics", s1)) core <- "Brute"
      else if (grepl("perception", s1)) core <- "Watcher"
      else if (grepl("deception", s1)) core <- "Trickster"
      else if (grepl("persuasion", s1)) core <- "Orator"
      
      # =====================
      # ASPECT
      # =====================
      aspect <- "Operative"
      
      if (grepl("stealth", s2)) aspect <- "Ghost"
      else if (grepl("survival", s2)) aspect <- "Hunter"
      else if (grepl("arcana", s2)) aspect <- "Seer"
      else if (grepl("athletics", s2)) aspect <- "Enforcer"
      else if (grepl("perception", s2)) aspect <- "Observer"
      else if (grepl("deception", s2)) aspect <- "Liar"
      else if (grepl("persuasion", s2)) aspect <- "Diplomat"
      
      # =====================
      # MODIFIER
      # =====================
      modifier <- NULL
      
      avg <- mean(sorted[1:min(5, length(sorted))])
      
      if (sorted[1] > avg + 3) modifier <- "Elite"
      else if (sorted[1] > avg + 1) modifier <- "Cunning"
      else if (sorted[1] < 1) modifier <- "Unproven"
      
      # =====================
      # TITLE
      # =====================
      title <- paste(core, aspect)
      if (!is.null(modifier)) title <- paste(modifier, title)
      
      # =====================
      # DESCRIPTIONS
      # =====================
      
      if (grepl("stealth", s1)) {
        desc <- c(desc, "You operate best unseen, striking from obscurity.")
      }
      if (grepl("perception", s1) || grepl("perception", s2)) {
        desc <- c(desc, "Your awareness borders on instinct.")
      }
      if (grepl("survival", s1)) {
        desc <- c(desc, "The wilderness is your ally.")
      }
      if (grepl("arcana", s1)) {
        desc <- c(desc, "You read the threads of magic with practiced insight.")
      }
      if (grepl("deception", s1) || grepl("deception", s2)) {
        desc <- c(desc, "Truth is a tool, not a rule.")
      }
      
      if (length(desc) == 0) desc <- "Your talents are still taking shape."
      
      list(
        title = title,
        desc = unique(desc),
        top_skills = top
      )
    })
    
    output$skill_identity_ui <- renderUI({
      id <- skill_identity()
      
      tagList(
        tags$div(style="font-weight:900; font-size:18px;",
                 paste0("✨ ", id$title)),
        
        tags$div(style="opacity:.7; font-size:12px;",
                 paste("Top Skills:", paste(id$top_skills, collapse=", ")))
      )
    })
    
    # =============================
    # Observers
    # =============================
    
    # Ability write-back
    lapply(ABILITIES, function(ab) {
      observeEvent(input[[ab]], {
        if (isTRUE(restoring())) return()
        state$char$abilities[[ab]] <- input[[ab]]
      }, ignoreInit = TRUE)
    })
    
    # Save proficiency write-back
    lapply(ABILITIES, function(ab) {
      observeEvent(input[[paste0("save_prof_", ab)]], {
        if (isTRUE(restoring())) return()
        state$char$prof$saves[[ab]] <- isTRUE(input[[paste0("save_prof_", ab)]])
      }, ignoreInit = TRUE)
    })
    
    # Save rolls
    lapply(ABILITIES, function(ab) {
      observeEvent(input[[paste0("save_", ab)]], {
        if (isTRUE(restoring())) return()
        roll_save(ab, "Normal")
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("save_adv_", ab)]], {
        if (isTRUE(restoring())) return()
        roll_save(ab, "Adv")
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("save_dis_", ab)]], {
        if (isTRUE(restoring())) return()
        roll_save(ab, "Disadv")
      }, ignoreInit = TRUE)
    })
    
    # Skill proficiency toggle
    lapply(seq_len(nrow(SKILLS_LIST)), function(i) {
      key <- skill_key(SKILLS_LIST$Skill[i])
      
      observeEvent(input[[paste0("prof_toggle_", key)]], {
        if (isTRUE(restoring())) return()
        
        current <- state$char$prof$skills[[key]] %||% "None"
        
        next_val <- switch(current,
                           "None"="Proficient",
                           "Proficient"="Expertise",
                           "Expertise"="None")
        
        state$char$prof$skills[[key]] <- next_val
      }, ignoreInit = TRUE)
    })
    
    #Tooltip helper
    skill_with_tooltip <- function(skill) {
      desc <- SKILL_DESC[[skill]] %||% ""
      
      tags$span(
        title = desc,   # 👈 native browser tooltip
        style = "cursor: help; border-bottom:1px dotted rgba(255,255,255,0.3);",
        skill
      )
    }
    
    # Skill rolls
    lapply(seq_len(nrow(SKILLS_LIST)), function(i) {
      skill <- SKILLS_LIST$Skill[i]
      ability <- SKILLS_LIST$Ability[i]
      key <- skill_key(skill)
      
      make_roll <- function(mode) {
        pb <- prof_bonus()
        mods <- ability_mods()
        
        prof_level <- state$char$prof$skills[[key]] %||% "None"
        mult <- switch(prof_level, "None"=0,"Proficient"=1,"Expertise"=2,0)
        
        total_mod <- (mods[[ability]] %||% 0) + (pb * mult)
        
        rr <- roll_d20(mode)
        total <- rr$val + total_mod
        
        add_log(paste0("🎲 Skill - ", skill, ": ", rr$txt, " + ", total_mod, " = ", total))
      }
      
      observeEvent(input[[paste0("roll_", key)]], {
        if (isTRUE(restoring())) return()
        make_roll("Normal")
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("roll_adv_", key)]], {
        if (isTRUE(restoring())) return()
        make_roll("Adv")
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("roll_dis_", key)]], {
        if (isTRUE(restoring())) return()
        make_roll("Disadv")
      }, ignoreInit = TRUE)
    })
    
    # Rehydrate
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