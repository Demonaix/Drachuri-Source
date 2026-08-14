library(shiny)
library(DT)

controlNpcCreatorUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "control-card",
      div(class = "control-section-title", "🧌 NPC / Enemy Creator"),
      
      div(
        class = "control-card",
        h4("Create NPC Template"),
        
        fluidRow(
          column(4, textInput(ns("npc_name"), "Name", placeholder = "Bandit")),
          column(2, numericInput(ns("npc_hp"), "HP Max", value = 10, min = 1)),
          column(2, numericInput(ns("npc_ac"), "AC", value = 12, min = 1)),
          column(2, numericInput(ns("npc_speed"), "Speed", value = 30, min = 0)),
          column(2, textInput(ns("npc_tags"), "Tags", placeholder = "humanoid,bandit"))
        ),
        
        tags$hr(),
        h5("Attack Options"),
        uiOutput(ns("npc_attacks_ui")),
        actionButton(ns("add_attack_row"), "Add Attack", class = "btn btn-default"),
        
        tags$br(),
        tags$br(),
        actionButton(ns("create_npc"), "Create NPC Template", class = "btn btn-success")
      ),
      
      div(
        class = "control-card",
        h4("NPC Templates"),
        actionButton(ns("refresh_npcs"), "Refresh", class = "btn btn-default"),
        br(), br(),
        DTOutput(ns("npc_tbl")),
        br(),
        actionButton(ns("delete_npc"), "Delete Selected", class = "btn btn-danger")
      )
    )
  )
}

controlNpcCreatorServer <- function(id, ctrl = NULL, bump_refresh = NULL) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    # -----------------------------
    # Local helper functions
    # -----------------------------
    make_npc_id <- function(name) {
      clean <- tolower(gsub("[^a-zA-Z0-9]+", "_", name))
      clean <- gsub("^_|_$", "", clean)
      if (!nzchar(clean)) clean <- "npc"
      paste0("npc_", clean, "_", paste0(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))
    }
    
    create_npc_template <- function(name, hp_max, ac, movement_speed,
                                    attack_name, attack_bonus, damage_expr,
                                    damage_type, attacks_json = "", tags = "") {
      con <- get_db_connection()
      if (is.null(con)) return(NULL)
      on.exit(release_db_connection(con), add = TRUE)
      
      npc_id <- make_npc_id(name)
      
      ok <- tryCatch({
        DBI::dbExecute(
          con,
          "
          insert into npc_templates
            (npc_id, name, hp_max, ac, movement_speed,
             attack_name, attack_bonus, damage_expr, damage_type,
             attacks_json, tags, created_at, updated_at)
          values
            (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
          ",
          params = list(
            npc_id,
            as.character(name),
            as.integer(hp_max),
            as.integer(ac),
            as.integer(movement_speed),
            as.character(attack_name),
            as.integer(attack_bonus),
            as.character(damage_expr),
            as.character(damage_type),
            as.character(attacks_json %||% ""),
            as.character(tags %||% "")
          )
        )
        TRUE
      }, error = function(e) {
        message("create_npc_template failed: ", e$message)
        FALSE
      })
      
      if (isTRUE(ok)) npc_id else NULL
    }
    
    list_npc_templates <- function() {
      con <- get_db_connection()
      if (is.null(con)) return(data.frame())
      on.exit(release_db_connection(con), add = TRUE)
      
      tryCatch(
        DBI::dbGetQuery(
          con,
          "select * from npc_templates order by lower(name)"
        ),
        error = function(e) {
          message("list_npc_templates failed: ", e$message)
          data.frame()
        }
      )
    }
    
    delete_npc_template <- function(npc_id) {
      con <- get_db_connection()
      if (is.null(con)) return(FALSE)
      on.exit(release_db_connection(con), add = TRUE)
      
      tryCatch({
        DBI::dbExecute(
          con,
          "delete from npc_templates where npc_id = ?",
          params = list(as.character(npc_id))
        )
        TRUE
      }, error = function(e) {
        message("delete_npc_template failed: ", e$message)
        FALSE
      })
    }
    
    # -----------------------------
    # State
    # -----------------------------
    npc_templates_rv <- reactiveVal(data.frame())
    attack_row_ids <- reactiveVal(c(1L, 2L))
    attack_remove_obs_ids <- reactiveVal(integer())
    
    load_npc_templates <- function() {
      df <- list_npc_templates()
      if (!is.data.frame(df)) df <- data.frame()
      npc_templates_rv(df)
    }
    
    collect_attacks <- function() {
      ids <- attack_row_ids()
      
      attacks <- lapply(ids, function(i) {
        nm <- as.character(input[[paste0("attack_name_", i)]] %||% "")
        hit <- suppressWarnings(as.integer(input[[paste0("attack_hit_", i)]] %||% 0L))
        dmg <- as.character(input[[paste0("attack_dmg_", i)]] %||% "")
        typ <- as.character(input[[paste0("attack_type_", i)]] %||% "")
        
        if (!nzchar(nm) || !nzchar(dmg)) return(NULL)
        if (is.na(hit)) hit <- 0L
        
        list(
          name = nm,
          hit = hit,
          dmg = dmg,
          type = typ
        )
      })
      
      Filter(Negate(is.null), attacks)
    }
    
    # -----------------------------
    # Initial load / refresh
    # -----------------------------
    observeEvent(TRUE, {
      load_npc_templates()
    }, once = TRUE)
    
    observeEvent(input$refresh_npcs, {
      load_npc_templates()
    }, ignoreInit = TRUE)
    
    # -----------------------------
    # Attack rows
    # -----------------------------
    output$npc_attacks_ui <- renderUI({
      ids <- attack_row_ids()
      ns <- session$ns
      
      rows <- lapply(ids, function(i) {
        fluidRow(
          column(
            3,
            textInput(
              ns(paste0("attack_name_", i)),
              "Attack Name",
              value = if (i == 1) "Claw" else if (i == 2) "Bite" else ""
            )
          ),
          column(
            2,
            numericInput(
              ns(paste0("attack_hit_", i)),
              "Hit Bonus",
              value = if (i == 1) 4 else if (i == 2) 5 else 0,
              min = -10,
              max = 30
            )
          ),
          column(
            3,
            textInput(
              ns(paste0("attack_dmg_", i)),
              "Damage",
              value = if (i == 1) "1d6+2" else if (i == 2) "1d8+2" else "1d6"
            )
          ),
          column(
            3,
            selectInput(
              ns(paste0("attack_type_", i)),
              "Damage Type",
              choices = c(
                "slashing", "piercing", "bludgeoning",
                "fire", "cold", "lightning", "acid", "poison",
                "necrotic", "radiant", "psychic", "force", "thunder"
              ),
              selected = if (i == 1) "slashing" else if (i == 2) "piercing" else "slashing"
            )
          ),
          column(
            1,
            br(),
            actionButton(ns(paste0("remove_attack_", i)), "✕", class = "btn btn-danger btn-sm")
          )
        )
      })
      
      tagList(rows)
    })
    
    observeEvent(input$add_attack_row, {
      ids <- attack_row_ids()
      next_id <- if (length(ids)) max(ids) + 1L else 1L
      attack_row_ids(c(ids, next_id))
    }, ignoreInit = TRUE)
    
    observe({
      ids <- attack_row_ids()
      made <- attack_remove_obs_ids()
      
      new_ids <- setdiff(ids, made)
      if (!length(new_ids)) return()
      
      for (i in new_ids) {
        local({
          row_id <- i
          observeEvent(input[[paste0("remove_attack_", row_id)]], {
            ids_now <- attack_row_ids()
            if (length(ids_now) <= 1) return()
            attack_row_ids(setdiff(ids_now, row_id))
          }, ignoreInit = TRUE)
        })
      }
      
      attack_remove_obs_ids(unique(c(made, new_ids)))
    })
    
    # -----------------------------
    # Create NPC
    # -----------------------------
    observeEvent(input$create_npc, {
      nm <- as.character(input$npc_name %||% "")
      hp <- suppressWarnings(as.integer(input$npc_hp %||% 10L))
      ac <- suppressWarnings(as.integer(input$npc_ac %||% 12L))
      spd <- suppressWarnings(as.integer(input$npc_speed %||% 30L))
      tags <- as.character(input$npc_tags %||% "")
      
      if (!nzchar(nm)) {
        showNotification("Enter an NPC name.", type = "error")
        return()
      }
      
      if (is.na(hp) || hp < 1L) hp <- 10L
      if (is.na(ac) || ac < 1L) ac <- 12L
      if (is.na(spd) || spd < 0L) spd <- 30L
      
      attacks <- collect_attacks()
      if (!length(attacks)) {
        showNotification("Add at least one valid attack.", type = "error")
        return()
      }
      
      primary <- attacks[[1]]
      attacks_json <- jsonlite::toJSON(attacks, auto_unbox = TRUE)
      
      npc_id <- create_npc_template(
        name = nm,
        hp_max = hp,
        ac = ac,
        movement_speed = spd,
        attack_name = as.character(primary$name %||% "Attack"),
        attack_bonus = as.integer(primary$hit %||% 0L),
        damage_expr = as.character(primary$dmg %||% "1d4"),
        damage_type = as.character(primary$type %||% "bludgeoning"),
        attacks_json = attacks_json,
        tags = tags
      )
      
      if (is.null(npc_id) || !nzchar(npc_id)) {
        showNotification("Failed to create NPC template.", type = "error")
        return()
      }
      
      updateTextInput(session, "npc_name", value = "")
      updateTextInput(session, "npc_tags", value = "")
      load_npc_templates()
      
      if (is.function(bump_refresh)) bump_refresh()
      
      showNotification(paste0("Created NPC template: ", nm), type = "message")
    }, ignoreInit = TRUE)
    
    # -----------------------------
    # Table
    # -----------------------------
    output$npc_tbl <- renderDT({
      df <- npc_templates_rv()
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
      
      keep <- intersect(
        c("npc_id", "name", "hp_max", "ac", "movement_speed", "attack_name",
          "attack_bonus", "damage_expr", "damage_type", "tags", "updated_at"),
        names(df)
      )
      
      DT::datatable(
        df[, keep, drop = FALSE],
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 8,
          scrollX = TRUE
        )
      )
    })
    
    # -----------------------------
    # Delete selected NPC
    # -----------------------------
    observeEvent(input$delete_npc, {
      idx <- input$npc_tbl_rows_selected
      df <- npc_templates_rv()
      
      if (is.null(idx) || length(idx) == 0 || !is.data.frame(df) || nrow(df) == 0) {
        showNotification("Select an NPC template first.", type = "warning")
        return()
      }
      
      npc_id <- as.character(df$npc_id[idx] %||% "")
      nm <- as.character(df$name[idx] %||% "NPC")
      
      if (!nzchar(npc_id)) {
        showNotification("Selected NPC has no ID.", type = "error")
        return()
      }
      
      ok <- delete_npc_template(npc_id)
      
      if (!isTRUE(ok)) {
        showNotification("Failed to delete NPC template.", type = "error")
        return()
      }
      
      load_npc_templates()
      if (is.function(bump_refresh)) bump_refresh()
      
      showNotification(paste0("Deleted NPC template: ", nm), type = "message")
    }, ignoreInit = TRUE)
  })
}