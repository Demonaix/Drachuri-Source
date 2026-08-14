controlPlayersUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "card",
        h3("👥 Players"),
        
      
        
        # -------------------------
        # Add player
        # -------------------------
        div(class = "card",
            h4("Add Player"),
            
            selectInput(ns("character_id"), "Character", choices = NULL),
            actionButton(ns("refresh_characters"), "Refresh Characters", class = "btn btn-default"),
            
            fluidRow(
              column(6,
                     textInput(ns("display_name"), "Display Name (optional)")
              ),
              column(6,
                     br(),
                     actionButton(ns("add_player"), "Add to Session", class = "btn btn-primary")
              )
            )
        ),
        
        # -------------------------
        # Current players
        # -------------------------
        div(class = "card",
            h4("Players in Session"),
            DT::DTOutput(ns("players_tbl")),
            br(),
            actionButton(ns("remove_player"), "Remove Selected", class = "btn btn-danger")
        )
    )
  )
}

controlPlayersServer <- function(id, ctrl, session_tbl, players_tbl, positions_tbl, bump_refresh) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    `%||%` <- get("%||%", inherits = TRUE)
    
    # --------------------------------
    # Load sessions
    # --------------------------------
    characters_rv <- reactiveVal(data.frame())
    
    load_characters <- function() {
      chars <- tryCatch(
        list_characters_in_db(),
        error = function(e) {
          message("list_characters_in_db failed: ", e$message)
          data.frame()
        }
      )
      
      if (!is.data.frame(chars)) chars <- data.frame()
      characters_rv(chars)
    }
    
    observeEvent(TRUE, {
      load_characters()
    }, once = TRUE)
    
    observe({
      chars <- characters_rv()
      
      if (!is.data.frame(chars) || nrow(chars) == 0) {
        updateSelectInput(session, "character_id", choices = c("No characters found" = ""))
        return()
      }
      
      if (!all(c("character_id", "name") %in% names(chars))) {
        warning("⚠️ list_characters_in_db missing required columns")
        updateSelectInput(session, "character_id", choices = c("Invalid character data" = ""))
        return()
      }
      
      ids <- as.character(chars$character_id)
      labels <- paste0(chars$name, " (", substr(ids, 1, 6), ")")
      
      updateSelectInput(session, "character_id", choices = stats::setNames(ids, labels))
    })

    
    
    # --------------------------------
    # Show players
    # --------------------------------
    library(DT)
    output$players_tbl <- DT::renderDT({
      df <- players_tbl()
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
      
      DT::datatable(
        df,
        selection = "single",
        options = list(pageLength = 5),
        rownames = FALSE
      )
    })
    

    observeEvent(input$refresh_characters, {
      load_characters()
    })
    # --------------------------------
    # Add player
    # --------------------------------
    observeEvent(input$add_player, {
      sid <- ctrl$session_id
      cid <- as.character(input$character_id %||% "")
      
      if (is.null(sid) || is.na(sid) || sid < 1 || !nzchar(cid)) return()
      
      ok <- add_character_to_session(
        session_id = sid,
        character_id = cid,
        display_name = input$display_name
      )
      
      if (ok) {
        showNotification("✅ Player added to session", type = "message")
        bump_refresh()
      } else {
        showNotification("❌ Failed to add player", type = "error")
      }
    })
    
    # --------------------------------
    # Remove player
    # --------------------------------
    observeEvent(input$remove_player, {
      idx <- input$players_tbl_rows_selected
      df <- players_tbl()
      
      if (is.null(idx) || !nrow(df)) return()
      
      cid <- df$character_id[idx]
      
      ok <- remove_character_from_session(
        session_id = ctrl$session_id,
        character_id = cid
      )
      
      if (ok) {
        showNotification("🗑️ Player removed", type = "message")
        bump_refresh()
      }
    })
    

  
    
  })
}