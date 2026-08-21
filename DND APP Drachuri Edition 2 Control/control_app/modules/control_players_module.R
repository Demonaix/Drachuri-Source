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
            selectInput(ns("remove_character_id"), "Player to remove", choices = character()),
            uiOutput(ns("players_summary")),
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

    
    
    output$players_summary<-renderUI({df<-players_tbl();if(!is.data.frame(df)||!nrow(df))return(p(class="control-mini","No players in the active session."));tags$ul(lapply(seq_len(nrow(df)),function(i){nm<-as.character(df$display_name[[i]]%||%df$char_name[[i]]%||%df$character_id[[i]]);hp<-as.integer(df$current_hp[[i]]%||%0L);tags$li(strong(nm),paste0(" — ",hp," HP",if("is_active"%in%names(df)&&!isTRUE(df$is_active[[i]]))" · inactive"else""))}))})
    observe({df<-players_tbl();if(!is.data.frame(df)||!nrow(df))return(updateSelectInput(session,"remove_character_id",choices=character()));ids<-as.character(df$character_id);labels<-as.character(df$display_name%||%df$char_name%||%ids);updateSelectInput(session,"remove_character_id",choices=setNames(ids,labels),selected=isolate(input$remove_character_id%||%ids[[1L]]))})
    

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
      df <- players_tbl()
      cid<-as.character(input$remove_character_id%||%"")
      if(!nzchar(cid))return()
      
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
