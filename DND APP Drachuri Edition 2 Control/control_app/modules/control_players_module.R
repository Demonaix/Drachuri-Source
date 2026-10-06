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
        ),

        div(class = "card",
            h4("Player Resources & Status"),
            selectInput(ns("manage_character_id"), "Player", choices = character()),
            uiOutput(ns("managed_player_summary")),
            div(style="display:flex;gap:8px;flex-wrap:wrap;margin:10px 0 16px",
                actionButton(ns("refill_hp"), "Refill HP", class = "btn btn-success"),
                actionButton(ns("refill_sindre"), "Refill Sindre", class = "btn btn-primary"),
                actionButton(ns("refill_both"), "Refill Both", class = "btn btn-warning")
            ),
            fluidRow(
              column(6,selectInput(ns("status_add"),"Add status",choices=character())),
              column(6,selectInput(ns("status_remove"),"Remove active status",choices=character()))
            ),
            div(style="display:flex;gap:8px;flex-wrap:wrap",
                actionButton(ns("add_status"),"Add Status",class="btn btn-primary"),
                actionButton(ns("remove_status"),"Remove Status",class="btn btn-default")
            )
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
    remove_player_choice_signature <- reactiveVal(NULL)
    manage_player_choice_signature <- reactiveVal(NULL)
    manage_rev <- reactiveVal(0L)
    
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
    observe({
      df <- players_tbl()
      if (!is.data.frame(df) || !nrow(df)) {
        if (!identical(remove_player_choice_signature(), "")) {
          remove_player_choice_signature("")
          updateSelectInput(session, "remove_character_id", choices = character())
        }
        return()
      }
      ids <- as.character(df$character_id)
      labels <- as.character(df$display_name %||% df$char_name %||% ids)
      signature <- paste(ids, labels, collapse = "|")
      if (identical(signature, remove_player_choice_signature())) return()
      remove_player_choice_signature(signature)
      selected <- as.character(isolate(input$remove_character_id) %||% ids[[1L]])
      if (!selected %in% ids) selected <- ids[[1L]]
      updateSelectInput(session, "remove_character_id", choices = setNames(ids, labels), selected = selected)

      if (!identical(signature, manage_player_choice_signature())) {
        manage_player_choice_signature(signature)
        managed <- as.character(isolate(input$manage_character_id) %||% ids[[1L]])
        if (!managed %in% ids) managed <- ids[[1L]]
        updateSelectInput(session, "manage_character_id", choices = setNames(ids, labels), selected = managed)
      }
    })

    condition_choices <- function() {
      defs <- tryCatch(status_condition_definitions(), error=function(e) list())
      keys <- names(defs)
      stats::setNames(keys, tools::toTitleCase(keys))
    }
    observeEvent(TRUE, {
      choices <- condition_choices()
      updateSelectInput(session,"status_add",choices=choices,selected=if(length(choices))unname(choices[[1L]])else character())
    }, once=TRUE)

    managed_character <- reactive({
      manage_rev()
      cid <- as.character(input$manage_character_id %||% "")
      if(!nzchar(cid)) return(NULL)
      tryCatch(validate_character(load_character_from_db(cid)), error=function(e) NULL)
    })

    active_conditions <- reactive({
      ch <- managed_character()
      if(is.null(ch)) return(character())
      values <- unique(tolower(trimws(as.character(c(ch$status$conditions %||% character(),ch$status$effects %||% character())))))
      intersect(values,names(tryCatch(status_condition_definitions(),error=function(e)list())))
    })

    observe({
      values <- active_conditions()
      values <- values[nzchar(values)]
      updateSelectInput(session,"status_remove",choices=stats::setNames(values,tools::toTitleCase(values)),selected=if(length(values))values[[1L]]else character())
    })

    output$managed_player_summary <- renderUI({
      ch <- managed_character()
      if(is.null(ch)) return(p(class="control-mini","Choose a player."))
      hp <- ch$resources$hp %||% list(); si <- ch$resources$sindre %||% list(); conditions <- active_conditions()
      div(class="control-kpi-row",
          span(class="control-kpi",paste0("HP ",hp$cur%||%0," / ",hp$max%||%0)),
          span(class="control-kpi",paste0("Sindre ",si$cur%||%0," / ",si$total%||%0)),
          span(class="control-kpi",if(length(conditions))paste(tools::toTitleCase(conditions),collapse=", ")else"No active statuses"))
    })

    save_managed_character <- function(ch, message) {
      cid <- as.character(input$manage_character_id %||% "")
      if(!nzchar(cid) || is.null(ch) || is.null(save_character_to_db(validate_character(ch),cid))) {
        showNotification("The player could not be updated.",type="error",duration=8)
        return(FALSE)
      }
      manage_rev(manage_rev()+1L); bump_refresh(); showNotification(message,type="message")
      TRUE
    }

    refill_player <- function(hp=FALSE,sindre=FALSE) {
      ch <- managed_character(); cid <- as.character(input$manage_character_id%||%""); sid <- suppressWarnings(as.integer(ctrl$session_id%||%NA))
      if(is.null(ch)||!nzchar(cid)||is.na(sid)) return()
      if(isTRUE(hp)) {
        ch$resources$hp$cur <- as.integer(ch$resources$hp$max%||%0L)
        ch$resources$hp$temp <- 0L
      }
      if(isTRUE(sindre)) ch$resources$sindre$cur <- as.integer(ch$resources$sindre$total%||%0L)
      if(!save_managed_character(ch,if(hp&&sindre)"HP and Sindre refilled."else if(hp)"HP refilled."else"Sindre refilled.")) return()
      if(isTRUE(hp)) set_session_hp(sid,cid,ch$resources$hp$cur,0L)
      bump_refresh()
    }
    observeEvent(input$refill_hp,refill_player(hp=TRUE),ignoreInit=TRUE)
    observeEvent(input$refill_sindre,refill_player(sindre=TRUE),ignoreInit=TRUE)
    observeEvent(input$refill_both,refill_player(hp=TRUE,sindre=TRUE),ignoreInit=TRUE)

    observeEvent(input$add_status,{
      ch<-managed_character(); key<-tolower(as.character(input$status_add%||%""));if(is.null(ch)||!nzchar(key))return()
      ch$status$conditions<-unique(c(as.character(ch$status$conditions%||%character()),key))
      save_managed_character(ch,paste(tools::toTitleCase(key),"added."))
    },ignoreInit=TRUE)
    observeEvent(input$remove_status,{
      ch<-managed_character(); key<-tolower(as.character(input$status_remove%||%""));if(is.null(ch)||!nzchar(key))return()
      ch$status$conditions<-as.character(ch$status$conditions%||%character())[tolower(as.character(ch$status$conditions%||%character()))!=key]
      ch$status$effects<-as.character(ch$status$effects%||%character())[tolower(as.character(ch$status$effects%||%character()))!=key]
      save_managed_character(ch,paste(tools::toTitleCase(key),"removed."))
    },ignoreInit=TRUE)
    

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
