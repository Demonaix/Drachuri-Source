controlSessionsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "control-card",
      
      div(class = "control-section-title", "Sessions"),
      
      fluidRow(
        column(
          4,
          selectInput(
            ns("session_select"),
            "Select Session",
            choices = NULL
          )
        ),
        column(
          2,
          actionButton(ns("refresh_sessions"), "Refresh", class = "btn btn-default")
        ),
        column(
          2,
          actionButton(ns("set_active"), "Set Active", class = "btn btn-primary")
        )
      ),
      
      tags$hr(),
      
      h4("Create New Session"),
      
      fluidRow(
        column(
          4,
          textInput(ns("new_session_name"), "Session Name", placeholder = "My Campaign")
        ),
        column(
          3,
          selectInput(
            ns("new_session_mode"),
            "Mode",
            choices = c("exploration", "combat", "downtime"),
            selected = "exploration"
          )
        ),
        column(
          2,
          actionButton(ns("create_session"), "Create", class = "btn btn-success")
        )
      ),
      
      tags$hr(),
      
      h4("Session Details"),
      
      tableOutput(ns("session_table"))
    )
  )
}

controlSessionsServer <- function(id, ctrl, session_tbl, players_tbl, bump_refresh) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    ns <- session$ns
    
    # ------------------------------------------
    # Load sessions list
    # ------------------------------------------
    sessions_rv <- reactiveVal(data.frame())
    session_choice_signature <- reactiveVal(NULL)
    
    load_sessions <- function() {
      df <- tryCatch(
        get_all_sessions(),
        error = function(e) data.frame()
      )
      
      sessions_rv(df)
      
      if (is.data.frame(df) && nrow(df) > 0) {
        id_col <- if ("session_id" %in% names(df)) "session_id" else if ("id" %in% names(df)) "id" else NULL
        
        if (!is.null(id_col) && is.null(ctrl$session_id)) {
          first_id <- suppressWarnings(as.integer(df[[id_col]][1]))
          if (!is.na(first_id) && first_id > 0) {
            ctrl$session_id <- first_id
          }
        }
      } else {
        ctrl$session_id <- NULL
      }
    }
    
    # initial load
    observeEvent(TRUE, {
      load_sessions()
    }, once = TRUE)
    
    # manual refresh
    observeEvent(input$refresh_sessions, {
      load_sessions()
    })
    
    # ------------------------------------------
    # Update dropdown
    # ------------------------------------------
    observe({
      df <- sessions_rv()
      
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        if (!identical(session_choice_signature(), "")) {
          session_choice_signature("")
          updateSelectInput(session, "session_select", choices = c())
        }
        return()
      }
      
      # allow either id or session_id
      id_col <- if ("session_id" %in% names(df)) "session_id" else if ("id" %in% names(df)) "id" else NULL
      name_col <- if ("name" %in% names(df)) "name" else NULL
      
      if (is.null(id_col) || is.null(name_col)) {
        warning("⚠️ get_all_sessions missing required columns")
        if (!identical(session_choice_signature(), "")) {
          session_choice_signature("")
          updateSelectInput(session, "session_select", choices = c())
        }
        return()
      }
      
      ids <- as.character(df[[id_col]])
      labels <- paste0(as.character(df[[name_col]]), " (", ids, ")")
      signature <- paste(ids, labels, collapse = "|")
      if (identical(signature, session_choice_signature())) return()
      session_choice_signature(signature)
      
      selected_id <- as.character(isolate(input$session_select) %||% ctrl$session_id %||% "")
      
      if (!nzchar(selected_id) || !selected_id %in% ids) {
        selected_id <- ids[1]
      }
      
      updateSelectInput(
        session,
        "session_select",
        choices = stats::setNames(ids, labels),
        selected = selected_id
      )
    })
    
    # ------------------------------------------
    # Set active session
    # ------------------------------------------
    observeEvent(input$set_active, {
      sid <- suppressWarnings(as.integer(input$session_select))
      
      if (is.na(sid) || sid < 1) return()
      
      ctrl$session_id <- sid
      bump_refresh()
      showNotification(paste0("Using session #", sid), type = "message")
    })
    
    # ------------------------------------------
    # Create new session
    # ------------------------------------------
    observeEvent(input$create_session, {
      nm <- as.character(input$new_session_name %||% "")
      mode <- as.character(input$new_session_mode %||% "exploration")
      
      if (!nzchar(nm)) {
        showNotification("Enter a session name", type = "error")
        return()
      }
      
      sid <- tryCatch(
        create_session(
          name = nm,
          mode = mode
        ),
        error = function(e) NULL
      )
      
      if (is.null(sid)) {
        showNotification("Failed to create session", type = "error")
        return()
      }
      
      showNotification(paste0("Session created (ID ", sid, ")"), type = "message")
      
      load_sessions()
      
      ctrl$session_id <- as.integer(sid)
      bump_refresh()
      
      updateSelectInput(session, "session_select", selected = as.character(sid))
    })
    
    # ------------------------------------------
    # Session table output
    output$session_table <- renderTable({
      df <- sessions_rv()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      
      keep <- c("session_id", "id", "name", "mode", "status", "updated_at")
      keep <- keep[keep %in% names(df)]
      
      if (!length(keep)) return(NULL)
      
      df[, keep, drop = FALSE]
    }, striped = TRUE, bordered = TRUE)
  })
}
