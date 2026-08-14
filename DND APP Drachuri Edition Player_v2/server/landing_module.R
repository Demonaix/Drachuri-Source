library(shiny)
library(shinyjs)
cat("SOURCED landing_module.R\n")
# -----------------------------
# Safe helpers (data access)
# -----------------------------
get_races_safe <- function() {
  if (exists("RACES")) names(RACES) else c("Na'Haran")
}

get_classes_safe <- function() {
  if (exists("CLASSES")) names(CLASSES) else character()
}

get_subclasses_safe <- function(class = NULL, race = NULL) {
  
  # Prefer CLASS subclasses
  if (!is.null(class) && nzchar(class) && exists("CLASSES")) {
    cls <- CLASSES[[class]]
    if (!is.null(cls) && !is.null(cls$subclasses)) {
      return(names(cls$subclasses))
    }
  }
  
  # Fallback to RACE subraces
  if (!is.null(race) && nzchar(race) && exists("RACES")) {
    r <- RACES[[race]]
    if (!is.null(r) && !is.null(r$subraces)) {
      return(names(r$subraces))
    }
  }
  
  character()
}

# -----------------------------
# UI
# -----------------------------
landingUI <- function(id) {
  ns <- NS(id)
  
  div(
    id = ns("root"),
    
    h3("Begin your journey"),
    tags$p("Create a new character, or load one from a backup."),
    
    div(
      style = "display:flex; gap:12px; flex-wrap:wrap;",
      actionButton(ns("new_open"), "New Character", class = "btn btn-warning"),
      actionButton(ns("upload_open"), "Load Character", class = "btn btn-default"),
      actionButton(ns("load_db"), "Continue Adventure")
    ),
    div(
      id = ns("db_wrap"),
      style = "display:none;",
      
      h4("Load from Server"),
      
      selectInput(ns("db_character"), "Select Character", choices = character()),
      
      div(
        style = "display:flex; gap:10px; margin-top:10px;",
        actionButton(ns("db_load"), "Load", class = "btn btn-primary"),
        actionButton(ns("db_cancel"), "Cancel", class = "btn btn-default")
      )
    ),
    
    tags$hr(),
    
    # ---- New character ----
    div(
      id = ns("new_wrap"),
      style = "display:none;",
      
      h4("New Character"),
      
      textInput(ns("name"), "Character Name"),
      
      selectInput(
        ns("race"),
        "Race",
        choices = get_races_safe(),
        selected = get_races_safe()[1]
      ),
      
      selectInput(
        ns("class"),
        "Class",
        choices = c("", get_classes_safe()),
        selected = ""
      ),
      
      selectInput(
        ns("subclass"),
        "Archetype",
        choices = character(),
        selected = ""
      ),
      
      numericInput(ns("level"), "Level", value = 1, min = 1, max = 20),
      
      div(
        style = "display:flex; gap:10px; margin-top:10px; flex-wrap:wrap;",
        actionButton(ns("new_save"), "Done", class = "btn btn-primary"),
        actionButton(ns("new_cancel"), "Cancel", class = "btn btn-default")
      )
    ),
    
    # ---- Upload ----
    div(
      id = ns("upload_wrap"),
      style = "display:none;",
      
      h4("Load Character"),
      
      fileInput(ns("upload_file"), "Upload Character Backup (.rds)", accept = ".rds"),
      
      div(
        style = "display:flex; gap:10px; margin-top:10px; flex-wrap:wrap;",
        actionButton(ns("upload_done"), "Load", class = "btn btn-primary"),
        actionButton(ns("upload_cancel"), "Cancel", class = "btn btn-default")
      )
    ),
    
    tags$hr(),
    
    # ---- Reset ----
    div(
      h4("Reset"),
      tags$p("This will clear the current character."),
      
      actionButton(ns("reset_open"), "Clear Cache / Reset", class = "btn btn-danger"),
      
      div(
        id = ns("reset_confirm_wrap"),
        style = "display:none;",
        
        tags$strong("Are you sure?"),
        
        div(
          style = "display:flex; gap:10px; margin-top:10px;",
          actionButton(ns("reset_yes"), "Yes", class = "btn btn-danger"),
          actionButton(ns("reset_no"), "Cancel", class = "btn btn-default")
        )
      )
    )
  )
}

landingTabUI <- function(id) {
  tabPanel(title = "Landing", value = "landing", landingUI(id))
}

# -----------------------------
# Server
# -----------------------------
landingTabServer <- function(
    id,
    state,
    restoring,
    add_log,
    char_rev,
    bump_char_rev,
    hydrate_core_ui,
    tabset_id = "main_tabs",
    camp_value = "camp",
    landing_stage_id = "landing-stage"
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cat("LANDING MODULE SERVER STARTED\n")
    cat("landing ns prefix:", session$ns("test"), "\n")
    
    # -------------------------
    # Helpers
    # -------------------------
    
    log_safe <- function(msg) {
      if (is.function(add_log)) {
        try(add_log(msg, toast = TRUE), silent = TRUE)
      }
    }
    
    toggle_ns <- function(id, show = TRUE) {
      shinyjs::runjs(sprintf(
        "var el=document.getElementById(%s); if(el){ el.style.display='%s'; }",
        jsonlite::toJSON(ns(id), auto_unbox = TRUE),
        if (show) "block" else "none"
      ))
    }
    
    hide_all <- function(...) {
      ids <- c(...)
      for (id in ids) toggle_ns(id, FALSE)
    }
    
    dismiss_landing <- function() {
      shinyjs::runjs(sprintf(
        "var el=document.getElementById(%s); if(el){ el.style.display='none'; }",
        jsonlite::toJSON(landing_stage_id, auto_unbox = TRUE)
      ))
    }
    
    go_camp <- function() {
      shinyjs::runjs(sprintf(
        "$('#%s a[data-value=\"%s\"]').tab('show');",
        tabset_id, camp_value
      ))
    }
    
#Load from server button
    observeEvent(input$load_db, {
      toggle_ns("db_wrap", TRUE)
      hide_all("new_wrap", "upload_wrap", "reset_confirm_wrap")
      
      chars <- tryCatch(
        list_characters_in_db(),
        error = function(e) NULL
      )
      
      if (is.null(chars) || !is.data.frame(chars) || nrow(chars) == 0) {
        updateSelectInput(session, "db_character", choices = character(0))
        log_safe("⚠️ Server unavailable or no saved characters found.")
        return()
      }
      
      ids <- as.character(chars$character_id %||% "")
      labels <- as.character(chars$name %||% "")
      
      blank_idx <- is.na(labels) | !nzchar(labels)
      labels[blank_idx] <- paste("Character", substr(ids[blank_idx], 1, 8))
      
      keep <- nzchar(ids)
      ids <- ids[keep]
      labels <- labels[keep]
      
      choices <- stats::setNames(ids, labels)
      
      updateSelectInput(session, "db_character", choices = choices)
    })
    
    observeEvent(input$db_load, {
      req(input$db_character)
      
      char_id <- input$db_character
      
      x <- tryCatch(
        load_character_from_db(char_id),
        error = function(e) NULL
      )
      
      if (is.null(x)) {
        log_safe("⚠️ Failed to load character from server.")
        return()
      }
      
      state$char <- validate_character(x)
      state$char_id <- char_id
      state$sync_enabled <- TRUE
      
      if (is.function(hydrate_core_ui)) hydrate_core_ui()
      if (is.function(bump_char_rev)) bump_char_rev()
      
      log_safe(paste0("📦 Loaded from server: ", state$char$meta$name %||% char_id))
      
      toggle_ns("db_wrap", FALSE)
      dismiss_landing()
      go_camp()
    })
    
    observeEvent(input$db_cancel, {
      toggle_ns("db_wrap", FALSE)
    })
    
  
    
    # -------------------------
    # Subclass updater (FIXED)
    # -------------------------
    
    update_subclasses <- function() {
      
      cat("---- SUBCLASS UPDATE ----\n")
      cat("CLASS:", input$class, "\n")
      cat("RACE:", input$race, "\n")
      
      subs <- get_subclasses_safe(
        class = input$class,
        race  = input$race
      )
      
      print(subs)
      
      updateSelectInput(
        session,
        "subclass",
        choices = c("", subs),
        selected = if (length(subs) > 0) subs[1] else ""
      )
    }
    
    # ✅ SINGLE observer (handles init + changes)
    observe({
      req(input$race)   # ensures at least race exists
      update_subclasses()
    })
    
    # -------------------------
    # UI toggles
    # -------------------------
    
    observeEvent(input$new_open, {
      toggle_ns("new_wrap", TRUE)
      hide_all("upload_wrap", "reset_confirm_wrap")
    }, ignoreInit = TRUE)
    
    observeEvent(input$upload_open, {
      toggle_ns("upload_wrap", TRUE)
      hide_all("new_wrap", "reset_confirm_wrap")
    }, ignoreInit = TRUE)
    
    observeEvent(input$new_cancel,    { toggle_ns("new_wrap", FALSE) }, ignoreInit = TRUE)
    observeEvent(input$upload_cancel, { toggle_ns("upload_wrap", FALSE) }, ignoreInit = TRUE)
    
    observeEvent(input$reset_open, { toggle_ns("reset_confirm_wrap", TRUE) }, ignoreInit = TRUE)
    observeEvent(input$reset_no,   { toggle_ns("reset_confirm_wrap", FALSE) }, ignoreInit = TRUE)
    
    # -------------------------
    # Reset
    # -------------------------
    
    observeEvent(input$reset_yes, {
      if (isTRUE(restoring())) return()
      
      state$char <- validate_character(new_character())
      state$char_id <- NULL
      state$sync_enabled <- FALSE
      
      if (is.function(hydrate_core_ui)) hydrate_core_ui()
      if (is.function(bump_char_rev)) bump_char_rev()
      
      log_safe("🧽 Reset character.")
      
      hide_all("reset_confirm_wrap", "new_wrap", "upload_wrap")
    }, ignoreInit = TRUE)
    
    # -------------------------
    # Create character
    # -------------------------
    
    observeEvent(input$new_save, {
      if (isTRUE(restoring())) return()
      
      default_race <- get_races_safe()[1]
      
      x <- validate_character(new_character())
      
      x$meta$name  <- trimws(input$name %||% "")
      x$meta$race  <- input$race %||% default_race
      x$build$class <- input$class %||% ""
      x$build$path  <- input$subclass %||% ""
      x$build$level <- as.integer(input$level %||% 1)
      
      state$char <- validate_character(x)
      state$char_id <- NULL
      
      new_id <- save_character_to_db(state$char)
      state$char_id <- new_id
      state$sync_enabled <- TRUE
      
      if (is.function(hydrate_core_ui)) hydrate_core_ui()
      if (is.function(bump_char_rev)) bump_char_rev()
      
      log_safe("🧙 Character created.")
      
      toggle_ns("new_wrap", FALSE)
      dismiss_landing()
      go_camp()
    }, ignoreInit = TRUE)
    
    observeEvent(input$new_save, {
      print("NEW SAVE CLICKED")
    })  
    # -------------------------
    # Upload
    # -------------------------
    
    observeEvent(input$upload_done, {
      if (isTRUE(restoring())) return()
      req(input$upload_file$datapath)
      
      x <- tryCatch(readRDS(input$upload_file$datapath), error = function(e) NULL)
      
      if (is.null(x)) {
        log_safe("⚠️ Upload failed.")
        return()
      }
      
      state$char <- validate_character(x)
      state$char_id <- NULL
      state$sync_enabled <- TRUE
      
      synced_id <- tryCatch(
        save_character_to_db(state$char),
        error = function(e) NULL
      )
      
      if (!is.null(synced_id)) {
        state$char_id <- synced_id
        log_safe(paste0("📦 Loaded backup and synced to server: ", state$char$meta$name %||% "(unnamed)"))
      } else {
        log_safe(paste0("📦 Loaded backup offline: ", state$char$meta$name %||% "(unnamed)"))
      }
      
      if (is.function(hydrate_core_ui)) hydrate_core_ui()
      if (is.function(bump_char_rev)) bump_char_rev()
      
      toggle_ns("upload_wrap", FALSE)
      dismiss_landing()
      go_camp()
    }, ignoreInit = TRUE)
    
  })
}