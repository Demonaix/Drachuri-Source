# server/sidebar_module.R
library(shiny)
library(shinyjs)

sidebarTabUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Settings",
    value = "character",
    
    div(
      id = ns("root"),
      
      # ---- MENU / STEP 0 ----
      div(
        id = ns("menu"),
        
        div(class = "card",
            h4("Character Menu"),
            p("Choose an option: create a new character or load one from a backup."),
            
            div(
              style = "display:flex; gap:10px; flex-wrap:wrap;",
              actionButton(ns("go_new"), "New Character", class = "btn btn-warning"),
              actionButton(ns("go_upload"), "Upload Character", class = "btn btn-default"),
              actionButton(ns("clear_cache"), "Clear Character", class = "btn btn-danger")
            )
        ),

        div(class = "card",
            h4("Display & Sound"),
            tags$label(`for` = ns("master_volume"), "Music & ambience volume"),
            tags$div(
              style = "display:flex;align-items:center;gap:12px;max-width:520px;",
              tags$input(
                id = ns("master_volume"), type = "range",
                min = "0", max = "1", step = "0.05", value = "0.7",
                style = "flex:1;"
              ),
              tags$span(id = ns("master_volume_value"), "70%", style = "min-width:44px;font-weight:700;")
            ),
            tags$p(class = "help-block", "This setting is remembered on this computer."),
            tags$button(
              id = ns("browser_fullscreen"), type = "button",
              class = "btn btn-default",
              "Enter Full Screen"
            ),
            tags$p(class = "help-block", "Press Escape at any time to leave full screen.")
        ),

        div(class = "card",
            h4("Report an Issue"),
            p("Send a problem or improvement idea directly to the DM."),
            radioButtons(
              ns("issue_category"), "Report type",
              choices = c("Error / something went wrong" = "error",
                          "Feature upgrade / suggestion" = "feature_upgrade"),
              selected = "error", inline = TRUE
            ),
            textAreaInput(
              ns("issue_description"), "What happened, or what would you like changed?",
              rows = 5, width = "100%",
              placeholder = "Include what you clicked, what you expected, and what happened instead."
            ),
            tags$p(
              class = "help-block",
              "The recent Player diagnostic logs are attached automatically (maximum 200 KB). Password-like values are redacted."
            ),
            actionButton(ns("submit_issue"), "Send Report to DM", class = "btn btn-primary"),
            uiOutput(ns("issue_status"))
        ),
        
        # Download + log always accessible
        div(class = "card",
            h4("Save / Download"),
            downloadButton("download_char", "Download Character Backup"),
            tags$hr(),
            h4("Action Log"),
            actionButton("clear_log", "Clear Log", class = "btn btn-sm"),
            div(class = "log-box", verbatimTextOutput("log"))
        )
      ),
      
      # ---- STEP 1A: NEW CHARACTER FORM ----
      shinyjs::hidden(
        div(
          id = ns("new_step"),
          class = "card",
          h4("New Character"),
          
          uiOutput("portrait_ui"),
          div(style = "display:none;", textInput("portrait_file", NULL, value = NULL)),
          fileInput("portrait_upload", "Upload Character Portrait",
                    accept = c("image/png", "image/jpeg")),
          tags$hr(),
          
          # IMPORTANT: GLOBAL ids used by core_character.R
          textInput("name", "Character Name"),
          selectInput(
            "char_race",
            "Race",
            choices = c("Na'Haran", "Isildur", "Rhodrian", "Tylwyth Teg", "Other")
          ),
          selectInput(
            "class",
            "Class",
            choices = c("", "Na'Haran Sorcerer", "Hanianol Sorcerer", "Rogue", "Fighter", "Barbarian")
          ),
          selectInput("char_sub_class", "Archetype", choices = c(""), selected = ""),
          numericInput("level", "Level", value = 1, min = 1, max = 20),
          
          div(style = "display:flex; gap:10px; margin-top:12px; flex-wrap:wrap;",
              actionButton(ns("new_save"), "Save", class = "btn btn-primary"),
              actionButton(ns("back_from_new"), "Back", class = "btn btn-default")
          )
        )
      ),
      
      # ---- STEP 1B: UPLOAD CHARACTER ----
      shinyjs::hidden(
        div(
          id = ns("upload_step"),
          class = "card",
          h4("Upload Character Backup"),
          
          # IMPORTANT: GLOBAL id used by core_character.R
          fileInput("upload_char", "Upload Character Backup (.rds)", accept = ".rds"),
          
          div(style = "display:flex; gap:10px; margin-top:12px; flex-wrap:wrap;",
              actionButton(ns("upload_done"), "Done", class = "btn btn-primary"),
              actionButton(ns("back_from_upload"), "Back", class = "btn btn-default")
          )
        )
      )
    )
  )
}

sidebarTabServer <- function(id, state, restoring, char_rev, add_log = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_msg <- function(txt) {
      if (is.function(add_log)) {
        add_log(txt, toast = TRUE)
      } else {
        shinyjs::runjs(sprintf(
          "if (window.showToast) showToast(%s, 2200);",
          jsonlite::toJSON(txt, auto_unbox = TRUE)
        ))
      }
    }

    issue_status <- reactiveVal(NULL)
    output$issue_status <- renderUI({
      msg <- issue_status()
      if (is.null(msg)) return(NULL)
      tags$p(style = "margin-top:10px;font-weight:700;color:#35653b;", msg)
    })

    collect_issue_logs <- function(max_bytes = 200000L) {
      candidates <- unique(c(
        file.path(Sys.getenv("DRACHURI_LOG_DIR", unset = ""), "player.log"),
        file.path(Sys.getenv("DRACHURI_LOG_DIR", unset = ""), "launcher-console.log"),
        file.path(getwd(), "launcher", "logs", "launcher.log"),
        file.path(getwd(), "logs", "player.log")
      ))
      candidates <- candidates[nzchar(dirname(candidates)) & file.exists(candidates)]
      if (!length(candidates)) return("No diagnostic log files were found.")
      sections <- lapply(candidates, function(path) {
        lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
        lines <- tail(lines, 1500L)
        txt <- paste(lines, collapse = "\n")
        txt <- gsub("(?i)(password|passwd|pwd|token|secret)(\\s*[=:]\\s*)[^\\s,;]+", "\\1\\2[REDACTED]", txt, perl = TRUE)
        paste0("--- ", basename(path), " ---\n", txt)
      })
      result <- paste(unlist(sections), collapse = "\n\n")
      while (nchar(result, type = "bytes") > max_bytes && length(sections)) {
        result <- substr(result, max(1L, nchar(result) - floor(nchar(result) * 0.85)), nchar(result))
      }
      result
    }

    observeEvent(input$submit_issue, {
      description <- trimws(as.character(input$issue_description %||% ""))
      if (!nzchar(description)) {
        showNotification("Please add a short description before sending.", type = "warning", duration = 6)
        return()
      }
      if (isTRUE(state$offline_mode)) {
        showNotification("Reports need an online connection to reach the DM.", type = "error", duration = 8)
        return()
      }
      character_name <- as.character(state$char$meta$name %||% "Unknown Player")
      report_id <- create_player_issue_report(
        session_id = state$active_session_id,
        character_id = state$char_id,
        character_name = character_name,
        category = input$issue_category %||% "error",
        description = description,
        log_text = collect_issue_logs(),
        app_version = Sys.getenv("DRACHURI_APP_VERSION", unset = "0.4.0-launcher"),
        platform = paste(R.version$platform, Sys.info()[["sysname"]], Sys.info()[["release"]])
      )
      if (is.null(report_id)) {
        showNotification("The report could not be saved. Please check the connection and try again.", type = "error", duration = 10)
        return()
      }
      updateTextAreaInput(session, "issue_description", value = "")
      issue_status(paste0("Report #", report_id, " was sent to the DM."))
      showNotification(paste0("Report #", report_id, " sent. Thank you."), type = "message", duration = 8)
    }, ignoreInit = TRUE)
    
    show_menu <- function() {
      shinyjs::show("menu")
      shinyjs::hide("new_step")
      shinyjs::hide("upload_step")
    }
    
    show_new <- function() {
      shinyjs::hide("menu")
      shinyjs::show("new_step")
      shinyjs::hide("upload_step")
    }
    
    show_upload <- function() {
      shinyjs::hide("menu")
      shinyjs::hide("new_step")
      shinyjs::show("upload_step")
    }
    
    # Default to menu
    observeEvent(TRUE, {
      show_menu()
    }, once = TRUE)
    
    # ---- Menu actions ----
    
    # New Character: reset state then open form
    observeEvent(input$go_new, {
      shinyjs::click("new_char")   # core resets state + bumps char_rev
      show_new()
      log_msg("Creating a new character…")
    }, ignoreInit = TRUE)
    
    # Upload Character: reveal upload input only after click
    observeEvent(input$go_upload, {
      show_upload()
      log_msg("Select a character backup to upload.")
    }, ignoreInit = TRUE)
    
    # Clear Character: confirm first
    observeEvent(input$clear_cache, {
      showModal(modalDialog(
        title = "Clear character?",
        tags$div(
          style = "line-height:1.35;",
          tags$p(tags$strong("This will reset everything back to default values across the entire app.")),
          tags$p("Have you saved your character backup first?")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_clear"), "Yes, clear everything", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)
    
    # If confirmed: perform the reset (global)
    observeEvent(input$confirm_clear, {
      removeModal()
      
      # This is the “global reset” across the app:
      shinyjs::click("new_char")   # triggers core_character.R logic
      
      show_menu()
      log_msg("Character cleared. Everything reset to defaults.")
    }, ignoreInit = TRUE)
    
    # ---- New Character step ----
    observeEvent(input$new_save, {
      # Core already captured inputs live; this is just “confirm”.
      show_menu()
      log_msg("Character saved.")
    }, ignoreInit = TRUE)
    
    observeEvent(input$back_from_new, {
      show_menu()
    }, ignoreInit = TRUE)
    
    # ---- Upload step ----
    observeEvent(input$upload_done, {
      show_menu()
      log_msg("Upload complete.")
    }, ignoreInit = TRUE)
    
    observeEvent(input$back_from_upload, {
      show_menu()
    }, ignoreInit = TRUE)
    
    # If a character is loaded/replaced elsewhere, collapse back to menu
    observeEvent(char_rev(), {
      show_menu()
    }, ignoreInit = TRUE)
  })
}
