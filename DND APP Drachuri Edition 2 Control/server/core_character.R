# server/core_character.R
library(shiny)
library(shinyjs)
library(jsonlite)
cat("SOURCED core_character.R\n")
characterCoreServer <- function(input, output, session) {
  
  initialise_hp_if_needed <- function(x) {
    x <- validate_character(x)
    
    # Decide what the character's intended starting HP should be.
    # Replace this with your real rule.
    #
    # Example options:
    # 1) fixed starting HP:
    # target_max <- 12
    #
    # 2) from a field already stored somewhere:
    # target_max <- as.integer(x$meta$starting_hp %||% 12)
    #
    # 3) from race / class / level / custom logic:
    # target_max <- compute_starting_hp(x)
    #
    # For now, set your chosen default here:
    target_max <- 12
    
    target_max <- suppressWarnings(as.integer(target_max))
    if (is.na(target_max) || target_max < 1) target_max <- 1
    
    hp_missing <- is.null(x$resources$hp) ||
      !is.list(x$resources$hp) ||
      is.null(x$resources$hp$max) ||
      is.null(x$resources$hp$cur)
    
    hp_legacy_default <- identical(as.integer(x$resources$hp$max %||% NA), 10L) &&
      identical(as.integer(x$resources$hp$cur %||% NA), 10L)
    
    if (hp_missing || hp_legacy_default) {
      x$resources$hp$max <- target_max
      x$resources$hp$cur <- target_max
      x$resources$hp$temp <- as.integer(x$resources$hp$temp %||% 0)
    }
    
    x
  }
  
  # ----------------------------
  # Revision counter ("character replaced" signal)
  # ----------------------------
  char_rev <- reactiveVal(0)
  bump_char_rev <- function() char_rev(char_rev() + 1)
  
  # ----------------------------
  # Canonical state
  # ----------------------------
 
  
  state <- reactiveValues(
    char = validate_character(new_character()),
    char_id = NULL,
    active_session_id = NULL,
    offline_mode = FALSE,
    offline_modal_shown = FALSE,
    sync_enabled = FALSE
  )
  
  
  load_character_into_state <- function(char_id) {
    x <- load_character_from_db(char_id)
    if (is.null(x)) return(FALSE)
    
    state$char <- initialise_hp_if_needed(validate_character(x))
    state$char_id <- char_id
    hydrate_core_ui()
    bump_char_rev()
    TRUE
  }
  

  
  # Guard to prevent observers firing during restore/hydrate
  restoring <- reactiveVal(FALSE)
  
  # ----------------------------
  # Log helper
  # ----------------------------
  add_log <- function(msg, toast = TRUE, toast_ms = 2500,
                      flash = c("none", "red", "gold")) {
    flash <- match.arg(flash)
    
    # Ensure schema
    state$char <- validate_character(state$char)
    
    state$char$journal$log <- c(
      state$char$journal$log,
      paste0(format(Sys.time(), "%H:%M:%S"), " | ", msg)
    )
    
    if (isTRUE(toast)) {
      shinyjs::runjs(sprintf(
        "if (window.showToast) showToast(%s, %d);",
        jsonlite::toJSON(msg, auto_unbox = TRUE),
        toast_ms
      ))
    }
    
    if (flash != "none") {
      shinyjs::runjs(sprintf(
        "if (window.flashScreen) flashScreen(%s);",
        jsonlite::toJSON(flash, auto_unbox = TRUE)
      ))
    }
  }
  
  # ----------------------------
  # UI -> State (identity/build)
  # ----------------------------
  observeEvent(input$name, {
    if (isTRUE(restoring())) return()
    state$char <- validate_character(state$char)
    state$char$meta$name <- input$name %||% ""
  }, ignoreInit = TRUE)
  
  # Optional inputs (only if present in your UI)
  observeEvent(input$class, {
    if (isTRUE(restoring())) return()
    state$char <- validate_character(state$char)
    state$char$build$class <- input$class %||% ""
  }, ignoreInit = TRUE)
  
  observeEvent(input$char_sub_class, {
    if (isTRUE(restoring())) return()
    state$char <- validate_character(state$char)
    state$char$build$path <- input$char_sub_class %||% ""
  }, ignoreInit = TRUE)
  
  observeEvent(input$char_race, {
    if (isTRUE(restoring())) return()
    state$char <- validate_character(state$char)
    # keep this in meta (or move if you have a better place)
    state$char$meta$race <- input$char_race %||% ""
  }, ignoreInit = TRUE)
  
  observeEvent(input$level, {
    if (isTRUE(restoring())) return()
    state$char <- validate_character(state$char)
    state$char$build$level <- as.integer(input$level %||% 1)
  }, ignoreInit = TRUE)
  
  # Abilities (homebrew: bld_str instead of wis)
  for (ab in c("str", "dex", "con", "int", "bld_str", "cha")) {
    local({
      id <- ab
      observeEvent(input[[id]], {
        if (isTRUE(restoring())) return()
        state$char <- validate_character(state$char)
        state$char$abilities[[id]] <- suppressWarnings(as.numeric(input[[id]]))
      }, ignoreInit = TRUE)
    })
  }
  
  # ----------------------------
  # Hydrate base inputs (after new/load)
  # ----------------------------
  safe_update_text <- function(id, value) {
    try(updateTextInput(session, id, value = value), silent = TRUE)
  }
  safe_update_select <- function(id, selected) {
    try(updateSelectInput(session, id, selected = selected), silent = TRUE)
  }
  safe_update_numeric <- function(id, value) {
    try(updateNumericInput(session, id, value = value), silent = TRUE)
  }
  
  hydrate_core_ui <- function() {
    state$char <- validate_character(state$char)
    x <- state$char
    
    restoring(TRUE)
    on.exit(restoring(FALSE), add = TRUE)
    
    safe_update_text("name", x$meta$name %||% "")
    
    safe_update_select("class", x$build$class %||% "")
    safe_update_select("char_sub_class", x$build$path %||% "")
    safe_update_numeric("level", x$build$level %||% 1)
    
    # optional
    safe_update_select("char_race", x$meta$race %||% "")
    
    for (ab in c("str", "dex", "con", "int", "bld_str", "cha")) {
      safe_update_numeric(ab, x$abilities[[ab]] %||% 10)
    }
  }
  
  # ----------------------------
  # New character (core reset)
  # ----------------------------
  create_new_character <- function() {
    x <- validate_character(new_character())
    x <- initialise_hp_if_needed(x)
    
    state$char <- x
    hydrate_core_ui()
    bump_char_rev()
    add_log("Created new character.")
  }
  
  observeEvent(input$new_char, {
    if (isTRUE(restoring())) return()
    create_new_character()
  }, ignoreInit = TRUE)
  
  # ----------------------------
  # Download / Upload (Save / Load)
  # ----------------------------
  output$download_char <- downloadHandler(
    filename = function() {
      x <- validate_character(state$char)
      nm <- x$meta$name %||% ""
      nm <- if (!nzchar(nm)) "character" else gsub("[^A-Za-z0-9_-]+", "_", nm)
      paste0(nm, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    },
    content = function(file) {
      x <- validate_character(state$char)
      x$meta$updated_at <- Sys.time()
      saveRDS(x, file)
    }
  )
  #New
 
  observeEvent(input$upload_char, {
    req(input$upload_char$datapath)
    x <- tryCatch(readRDS(input$upload_char$datapath), error = function(e) NULL)
    if (is.null(x)) {
      add_log("Upload failed: not a valid RDS.", toast = TRUE, flash = "red")
      return()
    }
    
    state$char <- initialise_hp_if_needed(validate_character(x))
    hydrate_core_ui()
    bump_char_rev()
    add_log(
      paste0("Loaded character: ", state$char$meta$name %||% "(unnamed)"),
      toast = TRUE,
      flash = "gold"
    )
  }, ignoreInit = TRUE)
  
  add_blood_intake <- function(pints) {
    state$char <- validate_character(state$char)
    state$char$blood$addiction$current_day_intake <-
      state$char$blood$addiction$current_day_intake + pints
  }
  
  # ----------------------------
  # Derived displays
  # ----------------------------
  output$title_name <- renderText({
    x <- validate_character(state$char)
    nm <- x$meta$name %||% ""
    if (!nzchar(nm)) "Unnamed Character" else nm
  })
  
  output$summary <- renderPrint({
    x <- validate_character(state$char)
    mods <- lapply(x$abilities, mod_calc)
    
    list(
      class = x$build$class,
      archetype = x$build$path,
      level = x$build$level,
      abilities = x$abilities,
      modifiers = mods,
      hp = x$resources$hp,
      sindre = x$resources$sindre,
      conditions = x$status$conditions,
      effects = x$status$effects
    )
  })
  
  output$log <- renderText({
    x <- validate_character(state$char)
    paste(x$journal$log %||% character(0), collapse = "\n")
  })
  
  observeEvent(input$clear_log, {
    state$char <- validate_character(state$char)
    state$char$journal$log <- character(0)
  }, ignoreInit = TRUE)
  
  # ----------------------------
  # Optional roll demo (only if input exists)
  # ----------------------------
  roll_counter <- reactiveVal(0)
  observeEvent(input$roll_d20, {
    if (isTRUE(restoring())) return()
    roll_counter(roll_counter() + 1)
    roll_id <- paste0(as.integer(Sys.time()), "-", roll_counter())
    roll <- sample(1:20, 1)
    add_log(paste0("ROLL ", roll_id, " | d20 = ", roll))
  }, ignoreInit = TRUE)
  
  # Initial hydrate
  observeEvent(TRUE, {
    hydrate_core_ui()
  }, once = TRUE)
  
  observeEvent(input$char_race, {
    if (restoring()) return()
    state$char$meta$race <- input$char_race
  }, ignoreInit = TRUE)
  
  # Return handles for other modules
  list(
    state = state,
    restoring = restoring,
    add_log = add_log,
    char_rev = char_rev,
    bump_char_rev = bump_char_rev,
    hydrate_core_ui = hydrate_core_ui,
    create_new_character = create_new_character,
    load_character_into_state = load_character_into_state  # 👈 NEW
  )
}