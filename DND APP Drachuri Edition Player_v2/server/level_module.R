library(shiny)

normalise_character_classes <- function(char, class_defs = CLASSES) {
  classes <- char$build$classes %||% list()

  if (!is.list(classes) || length(classes) == 0L) {
    primary_class <- as.character(char$build$class %||% "")
    primary_level <- suppressWarnings(as.integer(char$build$level %||% 1L))
    if (is.na(primary_level) || primary_level < 1L) primary_level <- 1L

    classes <- list(list(
      class = primary_class,
      level = primary_level,
      subclass = ""
    ))
  }

  lapply(seq_along(classes), function(i) {
    entry <- classes[[i]]
    class_name <- as.character(entry$class %||% "")
    level <- suppressWarnings(as.integer(entry$level %||% 1L))
    if (is.na(level) || level < 1L) level <- 1L
    if (level > 20L) level <- 20L

    subclass <- as.character(entry$subclass %||% "")
    legacy_path <- as.character(char$build$path %||% "")
    available_subclasses <- names(class_defs[[class_name]]$subclasses %||% list())
    if (i == 1L && !nzchar(subclass) && legacy_path %in% available_subclasses) {
      subclass <- legacy_path
    }

    list(
      class = class_name,
      level = level,
      subclass = subclass
    )
  })
}

level_options_for <- function(class_name, level, level_options = LEVEL_OPTIONS,
                              class_defs = CLASSES) {
  options <- level_options[[as.character(class_name)]][[as.character(level)]] %||% list()
  if (!is.list(options)) options <- list()

  level_features <- class_defs[[as.character(class_name)]]$levels[[as.character(level)]]$features %||% list()
  has_asi <- "asi" %in% names(level_features) || any(vapply(
    level_features,
    function(feature) "stat_increase" %in% infer_class_feature_tags("", feature),
    logical(1)
  ))

  if (has_asi && !any(vapply(options, function(x) grepl("^asi_", x$id %||% ""), logical(1)))) {
    stats <- c(
      "Strength" = "str", "Dexterity" = "dex", "Constitution" = "con",
      "Intelligence" = "int", "Blood Strength" = "bld_str", "Charisma" = "cha"
    )
    options <- c(options, list(
      list(id = "asi_first", label = "Ability Score Increase (+1)", options = stats),
      list(id = "asi_second", label = "Ability Score Increase (+1)", options = stats)
    ))
  }

  options
}

level_features_for <- function(class_name, level, subclass = "", class_defs = CLASSES) {
  class_data <- class_defs[[as.character(class_name)]] %||% list()
  base_features <- class_data$levels[[as.character(level)]]$features %||% list()
  subclass_features <- list()

  if (nzchar(as.character(subclass %||% ""))) {
    subclass_features <- class_data$subclasses[[subclass]]$levels[[as.character(level)]]$features %||% list()
  }

  c(base_features, subclass_features)
}

class_max_level <- function(class_name, class_defs = CLASSES) {
  levels <- suppressWarnings(as.integer(names(class_defs[[as.character(class_name)]]$levels %||% list())))
  levels <- levels[!is.na(levels)]
  if (!length(levels)) return(0L)
  max(levels)
}

apply_character_level_up <- function(char, class_index, selections = list(), hp_gain = 0L,
                                     class_defs = CLASSES, level_options = LEVEL_OPTIONS,
                                     timestamp = Sys.time()) {
  classes <- normalise_character_classes(char, class_defs)
  class_index <- suppressWarnings(as.integer(class_index))
  if (is.na(class_index) || class_index < 1L || class_index > length(classes)) {
    stop("Choose a valid class to advance.")
  }

  entry <- classes[[class_index]]
  class_name <- as.character(entry$class %||% "")
  if (!nzchar(class_name) || !class_name %in% names(class_defs)) {
    stop("This character does not have a valid class to advance.")
  }

  old_level <- suppressWarnings(as.integer(entry$level %||% 1L))
  new_level <- old_level + 1L
  max_level <- class_max_level(class_name, class_defs)
  if (new_level > max_level) {
    stop(paste0("Progression data for ", class_name, " currently ends at level ", max_level, "."))
  }

  required <- level_options_for(class_name, new_level, level_options, class_defs)
  stored_choices <- list()

  for (choice in required) {
    choice_id <- as.character(choice$id %||% "")
    allowed <- unname(as.character(choice$options %||% character()))
    selected <- as.character(selections[[choice_id]] %||% "")

    if (!nzchar(choice_id) || !nzchar(selected) || !selected %in% allowed) {
      stop(as.character(choice$label %||% "Complete every level-up choice."))
    }

    stored_choices[[choice_id]] <- selected
    if (identical(choice_id, "subclass")) entry$subclass <- selected
  }

  entry$level <- new_level
  classes[[class_index]] <- entry

  char$build$classes <- classes
  char$build$class <- as.character(classes[[1L]]$class %||% class_name)
  char$build$level <- sum(vapply(classes, function(x) as.integer(x$level %||% 0L), integer(1)))
  if (class_index == 1L && nzchar(entry$subclass)) char$build$path <- entry$subclass

  char$build$level_choices <- char$build$level_choices %||% list()
  char$build$level_choices[[class_name]] <- char$build$level_choices[[class_name]] %||% list()
  char$build$level_choices[[class_name]][[as.character(new_level)]] <- stored_choices

  asi_choices <- unname(unlist(stored_choices[grepl("^asi_", names(stored_choices))]))
  if (length(asi_choices)) {
    char$abilities <- char$abilities %||% list()
    for (stat in asi_choices) {
      current <- suppressWarnings(as.integer(char$abilities[[stat]] %||% 10L))
      if (is.na(current)) current <- 10L
      char$abilities[[stat]] <- min(20L, current + 1L)
    }
  }

  hp_gain <- suppressWarnings(as.integer(hp_gain))
  if (is.na(hp_gain) || hp_gain < 0L) hp_gain <- 0L
  char$resources <- char$resources %||% list()
  char$resources$hp <- char$resources$hp %||% list(cur = 0L, max = 0L, temp = 0L)
  old_hp_max <- suppressWarnings(as.integer(char$resources$hp$max %||% 0L))
  old_hp_cur <- suppressWarnings(as.integer(char$resources$hp$cur %||% 0L))
  if (is.na(old_hp_max)) old_hp_max <- 0L
  if (is.na(old_hp_cur)) old_hp_cur <- 0L
  char$resources$hp$max <- old_hp_max + hp_gain
  char$resources$hp$cur <- old_hp_cur + hp_gain

  char$build$level_history <- char$build$level_history %||% list()
  char$build$level_history <- append(char$build$level_history, list(list(
    class = class_name,
    from_level = old_level,
    to_level = new_level,
    subclass = as.character(entry$subclass %||% ""),
    choices = stored_choices,
    hp_gain = hp_gain,
    applied_at = as.character(timestamp)
  )))

  char <- apply_unlocked_class_effects(char, class_defs)

  list(
    character = char,
    class_name = class_name,
    old_level = old_level,
    new_level = new_level,
    subclass = as.character(entry$subclass %||% ""),
    features = level_features_for(class_name, new_level, entry$subclass, class_defs)
  )
}

levelTabUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Level Up",
    value = "level",
    tags$style(HTML("
      .levelup-wrap{display:grid;grid-template-columns:minmax(260px,.8fr) minmax(360px,1.2fr);gap:14px}
      .levelup-card{border:1px solid rgba(191,167,111,.6);background:rgba(255,255,245,.92);border-radius:14px;padding:16px;box-shadow:0 8px 22px rgba(0,0,0,.12)}
      .levelup-card h3,.levelup-card h4{margin-top:0}.levelup-muted{font-size:12px;opacity:.76}
      .levelup-class{display:flex;justify-content:space-between;gap:10px;padding:9px 0;border-bottom:1px solid rgba(191,167,111,.3)}
      .levelup-feature{padding:10px;border:1px solid rgba(191,167,111,.4);border-radius:10px;margin-top:8px;background:rgba(255,255,255,.55)}
      .levelup-feature-name{font-weight:900}.levelup-feature-desc{font-size:12px;opacity:.82;margin-top:3px}
      .levelup-tags{display:flex;flex-wrap:wrap;gap:5px;margin-top:7px}.levelup-tag{font-size:10px;font-weight:800;padding:2px 7px;border-radius:999px;background:#efe1c5;color:#60401f;text-transform:uppercase}
      .levelup-next{font-size:22px;font-weight:900;color:#70451f}.levelup-choice{padding:10px;margin-top:10px;border-left:4px solid #b2783d;background:rgba(244,226,191,.45)}
      .levelup-confirm{margin-top:14px;width:100%;font-weight:900}
      @media(max-width:850px){.levelup-wrap{grid-template-columns:1fr}}
    ")),
    div(
      class = "levelup-wrap",
      div(
        class = "levelup-card",
        h3("Character Progression"),
        p(class = "levelup-muted", "Choose which existing class gains one level. Multiclass characters can advance either class."),
        uiOutput(ns("current_classes_ui")),
        selectInput(ns("advance_class"), "Class to advance", choices = character()),
        uiOutput(ns("level_summary_ui"))
      ),
      div(
        class = "levelup-card",
        h3("Next Level"),
        uiOutput(ns("new_features_ui")),
        uiOutput(ns("level_choices_ui")),
        numericInput(ns("hp_gain"), "Maximum HP gained", value = 1L, min = 0L, step = 1L),
        p(class = "levelup-muted", "This amount is added to both maximum and current HP when the level is confirmed."),
        uiOutput(ns("confirm_level_up_ui"))
      )
    )
  )
}

levelTabServer <- function(id, state, restoring, add_log, char_rev) {
  moduleServer(id, function(input, output, session) {
    `%||%` <- get("%||%", inherits = TRUE)

    log_safe <- function(msg, toast = TRUE, flash = "none") {
      if (is.function(add_log)) try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
    }

    classes_r <- reactive({
      char_rev()
      normalise_character_classes(state$char)
    })

    observe({
      classes <- classes_r()
      labels <- vapply(seq_along(classes), function(i) {
        entry <- classes[[i]]
        subclass <- as.character(entry$subclass %||% "")
        paste0(
          entry$class %||% "Unknown class", " — level ", entry$level %||% 1L,
          if (nzchar(subclass)) paste0(" (", subclass, ")") else ""
        )
      }, character(1))
      choices <- stats::setNames(as.character(seq_along(classes)), labels)
      selected <- as.character(input$advance_class %||% "1")
      if (!selected %in% unname(choices)) selected <- unname(choices)[1]
      updateSelectInput(session, "advance_class", choices = choices, selected = selected)
    })

    selected_index <- reactive({
      idx <- suppressWarnings(as.integer(input$advance_class %||% 1L))
      classes <- classes_r()
      if (is.na(idx) || idx < 1L || idx > length(classes)) idx <- 1L
      idx
    })

    selected_entry <- reactive(classes_r()[[selected_index()]])
    next_level <- reactive(as.integer(selected_entry()$level %||% 1L) + 1L)

    observeEvent(list(input$advance_class, char_rev()), {
      entry <- selected_entry()
      hit_die <- suppressWarnings(as.integer(CLASSES[[entry$class]]$hit_die %||% 1L))
      if (is.na(hit_die) || hit_die < 1L) hit_die <- 1L
      updateNumericInput(session, "hp_gain", value = floor(hit_die / 2) + 1L)
    }, ignoreInit = FALSE)

    output$current_classes_ui <- renderUI({
      classes <- classes_r()
      tagList(lapply(classes, function(entry) {
        div(
          class = "levelup-class",
          tags$strong(entry$class %||% "Unknown class"),
          tags$span(
            paste0("Level ", entry$level %||% 1L),
            if (nzchar(entry$subclass %||% "")) paste0(" • ", entry$subclass) else ""
          )
        )
      }))
    })

    output$level_summary_ui <- renderUI({
      entry <- selected_entry()
      target <- next_level()
      max_level <- class_max_level(entry$class)
      if (target > max_level) return(div(
        class = "alert alert-info",
        paste0("Progression data for this class currently ends at level ", max_level, ".")
      ))
      tagList(
        tags$hr(),
        div(class = "levelup-next", paste(entry$class, entry$level, "→", target)),
        div(class = "levelup-muted", "Only this class advances; other class levels are unchanged.")
      )
    })

    chosen_subclass <- reactive({
      entry <- selected_entry()
      existing <- as.character(entry$subclass %||% "")
      choice <- as.character(input$level_choice_subclass %||% "")
      required <- level_options_for(entry$class, next_level())
      subclass_choice <- Filter(
        function(x) identical(as.character(x$id %||% ""), "subclass"),
        required
      )
      allowed <- if (length(subclass_choice)) {
        as.character(subclass_choice[[1L]]$options %||% character())
      } else {
        character()
      }
      if (nzchar(choice) && choice %in% allowed) choice else existing
    })

    output$new_features_ui <- renderUI({
      entry <- selected_entry()
      target <- next_level()
      if (target > class_max_level(entry$class)) return(NULL)
      features <- level_features_for(entry$class, target, chosen_subclass())
      if (length(features) == 0L) {
        return(p(class = "levelup-muted", "No feature is listed for this level. The level still advances hit points and class scaling."))
      }
      feature_ids <- names(features)
      if (is.null(feature_ids)) feature_ids <- paste0("feature_", seq_along(features))
      tagList(Map(function(feature_id, feature) {
        metadata <- class_feature_metadata(
          entry$class, target, feature_id, feature, chosen_subclass()
        )
        div(
          class = "levelup-feature",
          div(class = "levelup-feature-name", feature$name %||% "Class feature"),
          div(class = "levelup-feature-desc", feature$desc %||% ""),
          div(
            class = "levelup-tags",
            lapply(metadata$tags, function(tag) div(class = "levelup-tag", gsub("_", " ", tag)))
          )
        )
      }, feature_ids, features))
    })

    output$level_choices_ui <- renderUI({
      entry <- selected_entry()
      choices <- level_options_for(entry$class, next_level())
      if (length(choices) == 0L) return(NULL)
      tagList(lapply(choices, function(choice) {
        choice_id <- as.character(choice$id %||% "choice")
        div(
          class = "levelup-choice",
          selectInput(
            session$ns(paste0("level_choice_", choice_id)),
            as.character(choice$label %||% "Choose an option"),
            choices = c("Choose…" = "", choice$options %||% character()),
            selected = ""
          )
        )
      }))
    })

    output$confirm_level_up_ui <- renderUI({
      entry <- selected_entry()
      if (!nzchar(as.character(entry$class %||% "")) || !entry$class %in% names(CLASSES)) {
        return(div(
          class = "alert alert-warning",
          "This character's class is not present in the progression data yet, so it cannot be levelled safely."
        ))
      }
      if (next_level() > class_max_level(entry$class)) return(NULL)
      actionButton(
        session$ns("confirm_level_up"),
        "Confirm Level Up",
        class = "btn btn-primary levelup-confirm"
      )
    })

    observeEvent(input$confirm_level_up, {
      if (isTRUE(restoring())) return()
      entry <- selected_entry()
      target <- next_level()
      if (target > class_max_level(entry$class)) {
        showNotification("No further progression is defined for this class yet.", type = "error")
        return()
      }

      required <- level_options_for(entry$class, target)
      selections <- list()
      for (choice in required) {
        choice_id <- as.character(choice$id %||% "")
        selections[[choice_id]] <- input[[paste0("level_choice_", choice_id)]] %||% ""
      }

      result <- tryCatch(
        apply_character_level_up(
          state$char,
          class_index = selected_index(),
          selections = selections,
          hp_gain = input$hp_gain
        ),
        error = function(e) e
      )

      if (inherits(result, "error")) {
        showNotification(conditionMessage(result), type = "error")
        return()
      }

      state$char <- result$character
      if (is.function(char_rev)) char_rev(isolate(char_rev()) + 1L)
      log_safe(paste0(
        "⬆️ ", result$class_name, " advanced to level ", result$new_level,
        if (nzchar(result$subclass)) paste0(" (", result$subclass, ")") else "", "."
      ))
      showNotification(paste(result$class_name, "is now level", result$new_level), type = "message")
    }, ignoreInit = TRUE)
  })
}
