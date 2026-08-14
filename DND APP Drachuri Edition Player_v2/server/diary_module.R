# server/diary_module.R
library(shiny)

# ----------------------------
# UI
# ----------------------------
diaryTabUI <- function(id) {
  ns <- NS(id)
  
  tags$audio(
    id = "diary_sfx",
    src = "pencil.wav",  # or pencil.mp3
    loop = NA,
    preload = "auto",
    style = "display:none;"
  )
  
  # A "hard" Shiny button: no Shiny binding needed, we set input manually.
  hardBtn <- function(input_id, label, class = "btn btn-default btn-sm") {
    tags$button(
      type = "button",
      class = paste(class, "diary-btn"),
      style = "pointer-events:auto;",  # belt + braces
      onclick = sprintf(
        "if(window.Shiny){Shiny.setInputValue('%s', Date.now(), {priority:'event'});} console.log('[Diary] click %s');",
        ns(input_id), ns(input_id)
      ),
      label
    )
  }
  
  tabPanel(
    "Diary",
    
    tags$script(HTML(sprintf("
(function(){
  var key = '__DIARY_%s__';
  if (window[key]) return;
  window[key] = true;
  console.log('[Diary] mounted: %s');

  // Click tracer (optional)
  document.addEventListener('click', function(e){
    var t = e.target;
    if (!t) return;
    if (t.classList && t.classList.contains('diary-btn')){
      console.log('[Diary BTN]', t.innerText || '(no text)');
    }
  }, true);
})();
", ns("root"), ns("root")))),
    
    tags$style(HTML({
      root <- ns("root")
      pfx  <- paste0("#", root, " ")
      paste0(
        pfx, ".card{border:1px solid rgba(150,120,70,0.55);border-radius:14px;background:rgba(255,255,245,0.85);padding:14px;margin-bottom:14px;box-shadow:0 4px 10px rgba(0,0,0,0.08);}\n",
        pfx, ".titlebar{display:flex;align-items:center;justify-content:space-between;gap:10px;margin-bottom:10px;}\n",
        pfx, ".subtle{opacity:.85;font-size:13px;}\n",
        pfx, ".btn-pill{height:30px;padding:0 10px!important;line-height:28px!important;border-radius:999px;font-size:12px;}\n",
        pfx, ".entry-box .form-control{border-radius:12px;}\n",
        pfx, ".entry-card{border:1px solid rgba(150,120,70,0.45);border-radius:14px;background:rgba(255,255,250,0.92);padding:12px;margin-bottom:10px;}\n",
        pfx, ".entry-head{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;}\n",
        pfx, ".entry-title{font-weight:800;font-size:16px;}\n",
        pfx, ".entry-meta{opacity:.85;font-size:12px;margin-top:2px;}\n",
        pfx, ".entry-body{white-space:pre-wrap;word-break:break-word;margin-top:10px;}\n",
        pfx, ".chipbar{display:flex;gap:6px;flex-wrap:wrap;margin-top:6px;}\n",
        pfx, ".chip{display:inline-flex;align-items:center;gap:6px;padding:4px 10px;border-radius:999px;border:1px solid rgba(150,120,70,0.35);background:rgba(255,255,245,0.75);font-size:12px;}\n",
        pfx, ".chip.mood{font-weight:700;}\n",
        pfx, ".tools{display:flex;gap:6px;flex-wrap:wrap;justify-content:flex-end;}\n",
        pfx, ".hr{border-top:1px solid rgba(150,120,70,0.25);margin:12px 0;}\n"
      )
    })),
    
    div(
      id = ns("root"),
      
      # --- Write card ---
      div(
        class = "card entry-box",
        div(
          class = "titlebar",
          h4("📖 Field Journal"),
          div(
            style = "display:flex; gap:8px; align-items:center;",
            hardBtn("save_entry",  "Save",        class = "btn btn-primary btn-pill"),
            hardBtn("clear_entry", "Clear",       class = "btn btn-default btn-pill"),
            hardBtn("delete_last", "Delete last", class = "btn btn-danger btn-pill")
          )
        ),
        
        fluidRow(
          column(8, textInput(ns("title"), "Entry title", placeholder = "e.g. The moon over Annwn...")),
          column(4, selectInput(
            ns("mood"),
            "Tone",
            choices = c("Neutral","Hungry","Hopeful","Wary","Afraid","Furious","Victorious","Grieving","Inspired","Horny"),
            selected = "Neutral"
          ))
        ),
        
        textInput(ns("tags"), "Tags (comma separated)", placeholder = "e.g. travel, fae, battle, dream"),
        
        textAreaInput(
          ns("body"),
          NULL,
          value = "",
          rows = 8,
          width = "100%",
          placeholder = "Write as if it’s your character’s own hand…\n\nWhat happened? What did it feel like? What do you fear? What do you want?"
        ),
        
        div(class = "subtle", uiOutput(ns("writing_hint_ui")))
      ),
      
      # --- Browse card ---
      div(
        class = "card",
        div(
          class = "titlebar",
          h4("🗂️ Entries"),
          div(
            class = "tools",
            textInput(ns("search"), NULL, placeholder = "Search…", width = "220px"),
            uiOutput(ns("tag_filter_ui")),
            hardBtn("copy_latest", "Copy latest", class = "btn btn-default btn-pill"),
            hardBtn("copy_all",    "Copy all",    class = "btn btn-default btn-pill")
          )
        ),
        
        div(class="hr"),
        uiOutput(ns("entries_ui")),
        uiOutput(ns("clipboard_ui"))
      )
    )
  )
}

diaryTabServer <- function(id, state, restoring, add_log, char_rev) {
  moduleServer(id, function(input, output, session) {
    
    # ---- super-safe fallbacks (no missing()) ----
    if (is.null(state)) stop("diaryTabServer: state is NULL")
    if (is.null(restoring)) restoring <- reactiveVal(FALSE)
    if (is.null(char_rev))  char_rev  <- reactiveVal(0)
    
    safe_log <- function(msg, flash = "none", toast = FALSE) {
      if (is.null(add_log)) {
        cat("[Diary log]", msg, "\n")
        return(invisible(NULL))
      }
      tryCatch(
        add_log(msg = msg, flash = flash, toast = toast),
        error = function(e1) {
          tryCatch(add_log(msg), error = function(e2) NULL)
        }
      )
    }
    
    hp_label <- function(cur, max, temp = 0) {
      cur  <- suppressWarnings(as.numeric(cur))
      max  <- suppressWarnings(as.numeric(max))
      temp <- suppressWarnings(as.numeric(temp))
      
      if (is.na(max) || max <= 0) return("Unknown")
      if (is.na(cur)) cur <- 0
      if (cur <= 0) return("Down")
      
      pct <- cur / max
      lab <- if (pct >= 0.90) "Thriving"
      else if (pct >= 0.70) "Steady"
      else if (pct >= 0.50) "Bruised"
      else if (pct >= 0.30) "Wounded"
      else if (pct >= 0.15) "Critical"
      else "Near Death"
      
      if (!is.na(temp) && temp > 0) lab <- paste0(lab, " (Shielded)")
      lab
    }
    
    ensure_diary <- function() {
      state$char <- validate_character(state$char)
      
      state$char$diary <- state$char$diary %||% list()
      if (!is.data.frame(state$char$diary$entries)) {
        state$char$diary$entries <- data.frame(
          id = character(0),
          ts = as.POSIXct(character(0), tz = "UTC"),
          title = character(0),
          mood = character(0),
          tags = character(0),
          body = character(0),
          hp_cur = integer(0),
          hp_max = integer(0),
          hp_temp = integer(0),
          effects = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      df <- state$char$diary$entries
      req <- c("id","ts","title","mood","tags","body","hp_cur","hp_max","hp_temp","effects")
      for (nm in req) if (!nm %in% names(df)) df[[nm]] <- NA
      
      if (!inherits(df$ts, "POSIXct")) suppressWarnings(df$ts <- as.POSIXct(df$ts, tz = "UTC"))
      suppressWarnings(df$hp_cur  <- as.integer(df$hp_cur))
      suppressWarnings(df$hp_max  <- as.integer(df$hp_max))
      suppressWarnings(df$hp_temp <- as.integer(df$hp_temp))
      
      df$title[is.na(df$title)] <- ""
      df$mood[is.na(df$mood) | df$mood == ""] <- "Neutral"
      df$tags[is.na(df$tags)] <- ""
      df$body[is.na(df$body)] <- ""
      df$effects[is.na(df$effects)] <- ""
      
      state$char$diary$entries <- df
      invisible(TRUE)
    }
    
    observeEvent(TRUE, ensure_diary(), once = TRUE)
    
    # ---- writing hint ----
    output$writing_hint_ui <- renderUI({
      txt <- trimws(input$body %||% "")
      n <- nchar(txt)
      hint <- if (n < 40) "Tip: add a detail — smell, sound, a name."
      else if (n < 140) "Nice. Add a fear, vow, or question."
      else "This reads like a real page. Add a closing omen or note."
      tags$span(hint)
    })
    
    # ---- DEBUG: prove clicks reach R ----
    observeEvent(input$save_entry,  { cat("[Diary] SAVE event:",  input$save_entry,  "\n") }, ignoreInit = TRUE)
    observeEvent(input$clear_entry, { cat("[Diary] CLEAR event:", input$clear_entry, "\n") }, ignoreInit = TRUE)
    observeEvent(input$delete_last, { cat("[Diary] DEL event:",   input$delete_last, "\n") }, ignoreInit = TRUE)
    observeEvent(input$copy_latest, { cat("[Diary] COPY LATEST:", input$copy_latest, "\n") }, ignoreInit = TRUE)
    observeEvent(input$copy_all,    { cat("[Diary] COPY ALL:",    input$copy_all,    "\n") }, ignoreInit = TRUE)
    
    # ---- Save entry ----
    observeEvent(input$save_entry, {
      if (isTRUE(restoring())) return()
      ensure_diary()
      
      body <- trimws(input$body %||% "")
      if (!nzchar(body)) {
        safe_log("⚠️ Cannot save an empty entry.", toast = TRUE)
        return()
      }
      
      x <- validate_character(state$char)
      hp_eff <- get_effective_hp_state(state)
      
      effects_vec <- x$status$effects %||% character()
      effects_str <- paste(effects_vec, collapse = ", ")
      
      title <- trimws(input$title %||% "")
      mood  <- trimws(input$mood %||% "Neutral")
      tags  <- trimws(input$tags %||% "")
      
      new_row <- data.frame(
        id = paste0("d", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
        ts = Sys.time(),
        title = title,
        mood = if (nzchar(mood)) mood else "Neutral",
        tags = tags,
        body = body,
        hp_cur  = as.integer(hp_eff$cur %||% NA),
        hp_max  = as.integer(hp_eff$max %||% NA),
        hp_temp = as.integer(hp_eff$temp %||% 0),
        effects = effects_str,
        stringsAsFactors = FALSE
      )
      
      state$char$diary$entries <- rbind(state$char$diary$entries, new_row)
      
      updateTextInput(session, "title", value = "")
      updateTextInput(session, "tags", value = "")
      updateSelectInput(session, "mood", selected = "Neutral")
      updateTextAreaInput(session, "body", value = "")
      
      safe_log("📝 Saved to field journal.", toast = TRUE)
    }, ignoreInit = TRUE)
    
    # ---- Clear draft ----
    observeEvent(input$clear_entry, {
      if (isTRUE(restoring())) return()
      updateTextAreaInput(session, "body", value = "")
      safe_log("🧽 Cleared draft.", toast = TRUE)
    }, ignoreInit = TRUE)
    
    # ---- Delete last ----
    observeEvent(input$delete_last, {
      if (isTRUE(restoring())) return()
      ensure_diary()
      
      df <- state$char$diary$entries
      if (nrow(df) == 0) {
        safe_log("ℹ️ No entries to delete.", toast = TRUE)
        return()
      }
      
      idx <- order(df$ts, decreasing = TRUE)[1]
      state$char$diary$entries <- df[-idx, , drop = FALSE]
      safe_log("🗑️ Deleted latest entry.", toast = TRUE)
    }, ignoreInit = TRUE)
    
    # ---- Render entries ----
    output$entries_ui <- renderUI({
      df <- state$char$diary$entries
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(shiny::tags$em("No entries yet."))
      
      q <- trimws(input$search %||% "")
      if (nzchar(q)) {
        qq <- tolower(q)
        keep <- grepl(qq, tolower(df$title), fixed = TRUE) |
          grepl(qq, tolower(df$body),  fixed = TRUE) |
          grepl(qq, tolower(df$tags),  fixed = TRUE) |
          grepl(qq, tolower(df$mood),  fixed = TRUE)
        df <- df[keep, , drop = FALSE]
        if (nrow(df) == 0) return(shiny::tags$em("No entries match your search."))
      }
      
      df <- df[order(df$ts, decreasing = TRUE), , drop = FALSE]
      
      shiny::tagList(lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, , drop = FALSE]
        
        ts <- row$ts[[1]]
        ts_txt <- if (!is.na(ts)) format(ts, "%Y-%m-%d %H:%M") else "Unknown time"
        
        title <- row$title[[1]] %||% ""
        mood  <- row$mood[[1]]  %||% "Neutral"
        tag_str <- row$tags[[1]] %||% ""   # <-- RENAMED (was `tags`)
        body  <- row$body[[1]]  %||% ""
        
        hp_txt <- hp_label(row$hp_cur[[1]], row$hp_max[[1]], row$hp_temp[[1]])
        
        effects <- row$effects[[1]] %||% ""
        effects_txt <- if (nzchar(trimws(effects))) effects else "None"
        
        chips <- list(
          shiny::tags$div(class = "chip mood", paste0("Mood: ", mood)),
          shiny::tags$div(class = "chip",      paste0("HP: ", hp_txt))
        )
        if (nzchar(tag_str)) {
          chips <- c(chips, list(shiny::tags$div(class="chip", paste0("Tags: ", tag_str))))
        }
        
        shiny::tags$div(
          class = "entry-card",
          shiny::tags$div(
            class = "entry-head",
            shiny::tags$div(
              shiny::tags$div(class = "entry-title", if (nzchar(title)) title else "Untitled entry"),
              shiny::tags$div(class = "entry-meta", ts_txt),
              shiny::tags$div(class = "chipbar", chips),
              shiny::tags$div(class = "entry-meta", shiny::tags$strong("Status: "), effects_txt)
            )
          ),
          shiny::tags$div(class = "entry-body", body)
        )
      }))
    })
    
    output$tag_filter_ui <- renderUI(NULL)
    output$clipboard_ui  <- renderUI(NULL)
    
    observeEvent(char_rev(), {
      restoring(TRUE)
      on.exit(restoring(FALSE), add = TRUE)
      ensure_diary()
      
      updateTextInput(session, "title", value = "")
      updateTextInput(session, "tags", value = "")
      updateSelectInput(session, "mood", selected = "Neutral")
      updateTextAreaInput(session, "body", value = "")
    }, ignoreInit = TRUE)
  })
}