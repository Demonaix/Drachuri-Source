# server/party_hud_module.R
library(shiny)

partyHudUI <- function(id) {
  ns <- NS(id)
  root_id <- ns("partyhud_root")
  
  css <- paste0("
#", root_id, "{
  position: fixed;
  top: 110px;
  left: 0px;
  width: 108px;
  z-index: 10000;
  pointer-events: none;
}

    #", root_id, " .partyhud-shell{
      display: flex;
      flex-direction: column;
      align-items: flex-end;
      gap: 6px;
      width: 100%;
    }

    #", root_id, " .partyhud-label{
      align-self: flex-end;
      margin-right: 2px;
      padding: 3px 8px;
      border-radius: 999px;
      background: rgba(255,255,245,0.90);
      color: #3e2f1c;
      border: 1px solid rgba(191,167,111,0.82);
      font-size: 10px;
      font-weight: 900;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      box-shadow: 0 8px 20px rgba(0,0,0,0.22);
      pointer-events: auto;
    }

    #", root_id, " .partyhud-empty{
      width: 96px;
      padding: 7px 9px;
      border-radius: 10px;
      border: 1px solid rgba(191,167,111,0.82);
      background: rgba(255,255,245,0.90);
      color: #3e2f1c;
      font-size: 10px;
      box-shadow: 0 8px 20px rgba(0,0,0,0.18);
      pointer-events: auto;
    }

    #", root_id, " .party-row{
      width: 96px;
      pointer-events: none;
    }

    #", root_id, " .party-strip{
      width: 100%;
     padding: 5px 6px;
      border-radius: 10px;
      border: 1px solid rgba(191,167,111,0.82);
      background: rgba(255,255,245,0.90);
      color: #3e2f1c;
      overflow: hidden;
      box-shadow: 0 8px 20px rgba(0,0,0,0.18);
      pointer-events: auto;
    }

    #", root_id, " .party-name{
      font-size: 10px;
      font-weight: 900;
      line-height: 1.05;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      margin-bottom: 2px;
    }

    #", root_id, " .party-sub{
      font-size: 7px;
      opacity: 0.80;
      line-height: 1.05;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      margin-bottom: 4px;
      text-transform: uppercase;
    }
    
    #", root_id, " .party-meta{
  font-size: 7px;
  opacity: 0.80;
  line-height: 1.05;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  text-transform: uppercase;
}

#", root_id, " .party-meta + .party-meta{
  margin-top: 1px;
}

#", root_id, " .party-meta-wrap{
  margin-bottom: 4px;
}

    #", root_id, " .party-status{
      display: flex;
      gap: 3px;
      margin-bottom: 4px;
      min-height: 10px;
    }

    #", root_id, " .party-status-icon{
      font-size: 9px;
      line-height: 1;
    }

    #", root_id, " .party-minirow{
      display: grid;
grid-template-columns: 10px 1fr;
      gap: 5px;
      align-items: center;
    }

    #", root_id, " .party-minirow + .party-minirow{
      margin-top: 3px;
    }

    #", root_id, " .party-mini-label{
      font-size: 8px;
      font-weight: 900;
      text-transform: uppercase;
      opacity: 0.82;
    }

    #", root_id, " .party-bar{
      position: relative;
      height: 7px;
      border-radius: 999px;
      overflow: hidden;
      background: rgba(40,40,40,0.14);
      border: 1px solid rgba(191,167,111,0.32);
    }

    #", root_id, " .party-fill{
      position: absolute;
      top: 0;
      left: 0;
      height: 100%;
      border-radius: 999px;
    }

    #", root_id, " .party-fill.hp{
      background: linear-gradient(90deg, rgba(170,35,35,0.92), rgba(240,120,60,0.90));
      z-index: 1;
    }

    #", root_id, " .party-fill.hp.temp{
      background: linear-gradient(90deg, rgba(220,235,255,0.42), rgba(145,205,255,0.82));
      z-index: 2;
    }

    #", root_id, " .party-fill.magic{
      background: linear-gradient(90deg, rgba(40,80,190,0.92), rgba(100,190,255,0.92));
      z-index: 1;
    }

    #", root_id, " .party-fill.magic.temp{
      background: linear-gradient(90deg, rgba(220,245,255,0.42), rgba(190,250,255,0.84));
      z-index: 2;
    }

  ")
  
  tagList(
    tags$style(HTML(css)),
    div(
      id = root_id,
      div(
        class = "partyhud-shell",
        uiOutput(ns("party_cards"))
      )
    )
  )
}

partyHudServer <- function(id, state, restoring = NULL, add_log = NULL,
                           char_rev = NULL, live_snapshot = NULL) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    log_safe <- function(msg, toast = FALSE, flash = "none") {
      if (is.function(add_log)) {
        try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
      } else {
        message(msg)
      }
    }
    
    hud_rows <- reactiveVal(data.frame())
    hud_sig  <- reactiveVal("init")
    
    resolved_session_id <- reactive({
      sid <- suppressWarnings(as.integer(state$active_session_id %||% NA))
      if (!is.na(sid) && sid > 0) return(sid)
      
      eid <- suppressWarnings(as.integer(state$active_encounter_id %||% NA))
      if (!is.na(eid) && eid > 0) {
        enc <- tryCatch(get_encounter(eid), error = function(e) data.frame())
        if (is.data.frame(enc) && nrow(enc) > 0) {
          sid2 <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
          if (!is.na(sid2) && sid2 > 0) return(sid2)
        }
      }
      
      NA_integer_
    })
    
    has_live_session <- reactive({
      cid <- as.character(state$char_id %||% "")
      sid <- resolved_session_id()
      
      !isTRUE(state$offline_mode) &&
        !is.na(sid) &&
        nzchar(cid)
    })
 
    
    mini_bar_ui <- function(cur, maxv, temp = 0, cls = c("hp", "magic")) {
      cls <- match.arg(cls)
      
      cur  <- suppressWarnings(as.integer(cur %||% 0))
      maxv <- suppressWarnings(as.integer(maxv %||% 0))
      temp <- suppressWarnings(as.integer(temp %||% 0))
      
      if (is.na(cur)) cur <- 0L
      if (is.na(maxv)) maxv <- 0L
      if (is.na(temp)) temp <- 0L
      
      cur  <- max(0L, cur)
      maxv <- max(0L, maxv)
      temp <- max(0L, temp)
      
      if (maxv <= 0L) {
        return(tags$div(class = "party-bar"))
      }
      
      base_pct <- max(0, min(100, round((cur / maxv) * 100)))
      temp_pct <- max(0, min(100, round(((cur + temp) / maxv) * 100)))
      
      tags$div(
        class = "party-bar",
        tags$div(
          class = paste("party-fill", cls),
          style = paste0("width:", base_pct, "%;")
        ),
        if (temp > 0) {
          tags$div(
            class = paste("party-fill", cls, "temp"),
            style = paste0("width:", temp_pct, "%;")
          )
        }
      )
    }
    
    fetch_session_players_safe <- function(session_id) {
      if (isTRUE(state$offline_mode)) return(data.frame())
      
      tryCatch(
        get_session_players(session_id),
        error = function(e) {
          log_safe(paste("Party HUD read failed:", e$message), toast = FALSE)
          data.frame()
        }
      )
    }
    
    fetch_character_summary_safe <- function(character_id) {
      ch <- tryCatch(
        load_character_from_db(character_id),
        error = function(e) NULL
      )
      
      if (is.null(ch)) return(NULL)
      
      ch <- tryCatch(validate_character(ch), error = function(e) NULL)
      if (is.null(ch)) return(NULL)
      
      effects <- ch$status$effects %||% character(0)
      if (isTRUE(ch$status$bloodlust)) {
        effects <- c(effects, "Bloodlust")
      }
      
      status_icons <- character(0)
      
      if (any(grepl("^Exhaustion", effects))) status_icons <- c(status_icons, "😵")
      if ("Bloodlust" %in% effects)          status_icons <- c(status_icons, "🩸")
      if ("Poisoned" %in% effects)           status_icons <- c(status_icons, "☠️")
      if ("Blinded" %in% effects)            status_icons <- c(status_icons, "🙈")
      if ("Restrained" %in% effects)         status_icons <- c(status_icons, "🪢")
      if ("Stunned" %in% effects)            status_icons <- c(status_icons, "💫")
      if ("Unconscious" %in% effects)        status_icons <- c(status_icons, "💤")
      
      race_txt  <- as.character(ch$meta$race %||% "")
      class_txt <- as.character(ch$build$class %||% "")
      lvl_txt   <- as.character(ch$build$level %||% "?")
      
      sub_txt <- paste(
        paste0("Lv ", lvl_txt),
        if (nzchar(race_txt)) race_txt else NULL,
        if (nzchar(class_txt)) class_txt else NULL
      )
      
      list(
        race = if (nzchar(race_txt)) race_txt else "—",
        class = if (nzchar(class_txt)) class_txt else "—",
        level = lvl_txt,
        subline = trimws(sub_txt),
        hp_max = as.integer(ch$resources$hp$max %||% 1),
        sindre_cur = as.integer(ch$resources$sindre$cur %||% 0),
        sindre_max = as.integer(ch$resources$sindre$total %||% 0),
        sindre_temp = as.integer(ch$resources$sindre$temp %||% 0),
        status_icons = unique(status_icons)
      )
    }
    
    build_party_rows <- function() {
      if (isTRUE(state$offline_mode)) return(data.frame())
      if (!isTRUE(has_live_session())) return(data.frame())
      
      sid <- resolved_session_id()
      my_cid <- as.character(state$char_id %||% "")
      
      if (is.na(sid)) return(data.frame())
      
      if (is.function(live_snapshot)) {
        snapshot <- live_snapshot()
        rows <- snapshot$players %||% data.frame()
      } else {
        rows <- fetch_session_players_safe(sid)
      }
      if (!is.data.frame(rows) || nrow(rows) == 0) return(data.frame())
      
      if ("character_id" %in% names(rows)) {
        rows <- rows[as.character(rows$character_id) != my_cid, , drop = FALSE]
      }
      
      if ("is_active" %in% names(rows)) {
        rows <- rows[rows$is_active %in% TRUE, , drop = FALSE]
      }
      
      rows
    }
    
    
    observe({
      cat("\n[PARTY HUD]\n")
      cat("char_id:", as.character(state$char_id %||% "NULL"), "\n")
      cat("active_session_id:", as.character(state$active_session_id %||% "NULL"), "\n")
      cat("active_encounter_id:", as.character(state$active_encounter_id %||% "NULL"), "\n")
      cat("resolved_session_id:", as.character(resolved_session_id() %||% NA), "\n")
      cat("offline_mode:", as.character(state$offline_mode %||% FALSE), "\n")
    })
    
    build_party_signature <- function(rows) {
      if (!is.data.frame(rows) || nrow(rows) == 0) return("empty")
      
      cols <- intersect(
        c("character_id", "display_name", "current_hp", "temp_hp", "is_active", "updated_at"),
        names(rows)
      )
      
      if (length(cols) == 0) return("no-cols")
      
      paste(
        apply(rows[, cols, drop = FALSE], 1, function(x) paste(x, collapse = "||")),
        collapse = "///"
      )
    }
    
    observe({
      rows <- build_party_rows()
      sig <- build_party_signature(rows)
      
      if (!identical(sig, hud_sig())) {
        hud_sig(sig)
        hud_rows(rows)
      }
    })
    
    output$party_cards <- renderUI({
      if (isTRUE(state$offline_mode)) {
        return(NULL)
      }
      
      if (!has_live_session()) {
        return(NULL)
      }
      
      rows <- hud_rows()
      
      if (!is.data.frame(rows) || nrow(rows) == 0) {
        return(
          tagList(
            tags$div(class = "partyhud-label", "Party"),
            tags$div(class = "partyhud-empty", "No other active players.")
          )
        )
      }
      
      strips <- lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, , drop = FALSE]
        
        nm  <- as.character(row$display_name[1] %||% "Unknown")
        cur_hp  <- as.integer(row$current_hp[1] %||% 0)
        temp_hp <- as.integer(row$temp_hp[1] %||% 0)
        
        if (is.na(cur_hp)) cur_hp <- 0L
        if (is.na(temp_hp)) temp_hp <- 0L
        
        extra <- NULL
        if ("character_id" %in% names(row)) {
          extra <- fetch_character_summary_safe(as.character(row$character_id[1]))
        }
        
        hp_max <- suppressWarnings(as.integer(extra$hp_max %||% NA))
        if (is.na(hp_max) || hp_max <= 0L) hp_max <- max(cur_hp, 1L)
        
        sindre_cur  <- suppressWarnings(as.integer(extra$sindre_cur %||% 0))
        sindre_max  <- suppressWarnings(as.integer(extra$sindre_max %||% 0))
        sindre_temp <- suppressWarnings(as.integer(extra$sindre_temp %||% 0))
        
        if (is.na(sindre_cur))  sindre_cur <- 0L
        if (is.na(sindre_max))  sindre_max <- 0L
        if (is.na(sindre_temp)) sindre_temp <- 0L
        
        race_txt <- as.character(extra$race %||% "")
        class_txt <- as.character(extra$class %||% "")
        
        if (!nzchar(race_txt))  race_txt  <- "—"
        if (!nzchar(class_txt)) class_txt <- "—"
        
        status_icons <- extra$status_icons %||% character(0)
        
        tags$div(
          class = "party-row",
          tags$div(
            class = "party-strip",
            tags$div(class = "party-name", nm),
            tags$div(
              class = "party-meta-wrap",
              tags$div(class = "party-meta", race_txt),
              tags$div(class = "party-meta", class_txt)
            ),
            tags$div(
              class = "party-status",
              if (length(status_icons) > 0) {
                lapply(status_icons, function(ic) {
                  tags$span(class = "party-status-icon", ic)
                })
              }
            ),
            tags$div(
              class = "party-minirow",
              tags$span(class = "party-mini-label", "HP"),
              mini_bar_ui(cur_hp, hp_max, temp_hp, "hp")
            ),
            tags$div(
              class = "party-minirow",
              tags$span(class = "party-mini-label", "SI"),
              mini_bar_ui(sindre_cur, sindre_max, sindre_temp, "magic")
            )
          )
        )
      })
      
      tagList(
        tags$div(class = "partyhud-label", "Party"),
        strips
      )
    })
  })
}
