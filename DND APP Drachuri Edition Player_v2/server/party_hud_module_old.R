# server/party_hud_module.R
library(shiny)

partyHudUI <- function(id) {
  ns <- NS(id)
  root_id <- ns("partyhud_root")
  
  css <- paste0("
    #", root_id, "{
      position: fixed;
      top: 92px;
      right: 0;
      width: 230px;
      z-index: 9998;
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
      margin-right: 8px;
      padding: 3px 8px;
      border-radius: 999px;
      background: rgba(12,10,10,0.70);
      color: rgba(245,230,200,0.95);
      border: 1px solid rgba(191,167,111,0.24);
      font-size: 10px;
      font-weight: 900;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      pointer-events: auto;
    }

    #", root_id, " .partyhud-empty{
      width: 180px;
      padding: 7px 9px;
      border-radius: 10px 0 0 10px;
      border: 1px solid rgba(191,167,111,0.22);
      border-right: none;
      background: rgba(12,10,10,0.74);
      color: rgba(245,230,200,0.88);
      font-size: 10px;
      pointer-events: auto;
    }

    #", root_id, " .party-row{
      width: 100%;
      display: flex;
      justify-content: flex-end;
      pointer-events: none;
    }

    #", root_id, " .party-row-inner{
      display: flex;
      align-items: stretch;
      justify-content: flex-end;
      width: 210px;
      pointer-events: auto;
    }

    #", root_id, " .party-strip{
      width: 188px;
      padding: 6px 7px 6px 8px;
      border-radius: 10px 0 0 10px;
      border: 1px solid rgba(191,167,111,0.22);
      border-right: none;
      background: rgba(12,10,10,0.78);
      color: #f5e6c8;
      overflow: hidden;
      transition: width 160ms ease, opacity 160ms ease, padding 160ms ease;
    }

    #", root_id, " .party-row.collapsed .party-strip{
      width: 0;
      padding-left: 0;
      padding-right: 0;
      border-color: transparent;
      opacity: 0;
      overflow: hidden;
    }

    #", root_id, " .party-toggle{
      width: 22px;
      min-width: 22px;
      padding: 0;
      border-radius: 8px 0 0 8px;
      border: 1px solid rgba(191,167,111,0.24);
      border-right: none;
      background: rgba(18,16,16,0.90);
      color: rgba(245,230,200,0.92);
      font-size: 11px;
      font-weight: 900;
      line-height: 1;
      cursor: pointer;
    }

    #", root_id, " .party-toggle:hover{
      background: rgba(30,26,26,0.95);
    }

    #", root_id, " .party-toggle:focus{
      outline: none;
      box-shadow: none;
    }

    #", root_id, " .party-name{
      font-size: 11px;
      font-weight: 900;
      line-height: 1.05;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      margin-bottom: 2px;
    }

    #", root_id, " .party-sub{
      font-size: 9px;
      opacity: 0.74;
      line-height: 1.05;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      margin-bottom: 5px;
    }

    #", root_id, " .party-status{
      display: flex;
      gap: 3px;
      margin-bottom: 4px;
      min-height: 11px;
    }

    #", root_id, " .party-status-icon{
      font-size: 10px;
      line-height: 1;
    }

    #", root_id, " .party-minirow{
      display: grid;
      grid-template-columns: 18px 1fr 42px;
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
      background: rgba(255,255,255,0.08);
    }

    #", root_id, " .party-fill{
      position: absolute;
      top: 0;
      left: 0;
      height: 100%;
      border-radius: 999px;
    }

    #", root_id, " .party-fill.hp{
      background: linear-gradient(90deg, #8e1f1f, #d66d4a);
      z-index: 1;
    }

    #", root_id, " .party-fill.hp.temp{
      background: linear-gradient(90deg, rgba(220,235,255,0.42), rgba(145,205,255,0.82));
      z-index: 2;
    }

    #", root_id, " .party-fill.magic{
      background: linear-gradient(90deg, #355cbb, #63b3ff);
      z-index: 1;
    }

    #", root_id, " .party-fill.magic.temp{
      background: linear-gradient(90deg, rgba(220,245,255,0.42), rgba(190,250,255,0.84));
      z-index: 2;
    }

    #", root_id, " .party-mini-value{
      font-size: 8px;
      font-weight: 800;
      text-align: right;
      white-space: nowrap;
      opacity: 0.9;
    }

    body.parchment #", root_id, "{
      display: none;
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

partyHudServer <- function(id, state, restoring = NULL, add_log = NULL, char_rev = NULL) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    log_safe <- function(msg, toast = FALSE, flash = "none") {
      if (is.function(add_log)) {
        try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
      } else {
        message(msg)
      }
    }
    
    collapsed_ids <- reactiveVal(character(0))
    refresh_key   <- reactiveVal(0)
    last_toggle_vals <- reactiveVal(list())
    hud_rows <- reactiveVal(data.frame())
    hud_sig  <- reactiveVal("init")
    
    bump_refresh <- function() {
      refresh_key(refresh_key() + 1)
    }
    
    is_collapsed <- function(cid) {
      cid <- as.character(cid %||% "")
      nzchar(cid) && cid %in% (collapsed_ids() %||% character(0))
    }
    
    toggle_collapsed <- function(cid) {
      cid <- as.character(cid %||% "")
      if (!nzchar(cid)) return(invisible(FALSE))
      
      cur <- collapsed_ids() %||% character(0)
      
      if (cid %in% cur) {
        collapsed_ids(setdiff(cur, cid))
      } else {
        collapsed_ids(unique(c(cur, cid)))
      }
      
      invisible(TRUE)
    }
    
    has_live_session <- reactive({
      sid <- state$active_session_id %||% NULL
      cid <- state$char_id %||% NULL
      
      !isTRUE(state$offline_mode) &&
        !is.null(sid) && nzchar(as.character(sid)) &&
        !is.null(cid) && nzchar(as.character(cid))
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
        return(tagList(
          tags$div(class = "party-bar"),
          tags$span(class = "party-mini-value", as.character(cur))
        ))
      }
      
      base_pct <- max(0, min(100, round((cur / maxv) * 100)))
      temp_pct <- max(0, min(100, round(((cur + temp) / maxv) * 100)))
      
      value_txt <- if (temp > 0) {
        sprintf("%d/%d+%d", cur, maxv, temp)
      } else {
        sprintf("%d/%d", cur, maxv)
      }
      
      tagList(
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
        ),
        tags$span(class = "party-mini-value", value_txt)
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
        if (nzchar(class_txt)) class_txt else NULL,
        sep = " "
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
      
      sid <- suppressWarnings(as.integer(state$active_session_id %||% NA))
      my_cid <- as.character(state$char_id %||% "")
      
      if (is.na(sid)) return(data.frame())
      
      rows <- fetch_session_players_safe(sid)
      if (!is.data.frame(rows) || nrow(rows) == 0) return(data.frame())
      
      if ("character_id" %in% names(rows)) {
        rows <- rows[as.character(rows$character_id) != my_cid, , drop = FALSE]
      }
      
      if ("is_active" %in% names(rows)) {
        rows <- rows[rows$is_active %in% TRUE, , drop = FALSE]
      }
      
      rows
    }
    
    build_party_signature <- function(rows) {
      if (!is.data.frame(rows) || nrow(rows) == 0) return("empty")
      
      cols <- intersect(
        c("character_id", "display_name", "current_hp", "temp_hp", "is_active", "updated_at"),
        names(rows)
      )
      
      if (length(cols) == 0) return("no-cols")
      
      paste(apply(rows[, cols, drop = FALSE], 1, function(x) paste(x, collapse = "||")), collapse = "///")
    }
    
    observe({
      invalidateLater(2000, session)
      
      rows <- build_party_rows()
      sig <- build_party_signature(rows)
      
      if (!identical(sig, hud_sig())) {
        hud_sig(sig)
        hud_rows(rows)
        
        cat("PARTY HUD UPDATED\n")
        cat("signature =", sig, "\n")
        cat("rows =", if (is.data.frame(rows)) nrow(rows) else 0, "\n")
      }
    })
    
    observe({
      rows <- hud_rows()
      ids <- character(0)
      
      if (is.data.frame(rows) && nrow(rows) > 0 && "character_id" %in% names(rows)) {
        ids <- unique(as.character(rows$character_id))
        ids <- ids[nzchar(ids)]
      }
      
      current_vals <- last_toggle_vals()
      
      for (cid in ids) {
        btn_id <- paste0("toggle_", cid)
        val <- input[[btn_id]] %||% 0
        old <- current_vals[[btn_id]] %||% 0
        
        if (!identical(val, old) && isTRUE(val > old)) {
          toggle_collapsed(cid)
        }
        
        current_vals[[btn_id]] <- val
      }
      
      current_vals <- current_vals[names(current_vals) %in% paste0("toggle_", ids)]
      last_toggle_vals(current_vals)
    })
    
    observe({
      rows <- hud_rows()
      live_ids <- character(0)
      
      if (is.data.frame(rows) && nrow(rows) > 0 && "character_id" %in% names(rows)) {
        live_ids <- unique(as.character(rows$character_id))
        live_ids <- live_ids[nzchar(live_ids)]
      }
      
      keep <- intersect(collapsed_ids() %||% character(0), live_ids)
      if (!identical(keep, collapsed_ids())) {
        collapsed_ids(keep)
      }
    })
    
    output$party_cards <- renderUI({
      if (is.reactive(char_rev)) {
        try(char_rev(), silent = TRUE)
      }
      
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
        
        cid <- as.character(row$character_id[1] %||% paste0("row_", i))
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
        
        subline <- as.character(extra$subline %||% "")
        if (!nzchar(subline)) {
          lvl_txt <- as.character(extra$level %||% "?")
          class_txt <- as.character(extra$class %||% "")
          subline <- trimws(paste(paste0("Lv ", lvl_txt), class_txt))
        }
        
        status_icons <- extra$status_icons %||% character(0)
        collapsed <- is_collapsed(cid)
        
        tags$div(
          class = paste("party-row", if (collapsed) "collapsed"),
          tags$div(
            class = "party-row-inner",
            tags$div(
              class = "party-strip",
              tags$div(class = "party-name", nm),
              tags$div(class = "party-sub", subline),
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
            ),
            actionButton(
              inputId = session$ns(paste0("toggle_", cid)),
              label = if (collapsed) "◁" else "▷",
              class = "party-toggle"
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