hudUI <- function(id) {
  ns <- NS(id)
  
  div(
    id = ns("hud_root"),
    class = "global-hud",
    
    div(
      class = "hudbar-inner",
      
      # LEFT GROUP (identity)
      div(
        class = "hud-group hud-left",
        div(class = "hud-name", uiOutput(ns("hud_name"), inline = TRUE)),
        div(class = "hud-level", uiOutput(ns("hud_level"), inline = TRUE)),
        uiOutput(ns("hud_identity"), inline = TRUE)
      ),
      
      # MIDDLE (resources)
      div(
        class = "hud-group hud-center",
        uiOutput(ns("hud_hp_inline"), inline = TRUE),
        uiOutput(ns("hud_magic_inline"), inline = TRUE)
      ),
      
      # STATUS / WORLD
      div(
        class = "hud-group hud-status-wrap",
        uiOutput(ns("hud_status")),
        tags$div(class="hud-time", uiOutput(ns("camp_time"), inline = TRUE))
      ),
      
      
      # RIGHT (numbers)
      div(
        class = "hud-group hud-right",
        uiOutput(ns("hud_gold"), inline = TRUE),
        uiOutput(ns("hud_ac"), inline = TRUE),
        uiOutput(ns("hud_pp"), inline = TRUE)
      )
    )
  )
}

hudServer <- function(id, state, live_snapshot = NULL) {
  moduleServer(id, function(input, output, session) {

    session_hp_poll <- reactive({
      if (!is.function(live_snapshot)) return(data.frame())
      snapshot <- live_snapshot()
      row <- snapshot$self_player %||% data.frame()
      if (!is.data.frame(row)) data.frame() else row
    })
    
    calc_ac <- function(x) {
      dex <- x$abilities$dex %||% 10
      dex_mod <- mod_calc(dex)
      
      arm <- x$combat$armors
      if (!is.data.frame(arm) || nrow(arm) == 0) return(10 + dex_mod)
      
      worn <- arm[isTRUE(arm$worn), , drop = FALSE]
      if (nrow(worn) == 0) return(10 + dex_mod)
      
      if ("base_ac" %in% names(worn)) worn <- worn[order(worn$base_ac, decreasing = TRUE), , drop = FALSE]
      a <- worn[1, , drop = FALSE]
      
      base <- suppressWarnings(as.numeric(a$base_ac[[1]]))
      if (is.na(base)) base <- 10
      
      type <- trimws(as.character(a$type[[1]] %||% ""))
      
      if (identical(type, "Heavy")) {
        dex_add <- 0
      } else if (identical(type, "Medium")) {
        dex_add <- min(2, dex_mod)
      } else if (identical(type, "Custom")) {
        cap <- suppressWarnings(as.numeric(a$custom_max_dex[[1]]))
        if (is.na(cap)) cap <- 0
        dex_add <- min(cap, dex_mod)
      } else {
        dex_add <- dex_mod
      }
      
      as.integer(base + dex_add)
    }
    
    calc_passive <- function(x) {
      as.integer(10 + mod_calc(x$abilities$bld_str %||% 10))
    }

    output$hud_gold <- renderUI({
      x<-validate_character(state$char)
      span(class="hud-stat",paste0("💰 ",as.numeric(x$inventory$gold%||%0)))
    })
    
    get_effective_hp_for_hud <- function(x) {
      x <- validate_character(x)
      
      hp <- x$resources$hp %||% list(max = 0, cur = 0, temp = 0)
      
      max_hp <- as.integer(hp$max %||% 0)
      cur_hp <- as.integer(hp$cur %||% 0)
      tmp_hp <- as.integer(hp$temp %||% 0)
      
      max_hp <- max(0L, max_hp)
      cur_hp <- max(0L, cur_hp)
      tmp_hp <- max(0L, tmp_hp)
      
      row <- session_hp_poll()
      
      if (!is.null(row) && nrow(row) > 0) {
        if ("current_hp" %in% names(row) && !is.na(row$current_hp[1])) {
          cur_hp <- max(0L, as.integer(row$current_hp[1]))
        }
        if ("temp_hp" %in% names(row) && !is.na(row$temp_hp[1])) {
          tmp_hp <- max(0L, as.integer(row$temp_hp[1]))
        }
      }
      
      list(
        max = max_hp,
        cur = cur_hp,
        temp = tmp_hp
      )
    }
    
    output$hud_name <- renderUI({
      x <- validate_character(state$char)
      nm <- x$meta$name %||% ""
      if (!nzchar(nm)) nm <- "Unnamed Character"
      tags$span(nm)
    })
    
    output$hud_level <- renderUI({
      x <- validate_character(state$char)
      lv <- x$build$level %||% 1
      tags$span(paste0("Level ", lv))
    })
    
    output$hud_identity <- renderUI({
      x <- validate_character(state$char)
      
      race <- x$meta$race %||% "—"
      cls  <- x$build$class %||% "—"
      path <- x$build$path %||% "—"
      
      tags$div(
        class = "identity-block",
        tags$div(class = "identity-line", race),
        tags$div(class = "identity-line identity-sub", path)
      )
    })
    
    output$hud_hp_inline <- renderUI({
      x <- validate_character(state$char)
      hp_display <- get_effective_hp_for_hud(x)
      
      max_hp <- hp_display$max
      cur_hp <- hp_display$cur
      tmp_hp <- hp_display$temp
      
      base_pct <- if (max_hp <= 0) 0 else (cur_hp / max_hp) * 100
      temp_pct <- if (max_hp <= 0) 0 else ((cur_hp + tmp_hp) / max_hp) * 100
      
      base_pct <- max(0, min(100, round(base_pct)))
      temp_pct <- max(0, min(100, round(temp_pct)))
      
      label <- if (tmp_hp > 0) {
        sprintf("%d/%d (+%d)", cur_hp, max_hp, tmp_hp)
      } else {
        sprintf("%d/%d", cur_hp, max_hp)
      }
      
      tags$div(
        class = "bar-wrap",
        tags$span(class = "bar-label", "HP"),
        
        tags$div(
          class = "bar",
          tags$div(class = "fill hp", style = sprintf("width:%d%%;", base_pct)),
          if (tmp_hp > 0)
            tags$div(class = "fill hp temp", style = sprintf("width:%d%%;", temp_pct))
        ),
        
        tags$span(class = "bar-label", label)
      )
    })
    
    output$hud_magic_inline <- renderUI({
      x <- validate_character(state$char)
      s <- x$resources$sindre %||% list(cur = 0, total = NA, temp = 0)
      
      cur  <- as.integer(s$cur %||% 0)
      tot  <- as.integer(s$total %||% NA)
      temp <- as.integer(s$temp %||% 0)
      
      cur  <- max(0, cur)
      temp <- max(0, temp)
      
      if (is.na(tot) || tot <= 0) {
        return(
          tags$div(
            class = "bar-wrap",
            tags$span(class = "bar-label", "Sindre"),
            tags$div(class = "bar"),
            tags$span(class = "bar-label", paste0(cur))
          )
        )
      }
      
      base_pct <- (cur / tot) * 100
      temp_pct <- ((cur + temp) / tot) * 100
      
      base_pct <- max(0, min(100, round(base_pct)))
      temp_pct <- max(0, min(100, round(temp_pct)))
      
      label <- if (temp > 0) {
        sprintf("%d/%d (+%d)", cur, tot, temp)
      } else {
        sprintf("%d/%d", cur, tot)
      }
      
      tags$div(
        class = "bar-wrap",
        tags$span(class = "bar-label", "Sindre"),
        
        tags$div(
          class = "bar",
          tags$div(class = "fill magic", style = sprintf("width:%d%%;", base_pct)),
          if (temp > 0)
            tags$div(class = "fill magic temp", style = sprintf("width:%d%%;", temp_pct))
        ),
        
        tags$span(class = "bar-label", label)
      )
    })
    
    output$hud_ac <- renderUI({
      x <- validate_character(state$char)
      ac <- calc_ac(x)
      tags$div(
        class = "pill",
        tags$span(class = "pill-label", "AC"),
        tags$span(as.character(ac))
      )
    })
    
    output$hud_pp <- renderUI({
      x <- validate_character(state$char)
      pp <- calc_passive(x)
      tags$div(
        class = "pill",
        tags$span(class = "pill-label", "Passive"),
        tags$span(as.character(pp))
      )
    })
    
    output$camp_time <- renderUI({
      x <- validate_character(state$char)
      cal <- get_celtic_date(x$meta$day)
      tags$div(sprintf("📅 Year %d • %s • Day %d", cal$year, cal$season, cal$day_of_season))
    })
    
    output$hud_status <- renderUI({
      x <- validate_character(state$char)
      st <- x$status %||% list()
      
      icons <- list()
      
      if (isTRUE(st$prone)) {
        icons <- append(icons, list(span(class = "status-icon", "🛌")))
      }
      
      if (!is.null(st$exhaustion) && st$exhaustion > 0) {
        icons <- append(icons, list(
          span(class = "status-icon", paste0("😵", st$exhaustion))
        ))
      }
      
      if (isFALSE(st$ate_today)) {
        icons <- append(icons, list(span(class = "status-icon", "🍖")))
      }
      
      if (isFALSE(st$drank_today)) {
        icons <- append(icons, list(span(class = "status-icon", "💧")))
      }
      
      if (isTRUE(st$cold)) {
        icons <- append(icons, list(span(class = "status-icon", "❄️")))
      }
      
      if (length(icons) == 0) return(NULL)
      
      tags$div(class = "status-bar", icons)
    })
  })
}
