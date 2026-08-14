# server/dice_module.R
library(shiny)

diceTabUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Dice",
    value = "dice",
    
    div(
      id = ns("root"),
      class = "card",
      
      h3("🎲 Dice Roller"),
      tags$p(style="opacity:.85;", "Roll anything from a d4 to a d100. Supports advantage/disadvantage and modifiers."),
      
      fluidRow(
        column(
          3,
          selectInput(
            ns("die"),
            "Die",
            choices = c("d4"=4, "d6"=6, "d8"=8, "d10"=10, "d12"=12, "d20"=20, "d100"=100),
            selected = 20
          )
        ),
        column(
          3,
          numericInput(ns("n"), "Number of dice", value = 1, min = 1, max = 50, step = 1)
        ),
        column(
          3,
          numericInput(ns("mod"), "Modifier", value = 0, min = -99, max = 99, step = 1)
        ),
        column(
          3,
          selectInput(
            ns("mode"),
            "Mode",
            choices = c("Normal", "Advantage", "Disadvantage"),
            selected = "Normal"
          )
        )
      ),
      
      div(
        style="display:flex; gap:8px; flex-wrap:wrap; margin: 8px 0 12px 0;",
        actionButton(ns("mod_m5"), "-5", class="btn btn-default"),
        actionButton(ns("mod_m1"), "-1", class="btn btn-default"),
        actionButton(ns("mod_0"),  "0",  class="btn btn-default"),
        actionButton(ns("mod_p1"), "+1", class="btn btn-default"),
        actionButton(ns("mod_p5"), "+5", class="btn btn-default"),
        div(style="flex:1;"),
        actionButton(ns("roll"), "Roll", class="btn btn-primary"),
        actionButton(ns("clear_hist"), "Clear history", class="btn btn-default")
      ),
      
      tags$hr(),
      
      div(
        class = "card",
        h4("Result"),
        uiOutput(ns("result_ui"))
      ),
      
      div(
        class = "card",
        h4("History"),
        div(class="scroll-pane", uiOutput(ns("history_ui")))
      )
    )
  )
}

diceTabServer <- function(id, state, restoring, add_log, char_rev) {
  moduleServer(id, function(input, output, session) {
    
    if (is.null(restoring)) restoring <- reactiveVal(FALSE)
    if (is.null(char_rev))  char_rev  <- reactiveVal(0)
    
    # local history (also logged to core journal via add_log)
    hist <- reactiveVal(data.frame(
      ts = as.POSIXct(character(), tz="UTC"),
      expr = character(),
      detail = character(),
      total = integer(),
      stringsAsFactors = FALSE
    ))
    
    safe_log <- function(msg) {
      if (is.function(add_log)) {
        try(add_log(msg, toast = TRUE), silent = TRUE)
      } else {
        message(msg)
      }
    }
    
    roll_once <- function(sides, n = 1) {
      sample.int(sides, size = n, replace = TRUE)
    }
    
    format_roll <- function(vec) paste(vec, collapse = ", ")
    
    # quick modifier buttons
    observeEvent(input$mod_m5, { updateNumericInput(session, "mod", value = -5) }, ignoreInit = TRUE)
    observeEvent(input$mod_m1, { updateNumericInput(session, "mod", value = -1) }, ignoreInit = TRUE)
    observeEvent(input$mod_0,  { updateNumericInput(session, "mod", value =  0) }, ignoreInit = TRUE)
    observeEvent(input$mod_p1, { updateNumericInput(session, "mod", value =  1) }, ignoreInit = TRUE)
    observeEvent(input$mod_p5, { updateNumericInput(session, "mod", value =  5) }, ignoreInit = TRUE)
    
    # last result storage
    last_result <- reactiveVal(NULL)
    
    observeEvent(input$roll, {
      if (isTRUE(restoring())) return()
      
      sides <- as.integer(input$die %||% 20)
      n     <- as.integer(input$n %||% 1)
      mod   <- as.integer(input$mod %||% 0)
      mode  <- as.character(input$mode %||% "Normal")
      
      n <- max(1, min(50, n))
      sides <- max(2, sides)
      
      # roll
      if (mode %in% c("Advantage", "Disadvantage") && n == 1 && sides == 20) {
        a <- roll_once(20, 1)
        b <- roll_once(20, 1)
        chosen <- if (mode == "Advantage") max(a, b) else min(a, b)
        total <- chosen + mod
        
        expr <- sprintf("1d20 %s %d (%s)", if (mod >= 0) "+" else "-", abs(mod), mode)
        detail <- sprintf("Rolls: [%d, %d] → %d  |  mod %s%d",
                          a, b, chosen, if (mod >= 0) "+" else "-", abs(mod))
        
        res <- list(
          expr = expr,
          rolls = c(a, b),
          chosen = chosen,
          mod = mod,
          total = total,
          mode = mode,
          sides = 20,
          n = 1,
          detail = detail
        )
      } else {
        rolls <- roll_once(sides, n)
        subtotal <- sum(rolls)
        total <- subtotal + mod
        
        expr <- sprintf("%dd%d %s %d", n, sides, if (mod >= 0) "+" else "-", abs(mod))
        detail <- sprintf("Rolls: [%s] → %d  |  mod %s%d",
                          format_roll(rolls), subtotal, if (mod >= 0) "+" else "-", abs(mod))
        
        res <- list(
          expr = expr,
          rolls = rolls,
          chosen = NA_integer_,
          mod = mod,
          total = total,
          mode = "Normal",
          sides = sides,
          n = n,
          detail = detail
        )
      }
      
      last_result(res)
      
      # add to history
      df <- hist()
      df <- rbind(df, data.frame(
        ts = Sys.time(),
        expr = res$expr,
        detail = res$detail,
        total = as.integer(res$total),
        stringsAsFactors = FALSE
      ))
      # keep last 50
      if (nrow(df) > 50) df <- tail(df, 50)
      hist(df)
      
      # log to core (journal/toast)
      safe_log(paste0("🎲 ", res$expr, " → **", res$total, "** (", res$detail, ")"))
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$clear_hist, {
      hist(hist()[0, ])
      last_result(NULL)
      safe_log("🎲 Dice history cleared.")
    }, ignoreInit = TRUE)
    
    # clear module state on character replace (optional)
    observeEvent(char_rev(), {
      # keep history if you want; or clear it:
      # hist(hist()[0, ])
      last_result(NULL)
    }, ignoreInit = TRUE)
    
    output$result_ui <- renderUI({
      res <- last_result()
      if (is.null(res)) {
        return(tags$em("No roll yet."))
      }
      
      tags$div(
        tags$div(style="font-weight:900; font-size:18px;", paste0(res$total)),
        tags$div(style="opacity:.9;", res$expr),
        tags$div(style="margin-top:8px; font-family:monospace; white-space:pre-wrap;", res$detail)
      )
    })
    
    output$history_ui <- renderUI({
      df <- hist()
      if (is.null(df) || nrow(df) == 0) return(tags$em("No history yet."))
      
      tagList(lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, , drop = FALSE]
        ts <- row$ts[[1]]
        ts_txt <- if (!is.na(ts)) format(ts, "%H:%M:%S") else ""
        div(
          class = "weapon-card", # reuse your existing card styling
          div(style="display:flex; justify-content:space-between; gap:10px;",
              div(style="font-weight:900;", paste0(row$expr[[1]], " → ", row$total[[1]])),
              div(style="opacity:.75; font-family:monospace;", ts_txt)
          ),
          div(style="margin-top:6px; font-family:monospace; white-space:pre-wrap; opacity:.9;",
              row$detail[[1]]
          )
        )
      }))
    })
  })
}