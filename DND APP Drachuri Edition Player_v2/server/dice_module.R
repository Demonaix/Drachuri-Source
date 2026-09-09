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
      tags$style(HTML(".dice-result-cards{display:flex;flex-wrap:wrap;justify-content:center;align-items:center;gap:12px;margin:10px 0 18px}.dice-result-card{position:relative;width:min(150px,28vw);aspect-ratio:1/1;border-radius:10px;overflow:hidden;box-shadow:0 5px 14px #2d1d0c55;opacity:.82;transition:.16s}.dice-result-card.is-selected{opacity:1;outline:4px solid #b88b2d;transform:translateY(-5px)}.dice-result-card img{width:100%;height:100%;object-fit:contain}.dice-result-label{position:absolute;left:8px;right:8px;bottom:8px;padding:4px;background:#fff4d9dd;border-radius:6px;text-align:center;font-weight:800}.dice-result-fallback{display:flex;flex-direction:column;align-items:center;justify-content:center;background:#ead8aa;color:#3e2f1c}.dice-result-fallback strong{font-size:42px}.dice-modifier-card{width:92px;height:122px;display:flex;flex-direction:column;align-items:center;justify-content:center;background:linear-gradient(145deg,#f1dfae,#c8a564);border:3px double #725020;border-radius:10px;box-shadow:0 5px 14px #2d1d0c55}.dice-modifier-sign{font:900 34px Cinzel,Georgia,serif}.dice-modifier-label{font:800 10px Cinzel,Georgia,serif;text-transform:uppercase}")),
      
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
    pending_manual_roll <- reactiveVal(NULL)

    manual_mode <- function() isTRUE(session$rootScope()$input$manual_roll_mode_enabled)

    finish_roll <- function(sides, n, mod, mode, supplied_rolls = NULL) {
      roll_once_or_use <- function(count) {
        if (!is.null(supplied_rolls)) as.integer(supplied_rolls) else roll_once(sides, count)
      }
    
      if (mode %in% c("Advantage", "Disadvantage") && n == 1 && sides == 20) {
        pair <- roll_once_or_use(2L); a <- pair[[1L]]; b <- pair[[2L]]
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
        rolls <- roll_once_or_use(n)
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
    }

    observeEvent(input$roll, {
      if (isTRUE(restoring())) return()
      sides <- max(2L, as.integer(input$die %||% 20L)); n <- max(1L, min(50L, as.integer(input$n %||% 1L)))
      mod <- as.integer(input$mod %||% 0L); mode <- as.character(input$mode %||% "Normal")
      count <- if (mode %in% c("Advantage", "Disadvantage") && n == 1L && sides == 20L) 2L else n
      if (!manual_mode()) return(finish_roll(sides, n, mod, mode))
      pending_manual_roll(list(sides=sides,n=n,mod=mod,mode=mode,count=count))
      showModal(modalDialog(title="Roll your physical dice",
        p(sprintf("Roll %dd%d%s, then enter %s below.",count,sides,if(mode=="Normal")""else paste0(" for ",tolower(mode)),if(count==1L)"the natural result"else"the results separated by spaces or commas")),
        textInput(session$ns("manual_dice_results"),"Natural dice result",placeholder=if(count==1L)paste0("1–",sides)else paste(rep(paste0("1–",sides),count),collapse=", ")),
        footer=tagList(modalButton("Cancel"),actionButton(session$ns("confirm_manual_dice"),"Use Result",class="btn btn-primary")),easyClose=FALSE))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_manual_dice, {
      req <- pending_manual_roll(); if(is.null(req)) return()
      values <- manual_dice_values(input$manual_dice_results,req$sides,req$count)
      if(is.null(values)) return(showNotification(sprintf("Enter exactly %d result%s between 1 and %d.",req$count,if(req$count==1L)""else"s",req$sides),type="error"))
      pending_manual_roll(NULL); removeModal(); finish_roll(req$sides,req$n,req$mod,req$mode,values)
    },ignoreInit=TRUE)
    
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
        tags$div(class="dice-result-cards",lapply(seq_along(res$rolls),function(i)dice_result_card_ui(res$sides,res$rolls[[i]],!is.na(res$chosen)&&res$rolls[[i]]==res$chosen,if(!is.na(res$chosen))if(res$rolls[[i]]==res$chosen)"Used"else"Discarded"else NULL)),if(as.integer(res$mod%||%0L)!=0L)modifier_result_card_ui(res$mod)),
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
