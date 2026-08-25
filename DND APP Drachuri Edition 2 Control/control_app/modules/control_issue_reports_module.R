controlIssueReportsUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "control-card",
        div(class = "control-section-title", "Player Reports"),
        p("Errors and feature-upgrade requests submitted from Player Settings, including recent diagnostic logs."),
        div(style = "display:flex;gap:10px;align-items:flex-end;flex-wrap:wrap;",
            selectInput(ns("status"), "Show", choices = c("Open"="open", "Acknowledged"="acknowledged", "Resolved"="resolved", "All"="all"), selected = "open"),
            actionButton(ns("refresh"), "Refresh", class = "btn btn-default")
        ),
        DT::DTOutput(ns("reports"))
    ),
    uiOutput(ns("detail"))
  )
}

controlIssueReportsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reports <- reactiveVal(data.frame())
    known_ids <- reactiveVal(NULL)
    reload <- function(notify = FALSE) {
      rows <- get_player_issue_reports(status = input$status %||% "open")
      previous <- known_ids()
      ids <- if (nrow(rows)) as.integer(rows$id) else integer()
      if (isTRUE(notify) && !is.null(previous)) {
        fresh <- rows[ids %in% setdiff(ids, previous) & rows$status == "open", , drop = FALSE]
        if (nrow(fresh)) for (i in rev(seq_len(nrow(fresh)))) {
          showNotification(
            paste0(fresh$character_name[[i]], " sent a ", if (fresh$category[[i]] == "error") "error report" else "feature request", " (#", fresh$id[[i]], ")."),
            type = if (fresh$category[[i]] == "error") "warning" else "message", duration = 10
          )
        }
      }
      known_ids(ids)
      reports(rows)
    }
    observeEvent(input$status, reload(FALSE), ignoreInit = FALSE)
    observeEvent(input$refresh, reload(FALSE), ignoreInit = TRUE)
    observe({ invalidateLater(5000, session); reload(TRUE) })

    output$reports <- DT::renderDT({
      rows <- reports()
      if (!nrow(rows)) return(DT::datatable(data.frame(Message = "No reports in this view."), options = list(dom = "t"), rownames = FALSE))
      view <- data.frame(
        ID = rows$id,
        Submitted = format(as.POSIXct(rows$created_at), "%Y-%m-%d %H:%M"),
        Player = rows$character_name,
        Type = ifelse(rows$category == "error", "Error", "Feature upgrade"),
        Status = tools::toTitleCase(rows$status),
        Summary = substr(rows$description, 1L, 120L),
        check.names = FALSE
      )
      DT::datatable(view, selection = "single", rownames = FALSE,
                    options = list(pageLength = 12, scrollX = TRUE, order = list(list(0, "desc"))))
    })

    selected <- reactive({
      idx <- input$reports_rows_selected
      rows <- reports()
      if (!length(idx) || !nrow(rows) || idx[[1L]] > nrow(rows)) return(NULL)
      rows[idx[[1L]], , drop = FALSE]
    })
    output$detail <- renderUI({
      row <- selected()
      if (is.null(row)) return(NULL)
      ns <- session$ns
      div(class = "control-card",
          div(class = "control-section-title", paste0("Report #", row$id[[1L]], " — ", row$character_name[[1L]])),
          tags$p(tags$strong("Description")), tags$p(style = "white-space:pre-wrap;", row$description[[1L]]),
          tags$p(class = "control-mini", paste("Session:", row$session_id[[1L]] %||% "none", "• App:", row$app_version[[1L]] %||% "unknown", "•", row$platform[[1L]] %||% "unknown")),
          textAreaInput(ns("dm_notes"), "DM notes", value = row$dm_notes[[1L]] %||% "", rows = 3, width = "100%"),
          div(style = "display:flex;gap:8px;flex-wrap:wrap;",
              actionButton(ns("acknowledge"), "Acknowledge", class = "btn btn-primary"),
              actionButton(ns("resolve"), "Mark Resolved", class = "btn btn-success"),
              actionButton(ns("reopen"), "Reopen", class = "btn btn-default")
          ),
          tags$details(style = "margin-top:14px;", tags$summary("Attached diagnostic log"),
                       tags$pre(style = "max-height:440px;overflow:auto;white-space:pre-wrap;", row$log_text[[1L]] %||% "No log attached."))
      )
    })
    set_status <- function(status) {
      row <- selected(); if (is.null(row)) return()
      ok <- update_player_issue_report(row$id[[1L]], status, input$dm_notes %||% "")
      showNotification(if (ok) "Report updated." else "Report update failed.", type = if (ok) "message" else "error")
      if (ok) reload(FALSE)
    }
    observeEvent(input$acknowledge, set_status("acknowledged"), ignoreInit = TRUE)
    observeEvent(input$resolve, set_status("resolved"), ignoreInit = TRUE)
    observeEvent(input$reopen, set_status("open"), ignoreInit = TRUE)
  })
}
