library(shiny)

inventoryTabUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Inventory",
    
    tags$style(HTML({
      root <- ns("root")
      pfx  <- paste0("#", root, " ")
      
      paste0(
        pfx, ".card{border:1px solid rgba(150,120,70,0.55);border-radius:14px;background:rgba(255,255,245,0.85);padding:14px;margin-bottom:14px;box-shadow:0 4px 10px rgba(0,0,0,0.08);}\n",
        pfx, ".card-titlebar{display:flex;align-items:center;justify-content:space-between;margin-bottom:10px;gap:10px;}\n",
        pfx, ".item-card{border:1px solid rgba(150,120,70,0.45);border-radius:14px;background:rgba(255,255,250,0.92);padding:12px;margin-bottom:10px;}\n",
        pfx, ".item-head{display:flex;justify-content:space-between;gap:10px;align-items:flex-start;}\n",
        pfx, ".item-title{font-size:18px;font-weight:700;}\n",
        pfx, ".item-sub{font-size:13px;opacity:.9;margin-top:4px;line-height:1.35;}\n",
        pfx, ".item-actions{display:flex;gap:6px;flex-wrap:wrap;}\n",
        pfx, ".status-bar{display:flex;gap:10px;margin-bottom:10px;flex-wrap:wrap;}\n",
        pfx, ".status-pill{padding:6px 10px;border-radius:999px;border:1px solid rgba(150,120,70,0.4);background:rgba(255,255,245,0.8);}\n"
      )
    })),
    
    div(
      id = ns("root"),
      
      div(
        class = "card",
        
        div(
          class = "card-titlebar",
          h4("🎒 Inventory"),
          actionButton(ns("add_item"), "➕ Add Item")
        ),
        
        div(
          class = "status-bar",
          div(
            class = "status-pill",
            "💰 Gold",
            numericInput(ns("gold"), NULL, value = 0, min = 0, width = "100px")
          ),
          div(
            class = "status-pill",
            textOutput(ns("weight_text"))
          )
        ),
        
        uiOutput(ns("items_active_ui")),
        
        tags$details(
          tags$summary("🧳 Item Bag"),
          uiOutput(ns("items_bag_ui"))
        )
      )
    )
  )
}

inventoryTabServer <- function(id, state, restoring, add_log, char_rev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    safe_log <- function(msg, flash = "none", toast = FALSE) {
      if (is.function(add_log)) {
        tryCatch(
          add_log(msg = msg, flash = flash, toast = toast),
          error = function(e1) {
            tryCatch(add_log(msg), error = function(e2) NULL)
          }
        )
      }
    }
    
    obs_ids <- reactiveVal(character())
    
    uid <- function() {
      paste0("i_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
    }
    
    type_icon <- function(type) {
      switch(
        as.character(type %||% "item"),
        "weapon" = "🗡️",
        "armor"  = "🛡️",
        "blood"  = "🩸",
        "heart"  = "❤️",
        "glyph"  = "🔮",
        "item"   = "🎒",
        "❓"
      )
    }
    
    # ----------------------------
    # Shared inventory access
    # ----------------------------
    get_items <- reactive({
      x <- validate_character(state$char)
      inventory_normalize(x$inventory$items)
    })
    
    set_items <- function(df) {
      x <- validate_character(state$char)
      x$inventory$items <- inventory_normalize(df)
      state$char <- x
    }
    
    sync_gold <- function() {
      x <- validate_character(state$char)
      x$inventory$gold <- suppressWarnings(as.numeric(input$gold %||% 0))
      if (is.na(x$inventory$gold)) x$inventory$gold <- 0
      state$char <- x
    }
    
    load_from_state <- function() {
      x <- validate_character(state$char)
      updateNumericInput(session, "gold", value = x$inventory$gold %||% 0)
    }
    
    # ----------------------------
    # Filters
    # ----------------------------
    general_items <- reactive({
      df <- get_items()
      if (!nrow(df)) return(df)
      
      keep <- !(df$type %in% c("weapon", "armor", "blood", "heart"))
      df <- df[keep, , drop = FALSE]
      
      in_bag <- as.logical(df$in_bag)
      in_bag[is.na(in_bag)] <- FALSE
      df$in_bag <- in_bag
      
      df
    })
    
    # ----------------------------
    # Carry weight
    # ----------------------------
    carry_capacity <- reactive({
      x <- validate_character(state$char)
      str <- x$abilities$str %||% 10
      suppressWarnings(as.numeric(str %||% 10) * 15)
    })
    
    current_weight <- reactive({
      df <- get_items()
      if (!nrow(df)) return(0)
      
      wt <- suppressWarnings(as.numeric(df$weight))
      qty <- suppressWarnings(as.numeric(df$qty))
      
      wt[is.na(wt)] <- 0
      qty[is.na(qty)] <- 0
      
      sum(wt * qty, na.rm = TRUE)
    })
    
    output$weight_text <- renderText({
      paste0("⚖️ ", round(current_weight(), 2), " / ", round(carry_capacity(), 2), " lbs")
    })
    
    # ----------------------------
    # Item card
    # ----------------------------
    item_card <- function(i, mode = "active") {
      p <- paste0("i_", i$id, "_")
      
      if (isTRUE(i$edit)) {
        return(
          div(
            class = "item-card",
            textInput(ns(paste0(p, "name")), "Name", value = i$name %||% ""),
            selectInput(
              ns(paste0(p, "type")),
              "Type",
              choices = c("item", "glyph"),
              selected = if ((i$type %||% "item") %in% c("item", "glyph")) i$type else "item"
            ),
            textAreaInput(ns(paste0(p, "desc")), "Description", value = i$desc %||% ""),
            fluidRow(
              column(4, numericInput(ns(paste0(p, "qty")), "Qty", value = i$qty %||% 1, min = 0, step = 1)),
              column(4, numericInput(ns(paste0(p, "weight")), "Weight", value = i$weight %||% 0, min = 0, step = 0.1)),
              column(4, numericInput(ns(paste0(p, "value")), "Value", value = i$value %||% 0, min = 0, step = 1))
            ),
            div(
              style = "display:flex; gap:8px; flex-wrap:wrap;",
              actionButton(ns(paste0(p, "save")), "💾 Save"),
              actionButton(ns(paste0(p, "cancel")), "↩ Cancel")
            )
          )
        )
      }
      
      div(
        class = "item-card",
        div(
          class = "item-head",
          div(
            div(
              class = "item-title",
              paste0(type_icon(i$type), " ", i$name %||% "Unnamed Item")
            ),
            div(
              class = "item-sub",
              paste0(
                "Type: ", toupper(i$type %||% "item"),
                " • Qty: ", i$qty %||% 1,
                " • ", i$weight %||% 0, " lbs",
                " • ", i$value %||% 0, "g"
              ),
              if (isTRUE(i$equipped)) tags$span(" • EQUIPPED"),
              tags$br(),
              i$desc %||% ""
            )
          ),
          div(
            class = "item-actions",
            actionButton(ns(paste0(p, "edit")), "✎"),
            actionButton(ns(paste0(p, "bag")), if (identical(mode, "active")) "🧳" else "↩︎"),
            actionButton(ns(paste0(p, "del")), "✖")
          )
        )
      )
    }
    
    # ----------------------------
    # UI outputs
    # ----------------------------
    output$items_active_ui <- renderUI({
      df <- general_items()
      if (!nrow(df)) return(tags$em("No items."))
      
      df <- df[!df$in_bag, , drop = FALSE]
      if (!nrow(df)) return(tags$em("No items."))
      
      tagList(lapply(seq_len(nrow(df)), function(i) item_card(df[i, , drop = FALSE], "active")))
    })
    
    output$items_bag_ui <- renderUI({
      df <- general_items()
      if (!nrow(df)) return(tags$em("Bag empty."))
      
      df <- df[df$in_bag, , drop = FALSE]
      if (!nrow(df)) return(tags$em("Bag empty."))
      
      tagList(lapply(seq_len(nrow(df)), function(i) item_card(df[i, , drop = FALSE], "bag")))
    })
    
    # ----------------------------
    # Dynamic row observers
    # ----------------------------
    observeEvent(get_items(), {
      df <- get_items()
      if (!nrow(df)) return()
      
      new_ids <- setdiff(df$id, obs_ids())
      if (!length(new_ids)) return()
      
      for (id in new_ids) {
        local({
          iid <- id
          p <- paste0("i_", iid, "_")
          
          get_row <- function() {
            d <- get_items()
            d[d$id == iid, , drop = FALSE]
          }
          
          set_row <- function(r) {
            d <- get_items()
            idx <- which(d$id == iid)
            if (length(idx) != 1) return()
            d[idx, ] <- r
            set_items(d)
          }
          
          observeEvent(input[[paste0(p, "del")]], {
            d <- get_items()
            d <- d[d$id != iid, , drop = FALSE]
            set_items(d)
            safe_log("🗑️ Item removed.", toast = TRUE)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p, "bag")]], {
            r <- get_row()
            if (nrow(r) != 1) return()
            r$in_bag <- !isTRUE(r$in_bag[[1]])
            set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p, "edit")]], {
            r <- get_row()
            if (nrow(r) != 1) return()
            r$edit <- TRUE
            set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p, "cancel")]], {
            r <- get_row()
            if (nrow(r) != 1) return()
            r$edit <- FALSE
            set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p, "save")]], {
            r <- get_row()
            if (nrow(r) != 1) return()
            
            r$name   <- as.character(input[[paste0(p, "name")]] %||% "Unnamed Item")
            r$type   <- as.character(input[[paste0(p, "type")]] %||% "item")
            r$desc   <- as.character(input[[paste0(p, "desc")]] %||% "")
            r$qty    <- suppressWarnings(as.numeric(input[[paste0(p, "qty")]] %||% 1))
            r$weight <- suppressWarnings(as.numeric(input[[paste0(p, "weight")]] %||% 0))
            r$value  <- suppressWarnings(as.numeric(input[[paste0(p, "value")]] %||% 0))
            r$edit   <- FALSE
            
            if (is.na(r$qty) || r$qty < 0) r$qty <- 1
            if (is.na(r$weight) || r$weight < 0) r$weight <- 0
            if (is.na(r$value) || r$value < 0) r$value <- 0
            
            set_row(r)
            safe_log("💾 Item updated.", toast = TRUE)
          }, ignoreInit = TRUE)
        })
      }
      
      obs_ids(unique(c(obs_ids(), new_ids)))
    }, ignoreInit = TRUE)
    
    # ----------------------------
    # Add item
    # ----------------------------
    observeEvent(input$add_item, {
      if (isTRUE(restoring())) return()
      
      d <- get_items()
      
      new <- data.frame(
        id = uid(),
        name = "New Item",
        type = "item",
        desc = "",
        value = 0,
        weight = 1,
        qty = 1,
        in_bag = FALSE,
        equipped = FALSE,
        meta = I(list(list())),
        edit = TRUE,
        stringsAsFactors = FALSE
      )
      
      set_items(rbind(d, new))
    }, ignoreInit = TRUE)
    
    # ----------------------------
    # Gold sync
    # ----------------------------
    observeEvent(input$gold, {
      if (isTRUE(restoring())) return()
      sync_gold()
    }, ignoreInit = TRUE)
    
    # ----------------------------
    # Character reload
    # ----------------------------
    observeEvent(char_rev(), {
      restoring(TRUE)
      on.exit(restoring(FALSE), add = TRUE)
      
      obs_ids(character())
      load_from_state()
    }, ignoreInit = TRUE)
    
    observeEvent(TRUE, {
      load_from_state()
    }, once = TRUE)
  })
}