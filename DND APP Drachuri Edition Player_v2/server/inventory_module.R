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
          div(actionButton(ns("trade_item"), "🤝 Trade"), actionButton(ns("add_item"), "➕ Add Item"))
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

inventoryTabServer <- function(id, state, restoring, add_log, char_rev, session_id=NULL, character_id=NULL) {
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
    shown_trade_ids <- reactiveVal(integer())
    skipped_assignment_ids <- reactiveVal(character())
    active_assignment <- reactiveVal(NULL)
    
    uid <- function() {
      paste0("i_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
    }
    current_trade_session <- function(){value<-if(is.function(session_id))session_id() else session_id;suppressWarnings(as.integer(value%||%NA))}
    current_trade_character <- function(){value<-if(is.function(character_id))character_id() else character_id;if(is.null(value)||!length(value)||is.na(value[[1]]))"" else as.character(value[[1]])}

    observe({
      invalidateLater(2500, session)
      cid <- current_trade_character()
      if (!nzchar(cid) || isTRUE(state$offline_mode) || !is.null(active_assignment()) ||
          !is.null(session$userData$pending_trade_id) || !is.null(session$userData$pending_note_id) ||
          !is.null(session$userData$pending_opportunity)) return()
      pending <- pending_equipment_assignments(cid)
      if (!nrow(pending)) return()
      pending <- pending[!pending$instance_id %in% skipped_assignment_ids(), , drop = FALSE]
      if (!nrow(pending)) return()
      equipment <- pending[1L, , drop = FALSE]
      active_assignment(equipment)
      material_text <- if (!is.na(equipment$default_material[[1L]]) && nzchar(equipment$default_material[[1L]])) {
        paste0(equipment$default_material[[1L]], " is fixed by the equipment's construction; only build quality will be rolled.")
      } else "Roll its material and build quality using the campaign drop weights."
      showModal(modalDialog(
        title = paste0("Discover ", equipment$equipment_name[[1L]], "'s make"),
        p("This equipment was migrated from your original character inventory."),
        p(material_text),
        p("The result is permanent, recorded in the equipment history, and travels with the weapon when traded."),
        footer = tagList(
          actionButton(ns("assignment_later"), "Later"),
          actionButton(ns("roll_assignment"), "Roll material & quality", class = "btn btn-primary")
        )
      ))
    })

    observeEvent(input$assignment_later, {
      equipment <- active_assignment(); if (is.null(equipment)) return()
      skipped_assignment_ids(unique(c(skipped_assignment_ids(), as.character(equipment$instance_id[[1L]]))))
      active_assignment(NULL); removeModal()
    }, ignoreInit = TRUE)

    observeEvent(input$roll_assignment, {
      equipment <- active_assignment(); if (is.null(equipment)) return()
      result <- roll_equipment_assignment(current_trade_character(), equipment$instance_id[[1L]])
      if (is.null(result)) {
        showNotification("The equipment roll could not be saved. Please try again.", type = "error")
        return()
      }
      refreshed <- load_character_from_db(current_trade_character())
      if (!is.null(refreshed)) state$char <- validate_character(refreshed)
      active_assignment(NULL); removeModal()
      showNotification(paste0(
        equipment$equipment_name[[1L]], ": ", result$material[[1L]], " — ", result$build_quality[[1L]],
        " (attack ", sprintf("%+g", result$attack_bonus[[1L]]),
        ", damage ", sprintf("%+g", result$damage_modifier[[1L]]), ")"
      ), type = "message", duration = 10)
    }, ignoreInit = TRUE)

    observeEvent(input$trade_item, {
      sid<-current_trade_session();cid<-current_trade_character()
      if(length(sid)!=1L||is.na(sid)||sid<1L||!nzchar(cid)){showNotification("Join a session with another player before trading.",type="warning");return()}
      players<-get_session_players(sid);players<-players[as.character(players$character_id)!=cid,,drop=FALSE]
      if(!nrow(players)){showNotification("Join a session with another player before trading.",type="warning");return()}
      inv<-get_items();item_choices<-if(nrow(inv))setNames(inv$id,paste0(inv$name," · ",inv$type," · qty ",inv$qty)) else character()
      showModal(modalDialog(title="Offer a trade",selectInput(ns("trade_recipient"),"Player",choices=setNames(as.character(players$character_id),as.character(players$char_name))),
        radioButtons(ns("trade_kind"),"Offer",choices=c("Item / weapon / armour"="item","Gold"="gold"),inline=TRUE),uiOutput(ns("trade_value_ui")),
        p("The offer is held safely until the other player accepts or declines."),footer=tagList(modalButton("Cancel"),actionButton(ns("send_trade"),"Send offer",class="btn btn-success"))))
      output$trade_value_ui<-renderUI(if(identical(input$trade_kind,"gold"))numericInput(ns("trade_gold"),"Gold",1,min=1,max=validate_character(state$char)$inventory$gold) else selectInput(ns("trade_item_id"),"Item",choices=item_choices))
    },ignoreInit=TRUE)

    observeEvent(input$send_trade, {
      sid<-current_trade_session();cid<-current_trade_character()
      result<-create_trade_offer(sid,cid,input$trade_recipient,input$trade_kind,item_id=input$trade_item_id,gold_amount=input$trade_gold%||%0L)
      if(is.null(result)){showNotification("Trade could not be created. The item or gold may no longer be available.",type="error");return()}
      state$char<-result$sender;load_from_state();removeModal();showNotification("Trade offer sent.",type="message")
    },ignoreInit=TRUE)

    observe({
      invalidateLater(3000,session);cid<-current_trade_character();if(!nzchar(cid)||isTRUE(state$offline_mode))return()
      external_update<-consume_character_refresh(cid);if(!is.null(external_update)&&is.list(external_update$character)){state$char<-external_update$character;load_from_state();showNotification(paste(unique(external_update$messages),collapse="\n"),type="message",duration=8)}
      sender_update<-consume_trade_sender_update(cid);if(!is.null(sender_update)&&is.list(sender_update$character)){state$char<-sender_update$character;load_from_state();showNotification("A trade was resolved; your inventory and purse have been refreshed.",type="message")}
      if(!is.null(session$userData$pending_merchant_id)||!is.null(session$userData$pending_trade_id)||!is.null(session$userData$pending_note_id)||!is.null(session$userData$pending_opportunity))return()
      offers<-get_pending_trade_offers(cid);if(!nrow(offers))return();fresh<-offers[!offers$id%in%shown_trade_ids(),,drop=FALSE];if(!nrow(fresh))return();o<-fresh[1,,drop=FALSE];shown_trade_ids(unique(c(shown_trade_ids(),o$id)))
      summary<-enemy_db_json(o$summary[[1]],list());meta<-summary$meta%||%list();stat_line<-if(identical(summary$type,"weapon"))paste0("\nDamage: ",meta$damage1%||%"—"," ",meta$dmg_type1%||%""," · uses ",toupper(meta$stat%||%"str")," · material ",meta$material%||%"standard") else if(identical(summary$type,"armor"))paste0("\nArmour: AC ",meta$base_ac%||%"—"," · ",meta$type%||%"Armour") else ""
      details<-if(o$offer_kind[[1]]=="gold")paste0(o$gold_amount[[1]]," gold") else paste0(summary$name%||%"Item"," (",summary$type%||%"item",") · qty ",summary$qty%||%1," · ",summary$weight%||%0," lb · value ",summary$value%||%0,"g",stat_line,"\n",summary$desc%||%"")
      showModal(modalDialog(title=paste0(o$sender_name[[1]]%||%"Another player"," would like to trade"),tags$pre(style="white-space:pre-wrap",details),p("Accept this trade?"),footer=tagList(actionButton(ns("decline_trade"),"Decline",class="btn btn-default"),actionButton(ns("accept_trade"),"Accept",class="btn btn-success"))))
      session$userData$pending_trade_id<-o$id[[1]]
    })

    resolve_visible_trade <- function(accept){id<-session$userData$pending_trade_id;if(is.null(id))return();result<-resolve_trade_offer(id,current_trade_character(),accept);removeModal();session$userData$pending_trade_id<-NULL;if(is.null(result)){showNotification("Trade is no longer available.",type="error");return()};if(isTRUE(accept)){state$char<-result$character;load_from_state()};showNotification(if(accept)"Trade accepted and added to your character." else "Trade declined; it was returned to the sender.",type="message")}
    observeEvent(input$accept_trade,resolve_visible_trade(TRUE),ignoreInit=TRUE)
    observeEvent(input$decline_trade,resolve_visible_trade(FALSE),ignoreInit=TRUE)
    
    type_icon <- function(type) {
      switch(
        as.character(type %||% "item"),
        "weapon" = "🗡️",
        "armor"  = "🛡️",
        "consumable" = "🍎",
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
      freezeReactiveValue(input, "gold")
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
              if(identical(as.character((i$meta%||%list())$category%||%""),"food"))tags$span(paste0(" • ",(i$meta%||%list())$food_rations_remaining%||%((i$meta%||%list())$ration_value%||%1)*(i$qty%||%1)," ration(s) • fresh through day ",(i$meta%||%list())$fresh_until_day%||%((validate_character(state$char)$meta$day%||%1)+((i$meta%||%list())$shelf_life_days%||%3)))),
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
