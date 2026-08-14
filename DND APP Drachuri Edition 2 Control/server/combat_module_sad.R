# server/combat_module_ui.R
library(shiny)


# To be added bite attack on nat 1
# if (x$status$bloodlust && roll == 1) {
#trigger_bite_attack()
#}

# Combat UI only (server lives in server/combat_module.R)
combatTabUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Combat",
    
    # Module-local styles (keeps main UI clean)
    # Replace the whole tags$style(HTML(sprintf(" ... ", ... ))) with:
    
    tags$style(HTML({
      root <- ns("root")
      pfx  <- paste0("#", root, " ")
      
      paste0(
        "/* Cards */\n",
        pfx, ".card{border:1px solid rgba(150,120,70,0.55);border-radius:14px;background:rgba(255,255,245,0.85);padding:14px;margin-bottom:14px;box-shadow:0 4px 10px rgba(0,0,0,0.08);}\n",
        pfx, ".card-titlebar{display:flex;align-items:center;justify-content:space-between;gap:10px;margin-bottom:10px;}\n",
        pfx, ".btn-xs{width:34px;height:34px;padding:0!important;line-height:34px!important;border-radius:10px;}\n",
        pfx, ".btn-pill{height:30px;padding:0 10px!important;line-height:28px!important;border-radius:999px;font-size:12px;}\n",
        
        "/* Item cards */\n",
        pfx, ".item-card{border:1px solid rgba(150,120,70,0.45);border-radius:14px;background:rgba(255,255,250,0.92);padding:12px;margin-bottom:10px;}\n",
        pfx, ".item-head{display:flex;gap:10px;align-items:center;justify-content:space-between;}\n",
        pfx, ".item-title{font-size:18px;font-weight:700;}\n",
        pfx, ".item-sub{font-size:13px;opacity:.9;line-height:1.25;margin-top:2px;}\n",
        pfx, ".item-actions{display:flex;gap:6px;align-items:center;flex-wrap:wrap;justify-content:flex-end;}\n",
        pfx, ".item-body{margin-top:10px;}\n",
        pfx, ".item-card .form-group{margin-bottom:8px!important;}\n",
        pfx, ".item-card .form-control{height:34px;padding:4px 8px;font-size:14px;border-radius:10px;}\n",
        pfx, ".item-card .checkbox{margin:0!important;}\n",
        pfx, ".item-card input[type='checkbox']{transform:scale(1.1);}\n",
        
        "/* Toggle row */\n",
        pfx, ".toggles{display:flex;gap:8px;flex-wrap:wrap;margin-top:6px;}\n",
        pfx, ".toggle-chip{display:inline-flex;align-items:center;gap:6px;padding:4px 10px;border-radius:999px;border:1px solid rgba(150,120,70,0.35);background:rgba(255,255,245,0.75);font-size:12px;}\n",
        pfx, ".toggle-chip.on{font-weight:700;}\n",
        pfx, ".toggle-chip.off{opacity:.75;}\n",
        
        "/* Bag */\n",
        pfx, "details.bag summary{cursor:pointer;font-weight:700;margin-top:6px;opacity:.95;}\n",
        
        "/* Status pills */\n",
        pfx, ".status-bar{display:flex;gap:8px;flex-wrap:wrap;align-items:center;}\n",
        pfx, ".status-pill{display:inline-flex;align-items:center;gap:8px;padding:6px 10px;border-radius:999px;border:1px solid rgba(150,120,70,0.4);background:rgba(255,255,245,0.8);font-size:13px;}\n",
        pfx, ".status-pill .rm{width:22px;height:22px;line-height:20px;text-align:center;border-radius:999px;border:1px solid rgba(180,60,60,0.35);background:rgba(255,230,230,0.8);cursor:pointer;font-weight:700;}\n",
        
        "/* HP bar + hearts */\n",
        pfx, ".hp-wrap{display:flex;flex-direction:column;gap:10px;}\n",
        pfx, ".hp-bar{width:100%;height:18px;border-radius:999px;background:rgba(120,120,120,0.18);overflow:hidden;border:1px solid rgba(150,120,70,0.35);}\n",
        pfx, ".hp-bar>div{height:100%;background:linear-gradient(90deg,rgba(180,40,40,0.8),rgba(240,120,60,0.85));width:0%;}\n",
        pfx, ".hp-label{display:flex;justify-content:space-between;font-size:13px;opacity:.9;}\n",
        pfx, ".hearts{display:flex;flex-wrap:wrap;gap:4px;}\n",
        pfx, ".heart{display:inline-block;width:16px;height:16px;font-size:16px;line-height:16px;}\n",
        
        "@media (max-width:768px){",
        pfx, ".item-actions{justify-content:flex-start;}",
        "}\n"
      )
    })),
    
    div(id = ns("root"),
        br(),
        # -------- Weapons --------
        div(class = "card",
            div(class = "card-titlebar",
                h4("🗡️ Weapons"),
                actionButton(ns("add_weapon"), "➕ Add Weapon", class = "btn-sm")
            ),
            uiOutput(ns("weapons_active_ui")),
            tags$details(class="bag",
                         tags$summary("🧳 Weapons Bag"),
                         uiOutput(ns("weapons_bag_ui"))
            )
        ),
        
        # -------- Armor --------
        div(class = "card",
            div(class = "card-titlebar",
                h4("🛡️ Armor"),
                actionButton(ns("add_armor"), "➕ Add Armor", class = "btn-sm")
            ),
            uiOutput(ns("armor_active_ui")),
            tags$details(class="bag",
                         tags$summary("🧳 Armor Bag"),
                         uiOutput(ns("armor_bag_ui"))
            )
        ),
        
        # -------- Status Effects --------
        div(class = "card",
            div(class = "card-titlebar",
                h4("✨ Status Effects"),
                div(
                  style="display:flex; gap:8px; align-items:center;",
                  selectInput(ns("status_add"), NULL, choices = NULL, selected = NULL, width = "220px"),
                  actionButton(ns("status_add_btn"), "Add", class = "btn-pill"),
                  actionButton(ns("status_clear"), "Clear", class = "btn-pill btn-warning")
                )
            ),
            uiOutput(ns("status_pills_ui"))
        ),
        verbatimTextOutput("session_debug"),
        # -------- Defenses --------
        div(class = "card",
            h4("❤️ Defenses"),
            div(class="hp-wrap",
                uiOutput(ns("hp_ui")),
                fluidRow(
                  column(4, numericInput(ns("max_hp"), "Max HP", value = 10, min = 0)),
                  column(4, numericInput(ns("current_hp"), "Current HP", value = 10, min = 0)),
                  column(4, numericInput(ns("temp_hp"), "Temp HP", value = 0, min = 0))
                )
            ),
            
            tags$hr(),
            
            fluidRow(
              column(4, checkboxInput(ns("ac_manual"), "Manual AC?", value = FALSE)),
              column(4, conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("ac_manual")),
                numericInput(ns("ac_manual_value"), "AC (Manual)", value = 10, min = 0)
              )),
              column(4, conditionalPanel(
                condition = sprintf("input['%s'] == false", ns("ac_manual")),
                h5("AC (Auto)"), textOutput(ns("auto_ac"))
              ))
            ),
            
            tags$hr(),
            
            fluidRow(
              column(4, numericInput(ns("damage_amt"), "Amount", value = 0, min = 0)),
              column(4, br(), actionButton(ns("apply_damage"), "💢 Take Damage", class="btn-danger")),
              column(4, br(), actionButton(ns("heal_damage"), "✨ Heal", class="btn-success"))
            )
        )
    )
  )
}

# server/combat_module.R
library(shiny)

# Combat server module.
# Requires:
# - %||% helper (from your library.R)
# - mod_calc()
# - validate_character()
# - constants in plug (optional but recommended):
#   COMBAT_ARMOR_TYPES, COMBAT_WEAPON_STATS, COMBAT_DMG_TYPES
#
# UI is expected from: server/combat_module_ui.R

combatTabServer <- function(id, state, restoring, add_log, char_rev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ----------------------------
    # Safe logger (core can evolve)
    # ----------------------------
    safe_log <- function(msg, flash = "none", toast = FALSE) {
      # Try richer signature first; fall back to msg-only
      tryCatch(
        add_log(msg = msg, flash = flash, toast = toast),
        error = function(e1) {
          tryCatch(add_log(msg), error = function(e2) NULL)
        }
      )
    }
    
    # ----------------------------
    # Fallback constants
    # ----------------------------
    if (!exists("COMBAT_ARMOR_TYPES", inherits = TRUE)) {
      COMBAT_ARMOR_TYPES <- c("Light", "Medium", "Heavy", "Custom")
    }
    if (!exists("COMBAT_WEAPON_STATS", inherits = TRUE)) {
      COMBAT_WEAPON_STATS <- c("str", "dex", "con", "int", "bld_str", "cha")
    }
    if (!exists("COMBAT_DMG_TYPES", inherits = TRUE)) {
      COMBAT_DMG_TYPES <- c("Slashing","Piercing","Bludgeoning","Fire","Cold","Lightning",
                            "Poison","Acid","Necrotic","Radiant","Psychic","Thunder","Force","Other")
    }
    
    # ----------------------------
    # Helpers
    # ----------------------------
    
    get_inventory <- reactive({
      state$char$inventory$items %||% data.frame()
    })
    
    get_items_by_type <- function(type, active_only = FALSE) {
      df <- get_inventory()
      df <- df[df$type == type, , drop = FALSE]
      
      if (active_only) {
        df <- df[!isTRUE(df$in_bag), , drop = FALSE]
      }
      
      df
    }
    
    prof_bonus <- reactive({
      lvl <- state$char$build$level %||% 1
      ceiling(lvl / 4) + 1
    })
    
    ability_mod <- function(stat) {
      mod_calc(state$char$abilities[[stat]] %||% 10)
    }
    
    parse_damage_expr <- function(expr) {
      cleaned <- gsub("[()\\s]", "", expr %||% "")
      m <- regexec("^([0-9]+)d([0-9]+)([+-][0-9]+)?$", cleaned)
      r <- regmatches(cleaned, m)[[1]]
      if (length(r) < 3) return(NULL)
      list(
        n = as.integer(r[2]),
        die = as.integer(r[3]),
        flat = if (length(r) >= 4 && nzchar(r[4])) as.integer(r[4]) else 0
      )
    }
    
    roll_damage <- function(expr) {
      p <- parse_damage_expr(expr)
      if (is.null(p) || p$n <= 0 || p$die <= 0) return(NULL)
      rolls <- sample(1:p$die, p$n, replace = TRUE)
      list(rolls = rolls, flat = p$flat, subtotal = sum(rolls) + p$flat)
    }
    
    roll_d20_adv <- function(mode = c("Normal","Adv","Disadv")) {
      mode <- match.arg(mode)
      r1 <- sample(1:20, 1)
      r2 <- sample(1:20, 1)
      if (mode == "Normal") return(list(kept = r1, detail = paste0("d20(", r1, ")"), nat = r1))
      if (mode == "Adv")    return(list(kept = max(r1, r2), detail = paste0("Adv d20(", r1, ",", r2, ") → ", max(r1, r2)), nat = max(r1, r2)))
      list(kept = min(r1, r2), detail = paste0("Disadv d20(", r1, ",", r2, ") → ", min(r1, r2)), nat = min(r1, r2))
    }
    
    fmt_mod <- function(x) ifelse(x >= 0, paste0("+", x), as.character(x))
    
    uid <- function(prefix) paste0(prefix, as.integer(Sys.time()), "_", sample(1000:9999, 1))
    
    move_within_subset <- function(df, row_id, dir = c("up","down"), subset_pred) {
      dir <- match.arg(dir)
      if (nrow(df) <= 1) return(df)
      
      idx <- which(df$id == row_id)
      if (length(idx) != 1) return(df)
      
      keep <- subset_pred(df)
      pos <- which(keep)
      
      # find idx position inside subset
      sidx <- which(pos == idx)
      if (length(sidx) != 1) return(df)
      
      if (dir == "up" && sidx > 1) {
        swap_idx <- pos[sidx - 1]
      } else if (dir == "down" && sidx < length(pos)) {
        swap_idx <- pos[sidx + 1]
      } else {
        return(df)
      }
      
      df[c(idx, swap_idx), ] <- df[c(swap_idx, idx), ]
      df
    }
    
    # ----------------------------
    # Data schema
    # ----------------------------
    
    
    # ----------------------------
    # Status Effects (pills)
    # ----------------------------
    STATUS_LIBRARY <- list(
      "Prone" = "Disadvantage on attack rolls. Melee attacks against you have advantage; ranged attacks have disadvantage. Standing uses half your movement.",
      "Grappled" = "Speed becomes 0. Ends if the grappler is incapacitated or you’re moved out of reach.",
      "Restrained" = "Speed 0. Attacks against you have advantage; your attacks have disadvantage. Disadvantage on DEX saves.",
      "Stunned" = "Incapacitated, can’t move. Auto-fail STR/DEX saves. Attacks against you have advantage.",
      "Unconscious" = "Incapacitated, prone, unaware. Auto-fail STR/DEX saves. Attacks have advantage; hits from within 5ft are crits.",
      "Poisoned" = "Disadvantage on attack rolls and ability checks.",
      "Blinded" = "Can’t see. Attacks against you have advantage; your attacks have disadvantage.",
      "Frightened" = "Disadvantage while source is in line of sight; can’t willingly move closer.",
      "Exhaustion (1)" = "Disadvantage on ability checks.",
      "Exhaustion (2)" = "Speed halved.",
      "Exhaustion (3)" = "Disadvantage on attack rolls and saving throws.",
      "Exhaustion (4)" = "Hit point maximum halved.",
      "Exhaustion (5)" = "Speed reduced to 0.",
      "Exhaustion (6)" = "Death."
    )
    status_choices <- names(STATUS_LIBRARY)
    
    observeEvent(TRUE, {
      updateSelectInput(session, "status_add", choices = status_choices, selected = status_choices[[1]])
    }, once = TRUE)
    
    output$status_pills_ui <- renderUI({
      eff <- state$char$status$effects %||% character()
      if (length(eff) == 0) return(tags$em("No active effects."))
      
      pills <- lapply(seq_along(eff), function(i) {
        e <- eff[[i]]
        tip <- STATUS_LIBRARY[[e]] %||% ""
        rm_id <- paste0("rm_effect_", i)
        
        tags$div(
          class = "status-pill",
          title = paste0(e, ": ", tip),
          tags$span(e),
          actionButton(ns(rm_id), "×", class = "rm", style = "padding:0;")
        )
      })
      
      tags$div(class = "status-bar", pills)
    })
    
    observeEvent(input$status_add_btn, {
      if (isTRUE(restoring())) return()
      e <- input$status_add
      if (is.null(e) || e == "") return()
      cur <- state$char$status$effects %||% character()
      if (!(e %in% cur)) {
        state$char$status$effects <- c(cur, e)
        safe_log(paste0("✨ Status added: ", e), toast = TRUE)
      } else {
        safe_log(paste0("ℹ️ Status already active: ", e), toast = TRUE)
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$status_clear, {
      if (isTRUE(restoring())) return()
      state$char$status$effects <- character()
      safe_log("🧹 Cleared all status effects.", toast = TRUE)
    }, ignoreInit = TRUE)
    
    observe({
      eff <- state$char$status$effects %||% character()
      for (i in seq_along(eff)) {
        local({
          idx <- i
          observeEvent(input[[paste0("rm_effect_", idx)]], {
            if (isTRUE(restoring())) return()
            cur <- state$char$status$effects %||% character()
            if (idx <= length(cur)) {
              removed <- cur[[idx]]
              
              # Prevent removing exhaustion
              if (grepl("^Exhaustion", removed)) {
                safe_log("⚠️ Exhaustion cannot be removed manually.", toast = TRUE)
                return()
              }
              state$char$status$effects <- cur[-idx]
              safe_log(paste0("🧼 Status removed: ", removed), toast = TRUE)
            }
          }, ignoreInit = TRUE)
        })
      }
    })
    
    # ----------------------------
    # Sync with character state
    # ----------------------------
    #temp debug
    output$session_debug <- renderPrint({
      get_session_overview(1)
    })
    
    
    # ----------------------------
    # Weapons UI (active + bag)
    # ----------------------------
    weapon_card_ui <- function(w, mode = c("active","bag")) {
      mode <- match.arg(mode)
      prefix <- paste0("w_", w$id, "_")
      
      # derived summary
      stat_mod <- ability_mod(w$stat %||% "str")
      pb <- prof_bonus()
      prof <- isTRUE(w$proficient)
      hit_bonus <- (w$to_hit_bonus %||% 0) + stat_mod + if (prof) pb else 0
      dmg1 <- trimws(w$damage1 %||% "")
      dmg2 <- trimws(w$damage2 %||% "")
      dmg_line <- paste(
        c(
          if (nzchar(dmg1)) paste0(dmg1, " ", (w$dmg_type1 %||% "")) else NULL,
          if (nzchar(dmg2)) paste0(dmg2, " ", (w$dmg_type2 %||% "")) else NULL
        ),
        collapse = " + "
      )
      if (!nzchar(dmg_line)) dmg_line <- "—"
      
      # Quick toggles as small actionButtons (server will toggle)
      toggles <- tags$div(
        class = "toggles",
        actionButton(ns(paste0(prefix, "toggle_prof")), paste0("Prof: ", if (prof) "ON" else "OFF"),
                     class = paste("btn-pill", if (prof) "btn-success" else "btn-default")),
        actionButton(ns(paste0(prefix, "cycle_adv")), paste0("Roll: ", w$adv %||% "Normal"),
                     class = "btn-pill")
      )
      
      if (isTRUE(w$edit)) {
        # EDIT MODE
        tags$div(
          class = "item-card",
          tags$div(
            class = "item-head",
            tags$div(
              tags$div(class = "item-title", "Editing Weapon"),
              tags$div(class = "item-sub", "Fill fields, then Save.")
            ),
            tags$div(
              class = "item-actions",
              actionButton(ns(paste0(prefix, "save")), "💾 Save", class = "btn-sm btn-primary"),
              actionButton(ns(paste0(prefix, "cancel")), "↩︎ Cancel", class = "btn-sm"),
              actionButton(ns(paste0(prefix, "to_bag")), if (mode == "active") "🧳 Bag" else "↩︎ Unbag", class="btn-sm"),
              actionButton(ns(paste0(prefix, "up")), "▲", class="btn-xs"),
              actionButton(ns(paste0(prefix, "down")), "▼", class="btn-xs"),
              actionButton(ns(paste0(prefix, "remove")), "✖", class="btn-xs btn-danger")
            )
          ),
          tags$div(
            class = "item-body",
            fluidRow(
              column(6, textInput(ns(paste0(prefix, "name")), "Name", value = w$name %||% "")),
              column(3, selectInput(ns(paste0(prefix, "stat")), "Stat", choices = COMBAT_WEAPON_STATS, selected = w$stat %||% "str")),
              column(3, numericInput(ns(paste0(prefix, "hit")), "Hit bonus", value = w$to_hit_bonus %||% 0))
            ),
            fluidRow(
              column(3, selectInput(ns(paste0(prefix, "adv")), "Roll mode", choices = c("Normal","Adv","Disadv"), selected = w$adv %||% "Normal")),
              column(3, checkboxInput(ns(paste0(prefix, "prof")), "Proficient", value = isTRUE(w$proficient))),
              column(3, textInput(ns(paste0(prefix, "dmg1")), "Damage 1", value = w$damage1 %||% "1d6")),
              column(3, selectInput(ns(paste0(prefix, "type1")), "Type 1", choices = COMBAT_DMG_TYPES, selected = w$dmg_type1 %||% "Slashing"))
            ),
            fluidRow(
              column(6, textInput(ns(paste0(prefix, "dmg2")), "Damage 2 (optional)", value = w$damage2 %||% "")),
              column(6, selectInput(ns(paste0(prefix, "type2")), "Type 2", choices = COMBAT_DMG_TYPES, selected = w$dmg_type2 %||% "Other"))
            )
          )
        )
      } else {
        # VIEW MODE
        tags$div(
          class = "item-card",
          tags$div(
            class = "item-head",
            tags$div(
              tags$div(class = "item-title", w$name %||% "Weapon"),
              tags$div(class = "item-sub",
                       paste0("To Hit: ", fmt_mod(hit_bonus), " • ",
                              "Stat: ", toupper(w$stat %||% "STR"),
                              if (prof) paste0(" • Prof(+", pb, ")") else "",
                              " • ", (w$adv %||% "Normal")),
                       tags$br(),
                       paste0("Damage: ", dmg_line)
              ),
              toggles
            ),
            tags$div(
              class = "item-actions",
              actionButton(ns(paste0(prefix, "roll_hit")), "🎯", class = "btn-xs"),
              actionButton(ns(paste0(prefix, "roll_dmg")), "💥", class = "btn-xs"),
              actionButton(ns(paste0(prefix, "edit")), "✎", class = "btn-xs"),
              actionButton(ns(paste0(prefix, "to_bag")), if (mode == "active") "🧳" else "↩︎", class = "btn-xs"),
              actionButton(ns(paste0(prefix, "up")), "▲", class="btn-xs"),
              actionButton(ns(paste0(prefix, "down")), "▼", class="btn-xs"),
              actionButton(ns(paste0(prefix, "remove")), "✖", class = "btn-xs btn-danger")
            )
          )
        )
      }
    }
    
    output$weapons_active_ui <- renderUI({
      df <- get_items_by_type("weapon", TRUE)
      if (nrow(df) == 0) return(tags$em("No weapons equipped."))
      
      tagList(lapply(seq_len(nrow(df)), function(i) {
        w <- df[i,]
        meta <- w$meta[[1]]
        
        weapon_card_ui(list(
          id = w$id,
          name = w$name,
          stat = meta$stat %||% "str",
          adv = meta$adv %||% "Normal",
          to_hit_bonus = meta$to_hit_bonus %||% 0,
          damage1 = meta$damage1 %||% "",
          dmg_type1 = meta$dmg_type1 %||% "",
          damage2 = meta$damage2 %||% "",
          dmg_type2 = meta$dmg_type2 %||% "",
          proficient = meta$proficient %||% TRUE,
          in_bag = w$in_bag,
          edit = w$edit
        ), mode = "active")
      }))
    })
    
    output$weapons_bag_ui <- renderUI({
      df <- get_items_by_type("weapon")
      df <- df[isTRUE(df$in_bag), , drop = FALSE]
      if (nrow(df) == 0) return(tags$em("Bag is empty."))
      
      tagList(lapply(seq_len(nrow(df)), function(i) {
        w <- df[i,]
        meta <- w$meta[[1]]
        
        weapon_card_ui(list(
          id = w$id,
          name = w$name,
          stat = meta$stat %||% "str",
          adv = meta$adv %||% "Normal",
          to_hit_bonus = meta$to_hit_bonus %||% 0,
          damage1 = meta$damage1 %||% "",
          dmg_type1 = meta$dmg_type1 %||% "",
          damage2 = meta$damage2 %||% "",
          dmg_type2 = meta$dmg_type2 %||% "",
          proficient = meta$proficient %||% TRUE,
          in_bag = w$in_bag,
          edit = w$edit
        ), mode = "bag")
      }))
    })
    
    # ----------------------------
    # Armor UI (active + bag)
    # ----------------------------
    armor_card_ui <- function(a, mode = c("active","bag")) {
      mode <- match.arg(mode)
      prefix <- paste0("a_", a$id, "_")
      
      dm <- ability_mod("dex")
      pb <- prof_bonus()
      
      max_dex <- switch(
        a$type %||% "Light",
        "Light" = Inf,
        "Medium" = 2,
        "Heavy" = 0,
        "Custom" = a$custom_max_dex %||% 0,
        Inf
      )
      dex_part <- min(dm, max_dex)
      prof <- isTRUE(a$proficient)
      worn <- isTRUE(a$worn)
      ac <- (a$base_ac %||% 10) + dex_part + if (prof) pb else 0
      
      toggles <- tags$div(
        class = "toggles",
        actionButton(ns(paste0(prefix, "toggle_prof")), paste0("Prof: ", if (prof) "ON" else "OFF"),
                     class = paste("btn-pill", if (prof) "btn-success" else "btn-default")),
        actionButton(ns(paste0(prefix, "toggle_worn")), paste0("Worn: ", if (worn) "ON" else "OFF"),
                     class = paste("btn-pill", if (worn) "btn-success" else "btn-default"))
      )
      
      if (isTRUE(a$edit)) {
        tags$div(
          class = "item-card",
          tags$div(
            class = "item-head",
            tags$div(
              tags$div(class = "item-title", "Editing Armor"),
              tags$div(class = "item-sub", "Fill fields, then Save.")
            ),
            tags$div(
              class = "item-actions",
              actionButton(ns(paste0(prefix, "save")), "💾 Save", class = "btn-sm btn-primary"),
              actionButton(ns(paste0(prefix, "cancel")), "↩︎ Cancel", class = "btn-sm"),
              actionButton(ns(paste0(prefix, "to_bag")), if (mode == "active") "🧳 Bag" else "↩︎ Unbag", class="btn-sm"),
              actionButton(ns(paste0(prefix, "up")), "▲", class="btn-xs"),
              actionButton(ns(paste0(prefix, "down")), "▼", class="btn-xs"),
              actionButton(ns(paste0(prefix, "remove")), "✖", class="btn-xs btn-danger")
            )
          ),
          tags$div(
            class = "item-body",
            fluidRow(
              column(6, textInput(ns(paste0(prefix, "name")), "Name", value = a$name %||% "")),
              column(2, numericInput(ns(paste0(prefix, "base")), "Base AC", value = a$base_ac %||% 11, min = 0)),
              column(2, selectInput(ns(paste0(prefix, "type")), "Type", choices = COMBAT_ARMOR_TYPES, selected = a$type %||% "Light")),
              column(2, checkboxInput(ns(paste0(prefix, "prof")), "Proficient", value = isTRUE(a$proficient)))
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Custom'", ns(paste0(prefix, "type"))),
              numericInput(ns(paste0(prefix, "dex")), "Custom Max DEX", value = a$custom_max_dex %||% 0, min = 0)
            ),
            checkboxInput(ns(paste0(prefix, "worn")), "Currently Worn", value = isTRUE(a$worn))
          )
        )
      } else {
        subline <- paste0(
          "AC if worn: ", ac,
          " • Base: ", (a$base_ac %||% 10),
          " • Type: ", (a$type %||% "Light"),
          if (prof) paste0(" • Prof(+", pb, ")") else "",
          if (worn) " • WORN" else ""
        )
        
        tags$div(
          class = "item-card",
          tags$div(
            class = "item-head",
            tags$div(
              tags$div(class = "item-title", a$name %||% "Armor"),
              tags$div(class = "item-sub", subline),
              toggles
            ),
            tags$div(
              class = "item-actions",
              actionButton(ns(paste0(prefix, "edit")), "✎", class = "btn-xs"),
              actionButton(ns(paste0(prefix, "to_bag")), if (mode == "active") "🧳" else "↩︎", class = "btn-xs"),
              actionButton(ns(paste0(prefix, "up")), "▲", class="btn-xs"),
              actionButton(ns(paste0(prefix, "down")), "▼", class="btn-xs"),
              actionButton(ns(paste0(prefix, "remove")), "✖", class = "btn-xs btn-danger")
            )
          )
        )
      }
    }
    
    output$armor_active_ui <- renderUI({
      df <- get_items_by_type("armor", TRUE)
      if (nrow(df) == 0) return(tags$em("No armor equipped."))
      
      tagList(lapply(seq_len(nrow(df)), function(i) {
        a <- df[i,]
        meta <- a$meta[[1]]
        
        armor_card_ui(list(
          id = a$id,
          name = a$name,
          base_ac = meta$base_ac %||% 10,
          type = meta$type %||% "Light",
          custom_max_dex = meta$custom_max_dex %||% 0,
          proficient = meta$proficient %||% TRUE,
          worn = a$equipped,
          in_bag = a$in_bag,
          edit = a$edit
        ), mode = "active")
      }))
    })
    
    
    output$armor_bag_ui <- renderUI({
      df <- get_items_by_type("armor")
      df <- df[isTRUE(df$in_bag), , drop = FALSE]
      if (nrow(df) == 0) return(tags$em("Bag is empty."))
      
      tagList(lapply(seq_len(nrow(df)), function(i) {
        a <- df[i,]
        meta <- a$meta[[1]]
        
        armor_card_ui(list(
          id = a$id,
          name = a$name,
          base_ac = meta$base_ac %||% 10,
          type = meta$type %||% "Light",
          custom_max_dex = meta$custom_max_dex %||% 0,
          proficient = meta$proficient %||% TRUE,
          worn = a$equipped,
          in_bag = a$in_bag,
          edit = a$edit
        ), mode = "bag")
      }))
    })
    #Loop###########
    observe({
      df <- get_inventory()
      
      for (id in df$id) {
        local({
          iid <- id
          p_w <- paste0("w_", iid, "_")
          p_a <- paste0("a_", iid, "_")
          
          get_row <- function() {
            d <- isolate(get_inventory())
            d[d$id == iid, , drop = FALSE]
          }
          
          set_row <- function(r) {
            d <- isolate(get_inventory())
            idx <- which(d$id == iid)
            d[idx,] <- r
            state$char$inventory$items <- d
          }
          
          # -----------------------
          # DELETE
          # -----------------------
          observeEvent(input[[paste0(p_w,"remove")]], {
            d <- get_inventory()
            state$char$inventory$items <- d[d$id != iid,]
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p_a,"remove")]], {
            d <- get_inventory()
            state$char$inventory$items <- d[d$id != iid,]
          }, ignoreInit = TRUE)
          
          # -----------------------
          # BAG
          # -----------------------
          observeEvent(input[[paste0(p_w,"to_bag")]], {
            r <- get_row()
            r$in_bag <- !r$in_bag
            set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p_a,"to_bag")]], {
            r <- get_row()
            r$in_bag <- !r$in_bag
            if (r$type == "armor") r$equipped <- FALSE
            set_row(r)
          }, ignoreInit = TRUE)
          
          # =======================
          # 🗡️ WEAPON EDIT
          # =======================
          observeEvent(input[[paste0(p_w,"edit")]], {
            r <- get_row(); r$edit <- TRUE; set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p_w,"cancel")]], {
            r <- get_row(); r$edit <- FALSE; set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p_w,"save")]], {
            r <- get_row()
            meta <- r$meta[[1]]
            
            r$name <- input[[paste0(p_w,"name")]]
            meta$stat <- input[[paste0(p_w,"stat")]]
            meta$adv  <- input[[paste0(p_w,"adv")]]
            meta$to_hit_bonus <- input[[paste0(p_w,"hit")]]
            meta$damage1 <- input[[paste0(p_w,"dmg1")]]
            meta$dmg_type1 <- input[[paste0(p_w,"type1")]]
            meta$damage2 <- input[[paste0(p_w,"dmg2")]]
            meta$dmg_type2 <- input[[paste0(p_w,"type2")]]
            meta$proficient <- isTRUE(input[[paste0(p_w,"prof")]])
            
            r$meta[[1]] <- meta
            r$edit <- FALSE
            set_row(r)
            
            safe_log(paste0("💾 Saved weapon: ", r$name), toast = TRUE)
          }, ignoreInit = TRUE)
          
          # =======================
          # 🎯 HIT ROLL
          # =======================
          observeEvent(input[[paste0(p_w,"roll_hit")]], {
            r <- get_row()
            meta <- r$meta[[1]]
            
            stat_mod <- ability_mod(meta$stat)
            pb <- prof_bonus()
            rr <- roll_d20_adv(meta$adv %||% "Normal")
            
            total <- rr$kept + stat_mod + (meta$to_hit_bonus %||% 0) + 
              if (isTRUE(meta$proficient)) pb else 0
            
            safe_log(paste0("🎯 ", r$name, ": ", rr$detail, " = ", total), toast = TRUE)
          }, ignoreInit = TRUE)
          
          # =======================
          # 💥 DAMAGE
          # =======================
          observeEvent(input[[paste0(p_w,"roll_dmg")]], {
            r <- get_row()
            meta <- r$meta[[1]]
            
            d1 <- roll_damage(meta$damage1)
            d2 <- if (nzchar(meta$damage2)) roll_damage(meta$damage2) else NULL
            
            total <- (d1$subtotal %||% 0) + (d2$subtotal %||% 0)
            
            safe_log(paste0("💥 ", r$name, " deals ", total), toast = TRUE)
          }, ignoreInit = TRUE)
          
          #toggles
          observeEvent(input[[paste0(p_w,"toggle_prof")]], {
            r <- get_row()
            meta <- r$meta[[1]]
            meta$proficient <- !isTRUE(meta$proficient)
            r$meta[[1]] <- meta
            set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p_w,"cycle_adv")]], {
            r <- get_row()
            meta <- r$meta[[1]]
            
            modes <- c("Normal","Adv","Disadv")
            cur <- match(meta$adv %||% "Normal", modes)
            meta$adv <- modes[(cur %% 3) + 1]
            
            r$meta[[1]] <- meta
            set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p_a,"toggle_prof")]], {
            r <- get_row()
            meta <- r$meta[[1]]
            meta$proficient <- !isTRUE(meta$proficient)
            r$meta[[1]] <- meta
            set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p_a,"toggle_worn")]], {
            r <- get_row()
            r$equipped <- !isTRUE(r$equipped)
            set_row(r)
          }, ignoreInit = TRUE)
          
          # =======================
          # 🛡️ ARMOR EDIT
          # =======================
          observeEvent(input[[paste0(p_a,"edit")]], {
            r <- get_row(); r$edit <- TRUE; set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p_a,"cancel")]], {
            r <- get_row(); r$edit <- FALSE; set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p_a,"save")]], {
            r <- get_row()
            meta <- r$meta[[1]]
            
            r$name <- input[[paste0(p_a,"name")]]
            meta$base_ac <- input[[paste0(p_a,"base")]]
            meta$type <- input[[paste0(p_a,"type")]]
            meta$proficient <- isTRUE(input[[paste0(p_a,"prof")]])
            
            if (meta$type == "Custom") {
              meta$custom_max_dex <- input[[paste0(p_a,"dex")]]
            }
            
            r$equipped <- isTRUE(input[[paste0(p_a,"worn")]])
            
            r$meta[[1]] <- meta
            r$edit <- FALSE
            set_row(r)
            
            safe_log(paste0("💾 Saved armor: ", r$name), toast = TRUE)
          }, ignoreInit = TRUE)
          
        })
      }
    })
    
    #loop end
    
    
    # ----------------------------
    # Add weapon / armor
    # ----------------------------
    observeEvent(input$add_weapon, {
      if (isTRUE(restoring())) return()
      
      df <- get_inventory()
      
      new <- data.frame(
        id = uid("w"),
        name = "New Weapon",
        type = "weapon",
        desc = "",
        value = 0,
        weight = 2,
        qty = 1,
        in_bag = FALSE,
        equipped = TRUE,
        meta = I(list(list(
          stat = "str",
          adv = "Normal",
          to_hit_bonus = 0,
          damage1 = "1d6",
          dmg_type1 = "Slashing",
          damage2 = "",
          dmg_type2 = "Other",
          proficient = TRUE
        ))),
        edit = TRUE,
        stringsAsFactors = FALSE
      )
      
      state$char$inventory$items <- rbind(df, new)
    })
    
    observeEvent(input$add_armor, {
      if (isTRUE(restoring())) return()
      
      df <- get_inventory()
      
      new <- data.frame(
        id = uid("a"),
        name = "New Armor",
        type = "armor",
        desc = "",
        value = 0,
        weight = 10,
        qty = 1,
        in_bag = FALSE,
        equipped = FALSE,
        meta = I(list(list(
          base_ac = 11,
          type = "Light",
          custom_max_dex = 0,
          proficient = TRUE
        ))),
        edit = TRUE,
        stringsAsFactors = FALSE
      )
      
      state$char$inventory$items <- rbind(df, new)
    })
    
    # ----------------------------
    # AC output (uses worn armors; manual override supported)
    # ----------------------------
    output$auto_ac <- renderText({
      if (isTRUE(input$ac_manual)) {
        return(input$ac_manual_value %||% 10)
      }
      
      pb <- prof_bonus()
      dm <- ability_mod("dex")
      
      a <- get_items_by_type("armor")
      best <- 10 + dm
      
      if (nrow(a) == 0) return(best)
      
      for (i in seq_len(nrow(a))) {
        row <- a[i, ]
        if (!isTRUE(row$equipped) || isTRUE(row$in_bag)) next
        
        meta <- row$meta[[1]]
        
        max_dex <- switch(
          meta$type %||% "Light",
          "Light" = Inf,
          "Medium" = 2,
          "Heavy" = 0,
          "Custom" = meta$custom_max_dex %||% 0,
          Inf
        )
        
        dex_part <- min(dm, max_dex)
        
        ac <- (meta$base_ac %||% 10) + dex_part + 
          if (isTRUE(meta$proficient)) pb else 0
        best <- max(best, ac)
      }
      
      best
    })
    
    # ----------------------------
    # HP bar + hearts UI
    # ----------------------------
    output$hp_ui <- renderUI({
      x <- validate_character(state$char)
      hp <- get_effective_hp_state(state)
      
      max_hp  <- as.integer(x$resources$hp$max %||% 10)
      cur_hp  <- as.integer(hp$cur %||% max_hp)
      temp_hp <- as.integer(hp$temp %||% 0)
      
      pct <- if (max_hp > 0) round(100 * cur_hp / max_hp) else 0
      
      # hearts scaling: show up to 20 hearts, each represents chunk HP
      hearts_n <- min(20, max(1, ceiling(max_hp / 1)))  # start from 1, then cap
      chunk <- ceiling(max_hp / hearts_n)
      filled <- floor(cur_hp / chunk)
      partial <- (cur_hp %% chunk) > 0
      
      hearts <- lapply(seq_len(hearts_n), function(i) {
        # Use text hearts to avoid extra deps
        if (i <= filled) {
          tags$span(class="heart", title=paste0("~", chunk, " HP"), "♥")
        } else if (i == filled + 1 && partial) {
          tags$span(class="heart", title=paste0("~", chunk, " HP"), "♡")
        } else {
          tags$span(class="heart", title=paste0("~", chunk, " HP"), "♡")
        }
      })
      
      tagList(
        tags$div(class="hp-label",
                 tags$span(paste0("HP: ", cur_hp, " / ", max_hp, if (temp_hp > 0) paste0("  (Temp ", temp_hp, ")") else "")),
                 tags$span(paste0(pct, "%"))
        ),
        tags$div(class="hp-bar",
                 tags$div(style = paste0("width:", pct, "%;"))
        ),
        br(),
        tags$div(class="hearts", hearts)
      )
    })
    
    # ----------------------------
    # HP / damage / heal logic
    # ----------------------------
    observeEvent(input$max_hp, {
      if (isTRUE(restoring())) return()
      
      x <- validate_character(state$char)
      new_max <- max(0L, as.integer(input$max_hp %||% 0))
      x$resources$hp$max <- new_max
      state$char <- x
      
      hp <- get_effective_hp_state(state)
      if (hp$cur > new_max) {
        set_effective_hp_state(state, cur = new_max, temp = hp$temp)
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$current_hp, {
      if (isTRUE(restoring())) return()
      
      x <- validate_character(state$char)
      max_hp <- as.integer(x$resources$hp$max %||% 0)
      new_cur <- max(0L, min(max_hp, as.integer(input$current_hp %||% 0)))
      
      set_effective_hp_state(state, cur = new_cur, temp = get_effective_hp_state(state)$temp)
    }, ignoreInit = TRUE)
    
    observeEvent(input$temp_hp, {
      if (isTRUE(restoring())) return()
      
      hp <- get_effective_hp_state(state)
      new_temp <- max(0L, as.integer(input$temp_hp %||% 0))
      
      set_effective_hp_state(state, cur = hp$cur, temp = new_temp)
    }, ignoreInit = TRUE)
    
    observeEvent(input$apply_damage, {
      if (isTRUE(restoring())) return()
      
      dmg <- as.integer(input$damage_amt %||% 0)
      if (dmg <= 0) return()
      
      res <- apply_damage_to_state(state, dmg)
      if (is.null(res)) {
        safe_log("⚠️ Could not apply damage.", toast = TRUE)
        return()
      }
      
      updateNumericInput(session, "temp_hp", value = res$temp_after)
      updateNumericInput(session, "current_hp", value = res$hp_after)
      
      absorbed <- res$temp_before - res$temp_after
      
      safe_log(
        paste0(
          "💢 Took ", dmg,
          " damage (Temp absorbed ", absorbed, "). Current HP: ", res$hp_after
        ),
        toast = TRUE
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$heal_damage, {
      if (isTRUE(restoring())) return()
      
      heal <- as.integer(input$damage_amt %||% 0)
      if (heal <= 0) return()
      
      res <- apply_healing_to_state(state, heal)
      if (is.null(res)) {
        safe_log("⚠️ Could not apply healing.", toast = TRUE)
        return()
      }
      
      gained <- res$hp_after - res$hp_before
      
      updateNumericInput(session, "current_hp", value = res$hp_after)
      updateNumericInput(session, "temp_hp", value = res$temp_after)
      
      safe_log(
        paste0("✨ Healed ", gained, " HP. Current HP: ", res$hp_after),
        toast = TRUE
      )
    }, ignoreInit = TRUE)
    
    combat_hp_poll <- reactivePoll(
      intervalMillis = 1500,
      session = session,
      checkFunc = function() {
        sid <- state$active_session_id
        cid <- state$char_id
        
        if (is.null(sid) || is.null(cid)) return("no-session")
        
        row <- tryCatch(
          get_session_player_row(sid, cid),
          error = function(e) data.frame()
        )
        
        if (nrow(row) == 0) return("no-row")
        
        paste(
          row$current_hp[1] %||% "",
          row$temp_hp[1] %||% "",
          row$updated_at[1] %||% "",
          sep = "|"
        )
      },
      valueFunc = function() {
        sid <- state$active_session_id
        cid <- state$char_id
        
        if (is.null(sid) || is.null(cid)) return(NULL)
        
        tryCatch(
          get_session_player_row(sid, cid),
          error = function(e) data.frame()
        )
      }
    )
    
    observe({
      state$char
      state$char_id
      state$active_session_id
      row <- combat_hp_poll()
      
      x <- validate_character(state$char)
      max_hp <- as.integer(x$resources$hp$max %||% 10)
      
      hp <- get_effective_hp_state(state)
      
      if (isTRUE(restoring())) return()
      
      restoring(TRUE)
      on.exit(restoring(FALSE), add = TRUE)
      
      updateNumericInput(session, "max_hp", value = max_hp)
      updateNumericInput(session, "current_hp", value = hp$cur)
      updateNumericInput(session, "temp_hp", value = hp$temp)
    })
    
    # ----------------------------
    # Hydrate on character replace
    # ----------------------------
    
    observeEvent(char_rev(), {
      restoring(TRUE)
      on.exit(restoring(FALSE), add = TRUE)
      
      x <- validate_character(state$char)
      hp_eff <- get_effective_hp_state(state)
      
      updateNumericInput(session, "max_hp", value = x$resources$hp$max %||% 10)
      updateNumericInput(session, "current_hp", value = hp_eff$cur %||% (x$resources$hp$max %||% 10))
      updateNumericInput(session, "temp_hp", value = hp_eff$temp %||% 0)
      updateNumericInput(session, "damage_amt", value = 0)
      
      updateSelectInput(session, "status_add", choices = status_choices, selected = status_choices[[1]])
    }, ignoreInit = TRUE)
    
    
    
    observeEvent(TRUE, {
      
    }, once = TRUE)
  })
}
