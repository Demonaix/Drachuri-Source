# server/armoury_module.R
library(shiny)

armouryTabUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Armory",
    value = "armoury",
    
    tags$style(HTML({
      root <- ns("root")
      pfx  <- paste0("#", root, " ")
      
      paste0(
        pfx, ".card{border:1px solid rgba(150,120,70,0.55);border-radius:14px;background:rgba(255,255,245,0.85);padding:14px;margin-bottom:14px;box-shadow:0 4px 10px rgba(0,0,0,0.08);}\n",
        pfx, ".card-titlebar{display:flex;align-items:center;justify-content:space-between;gap:10px;margin-bottom:10px;}\n",
        pfx, ".item-card{border:1px solid rgba(150,120,70,0.45);border-radius:14px;background:rgba(255,255,250,0.92);padding:12px;margin-bottom:10px;}\n",
        pfx, ".item-head{display:flex;gap:10px;align-items:flex-start;justify-content:space-between;}\n",
        pfx, ".item-title{font-size:18px;font-weight:700;}\n",
        pfx, ".item-sub{font-size:13px;opacity:.9;line-height:1.3;margin-top:2px;}\n",
        pfx, ".item-actions{display:flex;gap:6px;flex-wrap:wrap;justify-content:flex-end;}\n",
        pfx, ".pill{display:inline-flex;align-items:center;gap:6px;padding:5px 10px;border-radius:999px;border:1px solid rgba(150,120,70,0.35);background:rgba(255,255,245,0.8);font-size:12px;font-weight:700;}\n",
        pfx, ".pill-row{display:flex;gap:8px;flex-wrap:wrap;}\n",
        pfx, "details.bag summary{cursor:pointer;font-weight:700;margin-top:6px;opacity:.95;}\n"
      )
    })),
    
    div(
      id = ns("root"),
      
      div(
        class = "card",
        h4("🛡️ Defense"),
        uiOutput(ns("ac_ui"))
      ),
      
      div(
        class = "card",
        div(
          class = "card-titlebar",
          h4("🗡️ Weapons"),
          actionButton(ns("add_weapon"), "➕ Add Weapon", class = "btn btn-default btn-sm")
        ),
        uiOutput(ns("weapons_active_ui")),
        tags$details(
          class = "bag",
          tags$summary("🧳 Weapons Bag"),
          uiOutput(ns("weapons_bag_ui"))
        )
      ),
      
      div(
        class = "card",
        div(
          class = "card-titlebar",
          h4("🛡️ Armor"),
          actionButton(ns("add_armor"), "➕ Add Armor", class = "btn btn-default btn-sm")
        ),
        uiOutput(ns("armor_active_ui")),
        tags$details(
          class = "bag",
          tags$summary("🧳 Armor Bag"),
          uiOutput(ns("armor_bag_ui"))
        )
      )
    )
  )
}

armouryTabServer <- function(id, state, restoring, add_log, char_rev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- get("%||%", inherits = TRUE)
    
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
    
    if (!exists("COMBAT_ARMOR_TYPES", inherits = TRUE)) {
      COMBAT_ARMOR_TYPES <- c("Light", "Medium", "Heavy", "Custom")
    }
    
    if (!exists("COMBAT_WEAPON_STATS", inherits = TRUE)) {
      COMBAT_WEAPON_STATS <- c("str", "dex", "con", "int", "bld_str", "cha")
    }
    
    uid <- function(prefix) {
      paste0(prefix, as.integer(Sys.time()), "_", sample(1000:9999, 1))
    }
    
    # --------------------------------
    # Shared helper wrappers
    # --------------------------------
    weapon_meta_defaults_local <- function(meta = NULL) {
      if (exists("weapon_meta_defaults_global", inherits = TRUE)) {
        return(weapon_meta_defaults_global(meta))
      }
      
      meta <- meta %||% list()
      if (!is.list(meta)) meta <- list()
      
      meta$stat <- as.character(meta$stat %||% "str")
      if (!meta$stat %in% COMBAT_WEAPON_STATS) meta$stat <- "str"
      
      meta$adv <- as.character(meta$adv %||% "Normal")
      if (!meta$adv %in% c("Normal", "Adv", "Disadv")) meta$adv <- "Normal"
      
      meta$to_hit_bonus <- suppressWarnings(as.numeric(meta$to_hit_bonus %||% 0))
      if (is.na(meta$to_hit_bonus)) meta$to_hit_bonus <- 0
      
      meta$damage1 <- as.character(meta$damage1 %||% "1d6")
      meta$dmg_type1 <- as.character(meta$dmg_type1 %||% "Slashing")
      meta$damage2 <- as.character(meta$damage2 %||% "")
      meta$dmg_type2 <- as.character(meta$dmg_type2 %||% "Other")
      meta$proficient <- isTRUE(meta$proficient)
      
      meta
    }
    
    armor_meta_defaults_local <- function(meta = NULL) {
      if (exists("armor_meta_defaults_global", inherits = TRUE)) {
        return(armor_meta_defaults_global(meta))
      }
      
      meta <- meta %||% list()
      if (!is.list(meta)) meta <- list()
      
      meta$base_ac <- suppressWarnings(as.numeric(meta$base_ac %||% 11))
      if (is.na(meta$base_ac)) meta$base_ac <- 11
      
      meta$type <- as.character(meta$type %||% "Light")
      if (!meta$type %in% COMBAT_ARMOR_TYPES) meta$type <- "Light"
      
      meta$custom_max_dex <- suppressWarnings(as.numeric(meta$custom_max_dex %||% 0))
      if (is.na(meta$custom_max_dex)) meta$custom_max_dex <- 0
      
      meta$proficient <- isTRUE(meta$proficient)
      
      meta
    }
    
    prof_bonus <- reactive({
      x <- validate_character(state$char)
      lvl <- suppressWarnings(as.integer(x$build$level %||% 1))
      if (is.na(lvl) || lvl < 1) lvl <- 1
      ceiling(lvl / 4) + 1
    })
    
    ability_mod <- function(stat) {
      x <- validate_character(state$char)
      mod_calc(x$abilities[[stat]] %||% 10)
    }
    
    # --------------------------------
    # Shared inventory
    # --------------------------------
    get_inventory <- reactive({
      x <- validate_character(state$char)
      inventory_normalize(x$inventory$items)
    })
    
    write_inventory <- function(df) {
      x <- validate_character(state$char)
      x$inventory$items <- inventory_normalize(df)
      state$char <- x
    }
    
    get_items_by_type <- function(type, active_only = FALSE) {
      df <- get_inventory()
      if (!nrow(df)) return(df)
      
      df <- df[df$type == type, , drop = FALSE]
      
      in_bag <- as.logical(df$in_bag)
      in_bag[is.na(in_bag)] <- FALSE
      df$in_bag <- in_bag
      
      if (active_only) {
        df <- df[!df$in_bag, , drop = FALSE]
      }
      
      df
    }
    
    weapon_hit_bonus <- function(row) {
      meta <- weapon_meta_defaults_local(row$meta[[1]])
      stat <- meta$stat %||% "str"
      prof <- isTRUE(meta$proficient)
      
      as.numeric(meta$to_hit_bonus %||% 0) +
        as.numeric(meta$material_attack_bonus %||% 0) +
        as.numeric(meta$quality_attack_bonus %||% 0) +
        ability_mod(stat) +
        if (prof) prof_bonus() else 0
    }
    
    armor_item_ac <- function(row) {
      dex_mod <- ability_mod("dex")
      pb <- prof_bonus()
      meta <- armor_meta_defaults_local(row$meta[[1]])
      
      type <- meta$type %||% "Light"
      base_ac <- as.numeric(meta$base_ac %||% 10)
      prof <- isTRUE(meta$proficient)
      
      max_dex <- switch(
        type,
        "Light" = Inf,
        "Medium" = 2,
        "Heavy" = 0,
        "Custom" = as.numeric(meta$custom_max_dex %||% 0),
        Inf
      )
      
      dex_add <- min(dex_mod, max_dex)
      base_ac + dex_add + as.numeric(meta$material_armour_modifier %||% 0) +
        as.numeric(meta$quality_armour_modifier %||% 0) + if (prof) pb else 0
    }
    
    calc_auto_ac_local <- function() {
      if (exists("calc_auto_ac_for_char", inherits = TRUE)) {
        return(calc_auto_ac_for_char(state$char))
      }
      
      x <- validate_character(state$char)
      df <- inventory_normalize(x$inventory$items)
      
      equipped <- as.logical(df$equipped)
      equipped[is.na(equipped)] <- FALSE
      in_bag <- as.logical(df$in_bag)
      in_bag[is.na(in_bag)] <- FALSE
      
      arm <- df[df$type == "armor" & equipped & !in_bag, , drop = FALSE]
      if (nrow(arm) == 0) {
        return(10 + ability_mod("dex"))
      }
      
      armor_item_ac(arm[1, , drop = FALSE])
    }
    # --------------------------------
    # Defense UI
    # --------------------------------
    output$ac_ui <- renderUI({
      tags$div(
        class = "pill-row",
        tags$div(class = "pill", "Auto AC", tags$strong(calc_auto_ac_local())),
        tags$div(class = "pill", "Prof Bonus", tags$strong(paste0("+", prof_bonus())))
      )
    })
    
    # --------------------------------
    # Card renderers
    # --------------------------------
    weapon_card <- function(w, mode = "active") {
      p <- paste0("itm_", w$id, "_")
      meta <- weapon_meta_defaults_local(w$meta[[1]])
      glyph_until <- suppressWarnings(as.integer(meta$glyph_active_until_day %||% NA_integer_))
      current_day <- suppressWarnings(as.integer(validate_character(state$char)$meta$day %||% 1L))
      glyph_active <- !is.na(glyph_until) && current_day <= glyph_until && nzchar(as.character(meta$glyph_damage %||% ""))
      glyph_days_left<-if(!is.na(glyph_until))max(0L,glyph_until-current_day)else NA_integer_
      glyph_rank<-as.character(meta$glyph_rank%||%"Minor")
      glyph_line <- if (glyph_active) paste0(" • ✧ ",glyph_rank," ",meta$glyph_name%||%"Enhancement",": +",meta$glyph_damage," ",meta$glyph_damage_type," • ",glyph_days_left," day(s) remaining (through day ",glyph_until,")") else if (!is.na(glyph_until)) paste0(" • ✧ ",glyph_rank," enhancement DEPLETED — replenish in Glyphs") else ""
      displayed_damage2<-if(glyph_active)as.character(meta$glyph_damage)else as.character(meta$damage2)
      displayed_type2<-if(glyph_active)as.character(meta$glyph_damage_type)else as.character(meta$dmg_type2)
      hit_bonus <- weapon_hit_bonus(w)
      damage_modifier <- as.numeric(meta$material_damage_modifier %||% 0) + as.numeric(meta$quality_damage_modifier %||% 0)
      
      if (isTRUE(w$edit)) {
        return(
          div(
            class = "item-card",
            textInput(ns(paste0(p, "name")), "Weapon Name", value = w$name[[1]] %||% ""),
            textAreaInput(ns(paste0(p, "desc")), "Description", value = w$desc[[1]] %||% ""),
            fluidRow(
              column(4, numericInput(ns(paste0(p, "qty")), "Qty", value = w$qty[[1]] %||% 1, min = 1, step = 1)),
              column(4, numericInput(ns(paste0(p, "weight")), "Weight", value = w$weight[[1]] %||% 0, min = 0, step = 0.1)),
              column(4, numericInput(ns(paste0(p, "value")), "Value", value = w$value[[1]] %||% 0, min = 0, step = 1))
            ),
            fluidRow(
              column(4, selectInput(ns(paste0(p, "stat")), "Attack Stat", choices = COMBAT_WEAPON_STATS, selected = meta$stat)),
              column(4, numericInput(ns(paste0(p, "to_hit_bonus")), "Extra Hit Bonus", value = meta$to_hit_bonus, step = 1)),
              column(4, selectInput(ns(paste0(p, "adv")), "Roll State", choices = c("Normal", "Adv", "Disadv"), selected = meta$adv))
            ),
            fluidRow(
              column(6, textInput(ns(paste0(p, "damage1")), "Damage 1", value = meta$damage1)),
              column(6, textInput(ns(paste0(p, "dmg_type1")), "Damage Type 1", value = meta$dmg_type1))
            ),
            fluidRow(
              column(6, textInput(ns(paste0(p, "damage2")), if(glyph_active)"Enchanted Damage 2"else"Damage 2", value = displayed_damage2)),
              column(6, textInput(ns(paste0(p, "dmg_type2")), if(glyph_active)"Enchanted Damage Type 2"else"Damage Type 2", value = displayed_type2))
            ),
            fluidRow(
              column(6, selectInput(ns(paste0(p, "material")), "Material", c("Copper","Iron","Steel","Titanium Copper","Wood"), selected = meta$material %||% "Steel")),
              column(6, selectInput(ns(paste0(p, "build_quality")), "Build quality", c("Very-Poorly-Crafted","Poorly-Crafted","Passably-Crafted","Bog-Standard","Well-Crafted","Master-Crafted"), selected = meta$build_quality %||% "Bog-Standard"))
            ),
            checkboxInput(ns(paste0(p, "proficient")), "Proficient", value = isTRUE(meta$proficient)),
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
            div(class = "item-title", paste0("🗡️ ", w$name[[1]] %||% "Weapon")),
            div(
              class = "item-sub",
              paste0(
                "To Hit: ",
                ifelse(hit_bonus >= 0, paste0("+", hit_bonus), hit_bonus),
                " • Stat: ", toupper(meta$stat),
                " • Damage: ", meta$damage1,
                if (nzchar(displayed_damage2)) paste0(" + ", displayed_damage2," ",displayed_type2) else "",
                glyph_line,
                if (damage_modifier != 0) sprintf(" %+g", damage_modifier) else "",
                if (nzchar(as.character(meta$material %||% ""))) paste0(" • ", meta$material, " / ", meta$build_quality %||% "Unrated") else "",
                " • Qty: ", w$qty[[1]] %||% 1,
                " • ", w$weight[[1]] %||% 0, " lbs",
                " • ", w$value[[1]] %||% 0, "g"
              ),
              if (isTRUE(w$equipped[[1]]) && !isTRUE(w$in_bag[[1]])) tags$span(" • EQUIPPED"),
              if (nzchar(w$desc[[1]] %||% "")) tagList(tags$br(), w$desc[[1]])
            )
          ),
          div(
            class = "item-actions",
            actionButton(ns(paste0(p, "equip")), if (isTRUE(w$equipped[[1]]) && !isTRUE(w$in_bag[[1]])) "✅" else "⚔️"),
            actionButton(ns(paste0(p, "bag")), if (identical(mode, "active")) "🧳" else "↩︎"),
            actionButton(ns(paste0(p, "edit")), "✎"),
            actionButton(ns(paste0(p, "del")), "✖")
          )
        )
      )
    }
    
    armor_card <- function(a, mode = "active") {
      p <- paste0("itm_", a$id, "_")
      meta <- armor_meta_defaults_local(a$meta[[1]])
      this_ac <- armor_item_ac(a)
      
      if (isTRUE(a$edit)) {
        return(
          div(
            class = "item-card",
            textInput(ns(paste0(p, "name")), "Armor Name", value = a$name[[1]] %||% ""),
            textAreaInput(ns(paste0(p, "desc")), "Description", value = a$desc[[1]] %||% ""),
            fluidRow(
              column(4, numericInput(ns(paste0(p, "qty")), "Qty", value = a$qty[[1]] %||% 1, min = 1, step = 1)),
              column(4, numericInput(ns(paste0(p, "weight")), "Weight", value = a$weight[[1]] %||% 0, min = 0, step = 0.1)),
              column(4, numericInput(ns(paste0(p, "value")), "Value", value = a$value[[1]] %||% 0, min = 0, step = 1))
            ),
            fluidRow(
              column(4, numericInput(ns(paste0(p, "base_ac")), "Base AC", value = meta$base_ac, min = 0, step = 1)),
              column(4, selectInput(ns(paste0(p, "armor_type")), "Armor Type", choices = COMBAT_ARMOR_TYPES, selected = meta$type)),
              column(4, numericInput(ns(paste0(p, "custom_max_dex")), "Custom Max Dex", value = meta$custom_max_dex, step = 1))
            ),
            fluidRow(
              column(6, selectInput(ns(paste0(p, "material")), "Material", c("Copper","Iron","Steel","Titanium Copper","Wood"), selected = meta$material %||% "Steel")),
              column(6, selectInput(ns(paste0(p, "build_quality")), "Build quality", c("Very-Poorly-Crafted","Poorly-Crafted","Passably-Crafted","Bog-Standard","Well-Crafted","Master-Crafted"), selected = meta$build_quality %||% "Bog-Standard"))
            ),
            checkboxInput(ns(paste0(p, "proficient")), "Proficient", value = isTRUE(meta$proficient)),
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
            div(class = "item-title", paste0("🛡️ ", a$name[[1]] %||% "Armor")),
            div(
              class = "item-sub",
              paste0(
                "AC: ", round(this_ac, 0),
                " • Base AC: ", meta$base_ac,
                " • Type: ", meta$type,
                if (nzchar(as.character(meta$material %||% ""))) paste0(" • ", meta$material, " / ", meta$build_quality %||% "Unrated") else "",
                " • Qty: ", a$qty[[1]] %||% 1,
                " • ", a$weight[[1]] %||% 0, " lbs",
                " • ", a$value[[1]] %||% 0, "g"
              ),
              if (isTRUE(a$equipped[[1]]) && !isTRUE(a$in_bag[[1]])) tags$span(" • WORN"),
              if (nzchar(a$desc[[1]] %||% "")) tagList(tags$br(), a$desc[[1]])
            )
          ),
          div(
            class = "item-actions",
            actionButton(ns(paste0(p, "equip")), if (isTRUE(a$equipped[[1]]) && !isTRUE(a$in_bag[[1]])) "✅" else "🛡️"),
            actionButton(ns(paste0(p, "bag")), if (identical(mode, "active")) "🧳" else "↩︎"),
            actionButton(ns(paste0(p, "edit")), "✎"),
            actionButton(ns(paste0(p, "del")), "✖")
          )
        )
      )
    }
    
    # --------------------------------
    # Outputs
    # --------------------------------
    output$weapons_active_ui <- renderUI({
      df <- get_items_by_type("weapon", active_only = TRUE)
      if (!nrow(df)) return(tags$em("No ready weapons."))
      tagList(lapply(seq_len(nrow(df)), function(i) weapon_card(df[i, , drop = FALSE], "active")))
    })
    
    output$weapons_bag_ui <- renderUI({
      df <- get_items_by_type("weapon", active_only = FALSE)
      if (!nrow(df)) return(tags$em("Bag is empty."))
      
      in_bag <- as.logical(df$in_bag)
      in_bag[is.na(in_bag)] <- FALSE
      df <- df[in_bag, , drop = FALSE]
      
      if (!nrow(df)) return(tags$em("Bag is empty."))
      tagList(lapply(seq_len(nrow(df)), function(i) weapon_card(df[i, , drop = FALSE], "bag")))
    })
    
    output$armor_active_ui <- renderUI({
      df <- get_items_by_type("armor", active_only = TRUE)
      if (!nrow(df)) return(tags$em("No worn armor."))
      tagList(lapply(seq_len(nrow(df)), function(i) armor_card(df[i, , drop = FALSE], "active")))
    })
    
    output$armor_bag_ui <- renderUI({
      df <- get_items_by_type("armor", active_only = FALSE)
      if (!nrow(df)) return(tags$em("Bag is empty."))
      
      in_bag <- as.logical(df$in_bag)
      in_bag[is.na(in_bag)] <- FALSE
      df <- df[in_bag, , drop = FALSE]
      
      if (!nrow(df)) return(tags$em("Bag is empty."))
      tagList(lapply(seq_len(nrow(df)), function(i) armor_card(df[i, , drop = FALSE], "bag")))
    })
    
    # --------------------------------
    # Dynamic observers
    # --------------------------------
    observeEvent(get_inventory(), {
      df <- get_inventory()
      if (!nrow(df)) return()
      
      armory_df <- df[df$type %in% c("weapon", "armor"), , drop = FALSE]
      if (!nrow(armory_df)) return()
      
      new_ids <- setdiff(armory_df$id, obs_ids())
      if (!length(new_ids)) return()
      
      for (id in new_ids) {
        local({
          iid <- id
          p <- paste0("itm_", iid, "_")
          
          get_row <- function() {
            d <- get_inventory()
            d[d$id == iid, , drop = FALSE]
          }
          
          set_row <- function(r) {
            d <- get_inventory()
            idx <- which(d$id == iid)
            if (length(idx) != 1) return()
            d[idx, ] <- r
            write_inventory(d)
          }
          
          observeEvent(input[[paste0(p, "del")]], {
            d <- get_inventory()
            d <- d[d$id != iid, , drop = FALSE]
            write_inventory(d)
            safe_log("🗑️ Armory item removed.", toast = TRUE)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p, "bag")]], {
            r <- get_row()
            if (nrow(r) != 1) return()
            
            r$in_bag <- !isTRUE(r$in_bag[[1]])
            if (isTRUE(r$in_bag[[1]])) {
              r$equipped <- FALSE
            }
            set_row(r)
          }, ignoreInit = TRUE)
          
          observeEvent(input[[paste0(p, "equip")]], {
            r <- get_row()
            if (nrow(r) != 1) return()
            
            d <- get_inventory()
            idx <- which(d$id == iid)
            if (length(idx) != 1) return()
            
            this_type <- as.character(r$type[[1]] %||% "")
            
            if (identical(this_type, "armor")) {
              same_type_idx <- which(d$type == "armor")
              if (length(same_type_idx)) {
                d$equipped[same_type_idx] <- FALSE
              }
              d$equipped[idx] <- TRUE
              d$in_bag[idx] <- FALSE
              write_inventory(d)
              safe_log("🛡️ Armor equipped.", toast = TRUE)
              
            } else if (identical(this_type, "weapon")) {
              equipped_weapon_count <- sum(d$type == "weapon" & as.logical(d$equipped) & !as.logical(d$in_bag), na.rm = TRUE)
              if (!isTRUE(d$equipped[idx]) && equipped_weapon_count >= 2L) {
                safe_log("⚠️ You can ready a maximum of two weapons. Stow one first.", toast = TRUE)
                return()
              }
              d$equipped[idx] <- !isTRUE(d$equipped[idx])
              if (isTRUE(d$equipped[idx])) d$in_bag[idx] <- FALSE
              write_inventory(d)
              safe_log(
                if (isTRUE(d$equipped[idx])) "🗡️ Weapon readied." else "↩️ Weapon stowed.",
                toast = TRUE
              )
            }
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
            
            this_type <- as.character(r$type[[1]] %||% "")
            
            r$name   <- as.character(input[[paste0(p, "name")]] %||% "")
            r$desc   <- as.character(input[[paste0(p, "desc")]] %||% "")
            r$qty    <- suppressWarnings(as.numeric(input[[paste0(p, "qty")]] %||% 1))
            r$weight <- suppressWarnings(as.numeric(input[[paste0(p, "weight")]] %||% 0))
            r$value  <- suppressWarnings(as.numeric(input[[paste0(p, "value")]] %||% 0))
            r$edit   <- FALSE
            
            if (is.na(r$qty) || r$qty < 1) r$qty <- 1
            if (is.na(r$weight) || r$weight < 0) r$weight <- 0
            if (is.na(r$value) || r$value < 0) r$value <- 0
            
            meta <- r$meta[[1]] %||% list()
            
            if (identical(this_type, "weapon")) {
              meta <- weapon_meta_defaults_local(meta)
              meta$stat <- as.character(input[[paste0(p, "stat")]] %||% "str")
              meta$to_hit_bonus <- suppressWarnings(as.numeric(input[[paste0(p, "to_hit_bonus")]] %||% 0))
              meta$adv <- as.character(input[[paste0(p, "adv")]] %||% "Normal")
              meta$damage1 <- as.character(input[[paste0(p, "damage1")]] %||% "1d6")
              meta$dmg_type1 <- as.character(input[[paste0(p, "dmg_type1")]] %||% "Slashing")
              if(nzchar(as.character(meta$glyph_damage%||%""))){meta$glyph_damage<-as.character(input[[paste0(p,"damage2")]]%||%meta$glyph_damage);meta$glyph_damage_type<-as.character(input[[paste0(p,"dmg_type2")]]%||%meta$glyph_damage_type)}else{meta$damage2 <- as.character(input[[paste0(p, "damage2")]] %||% "");meta$dmg_type2 <- as.character(input[[paste0(p, "dmg_type2")]] %||% "Other")}
              meta$material <- as.character(input[[paste0(p, "material")]] %||% "Steel")
              meta$build_quality <- as.character(input[[paste0(p, "build_quality")]] %||% "Bog-Standard")
              meta$material_id <- NULL; meta$condition_id <- NULL
              meta$material_attack_bonus <- NULL; meta$material_damage_modifier <- NULL
              meta$quality_attack_bonus <- NULL; meta$quality_damage_modifier <- NULL
              meta <- utils::modifyList(meta, lookup_equipment_provenance(meta$material, meta$build_quality))
              meta$proficient <- isTRUE(input[[paste0(p, "proficient")]])
            }
            
            if (identical(this_type, "armor")) {
              meta <- armor_meta_defaults_local(meta)
              meta$base_ac <- suppressWarnings(as.numeric(input[[paste0(p, "base_ac")]] %||% 11))
              meta$type <- as.character(input[[paste0(p, "armor_type")]] %||% "Light")
              meta$custom_max_dex <- suppressWarnings(as.numeric(input[[paste0(p, "custom_max_dex")]] %||% 0))
              meta$material <- as.character(input[[paste0(p, "material")]] %||% "Steel")
              meta$build_quality <- as.character(input[[paste0(p, "build_quality")]] %||% "Bog-Standard")
              meta$material_id <- NULL; meta$condition_id <- NULL
              meta$material_armour_modifier <- NULL; meta$quality_armour_modifier <- NULL
              meta <- utils::modifyList(meta, lookup_equipment_provenance(meta$material, meta$build_quality, meta$type))
              meta$proficient <- isTRUE(input[[paste0(p, "proficient")]])
            }
            
            r$meta <- list(meta)
            set_row(r)
            safe_log("💾 Armory item updated.", toast = TRUE)
          }, ignoreInit = TRUE)
        })
      }
      
      obs_ids(unique(c(obs_ids(), new_ids)))
    }, ignoreInit = TRUE)
    
    # --------------------------------
    # Add weapon
    # --------------------------------
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
        equipped = FALSE,
        in_bag = FALSE,
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
      
      write_inventory(rbind(df, new))
      safe_log("🗡️ Added weapon.", toast = TRUE)
    }, ignoreInit = TRUE)
    
    # --------------------------------
    # Add armor
    # --------------------------------
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
        equipped = FALSE,
        in_bag = FALSE,
        meta = I(list(list(
          base_ac = 11,
          type = "Light",
          custom_max_dex = 0,
          proficient = TRUE
        ))),
        edit = TRUE,
        stringsAsFactors = FALSE
      )
      
      write_inventory(rbind(df, new))
      safe_log("🛡️ Added armor.", toast = TRUE)
    }, ignoreInit = TRUE)
    
    observeEvent(char_rev(), {
      if (isTRUE(restoring())) return()
      obs_ids(character())
    }, ignoreInit = TRUE)
  })
}
