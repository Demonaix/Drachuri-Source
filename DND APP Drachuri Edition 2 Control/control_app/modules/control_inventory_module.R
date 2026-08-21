library(shiny)

controlInventoryUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "control-card",
        div(class = "control-section-title", "Control Inventory"),
        p(class = "control-mini", "Reusable master catalogue. Giving an item sends a copy; the control item remains."),
        fluidRow(
          column(5, fluidRow(column(6, selectInput(ns("pool"), "Pool", choices = "All")), column(6, selectInput(ns("type_filter"), "Type", choices = "All"))),
                 selectInput(ns("item_id"), "Catalogue item", choices = character()),
                 uiOutput(ns("item_preview"))),
          column(7,
                 textInput(ns("name"), "Name"),
                 selectInput(ns("type"), "Type", c("Weapon"="weapon", "Armour"="armor", "Item"="item", "Consumable"="consumable")),
                 textInput(ns("pools"), "Pools", placeholder = "barbarian, bandit, guard"),
                 textInput(ns("desc"), "Description"),
                 selectInput(ns("category"), "Item category", choices=c("Mundane loot"="mundane_loot","Consumable"="consumable","Crafting material"="crafting","Tool"="tool","Treasure"="treasure","Quest item"="quest")),
                 fluidRow(column(4, numericInput(ns("value"), "Gold value", 0, min=0)), column(4, numericInput(ns("weight"), "Weight", 0, min=0)), column(4, textInput(ns("damage"), "Damage / Base AC"))),
                 selectInput(ns("stat"), "Weapon stat / armour class", c("STR"="str", "DEX"="dex", "Light"="Light", "Medium"="Medium", "Heavy"="Heavy", "Shield"="Shield")),
                 fluidRow(
                   column(4, selectInput(ns("damage_type_1"), "Primary damage type", choices = c(enemy_damage_types(), "other"))),
                   column(4, textInput(ns("damage_2"), "Extra damage", placeholder = "e.g. 1d4")),
                   column(4, selectInput(ns("damage_type_2"), "Extra damage type", choices = c(enemy_damage_types(), "other")))
                 ),
                 checkboxInput(ns("lock_provenance"), "Lock this weapon to one material and build quality", FALSE),
                 fluidRow(
                   column(6, selectInput(ns("locked_material"), "Locked material", c("Copper","Iron","Steel","Titanium Copper","Wood"))),
                   column(6, selectInput(ns("locked_quality"), "Locked build quality", c("Very-Poorly-Crafted","Poorly-Crafted","Passably-Crafted","Bog-Standard","Well-Crafted","Master-Crafted")))
                 ),
                 actionButton(ns("save_item"), "Add to Catalogue", class="btn btn-primary"),
                 actionButton(ns("update_item"), "Update Selected", class="btn btn-default")))
    ),
    div(class = "control-card",
        div(class = "control-section-title", "Give Copy to Player"),
        selectInput(ns("player_id"), "Player", choices = character(), width="320px"),
        actionButton(ns("give_item"), "Give Selected Item", class="btn btn-success"))
  )
}

controlInventoryServer <- function(id, ctrl, players_tbl = NULL, bump_refresh = NULL) {
  moduleServer(id, function(input, output, session) {
    `%||%` <- get("%||%", inherits=TRUE)
    store_path <- file.path("control_app", "data", "control_inventory.rds")
    dir.create(dirname(store_path), recursive=TRUE, showWarnings=FALSE)
    default_pools <- list(shortsword=c("bandit","rogue","guard"), dagger=c("rogue","cultist","bandit"), battleaxe=c("barbarian"), spear=c("guard"), shortbow=c("bandit","ranger"),
                          leather_armor=c("bandit","rogue"), padded_armor="rogue", studded_leather=c("rogue","ranger"), hide_armor="barbarian", chain_shirt="guard",
                          scale_mail="fighter", breastplate="fighter", half_plate="fighter", ring_mail="guard", chain_mail="fighter", splint_armor="fighter", plate_armor="fighter", shield=c("guard","fighter"))
    seed <- lapply(names(enemy_loot_catalog()), function(key) {
      x <- enemy_loot_catalog()[[key]]; x$id <- key; x$pools <- default_pools[[key]] %||% character(); x
    })
    catalogue <- reactiveVal({
      saved <- if (file.exists(store_path)) tryCatch(readRDS(store_path), error=function(e) list()) else list()
      saved <- lapply(saved,function(x){if((x$type%||%"")=="weapon"&&!isTRUE(x$meta$lock_provenance)){x$name<-sub("^(Titanium Copper|Steel|Iron|Copper)[[:space:]]+","",x$name,ignore.case=TRUE);x$desc<-gsub("\\b(steel|iron|copper)[- ]?(headed|bladed)?[[:space:]]*","",x$desc,ignore.case=TRUE);x$meta$material<-NULL;x$meta$build_quality<-NULL};x})
      existing <- vapply(saved, function(x) as.character(x$id %||% ""), character(1))
      c(saved, Filter(function(x) !x$id %in% existing, seed))
    })
    save_store <- function(x) tryCatch({ saveRDS(x, store_path); TRUE }, error=function(e) FALSE)
    observe({
      items <- catalogue(); pools <- sort(unique(unlist(lapply(items, function(x) x$pools %||% character())))); types<-sort(unique(vapply(items,function(x)x$type,character(1))))
      updateSelectInput(session, "pool", choices=c("All", pools), selected=input$pool%||%"All")
      updateSelectInput(session, "type_filter", choices=c("All",types), selected=input$type_filter%||%"All")
      chosen <- input$pool %||% "All"; chosen_type<-input$type_filter%||%"All"; shown <- Filter(function(x) (identical(chosen,"All") || chosen %in% (x$pools %||% character())) && (identical(chosen_type,"All")||identical(x$type,chosen_type)), items)
      vals<-vapply(shown, `[[`, "", "id"); keep<-input$item_id%||%""; updateSelectInput(session, "item_id", choices=setNames(vals, vapply(shown, `[[`, "", "name")), selected=if(keep%in%vals)keep else if(length(vals))vals[1] else character())
    })
    selected <- reactive({ found<-Filter(function(x) identical(x$id, input$item_id %||% ""), catalogue()); if(length(found)) found[[1]] else NULL })
    output$item_preview <- renderUI({ x<-selected(); if(is.null(x)) return(NULL); tagList(h4(x$name), p(x$desc), tags$strong(paste(x$type,"•",x$value,"gold •",x$weight,"lb")), p(paste("Pools:",paste(x$pools%||%"none",collapse=", ")))) })
    observeEvent(input$item_id, { x<-selected(); if(is.null(x))return(); updateTextInput(session,"name",value=x$name);updateSelectInput(session,"type",selected=x$type);updateTextInput(session,"pools",value=paste(x$pools%||%character(),collapse=", "));updateTextInput(session,"desc",value=x$desc);updateSelectInput(session,"category",selected=as.character(x$meta$category%||%if(identical(x$type,"consumable"))"consumable"else"mundane_loot"));updateNumericInput(session,"value",value=x$value);updateNumericInput(session,"weight",value=x$weight); updateTextInput(session,"damage",value=as.character(x$meta$damage1%||%x$meta$base_ac%||%"")); updateSelectInput(session,"stat",selected=as.character(x$meta$stat%||%x$meta$type%||%"str"));updateSelectInput(session,"damage_type_1",selected=tolower(as.character(x$meta$dmg_type1%||%"other")));updateTextInput(session,"damage_2",value=as.character(x$meta$damage2%||%""));updateSelectInput(session,"damage_type_2",selected=tolower(as.character(x$meta$dmg_type2%||%"other")));updateCheckboxInput(session,"lock_provenance",value=isTRUE(x$meta$lock_provenance));updateSelectInput(session,"locked_material",selected=as.character(x$meta$material%||%"Steel"));updateSelectInput(session,"locked_quality",selected=as.character(x$meta$build_quality%||%"Bog-Standard")) },ignoreInit=TRUE)
    observe({
      ctrl$refresh_key; sid<-suppressWarnings(as.integer(ctrl$session_id%||%NA))
      p <- if(!is.na(sid))tryCatch(get_session_players(sid),error=function(e)data.frame()) else if (is.reactive(players_tbl)) players_tbl() else data.frame()
      if(!is.data.frame(p)||!nrow(p)) return(updateSelectInput(session,"player_id",choices=character()))
      ids<-as.character(p$character_id%||%p$id); names<-as.character(p$char_name%||%p$name%||%ids)
      labels<-as.character(p$display_name%||%names);labels[is.na(labels)|!nzchar(labels)]<-names[is.na(labels)|!nzchar(labels)]
      keep<-isolate(input$player_id%||%"");updateSelectInput(session,"player_id",choices=setNames(ids,labels),selected=if(keep%in%ids)keep else ids[[1L]])
    })
    observeEvent(input$save_item, {
      nm<-trimws(input$name%||%""); if(!nzchar(nm)) return(showNotification("Enter an item name.",type="error"))
      typ<-input$type%||%"item"; raw<-trimws(input$damage%||%""); meta<-list()
      if(typ=="weapon") meta<-list(stat=input$stat%||%"str",adv="Normal",to_hit_bonus=0,damage1=if(nzchar(raw))raw else "1d4",dmg_type1=input$damage_type_1%||%"other",damage2=trimws(input$damage_2%||%""),dmg_type2=input$damage_type_2%||%"other",proficient=TRUE,lock_provenance=isTRUE(input$lock_provenance),material=if(isTRUE(input$lock_provenance))input$locked_material else NULL,build_quality=if(isTRUE(input$lock_provenance))input$locked_quality else NULL)
      if(typ=="armor") { ac<-suppressWarnings(as.numeric(raw)); if(is.na(ac)) ac<-11; meta<-list(base_ac=ac,type=input$stat%||%"Light",custom_max_dex=if((input$stat%||%"")=="Medium")2 else 0,proficient=TRUE) }
      if(!typ%in%c("weapon","armor")) meta$category<-input$category%||%if(typ=="consumable")"consumable"else"mundane_loot"
      x<-list(id=paste0("custom_",format(Sys.time(),"%Y%m%d%H%M%S")),name=nm,type=typ,desc=input$desc%||%"",value=as.numeric(input$value%||%0),weight=as.numeric(input$weight%||%0),qty=1,meta=meta,pools=trimws(strsplit(input$pools%||%"",",",fixed=TRUE)[[1]])); x$pools<-x$pools[nzchar(x$pools)]
      items<-c(catalogue(),list(x)); catalogue(items); local_ok<-save_store(items); db_ok<-save_control_catalogue_definition(x); if(!local_ok||!db_ok)return(showNotification("The catalogue item could not be saved everywhere.",type="error")); showNotification("Added to control catalogue.")
    })
    observeEvent(input$update_item, {
      old<-selected(); if(is.null(old))return(); typ<-input$type%||%old$type; raw<-trimws(input$damage%||%""); meta<-old$meta%||%list()
      if(typ=="weapon"){meta$stat<-input$stat%||%"str";meta$damage1<-if(nzchar(raw))raw else "1d4";meta$dmg_type1<-input$damage_type_1%||%"other";meta$damage2<-trimws(input$damage_2%||%"");meta$dmg_type2<-input$damage_type_2%||%"other";meta$lock_provenance<-isTRUE(input$lock_provenance);meta$material<-if(isTRUE(input$lock_provenance))input$locked_material else NULL;meta$build_quality<-if(isTRUE(input$lock_provenance))input$locked_quality else NULL}
      if(typ=="armor"){ac<-suppressWarnings(as.numeric(raw));if(!is.na(ac))meta$base_ac<-ac;meta$type<-input$stat%||%"Light"}
      if(!typ%in%c("weapon","armor"))meta$category<-input$category%||%if(typ=="consumable")"consumable"else"mundane_loot"
      old$name<-trimws(input$name%||%old$name);old$type<-typ;old$pools<-trimws(strsplit(input$pools%||%"",",",fixed=TRUE)[[1]]);old$pools<-old$pools[nzchar(old$pools)];old$desc<-input$desc%||%"";old$value<-as.numeric(input$value%||%0);old$weight<-as.numeric(input$weight%||%0);old$meta<-meta
      items<-catalogue();idx<-which(vapply(items,function(x)identical(x$id,old$id),logical(1)))[1];items[[idx]]<-old;catalogue(items);local_ok<-save_store(items);db_ok<-save_control_catalogue_definition(old);if(!local_ok||!db_ok)return(showNotification("The catalogue item could not be updated everywhere.",type="error"));showNotification("Catalogue item updated.")
    })
    observeEvent(input$give_item, {
      x<-selected(); cid<-as.character(input$player_id%||%""); if(is.null(x)||!nzchar(cid)) return(showNotification("Choose an item and player.",type="error"))
      char<-load_character_from_db(cid); if(is.null(char)) return(showNotification("Could not load player.",type="error"))
      char<-validate_character(char); row<-enemy_loot_to_inventory_row(x,id=paste0("gift_",as.integer(Sys.time()),"_",sample(1000:9999,1)))
      char$inventory$items<-inventory_normalize(rbind(inventory_normalize(char$inventory$items),row))
      if(is.null(save_character_to_db(char,cid))) return(showNotification("Could not save gift.",type="error"))
      if((x$type%||%"")%in%c("weapon","armor","armour")&&is.null(roll_equipment_assignment(cid,row$id[[1]],"control")))return(showNotification("The gift was added, but its material/build roll failed.",type="warning"))
      if(is.function(bump_refresh)) bump_refresh(); showNotification(paste("Gave",x$name,"to",char$meta$name%||%"player"),type="message")
    })
  })
}
