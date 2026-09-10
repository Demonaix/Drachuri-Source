library(shiny)

controlInventoryUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "control-card",
        div(class = "control-section-title", "Control Inventory"),
        p(class = "control-mini", "Select a row to inspect it. Edit opens the definition form; Give sends a copy while retaining the catalogue entry."),
        div(style="display:none",selectInput(ns("item_id"),"Selected item",choices=character())),
        tabsetPanel(id=ns("catalogue_tab"),
          tabPanel("All",DT::DTOutput(ns("cat_all"))),tabPanel("Weapons",DT::DTOutput(ns("cat_weapons"))),tabPanel("Armour",DT::DTOutput(ns("cat_armour"))),tabPanel("Animals",DT::DTOutput(ns("cat_animals"))),
          tabPanel("Food",DT::DTOutput(ns("cat_food"))),tabPanel("Magical",DT::DTOutput(ns("cat_magical"))),tabPanel("Mundane",DT::DTOutput(ns("cat_mundane"))),
          tabPanel("Crafting",DT::DTOutput(ns("cat_crafting"))),tabPanel("Consumables",DT::DTOutput(ns("cat_consumable"))),tabPanel("Blood & Hearts",DT::DTOutput(ns("cat_biological"))),tabPanel("Tools",DT::DTOutput(ns("cat_tool"))),tabPanel("Treasure & Quest",DT::DTOutput(ns("cat_special")))
        ),
        div(style="display:flex;gap:8px;align-items:flex-end;flex-wrap:wrap;margin-top:12px",actionButton(ns("new_item"),"New Definition",class="btn btn-primary"),actionButton(ns("edit_selected"),"Edit Selected",class="btn btn-default"),selectInput(ns("player_id"),NULL,choices=character(),width="260px"),actionButton(ns("give_item"),"Give Copy to Player",class="btn btn-success")),
        uiOutput(ns("item_preview"))
    ),
    shinyjs::hidden(div(id=ns("editor_panel"),class="control-card",div(class="control-section-title","Item Definition Editor"),fluidRow(column(6,
      textInput(ns("name"),"Name"),selectInput(ns("type"),"Definition type",c("Weapon"="weapon","Armour"="armor","Animal"="animal","Food"="food","Magical item"="magical_item","Mundane item"="mundane_loot","Crafting material"="crafting","Consumable"="consumable","Tool"="tool","Treasure"="treasure","Quest item"="quest")),conditionalPanel("!['mundane_loot','food','animal'].includes(input.type)",textInput(ns("pools"),"Pools",placeholder="barbarian, bandit, guard"),ns=ns),textInput(ns("desc"),"Description"),conditionalPanel("input.type == 'food'",fluidRow(column(6,numericInput(ns("ration_value"),"Rations supplied",1,min=1,max=20)),column(6,numericInput(ns("shelf_life_days"),"Fresh for days",3,min=0,max=365))),ns=ns),conditionalPanel("input.type == 'animal'",textInput(ns("animal_species"),"Species","Horse"),fluidRow(column(3,numericInput(ns("animal_speed"),"Speed (ft)",40,min=5)),column(3,numericInput(ns("animal_ac"),"AC",10,min=1)),column(3,numericInput(ns("animal_hp"),"Maximum HP",10,min=1)),column(3,checkboxInput(ns("animal_mountable"),"Mountable",TRUE))),ns=ns),fluidRow(column(4,numericInput(ns("value"),"Gold value",0,min=0)),column(4,numericInput(ns("weight"),"Weight",0,min=0)),column(4,textInput(ns("damage"),"Damage / Base AC"))),actionButton(ns("save_item"),"Add to Catalogue",class="btn btn-primary"),actionButton(ns("update_item"),"Update Selected",class="btn btn-default")),column(6,
      selectInput(ns("stat"),"Weapon stat / armour class",c("STR"="str","DEX"="dex","Light"="Light","Medium"="Medium","Heavy"="Heavy","Shield"="Shield")),conditionalPanel("input.type == 'armor'",fluidRow(column(6,selectInput(ns("equipment_slot"),"Equipment slot",c("Body armour"="body","Shield"="shield","Headgear"="head","Accessory"="accessory"))),column(6,numericInput(ns("ac_bonus"),"Additive AC bonus",0,min=0,max=5))),p(class="control-mini","Body armour uses Base AC. Shields, helms and accessories add only the best bonus from their slot."),ns=ns),fluidRow(column(4,selectInput(ns("damage_type_1"),"Primary damage type",choices=c(enemy_damage_types(),"other"))),column(4,textInput(ns("damage_2"),"Extra damage",placeholder="e.g. 1d4")),column(4,selectInput(ns("damage_type_2"),"Extra damage type",choices=c(enemy_damage_types(),"other")))),checkboxInput(ns("lock_provenance"),"Lock this weapon to one material and build quality",FALSE),fluidRow(column(6,selectInput(ns("locked_material"),"Locked material",c("Copper","Iron","Steel","Titanium Copper","Wood"))),column(6,selectInput(ns("locked_quality"),"Locked build quality",c("Very-Poorly-Crafted","Poorly-Crafted","Passably-Crafted","Bog-Standard","Well-Crafted","Master-Crafted"))))))))
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
    concentrations <- c(Diluted=10L,Standard=25L,Potent=50L,Concentrated=100L)
    blood_stock <- Map(function(label,sindre) list(
      id=paste0("stored_blood_",tolower(label)),
      name=paste(label,"Stored Blood"),
      type="item",desc=paste0("A sealed one-pint bottle containing ",sindre," Sindre."),value=0,weight=1,qty=1,
      meta=list(catalogue_id="stored_blood_pint",category="blood",source="DM blood reserve",sindre_per_unit=as.numeric(sindre)),pools=character()
    ),names(concentrations),unname(concentrations))
    seed <- c(seed, blood_stock)
    catalogue <- reactiveVal({
      saved <- if (file.exists(store_path)) tryCatch(readRDS(store_path), error=function(e) list()) else list()
      saved <- lapply(saved,function(x){if((x$type%||%"")=="weapon"&&!isTRUE(x$meta$lock_provenance)){x$name<-sub("^(Titanium Copper|Steel|Iron|Copper)[[:space:]]+","",x$name,ignore.case=TRUE);x$desc<-gsub("\\b(steel|iron|copper)[- ]?(headed|bladed)?[[:space:]]*","",x$desc,ignore.case=TRUE);x$meta$material<-NULL;x$meta$build_quality<-NULL};x})
      db_items<-get_control_catalogue_definitions();existing<-vapply(saved,function(x)as.character(x$id%||%""),character(1));merged<-c(saved,Filter(function(x)!as.character(x$id%||%"")%in%existing,db_items));existing<-vapply(merged,function(x)as.character(x$id%||%""),character(1))
      c(merged,Filter(function(x)!x$id%in%existing,seed))
    })
    item_choice_signature <- reactiveVal(NULL)
    player_choice_signature <- reactiveVal(NULL)
    save_store <- function(x) tryCatch({ saveRDS(x, store_path); TRUE }, error=function(e) FALSE)
    item_category<-function(x){if(identical(x$type,"weapon"))"weapon"else if((x$type%||%"")%in%c("armor","armour"))"armor"else if(identical(x$type,"animal"))"animal"else as.character(x$meta$category%||%if(identical(x$type,"consumable"))"consumable"else"mundane_loot")}
    table_items<-function(key){items<-catalogue();Filter(function(x){magic<-isTRUE((x$meta%||%list())$is_magical)||item_category(x)=="magical_item";switch(key,all=TRUE,weapons=item_category(x)=="weapon",armour=item_category(x)=="armor",animals=item_category(x)=="animal",food=item_category(x)=="food",magical=magic,mundane=item_category(x)=="mundane_loot"&&!magic,crafting=item_category(x)=="crafting",consumable=item_category(x)=="consumable",biological=item_category(x)%in%c("blood","heart"),tool=item_category(x)=="tool",special=item_category(x)%in%c("treasure","quest"),FALSE)},items)}
    table_frame<-function(key){
      items<-table_items(key);if(!length(items))return(data.frame())
      do.call(rbind,lapply(items,function(x){
        m<-x$meta%||%list();category<-item_category(x)
        detail<-if(category=="weapon")paste(m$damage1%||%"1d4",m$dmg_type1%||%"other",toupper(m$stat%||%"str"))else if(category=="armor"){slot<-m$equipment_slot%||%if(identical(m$type,"Shield"))"shield"else"body";if(slot=="body")paste0("AC ",m$base_ac%||%"—"," · ",m$type%||%"Armour")else paste0(tools::toTitleCase(slot)," · +",m$ac_bonus%||%0," AC")}else if(category=="animal")paste0(m$species%||%"Animal"," · Speed ",m$speed%||%30," · AC ",m$armour_class%||%10," · HP ",m$max_hp%||%5," · ",if(isTRUE(m$mountable))"Mountable"else"Unmountable")else as.character(x$desc%||%"")
        pools<-if(category%in%c("mundane_loot","food"))"Automatic (non-animal NPCs)"else paste(x$pools%||%character(),collapse=", ")
        data.frame(Name=x$name%||%"Item",Type=gsub("_"," ",category),Value=as.numeric(x$value%||%0),Weight=as.numeric(x$weight%||%0),Characteristics=detail,Pools=pools,stringsAsFactors=FALSE)
      }))
    }
    for(key in c("all","weapons","armour","animals","food","magical","mundane","crafting","consumable","biological","tool","special"))local({k<-key;output[[paste0("cat_",k)]]<-DT::renderDT(DT::datatable(table_frame(k),selection="single",rownames=FALSE,options=list(pageLength=10,scrollX=TRUE)));observeEvent(input[[paste0("cat_",k,"_rows_selected")]],{idx<-input[[paste0("cat_",k,"_rows_selected")]];items<-table_items(k);if(length(idx)&&idx<=length(items))updateSelectInput(session,"item_id",selected=as.character(items[[idx]]$id))},ignoreInit=TRUE)})
    observeEvent(input$edit_selected,{if(is.null(selected()))return(showNotification("Select an item row first.",type="warning"));shinyjs::show("editor_panel")},ignoreInit=TRUE)
    observeEvent(input$new_item,{shinyjs::show("editor_panel");updateTextInput(session,"name",value="New Item");updateSelectInput(session,"type",selected="mundane_loot");updateTextInput(session,"desc",value="");updateTextInput(session,"pools",value="");updateNumericInput(session,"value",value=0);updateNumericInput(session,"weight",value=0)},ignoreInit=TRUE)
    observe({
      items<-catalogue();vals<-vapply(items,`[[`,"","id");labels<-vapply(items,`[[`,"","name");sig<-paste(vals,labels,collapse="|");if(identical(sig,item_choice_signature()))return();item_choice_signature(sig);keep<-isolate(input$item_id)%||%""
      updateSelectInput(session,"item_id",choices=setNames(vals,labels),selected=if(keep%in%vals)keep else character())
    })
    selected <- reactive({ found<-Filter(function(x) identical(x$id, input$item_id %||% ""), catalogue()); if(length(found)) found[[1]] else NULL })
    output$item_preview <- renderUI({ x<-selected();if(is.null(x))return(NULL);catg<-item_category(x);pool_text<-if(catg%in%c("mundane_loot","food"))"Automatic random loot for non-animal NPCs"else paste(x$pools%||%"none",collapse=", ");food_text<-if(catg=="food")paste0(" • ",x$meta$ration_value%||%1," ration(s) • fresh ",x$meta$shelf_life_days%||%3," day(s)")else"";tagList(h4(x$name),p(x$desc),tags$strong(paste0(gsub("_"," ",catg)," • ",x$value," gold • ",x$weight," lb",food_text)),p(paste("Pools:",pool_text))) })
    observeEvent(input$item_id, { x<-selected(); if(is.null(x))return();catg<-item_category(x);updateTextInput(session,"name",value=x$name);updateSelectInput(session,"type",selected=catg);updateTextInput(session,"pools",value=paste(x$pools%||%character(),collapse=", "));updateTextInput(session,"desc",value=x$desc);updateNumericInput(session,"ration_value",value=as.integer(x$meta$ration_value%||%1L));updateNumericInput(session,"shelf_life_days",value=as.integer(x$meta$shelf_life_days%||%3L));updateTextInput(session,"animal_species",value=as.character(x$meta$species%||%"Animal"));updateNumericInput(session,"animal_speed",value=as.integer(x$meta$speed%||%30L));updateNumericInput(session,"animal_ac",value=as.integer(x$meta$armour_class%||%10L));updateNumericInput(session,"animal_hp",value=as.integer(x$meta$max_hp%||%5L));updateCheckboxInput(session,"animal_mountable",value=isTRUE(x$meta$mountable));updateNumericInput(session,"value",value=x$value);updateNumericInput(session,"weight",value=x$weight); updateTextInput(session,"damage",value=as.character(x$meta$damage1%||%x$meta$base_ac%||%"")); updateSelectInput(session,"stat",selected=as.character(x$meta$stat%||%x$meta$type%||%"str"));updateSelectInput(session,"equipment_slot",selected=as.character(x$meta$equipment_slot%||%if(identical(x$meta$type,"Shield"))"shield"else"body"));updateNumericInput(session,"ac_bonus",value=as.integer(x$meta$ac_bonus%||%if(identical(x$meta$type,"Shield"))2L else 0L));updateSelectInput(session,"damage_type_1",selected=tolower(as.character(x$meta$dmg_type1%||%"other")));updateTextInput(session,"damage_2",value=as.character(x$meta$damage2%||%""));updateSelectInput(session,"damage_type_2",selected=tolower(as.character(x$meta$dmg_type2%||%"other")));updateCheckboxInput(session,"lock_provenance",value=isTRUE(x$meta$lock_provenance));updateSelectInput(session,"locked_material",selected=as.character(x$meta$material%||%"Steel"));updateSelectInput(session,"locked_quality",selected=as.character(x$meta$build_quality%||%"Bog-Standard")) },ignoreInit=TRUE)
    observe({
      ctrl$refresh_key; sid<-suppressWarnings(as.integer(ctrl$session_id%||%NA))
      p <- if(!is.na(sid))tryCatch(get_session_players(sid),error=function(e)data.frame()) else if (is.reactive(players_tbl)) players_tbl() else data.frame()
      if(!is.data.frame(p)||!nrow(p)){if(!identical(player_choice_signature(),"")){player_choice_signature("");updateSelectInput(session,"player_id",choices=character())};return()}
      ids<-as.character(p$character_id%||%p$id); names<-as.character(p$char_name%||%p$name%||%ids)
      labels<-as.character(p$display_name%||%names);labels[is.na(labels)|!nzchar(labels)]<-names[is.na(labels)|!nzchar(labels)]
      sig<-paste(ids,labels,collapse="|");if(identical(sig,player_choice_signature()))return();player_choice_signature(sig)
      keep<-isolate(input$player_id%||%"");updateSelectInput(session,"player_id",choices=setNames(ids,labels),selected=if(keep%in%ids)keep else ids[[1L]])
    })
    observeEvent(input$save_item, {
      nm<-trimws(input$name%||%""); if(!nzchar(nm)) return(showNotification("Enter an item name.",type="error"))
      chosen_type<-input$type%||%"mundane_loot";typ<-if(chosen_type%in%c("weapon","armor","animal"))chosen_type else if(chosen_type%in%c("food","consumable"))"consumable"else"item";raw<-trimws(input$damage%||%"");meta<-list()
      if(typ=="weapon") meta<-list(stat=input$stat%||%"str",adv="Normal",to_hit_bonus=0,damage1=if(nzchar(raw))raw else "1d4",dmg_type1=input$damage_type_1%||%"other",damage2=trimws(input$damage_2%||%""),dmg_type2=input$damage_type_2%||%"other",proficient=TRUE,lock_provenance=isTRUE(input$lock_provenance),material=if(isTRUE(input$lock_provenance))input$locked_material else NULL,build_quality=if(isTRUE(input$lock_provenance))input$locked_quality else NULL)
      if(typ=="armor") { ac<-suppressWarnings(as.numeric(raw)); if(is.na(ac)) ac<-if((input$equipment_slot%||%"body")=="body")11 else 0; meta<-list(base_ac=ac,type=input$stat%||%"Light",custom_max_dex=if((input$stat%||%"")=="Medium")2 else 0,proficient=TRUE,equipment_slot=input$equipment_slot%||%"body",ac_bonus=as.integer(input$ac_bonus%||%0L)) }
      if(typ=="animal")meta<-list(category="animal",species=trimws(input$animal_species%||%"Animal"),speed=as.integer(input$animal_speed%||%30L),armour_class=as.integer(input$animal_ac%||%10L),max_hp=as.integer(input$animal_hp%||%5L),mountable=isTRUE(input$animal_mountable))
      if(!typ%in%c("weapon","armor","animal")){meta$category<-chosen_type;if(chosen_type=="food"){meta$ration_value<-as.integer(input$ration_value%||%1L);meta$shelf_life_days<-as.integer(input$shelf_life_days%||%3L)}}
      pools<-if(chosen_type%in%c("mundane_loot","food","animal"))character()else trimws(strsplit(input$pools%||%"",",",fixed=TRUE)[[1]])
      x<-list(id=paste0("custom_",format(Sys.time(),"%Y%m%d%H%M%S")),name=nm,type=typ,desc=input$desc%||%"",value=as.numeric(input$value%||%0),weight=as.numeric(input$weight%||%0),qty=1,meta=meta,pools=pools[nzchar(pools)])
      items<-c(catalogue(),list(x)); catalogue(items); local_ok<-save_store(items); db_ok<-save_control_catalogue_definition(x); if(!local_ok||!db_ok)return(showNotification("The catalogue item could not be saved everywhere.",type="error")); showNotification("Added to control catalogue.")
    })
    observeEvent(input$update_item, {
      old<-selected();if(is.null(old))return();chosen_type<-input$type%||%item_category(old);typ<-if(chosen_type%in%c("weapon","armor","animal"))chosen_type else if(chosen_type%in%c("food","consumable"))"consumable"else"item";raw<-trimws(input$damage%||%"");meta<-if(typ==old$type)old$meta%||%list()else list()
      if(typ=="weapon"){meta$stat<-input$stat%||%"str";meta$damage1<-if(nzchar(raw))raw else "1d4";meta$dmg_type1<-input$damage_type_1%||%"other";meta$damage2<-trimws(input$damage_2%||%"");meta$dmg_type2<-input$damage_type_2%||%"other";meta$lock_provenance<-isTRUE(input$lock_provenance);meta$material<-if(isTRUE(input$lock_provenance))input$locked_material else NULL;meta$build_quality<-if(isTRUE(input$lock_provenance))input$locked_quality else NULL}
      if(typ=="armor"){ac<-suppressWarnings(as.numeric(raw));if(!is.na(ac))meta$base_ac<-ac;meta$type<-input$stat%||%"Light";meta$equipment_slot<-input$equipment_slot%||%"body";meta$ac_bonus<-as.integer(input$ac_bonus%||%0L)}
      if(typ=="animal")meta<-list(category="animal",species=trimws(input$animal_species%||%"Animal"),speed=as.integer(input$animal_speed%||%30L),armour_class=as.integer(input$animal_ac%||%10L),max_hp=as.integer(input$animal_hp%||%5L),mountable=isTRUE(input$animal_mountable))
      if(!typ%in%c("weapon","armor","animal")){meta$category<-chosen_type;if(chosen_type=="food"){meta$ration_value<-as.integer(input$ration_value%||%1L);meta$shelf_life_days<-as.integer(input$shelf_life_days%||%3L)}}
      old$name<-trimws(input$name%||%old$name);old$type<-typ;old$pools<-if(chosen_type%in%c("mundane_loot","food","animal"))character()else trimws(strsplit(input$pools%||%"",",",fixed=TRUE)[[1]]);old$pools<-old$pools[nzchar(old$pools)];old$desc<-input$desc%||%"";old$value<-as.numeric(input$value%||%0);old$weight<-as.numeric(input$weight%||%0);old$meta<-meta
      items<-catalogue();idx<-which(vapply(items,function(x)identical(x$id,old$id),logical(1)))[1];items[[idx]]<-old;catalogue(items);local_ok<-save_store(items);db_ok<-save_control_catalogue_definition(old);if(!local_ok||!db_ok)return(showNotification("The catalogue item could not be updated everywhere.",type="error"));showNotification("Catalogue item updated.")
    })
    observeEvent(input$give_item, {
      x<-selected(); cid<-as.character(input$player_id%||%""); if(is.null(x)||!nzchar(cid)) return(showNotification("Choose an item and player.",type="error"))
      if(identical(x$type,"animal")){result<-give_animal_to_character(cid,x$id);if(is.null(result)||!nrow(result))return(showNotification("The animal could not be added to the player's stable.",type="error"));queue_character_refresh(cid,"control_animal",paste0("The DM added ",x$name," to your stable."));return(showNotification(paste("Added",x$name,"to the player's stable."),type="message"))}
      char<-load_character_from_db(cid); if(is.null(char)) return(showNotification("Could not load player.",type="error"))
      char<-validate_character(char); row<-enemy_loot_to_inventory_row(x,id=paste0("gift_",as.integer(Sys.time()),"_",sample(1000:9999,1)))
      char$inventory$items<-inventory_normalize(rbind(inventory_normalize(char$inventory$items),row))
      if(is.null(save_character_to_db(char,cid))) return(showNotification("Could not save gift.",type="error"))
      assignment_failed<-(x$type%||%"")%in%c("weapon","armor","armour")&&is.null(roll_equipment_assignment(cid,row$id[[1]],"control"))
      if(!queue_character_refresh(cid,"control_gift",paste0("The DM gave you ",x$name,".")))return(showNotification("The item was saved, but the player refresh notice failed. It will appear after reconnecting.",type="warning"))
      if(assignment_failed)return(showNotification("The gift was added, but its material/build roll failed.",type="warning"))
      if(is.function(bump_refresh)) bump_refresh(); showNotification(paste("Gave",x$name,"to",char$meta$name%||%"player"),type="message")
    })
  })
}
