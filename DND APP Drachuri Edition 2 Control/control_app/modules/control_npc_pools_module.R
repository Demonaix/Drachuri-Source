library(shiny)

npc_pool_rule <- function(item_id, chance=100, group="", required=FALSE) {
  list(item_id=item_id,chance=as.numeric(chance),group=group,required=isTRUE(required))
}

npc_default_pool_catalogue <- function() {
  catalogue_version <- 2L
  rule <- npc_pool_rule
  make_pool <- function(id,name,base_type,features=character(),rules=list(),abilities=NULL,
                        resistances=NULL,immunities=NULL,vulnerabilities=NULL,condition_immunities=NULL) {
    base <- enemy_generator_types()[[base_type]]
    list(
      id=id,name=name,base_type=base_type,features=features,catalogue_version=catalogue_version,
      abilities=abilities %||% base$abilities,
      resistances=resistances %||% base$resistances %||% character(),
      immunities=immunities %||% base$immunities %||% character(),
      vulnerabilities=vulnerabilities %||% base$vulnerabilities %||% character(),
      condition_immunities=condition_immunities %||% base$condition_immunities %||% character(),
      rules=rules
    )
  }
  list(
    make_pool("custom","Custom Enemy","Custom"),
    make_pool("bandit","Bandit Raider","Bandit",rules=list(
      rule("leather_armor"),rule("shortsword",70,"sidearm",TRUE),rule("dagger",30,"sidearm",TRUE),
      rule("shortbow",45),rule("healing_draught",5))),
    make_pool("forest_outlaw","Forest Outlaw","Bandit",c("Archer"),list(
      rule("padded_armor",65,"outlaw_armour",TRUE),rule("studded_leather",35,"outlaw_armour",TRUE),
      rule("shortbow"),rule("dagger",75),rule("healing_draught",8))),
    make_pool("goblin_skirmisher","Goblin Skirmisher","Bandit",c("Speedy"),list(
      rule("padded_armor"),rule("dagger"),rule("shortbow",70),rule("shield",15))),
    make_pool("drachuri_reaver","Drachuri Clan Reaver","Bandit",c("Barbarian","Brute"),list(
      rule("hide_armor"),rule("battleaxe",65,"reaver_weapon",TRUE),rule("spear",35,"reaver_weapon",TRUE),
      rule("shield",25),rule("healing_draught",8))),
    make_pool("guard","Town Guard","Guard",rules=list(
      rule("chain_shirt"),rule("spear"),rule("shield",65),rule("dagger",25),rule("healing_draught",5))),
    make_pool("veteran_guard","Veteran House Guard","Guard",c("Boss","Armoured"),list(
      rule("chain_mail"),rule("spear",45,"guard_weapon",TRUE),rule("shortsword",55,"guard_weapon",TRUE),
      rule("shield",80),rule("healing_draught",15))),
    make_pool("mercenary","Mercenary Soldier","Guard",rules=list(
      rule("hide_armor",35,"mercenary_armour",TRUE),rule("scale_mail",65,"mercenary_armour",TRUE),
      rule("spear",40,"mercenary_weapon",TRUE),rule("shortsword",35,"mercenary_weapon",TRUE),
      rule("battleaxe",25,"mercenary_weapon",TRUE),rule("shield",50),rule("healing_draught",10))),
    make_pool("cultist","Blood Cult Acolyte","Cultist",rules=list(
      rule("leather_armor"),rule("dagger"),rule("bone_fragment",45),rule("healing_draught",10))),
    make_pool("blood_priest","Blood Cult Priest","Cultist",c("Spellcaster","Boss"),list(
      rule("studded_leather"),rule("dagger"),rule("bone_fragment",70),rule("healing_draught",35))),
    make_pool("animal","Wild Animal","Animal",rules=list(rule("animal_pelt"))),
    make_pool("dire_beast","Dire Beast","Animal",c("Brute"),list(rule("bear_pelt"))),
    make_pool("fen_beast","Fen Beast","Animal",c("Venomous"),list(rule("animal_pelt"))),
    make_pool("fae","Fae Wanderer","Fae",rules=list(rule("fae_dust"))),
    make_pool("fae_trickster","Annwn Fae Trickster","Fae",c("Speedy","Spellcaster"),list(
      rule("fae_dust"),rule("dagger",45),rule("healing_draught",12))),
    make_pool("mandred_warden","Mandred Warden","Fae",c("Armoured"),list(
      rule("studded_leather"),rule("spear"),rule("shield",60),rule("fae_dust"))),
    make_pool("undead","Restless Dead","Undead",rules=list(
      rule("ring_mail"),rule("bone_fragment"),rule("spear",55,"dead_weapon",TRUE),rule("dagger",45,"dead_weapon",TRUE))),
    make_pool("wasting_dead","Wasting Dead","Undead",c("Regenerator","Venomous"),list(
      rule("bone_fragment"),rule("ring_mail",25))),
    make_pool("construct","Stone Sentinel","Construct",rules=list(rule("glyph_mat_009",35))),
    make_pool("war_construct","Ancient War Construct","Construct",c("Boss","Armoured"),list(
      rule("plate_armor"),rule("battleaxe"),rule("shield",55),rule("glyph_mat_009",75))),
    make_pool("dragonkin","Dragonkin Raider","Dragonkin",rules=list(
      rule("scale_mail"),rule("spear"),rule("shield",35))),
    make_pool("dragonkin_hunter","Dragonkin Hunter","Dragonkin",c("Archer","Speedy"),list(
      rule("hide_armor"),rule("shortbow"),rule("dagger"),rule("healing_draught",10))),
    make_pool("dragonkin_champion","Dragonkin Champion","Dragonkin",c("Boss","Armoured"),list(
      rule("chain_mail"),rule("battleaxe"),rule("shield",70),rule("healing_draught",25)))
  )
}

merge_npc_pool_catalogue <- function(saved=list()) {
  defaults <- npc_default_pool_catalogue()
  retired_names <- c("example 2","new pool")
  saved <- Filter(function(x) {
    !tolower(trimws(as.character(x$name %||% ""))) %in% retired_names
  },saved)
  default_ids <- vapply(defaults,`[[`,"","id")
  saved_by_id <- setNames(saved,vapply(saved,function(x)as.character(x$id %||% ""),character(1)))
  defaults <- lapply(defaults,function(x) {
    old <- saved_by_id[[x$id]]
    if(!is.null(old) && identical(as.integer(old$catalogue_version %||% 0L),as.integer(x$catalogue_version))) old else x
  })
  custom <- Filter(function(x) !as.character(x$id %||% "") %in% default_ids,saved)
  c(defaults,custom)
}

controlNpcPoolsUI <- function(id) { ns<-NS(id); tagList(
  div(class="control-card",div(class="control-section-title","NPC Pools"),
      fluidRow(column(4,selectInput(ns("pool_id"),"Pool",choices=character()),textInput(ns("pool_name"),"Pool name"),selectInput(ns("base_type"),"Base stat foundation",choices=names(enemy_generator_types())),selectizeInput(ns("features"),"Features / characteristics",choices=character(),multiple=TRUE),actionButton(ns("save_pool"),"Save Pool",class="btn btn-primary"),actionButton(ns("new_pool"),"New Pool")),
               column(8,h4("Inventory chances"),fluidRow(column(4,selectInput(ns("item_id"),"Item",choices=character())),column(2,numericInput(ns("chance"),"Chance / weight %",100,min=0,max=100)),column(3,textInput(ns("group"),"Alternative group",placeholder="bow")),column(3,checkboxInput(ns("required"),"Guarantee one from group",FALSE))),actionButton(ns("add_rule"),"Add / Update Rule"),actionButton(ns("remove_rule"),"Remove Selected Rule"),selectInput(ns("rule_id"),"Current rules",choices=character()),tags$hr(),actionButton(ns("roll_preview"),"Roll Example Loadout",class="btn btn-default"),uiOutput(ns("preview"))))),
  div(class="control-card",h4("Pool statistics and damage traits"),fluidRow(lapply(names(c(str="STR",dex="DEX",con="CON",int="INT",cha="CHA",bld_str="Blood strength")),function(stat){labels<-c(str="STR",dex="DEX",con="CON",int="INT",cha="CHA",bld_str="Blood strength");column(2,numericInput(ns(paste0("ability_",stat)),labels[[stat]],10,min=1,max=30))})),fluidRow(column(3,selectizeInput(ns("resistances"),"Resistances",enemy_damage_types(),multiple=TRUE)),column(3,selectizeInput(ns("immunities"),"Immunities",enemy_damage_types(),multiple=TRUE)),column(3,selectizeInput(ns("vulnerabilities"),"Vulnerabilities",enemy_damage_types(),multiple=TRUE)),column(3,selectizeInput(ns("condition_immunities"),"Condition immunities",enemy_conditions(),multiple=TRUE)))),
  div(class="control-card",p(class="control-mini","Independent entries roll their percentage. Entries sharing a group are alternatives; a required group always chooses exactly one using their percentages as weights."))
) }

controlNpcPoolsServer <- function(id) { moduleServer(id,function(input,output,session){
  `%||%`<-get("%||%",inherits=TRUE); path<-file.path("control_app","data","npc_pools.rds");dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  inv_path<-file.path("control_app","data","control_inventory.rds")
  feature_path<-file.path("control_app","data","npc_features.rds")
  feature_choices<-reactive({invalidateLater(1500,session);x<-if(file.exists(feature_path))tryCatch(readRDS(feature_path),error=function(e)list())else list();if(length(x))setNames(vapply(x,`[[`,"","id"),vapply(x,`[[`,"","name"))else enemy_characteristic_labels()})
  feature_choice_sig<-reactiveVal("")
  db_inventory<-get_control_catalogue_definitions()
  inventory<-reactive({invalidateLater(1500,session);saved<-if(file.exists(inv_path))tryCatch(readRDS(inv_path),error=function(e)list())else list();base<-lapply(names(enemy_loot_catalog()),function(k){x<-enemy_loot_catalog()[[k]];x$id<-k;x});all<-c(saved,db_inventory,base);seen<-character();Filter(function(x){id<-as.character(x$id%||%"");category<-as.character((x$meta%||%list())$category%||%"");keep<-nzchar(id)&&!id%in%seen&&!category%in%c("mundane_loot","food");seen<<-c(seen,id);keep},all)})
  pools<-reactiveVal({
    saved<-if(file.exists(path))tryCatch(readRDS(path),error=function(e)list())else list()
    merged<-merge_npc_pool_catalogue(saved)
    if(!identical(saved,merged))saveRDS(merged,path)
    merged
  }); active<-reactive({x<-Filter(function(p)identical(p$id,input$pool_id%||%""),pools());if(length(x))x[[1]]else NULL}); selected_rule<-reactive({p<-active();if(is.null(p))return(NULL);x<-Filter(function(r)identical(r$item_id,input$rule_id%||%""),p$rules);if(length(x))x[[1]]else NULL})
  observe({ps<-pools();vals<-vapply(ps,`[[`,"","id");keep<-input$pool_id%||%"";updateSelectInput(session,"pool_id",choices=setNames(vals,vapply(ps,`[[`,"","name")),selected=if(keep%in%vals)keep else if(length(vals))vals[1] else character())})
  observe({choices<-feature_choices();sig<-paste(names(choices),choices,collapse="|");if(identical(sig,feature_choice_sig()))return();feature_choice_sig(sig);updateSelectizeInput(session,"features",choices=choices,selected=isolate(input$features%||%character()),server=TRUE)})
  observe({it<-inventory();vals<-vapply(it,`[[`,"","id");keep<-input$item_id%||%"";updateSelectInput(session,"item_id",choices=setNames(vals,vapply(it,`[[`,"","name")),selected=if(keep%in%vals)keep else if(length(vals))vals[1] else character())})
  hydrate_pool_fields<-function(p){base<-enemy_generator_types()[[p$base_type%||%"Custom"]];abilities<-p$abilities%||%base$abilities%||%list();for(stat in c("str","dex","con","int","cha","bld_str"))updateNumericInput(session,paste0("ability_",stat),value=as.integer(abilities[[stat]]%||%10L));for(f in c("resistances","immunities","vulnerabilities","condition_immunities"))updateSelectizeInput(session,f,selected=p[[f]]%||%base[[f]]%||%character())}
  observeEvent(input$pool_id,{p<-active();if(is.null(p))return();updateTextInput(session,"pool_name",value=p$name);updateSelectInput(session,"base_type",selected=p$base_type%||%"Custom");updateSelectizeInput(session,"features",selected=p$features);hydrate_pool_fields(p)},ignoreInit=TRUE)
  observe({p<-active();if(is.null(p))return();it<-inventory();nm<-setNames(vapply(p$rules,function(r)r$item_id,character(1)),vapply(p$rules,function(r){item<-Filter(function(x)identical(x$id,r$item_id),it);name<-if(length(item))item[[1]]$name else r$item_id;paste0(name," — ",r$chance,"%",if(nzchar(r$group))paste0(" [",r$group,if(isTRUE(r$required))", required" else "","]")else"")},character(1)));updateSelectInput(session,"rule_id",choices=nm)})
  observeEvent(input$rule_id,{r<-selected_rule();if(is.null(r))return();updateSelectInput(session,"item_id",selected=r$item_id);updateNumericInput(session,"chance",value=r$chance);updateTextInput(session,"group",value=r$group);updateCheckboxInput(session,"required",value=isTRUE(r$required))},ignoreInit=TRUE)
  alter<-function(remove=FALSE){ps<-pools();pi<-which(vapply(ps,function(p)identical(p$id,input$pool_id),logical(1)))[1];if(is.na(pi))return();rules<-ps[[pi]]$rules;id<-if(remove)input$rule_id else input$item_id;rules<-Filter(function(r)!identical(r$item_id,id),rules);if(!remove)rules<-c(rules,list(list(item_id=id,chance=as.numeric(input$chance%||%100),group=trimws(input$group%||%""),required=isTRUE(input$required))));ps[[pi]]$rules<-rules;pools(ps);saveRDS(ps,path)}
  observeEvent(input$add_rule,{alter(FALSE)});observeEvent(input$remove_rule,{alter(TRUE)})
  observeEvent(input$new_pool,{ps<-pools();id<-paste0("pool_",as.integer(Sys.time()));base<-enemy_generator_types()$Custom;ps<-c(ps,list(list(id=id,name="New Pool",base_type="Custom",features=character(),abilities=base$abilities,resistances=character(),immunities=character(),vulnerabilities=character(),condition_immunities=character(),rules=list())));pools(ps);saveRDS(ps,path);updateSelectInput(session,"pool_id",selected=id)})
  observeEvent(input$save_pool,{ps<-pools();id<-input$pool_id%||%"";idx<-which(vapply(ps,function(p)identical(p$id,id),logical(1)))[1];if(is.na(idx)){id<-paste0("pool_",as.integer(Sys.time()));ps<-c(ps,list(list(id=id,name=input$pool_name%||%"New Pool",base_type=input$base_type%||%"Custom",features=input$features%||%character(),rules=list())));idx<-length(ps)};ps[[idx]]$name<-input$pool_name%||%ps[[idx]]$name;ps[[idx]]$base_type<-input$base_type%||%"Custom";ps[[idx]]$features<-input$features%||%character();ps[[idx]]$abilities<-setNames(lapply(c("str","dex","con","int","cha","bld_str"),function(s)as.integer(input[[paste0("ability_",s)]]%||%10L)),c("str","dex","con","int","cha","bld_str"));for(f in c("resistances","immunities","vulnerabilities","condition_immunities"))ps[[idx]][[f]]<-input[[f]]%||%character();pools(ps);saveRDS(ps,path);showNotification("NPC pool saved.")})
  rolled<-eventReactive(input$roll_preview,{p<-active();if(is.null(p))return(character());rules<-p$rules;out<-vapply(Filter(function(r)!nzchar(r$group)&&runif(1)*100<=r$chance,rules),`[[`,"","item_id");groups<-split(Filter(function(r)nzchar(r$group),rules),vapply(Filter(function(r)nzchar(r$group),rules),`[[`,"","group"));for(g in groups){required<-any(vapply(g,function(r)isTRUE(r$required),logical(1)));if(required||runif(1)*100<=sum(vapply(g,`[[`,0,"chance")))out<-c(out,sample(vapply(g,`[[`,"","item_id"),1,prob=vapply(g,`[[`,0,"chance")))};out})
  output$preview<-renderUI({ids<-rolled();it<-inventory();names<-vapply(ids,function(id){x<-Filter(function(y)identical(y$id,id),it);if(length(x))x[[1]]$name else id},character(1));tagList(h4("Example"),if(length(names))tags$ul(lapply(names,tags$li))else p("No items rolled."))})
}) }
