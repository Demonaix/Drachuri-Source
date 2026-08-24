library(shiny)

npc_pool_rule <- function(item_id, chance=100, group="", required=FALSE) {
  list(item_id=item_id,chance=as.numeric(chance),group=group,required=isTRUE(required))
}
npc_pool_attack_rule <- function(attack_id, chance=100, group="", required=FALSE) {
  list(item_id=attack_id,chance=as.numeric(chance),group=group,required=isTRUE(required))
}

npc_default_pool_catalogue <- function() {
  catalogue_version <- 5L
  rule <- npc_pool_rule
  make_pool <- function(id,name,base_type,features=character(),rules=list(),abilities=NULL,
                        resistances=NULL,immunities=NULL,vulnerabilities=NULL,condition_immunities=NULL,
                        foundation="",status="Core",description="",restriction="",hp_max=NULL,ac=NULL,
                        movement_speed=NULL,gold=NULL,attack_ids=NULL,attack_rules=list()) {
    base <- enemy_generator_types()[[base_type]]
    list(
      id=id,name=name,base_type=base_type,features=features,catalogue_version=catalogue_version,
      abilities=abilities %||% base$abilities,
      resistances=resistances %||% base$resistances %||% character(),
      immunities=immunities %||% base$immunities %||% character(),
      vulnerabilities=vulnerabilities %||% base$vulnerabilities %||% character(),
      condition_immunities=condition_immunities %||% base$condition_immunities %||% character(),
      rules=rules,foundation=foundation,status=status,description=description,restriction=restriction,
      hp_max=hp_max,ac=ac,movement_speed=movement_speed,gold=gold,attack_ids=attack_ids,attack_rules=attack_rules
    )
  }
  list(
    make_pool("custom","Custom Enemy","Custom",foundation="Bespoke",description="A blank mechanical foundation for an authored enemy."),
    make_pool("oldrin_civilian","Oldrin Civilian","Custom",rules=list(rule("club",65,"civilian_weapon",TRUE),rule("dagger",35,"civilian_weapon",TRUE)),
      abilities=c(str=9L,dex=10L,con=10L,int=10L,cha=10L,bld_str=10L),foundation="Oldrin humanoid",
      description="An ordinary inhabitant of a town, village, farm, estate or road.",restriction="Occupation should shape skills and possessions.",hp_max=7L,ac=10L,gold=c(0L,5L),attack_ids="unarmed_strike",attack_rules=list(npc_pool_attack_rule("desperate_shove",30))),
    make_pool("oldrin_bandit_raider","Bandit Raider","Bandit",rules=list(
      rule("leather_armor"),rule("shortsword",70,"sidearm",TRUE),rule("dagger",30,"sidearm",TRUE),
      rule("shortbow",45),rule("healing_draught",5)),foundation="Usually Oldrin humanoid",
      description="A criminal, deserter, displaced person or opportunistic raider.",restriction="Bandit is an occupation, never a species or fixed faction.",attack_rules=list(npc_pool_attack_rule("dirty_kick",30),npc_pool_attack_rule("pocket_sand",20))),
    make_pool("forest_outlaw","Forest Outlaw","Bandit",c("Archer"),list(
      rule("padded_armor",65,"outlaw_armour",TRUE),rule("studded_leather",35,"outlaw_armour",TRUE),
      rule("shortbow"),rule("dagger",75),rule("healing_draught",8)),foundation="Usually Oldrin humanoid",
      description="A fugitive hunter or criminal living beyond House authority.",restriction="Do not automatically assign a political faction."),
    make_pool("oldrin_hunter","Oldrin Hunter / Scout","Bandit",c("Archer"),list(
      rule("leather_armor"),rule("shortbow",70,"hunter_weapon",TRUE),rule("spear",30,"hunter_weapon",TRUE),rule("dagger")),
      foundation="Oldrin humanoid",description="A hunter, tracker, messenger or wilderness scout.",gold=c(1L,8L)),
    make_pool("town_guard","Town Guard","Guard",rules=list(
      rule("chain_shirt"),rule("spear"),rule("shield",65),rule("dagger",25),rule("healing_draught",5)),
      foundation="Predominantly Oldrin humanoid",description="Local armed authority in an Oldrin settlement.",restriction="Allegiance must match the settlement or House."),
    make_pool("house_soldier","House Soldier","Guard",rules=list(
      rule("scale_mail",60,"soldier_armour",TRUE),rule("chain_mail",40,"soldier_armour",TRUE),
      rule("spear",60,"soldier_weapon",TRUE),rule("shortsword",40,"soldier_weapon",TRUE),rule("shield",70)),
      abilities=c(str=14L,dex=12L,con=14L,int=10L,cha=10L,bld_str=10L),foundation="Oldrin humanoid",
      description="A professional soldier serving one of Annwn's Great Houses.",restriction="House allegiance is mandatory metadata.",hp_max=28L,ac=16L,gold=c(3L,15L)),
    make_pool("veteran_house_guard","Veteran House Guard","Guard",c("Boss","Armoured"),list(
      rule("chain_mail"),rule("spear",45,"guard_weapon",TRUE),rule("shortsword",55,"guard_weapon",TRUE),
      rule("shield",80),rule("healing_draught",15)),foundation="Usually Oldrin humanoid",status="Uncommon / elite",
      description="An experienced retainer trusted with nobles, estates and strategic sites.",restriction="Do not randomly make them sorcerers."),
    make_pool("mercenary_soldier","Mercenary Soldier","Guard",rules=list(
      rule("hide_armor",35,"mercenary_armour",TRUE),rule("scale_mail",65,"mercenary_armour",TRUE),
      rule("spear",40,"mercenary_weapon",TRUE),rule("shortsword",35,"mercenary_weapon",TRUE),
      rule("battleaxe",25,"mercenary_weapon",TRUE),rule("shield",50),rule("healing_draught",10)),
      foundation="Predominantly Oldrin humanoid",description="A professional fighter whose allegiance is contractual.",restriction="No universal mercenary faction or culture."),
    make_pool("oldrin_sorcerer","Oldrin Sorcerer","Custom",c("Spellcaster"),list(rule("club",35,"sorcerer_sidearm",TRUE),rule("dagger",65,"sorcerer_sidearm",TRUE),rule("healing_draught",20)),
      abilities=c(str=9L,dex=12L,con=12L,int=15L,cha=13L,bld_str=16L),foundation="Oldrin humanoid",status="Core / uncommon",
      description="An Oldrin with magical capability connected to Annwn.",restriction="Sorcerer is not a species and does not imply blood drinking.",hp_max=25L,ac=12L,gold=c(5L,50L),attack_ids="unarmed_strike",attack_rules=list(npc_pool_attack_rule("mandred_push",30),npc_pool_attack_rule("mandred_grasp",20))),
    make_pool("oldrin_necromancer","Oldrin Necromancer","Custom",c("Spellcaster"),list(
      rule("dagger"),rule("bone_fragment",75),rule("healing_draught",20)),
      abilities=c(str=8L,dex=12L,con=13L,int=16L,cha=12L,bld_str=17L),foundation="Oldrin sorcerer",status="Core / rare",
      description="A sorcerer specialising in necrotic magic and undead.",restriction="Necromancy does not automatically imply Abyss worship.",hp_max=34L,ac=13L,gold=c(10L,80L),attack_ids="withering_touch",attack_rules=list(npc_pool_attack_rule("grave_bolt",100),npc_pool_attack_rule("spectral_grasp",35),npc_pool_attack_rule("life_drain",20))),
    make_pool("fae_wanderer","Fae Wanderer","Custom",rules=list(rule("club",30,"wanderer_weapon",TRUE),rule("dagger",70,"wanderer_weapon",TRUE)),
      abilities=c(str=10L,dex=15L,con=11L,int=13L,cha=14L,bld_str=14L),foundation="Tylwyth Teg / fae",
      description="A traveller, displaced fae, scout or solitary survivor.",restriction="Do not default fae to whimsical tricksters or blood drinkers.",hp_max=18L,ac=13L,gold=c(0L,4L),attack_ids="unarmed_strike"),
    make_pool("fae_hunter","Fae Hunter / Scout","Bandit",c("Archer","Speedy"),list(
      rule("leather_armor"),rule("shortbow",65,"fae_hunter_weapon",TRUE),rule("spear",35,"fae_hunter_weapon",TRUE),rule("dagger")),
      abilities=c(str=12L,dex=16L,con=12L,int=12L,cha=12L,bld_str=15L),foundation="Tylwyth Teg / fae",
      description="A fae hunter or scout surviving in hostile Oldrin territory.",hp_max=26L,ac=14L,gold=c(0L,4L)),
    make_pool("fae_warrior","Fae Warrior","Bandit",rules=list(
      rule("studded_leather"),rule("spear",55,"fae_weapon",TRUE),rule("shortsword",45,"fae_weapon",TRUE),rule("shortbow",50),rule("shield",35)),
      abilities=c(str=14L,dex=15L,con=13L,int=12L,cha=13L,bld_str=15L),foundation="Tylwyth Teg / fae",
      description="An armed Tylwyth Teg combatant.",restriction="Do not automatically make fae warriors Cythraul.",hp_max=36L,ac=15L,gold=c(0L,6L)),
    make_pool("cythraul","Cythraul","Custom",c("Boss","Spellcaster"),list(
      rule("shortsword",45,"cythraul_weapon",TRUE),rule("spear",30,"cythraul_weapon",TRUE),rule("dagger",25,"cythraul_weapon",TRUE)),
      abilities=c(str=15L,dex=17L,con=16L,int=14L,cha=16L,bld_str=19L),foundation="Magically altered fae",status="Restricted / rare",
      description="A fae who restored lost magical power through blood drinking after the Tears of Time.",
      restriction="Generate only deliberately. Never treat as a generic vampire, cultist or automatically evil addict.",hp_max=65L,ac=17L,gold=c(0L,20L),attack_ids="mandred_bolt",attack_rules=list(npc_pool_attack_rule("blood_feed",70))),
    make_pool("restless_dead","Restless Dead","Undead",rules=list(
      rule("ring_mail",35),rule("bone_fragment"),rule("spear",55,"dead_weapon",TRUE),rule("dagger",45,"dead_weapon",TRUE)),
      foundation="Undead",description="A corpse or spirit disturbed through necromantic influence.",restriction="Do not generate intelligent undead without an explicit subtype.",attack_ids="dead_grasp"),
    make_pool("necromantic_servitor","Necromantic Servitor","Undead",c("Regenerator"),list(
      rule("ring_mail",45),rule("spear",65,"servitor_weapon",TRUE),rule("shortsword",35,"servitor_weapon",TRUE),rule("bone_fragment")),
      foundation="Deliberately raised undead",description="An undead guard or minion controlled by a sorcerer.",
      restriction="A plausible necromantic creator or history should exist.",hp_max=25L,attack_ids="servitor_strike",attack_rules=list(npc_pool_attack_rule("restraining_grip",40))),
    make_pool("llechwyr","Llechwyr","Custom",c("Speedy","Spellcaster"),rules=list(),
      abilities=c(str=13L,dex=17L,con=14L,int=8L,cha=8L,bld_str=16L),foundation="Shadow creature",status="Restricted supernatural",
      description="An established shadow-creature of Annwn: hunter, omen and supernatural threat.",restriction="Never use as ordinary roadside wildlife; no automatic radiant vulnerability.",
      hp_max=48L,ac=15L,movement_speed=40L,gold=c(0L,0L),attack_ids="shadow_strike",attack_rules=list(npc_pool_attack_rule("shadow_pounce",35),npc_pool_attack_rule("drag_into_darkness",30))),
    make_pool("wild_animal","Wild Animal","Animal",rules=list(rule("animal_pelt")),foundation="Animal",
      description="Normal fauna of Annwn.",restriction="Keep mundane animals mundane; select attacks that fit its anatomy.",attack_ids=character(),attack_rules=list(npc_pool_attack_rule("animal_bite",50,"animal_primary",TRUE),npc_pool_attack_rule("animal_claw",50,"animal_primary",TRUE))),
    make_pool("great_beast","Great Beast","Animal",c("Brute"),list(rule("bear_pelt")),
      abilities=c(str=18L,dex=13L,con=17L,int=3L,cha=7L,bld_str=11L),foundation="Exceptional natural animal",status="Core / uncommon",
      description="A particularly large, old or dangerous natural animal.",restriction="A generator category, not necessarily an in-world taxonomic term; check anatomy before saving.",hp_max=55L,ac=14L,gold=c(0L,0L),attack_ids=character(),attack_rules=list(npc_pool_attack_rule("great_beast_maul",60,"beast_primary",TRUE),npc_pool_attack_rule("great_beast_grab",40,"beast_primary",TRUE),npc_pool_attack_rule("great_beast_pounce",50,"beast_mobility",TRUE),npc_pool_attack_rule("great_beast_charge",50,"beast_mobility",TRUE))),
    make_pool("sorcerous_construct","Sorcerous Construct","Construct",rules=list(rule("glyph_mat_009",45)),
      abilities=c(str=17L,dex=9L,con=18L,int=5L,cha=3L,bld_str=12L),foundation="Oldrin sorcerous construct",status="Provisional / context restricted",
      description="An artificial guard, labourer or weapon associated with sorcery and tinkering.",
      restriction="Only generate with a plausible sorcerous creator; not evidence of an ancient construct civilisation.",hp_max=55L,ac=16L,gold=c(0L,0L),attack_ids="integrated_strike")
  )
}

merge_npc_pool_catalogue <- function(saved=list()) {
  defaults <- npc_default_pool_catalogue()
  retired_names <- c("example 2","new pool","goblin skirmisher","drachuri clan reaver","blood cult acolyte",
                     "blood cult priest","dire beast","fen beast","annwn fae trickster","mandred warden",
                     "wasting dead","stone sentinel","ancient war construct","dragonkin raider","dragonkin hunter","dragonkin champion")
  retired_ids <- c("bandit","guard","veteran_guard","mercenary","cultist","blood_priest","animal","dire_beast",
                   "goblin_skirmisher","drachuri_reaver","fen_beast","fae","fae_trickster","mandred_warden","undead",
                   "wasting_dead","construct","war_construct","dragonkin","dragonkin_hunter","dragonkin_champion")
  saved <- Filter(function(x) {
    !tolower(trimws(as.character(x$name %||% ""))) %in% retired_names && !as.character(x$id %||% "") %in% retired_ids
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
               column(8,uiOutput(ns("pool_context")),h4("Inventory chances"),fluidRow(column(4,selectInput(ns("item_id"),"Item",choices=character())),column(2,numericInput(ns("chance"),"Chance / weight %",100,min=0,max=100)),column(3,textInput(ns("group"),"Alternative group",placeholder="bow")),column(3,checkboxInput(ns("required"),"Guarantee one from group",FALSE))),actionButton(ns("add_rule"),"Add / Update Rule"),actionButton(ns("remove_rule"),"Remove Selected Rule"),selectInput(ns("rule_id"),"Current rules",choices=character()),tags$hr(),actionButton(ns("roll_preview"),"Roll Example Loadout",class="btn btn-default"),uiOutput(ns("preview"))))),
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
  output$pool_context<-renderUI({p<-active();if(is.null(p))return(NULL);catalog<-enemy_attack_catalog();fixed<-vapply(p$attack_ids%||%character(),function(id)catalog[[id]]$name%||%id,character(1));optional<-vapply(p$attack_rules%||%list(),function(r)paste0(catalog[[r$item_id]]$name%||%r$item_id," ",r$chance,"%",if(nzchar(r$group%||%""))paste0(" [",r$group,"]")else""),character(1));div(class="trait-note",strong(paste(p$foundation%||%"Bespoke","·",p$status%||%"Custom")),tags$p(p$description%||%""),if(length(c(fixed,optional)))tags$p(strong("Special attacks: "),paste(c(fixed,optional),collapse=" · ")),if(nzchar(p$restriction%||%""))tags$small(strong("Generation rule: "),p$restriction))})
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
