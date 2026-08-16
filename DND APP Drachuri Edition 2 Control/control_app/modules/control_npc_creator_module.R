library(shiny)
library(DT)

controlNpcCreatorUI <- function(id) {
  ns <- NS(id)
  tagList(tags$style(HTML(paste0(
    "#",ns("root")," .generator-hero{background:linear-gradient(135deg,#302419,#66502f);color:#fff8df;padding:18px;border-radius:14px;margin-bottom:14px}",
    "#",ns("root")," .trait-guide{display:grid;grid-template-columns:repeat(auto-fit,minmax(230px,1fr));gap:8px;margin:10px 0 16px}",
    "#",ns("root")," .trait-note{padding:10px;border:1px solid #ccb77e;border-radius:10px;background:#fffdf4}",
    "#",ns("root")," .trait-note strong{display:block;color:#573b1d}",
    "#",ns("root")," .form-section{border-top:1px solid #dccb9e;padding-top:12px;margin-top:12px}",
    "#",ns("root")," .selectize-control.multi .item{background:#6b4f2b;color:white;border:0}"
  ))), div(id=ns("root"),class="control-card",
    div(class="generator-hero",h3("Enemy Generator"),p("Combine a creature foundation with traits, then inspect the validated combat and loot package before saving.")),
    div(class="control-section-title", "🧌 Enemy Generator"),
    fluidRow(
      column(4, selectInput(ns("enemy_type"), "Enemy type", choices=names(enemy_generator_types()))),
      column(8, checkboxGroupInput(ns("characteristics"), "Characteristics", choices=enemy_characteristic_labels(), inline=TRUE))
    ),
    uiOutput(ns("characteristic_guide")),
    actionButton(ns("apply_defaults"), "Build from type + characteristics", class="btn btn-primary"),
    tags$hr(),
    h4("Edit generated enemy before saving"),
    fluidRow(
      column(4, textInput(ns("npc_name"), "Template name", "Custom Enemy")),
      column(2, numericInput(ns("npc_hp"), "HP", 10, min=1)),
      column(2, numericInput(ns("npc_ac"), "AC", 12, min=1)),
      column(2, numericInput(ns("npc_speed"), "Speed (ft)", 30, min=0)),
      column(2, textInput(ns("npc_tags"), "Tags", ""))
    ),
    fluidRow(lapply(names(c(str="STR",dex="DEX",con="CON",int="INT",cha="CHA",bld_str="Blood strength")), function(stat) {
      labels <- c(str="STR",dex="DEX",con="CON",int="INT",cha="CHA",bld_str="Blood strength")
      column(2, numericInput(ns(paste0("ability_",stat)), labels[[stat]], 10, min=1, max=30))
    })),
    div(class="form-section",fluidRow(
      column(4, selectInput(ns("armor_id"), "Standard D&D armour", choices=setNames(names(enemy_armor_catalog()),vapply(enemy_armor_catalog(),function(x)x$name,character(1))))),
      column(4, selectizeInput(ns("attack_ids"), "Validated attacks", choices=character(), multiple=TRUE)),
      column(4, selectizeInput(ns("loot_ids"), "Additional guaranteed drops", choices=character(), multiple=TRUE))
    )),
    uiOutput(ns("armor_help")),
    fluidRow(
      column(3,numericInput(ns("gold_min"),"Minimum gold",0,min=0,step=1)),
      column(3,numericInput(ns("gold_max"),"Maximum gold",0,min=0,step=1)),
      column(6,helpText("Weapons used by the enemy and selected armour are always included in loot. Gold transfers directly to the looting player's purse."))
    ),
    fluidRow(
      column(3, selectizeInput(ns("resistances"), "Resistances", choices=enemy_damage_types(), multiple=TRUE)),
      column(3, selectizeInput(ns("immunities"), "Damage immunities", choices=enemy_damage_types(), multiple=TRUE)),
      column(3, selectizeInput(ns("vulnerabilities"), "Vulnerabilities", choices=enemy_damage_types(), multiple=TRUE)),
      column(3, selectizeInput(ns("condition_immunities"), "Condition immunities", choices=enemy_conditions(), multiple=TRUE))
    ),
    actionButton(ns("save_npc"), "Save new template", class="btn btn-success"),
    actionButton(ns("update_npc"), "Update selected template", class="btn btn-warning"),
    tags$hr(), h4("Saved templates"),
    fluidRow(
      column(4, selectInput(ns("filter_type"), "Filter by type", choices=c("All"=""))),
      column(6, checkboxGroupInput(ns("filter_characteristics"), "Filter by characteristic", choices=enemy_characteristic_labels(), inline=TRUE)),
      column(2, br(), actionButton(ns("refresh_npcs"), "Refresh"))
    ),
    DTOutput(ns("npc_tbl")),
    actionButton(ns("load_npc"), "Load selected for editing", class="btn btn-info"),
    actionButton(ns("delete_npc"), "Delete selected", class="btn btn-danger")
  ))
}

controlNpcCreatorServer <- function(id, ctrl=NULL, bump_refresh=NULL) {
  moduleServer(id, function(input, output, session) {
    `%||%` <- get("%||%", inherits=TRUE)
    templates <- reactiveVal(data.frame())
    selected_id <- reactiveVal("")
    make_id <- function(name) paste0("npc_", gsub("^_|_$", "", tolower(gsub("[^a-zA-Z0-9]+","_",name))), "_", paste0(sample(c(letters,0:9),8,TRUE),collapse=""))
    parse_json <- function(x, default=list()) {
      if (is.list(x) && !is.data.frame(x)) return(x)
      tryCatch(jsonlite::fromJSON(as.character(x %||% ""), simplifyVector=FALSE), error=function(e) default)
    }
    loot_choices <- setNames(names(enemy_loot_catalog()),vapply(enemy_loot_catalog(),function(a)paste0(a$name," — ",a$type," · ",a$weight," lb · ",a$value,"g"),character(1)))
    updateSelectizeInput(session,"loot_ids",choices=loot_choices,server=TRUE)
    attack_sig<-reactiveVal(""); pool_sig<-reactiveVal("")
    all_attacks<-reactive({invalidateLater(1500,session);base<-enemy_attack_catalog();saved<-tryCatch(readRDS(file.path("control_app","data","npc_attacks.rds")),error=function(e)list());for(a in saved)base[[a$id]]<-a;base})
    npc_pools<-reactive({invalidateLater(1500,session);tryCatch(readRDS(file.path("control_app","data","npc_pools.rds")),error=function(e)list())})
    observe({a<-all_attacks();choices<-setNames(names(a),vapply(a,function(x)paste0(x$name," — +",x$hit," / ",x$dmg," ",x$type),character(1)));sig<-paste(names(choices),choices,collapse="|");if(!identical(sig,attack_sig())){attack_sig(sig);updateSelectizeInput(session,"attack_ids",choices=choices,selected=isolate(input$attack_ids%||%character()),server=TRUE)}})
    observe({p<-npc_pools();choices<-if(length(p))setNames(vapply(p,`[[`,"","id"),vapply(p,`[[`,"","name"))else setNames(names(enemy_generator_types()),names(enemy_generator_types()));sig<-paste(names(choices),choices,collapse="|");if(!identical(sig,pool_sig())){pool_sig(sig);updateSelectInput(session,"enemy_type",choices=choices,selected=if((isolate(input$enemy_type)%||%"")%in%unname(choices))isolate(input$enemy_type)else unname(choices)[1])}})
    output$characteristic_guide <- renderUI({ mods<-enemy_generator_characteristics(); tagList(div(class="trait-guide",lapply(names(mods),function(id)div(class="trait-note",strong(enemy_characteristic_labels()[[id]]),span(mods[[id]]$desc))))) })
    output$armor_help <- renderUI({ a<-enemy_armor_catalog()[[input$armor_id%||%"unarmoured"]]; div(class="trait-note",strong(a$name),span(a$desc," Equipped armour is automatically guaranteed as loot.")) })
    load_templates <- function() {
      con <- get_db_connection(); if (is.null(con)) return(); on.exit(release_db_connection(con),add=TRUE)
      templates(tryCatch(DBI::dbGetQuery(con,"select * from npc_templates order by lower(enemy_type), lower(name)"),error=function(e)data.frame()))
    }
    apply_blueprint <- function(b, name=NULL) {
      updateTextInput(session,"npc_name",value=name %||% paste(b$enemy_type, if(length(b$characteristics)) paste(b$characteristics,collapse=" ") else "Enemy"))
      updateNumericInput(session,"npc_hp",value=b$hp_max); updateNumericInput(session,"npc_ac",value=b$ac); updateNumericInput(session,"npc_speed",value=b$movement_speed)
      for (stat in names(b$abilities)) updateNumericInput(session,paste0("ability_",stat),value=b$abilities[[stat]])
      updateSelectizeInput(session,"attack_ids",selected=b$attack_ids %||% character()); updateSelectizeInput(session,"loot_ids",selected=b$loot_ids %||% character())
      updateSelectInput(session,"armor_id",selected=b$armor_id%||%"unarmoured")
      updateNumericInput(session,"gold_min",value=as.integer((b$gold%||%c(0,0))[1])); updateNumericInput(session,"gold_max",value=as.integer((b$gold%||%c(0,0))[2]))
      for (f in c("resistances","immunities","vulnerabilities","condition_immunities")) updateSelectizeInput(session,f,selected=b[[f]] %||% character())
    }
    observeEvent(TRUE, load_templates(), once=TRUE)
    observeEvent(input$refresh_npcs, load_templates(), ignoreInit=TRUE)
    observeEvent(input$apply_defaults, { selected_id("");pool<-Filter(function(p)identical(p$id,input$enemy_type),npc_pools());if(length(pool)){p<-pool[[1]];features<-unique(c(p$features%||%character(),input$characteristics%||%character()));apply_blueprint(resolve_enemy_blueprint(p$base_type%||%"Custom",intersect(features,names(enemy_generator_characteristics()))));updateCheckboxGroupInput(session,"characteristics",selected=features)}else apply_blueprint(resolve_enemy_blueprint(input$enemy_type,input$characteristics)) }, ignoreInit=TRUE)
    observeEvent(input$armor_id, {
      armor<-enemy_armor_catalog()[[input$armor_id%||%"unarmoured"]]; dex<-as.integer(input$ability_dex%||%10L); dex_mod<-floor((dex-10L)/2L)
      ac<-if(identical(input$armor_id,"unarmoured"))10L+dex_mod else as.integer(armor$ac_base+min(dex_mod,armor$dex_cap)); updateNumericInput(session,"npc_ac",value=max(1L,ac))
    },ignoreInit=TRUE)
    current_record <- function(npc_id=NULL) {
      catalog<-all_attacks();attack_ids<-intersect(input$attack_ids%||%character(),names(catalog)); attacks<-unname(catalog[attack_ids]); if(!length(attacks)) stop("Choose at least one validated attack.")
      armor_id<-if(input$armor_id%in%names(enemy_armor_catalog()))input$armor_id else "unarmoured"; armor_loot<-enemy_armor_catalog()[[armor_id]]$loot_id%||%""
      loot_ids<-intersect(input$loot_ids%||%character(),names(enemy_loot_catalog())); carried<-Filter(nzchar,vapply(attacks,function(a)as.character(a$loot_id%||%""),character(1))); loot_ids<-unique(c(loot_ids,carried,Filter(nzchar,armor_loot)))
      abilities <- setNames(lapply(c("str","dex","con","int","cha","bld_str"), function(s) as.integer(input[[paste0("ability_",s)]] %||% 10L)),c("str","dex","con","int","cha","bld_str"))
      list(npc_id=npc_id %||% make_id(input$npc_name), name=trimws(input$npc_name), enemy_type=input$enemy_type,
        characteristics=as.list(input$characteristics %||% character()), hp_max=as.integer(input$npc_hp), ac=as.integer(input$npc_ac), movement_speed=as.integer(input$npc_speed), abilities=abilities,
        armor_id=armor_id,attack_ids=attack_ids,attacks=attacks,loot_ids=loot_ids,loot=enemy_loot_records(loot_ids),gold=c(as.integer(input$gold_min%||%0),as.integer(input$gold_max%||%0)),
        resistances=intersect(input$resistances%||%character(),enemy_damage_types()), immunities=intersect(input$immunities%||%character(),enemy_damage_types()), vulnerabilities=intersect(input$vulnerabilities%||%character(),enemy_damage_types()), condition_immunities=intersect(input$condition_immunities%||%character(),enemy_conditions()), tags=input$npc_tags)
    }
    persist <- function(rec, update=FALSE) {
      if(!nzchar(rec$name)) stop("Enter a template name."); p <- rec$attacks[[1]]; con <- get_db_connection(); if(is.null(con)) stop("Database unavailable."); on.exit(release_db_connection(con),add=TRUE)
      sql <- if(update) "UPDATE npc_templates SET name=$2,hp_max=$3,ac=$4,movement_speed=$5,attack_name=$6,attack_bonus=$7,damage_expr=$8,damage_type=$9,attacks_json=$10,tags=$11,enemy_type=$12,characteristics=$13::jsonb,abilities=$14::jsonb,attacks=$15::jsonb,loot=$16::jsonb,resistances=$17::text[],immunities=$18::text[],vulnerabilities=$19::text[],condition_immunities=$20::text[],gold_min=$21,gold_max=$22,armor_id=$23,updated_at=now() WHERE npc_id=$1" else "INSERT INTO npc_templates(npc_id,name,hp_max,ac,movement_speed,attack_name,attack_bonus,damage_expr,damage_type,attacks_json,tags,enemy_type,characteristics,abilities,attacks,loot,resistances,immunities,vulnerabilities,condition_immunities,gold_min,gold_max,armor_id) VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13::jsonb,$14::jsonb,$15::jsonb,$16::jsonb,$17::text[],$18::text[],$19::text[],$20::text[],$21,$22,$23)"
      DBI::dbExecute(con,sql,params=list(rec$npc_id,rec$name,rec$hp_max,rec$ac,rec$movement_speed,p$name,as.integer(p$hit),p$dmg,p$type,enemy_json(rec$attacks),rec$tags,rec$enemy_type,enemy_json(rec$characteristics),enemy_json(rec$abilities),enemy_json(rec$attacks),enemy_json(rec$loot),enemy_pg_array(rec$resistances),enemy_pg_array(rec$immunities),enemy_pg_array(rec$vulnerabilities),enemy_pg_array(rec$condition_immunities),min(rec$gold),max(rec$gold),rec$armor_id))
    }
    save_handler <- function(update=FALSE) tryCatch({ id <- if(update) selected_id() else NULL; if(update && !nzchar(id)) stop("Load a saved template first."); persist(current_record(id),update); load_templates(); if(is.function(bump_refresh)) bump_refresh(); showNotification(if(update) "Template updated." else "Template saved.",type="message") },error=function(e)showNotification(conditionMessage(e),type="error"))
    observeEvent(input$save_npc,save_handler(FALSE),ignoreInit=TRUE); observeEvent(input$update_npc,save_handler(TRUE),ignoreInit=TRUE)
    filtered <- reactive({ df<-templates(); if(!nrow(df)) return(df); if(nzchar(input$filter_type%||%"")) df<-df[df$enemy_type==input$filter_type,,drop=FALSE]
      wanted<-input$filter_characteristics%||%character(); if(length(wanted)) df<-df[vapply(df$characteristics,function(x) all(wanted %in% unlist(parse_json(x))),logical(1)),,drop=FALSE]; df })
    observe({ updateSelectInput(session,"filter_type",choices=c("All"="",sort(unique(templates()$enemy_type %||% character()))),selected=input$filter_type%||%"") })
    output$npc_tbl<-renderDT({ df<-filtered(); if(!nrow(df)) return(NULL); chars<-vapply(df$characteristics,function(x)paste(unlist(parse_json(x)),collapse=", "),character(1)); view<-data.frame(name=df$name,type=df$enemy_type,characteristics=chars,hp=df$hp_max,ac=df$ac,speed=df$movement_speed,attack=df$attack_name,tags=df$tags); datatable(view,selection="single",rownames=FALSE,options=list(pageLength=10,scrollX=TRUE)) })
    selected_row <- function() { i<-input$npc_tbl_rows_selected; df<-filtered(); if(!length(i)||!nrow(df)) return(NULL); df[i[1],,drop=FALSE] }
    observeEvent(input$load_npc,{ r<-selected_row(); if(is.null(r)){showNotification("Select a template.",type="warning");return()}; selected_id(as.character(r$npc_id[1])); chars<-unlist(parse_json(r$characteristics[[1]])); updateSelectInput(session,"enemy_type",selected=r$enemy_type[1]); updateCheckboxGroupInput(session,"characteristics",selected=chars)
      stored_attacks<-parse_json(r$attacks[[1]]); attack_ids<-names(Filter(function(def)any(vapply(stored_attacks,function(a)identical(a$name,def$name),logical(1))),all_attacks())); stored_loot<-parse_json(r$loot[[1]]); loot_ids<-names(Filter(function(def)any(vapply(stored_loot,function(a)identical(a$name,def$name),logical(1))),enemy_loot_catalog()))
      b<-list(enemy_type=r$enemy_type[1],characteristics=chars,hp_max=r$hp_max[1],ac=r$ac[1],armor_id=r$armor_id[1],movement_speed=r$movement_speed[1],abilities=parse_json(r$abilities[[1]]),attack_ids=attack_ids,loot_ids=loot_ids,gold=c(r$gold_min[1],r$gold_max[1]),resistances=enemy_db_values(r$resistances[[1]]),immunities=enemy_db_values(r$immunities[[1]]),vulnerabilities=enemy_db_values(r$vulnerabilities[[1]]),condition_immunities=enemy_db_values(r$condition_immunities[[1]])); apply_blueprint(b,r$name[1]); updateTextInput(session,"npc_tags",value=r$tags[1]) },ignoreInit=TRUE)
    observeEvent(input$delete_npc,{ r<-selected_row(); if(is.null(r)){showNotification("Select a template.",type="warning");return()}; con<-get_db_connection(); if(is.null(con))return(); on.exit(release_db_connection(con),add=TRUE); DBI::dbExecute(con,"delete from npc_templates where npc_id=$1",params=list(as.character(r$npc_id[1]))); selected_id(""); load_templates(); if(is.function(bump_refresh))bump_refresh() },ignoreInit=TRUE)
  })
}
