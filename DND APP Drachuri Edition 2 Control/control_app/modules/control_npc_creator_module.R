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
      column(6, selectizeInput(ns("attack_ids"), "Validated attacks", choices=character(), multiple=TRUE)),
      column(6, selectizeInput(ns("loot_ids"), "Guaranteed item drops", choices=character(), multiple=TRUE))
    )),
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
    attack_choices <- setNames(names(enemy_attack_catalog()),vapply(enemy_attack_catalog(),function(a)paste0(a$name," — +",a$hit," / ",a$dmg," ",a$type," (",a$material,")"),character(1)))
    loot_choices <- setNames(names(enemy_loot_catalog()),vapply(enemy_loot_catalog(),function(a)paste0(a$name," — ",a$type," · ",a$weight," lb · ",a$value,"g"),character(1)))
    updateSelectizeInput(session,"attack_ids",choices=attack_choices,server=TRUE)
    updateSelectizeInput(session,"loot_ids",choices=loot_choices,server=TRUE)
    output$characteristic_guide <- renderUI({ mods<-enemy_generator_characteristics(); tagList(div(class="trait-guide",lapply(names(mods),function(id)div(class="trait-note",strong(enemy_characteristic_labels()[[id]]),span(mods[[id]]$desc))))) })
    load_templates <- function() {
      con <- get_db_connection(); if (is.null(con)) return(); on.exit(release_db_connection(con),add=TRUE)
      templates(tryCatch(DBI::dbGetQuery(con,"select * from npc_templates order by lower(enemy_type), lower(name)"),error=function(e)data.frame()))
    }
    apply_blueprint <- function(b, name=NULL) {
      updateTextInput(session,"npc_name",value=name %||% paste(b$enemy_type, if(length(b$characteristics)) paste(b$characteristics,collapse=" ") else "Enemy"))
      updateNumericInput(session,"npc_hp",value=b$hp_max); updateNumericInput(session,"npc_ac",value=b$ac); updateNumericInput(session,"npc_speed",value=b$movement_speed)
      for (stat in names(b$abilities)) updateNumericInput(session,paste0("ability_",stat),value=b$abilities[[stat]])
      updateSelectizeInput(session,"attack_ids",selected=b$attack_ids %||% character()); updateSelectizeInput(session,"loot_ids",selected=b$loot_ids %||% character())
      updateNumericInput(session,"gold_min",value=as.integer((b$gold%||%c(0,0))[1])); updateNumericInput(session,"gold_max",value=as.integer((b$gold%||%c(0,0))[2]))
      for (f in c("resistances","immunities","vulnerabilities","condition_immunities")) updateSelectizeInput(session,f,selected=b[[f]] %||% character())
    }
    observeEvent(TRUE, load_templates(), once=TRUE)
    observeEvent(input$refresh_npcs, load_templates(), ignoreInit=TRUE)
    observeEvent(input$apply_defaults, { selected_id(""); apply_blueprint(resolve_enemy_blueprint(input$enemy_type,input$characteristics)) }, ignoreInit=TRUE)
    current_record <- function(npc_id=NULL) {
      attack_ids<-intersect(input$attack_ids%||%character(),names(enemy_attack_catalog())); attacks<-unname(enemy_attack_catalog()[attack_ids]); if(!length(attacks)) stop("Choose at least one validated attack.")
      loot_ids<-intersect(input$loot_ids%||%character(),names(enemy_loot_catalog())); carried<-Filter(nzchar,vapply(attacks,function(a)as.character(a$loot_id%||%""),character(1))); loot_ids<-unique(c(loot_ids,carried))
      abilities <- setNames(lapply(c("str","dex","con","int","cha","bld_str"), function(s) as.integer(input[[paste0("ability_",s)]] %||% 10L)),c("str","dex","con","int","cha","bld_str"))
      list(npc_id=npc_id %||% make_id(input$npc_name), name=trimws(input$npc_name), enemy_type=input$enemy_type,
        characteristics=as.list(input$characteristics %||% character()), hp_max=as.integer(input$npc_hp), ac=as.integer(input$npc_ac), movement_speed=as.integer(input$npc_speed), abilities=abilities,
        attack_ids=attack_ids,attacks=attacks,loot_ids=loot_ids,loot=enemy_loot_records(loot_ids),gold=c(as.integer(input$gold_min%||%0),as.integer(input$gold_max%||%0)),
        resistances=intersect(input$resistances%||%character(),enemy_damage_types()), immunities=intersect(input$immunities%||%character(),enemy_damage_types()), vulnerabilities=intersect(input$vulnerabilities%||%character(),enemy_damage_types()), condition_immunities=intersect(input$condition_immunities%||%character(),enemy_conditions()), tags=input$npc_tags)
    }
    persist <- function(rec, update=FALSE) {
      if(!nzchar(rec$name)) stop("Enter a template name."); p <- rec$attacks[[1]]; con <- get_db_connection(); if(is.null(con)) stop("Database unavailable."); on.exit(release_db_connection(con),add=TRUE)
      sql <- if(update) "UPDATE npc_templates SET name=$2,hp_max=$3,ac=$4,movement_speed=$5,attack_name=$6,attack_bonus=$7,damage_expr=$8,damage_type=$9,attacks_json=$10,tags=$11,enemy_type=$12,characteristics=$13::jsonb,abilities=$14::jsonb,attacks=$15::jsonb,loot=$16::jsonb,resistances=$17::text[],immunities=$18::text[],vulnerabilities=$19::text[],condition_immunities=$20::text[],gold_min=$21,gold_max=$22,updated_at=now() WHERE npc_id=$1" else "INSERT INTO npc_templates(npc_id,name,hp_max,ac,movement_speed,attack_name,attack_bonus,damage_expr,damage_type,attacks_json,tags,enemy_type,characteristics,abilities,attacks,loot,resistances,immunities,vulnerabilities,condition_immunities,gold_min,gold_max) VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13::jsonb,$14::jsonb,$15::jsonb,$16::jsonb,$17::text[],$18::text[],$19::text[],$20::text[],$21,$22)"
      DBI::dbExecute(con,sql,params=list(rec$npc_id,rec$name,rec$hp_max,rec$ac,rec$movement_speed,p$name,as.integer(p$hit),p$dmg,p$type,enemy_json(rec$attacks),rec$tags,rec$enemy_type,enemy_json(rec$characteristics),enemy_json(rec$abilities),enemy_json(rec$attacks),enemy_json(rec$loot),enemy_pg_array(rec$resistances),enemy_pg_array(rec$immunities),enemy_pg_array(rec$vulnerabilities),enemy_pg_array(rec$condition_immunities),min(rec$gold),max(rec$gold)))
    }
    save_handler <- function(update=FALSE) tryCatch({ id <- if(update) selected_id() else NULL; if(update && !nzchar(id)) stop("Load a saved template first."); persist(current_record(id),update); load_templates(); if(is.function(bump_refresh)) bump_refresh(); showNotification(if(update) "Template updated." else "Template saved.",type="message") },error=function(e)showNotification(conditionMessage(e),type="error"))
    observeEvent(input$save_npc,save_handler(FALSE),ignoreInit=TRUE); observeEvent(input$update_npc,save_handler(TRUE),ignoreInit=TRUE)
    filtered <- reactive({ df<-templates(); if(!nrow(df)) return(df); if(nzchar(input$filter_type%||%"")) df<-df[df$enemy_type==input$filter_type,,drop=FALSE]
      wanted<-input$filter_characteristics%||%character(); if(length(wanted)) df<-df[vapply(df$characteristics,function(x) all(wanted %in% unlist(parse_json(x))),logical(1)),,drop=FALSE]; df })
    observe({ updateSelectInput(session,"filter_type",choices=c("All"="",sort(unique(templates()$enemy_type %||% character()))),selected=input$filter_type%||%"") })
    output$npc_tbl<-renderDT({ df<-filtered(); if(!nrow(df)) return(NULL); chars<-vapply(df$characteristics,function(x)paste(unlist(parse_json(x)),collapse=", "),character(1)); view<-data.frame(name=df$name,type=df$enemy_type,characteristics=chars,hp=df$hp_max,ac=df$ac,speed=df$movement_speed,attack=df$attack_name,tags=df$tags); datatable(view,selection="single",rownames=FALSE,options=list(pageLength=10,scrollX=TRUE)) })
    selected_row <- function() { i<-input$npc_tbl_rows_selected; df<-filtered(); if(!length(i)||!nrow(df)) return(NULL); df[i[1],,drop=FALSE] }
    observeEvent(input$load_npc,{ r<-selected_row(); if(is.null(r)){showNotification("Select a template.",type="warning");return()}; selected_id(as.character(r$npc_id[1])); chars<-unlist(parse_json(r$characteristics[[1]])); updateSelectInput(session,"enemy_type",selected=r$enemy_type[1]); updateCheckboxGroupInput(session,"characteristics",selected=chars)
      stored_attacks<-parse_json(r$attacks[[1]]); attack_ids<-names(Filter(function(def)any(vapply(stored_attacks,function(a)identical(a$name,def$name),logical(1))),enemy_attack_catalog())); stored_loot<-parse_json(r$loot[[1]]); loot_ids<-names(Filter(function(def)any(vapply(stored_loot,function(a)identical(a$name,def$name),logical(1))),enemy_loot_catalog()))
      b<-list(enemy_type=r$enemy_type[1],characteristics=chars,hp_max=r$hp_max[1],ac=r$ac[1],movement_speed=r$movement_speed[1],abilities=parse_json(r$abilities[[1]]),attack_ids=attack_ids,loot_ids=loot_ids,gold=c(r$gold_min[1],r$gold_max[1]),resistances=enemy_db_values(r$resistances[[1]]),immunities=enemy_db_values(r$immunities[[1]]),vulnerabilities=enemy_db_values(r$vulnerabilities[[1]]),condition_immunities=enemy_db_values(r$condition_immunities[[1]])); apply_blueprint(b,r$name[1]); updateTextInput(session,"npc_tags",value=r$tags[1]) },ignoreInit=TRUE)
    observeEvent(input$delete_npc,{ r<-selected_row(); if(is.null(r)){showNotification("Select a template.",type="warning");return()}; con<-get_db_connection(); if(is.null(con))return(); on.exit(release_db_connection(con),add=TRUE); DBI::dbExecute(con,"delete from npc_templates where npc_id=$1",params=list(as.character(r$npc_id[1]))); selected_id(""); load_templates(); if(is.function(bump_refresh))bump_refresh() },ignoreInit=TRUE)
  })
}
