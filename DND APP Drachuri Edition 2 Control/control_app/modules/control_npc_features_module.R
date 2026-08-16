library(shiny)

controlNpcFeaturesUI <- function(id) {
  ns <- NS(id)
  tagList(div(class="control-card",
    div(class="control-section-title","NPC Feature / Characteristic Rules"),
    fluidRow(
      column(4, selectInput(ns("feature_id"),"Feature",choices=character()), textInput(ns("name"),"Name"),
             textInput(ns("desc"),"Description"), textInput(ns("ability_bonus"),"Ability changes",placeholder="str=2, dex=-1"),
             numericInput(ns("hp_multiplier"),"HP multiplier",1,min=.1,step=.1), numericInput(ns("ac_bonus"),"AC bonus",0),
             numericInput(ns("movement_bonus"),"Movement bonus",0), selectizeInput(ns("attacks"),"Added attacks",choices=character(),multiple=TRUE),
             actionButton(ns("new_feature"),"New Feature"), actionButton(ns("save_feature"),"Save Feature",class="btn btn-primary")),
      column(8, h4("Inventory contributed by this feature"),
             fluidRow(column(4,selectInput(ns("item_id"),"Item",choices=character())),
                      column(2,numericInput(ns("chance"),"Chance / weight",100,min=0,max=100)),
                      column(3,textInput(ns("group"),"Alternative group")),
                      column(3,checkboxInput(ns("required"),"Guarantee group",FALSE))),
             actionButton(ns("add_rule"),"Add / Update Rule"), actionButton(ns("remove_rule"),"Remove Rule"),
             selectInput(ns("rule_id"),"Current rules",choices=character()), actionButton(ns("roll_preview"),"Roll Example"), uiOutput(ns("preview")))
    )
  ))
}

controlNpcFeaturesServer<-function(id){moduleServer(id,function(input,output,session){`%||%`<-get("%||%",inherits=TRUE);path<-file.path("control_app","data","npc_features.rds");inv_path<-file.path("control_app","data","control_inventory.rds");dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  parse_bonus<-function(x){parts<-trimws(strsplit(x%||%"",",",fixed=TRUE)[[1]]);out<-numeric();for(p in parts){z<-trimws(strsplit(p,"=",fixed=TRUE)[[1]]);if(length(z)==2&&!is.na(as.numeric(z[2])))out[tolower(z[1])]<-as.numeric(z[2])};out}
  seed<-lapply(names(enemy_generator_characteristics()),function(k){x<-enemy_generator_characteristics()[[k]];list(id=k,name=x$name%||%gsub("_"," ",k),desc=x$desc%||%"",ability_bonus=x$ability_bonus%||%numeric(),hp_multiplier=x$hp_multiplier%||%1,ac_bonus=x$ac_bonus%||%0,movement_bonus=x$movement_bonus%||%0,attacks=x$attack_ids%||%character(),rules=lapply(x$loot_ids%||%character(),function(i)list(item_id=i,chance=100,group="",required=FALSE)))})
  features<-reactiveVal({x<-tryCatch(readRDS(path),error=function(e)list());if(length(x))x else seed});inventory<-reactive({invalidateLater(1500,session);x<-tryCatch(readRDS(inv_path),error=function(e)list());b<-lapply(names(enemy_loot_catalog()),function(k){z<-enemy_loot_catalog()[[k]];z$id<-k;z});ids<-vapply(x,function(z)z$id,character(1));c(x,Filter(function(z)!z$id%in%ids,b))});active<-reactive({x<-Filter(function(z)identical(z$id,input$feature_id%||%""),features());if(length(x))x[[1]]else NULL})
  observe({x<-features();vals<-vapply(x,`[[`,"","id");keep<-input$feature_id%||%"";updateSelectInput(session,"feature_id",choices=setNames(vals,vapply(x,`[[`,"","name")),selected=if(keep%in%vals)keep else vals[1]);updateSelectizeInput(session,"attacks",choices=setNames(names(enemy_attack_catalog()),vapply(enemy_attack_catalog(),`[[`,"","name")),selected=input$attacks%||%character(),server=TRUE)})
  observe({x<-inventory();vals<-vapply(x,`[[`,"","id");updateSelectInput(session,"item_id",choices=setNames(vals,vapply(x,`[[`,"","name")),selected=if((input$item_id%||%"")%in%vals)input$item_id else vals[1])})
  observeEvent(input$feature_id,{x<-active();if(is.null(x))return();updateTextInput(session,"name",value=x$name);updateTextInput(session,"desc",value=x$desc);updateTextInput(session,"ability_bonus",value=paste(names(x$ability_bonus),x$ability_bonus,sep="=",collapse=", "));updateNumericInput(session,"hp_multiplier",value=x$hp_multiplier);updateNumericInput(session,"ac_bonus",value=x$ac_bonus);updateNumericInput(session,"movement_bonus",value=x$movement_bonus);updateSelectizeInput(session,"attacks",selected=x$attacks)},ignoreInit=TRUE)
  observe({x<-active();if(is.null(x))return();it<-inventory();choices<-setNames(vapply(x$rules,function(r)r$item_id,character(1)),vapply(x$rules,function(r){z<-Filter(function(i)identical(i$id,r$item_id),it);paste0(if(length(z))z[[1]]$name else r$item_id," — ",r$chance,"%",if(nzchar(r$group))paste0(" [",r$group,"]")else"")},character(1)));updateSelectInput(session,"rule_id",choices=choices)})
  save_all<-function(x){features(x);saveRDS(x,path)};observeEvent(input$new_feature,{x<-features();id<-paste0("feature_",as.integer(Sys.time()));x<-c(x,list(list(id=id,name="New Feature",desc="",ability_bonus=numeric(),hp_multiplier=1,ac_bonus=0,movement_bonus=0,attacks=character(),rules=list())));save_all(x);updateSelectInput(session,"feature_id",selected=id)})
  observeEvent(input$save_feature,{x<-features();i<-which(vapply(x,function(z)identical(z$id,input$feature_id),logical(1)))[1];if(is.na(i))return();x[[i]]$name<-input$name%||%x[[i]]$name;x[[i]]$desc<-input$desc%||%"";x[[i]]$ability_bonus<-parse_bonus(input$ability_bonus);x[[i]]$hp_multiplier<-input$hp_multiplier%||%1;x[[i]]$ac_bonus<-input$ac_bonus%||%0;x[[i]]$movement_bonus<-input$movement_bonus%||%0;x[[i]]$attacks<-input$attacks%||%character();save_all(x);showNotification("Feature saved.")})
  alter<-function(remove=FALSE){x<-features();i<-which(vapply(x,function(z)identical(z$id,input$feature_id),logical(1)))[1];if(is.na(i))return();id<-if(remove)input$rule_id else input$item_id;x[[i]]$rules<-Filter(function(r)!identical(r$item_id,id),x[[i]]$rules);if(!remove)x[[i]]$rules<-c(x[[i]]$rules,list(list(item_id=id,chance=input$chance%||%100,group=trimws(input$group%||%""),required=isTRUE(input$required))));save_all(x)};observeEvent(input$add_rule,alter(FALSE));observeEvent(input$remove_rule,alter(TRUE))
  rolled<-eventReactive(input$roll_preview,{x<-active();if(is.null(x))return(character());r<-x$rules;out<-vapply(Filter(function(z)!nzchar(z$group)&&runif(1)*100<=z$chance,r),`[[`,"","item_id");g<-Filter(function(z)nzchar(z$group),r);for(set in split(g,vapply(g,`[[`,"","group"))){if(any(vapply(set,function(z)isTRUE(z$required),logical(1)))||runif(1)*100<=sum(vapply(set,`[[`,0,"chance")))out<-c(out,sample(vapply(set,`[[`,"","item_id"),1,prob=vapply(set,`[[`,0,"chance")))};out});output$preview<-renderUI({tags$ul(lapply(rolled(),tags$li))})
})}
