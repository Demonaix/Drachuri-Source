library(shiny)

questPlayerUI <- function(id){ns<-NS(id);tagList(tags$style(HTML(paste0(
  "#",ns("wrap"),"{position:fixed;right:22px;bottom:306px;z-index:1040}",
  "#",ns("open"),"{width:58px;height:58px;border-radius:50%;font-size:25px;background:#3f5870;color:#fff4cf;border:2px solid #d7b96d;box-shadow:0 5px 18px rgba(0,0,0,.4)}",
  "#",ns("badge"),"{position:absolute;right:-3px;top:-5px;background:#8f241d;color:#fff;border-radius:999px;min-width:22px;padding:2px 6px;text-align:center;font-weight:bold}",
  ".quest-journal{display:grid;grid-template-columns:minmax(210px,32%) minmax(0,1fr);gap:18px;min-height:430px}.quest-list{border-right:1px solid #b89a62;padding-right:12px;max-height:58vh;overflow:auto}.quest-list button{display:block;width:100%;text-align:left;margin:0 0 7px;padding:10px;border:1px solid #b89a62;border-radius:7px;background:#ead8ad;color:#3b2917;font-family:Cinzel,Georgia,serif}.quest-list button.active{background:#5d4430;color:#fff2cf}.quest-description{white-space:pre-wrap;font:16px/1.55 Georgia,serif}.quest-objectives{list-style:none;padding:0;margin-top:20px}.quest-objectives li{padding:8px 5px;border-bottom:1px solid rgba(130,91,43,.25)}.quest-objectives li.done{text-decoration:line-through;opacity:.62}@media(max-width:700px){.quest-journal{grid-template-columns:1fr}.quest-list{border-right:0;border-bottom:1px solid #b89a62;padding:0 0 10px;max-height:180px}}"
))),div(id=ns("wrap"),actionButton(ns("open"),"✦",title="Party Quests"),uiOutput(ns("badge"))))}

questPlayerServer <- function(id,state){moduleServer(id,function(input,output,session){
  `%||%`<-get("%||%",inherits=TRUE);rev<-reactiveVal(0L);poll<-reactiveTimer(4000,session);selected<-reactiveVal(NA_integer_)
  sid<-reactive(suppressWarnings(as.integer(state$active_session_id%||%NA)))
  quests<-reactive({poll();rev();if(is.na(sid()))return(data.frame());list_party_quests(sid(),FALSE)})
  output$badge<-renderUI({q<-quests();n<-if(nrow(q))sum(q$status=="active")else 0L;if(n>0)span(n)})
  body_ui<-function(){q<-quests();if(!nrow(q))return(div(class="quest-journal",div(class="quest-list",em("No quests recorded.")),div(h3("The road ahead is unwritten"),p("The Game Master has not added any shared quests yet."))));qid<-selected();if(is.na(qid)||!qid%in%q$id){qid<-as.integer(q$id[[1L]]);selected(qid)};bundle<-get_party_quest(qid);if(is.null(bundle))return(p("That quest is no longer available."));quest<-bundle$quest;objectives<-bundle$objectives
    div(class="quest-journal",div(class="quest-list",lapply(seq_len(nrow(q)),function(i){row<-q[i,,drop=FALSE];tags$button(type="button",class=if(as.integer(row$id[[1L]])==qid)"active"else NULL,onclick=sprintf("Shiny.setInputValue('%s',%d,{priority:'event'})",session$ns("select"),as.integer(row$id[[1L]])),div(row$title[[1L]]),tags$small(paste0(tools::toTitleCase(row$status[[1L]]),if(row$objective_count[[1L]]>0)paste0(" · ",row$completed_count[[1L]],"/",row$objective_count[[1L]])else"")))})),div(class="quest-detail",h2(quest$title[[1L]]),div(class="quest-description",if(nzchar(quest$description[[1L]]))quest$description[[1L]]else"No description has been recorded."),if(nrow(objectives))tags$ul(class="quest-objectives",lapply(seq_len(nrow(objectives)),function(i)tags$li(class=if(isTRUE(objectives$is_complete[[i]]))"done"else NULL,if(isTRUE(objectives$is_complete[[i]]))"✓ "else"◇ ",objectives$objective_text[[i]])))else p(em("No objectives recorded."))))
  }
  output$journal<-renderUI(body_ui())
  show_journal<-function(){showModal(modalDialog(title="Party Quest Journal",uiOutput(session$ns("journal")),footer=modalButton("Close"),size="l",easyClose=TRUE))}
  observeEvent(input$open,show_journal(),ignoreInit=TRUE)
  observeEvent(input$select,{selected(as.integer(input$select));rev(rev()+1L)},ignoreInit=TRUE)
})}
