controlGeographyClimateUI <- function(id) {
  ns<-NS(id)
  div(class="control-card",
    div(class="control-section-title","Geography & Climate"),
    p(class="control-mini","This is the party's shared clock. Changes appear for every connected player."),
    uiOutput(ns("clock_summary")),
    fluidRow(
      column(3,numericInput(ns("day_number"),"Campaign day",1,min=1,step=1)),
      column(3,selectInput(ns("time_of_day"),"Time of day",c("Dawn"="dawn","Day"="day","Dusk"="dusk","Night"="night"))),
      column(3,textInput(ns("geography"),"Geography","Temperate wilderness")),
      column(3,selectInput(ns("climate"),"Climate",c("Temperate","Cold","Hot","Arid","Tropical","Alpine")))
    ),
    fluidRow(column(8,textInput(ns("weather"),"Weather","Clear")),column(4,br(),actionButton(ns("save_environment"),"Save Environment",class="btn btn-primary"))),
    tags$hr(),
    h4("Phase control"),uiOutput(ns("phase_summary")),
    div(style="display:flex;gap:8px;flex-wrap:wrap;align-items:flex-end",selectInput(ns("phase_duration"),"Rest phase length",c("6 hours (one phase)"="6","12 hours (two phases)"="12"),selected="6",width="210px"),actionButton(ns("open_standard"),"Begin Standard Phase",class="btn btn-default"),actionButton(ns("open_rest"),"Begin Rest Phase",class="btn btn-primary")),
    div(class="control-mini",style="margin-top:10px;","When resolving, choose what begins immediately afterwards. Players will never be left between phases."),
    div(style="display:flex;gap:8px;flex-wrap:wrap;align-items:flex-end",selectInput(ns("next_phase_kind"),"Next phase",c("Standard phase"="standard","Rest phase"="rest"),selected="standard",width="190px"),selectInput(ns("next_phase_duration"),"Next rest length",c("6 hours"="6","12 hours"="12"),selected="6",width="160px"),actionButton(ns("resolve_phase"),"Resolve & Begin Next Phase",class="btn btn-warning")),
    uiOutput(ns("party_allocations"))
  )
}

controlGeographyClimateServer <- function(id,ctrl,bump_refresh=NULL) {
  moduleServer(id,function(input,output,session){
    `%||%`<-get("%||%",inherits=TRUE);clock<-reactiveVal(NULL);phase<-reactiveVal(NULL)
    sid<-reactive({x<-suppressWarnings(as.integer(ctrl$session_id%||%NA));if(is.na(x)||x<1L)NULL else x})
    refresh<-function(){if(is.null(sid())){clock(NULL);phase(NULL)}else{clock(get_session_environment(sid()));phase(get_open_session_phase(sid()))}}
    observe({invalidateLater(2500,session);sid();refresh()})
    observeEvent(clock(),{x<-clock();if(is.null(x))return();updateNumericInput(session,"day_number",value=x$day_number[[1L]]);updateSelectInput(session,"time_of_day",selected=x$time_of_day[[1L]]);updateTextInput(session,"geography",value=x$geography[[1L]]);updateSelectInput(session,"climate",selected=x$climate[[1L]]);updateTextInput(session,"weather",value=x$weather[[1L]])},ignoreInit=FALSE)
    output$clock_summary<-renderUI({x<-clock();if(is.null(x))return(div(class="alert alert-warning","Select an active session first."));div(class="control-kpi-row",span(class="control-kpi",paste("Day",x$day_number[[1L]])),span(class="control-kpi",time_of_day_label(x$time_of_day[[1L]])),span(class="control-kpi",x$weather[[1L]]),span(class="control-kpi",x$climate[[1L]]))})
    output$phase_summary<-renderUI({x<-phase();if(is.null(x))return(div(class="alert alert-info","No phase is open. Choose whether the next block of time is a standard or rest phase."));div(class="alert alert-success",strong(if(x$phase_kind[[1L]]=="rest")"Rest Mode is open"else"Standard phase is open"),paste0(" — ",x$duration_hours[[1L]]," hours. The shared clock has not moved yet."))})
    output$party_allocations<-renderUI({x<-phase();if(is.null(x))return(NULL);players<-get_session_players(sid());if("is_active"%in%names(players))players<-players[is.na(players$is_active)|players$is_active,,drop=FALSE];confirmed<-as.character(get_session_phase_confirmations(x$id[[1L]])$character_id%||%character());labels<-setNames(as.character(players$display_name%||%players$character_id),as.character(players$character_id));ready<-lapply(as.character(players$character_id),function(cid)div(class="control-mini",strong(labels[[cid]]%||%cid),if(cid%in%confirmed)" — ✓ Ready"else" — Waiting"));a<-if(x$phase_kind[[1L]]=="rest")get_session_phase_actions(x$id[[1L]])else data.frame();plans<-if(nrow(a)){rows<-split(a,as.character(a$character_id));lapply(names(rows),function(cid){r<-rows[[cid]];div(class="control-mini",strong(labels[[cid]]%||%cid),": ",paste0(r$label," (",r$hours,"h)",collapse=", "))})}else NULL;tagList(h4("Player confirmation"),ready,if(length(plans))tagList(h4("Player plans"),plans)else NULL)})
    changed<-function(message){refresh();if(is.function(bump_refresh))bump_refresh();showNotification(message,type="message")}
    observeEvent(input$save_environment,{req(sid());x<-set_session_environment(sid(),input$day_number,input$time_of_day,input$geography,input$climate,input$weather);if(is.null(x))return(showNotification("Environment could not be saved.",type="error"));changed("Party environment updated.")},ignoreInit=TRUE)
    begin<-function(kind){req(sid());if(!is.null(get_open_session_phase(sid())))return(showNotification("Resolve the current phase first.",type="warning"));hours<-if(kind=="rest")as.numeric(input$phase_duration%||%6)else 6;x<-open_session_phase(sid(),kind,hours);if(is.null(x))return(showNotification("The phase could not be opened.",type="error"));changed(if(kind=="rest")paste0("Rest Mode opened for ",hours," hours for all players.")else"Standard phase opened. Players continue normally until you resolve it.")}
    observeEvent(input$open_standard,{begin("standard")},ignoreInit=TRUE);observeEvent(input$open_rest,{begin("rest")},ignoreInit=TRUE)
    observeEvent(input$resolve_phase,{x<-phase();req(x);players<-get_session_players(sid());if("is_active"%in%names(players))players<-players[is.na(players$is_active)|players$is_active,,drop=FALSE];confirmed<-as.character(get_session_phase_confirmations(x$id[[1L]])$character_id%||%character());missing<-as.character(players$character_id)[!as.character(players$character_id)%in%confirmed];if(length(missing))return(showNotification(paste(length(missing),"active player(s) have not confirmed this phase."),type="warning",duration=8));next_kind<-as.character(input$next_phase_kind%||%"standard");result<-resolve_session_phase(x$id[[1L]],next_kind,as.numeric(input$next_phase_duration%||%6));if(is.null(result)||is.null(result$next_phase))return(showNotification("The phase resolved, but the next phase could not be opened. Please begin one immediately.",type="error",duration=10));changed(paste("Phase resolved. It is now",time_of_day_label(result$environment$time_of_day[[1L]]),"on day",result$environment$day_number[[1L]],"and the next",next_kind,"phase is open."))},ignoreInit=TRUE)
  })
}
