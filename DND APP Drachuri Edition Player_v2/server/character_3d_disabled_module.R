character3DTabUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel("Combat Markers", value="character_3d",
    shiny::tags$style(shiny::HTML(sprintf("#%s .marker-preview{display:flex;gap:30px;align-items:center;justify-content:center;min-height:180px}#%s .marker-2d{width:92px;height:92px;display:flex;align-items:center;justify-content:center;color:white;font-size:30px;font-weight:bold;border:5px solid rgba(255,255,255,.75);box-shadow:0 5px 18px rgba(0,0,0,.35)}#%s .marker-2d.shield{border-radius:45%% 45%% 55%% 55%% / 35%% 35%% 65%% 65%%}#%s .marker-2d.circle{border-radius:50%%}#%s .marker-2d.diamond{transform:rotate(45deg)}#%s .marker-2d.diamond span{transform:rotate(-45deg)}#%s .marker-3d{width:72px;height:112px;border-radius:50%%;filter:blur(1px);box-shadow:0 0 12px currentColor,0 0 30px currentColor,inset 0 0 20px currentColor;opacity:.78}",ns("root"),ns("root"),ns("root"),ns("root"),ns("root"),ns("root"),ns("root")))),
    shiny::div(id=ns("root"),shiny::h3("Combat Marker Workshop"),shiny::p("Customise how your character appears on the 2D and 3D combat maps."),
      shiny::fluidRow(
        shiny::column(6,shiny::div(class="magic-card",shiny::h4("2D combat marker"),shiny::selectInput(ns("marker_2d_shape"),"Token shape",c("Round"="circle","Shield"="shield","Diamond"="diamond")),shiny::tags$label(`for`=ns("marker_2d_color"),"Token colour"),shiny::tags$input(id=ns("marker_2d_color"),type="color",value="#4b91b5",class="form-control"),shiny::textInput(ns("marker_2d_symbol"),"Symbol (up to 2 characters)",""))),
        shiny::column(6,shiny::div(class="magic-card",shiny::h4("3D combat marker"),shiny::selectInput(ns("marker_3d_style"),"Marker aura",c("Arcane wisps"="wisps","Steady beacon"="beacon","Subtle"="subtle")),shiny::tags$label(`for`=ns("marker_3d_color"),"Aura colour"),shiny::tags$input(id=ns("marker_3d_color"),type="color",value="#77ddff",class="form-control")))
      ),
      shiny::div(class="magic-card",shiny::h4("Preview"),shiny::uiOutput(ns("marker_preview"))),
      shiny::actionButton(ns("save_markers"),"Save Combat Markers",class="btn btn-primary")
    )
  )
}

character3DTabServer <- function(id, state, restoring, add_log, char_rev = NULL) {
  shiny::moduleServer(id,function(input,output,session){
    `%||%`<-get("%||%",inherits=TRUE)
    current<-shiny::reactive({if(is.function(char_rev))char_rev();state$char$character_3d%||%list()})
    shiny::observe({x<-current();shiny::updateSelectInput(session,"marker_2d_shape",selected=x$marker_2d_shape%||%"circle");shiny::updateTextInput(session,"marker_2d_symbol",value=x$marker_2d_symbol%||%"");shiny::updateSelectInput(session,"marker_3d_style",selected=x$marker_3d_style%||%"wisps");session$sendInputMessage("marker_2d_color",list(value=x$marker_2d_color%||%"#4b91b5"));session$sendInputMessage("marker_3d_color",list(value=x$marker_3d_color%||%"#77ddff"))})
    output$marker_preview<-shiny::renderUI({symbol<-substr(trimws(input$marker_2d_symbol%||%""),1L,2L);if(!nzchar(symbol))symbol<-substr(toupper(state$char$meta$name%||%"?"),1L,1L);shiny::div(class="marker-preview",shiny::div(class=paste("marker-2d",input$marker_2d_shape%||%"circle"),style=paste0("background:",input$marker_2d_color%||%"#4b91b5"),shiny::span(symbol)),shiny::div(class="marker-3d",title=paste("3D aura:",input$marker_3d_style%||%"wisps"),style=paste0("color:",input$marker_3d_color%||%"#77ddff",";background:",input$marker_3d_color%||%"#77ddff")))})
    shiny::observeEvent(input$save_markers,{if(isTRUE(restoring()))return();x<-current();x$marker_2d_shape<-input$marker_2d_shape%||%"circle";x$marker_2d_color<-input$marker_2d_color%||%"#4b91b5";x$marker_2d_symbol<-substr(trimws(input$marker_2d_symbol%||%""),1L,2L);x$marker_3d_style<-input$marker_3d_style%||%"wisps";x$marker_3d_color<-input$marker_3d_color%||%"#77ddff";state$char$character_3d<-x;add_log("Combat marker appearance saved.")},ignoreInit=TRUE)
  })
}
