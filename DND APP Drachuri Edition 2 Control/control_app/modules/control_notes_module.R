library(shiny)

controlNotesUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="control-card",
      div(class="control-section-title","✉ Party Notes"),
      p("Review notes exchanged during the active session. Notes addressed to the Game Master can be acknowledged here."),
      actionButton(ns("refresh_notes"),"Refresh",class="btn btn-default"),
      uiOutput(ns("notes_ui"))
    )
  )
}

controlNotesServer <- function(id, session_id) {
  moduleServer(id,function(input,output,session){
    `%||%`<-get("%||%",inherits=TRUE);key<-reactiveVal(0L);poll<-reactiveTimer(3000,session)
    notes<-reactive({poll();key();sid<-suppressWarnings(as.integer(session_id()%||%NA));if(is.na(sid))return(data.frame());get_session_private_notes(sid,100L)})
    observeEvent(input$refresh_notes,{key(key()+1L)},ignoreInit=TRUE)
    output$notes_ui<-renderUI({rows<-notes();if(!nrow(rows))return(div(style="margin-top:14px","No notes have been sent in this session."));
      tagList(lapply(seq_len(nrow(rows)),function(i){row<-rows[i,,drop=FALSE];to_dm<-identical(as.character(row$recipient_character_id[[1]]),"__dm__");status<-as.character(row$status[[1]]%||%"sent");
        div(class="control-card",style="margin-top:12px;background:rgba(255,248,220,.92)",
          div(tags$strong(paste0(row$sender_name[[1]]%||%"Player"," → ",row$recipient_name[[1]]%||%if(to_dm)"Game Master"else"Player")),span(style="float:right",tools::toTitleCase(status))),
          div(style="white-space:pre-wrap;margin:10px 0;font-family:Georgia,serif",as.character(row$body[[1]])),
          tags$small(format(as.POSIXct(row$created_at[[1]]),"%d %b %Y %H:%M")),
          if(to_dm&&!identical(status,"acknowledged"))tags$button(type="button",class="btn btn-primary",style="float:right",onclick=sprintf("Shiny.setInputValue('%s', {id:%d, nonce:Math.random()}, {priority:'event'})",session$ns("ack_note"),as.integer(row$id[[1]])),"Acknowledge")else NULL
        )
      }))
    })
    observeEvent(input$ack_note,{id<-suppressWarnings(as.integer(input$ack_note$id%||%NA));if(!is.na(id)&&mark_private_note(id,"__dm__","acknowledged")){showNotification("The sender will be told that you acknowledged their note.",type="message");key(key()+1L)}},ignoreInit=TRUE)
  })
}
