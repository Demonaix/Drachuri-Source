# Card-based skills module. Legacy table UI: server/archive/skills_module_legacy_2026-08-25.R
library(shiny)

skillsTabUI <- function(id) {
  ns <- NS(id)
  tabPanel("Skills",
    tags$style(HTML("
      .sc-page{padding:10px;color:#3e2f1c}.sc-head{display:flex;justify-content:space-between;align-items:center;gap:12px}.sc-head h3{font-family:Cinzel,serif;margin:0}.sc-hint{font-size:12px;opacity:.72}
      .sc-lane{display:grid;grid-template-columns:180px 1px minmax(0,1fr);gap:18px;padding:20px 0;border-bottom:1px solid rgba(104,75,35,.22)}.sc-rule{background:#b89b62}.sc-ability{display:flex;flex-direction:column;align-items:center}
      .sc-card{position:relative;border:0!important;padding:0!important;background:#ead6a8!important;border-radius:10px!important;overflow:hidden;box-shadow:0 5px 14px rgba(45,29,12,.25)}.sc-card img{width:100%;height:100%;object-fit:cover;display:block}.sc-card:hover{transform:translateY(-3px);box-shadow:0 9px 18px rgba(45,29,12,.34)}
      .sc-ability .sc-card{width:160px;aspect-ratio:4/5}.sc-ability-name{font:700 14px Cinzel,serif;margin-top:8px}.sc-save{font-size:11px;opacity:.8}.sc-corner{position:absolute;top:7px;padding:3px 7px;background:rgba(255,248,226,.94);border:1px solid #9d793f;border-radius:7px;font:700 13px Georgia,serif;color:#392810}.sc-score{left:7px}.sc-mod{right:7px}
      .sc-skills{display:flex;align-items:flex-start;gap:16px;flex-wrap:wrap}.sc-slot{width:148px;text-align:center}.sc-slot.expertise{width:306px}.sc-stack{width:148px;height:185px;margin-bottom:8px}.sc-slot.expertise .sc-stack{width:306px}.sc-skill-button{border:0!important;padding:0!important;background:transparent!important;box-shadow:none!important}.sc-visible-cards{display:flex;gap:10px}.sc-visible-card{width:148px;aspect-ratio:4/5}.sc-visible-card img{width:100%;height:100%;object-fit:cover;display:block}.sc-name{font:700 12px Cinzel,serif;line-height:1.2}.sc-edit{display:inline-flex;margin-left:5px;padding:1px 5px;border:1px solid #9d793f;border-radius:6px!important;background:#e4cc98!important;color:#50381f!important;text-decoration:none!important;box-shadow:0 1px 2px rgba(56,35,16,.25)}.sc-edit:hover{background:#f0dfb8!important;color:#342315!important}.sc-slot.empty .sc-skill-button,.sc-slot.incomplete .sc-skill-button{width:148px;height:185px;border:2px dashed #8b765888!important;border-radius:10px!important;background:#ede0be55!important;position:relative}.sc-slot.empty .sc-skill-button:after,.sc-slot.incomplete .sc-skill-button:after{position:absolute;inset:0;display:grid;place-items:center;font:700 11px Cinzel,serif;color:#806b4b}.sc-slot.empty .sc-skill-button:after{content:'Untrained'}.sc-slot.incomplete .sc-skill-button:after{content:'Choose card'}
      .sc-picker{display:flex;gap:12px;flex-wrap:wrap;justify-content:center}.sc-art{width:145px}.sc-art img{width:100%;aspect-ratio:4/5;object-fit:cover;border-radius:8px}.sc-art.active{outline:4px solid #5a8f54}.sc-fan{display:flex;justify-content:center;align-items:end;min-height:285px;padding:16px}.sc-fan-card{width:180px;aspect-ratio:4/5;margin:0 -22px}.sc-fan-card:first-child{transform:rotate(-8deg) translateY(10px)}.sc-fan-card:nth-child(2){z-index:2}.sc-fan-card:last-child{transform:rotate(8deg) translateY(10px)}.sc-result{text-align:center;font:800 21px Cinzel,serif}.sc-breakdown{max-width:560px;margin:12px auto;padding:12px 16px;border:1px solid #b89b62;border-radius:10px;background:#fff8e8}.sc-breakdown-row{display:flex;justify-content:space-between;gap:20px;padding:4px 0}.sc-breakdown-total{border-top:1px solid #b89b62;margin-top:5px;padding-top:8px;font-weight:800}.sc-party-member{margin:10px 0;padding:10px;border:1px solid #c9ad75;border-radius:10px}.sc-party-cards{display:flex;gap:8px;flex-wrap:wrap}.sc-party-cards img{width:74px;aspect-ratio:4/5;object-fit:cover;border-radius:6px}
      @media(max-width:800px){.sc-lane{grid-template-columns:1fr}.sc-rule{height:1px}.sc-skills{justify-content:center}}
    ")),
    div(class="sc-page",div(class="sc-head",div(h3("Ability & Skill Cards"),div(class="sc-hint","Click a card to roll. Reliable supports low rolls, Wild Card adds risk, and Inspired rewards high rolls.")),uiOutput(ns("magic_action"))),uiOutput(ns("lanes")),tags$hr(),h4("Granted Proficiencies & Defences"),uiOutput(ns("traits"))))
}

skillsTabServer <- function(id,state,restoring,add_log,char_rev) {
  moduleServer(id,function(input,output,session){
    ns<-session$ns; abs<-c("str","dex","con","int","bld_str","cha")
    labs<-c(str="Strength",dex="Dexterity",con="Constitution",int="Intelligence",bld_str="Blood Strength",cha="Charisma")
    dirs<-c(str="strength",dex="dexterity",con="constitution",int="intelligence",bld_str="blood-strength",cha="charisma")
    key<-function(x)gsub(" ","_",tolower(x)); signed<-function(x)if(x>=0)paste0("+",x)else as.character(x)
    core<-c(athletics="brute",clutch="grasp",wrestling="grappler",throwing="hurler",dead_lift="titan",acrobatics="acrobat",sleight_of_hand="quickhand",stealth="shadow",precision="marksman",endurance="bulwark",tolerance="ironblood",fortitude="stalwart",arcana="arcanist",history="chronicler",investigation="investigator",nature="naturalist",religion="theologian",analysis="strategist",perception="watcher",survival="stalker",insight="reader",medicine="healer",animal_handling="beastfriend",mandred_connection="mandred-touched",deception="trickster",intimidation="menace",persuasion="orator",performance="virtuoso",presence="luminary")
    aspect<-c(athletics="enforcer",clutch="binder",wrestling="wrestler",throwing="artillerist",dead_lift="bearer",acrobatics="daredevil",sleight_of_hand="pilferer",stealth="ghost",precision="deadeye",endurance="survivor",tolerance="resistant",fortitude="guardian",arcana="seer",history="lorekeeper",investigation="inquisitor",nature="warden",religion="devotee",analysis="tactician",perception="observer",survival="hunter",insight="empath",medicine="physician",animal_handling="handler",mandred_connection="conduit",deception="liar",intimidation="dread",persuasion="diplomat",performance="muse",presence="leader")
    descriptor<-c(athletics="mighty",clutch="tenacious",wrestling="relentless",throwing="keen",dead_lift="mighty",acrobatics="nimble",sleight_of_hand="cunning",stealth="elusive",precision="keen",endurance="hardy",tolerance="hardened",fortitude="resolute",arcana="mystic",history="learned",investigation="shrewd",nature="wildwise",religion="devout",analysis="calculating",perception="watchful",survival="seasoned",insight="intuitive",medicine="practised",animal_handling="beastwise",mandred_connection="touched",deception="cunning",intimidation="fearsome",persuasion="silver-tongued",performance="mesmeric",presence="commanding")
    pb<-reactive(character_proficiency_bonus(state$char)); score<-function(ab){x<-suppressWarnings(as.integer(state$char$abilities[[ab]]%||%10L));if(is.na(x))10L else x}; amod<-function(ab)mod_calc(score(ab))
    ability_img<-function(ab){s<-score(ab);t<-if(s<=1)-5 else if(s<=3)-4 else if(s<=5)-3 else if(s<=7)-2 else if(s<=9)-1 else if(s<=11)0 else if(s<=13)1 else if(s<=15)2 else if(s<=17)3 else if(s<=19)4 else 5;paste0("assets/ability-cards/",dirs[[ab]],"/",if(t<0)paste0("minus-",abs(t))else paste0("plus-",t),".jpg")}
    rank<-function(k)as.character(state$char$prof$skills[[k]]%||%"None")
    card_types<-c("reliable","wild_card","inspired")
    card_labels<-c(reliable="Reliable",wild_card="Wild Card",inspired="Inspired")
    card_help<-c(reliable="Natural 1–5: add proficiency bonus again.",wild_card="Roll d6: 1 subtracts proficiency bonus; 6 adds it.",inspired="Natural 16–20: add proficiency bonus again.")
    variants<-function(k,r=rank(k)) character_skill_cards(state$char,k)
    card_art<-c(reliable="core",wild_card="aspect",inspired="descriptor")
    skill_img<-function(k,v){art<-unname(card_art[[v]]%||%"core");map<-switch(art,core=core,aspect=aspect,descriptor=descriptor);folder<-switch(art,core="skill-cores",aspect="skill-aspects",descriptor="skill-descriptors");paste0("assets/skill-cards/",folder,"/",unname(map[[k]]%||%core[[k]]),".jpg")}
    smod<-function(skill)character_skill_modifier(state$char,skill,SKILLS_LIST)
    has_feature<-function(fid)any(vapply(get_unlocked_class_features(state$char),function(x)identical(as.character(x$id%||%""),fid),logical(1)))

    output$lanes <- renderUI({
      tagList(lapply(abs, function(ab) {
        ss <- SKILLS_LIST[SKILLS_LIST$Ability == ab, , drop = FALSE]
        saveprof <- isTRUE(state$char$prof$saves[[ab]] %||% FALSE)
        skill_cards <- lapply(seq_len(nrow(ss)), function(i) {
          sk <- ss$Skill[[i]]; k <- key(sk); r <- rank(k)
          trained <- r %in% c("Proficient", "Expertise"); complete<-length(variants(k,r))==skill_card_count_for_rank(r)
          div(class = paste("sc-slot", if (!trained) "empty" else if(!complete)"incomplete"else"", if(r=="Expertise"&&complete)"expertise"else""),
            div(class="sc-stack",
              actionButton(ns(paste0("skill_", k)),
                if (trained&&complete) div(class="sc-visible-cards",lapply(variants(k,r),function(v)div(class="sc-card sc-visible-card",tags$img(src=skill_img(k,v),alt=paste(sk,card_labels[[v]]))))) else NULL,
                class="sc-skill-button", title=if(complete)SKILL_DESC[[sk]]%||%sk else"Choose the required skill card first")),
            div(class="sc-name", sk,
              actionLink(ns(paste0("edit_",k)), " ✒", class="sc-edit", title="Change training and cards")))
        })
        div(class="sc-lane",
          div(class="sc-ability",
            actionButton(ns(paste0("ability_",ab)),
              tagList(tags$img(src=ability_img(ab)), span(class="sc-corner sc-score",score(ab)),
                span(class="sc-corner sc-mod",signed(amod(ab)))),
              class="sc-card", title=paste("Roll",labs[[ab]],"save")),
            div(class="sc-ability-name",labs[[ab]]),
            div(class="sc-save",paste("Save", signed(amod(ab)+if(saveprof)pb()else 0L), if(saveprof)"• proficient"else""))),
          div(class="sc-rule"), div(class="sc-skills", tagList(skill_cards)))
      }))
    })

    pending<-reactiveVal(NULL)
    choose_roll<-function(kind,label,ab,skill=NULL){
      pending(list(kind=kind,label=label,ability=ab,skill=skill));char<-validate_character(state$char);base<-amod(ab);modifier<-if(kind=="save")base+if(isTRUE(char$prof$saves[[ab]]%||%FALSE))pb()else 0L else smod(skill);cards<-if(kind=="skill")variants(key(skill))else character()
      showModal(modalDialog(title=paste(label,if(kind=="save")"saving throw"else"check"),
        div(class="sc-breakdown",div(class="sc-breakdown-row",span(paste(labs[[ab]],"modifier")),strong(signed(base))),div(class="sc-breakdown-row",span(if(kind=="skill")paste("Training:",rank(key(skill)))else"Saving throw proficiency"),strong(signed(modifier-base))),div(class="sc-breakdown-row sc-breakdown-total",span("Current modifier"),strong(signed(modifier))),if(length(cards))lapply(cards,function(v)div(class="sc-breakdown-row",span(card_labels[[v]]),span(card_help[[v]])))),
        if(kind=="skill")textInput(ns("context"),"What is the check about?"),if(kind=="skill")radioButtons(ns("scope"),"Attempt",c("Complete alone"="solo","Invite the active party"="party"),inline=TRUE),
        div(class="sc-picker",actionButton(ns("do_dis"),tags$img(src="assets/ability-cards/roll-mode/disadvantage.jpg",alt="Disadvantage"),class="sc-art",title="Roll with disadvantage"),actionButton(ns("do_normal"),tags$img(src="assets/ability-cards/roll-mode/straight-roll.jpg",alt="Straight roll"),class="sc-art",title="Make a straight roll"),actionButton(ns("do_adv"),tags$img(src="assets/ability-cards/roll-mode/advantage.jpg",alt="Advantage"),class="sc-art",title="Roll with advantage")),footer=modalButton("Cancel"),size="l",easyClose=TRUE))
    }
    lapply(abs,function(ab)observeEvent(input[[paste0("ability_",ab)]],choose_roll("save",labs[[ab]],ab),ignoreInit=TRUE))
    editor<-reactiveVal(NULL);required_prompt_open<-reactiveVal(FALSE)
    show_skill_editor<-function(k,required=FALSE){
      row<-SKILLS_LIST[key(SKILLS_LIST$Skill)==k,,drop=FALSE];if(!nrow(row))return()
      sk<-as.character(row$Skill[[1L]]);r<-rank(k);needed<-skill_card_count_for_rank(r);editor(list(key=k,skill=sk,required=isTRUE(required),original_rank=r))
      showModal(modalDialog(title=if(required)paste("Choose",sk,"skill cards")else paste("Change",sk),
        if(required)tagList(div(class="alert alert-warning",paste(r,"requires",needed,if(needed==1L)"card."else"different cards.")),tags$strong(paste("Training:",r)))else selectInput(ns("edit_rank"),"Training",choices=c("Untrained"="None","Proficient","Expertise"),selected=r),
        checkboxGroupInput(ns("edit_cards"),"Skill cards",choices=setNames(card_types,paste0(card_labels," — ",card_help)),selected=variants(k,r)),
        div(class="sc-picker",lapply(card_types,function(v)div(class="sc-art",tags$img(src=skill_img(k,v)),tags$strong(card_labels[[v]]),p(class="sc-hint",card_help[[v]])))),
        footer=tagList(if(!required)actionButton(ns("cancel_skill_editor"),"Cancel"),actionButton(ns("save_skill_cards"),"Save",class="btn btn-primary")),size="l",easyClose=FALSE))
    }
    lapply(seq_len(nrow(SKILLS_LIST)),function(i){sk<-SKILLS_LIST$Skill[[i]];ab<-SKILLS_LIST$Ability[[i]];k<-key(sk)
      observeEvent(input[[paste0("skill_",k)]],{if(length(variants(k))==skill_card_count_for_rank(rank(k)))choose_roll("skill",sk,ab,sk)else show_skill_editor(k,TRUE)},ignoreInit=TRUE)
      observeEvent(input[[paste0("edit_",k)]],show_skill_editor(k,FALSE),ignoreInit=TRUE)
    })
    observeEvent(input$save_skill_cards,{
      info<-editor();if(is.null(info))return();new_rank<-if(isTRUE(info$required))info$original_rank else as.character(input$edit_rank%||%"None")
      needed<-skill_card_count_for_rank(new_rank);selected<-unique(as.character(input$edit_cards%||%character()));selected<-selected[selected%in%card_types]
      if(length(selected)!=needed)return(showNotification(paste0(new_rank," requires exactly ",needed," skill card",if(needed==1L)"."else"s."),type="error",duration=7))
      x<-validate_character(state$char);x$prof$skills[[info$key]]<-new_rank;x$prof$skill_cards[[info$key]]<-selected;state$char<-x
      add_log(paste0("🃏 ",info$skill," changed to ",new_rank,if(length(selected))paste0(" with ",paste(card_labels[selected],collapse=" + "))else"."))
      editor(NULL);required_prompt_open(FALSE);removeModal()
    },ignoreInit=TRUE)
    observeEvent(input$cancel_skill_editor,{editor(NULL);removeModal()},ignoreInit=TRUE)
    observe({
      char_rev();x<-validate_character(state$char);if(isTRUE(required_prompt_open())||!is.null(editor()))return()
      keys<-key(SKILLS_LIST$Skill);missing<-Filter(function(k){needed<-skill_card_count_for_rank(x$prof$skills[[k]]%||%"None");needed>0L&&length(character_skill_cards(x,k))<needed},keys)
      if(length(missing)){required_prompt_open(TRUE);show_skill_editor(missing[[1L]],TRUE)}
    })

    online_sid<-function(){s<-suppressWarnings(as.integer(state$active_session_id%||%NA));if(isTRUE(state$offline_mode)||is.na(s)||s<1)NA_integer_ else s}
    roll <- function(mode) {
      info <- pending(); if (is.null(info)) return()
      char <- validate_character(state$char)
      modifier <- if(info$kind=="save") amod(info$ability)+if(isTRUE(char$prof$saves[[info$ability]]%||%FALSE))pb()else 0L else smod(info$skill)
      if(info$kind=="save"&&identical(info$ability,"dex")&&identical(mode,"Straight")&&has_feature("danger_sense")) mode<-"Advantage"
      if(info$kind=="skill"&&key(info$skill)=="persuasion"&&isTRUE(char$status$mind_bender_active%||%FALSE)){mode<-if(mode=="Disadvantage")"Straight"else"Advantage";char$status$mind_bender_active<-FALSE;state$char<-char}
      scope<-as.character(input$scope%||%"solo");context<-trimws(as.character(input$context%||%""));sid<-online_sid();cid<-as.character(state$char_id%||%"")
      a<-sample.int(20L,1L);b<-sample.int(20L,1L);die<-if(mode=="Advantage")max(a,b)else if(mode=="Disadvantage")min(a,b)else a;dice<-if(mode=="Straight")as.character(a)else paste(a,b,sep=" / ")
      card_result<-if(info$kind=="skill")resolve_skill_card_roll(die,modifier,pb(),variants(key(info$skill)))else NULL
      if(info$kind=="skill"&&scope=="party"&&!is.na(sid)&&nzchar(cid)){
        requester<-as.character(char$meta$name%||%"A party member");created<-create_party_skill_check(sid,info$skill,info$ability,context,cid,modifier,"party",requester,card_result)
        removeModal();pending(NULL)
        if(is.null(created)) return(showNotification("Could not create party check.",type="error",duration=8))
        publish_session_notification(sid,paste0(requester," asks for help with a ",info$skill," check."),cid,requester,"skill_check",paste0("skill-help-",created$id[[1L]]))
        check_id<-as.integer(created$id[[1L]]);later::later(function(){finalize_party_skill_check(check_id,cid)},15)
        showNotification("The active party has 15 seconds to respond. Helpers contribute -1, 0, or +1; total support is capped by your proficiency bonus.",type="message",duration=10)
        return()
      }
      card_bonus<-if(is.null(card_result))0L else card_result$card_bonus;total<-if(is.null(card_result))die+modifier else card_result$total
      skill_cards <- NULL
      if(info$kind=="skill" && rank(key(info$skill)) %in% c("Proficient","Expertise")) {
        skill_cards <- tagList(lapply(variants(key(info$skill)),function(v)div(class="sc-card sc-fan-card",tags$img(src=skill_img(key(info$skill),v)))))
      }
      effect_text<-if(!is.null(card_result)&&length(card_result$effects))paste(card_result$effects,collapse=" • ")else"";selected_text<-if(mode=="Straight")paste("Natural roll",die)else paste(mode,paste0(dice," → ",die))
      removeModal();showModal(modalDialog(title=paste(info$label,"result"),div(class="sc-fan",div(class="sc-card sc-fan-card",tags$img(src=ability_img(info$ability))),skill_cards,div(class="sc-card sc-fan-card",tags$img(src=paste0("assets/ability-cards/roll-mode/",switch(mode,Advantage="advantage",Disadvantage="disadvantage","straight-roll"),".jpg")))),div(class="sc-breakdown",div(class="sc-breakdown-row",span(selected_text),strong(die)),div(class="sc-breakdown-row",span(paste(info$label,"modifier")),strong(signed(modifier))),if(card_bonus||nzchar(effect_text))div(class="sc-breakdown-row",span(if(nzchar(effect_text))effect_text else"Skill cards"),strong(signed(card_bonus))),div(class="sc-breakdown-row sc-breakdown-total",span("Total"),strong(total))),footer=modalButton("Done"),size="l",easyClose=TRUE))
      add_log(paste0("🎲 ",info$label,if(info$kind=="save")" save"else" check",if(nzchar(context))paste0(" — ",context)else"",": ",dice," ",signed(modifier),if(card_bonus)paste0(" ",signed(card_bonus)," cards")else""," = ",total,if(nzchar(effect_text))paste0(" (",effect_text,")")else""));pending(NULL)
    }
    observeEvent(input$do_normal,roll("Straight"),ignoreInit=TRUE);observeEvent(input$do_adv,roll("Advantage"),ignoreInit=TRUE);observeEvent(input$do_dis,roll("Disadvantage"),ignoreInit=TRUE)

    output$magic_action<-renderUI({if(!has_feature("mind_bender"))return(NULL);active<-isTRUE(state$char$status$mind_bender_active%||%FALSE);actionButton(ns("use_mind_bender"),if(active)"Mind Bender active"else"Mind Bender • 10 Sindre",class="btn btn-primary",disabled=if(active)"disabled"else NULL)})
    observeEvent(input$use_mind_bender,{x<-validate_character(state$char);cur<-as.integer(x$resources$sindre$cur%||%0L);if(cur<10L)return(showNotification("Not enough Sindre.",type="error"));x$resources$sindre$cur<-cur-10L;x$status$mind_bender_active<-TRUE;state$char<-x;add_log("🧠 Mind Bender activated.")},ignoreInit=TRUE)
    output$traits<-renderUI({x<-validate_character(state$char);p<-x$combat_profile%||%list();line<-function(n,v)div(strong(paste0(n,": ")),if(length(v))paste(gsub("_"," ",unique(as.character(v))),collapse=", ")else"None");div(class="magic-card",line("Tools",names(Filter(isTRUE,x$prof$tools%||%list()))),line("Damage resistances",p$resistances),line("Damage immunities",p$immunities),line("Damage vulnerabilities",p$vulnerabilities))})

    pending_help<-reactiveVal(NULL);seen_help<-reactiveVal(integer());last_result<-reactiveVal(NA_integer_)
    observe({invalidateLater(2500,session);sid<-online_sid();cid<-as.character(state$char_id%||%"");if(is.na(sid)||!nzchar(cid)||!is.null(pending_help()))return();rows<-get_pending_party_skill_checks(sid,cid);fresh<-rows[!rows$id%in%seen_help(),,drop=FALSE];if(!nrow(fresh))return();r<-fresh[1,,drop=FALSE];seen_help(unique(c(seen_help(),r$id)));pending_help(as.integer(r$id[[1L]]));showModal(modalDialog(title=paste(r$requester_name[[1L]],"asks for help"),p("Skill: ",strong(r$skill[[1L]])),if(nzchar(r$context[[1L]]))p(r$context[[1L]]),footer=tagList(actionButton(ns("pass_help"),"Not this time"),actionButton(ns("accept_help"),"Assist",class="btn btn-success"))))})
    respond_to_group<-function(participate){id<-pending_help();if(is.null(id))return();rows<-get_pending_party_skill_checks(online_sid(),state$char_id);r<-rows[rows$id==id,,drop=FALSE];if(!nrow(r)){pending_help(NULL);removeModal();return()};char<-validate_character(state$char);sk<-as.character(r$skill[[1L]]);result<-respond_party_skill_check(id,state$char_id,char$meta$name%||%"Party member",participate,character_skill_modifier(char,sk,SKILLS_LIST),pb(),character_skill_cards(char,sk));pending_help(NULL);removeModal();if(is.null(result))showNotification("This group check could not record your response.",type="warning")else if(result$status=="pending")showNotification(if(participate)"Your roll and cards have been added. Waiting for the rest of the party."else"You declined. Waiting for the rest of the party.",type="message")}
    observeEvent(input$pass_help,respond_to_group(FALSE),ignoreInit=TRUE)
    observeEvent(input$accept_help,respond_to_group(TRUE),ignoreInit=TRUE)
    show_party_result<-function(row){
      contrib<-get_party_skill_check_contributions(row$id[[1L]]);k<-key(row$skill[[1L]])
      members<-if(!nrow(contrib))p("No contribution details were recorded.")else lapply(seq_len(nrow(contrib)),function(i){
        cards<-tryCatch(as.character(jsonlite::fromJSON(as.character(contrib$cards_json[[i]]))),error=function(e)character())
        effects<-tryCatch(as.character(jsonlite::fromJSON(as.character(contrib$effects_json[[i]]))),error=function(e)character())
        is_leader<-identical(as.character(contrib$character_id[[i]]),as.character(row$requester_character_id[[1L]]));support<-if(is_leader)NA_integer_ else if(contrib$total[[i]]<10L)-1L else if(contrib$total[[i]]>=15L)1L else 0L
        summary<-paste("Natural",contrib$natural_roll[[i]],signed(contrib$modifier[[i]]),if(contrib$card_bonus[[i]])paste0(" ",signed(contrib$card_bonus[[i]])," cards")else"")
        div(class="sc-party-member",tags$strong(as.character(contrib$character_name[[i]])),
          if(contrib$response[[i]]=="declined")span(" — declined")else tagList(
            div(class="sc-breakdown-row",span(summary),strong(paste("=",contrib$total[[i]]))),
            div(class="sc-breakdown-row",span(if(is_leader)"Lead result"else"Support contribution"),strong(if(is_leader)"Base"else signed(support))),
            if(length(effects))p(class="sc-hint",paste(effects,collapse=" • ")),
            if(length(cards))div(class="sc-party-cards",lapply(cards,function(v)tags$img(src=skill_img(k,v),alt=card_labels[[v]])))
          ))
      })
      showModal(modalDialog(title=paste("Party",row$skill[[1L]],"result"),p("The requester supplies the lead result. Helpers contribute -1 below 10, 0 from 10–14, or +1 at 15+, capped by the requester’s proficiency bonus."),members,div(class="sc-result",paste("Party total:",row$final_total[[1L]])),footer=modalButton("Done"),size="l",easyClose=TRUE))
    }
    observe({invalidateLater(2500,session);sid<-online_sid();if(is.na(sid))return();rows<-get_recent_party_skill_results(sid);ids<-as.integer(rows$id%||%integer());if(is.na(last_result())){last_result(if(length(ids))max(ids,na.rm=TRUE)else 0L);return()};fresh<-rows[ids>last_result(),,drop=FALSE];if(!nrow(fresh))return();last_result(max(as.integer(fresh$id),na.rm=TRUE));for(i in seq_len(nrow(fresh))){add_log(paste0("🎲 Party ",fresh$skill[[i]]," check: ",fresh$final_total[[i]]));show_party_result(fresh[i,,drop=FALSE])}})
  })
}
