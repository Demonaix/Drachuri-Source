# server/party_hud_module.R
library(shiny)

partyHudUI <- function(id) {
  ns <- NS(id)
  root_id <- ns("partyhud_root")
  
  css <- paste0("
#", root_id, "{
  position: fixed;
  top: 110px;
  left: 0px;
  width: var(--party-hud-width, clamp(154px, 13.5vw, 194px));
  z-index: 10000;
  pointer-events: none;
  max-height: calc(100vh - 125px);
  overflow-y: auto;
  overflow-x: hidden;
  scrollbar-width: none;
  -ms-overflow-style: none;
}
#", root_id, "::-webkit-scrollbar{width:0;height:0;display:none;}

    #", root_id, " .partyhud-shell{
      display: flex;
      flex-direction: column;
      align-items: flex-end;
      gap: 8px;
      width: 100%;
    }

    #", root_id, " .partyhud-label{
      align-self: flex-end;
      margin-right: 2px;
      padding: 3px 8px;
      border-radius: 999px;
      background: rgba(255,255,245,0.90);
      color: #3e2f1c;
      border: 1px solid rgba(191,167,111,0.82);
      font-size: 10px;
      font-weight: 900;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      box-shadow: 0 8px 20px rgba(0,0,0,0.22);
      pointer-events: auto;
    }

    #", root_id, " .partyhud-empty{
      width: calc(100% - 12px);
      padding: 7px 9px;
      border-radius: 3px;
      border: 1px solid rgba(91,56,30,0.88);
      background: linear-gradient(135deg, rgba(255,244,200,0.94), rgba(196,153,88,0.94));
      color: #3e2f1c;
      font-size: 10px;
      box-shadow: 0 8px 20px rgba(0,0,0,0.18);
      pointer-events: auto;
    }

    #", root_id, " .party-row{
      width: calc(100% - 12px);
      pointer-events: none;
    }

    #", root_id, " .party-strip{
      width: 100%;
      box-sizing: border-box;
      position: relative;
      min-height: 92px;
      padding: 7px 7px 7px 72px;
      border-radius: 3px 6px 4px 2px;
      border: 1px solid rgba(91,56,30,0.9);
      background:
        radial-gradient(circle at 18% 12%, rgba(255,248,207,0.7), transparent 32%),
        linear-gradient(135deg, rgba(225,199,139,0.96), rgba(184,139,77,0.96));
      color: #3e2f1c;
      overflow: hidden;
      box-shadow: 0 3px 0 rgba(67,38,18,0.30), 0 9px 22px rgba(0,0,0,0.26);
      pointer-events: auto;
    }

    #", root_id, " .party-strip::after{
      content:'';
      position:absolute;
      inset:3px;
      border:1px solid rgba(91,56,30,0.24);
      pointer-events:none;
    }

    #", root_id, " .party-portrait{
      position:absolute;
      left:4px;
      top:4px;
      width:62px;
      height:82px;
      display:flex;
      align-items:center;
      justify-content:center;
      overflow:hidden;
      border:1px solid #5c3a20;
      background:linear-gradient(145deg,#d9bd82,#9f7440);
      box-shadow:0 2px 6px rgba(47,27,13,.42);
      transform:none;
      z-index:1;
    }

    #", root_id, " .party-portrait img{width:100%;height:100%;object-fit:cover;object-position:center 20%;transform:none;}
    #", root_id, " .party-portrait-initial{font:900 27px Georgia,serif;color:#51331e;text-shadow:0 1px rgba(255,239,190,.7);}
    #", root_id, " .party-portrait.enemy{filter:saturate(.65);}

    #", root_id, " .party-strip.active-turn{
      border-color: rgba(125,28,28,0.98);
      background: linear-gradient(135deg, rgba(231,194,154,0.98), rgba(181,105,78,0.98));
      box-shadow: 0 0 0 2px rgba(125,28,28,0.32), 0 8px 20px rgba(0,0,0,0.22);
    }

    #", root_id, " .party-strip.self-player{
      border-color: rgba(36,130,190,0.98);
      background: linear-gradient(135deg, rgba(224,205,158,0.98), rgba(139,177,174,0.98));
      box-shadow: 0 0 0 2px rgba(36,130,190,0.22), 0 8px 20px rgba(0,0,0,0.18);
    }

    #", root_id, " .party-strip.deck-available{cursor:pointer;transition:transform .14s ease,filter .14s ease;}
    #", root_id, " .party-strip.deck-available:hover,#", root_id, " .party-strip.deck-available:focus{transform:translateX(4px);filter:brightness(1.06);outline:2px solid rgba(215,185,109,.9);outline-offset:-2px;}

    .modal-dialog:has(.party-deck-modal){width:min(1120px,94vw)}
    .party-deck-intro{margin:-4px 0 14px;color:#655238}
    .party-deck-section{margin:15px 0 22px}.party-deck-section h4{font-family:Cinzel,Georgia,serif;border-bottom:1px solid rgba(139,103,51,.45);padding-bottom:6px}
    .party-deck-grid{display:flex;flex-wrap:wrap;gap:13px;align-items:flex-start}
    .party-deck-card{width:128px;padding:0 0 8px!important;border:1px solid #9c7740!important;border-radius:10px!important;overflow:hidden;background:#ead6a8!important;color:#392810!important;box-shadow:0 5px 13px rgba(45,29,12,.27);white-space:normal!important}
    .party-deck-card:hover,.party-deck-card:focus{transform:translateY(-5px);box-shadow:0 10px 20px rgba(45,29,12,.38)}
    .party-deck-card img{display:block;width:126px;height:158px;object-fit:cover}.party-deck-card span{display:block;padding:7px 6px 0;font:700 11px Cinzel,Georgia,serif;line-height:1.25}
    .party-deck-art{position:relative}.party-deck-art img{width:100%}.party-deck-ability-score,.party-deck-ability-mod{position:absolute;display:flex!important;align-items:center;justify-content:center;padding:0!important;border-radius:50%;font-family:Cinzel,Georgia,serif!important;font-weight:900!important;color:#2c1b0c;background:rgba(244,224,167,.94);border:2px solid #735025;box-shadow:0 2px 6px rgba(0,0,0,.35)}
    .party-deck-ability-score{left:7px;bottom:7px;width:34px;height:34px;font-size:15px!important}.party-deck-ability-mod{right:7px;bottom:7px;width:39px;height:39px;font-size:14px!important}
    .party-deck-detail{text-align:center}.party-deck-detail img{width:min(360px,76vw);aspect-ratio:4/5;object-fit:cover;border-radius:13px;box-shadow:0 12px 34px rgba(0,0,0,.42)}
    .party-deck-detail h3{font-family:Cinzel,Georgia,serif}.party-deck-detail p{max-width:640px;margin:12px auto 0;font-size:16px;line-height:1.5}

    #", root_id, " .party-name-row{
      display:flex;
      align-items:center;
      justify-content:space-between;
      gap:5px;
    }

    #", root_id, " .party-turn-order{
      flex:0 0 auto;
      min-width:20px;
      padding:1px 5px;
      border-radius:999px;
      background:rgba(62,47,28,0.10);
      font-size:8px;
      font-weight:900;
      text-align:center;
    }

    #", root_id, " .party-initiative{
      margin-bottom:4px;
      font-size:8px;
      font-weight:800;
      color:#7b4b24;
      text-transform:uppercase;
    }

    #", root_id, " .party-name{
      font-size: 10px;
      font-weight: 900;
      line-height: 1.05;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      margin-bottom: 2px;
    }

    #", root_id, " .party-sub{
      font-size: 7px;
      opacity: 0.80;
      line-height: 1.05;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      margin-bottom: 4px;
      text-transform: uppercase;
    }
    
    #", root_id, " .party-meta{
  font-size: 7px;
  opacity: 0.80;
  line-height: 1.05;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  text-transform: uppercase;
}

#", root_id, " .party-meta + .party-meta{
  margin-top: 1px;
}

#", root_id, " .party-meta-wrap{
  margin-bottom: 4px;
}

    #", root_id, " .party-status{
      display: flex;
      gap: 3px;
      margin-bottom: 4px;
      min-height: 10px;
    }

    #", root_id, " .party-status-icon{
      font-size: 9px;
      line-height: 1;
    }
    #", root_id, " .party-condition{font-size:7px;font-weight:900;padding:2px 5px;border-radius:999px;background:#6f3030;color:#fff;text-transform:uppercase}

    #", root_id, " .party-minirow{
      display: grid;
grid-template-columns: 10px 1fr;
      gap: 5px;
      align-items: center;
    }

    #", root_id, " .party-minirow + .party-minirow{
      margin-top: 3px;
    }

    #", root_id, " .party-mini-label{
      font-size: 8px;
      font-weight: 900;
      text-transform: uppercase;
      opacity: 0.82;
    }

    #", root_id, " .party-bar{
      position: relative;
      height: 7px;
      border-radius: 999px;
      overflow: hidden;
      background: rgba(40,40,40,0.14);
      border: 1px solid rgba(191,167,111,0.32);
    }

    #", root_id, " .party-fill{
      position: absolute;
      top: 0;
      left: 0;
      height: 100%;
      border-radius: 999px;
    }

    #", root_id, " .party-fill.hp{
      background: linear-gradient(90deg, rgba(170,35,35,0.92), rgba(240,120,60,0.90));
      z-index: 1;
    }

    #", root_id, " .party-fill.hp.temp{
      background: linear-gradient(90deg, rgba(220,235,255,0.42), rgba(145,205,255,0.82));
      z-index: 2;
    }

    #", root_id, " .party-fill.magic{
      background: linear-gradient(90deg, rgba(40,80,190,0.92), rgba(100,190,255,0.92));
      z-index: 1;
    }

    #", root_id, " .party-fill.magic.temp{
      background: linear-gradient(90deg, rgba(220,245,255,0.42), rgba(190,250,255,0.84));
      z-index: 2;
    }

  ")
  
  tagList(
    tags$style(HTML(css)),
    tags$script(HTML(sprintf("$(document).on('click keydown','#%s .party-strip.deck-available',function(e){if(e.type==='keydown'&&e.key!=='Enter'&&e.key!==' ')return;if(e.type==='keydown')e.preventDefault();Shiny.setInputValue('%s',$(this).data('characterId'),{priority:'event'});});$(document).on('click','.party-deck-card',function(){Shiny.setInputValue('%s',$(this).data('cardIndex'),{priority:'event'});});",root_id,ns("open_character_deck"),ns("open_deck_card")))),
    div(
      id = root_id,
      div(
        class = "partyhud-shell",
        uiOutput(ns("party_cards"))
      )
    )
  )
}

partyHudServer <- function(id, state, restoring = NULL, add_log = NULL,
                           char_rev = NULL, live_snapshot = NULL,
                           portrait_base = "assets/player-posters") {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)

    party_portrait_file <- function(name) {
      raw_name <- as.character(name %||% "")
      ascii_name <- iconv(raw_name, to = "ASCII//TRANSLIT")
      if (is.na(ascii_name)) ascii_name <- raw_name
      key <- tolower(trimws(ascii_name))
      key <- gsub("[^a-z0-9]+", "_", key)
      key <- gsub("^_+|_+$", "", key)
      portraits <- c(
        eman = "eman.png",
        dewydd_troell = "dewydd-troell.png",
        dewydd = "dewydd-troell.png",
        dewyd_troell = "dewydd-troell.png",
        dafydd_troell = "dewydd-troell.png",
        eleri = "eleri.png"
      )
      result <- unname(portraits[key])
      if (length(result) && !is.na(result)) result else ""
    }
    
    log_safe <- function(msg, toast = FALSE, flash = "none") {
      if (is.function(add_log)) {
        try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
      } else {
        message(msg)
      }
    }
    
    hud_rows <- reactiveVal(data.frame())
    hud_sig  <- reactiveVal("init")
    hud_combat <- reactiveVal(data.frame())
    hud_combat_sig <- reactiveVal("init")
    active_deck <- reactiveVal(NULL)
    
    resolved_session_id <- reactive({
      sid <- suppressWarnings(as.integer(state$active_session_id %||% NA))
      if (!is.na(sid) && sid > 0) return(sid)
      
      eid <- suppressWarnings(as.integer(state$active_encounter_id %||% NA))
      if (!is.na(eid) && eid > 0) {
        enc <- tryCatch(get_encounter(eid), error = function(e) data.frame())
        if (is.data.frame(enc) && nrow(enc) > 0) {
          sid2 <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
          if (!is.na(sid2) && sid2 > 0) return(sid2)
        }
      }
      
      NA_integer_
    })
    
    has_live_session <- reactive({
      cid <- as.character(state$char_id %||% "")
      sid <- resolved_session_id()
      
      !isTRUE(state$offline_mode) &&
        !is.na(sid) &&
        nzchar(cid)
    })
 
    
    mini_bar_ui <- function(cur, maxv, temp = 0, cls = c("hp", "magic")) {
      cls <- match.arg(cls)
      
      cur  <- suppressWarnings(as.integer(cur %||% 0))
      maxv <- suppressWarnings(as.integer(maxv %||% 0))
      temp <- suppressWarnings(as.integer(temp %||% 0))
      
      if (is.na(cur)) cur <- 0L
      if (is.na(maxv)) maxv <- 0L
      if (is.na(temp)) temp <- 0L
      
      cur  <- max(0L, cur)
      maxv <- max(0L, maxv)
      temp <- max(0L, temp)
      
      if (maxv <= 0L) {
        return(tags$div(class = "party-bar"))
      }
      
      base_pct <- max(0, min(100, round((cur / maxv) * 100)))
      temp_pct <- max(0, min(100, round(((cur + temp) / maxv) * 100)))
      
      tags$div(
        class = "party-bar",
        tags$div(
          class = paste("party-fill", cls),
          style = paste0("width:", base_pct, "%;")
        ),
        if (temp > 0) {
          tags$div(
            class = paste("party-fill", cls, "temp"),
            style = paste0("width:", temp_pct, "%;")
          )
        }
      )
    }
    
    fetch_session_players_safe <- function(session_id) {
      if (isTRUE(state$offline_mode)) return(data.frame())
      
      tryCatch(
        get_session_players(session_id),
        error = function(e) {
          log_safe(paste("Party HUD read failed:", e$message), toast = FALSE)
          data.frame()
        }
      )
    }
    
    fetch_character_summary_safe <- function(character_id) {
      ch <- tryCatch(
        load_character_from_db(character_id),
        error = function(e) NULL
      )
      
      if (is.null(ch)) return(NULL)
      
      ch <- tryCatch(validate_character(ch), error = function(e) NULL)
      if (is.null(ch)) return(NULL)
      
      effects <- ch$status$effects %||% character(0)
      if (isTRUE(ch$status$bloodlust)) {
        effects <- c(effects, "Bloodlust")
      }
      
      status_icons <- character(0)
      
      if (any(grepl("^Exhaustion", effects))) status_icons <- c(status_icons, "😵")
      if ("Bloodlust" %in% effects)          status_icons <- c(status_icons, "🩸")
      if ("Poisoned" %in% effects)           status_icons <- c(status_icons, "☠️")
      if ("Blinded" %in% effects)            status_icons <- c(status_icons, "🙈")
      if ("Restrained" %in% effects)         status_icons <- c(status_icons, "🪢")
      if ("Stunned" %in% effects)            status_icons <- c(status_icons, "💫")
      if ("Unconscious" %in% effects)        status_icons <- c(status_icons, "💤")
      
      race_txt  <- as.character(ch$meta$race %||% "")
      class_txt <- as.character(ch$build$class %||% "")
      lvl_txt   <- as.character(ch$build$level %||% "?")
      
      sub_txt <- paste(
        paste0("Lv ", lvl_txt),
        if (nzchar(race_txt)) race_txt else NULL,
        if (nzchar(class_txt)) class_txt else NULL
      )
      
      list(
        race = if (nzchar(race_txt)) race_txt else "—",
        class = if (nzchar(class_txt)) class_txt else "—",
        level = lvl_txt,
        subline = trimws(sub_txt),
        hp_max = as.integer(ch$resources$hp$max %||% 1),
        sindre_cur = as.integer(ch$resources$sindre$cur %||% 0),
        sindre_max = as.integer(ch$resources$sindre$total %||% 0),
        sindre_temp = as.integer(ch$resources$sindre$temp %||% 0),
        status_icons = unique(status_icons)
      )
    }

    ability_names<-c(str="Strength",dex="Dexterity",con="Constitution",int="Intelligence",bld_str="Blood Strength",cha="Charisma")
    ability_dirs<-c(str="strength",dex="dexterity",con="constitution",int="intelligence",bld_str="blood-strength",cha="charisma")
    ability_help<-c(str="Raw physical power: lifting, forcing, climbing and striking.",dex="Agility, balance, precision and controlled movement.",con="Health, stamina and resistance to physical hardship.",int="Reasoning, memory, investigation and learned knowledge.",bld_str="Instinct, perception and connection to blood or primal power.",cha="Presence, confidence, influence and force of personality.")
    skill_core<-c(athletics="brute",clutch="grasp",wrestling="grappler",throwing="hurler",dead_lift="titan",acrobatics="acrobat",sleight_of_hand="quickhand",stealth="shadow",precision="marksman",endurance="bulwark",tolerance="ironblood",fortitude="stalwart",arcana="arcanist",history="chronicler",investigation="investigator",nature="naturalist",religion="theologian",analysis="strategist",perception="watcher",survival="stalker",insight="reader",medicine="healer",animal_handling="beastfriend",mandred_connection="mandred-touched",deception="trickster",intimidation="menace",persuasion="orator",performance="virtuoso",presence="luminary")
    skill_aspect<-c(athletics="enforcer",clutch="binder",wrestling="wrestler",throwing="artillerist",dead_lift="bearer",acrobatics="daredevil",sleight_of_hand="pilferer",stealth="ghost",precision="deadeye",endurance="survivor",tolerance="resistant",fortitude="guardian",arcana="seer",history="lorekeeper",investigation="inquisitor",nature="warden",religion="devotee",analysis="tactician",perception="observer",survival="hunter",insight="empath",medicine="physician",animal_handling="handler",mandred_connection="conduit",deception="liar",intimidation="dread",persuasion="diplomat",performance="muse",presence="leader")
    skill_descriptor<-c(athletics="mighty",clutch="tenacious",wrestling="relentless",throwing="keen",dead_lift="mighty",acrobatics="nimble",sleight_of_hand="cunning",stealth="elusive",precision="keen",endurance="hardy",tolerance="hardened",fortitude="resolute",arcana="mystic",history="learned",investigation="shrewd",nature="wildwise",religion="devout",analysis="calculating",perception="watchful",survival="seasoned",insight="intuitive",medicine="practised",animal_handling="beastwise",mandred_connection="touched",deception="cunning",intimidation="fearsome",persuasion="silver-tongued",performance="mesmeric",presence="commanding")
    skill_card_labels<-c(reliable="Reliable",wild_card="Wild Card",inspired="Inspired")
    skill_card_help<-c(reliable="On a natural roll of 1–5, add the proficiency bonus again.",wild_card="Roll a d6: on 1 subtract the proficiency bonus; on 6 add it.",inspired="On a natural roll of 16–20, add the proficiency bonus again.")
    card_key<-function(x)gsub(" ","_",tolower(as.character(x)))
    ability_card_image<-function(ch,ab){score<-suppressWarnings(as.integer(ch$abilities[[ab]]%||%10L));if(is.na(score))score<-10L;mod<-mod_calc(score);paste0("assets/ability-cards/",ability_dirs[[ab]],"/",if(mod<0)paste0("minus-",abs(mod))else paste0("plus-",mod),".jpg")}
    skill_card_image<-function(skill_key,variant){art<-c(reliable="core",wild_card="aspect",inspired="descriptor")[[variant]]%||%"core";lookup<-switch(art,core=skill_core,aspect=skill_aspect,descriptor=skill_descriptor);folder<-switch(art,core="skill-cores",aspect="skill-aspects",descriptor="skill-descriptors");paste0("assets/skill-cards/",folder,"/",unname(lookup[[skill_key]]%||%skill_core[[skill_key]]),".jpg")}
    deck_card<-function(group,key,label,image,reason,score=NULL,modifier=NULL)list(group=group,key=key,label=label,image=image,reason=reason,score=score,modifier=modifier)

    build_character_deck<-function(character_id){
      ch<-if(identical(as.character(character_id),as.character(state$char_id%||%"")))validate_character(state$char)else tryCatch(validate_character(load_character_from_db(character_id)),error=function(e)NULL)
      if(is.null(ch))return(NULL)
      cards<-list()
      for(ab in names(ability_names)){score<-as.integer(ch$abilities[[ab]]%||%10L);mod<-mod_calc(score);cards[[length(cards)+1L]]<-deck_card("Abilities",paste0("ability_",ab),ability_names[[ab]],ability_card_image(ch,ab),paste0(ability_help[[ab]]," Score ",score,"; modifier ",if(mod>=0)"+"else"",mod,"."),score=score,modifier=mod)}
      for(i in seq_len(nrow(SKILLS_LIST))){skill<-as.character(SKILLS_LIST$Skill[[i]]);key<-card_key(skill);rank<-as.character(ch$prof$skills[[key]]%||%"None");variants<-character_skill_cards(ch,key);if(!length(variants))next;for(variant in variants)cards[[length(cards)+1L]]<-deck_card("Skill Cards",paste(key,variant,sep="_"),paste0(skill," — ",skill_card_labels[[variant]]),skill_card_image(key,variant),paste0(SKILL_DESC[[skill]]%||%skill," Training: ",rank,". ",skill_card_help[[variant]]))}
      sid<-resolved_session_id();env<-list(climate=ch$environment$temperature%||%"Temperate",weather="");fire<-isTRUE(ch$status$has_fire);actions<-character();phase<-NULL
      if(!is.na(sid)){fresh_env<-tryCatch(get_session_environment(sid),error=function(e)NULL);if(!is.null(fresh_env))env<-fresh_env;fire<-isTRUE(tryCatch(get_session_fire(sid),error=function(e)fire));phase<-tryCatch(get_open_session_phase(sid),error=function(e)NULL);if(!is.null(phase)&&identical(as.character(phase$phase_kind[[1L]]),"rest")){rows<-tryCatch(get_session_phase_actions(phase$id[[1L]],character_id),error=function(e)data.frame());if(nrow(rows))actions<-as.character(rows$action_type)}}
      rest_cards<-character_rest_status_cards(ch,fire,actions,env,if(is.null(phase))0 else as.numeric(phase$duration_hours[[1L]]));for(card in rest_cards)cards[[length(cards)+1L]]<-deck_card("Rest & Survival",card$key,card$label,card$image,card$reason)
      extra_conditions<-if(is.function(live_snapshot))encounter_condition_values(live_snapshot(),character_id)else character();condition_cards<-character_condition_status_cards(ch,extra_conditions);for(card in condition_cards)cards[[length(cards)+1L]]<-deck_card("Conditions",card$key,card$label,card$image,card$reason)
      list(character=ch,cards=cards)
    }

    card_art<-function(card){div(class="party-deck-art",tags$img(src=card$image,alt=card$label),if(!is.null(card$score))tags$span(class="party-deck-ability-score",card$score),if(!is.null(card$modifier))tags$span(class="party-deck-ability-mod",paste0(if(card$modifier>=0)"+"else"",card$modifier)))}
    show_character_deck<-function(){deck<-active_deck();if(is.null(deck))return();cards<-deck$cards;groups<-unique(vapply(cards,function(card)card$group,character(1)));showModal(modalDialog(class="party-deck-modal",title=paste0(deck$character$meta$name%||%"Character"," — Card Deck"),div(class="party-deck-intro","Select any card to enlarge it and read its complete effect."),lapply(groups,function(group){indices<-which(vapply(cards,function(card)identical(card$group,group),logical(1)));div(class="party-deck-section",h4(group),div(class="party-deck-grid",lapply(indices,function(index){card<-cards[[index]];tags$button(type="button",class="party-deck-card",`data-card-index`=index,title=paste("Open",card$label),card_art(card),tags$span(card$label))})))}),footer=modalButton("Close"),easyClose=TRUE,size="l"))}
    observeEvent(input$open_character_deck,{cid<-as.character(input$open_character_deck%||%"");if(!nzchar(cid))return();deck<-build_character_deck(cid);if(is.null(deck))return(showNotification("That character's card deck could not be loaded.",type="warning"));active_deck(deck);show_character_deck()},ignoreInit=TRUE)
    observeEvent(input$open_deck_card,{deck<-active_deck();index<-suppressWarnings(as.integer(input$open_deck_card));if(is.null(deck)||is.na(index)||index<1L||index>length(deck$cards))return();card<-deck$cards[[index]];showModal(modalDialog(class="party-deck-modal",title=card$label,div(class="party-deck-detail",card_art(card),h3(card$label),p(card$reason)),footer=tagList(actionButton(session$ns("back_to_deck"),"Back to Deck"),modalButton("Close")),easyClose=TRUE,size="l"))},ignoreInit=TRUE)
    observeEvent(input$back_to_deck,show_character_deck(),ignoreInit=TRUE)
    
    build_party_rows <- function() {
      if (isTRUE(state$offline_mode)) return(data.frame())
      if (!isTRUE(has_live_session())) return(data.frame())
      
      sid <- resolved_session_id()
      my_cid <- as.character(state$char_id %||% "")
      
      if (is.na(sid)) return(data.frame())
      
      if (is.function(live_snapshot)) {
        snapshot <- live_snapshot()
        rows <- snapshot$players %||% data.frame()
      } else {
        snapshot <- empty_player_live_snapshot()
        rows <- fetch_session_players_safe(sid)
      }
      if (!is.data.frame(rows) || nrow(rows) == 0) return(data.frame())

      combat <- snapshot$combat %||% data.frame()
      in_combat <- is.data.frame(combat) && nrow(combat) > 0 &&
        identical(as.character(combat$phase[1] %||% ""), "combat")

      rows$character_id <- as.character(rows$character_id %||% "")
      rows$actor_id <- rows$character_id
      rows$actor_type <- "player"
      if (!"max_hp" %in% names(rows)) rows$max_hp <- NA_integer_
      
      if ("is_active" %in% names(rows)) {
        rows <- rows[rows$is_active %in% TRUE, , drop = FALSE]
      }

      if (in_combat) {
        enemies <- snapshot$enemies %||% data.frame()
        if (is.data.frame(enemies) && nrow(enemies) > 0) {
          enemy_rows <- data.frame(
            actor_id = as.character(enemies$enemy_uuid %||% ""),
            actor_type = "enemy",
            character_id = NA_character_,
            display_name = as.character(enemies$name %||% "Enemy"),
            current_hp = suppressWarnings(as.integer(enemies$hp_current %||% 0L)),
            temp_hp = suppressWarnings(as.integer(enemies$temp_hp %||% 0L)),
            max_hp = suppressWarnings(as.integer(enemies$hp_max %||% NA_integer_)),
            initiative = suppressWarnings(as.integer(enemies$initiative %||% NA_integer_)),
            turn_order = suppressWarnings(as.integer(enemies$turn_order %||% NA_integer_)),
            is_active = as.logical(enemies$is_active %||% TRUE),
            stringsAsFactors = FALSE
          )
          rows <- dplyr::bind_rows(rows, enemy_rows)
        }
        summons <- snapshot$summons %||% data.frame()
        if (is.data.frame(summons) && nrow(summons) > 0) {
          summon_rows <- data.frame(
            actor_id = as.character(summons$summon_uuid %||% ""),
            actor_type = "summon", character_id = NA_character_,
            display_name = paste0(as.character(summons$name %||% "Summoned Beast"), " 🐾"),
            current_hp = suppressWarnings(as.integer(summons$hp_current %||% 0L)),
            temp_hp = suppressWarnings(as.integer(summons$temp_hp %||% 0L)),
            max_hp = suppressWarnings(as.integer(summons$hp_max %||% NA_integer_)),
            initiative = suppressWarnings(as.integer(summons$initiative %||% NA_integer_)),
            turn_order = suppressWarnings(as.integer(summons$turn_order %||% NA_integer_)),
            is_active = as.logical(summons$is_active %||% TRUE),
            stringsAsFactors = FALSE
          )
          rows <- dplyr::bind_rows(rows, summon_rows)
        }
      }

      if (in_combat && "turn_order" %in% names(rows)) {
        rows <- rows[order(rows$turn_order, na.last = TRUE), , drop = FALSE]
      }
      effects <- snapshot$effects %||% data.frame()
      rows$conditions <- vapply(as.character(rows$actor_id), function(actor_id) {
        if (!is.data.frame(effects) || !nrow(effects)) return("")
        hit <- effects[as.character(effects$target_actor_id %||% "") == actor_id &
                         as.character(effects$effect_type %||% "") == "condition", , drop = FALSE]
        if (!nrow(hit)) return("")
        values <- vapply(seq_len(nrow(hit)), function(j) {
          payload <- hit$payload[[j]]
          if (!is.list(payload)) payload <- tryCatch(jsonlite::fromJSON(as.character(payload)), error = function(e) list())
          as.character(payload$condition %||% "")
        }, character(1))
        paste(unique(values[nzchar(values)]), collapse = ",")
      }, character(1))
      
      rows
    }
    
    
    observe({
      cat("\n[PARTY HUD]\n")
      cat("char_id:", as.character(state$char_id %||% "NULL"), "\n")
      cat("active_session_id:", as.character(state$active_session_id %||% "NULL"), "\n")
      cat("active_encounter_id:", as.character(state$active_encounter_id %||% "NULL"), "\n")
      cat("resolved_session_id:", as.character(resolved_session_id() %||% NA), "\n")
      cat("offline_mode:", as.character(state$offline_mode %||% FALSE), "\n")
    })
    
    build_party_signature <- function(rows) {
      if (!is.data.frame(rows) || nrow(rows) == 0) return("empty")
      
      cols <- intersect(
        c("actor_id", "actor_type", "display_name", "current_hp", "temp_hp",
          "max_hp", "initiative", "turn_order", "is_active", "conditions"),
        names(rows)
      )
      
      if (length(cols) == 0) return("no-cols")
      
      paste(
        apply(rows[, cols, drop = FALSE], 1, function(x) paste(x, collapse = "||")),
        collapse = "///"
      )
    }
    
    observe({
      rows <- build_party_rows()
      sig <- build_party_signature(rows)
      
      if (!identical(sig, hud_sig())) {
        hud_sig(sig)
        hud_rows(rows)
      }

      snapshot <- if (is.function(live_snapshot)) live_snapshot() else empty_player_live_snapshot()
      combat <- snapshot$combat %||% data.frame()
      combat_cols <- intersect(
        c("phase", "round_number", "active_actor_id", "active_actor_type", "current_turn_order"),
        names(combat)
      )
      combat_sig <- if (!is.data.frame(combat) || !nrow(combat)) {
        "empty"
      } else if (!length(combat_cols)) {
        "no-cols"
      } else {
        paste(as.character(unlist(combat[1, combat_cols, drop = FALSE])), collapse = "||")
      }
      if (!identical(combat_sig, hud_combat_sig())) {
        hud_combat_sig(combat_sig)
        hud_combat(combat)
      }
    })
    
    output$party_cards <- renderUI({
      if (isTRUE(state$offline_mode)) {
        return(NULL)
      }
      
      if (!has_live_session()) {
        return(NULL)
      }
      
      rows <- hud_rows()

      combat <- hud_combat()
      in_combat <- is.data.frame(combat) && nrow(combat) > 0 &&
        identical(as.character(combat$phase[1] %||% ""), "combat")
      active_actor_id <- if (in_combat) {
        as.character(combat$active_actor_id[1] %||% "")
      } else {
        ""
      }
      round_number <- if (in_combat) {
        as.character(combat$round_number[1] %||% "—")
      } else {
        ""
      }
      
      if (!is.data.frame(rows) || nrow(rows) == 0) {
        return(
          tagList(
            tags$div(class = "partyhud-label", "Party"),
            tags$div(class = "partyhud-empty", "No active players.")
          )
        )
      }
      
      strips <- lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, , drop = FALSE]
        
        nm  <- as.character(row$display_name[1] %||% "Unknown")
        actor_id <- as.character(row$actor_id[1] %||% row$character_id[1] %||% "")
        actor_type <- as.character(row$actor_type[1] %||% "player")
        is_active_turn <- in_combat && nzchar(active_actor_id) && identical(actor_id, active_actor_id)
        is_self <- identical(actor_type, "player") && identical(actor_id, as.character(state$char_id %||% ""))
        initiative <- suppressWarnings(as.integer(row$initiative[1] %||% NA))
        turn_order <- suppressWarnings(as.integer(row$turn_order[1] %||% NA))
        cur_hp  <- as.integer(row$current_hp[1] %||% 0)
        temp_hp <- as.integer(row$temp_hp[1] %||% 0)
        
        if (is.na(cur_hp)) cur_hp <- 0L
        if (is.na(temp_hp)) temp_hp <- 0L
        
        extra <- NULL
        if (identical(actor_type, "player") && "character_id" %in% names(row) &&
            nzchar(as.character(row$character_id[1] %||% ""))) {
          extra <- fetch_character_summary_safe(as.character(row$character_id[1]))
        }
        
        hp_max <- suppressWarnings(as.integer(row$max_hp[1] %||% NA))
        if (is.na(hp_max) || hp_max <= 0L) {
          hp_max <- suppressWarnings(as.integer(extra$hp_max %||% NA))
        }
        if (is.na(hp_max) || hp_max <= 0L) hp_max <- max(cur_hp, 1L)
        
        sindre_cur  <- suppressWarnings(as.integer(extra$sindre_cur %||% 0))
        sindre_max  <- suppressWarnings(as.integer(extra$sindre_max %||% 0))
        sindre_temp <- suppressWarnings(as.integer(extra$sindre_temp %||% 0))
        
        if (is.na(sindre_cur))  sindre_cur <- 0L
        if (is.na(sindre_max))  sindre_max <- 0L
        if (is.na(sindre_temp)) sindre_temp <- 0L
        
        race_txt <- if (identical(actor_type, "enemy")) "Enemy" else as.character(extra$race %||% "")
        class_txt <- if (identical(actor_type, "enemy")) "Combatant" else as.character(extra$class %||% "")
        
        if (!nzchar(race_txt))  race_txt  <- "—"
        if (!nzchar(class_txt)) class_txt <- "—"
        
        status_icons <- extra$status_icons %||% character(0)
        conditions <- strsplit(as.character(row$conditions[1] %||% ""), ",", fixed = TRUE)[[1L]]
        conditions <- conditions[nzchar(conditions)]
        portrait_file <- if (identical(actor_type, "player")) party_portrait_file(nm) else ""
        portrait_initial <- toupper(substr(trimws(nm), 1L, 1L))
        if (!nzchar(portrait_initial)) portrait_initial <- "?"
        
        tags$div(
          class = "party-row",
          tags$div(
            class = paste("party-strip", if (is_active_turn) "active-turn" else "", if (is_self) "self-player" else "",if(identical(actor_type,"player"))"deck-available"else""),
            `data-character-id`=if(identical(actor_type,"player"))actor_id else NULL,
            role=if(identical(actor_type,"player"))"button"else NULL,
            tabindex=if(identical(actor_type,"player"))"0"else NULL,
            title=if(identical(actor_type,"player"))paste("View",nm,"card deck")else NULL,
            tags$div(
              class = paste("party-portrait", if (identical(actor_type, "enemy")) "enemy" else ""),
              if (nzchar(portrait_file)) {
                tags$img(src = paste0(sub("/$", "", portrait_base), "/", portrait_file), alt = paste(nm, "portrait"))
              } else {
                tags$span(class = "party-portrait-initial", portrait_initial)
              }
            ),
            tags$div(
              class = "party-name-row",
              tags$div(class = "party-name", paste0(nm, if (is_self) " (You)" else "")),
              if (in_combat) tags$span(
                class = "party-turn-order",
                if (!is.na(turn_order)) paste0("#", turn_order) else "#—"
              )
            ),
            if (in_combat) tags$div(
              class = "party-initiative",
              paste0(
                if (is_active_turn) "▶ Current turn • " else "",
                "Initiative ", if (!is.na(initiative)) initiative else "—"
              )
            ),
            tags$div(
              class = "party-meta-wrap",
              tags$div(class = "party-meta", race_txt),
              tags$div(class = "party-meta", class_txt)
            ),
            tags$div(
              class = "party-status",
              if (length(status_icons) > 0) {
                lapply(status_icons, function(ic) {
                  tags$span(class = "party-status-icon", ic)
                })
              },
              if (length(conditions)) lapply(conditions, function(condition) {
                tags$span(class = "party-condition", title = paste("Condition:", condition), condition)
              })
            ),
            tags$div(
              class = "party-minirow",
              tags$span(class = "party-mini-label", "HP"),
              mini_bar_ui(cur_hp, hp_max, temp_hp, "hp")
            ),
            if (identical(actor_type, "player")) tags$div(
              class = "party-minirow",
              tags$span(class = "party-mini-label", "SI"),
              mini_bar_ui(sindre_cur, sindre_max, sindre_temp, "magic")
            )
          )
        )
      })
      
      tagList(
        tags$div(
          class = "partyhud-label",
          if (in_combat) paste("Combat • Round", round_number) else "Party"
        ),
        strips
      )
    })
  })
}
