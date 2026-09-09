restTabUI <- function(id) {
  ns <- NS(id)
  screen_id <- ns("camp_screen")
  
  tabPanel(
    title = "Rest",
    value = "rest",
    
    tags$audio(
      id = ns("rest_fire_loop"),
      src = "fire_crackle.wav",
      loop = NA,
      preload = "auto",
      style = "display:none;"
    ),
    
    tags$audio(
      id = ns("rest_flint_sfx"),
      src = "flint.ogg",
      preload = "auto",
      style = "display:none;"
    ),
    
    tags$audio(
      id = ns("rest_eat_sfx"),
      src = "eat.wav",
      preload = "auto",
      style = "display:none;"
    ),
    
    tags$audio(
      id = ns("rest_forage_sfx"),
      src = "forage.wav",
      preload = "auto",
      style = "display:none;"
    ),
    
    tags$audio(
      id = ns("rest_drink_sfx"),
      src = "drink.wav",
      preload = "auto",
      style = "display:none;"
    ),
    
    tags$audio(
      id = ns("rest_refill_sfx"),
      src = "refill.wav",
      preload = "auto",
      style = "display:none;"
    ),
    
    tags$audio(
      id = ns("rest_gather_sfx"),
      src = "gather.wav",
      preload = "auto",
      style = "display:none;"
    ),
    
    tags$script(HTML(sprintf("
(function() {
  const ns = '%s';

  let fireLit = false;
  let audioUnlocked = false;
  let fadeTimer = null;

  function getEl(id) {
    return document.getElementById(ns + id);
  }

  function activePaneValue() {
    const pane = document.querySelector('.tab-content > .tab-pane.active');
    if (!pane) return null;
    return pane.getAttribute('data-value');
  }

  function isRestTabActive() {
    return activePaneValue() === 'rest';
  }

  function playOneShot(audioEl, volume) {
    if (!audioEl || !audioUnlocked) return;
    audioEl.pause();
    audioEl.currentTime = 0;
    audioEl.volume = volume;
    audioEl.play().catch(function(err) {
      console.log('[rest sfx blocked]', err);
    });
  }

  function stopFade() {
    if (fadeTimer) {
      clearInterval(fadeTimer);
      fadeTimer = null;
    }
  }

  function fadeTo(audioEl, targetVolume, duration, pauseWhenDone) {
    if (!audioEl) return;

    stopFade();

    const startVolume = isNaN(audioEl.volume) ? 0 : audioEl.volume;
    const delta = targetVolume - startVolume;

    if (Math.abs(delta) < 0.01) {
      audioEl.volume = targetVolume;
      if (pauseWhenDone && targetVolume === 0) {
        audioEl.pause();
      }
      return;
    }

    const intervalMs = 40;
    const steps = Math.max(1, Math.round(duration / intervalMs));
    let step = 0;

    fadeTimer = setInterval(function() {
      step += 1;
      const progress = step / steps;
      audioEl.volume = Math.max(0, Math.min(1, startVolume + delta * progress));

      if (step >= steps) {
        clearInterval(fadeTimer);
        fadeTimer = null;
        audioEl.volume = targetVolume;
        if (pauseWhenDone && targetVolume === 0) {
          audioEl.pause();
        }
      }
    }, intervalMs);
  }

  function syncRestFireLoop() {
    const fireLoop = getEl('rest_fire_loop');
    if (!fireLoop) {
      console.log('[REST AUDIO] fire loop element not found');
      return;
    }

    const shouldPlay = fireLit && isRestTabActive() && audioUnlocked;

    if (shouldPlay) {
      fireLoop.play().catch(function(err) {
        console.log('[rest fire loop blocked]', err);
      });
      fadeTo(fireLoop, 0.22, 900, false);
    } else {
      fadeTo(fireLoop, 0.00, 700, true);
    }
  }

  function unlockAudio() {
    audioUnlocked = true;
    console.log('[REST AUDIO] unlocked');
    syncRestFireLoop();
  }

  document.addEventListener('click', unlockAudio, { once: true });
  document.addEventListener('keydown', unlockAudio, { once: true });

  Shiny.addCustomMessageHandler(ns + 'play_rest_sfx', function(msg) {
    const name = msg && msg.name ? msg.name : '';
    console.log('[REST AUDIO] play_rest_sfx', name);

    if (name === 'flint')  playOneShot(getEl('rest_flint_sfx'), 0.28);
    if (name === 'eat')    playOneShot(getEl('rest_eat_sfx'), 0.20);
    if (name === 'forage') playOneShot(getEl('rest_forage_sfx'), 0.18);
    if (name === 'drink')  playOneShot(getEl('rest_drink_sfx'), 0.22);
    if (name === 'refill') playOneShot(getEl('rest_refill_sfx'), 0.20);
    if (name === 'gather') playOneShot(getEl('rest_gather_sfx'), 0.18);
  });

  Shiny.addCustomMessageHandler(ns + 'sync_rest_fire', function(msg) {
    fireLit = !!(msg && msg.lit);
    console.log('[REST AUDIO] sync_rest_fire', fireLit);
    syncRestFireLoop();
  });

  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function() {
    syncRestFireLoop();
  });

  setInterval(syncRestFireLoop, 1000);

  setTimeout(function() {
    console.log(
      '[REST AUDIO] init',
      getEl('rest_fire_loop'),
      getEl('rest_flint_sfx'),
      getEl('rest_eat_sfx'),
      getEl('rest_forage_sfx'),
      getEl('rest_drink_sfx'),
      getEl('rest_refill_sfx'),
      getEl('rest_gather_sfx')
    );
  }, 500);
})();
", ns("")))),
    
    tags$style(HTML(do.call(sprintf, c(list("
#%s {
  min-height: 100vh;
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  background: rgba(255,248,230,0.88);
  color: #2b2b2b;
  font-family: 'Cinzel', serif;
  padding: 12px;
  border-radius: 18px;
  border: 1px solid rgba(191,167,111,0.45);
  box-shadow: inset 0 0 0 1px rgba(255,255,255,0.35);
}

#%s .camp-topbar {
  display: flex;
  justify-content: space-between;
  background: rgba(255,248,230,0.9);
  color: #2b2b2b;
  padding: 10px 16px;
  border-radius: 14px;
  border: 1px solid rgba(191,167,111,0.5);
  box-shadow: 0 4px 12px rgba(0,0,0,0.15);
}

#%s .camp-center {
  position: relative;
  height: 260px;
}

#%s .camp-fire {
  position: absolute;
  top: 50%%;
  left: 50%%;
  transform: translate(-50%%, -50%%);
  width: 130px;
  filter: drop-shadow(0 0 15px orange);
  animation: fireGlow 1.8s infinite alternate;
}

@keyframes fireGlow {
  from { filter: drop-shadow(0 0 10px orange); }
  to   { filter: drop-shadow(0 0 30px red); }
}

#%s .camp-btn {
  position: absolute;
  transform: translate(-50%%, -50%%);
  padding: 10px 16px;
  border-radius: 12px;
  background: rgba(30,25,20,0.9);
  border: 1px solid #bfa76f;
  color: #f5e6c8;
  font-weight: bold;
  transition: all 0.2s ease;
}

#%s .camp-btn:hover {
  transform: translate(-50%%, -50%%) scale(1.05);
  background: rgba(60,45,30,0.95);
  box-shadow: 0 0 10px rgba(255,200,120,0.5);
}

#%s .btn-top    { top: 5%%;  left: 50%%; }
#%s .btn-left   { top: 50%%; left: 12%%; }
#%s .btn-right  { top: 50%%; left: 88%%; }
#%s .btn-bottom { top: 92%%; left: 50%%; }

#%s .camp-status {
  display: flex;
  justify-content: space-between;
  background: rgba(255,248,230,0.9);
  color: #2b2b2b;
  padding: 10px 16px;
  border-radius: 14px;
  border: 1px solid rgba(191,167,111,0.5);
  box-shadow: 0 4px 12px rgba(0,0,0,0.15);
}

#%s .resource-card {
  background: rgba(255,248,230,0.95);
  color: #2b2b2b;
  border-radius: 16px;
  padding: 16px;
  width: 45%%;
  border: 1px solid rgba(191,167,111,0.6);
  box-shadow: 0 6px 18px rgba(0,0,0,0.15);
}

#%s .resource-card h4 {
  font-weight: 900;
  letter-spacing: 1px;
}

#%s .resource-row {
  display: flex;
  gap: 8px;
  margin-top: 6px;
}

#%s .resource-card .btn {
  border-radius: 10px;
}

#%s .phase-clock{width:118px;height:118px;border-radius:50%%;display:grid;place-items:center;margin:auto;background:conic-gradient(#8e673b var(--spent),rgba(62,47,28,.14) 0);box-shadow:inset 0 0 0 10px rgba(255,248,230,.94)}
#%s .phase-clock-inner{text-align:center;font-weight:900;font-size:22px}.phase-clock-inner small{display:block;font-size:11px;font-weight:600}
#%s .rest-mode-panel{padding:14px;margin:10px 0;border:1px solid rgba(142,103,59,.45);border-radius:14px;background:rgba(255,250,235,.94)}
#%s .rest-status-wrap{margin:12px 0 18px;padding:14px;border:1px solid rgba(142,103,59,.45);border-radius:14px;background:rgba(255,250,235,.94)}
#%s .rest-status-head{display:flex;justify-content:space-between;gap:12px;align-items:baseline;margin-bottom:10px} #%s .rest-status-head h4{margin:0;font-family:Cinzel,serif}
#%s .rest-status-hand{display:flex;gap:12px;align-items:flex-start;overflow-x:auto;padding:5px 3px 12px;scrollbar-color:#9b7747 transparent}
#%s .rest-status-card{flex:0 0 142px;text-align:center;transition:transform .18s ease} #%s .rest-status-card:hover{transform:translateY(-4px)}
#%s .rest-status-card img{display:block;width:142px;aspect-ratio:4/5;object-fit:cover;border-radius:10px;box-shadow:0 7px 16px rgba(44,27,12,.32)}
#%s .rest-status-reason{font-size:10px;line-height:1.25;margin-top:6px;color:#503b24}
#%s .rest-status-card.risk img{box-shadow:0 0 0 2px #8e3e32,0 7px 16px rgba(44,27,12,.32)} #%s .rest-status-card.positive img{box-shadow:0 0 0 2px #587b4f,0 7px 16px rgba(44,27,12,.32)}
"), as.list(rep(screen_id, 28)))))),
    
    div(
      id = screen_id,
      
      div(
        class = "camp-topbar",
        uiOutput(ns("camp_time")),
        uiOutput(ns("camp_environment"))
      ),
      uiOutput(ns("rest_mode_panel")),
      
      h4(),
      h4(),
      h4(),
      h4(),
      
      div(
        class = "camp-center",
        uiOutput(ns("fire_visual")),
        actionButton(ns("light_fire"), "Light Fire", class = "camp-btn btn-bottom")
      ),
      
      h4(),
      
      div(
        style = "display:flex; justify-content:space-between; margin-top:12px; gap:12px;",
        
        div(
          class = "resource-card",
          h4("🍖 Rations"),
          uiOutput(ns("rations_display")),
          div(
            style = "display:flex; justify-content:space-around; margin-top:10px;",
            div(
              style = "text-align:center;",
              actionButton(ns("consume_rations_btn"), "🍖"),
              tags$div("Eat")
            ),
            div(
              style = "text-align:center;",
              actionButton(ns("forage_btn"), "🌿"),
              tags$div("Forage")
            )
          )
        ),
        
        div(
          class = "resource-card",
          h4("💧 Water"),
          uiOutput(ns("water_display")),
          div(
            style = "display:flex; justify-content:space-around; margin-top:10px;",
            div(
              style = "text-align:center;",
              actionButton(ns("consume_water_btn"), "💧"),
              tags$div("Drink")
            ),
            div(
              style = "text-align:center;",
              actionButton(ns("refill_btn"), "🏞️"),
              tags$div("Gather")
            )
          )
        ),
        
        div(
          class = "resource-card",
          h4("🪵 Wood"),
          uiOutput(ns("wood_display")),
          div(
            style = "display:flex; justify-content:space-around; margin-top:10px;",
            div(
              style = "text-align:center;",
              actionButton(ns("gather_wood_btn"), "🌲"),
              tags$div("Gather")
            )
          )
        )
      ),
      uiOutput(ns("active_status_cards"))
    )
  )
}

restTabServer <- function(
    id,
    state,
    restoring = NULL,
    add_log = NULL,
    char_rev = NULL
) {
  moduleServer(id, function(input, output, session) {
    
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    if (is.null(restoring)) restoring <- reactiveVal(FALSE)
    
    log_safe <- function(msg, toast = TRUE, flash = "none") {
      if (is.function(add_log)) {
        try(add_log(msg = msg, toast = toast, flash = flash), silent = TRUE)
      } else message(msg)
    }
    supply_defaults<-function(x){list(wood=x$resources$wood$cur%||%3L,wood_max=x$resources$wood$max%||%10L,water=x$resources$water$cur%||%3L,water_max=x$resources$water$max%||%5L,rations=x$resources$rations$cur%||%3L,rations_max=x$resources$rations$max%||%5L)}
    online_session_id<-function(){sid<-suppressWarnings(as.integer(state$active_session_id%||%NA));if(isTRUE(state$offline_mode)||is.na(sid)||sid<1L)NA_integer_ else sid}
    shared_environment<-reactiveVal(NULL)
    active_phase<-reactiveVal(NULL);watch_prompted_phase<-reactiveVal(NA_integer_);confirmation_rev<-reactiveVal(0L)
    rest_visible<-function()identical(as.character(state$active_tab%||%"camp"),"rest")
    observe({invalidateLater(2500,session);if(!rest_visible())return();sid<-online_session_id();if(is.na(sid))return();env<-get_session_environment(sid);if(is.null(env))return();shared_environment(env);x<-validate_character(state$char);day<-as.integer(env$day_number[[1L]]);if(!identical(as.integer(x$meta$day%||%1L),day)){x$meta$day<-day;state$char<-x}})
    observe({invalidateLater(2000,session);if(!rest_visible())return();sid<-online_session_id();if(is.na(sid)){active_phase(NULL);return()};active_phase(get_open_session_phase(sid))})
    phase_remaining<-reactive({invalidateLater(750,session);p<-active_phase();cid<-as.character(state$char_id%||%"");if(is.null(p)||!nzchar(cid)||p$phase_kind[[1L]]!="rest")return(0);session_phase_time_remaining(p$id[[1L]],cid)})
    spend_phase_time<-function(type,label,hours,start=NULL,end=NULL){p<-active_phase();cid<-as.character(state$char_id%||%"");if(is.null(p)||p$phase_kind[[1L]]!="rest")return(structure(list(),error="The DM has not opened Rest Mode."));allocate_session_phase_time(p$id[[1L]],cid,type,label,hours,start,end)}
    phase_confirmation_ui<-function(p){confirmation_rev();cid<-as.character(state$char_id%||%"");confirmed<-cid%in%as.character(get_session_phase_confirmations(p$id[[1L]])$character_id%||%character());div(style="margin-top:10px",actionButton(session$ns("confirm_phase_ready"),if(confirmed)"✓ Plan confirmed"else"Confirm rest plan",class=if(confirmed)"btn btn-success"else"btn btn-primary"),tags$small(style="margin-left:8px",if(confirmed)"The DM can see that you are ready. Click again to revise your plan."else"Every active player must confirm a Rest Mode plan."))}
    output$rest_mode_panel<-renderUI({
      p<-active_phase()
      if(is.null(p))return(div(class="rest-mode-panel",strong("No phase is open.")," The DM controls when party time advances."))
      if(p$phase_kind[[1L]]!="rest")return(div(class="rest-mode-panel",strong("Standard phase in progress.")," Sindre will regenerate for six hours when the DM resolves it; camp-time activities are unavailable and no player confirmation is required."))
      budget<-as.numeric(p$duration_hours[[1L]]);remaining<-phase_remaining();spent<-budget-remaining
      div(class="rest-mode-panel",fluidRow(
        column(3,div(class="phase-clock",style=paste0("--spent:",round(spent/budget*100),"%"),div(class="phase-clock-inner",paste0(remaining,"h"),tags$small("remaining")))),
        column(9,h4(paste0("Rest Mode — plan your ",budget," hours")),p("Long rest uses 6h; short rest, gathering, water collection and foraging use 1h; watches use 2h; glyphs use exact crafting time."),div(style="display:flex;gap:8px;flex-wrap:wrap",actionButton(session$ns("plan_short_rest"),"Allocate Short Rest (1h)"),actionButton(session$ns("plan_long_rest"),"Allocate Long Rest (6h)",class="btn btn-primary"),actionButton(session$ns("organise_watches"),"Organise Watches")),uiOutput(session$ns("phase_plan")),uiOutput(session$ns("watch_rota")),phase_confirmation_ui(p))
      ))
    })
    observeEvent(input$confirm_phase_ready,{p<-active_phase();req(p);if(p$phase_kind[[1L]]!="rest")return();cid<-as.character(state$char_id%||%"");rows<-get_session_phase_confirmations(p$id[[1L]]);ready<-cid%in%as.character(rows$character_id%||%character());ok<-confirm_session_phase_ready(p$id[[1L]],cid,!ready);if(!isTRUE(ok))return(showNotification("Your confirmation could not be saved.",type="error"));confirmation_rev(confirmation_rev()+1L);showNotification(if(ready)"Rest plan confirmation withdrawn."else"Rest plan confirmed — the DM can now see you are ready.",type="message")},ignoreInit=TRUE)
    output$active_status_cards<-renderUI({
      invalidateLater(1000,session)
      p<-active_phase();if(is.null(p))return(NULL)
      x<-validate_character(state$char);sid<-online_session_id();fire<-if(is.na(sid))isTRUE(x$status$has_fire)else get_session_fire(sid)
      cid<-as.character(state$char_id%||%"");actions<-character()
      if(p$phase_kind[[1L]]=="rest"&&nzchar(cid)){rows<-get_session_phase_actions(p$id[[1L]],cid);if(nrow(rows))actions<-as.character(rows$action_type)}
      env<-shared_environment()%||%list(climate=x$environment$temperature%||%"Temperate",weather="")
      cards<-character_rest_status_cards(x,fire,actions,env,as.numeric(p$duration_hours[[1L]]))
      div(class="rest-status-wrap",
        div(class="rest-status-head",h4("Active Rest Cards"),tags$small("These cards show what is currently protecting or threatening you when the phase resolves.")),
        div(class="rest-status-hand",lapply(cards,function(card)div(class=paste("rest-status-card",card$tone),title=card$reason,tags$img(src=card$image,alt=card$label),div(class="rest-status-reason",card$reason))))
      )
    })
    output$phase_plan<-renderUI({invalidateLater(750,session);p<-active_phase();cid<-as.character(state$char_id%||%"");if(is.null(p)||!nzchar(cid))return(NULL);a<-get_session_phase_actions(p$id[[1L]],cid);if(!nrow(a))return(tags$small("No time allocated yet."));tags$ul(lapply(seq_len(nrow(a)),function(i)tags$li(paste0(a$label[[i]]," — ",a$hours[[i]],"h"))))})
    output$watch_rota<-renderUI({invalidateLater(750,session);p<-active_phase();sid<-online_session_id();if(is.null(p)||is.na(sid))return(NULL);a<-get_session_phase_actions(p$id[[1L]]);a<-a[a$action_type=="watch",,drop=FALSE];if(!nrow(a))return(NULL);players<-get_session_players(sid);labels<-setNames(as.character(players$display_name%||%players$character_id),as.character(players$character_id));div(tags$strong("Watch rota: "),paste(vapply(seq_len(nrow(a)),function(i)paste0(labels[[as.character(a$character_id[[i]])]]%||%a$character_id[[i]]," — ",a$label[[i]]),character(1)),collapse="; "))})
    observeEvent(input$plan_short_rest,{r<-spend_phase_time("short_rest","Short rest",1);if(length(r$error%||%character()))showNotification(r$error,type="warning")else showNotification("One hour allocated to a short rest. Recovery applies when the DM resolves the phase.")},ignoreInit=TRUE)
    observeEvent(input$plan_long_rest,{r<-spend_phase_time("long_rest","Long rest",6);if(length(r$error%||%character()))showNotification(r$error,type="warning")else showNotification("All six hours allocated to a long rest. Recovery applies when the DM resolves the phase.")},ignoreInit=TRUE)
    show_watch_modal<-function(p){env<-shared_environment();phase_name<-if(is.null(env))"night"else env$time_of_day[[1L]];starts<-seq(0,max(0,as.numeric(p$duration_hours[[1L]])-2),by=2);choices<-setNames(as.character(starts),vapply(starts,function(s)paste(phase_hour_label(phase_name,s),"–",phase_hour_label(phase_name,s+2)),character(1)));showModal(modalDialog(title="Choose a watch",p("Each watch uses two hours of your phase budget. Choose one slot or take no watch."),selectInput(session$ns("watch_slot"),"Watch",choices=c("No watch"="",choices)),footer=tagList(modalButton("Later"),actionButton(session$ns("save_watch"),"Confirm Watch",class="btn btn-primary"))))}
    observeEvent(input$organise_watches,{p<-active_phase();if(is.null(p)||p$phase_kind[[1L]]!="rest")return(showNotification("Rest Mode is not open.",type="warning"));set_phase_watches_open(p$id[[1L]],TRUE);show_watch_modal(p)},ignoreInit=TRUE)
    observe({p<-active_phase();if(is.null(p)||!isTRUE(p$watches_open[[1L]])||identical(as.integer(p$id[[1L]]),watch_prompted_phase()))return();watch_prompted_phase(as.integer(p$id[[1L]]));show_watch_modal(p)})
    observeEvent(input$save_watch,{p<-active_phase();req(p);slot<-as.character(input$watch_slot%||%"");if(!nzchar(slot)){removeModal();return(showNotification("No watch selected."))};start<-as.numeric(slot);env<-shared_environment();label<-paste("Watch",phase_hour_label(env$time_of_day[[1L]],start),"–",phase_hour_label(env$time_of_day[[1L]],start+2));r<-spend_phase_time("watch",label,2,start,start+2);if(length(r$error%||%character()))showNotification(r$error,type="warning")else{removeModal();showNotification(paste(label,"recorded."))}},ignoreInit=TRUE)
    apply_supply_row<-function(x,row){if(is.null(row)||!is.data.frame(row)||!nrow(row))return(x);for(resource in c("wood","water","rations")){x$resources[[resource]]$cur<-as.integer(row[[resource]][[1L]]);x$resources[[resource]]$max<-as.integer(row[[paste0(resource,"_max")]][[1L]])};x}
    change_supply<-function(x,resource,amount=0L,fill=FALSE){sid<-online_session_id();if(!is.na(sid)){result<-adjust_session_supply(sid,resource,amount,fill,supply_defaults(x));if(is.null(result))return(NULL);result$char<-apply_supply_row(x,result$row);return(result)};before<-as.integer(x$resources[[resource]]$cur%||%0L);maximum<-as.integer(x$resources[[resource]]$max%||%if(resource=="wood")10L else 5L);after<-if(isTRUE(fill))maximum else max(0L,min(maximum,before+as.integer(amount)));x$resources[[resource]]$cur<-after;list(char=x,before=before,after=after,applied=!identical(before,after))}
    observe({invalidateLater(2500,session);if(!rest_visible()||isTRUE(restoring()))return();sid<-online_session_id();if(is.na(sid))return();x<-validate_character(state$char);row<-get_session_supplies(sid,supply_defaults(x));updated<-apply_supply_row(x,row);old<-vapply(c("wood","water","rations"),function(k)as.integer(x$resources[[k]]$cur%||%0L),integer(1));new<-vapply(c("wood","water","rations"),function(k)as.integer(updated$resources[[k]]$cur%||%0L),integer(1));if(!identical(old,new))state$char<-updated})
    gather_resource<-reactiveVal("");pending_help_id<-reactiveVal(NULL);shown_help_ids<-reactiveVal(integer());shown_gather_results<-reactiveVal(integer());gather_results_initialized<-reactiveVal(FALSE)
    gather_label<-function(resource)c(rations="forage for food",water="gather water",wood="gather firewood")[[resource]]
    begin_timed_gather<-function(resource,sfx){p<-active_phase();if(is.null(p)||p$phase_kind[[1L]]!="rest")return(showNotification("The DM has not opened Rest Mode.",type="warning"));if(phase_remaining()+1e-8<1)return(showNotification("You do not have an hour remaining for this action.",type="warning"));session$sendCustomMessage(session$ns("play_rest_sfx"),list(name=sfx));show_gather_modal(resource)}
    show_gather_modal<-function(resource){sid<-online_session_id();cid<-as.character(state$char_id%||%"");gather_resource(resource);helpers<-data.frame();if(!is.na(sid))helpers<-get_session_players(sid);if(nrow(helpers)){active<-if("is_active"%in%names(helpers))as.logical(helpers$is_active)else rep(TRUE,nrow(helpers));helpers<-helpers[as.character(helpers$character_id)!=cid&active,,drop=FALSE]};choices<-if(nrow(helpers))setNames(as.character(helpers$character_id),as.character(helpers$display_name%||%helpers$char_name))else character();manual<-isTRUE(session$rootScope()$input$manual_roll_mode_enabled);showModal(modalDialog(title=tools::toTitleCase(gather_label(resource)),p("This uses 1 hour of your Rest Mode time. Assistance uses 1 hour of the helper's time. Both make Survival checks and the better result determines the yield."),if(manual)tagList(div(class="alert alert-info","Roll one d20 for your Survival check."),numericInput(session$ns("manual_gather_roll"),"Natural d20 result",value=10,min=1,max=20,step=1)),selectInput(session$ns("gather_helper"),"Ask for assistance",choices=c("No assistant"="",choices)),footer=tagList(modalButton("Cancel"),actionButton(session$ns("confirm_gather"),"Begin",class="btn btn-success"))))}
    resolve_gather_result<-function(result){if(is.null(result))return(log_safe("⚠️ Gathering could not be resolved.",TRUE,"red"));if(!is.null(result$request_id))shown_gather_results(unique(c(shown_gather_results(),as.integer(result$request_id))));removeModal();resource<-result$resource%||%gather_resource();if(resource=="rations"){reward<-result$reward%||%list();if(identical(as.character(result$requester_id%||%""),as.character(state$char_id%||%""))){fresh<-load_character_from_db(state$char_id);if(!is.null(fresh))state$char<-fresh};message<-if(length(reward))paste0("found ",reward$name," (",reward$meta$ration_value," ration(s), fresh ",reward$meta$shelf_life_days," day(s))")else"found no safe food"}else message<-paste0("gained ",result$amount," ",resource);log_safe(paste0("🌿 ",tools::toTitleCase(gather_label(resource)),": check ",result$total,", ",message,if(isTRUE(result$assisted))" with assistance."else"."),TRUE,"green")}
    observeEvent(input$confirm_gather,{resource<-gather_resource();x<-validate_character(state$char);sid<-online_session_id();cid<-as.character(state$char_id%||%"");day<-as.integer(x$meta$day%||%1L);bonus<-camp_gathering_bonus(x);helper<-as.character(input$gather_helper%||%"");manual<-isTRUE(session$rootScope()$input$manual_roll_mode_enabled);natural<-if(manual)suppressWarnings(as.integer(input$manual_gather_roll%||%NA_integer_))else NA_integer_;if(manual&&(is.na(natural)||natural<1L||natural>20L))return(showNotification("Enter a natural d20 result from 1 to 20.",type="error"));spent<-spend_phase_time(paste0("gather_",resource),tools::toTitleCase(gather_label(resource)),1);if(length(spent$error%||%character()))return(showNotification(spent$error,type="warning"));if(is.na(sid)){roll<-(if(manual)natural else sample.int(20L,1L))+bonus;amount<-camp_gathering_yield(roll);if(resource=="rations"){reward<-camp_foraging_reward(roll);x<-add_foraged_food(x,reward,day);message<-if(length(reward))paste0("found ",reward$name," (",reward$meta$ration_value," ration(s))")else"found no safe food"}else{changed<-change_supply(x,resource,amount);if(is.null(changed))return();x<-changed$char;message<-paste0("gained ",amount," ",resource)};state$char<-x;removeModal();return(log_safe(paste0("🌿 Gathering check ",roll,": ",message,"."),TRUE,"green"))};request<-create_camp_gather_request(sid,day,cid,resource,bonus,if(nzchar(helper))helper else NULL,if(manual)natural else NULL);if(is.null(request))return(log_safe("⚠️ Gathering could not be started.",TRUE,"red"));if(nzchar(helper)){removeModal();log_safe("📯 Assistance request sent. Gathering will resolve if they accept.",TRUE,"gold")}else resolve_gather_result(resolve_camp_gather_request(request$id[[1L]],cid,TRUE,0L))},ignoreInit=TRUE)
    observe({invalidateLater(2500,session);cid<-as.character(state$char_id%||%"");if(!nzchar(cid)||isTRUE(state$offline_mode)||!is.null(pending_help_id())||!is.null(session$userData$pending_merchant_id)||!is.null(session$userData$pending_trade_id)||!is.null(session$userData$pending_note_id))return();requests<-get_pending_camp_gather_requests(cid);fresh<-requests[!requests$id%in%shown_help_ids(),,drop=FALSE];if(!nrow(fresh))return();r<-fresh[1,,drop=FALSE];shown_help_ids(unique(c(shown_help_ids(),r$id)));pending_help_id(as.integer(r$id[[1L]]));manual<-isTRUE(session$rootScope()$input$manual_roll_mode_enabled);showModal(modalDialog(title="Camp assistance requested",p(r$requester_name[[1L]]," asks you to ",gather_label(r$resource[[1L]])," together."),p("Accepting uses 1 hour of your remaining Rest Mode time."),if(manual)tagList(div(class="alert alert-info","Roll one d20 for your assisting Survival check."),numericInput(session$ns("manual_gather_help_roll"),"Natural d20 result",value=10,min=1,max=20,step=1)),footer=tagList(actionButton(session$ns("decline_gather_help"),"Decline"),actionButton(session$ns("accept_gather_help"),"Help",class="btn btn-success"))))})
    observeEvent(input$decline_gather_help,{id<-pending_help_id();if(!is.null(id))resolve_camp_gather_request(id,state$char_id,FALSE,0L);pending_help_id(NULL);removeModal()},ignoreInit=TRUE)
    observeEvent(input$accept_gather_help,{id<-pending_help_id();if(is.null(id))return();manual<-isTRUE(session$rootScope()$input$manual_roll_mode_enabled);natural<-if(manual)suppressWarnings(as.integer(input$manual_gather_help_roll%||%NA_integer_))else NULL;if(manual&&(is.na(natural)||natural<1L||natural>20L))return(showNotification("Enter a natural d20 result from 1 to 20.",type="error"));r<-spend_phase_time("gather_help","Help with gathering",1);if(length(r$error%||%character()))return(showNotification(r$error,type="warning"));result<-resolve_camp_gather_request(id,state$char_id,TRUE,camp_gathering_bonus(state$char),natural);pending_help_id(NULL);resolve_gather_result(result)},ignoreInit=TRUE)
    observe({invalidateLater(2500,session);cid<-as.character(state$char_id%||%"");if(!nzchar(cid)||isTRUE(state$offline_mode))return();rows<-get_finished_camp_gather_requests(cid);if(!isTRUE(gather_results_initialized())){shown_gather_results(as.integer(rows$id%||%integer()));gather_results_initialized(TRUE);return()};fresh<-rows[!rows$id%in%shown_gather_results(),,drop=FALSE];if(!nrow(fresh))return();shown_gather_results(unique(c(shown_gather_results(),as.integer(fresh$id))));r<-fresh[1,,drop=FALSE];if(r$status[[1L]]=="declined")showNotification("Your camp assistance request was declined. You may try solo or ask someone else.",type="warning",duration=8)else if(r$resource[[1L]]=="rations"){reward<-enemy_db_json(r$reward_json[[1L]],list());updated<-load_character_from_db(cid);if(!is.null(updated))state$char<-updated;showNotification(if(length(reward))paste0("Foraging complete: found ",reward$name," worth ",reward$meta$ration_value," ration(s).")else"Foraging complete: no safe food found.",type="message",duration=8)}else showNotification(paste0("Gathering complete: check ",r$result_total[[1L]],", +",r$yield_amount[[1L]]," ",r$resource[[1L]],"."),type="message",duration=8)})
    
    
    observe({
      if(!rest_visible())return()
      x <- validate_character(state$char);sid<-online_session_id();if(!is.na(sid))invalidateLater(2000,session)
      session$sendCustomMessage(
        session$ns("sync_rest_fire"),
        list(lit = if(!is.na(sid))get_session_fire(sid)else isTRUE(x$status$has_fire))
      )
    })
    
    # ----------------------------
    # TYLWYTH CHECK (RESTORED)
    # ----------------------------
    is_tylwyth <- reactive({
      x <- validate_character(state$char)
      race <- tolower(trimws(x$meta$race %||% ""))
      race == "tylwyth teg"
    })
    
    # ----------------------------
    # BLOOD WARNING MODAL (RESTORED)
    # ----------------------------
    show_blood_addiction_modal <- function(action = c("advance_day", "long_rest")) {
      action <- match.arg(action)
      confirm_id <- if (identical(action, "long_rest")) "force_long_rest" else "force_advance_day"
      confirm_label <- if (identical(action, "long_rest")) "⚠️ Rest Anyway" else "⚠️ Advance Anyway"
      showModal(modalDialog(
        title = "🩸 Blood Required",
        tags$p(
          "You are Tylwyth Teg and have not consumed blood today.",
          tags$br(),
          "Advancing may cause penalties."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns(confirm_id), confirm_label, class="btn-warning")
        ),
        easyClose = TRUE
      ))
    }
    
    observeEvent(input$force_advance_day, {
      removeModal()
      
      x <- validate_character(state$char)
      before <- x$meta$day %||% 1
      
      state$char <- advance_day_all(x, add_log, state = state)
      clamp_session_hp_to_max(state)
      
      log_safe(
        paste0("📅 Forced Day Advance: ", before, " → ", before + 1),
        toast = TRUE, flash = "gold"
      )
    })
    
    
    # ----------------------------
    # OUTPUTS
    # ----------------------------
    output$camp_time <- renderUI({
      x <- validate_character(state$char)
      env<-shared_environment();day<-if(is.null(env))x$meta$day else env$day_number[[1L]];cal <- get_celtic_date(day)
      tags$div(sprintf("📅 Year %d • %s • Day %d • %s", cal$year, cal$season, cal$day_of_season,if(is.null(env))"Local time"else time_of_day_label(env$time_of_day[[1L]])))
    })
    
    output$camp_environment <- renderUI({
      x <- validate_character(state$char)
      env<-shared_environment()
      if(is.null(env))return(tags$div(sprintf("🌡️ %s",x$environment$temperature%||%"Temperate")))
      tags$div(sprintf("🧭 %s • %s • %s",env$geography[[1L]],env$climate[[1L]],env$weather[[1L]]))
    })
    
    output$camp_status <- renderUI({
      hp <- get_effective_hp_state(state)
      max_hp <- as.integer((validate_character(state$char)$resources$hp$max) %||% 0)
      tags$div(HTML(sprintf("❤️ <b>%d/%d</b>&nbsp;&nbsp; Active survival effects are shown by your Rest Cards.",hp$cur,max_hp)))
    })
    
    output$rations_display <- renderUI({
      x <- validate_character(state$char)
      r <- x$resources$rations
      food<-food_rations_available(x)
      tags$div(sprintf("Carried food: %d ration%s • Party preserved rations: %d / %d",food,if(food==1)""else"s",r$cur %||% 0,r$max %||% 5))
    })
    
    output$wood_display <- renderUI({
      x <- validate_character(state$char)
      w <- x$resources$wood %||% list(cur=0,max=5)
      
      tags$div(sprintf("Total: %d / %d", w$cur, w$max))
    })
    
    observeEvent(input$gather_wood_btn,{begin_timed_gather("wood","gather")},ignoreInit=TRUE)
    
    output$water_display <- renderUI({
      x <- validate_character(state$char)
      w <- x$resources$water
      
      tags$div(sprintf("Total: %d / %d", w$cur %||% 0, w$max %||% 5))
    })
    
    
    output$fire_visual <- renderUI({
      x <- validate_character(state$char)
      sid<-suppressWarnings(as.integer(state$active_session_id%||%NA))
      if(!isTRUE(state$offline_mode)&&!is.na(sid)){invalidateLater(2500,session);has_fire<-get_session_fire(sid,list(wood=x$resources$wood$cur%||%3L,wood_max=x$resources$wood$max%||%10L))}else has_fire<-isTRUE(x$status$has_fire)
      
      if (has_fire) {
        img(src = "fire.png", class = "camp-fire", alt = "Lit campfire")
      } else {
        img(
          src = "fire.png", class = "camp-fire", alt = "Unlit campfire",
          style = "opacity:0.22; filter:grayscale(100%) brightness(35%);"
        )
      }
    })
    # ----------------------------
    # ADVANCE DAY (FIXED)
    # ----------------------------
    observeEvent(input$advance_day, {
      if (isTRUE(restoring())) return()
      sid<-suppressWarnings(as.integer(state$active_session_id%||%NA));cid<-as.character(state$char_id%||%"")
      if(!isTRUE(state$offline_mode)&&!is.na(sid)&&nzchar(cid)){perform_long_rest("skip");return()}
      
      x <- validate_character(state$char)
      
      # ----------------------------
      # ⚠️ PRE-DAY WARNINGS (ADD HERE)
      # ----------------------------
      if (!isTRUE(x$status$ate_today)) {
        log_safe("⚠️ You haven't eaten today.", TRUE, "gold")
      }
      
      if (!isTRUE(x$status$drank_today)) {
        log_safe("⚠️ You haven't had water today.", TRUE, "blue")
      }
      
      # ----------------------------
      # 🩸 BLOOD CHECK
      # ----------------------------
      blood <- x$resources$blood %||% list()
      intake <- (blood$addiction %||% list())$current_day_intake %||% 0
      
      a <- (blood$addiction %||% list())
      req <- required_intake(a)
      
      if (is_tylwyth() && intake < req) {
        show_blood_addiction_modal("advance_day")
        return()
      }
      
      # ----------------------------
      # ADVANCE DAY
      # ----------------------------
      before <- x$meta$day %||% 1
      state$char <- advance_day_all(x, add_log, state = state)
      clamp_session_hp_to_max(state)
      
      x <- restore_sindre(x, hours = 12, add_log = add_log)
      
      log_safe(
        paste0("📅 Advanced Day: ", before, " → ", before + 1),
        toast = TRUE, flash = "gold"
      )
    })
    
    # ----------------------------
    # LONG REST
    # ----------------------------
    perform_long_rest <- function(outcome=input$rest_outcome%||%"full") {
      outcome<-match.arg(as.character(outcome),c("full","half","skip"))
      x <- validate_character(state$char)
      session_id<-suppressWarnings(as.integer(state$active_session_id%||%NA));character_id<-as.character(state$char_id%||%"");cycle<-NULL
      if(!isTRUE(state$offline_mode)&&!is.na(session_id)&&nzchar(character_id)){
        env<-get_session_environment(session_id)
        if(is.null(env)||normalise_time_of_day(env$time_of_day[[1L]])!="night"){log_safe("🌙 A party long rest can begin at Night. The DM can advance time from Geography & Climate.",TRUE,"gold");return()}
        x$meta$day<-as.integer(env$day_number[[1L]])
        cycle<-begin_session_long_rest(session_id,character_id,x$meta$day%||%1L)
        if(is.null(cycle)){log_safe("⚠️ Party Long Rest could not be coordinated with the server. Nothing was changed.",TRUE,"red");return()}
        if(!isTRUE(cycle$can_apply)){log_safe(paste0("🌙 You already completed the party rest for day ",cycle$day_number,". Waiting for the rest of the party (",cycle$completed,"/",cycle$active,")."),TRUE,"gold");return()}
      }
      x$status$resting <- TRUE
      x$status$has_fire <- FALSE   # extinguish on long rest
      if(!is.null(cycle))set_session_fire(session_id,FALSE)
      if(identical(outcome,"full")){
        x <- restore_sindre(x, hours = 6, add_log = add_log)
        x <- reset_class_uses_for_rest(x, "long_rest")
      }else if(identical(outcome,"half")){
        x <- restore_sindre(x, hours = 3, add_log = add_log)
        x <- reset_class_uses_for_rest(x, "short_rest")
      }

      # Day-advance helpers can apply damage through the shared state. Publish
      # the prepared character first so they never read and restore stale data.
      state$char <- x
      state$char <- advance_day_all(state$char, add_log, state = state)
      max_hp <- get_effective_max_hp(state$char)
      if (is.na(max_hp) || max_hp < 1L) {
        log_safe("⚠️ Long Rest stopped: saved maximum HP is invalid. No HP value was overwritten.", TRUE, "red")
        return()
      }
      target_hp<-if(identical(outcome,"full"))max_hp else if(identical(outcome,"half"))min(max_hp,as.integer(state$char$resources$hp$cur%||%0L)+ceiling(max_hp/2)) else as.integer(state$char$resources$hp$cur%||%0L)
      ok <- set_effective_hp_state(state, cur = target_hp, temp = if(identical(outcome,"skip"))as.integer(state$char$resources$hp$temp%||%0L) else 0)
      if (!isTRUE(ok)) {
        log_safe("⚠️ Long Rest healing failed.", TRUE, "red")
        return()
      }
      clamp_session_hp_to_max(state)
      if(!is.null(cycle)){
        state$char$meta$day<-as.integer(cycle$day_number)
        saved_id<-tryCatch(save_character_to_db(state$char,char_id=character_id),error=function(e)NULL)
        if(is.null(saved_id)){log_safe("⚠️ Your party rest could not be saved, so it was not marked complete for the group.",TRUE,"red");return()}
        progress<-complete_session_long_rest(cycle$cycle_id,character_id,outcome)
        if(is.null(progress)){log_safe("⚠️ Your rest completed locally but the party completion marker could not be saved.",TRUE,"red");return()}
        label<-c(full="Full rest",half="Half rest",skip="No rest")[[outcome]]
        log_safe(paste0("🌙 ",label," recorded for party day ",cycle$day_number," (",progress$completed[[1L]],"/",progress$active[[1L]]," active characters)."),TRUE,"gold")
      }else log_safe("🌙 Rest complete. A new day begins.", TRUE, "gold")
    }
    observeEvent(input$long_rest, {
      x <- validate_character(state$char)
      addiction <- (x$resources$blood %||% list())$addiction %||% list()
      if (is_tylwyth() && as.numeric(addiction$current_day_intake %||% 0) < required_intake(addiction)) {
        show_blood_addiction_modal("long_rest")
        return()
      }
      perform_long_rest(input$rest_outcome%||%"full")
    })
    observeEvent(input$force_long_rest, {
      removeModal()
      perform_long_rest(input$rest_outcome%||%"full")
    }, ignoreInit = TRUE)
    
    # ----------------------------
    # SHORT REST
    # ----------------------------
    observeEvent(input$short_rest, {
      x <- validate_character(state$char)
      heal <- sample(1:8, 1)
      
      res <- apply_healing_to_state(state, heal)
      if (is.null(res)) {
        log_safe("⚠️ Short Rest healing failed.", TRUE, "red")
        return()
      }
      x <- validate_character(state$char)
      x <- restore_sindre(x, hours = 6, add_log = add_log)
      x <- reset_class_uses_for_rest(x, "short_rest")
      state$char <- x
      
      gained <- res$hp_after - res$hp_before
      log_safe(paste0("🛌 Rested for one phase: +", gained, " HP and Sindre regenerated at ",x$resources$sindre$regen%||%0," per hour."), TRUE, "green")
    })
    
    # ----------------------------
    # FIRE
    # ----------------------------
    observeEvent(input$light_fire, {
      p<-active_phase()
      if(is.null(p)||p$phase_kind[[1L]]!="rest")return(showNotification("A fire can only be lit during an official Rest Phase.",type="warning",duration=7))
      session$sendCustomMessage(session$ns("play_rest_sfx"), list(name = "flint"))
      
      x <- validate_character(state$char)
      
      wood <- x$resources$wood$cur %||% 0
      if (wood <= 0) {
        log_safe("⚠️ No wood to light a fire", TRUE, "gold")
        return()
      }
      result<-change_supply(x,"wood",-1L);if(is.null(result)||!isTRUE(result$applied))return(log_safe("⚠️ No party wood available",TRUE,"gold"));x<-result$char
      x$status$has_fire <- TRUE
      sid<-suppressWarnings(as.integer(state$active_session_id%||%NA));if(!isTRUE(state$offline_mode)&&!is.na(sid))set_session_fire(sid,TRUE)
      
      state$char <- x
      
      log_safe("🔥 Fire lit (used 1 wood)", TRUE, "red")
    })
    
    observe({
      x <- validate_character(state$char)
      sid<-suppressWarnings(as.integer(state$active_session_id%||%NA));lit<-if(!isTRUE(state$offline_mode)&&!is.na(sid)){invalidateLater(2500,session);get_session_fire(sid)}else isTRUE(x$status$has_fire)
      p<-active_phase();rest_open<-!is.null(p)&&identical(as.character(p$phase_kind[[1L]]),"rest")
      shinyjs::toggleState("light_fire",condition=rest_open)
      
      updateActionButton(
        session, "light_fire",
        label = if(!rest_open)"🔥 Fire — Rest Phase only"else if (isTRUE(lit)) "🔥 Stoke Fire" else "🔥 Light Fire"
      )
    })
    
    # ----------------------------
    # RATIONS (WITH TOASTS)
    # ----------------------------
    observeEvent(input$consume_rations_btn, {
      x <- validate_character(state$char)
      session$sendCustomMessage(session$ns("play_rest_sfx"), list(name = "eat"))
      food<-consume_food_ration(x)
      if(isTRUE(food$applied)){x<-food$char;food_name<-food$item_name}else{
        cur<-x$resources$rations$cur%||%0
        if(cur<=0)return(log_safe("⚠️ No edible food or preserved rations available",TRUE,"gold"))
        result<-change_supply(x,"rations",-1L);if(is.null(result)||!isTRUE(result$applied))return(log_safe("⚠️ No rations available",TRUE,"gold"));x<-result$char;food_name<-"a preserved party ration"
      }
      x$status$ate_today <- TRUE
      x$status$needs_hours<-x$status$needs_hours%||%list();x$status$needs_hours$food<-0
      state$char <- x
      log_safe(paste0("🍖 You eat ",food_name,". You feel sustained."),TRUE,"green")
    })
    
    observeEvent(input$forage_btn,{begin_timed_gather("rations","forage")},ignoreInit=TRUE)
    
    # ----------------------------
    # WATER (WITH TOASTS)
    # ----------------------------
    observeEvent(input$consume_water_btn, {
      x <- validate_character(state$char)
      session$sendCustomMessage(session$ns("play_rest_sfx"), list(name = "drink"))
      
      cur <- x$resources$water$cur %||% 0
      
      if (cur <= 0) {
        log_safe("⚠️ No water available", TRUE, "gold")
        return()
      }
      
      result<-change_supply(x,"water",-1L);if(is.null(result)||!isTRUE(result$applied))return(log_safe("⚠️ No party water available",TRUE,"gold"));x<-result$char
      x$status$drank_today <- TRUE
      x$status$needs_hours<-x$status$needs_hours%||%list();x$status$needs_hours$water<-0
      
      state$char <- x
      
      log_safe("💧 You drink water. You feel refreshed.", TRUE, "blue")
    })
    
    observeEvent(input$refill_btn,{begin_timed_gather("water","refill")},ignoreInit=TRUE)
    
  })
}
