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
  background:
    linear-gradient(rgba(20,15,10,0.65), rgba(20,15,10,0.85)),
    url('camp_bg.png') center/cover no-repeat;
  color: #f5e6c8;
  font-family: 'Cinzel', serif;
  padding: 12px;
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
"), as.list(rep(screen_id, 15)))))),
    
    div(
      id = screen_id,
      
      div(
        class = "camp-topbar",
        uiOutput(ns("camp_time")),
        uiOutput(ns("camp_environment"))
      ),
      
      h4(),
      h4(),
      h4(),
      h4(),
      
      div(
        class = "camp-center",
        uiOutput(ns("fire_visual")),
        actionButton(ns("short_rest"), "Short Rest", class = "camp-btn btn-top"),
        actionButton(ns("long_rest"),  "Long Rest",  class = "camp-btn btn-right"),
        actionButton(ns("advance_day"), "Skip Day",  class = "camp-btn btn-left"),
        actionButton(ns("light_fire"), "Light Fire", class = "camp-btn btn-bottom")
      ),
      
      h4(),
      
      div(
        class = "camp-status",
        uiOutput(ns("camp_status"))
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
              actionButton(ns("add_rations_btn"), "➕"),
              tags$div("Add")
            ),
            div(
              style = "text-align:center;",
              actionButton(ns("remove_rations_btn"), "➖"),
              tags$div("Remove")
            ),
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
              actionButton(ns("add_water_btn"), "➕"),
              tags$div("Add")
            ),
            div(
              style = "text-align:center;",
              actionButton(ns("remove_water_btn"), "➖"),
              tags$div("Remove")
            ),
            div(
              style = "text-align:center;",
              actionButton(ns("consume_water_btn"), "💧"),
              tags$div("Drink")
            ),
            div(
              style = "text-align:center;",
              actionButton(ns("refill_btn"), "🏞️"),
              tags$div("Refill")
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
      )
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
    
    
    observe({
      x <- validate_character(state$char)
      session$sendCustomMessage(
        session$ns("sync_rest_fire"),
        list(lit = isTRUE(x$status$has_fire))
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
    show_blood_addiction_modal <- function() {
      showModal(modalDialog(
        title = "🩸 Blood Required",
        tags$p(
          "You are Tylwyth Teg and have not consumed blood today.",
          tags$br(),
          "Advancing may cause penalties."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("force_advance_day"), "⚠️ Advance Anyway", class="btn-warning")
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
    
    
    #Remove rations
    
    # Remove rations
    observeEvent(input$remove_rations_btn, {
      x <- validate_character(state$char)
      x$resources$rations$cur <- max(0, (x$resources$rations$cur %||% 0) - 1)
      state$char <- x
      log_safe("➖ Removed 1 ration", TRUE, "gold")
    })
    
    # Remove water
    observeEvent(input$remove_water_btn, {
      x <- validate_character(state$char)
      x$resources$water$cur <- max(0, (x$resources$water$cur %||% 0) - 1)
      state$char <- x
      log_safe("➖ Removed water", TRUE, "blue")
    })
   
    # ----------------------------
    # OUTPUTS
    # ----------------------------
    output$camp_time <- renderUI({
      x <- validate_character(state$char)
      cal <- get_celtic_date(x$meta$day)
      tags$div(sprintf("📅 Year %d • %s • Day %d", cal$year, cal$season, cal$day_of_season))
    })
    
    output$camp_environment <- renderUI({
      x <- validate_character(state$char)
      tags$div(sprintf("🌡️ %s", x$environment$temperature %||% "Temparate"))
    })
    
    output$camp_status <- renderUI({
      x <- validate_character(state$char)
      hp <- get_effective_hp_state(state)
      max_hp <- as.integer((validate_character(state$char)$resources$hp$max) %||% 0)
      
      hunger_days <- x$status$hunger_days %||% 0
      thirst_days <- x$status$dehydration_days %||% 0
      
      ate_today   <- isTRUE(x$status$ate_today %||% FALSE)
      drank_today <- isTRUE(x$status$drank_today %||% FALSE)
      
      has_fire <- isTRUE(x$status$has_fire)
      
      temp <- x$environment$temperature %||% "Temperate"
      
      warmth_text <- if (has_fire) {
        "🔥 Warm"
      } else if (temp == "Cold") {
        "❄️ Freezing"
      } else {
        "😐 No Fire"
      }
      
      hunger_text <- if (ate_today) {
        "Fed Today"
      } else if (hunger_days == 0) {
        "Need to Eat"
      } else {
        paste0("Very Hungry (", hunger_days, ")")
      }
      
      thirst_text <- if (drank_today) {
        "Hydrated Today"
      } else if (thirst_days == 0) {
        "Need to Hydrate"
      } else {
        paste0(" Very Thirsty (", thirst_days, ")")
      }
      
      tags$div(
        HTML(sprintf("
  ❤️ <b>%d/%d</b>
  &nbsp;&nbsp; %s
  &nbsp;&nbsp; 🍖 %s
  &nbsp;&nbsp; 💧 %s
  &nbsp;&nbsp; 😵 <b>%d</b>
",
                     hp$cur, max_hp,
                     warmth_text,
                     hunger_text,
                     thirst_text,
                     x$status$exhaustion %||% 0
        ))
      )
    })
    
    output$rations_display <- renderUI({
      x <- validate_character(state$char)
      r <- x$resources$rations
      
      tags$div(sprintf("Total: %d / %d", r$cur %||% 0, r$max %||% 5))
    })
    
    output$wood_display <- renderUI({
      x <- validate_character(state$char)
      w <- x$resources$wood %||% list(cur=0,max=5)
      
      tags$div(sprintf("Total: %d / %d", w$cur, w$max))
    })
    
    observeEvent(input$gather_wood_btn, {
      x <- validate_character(state$char)
      session$sendCustomMessage(session$ns("play_rest_sfx"), list(name = "gather"))
      
      # ----------------------------
      # LIMIT: once per day
      # ----------------------------
      used <- x$status$gathered_wood_today %||% FALSE
      
      if (used) {
        log_safe("⚠️ You already gathered wood today.", TRUE, "gold")
        return()
      }
      
      x$status$gathered_wood_today <- TRUE
      
      # ----------------------------
      # GATHER LOGIC
      # ----------------------------
      gain <- sample(1:3,1)
      max_w <- x$resources$wood$max %||% 10
      cur   <- x$resources$wood$cur %||% 0
      
      x$resources$wood$cur <- min(max_w, cur + gain)
      
      state$char <- x
      
      log_safe(paste0("🌲 Gathered ", gain, " wood"), TRUE, "green")
    })
    
    output$water_display <- renderUI({
      x <- validate_character(state$char)
      w <- x$resources$water
      
      tags$div(sprintf("Total: %d / %d", w$cur %||% 0, w$max %||% 5))
    })
    
    
    output$fire_visual <- renderUI({
      x <- validate_character(state$char)
      has_fire <- isTRUE(x$status$has_fire)
      
      if (has_fire) {
        img(src = "fire.png", class = "camp-fire")
      } else {
        img(src = "embers.png", class = "camp-fire", style = "opacity:0.4; filter:grayscale(80%);")
      }
    })
    # ----------------------------
    # ADVANCE DAY (FIXED)
    # ----------------------------
    observeEvent(input$advance_day, {
      if (isTRUE(restoring())) return()
      
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
        show_blood_addiction_modal()
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
    observeEvent(input$long_rest, {
      x <- validate_character(state$char)
      max_hp <- as.integer(x$resources$hp$max %||% 0)
      
      ok <- set_effective_hp_state(state, cur = max_hp, temp = 0)
      if (!isTRUE(ok)) {
        log_safe("⚠️ Long Rest healing failed.", TRUE, "red")
        return()
      }
      
      x <- validate_character(state$char)
      x$status$resting <- TRUE
      x$status$has_fire <- FALSE   # extinguish on long rest
      
      x <- restore_sindre(x, hours = 12, add_log = add_log)
      
      state$char <- advance_day_all(x, add_log, state = state)
      clamp_session_hp_to_max(state)
      
      
      
      log_safe("🌙 Long Rest complete. A new day begins.", TRUE, "gold")
    })
    
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
      x <- restore_sindre(x, hours = 6, add_log = add_log)
      
      gained <- res$hp_after - res$hp_before
      log_safe(paste0("🛌 Rested +", gained, " HP"), TRUE, "green")
    })
    
    # ----------------------------
    # FIRE
    # ----------------------------
    observeEvent(input$light_fire, {
      session$sendCustomMessage(session$ns("play_rest_sfx"), list(name = "flint"))
      
      x <- validate_character(state$char)
      
      wood <- x$resources$wood$cur %||% 0
      
      if (wood <= 0) {
        log_safe("⚠️ No wood to light a fire", TRUE, "gold")
        return()
      }
      
      x$resources$wood$cur <- wood - 1
      x$status$has_fire <- TRUE
      
      state$char <- x
      
      log_safe("🔥 Fire lit (used 1 wood)", TRUE, "red")
    })
    
    observe({
      x <- validate_character(state$char)
      
      updateActionButton(
        session, "light_fire",
        label = if (isTRUE(x$status$has_fire)) "🔥 Stoke Fire" else "🔥 Light Fire"
      )
    })
    
    # ----------------------------
    # RATIONS (WITH TOASTS)
    # ----------------------------
    observeEvent(input$add_rations_btn, {
      x <- validate_character(state$char)
      max_r <- x$resources$rations$max %||% 5
      cur   <- x$resources$rations$cur %||% 0
      
      x$resources$rations$cur <- min(max_r, cur + 1)
      state$char <- x
      
      log_safe("➕ Gained 1 ration", TRUE, "green")
    })
    
    observeEvent(input$consume_rations_btn, {
      x <- validate_character(state$char)
      session$sendCustomMessage(session$ns("play_rest_sfx"), list(name = "eat"))
      
      cur <- x$resources$rations$cur %||% 0
      
      if (cur <= 0) {
        log_safe("⚠️ No rations to consume", TRUE, "gold")
        return()
      }
      
      x$resources$rations$cur <- cur - 1
      x$status$ate_today <- TRUE
      
      state$char <- x
      
      log_safe("🍖 You eat a ration. You feel sustained.", TRUE, "green")
    })
    
    observeEvent(input$forage_btn, {
      x <- validate_character(state$char)
      session$sendCustomMessage(session$ns("play_rest_sfx"), list(name = "forage"))
      
      used <- x$status$foraged_today %||% FALSE
      
      if (used) {
        log_safe("⚠️ You have already foraged today.", TRUE, "gold")
        return()
      }
      
      gain <- sample(0:2,1)
      max_r <- x$resources$rations$max %||% 5
      cur   <- x$resources$rations$cur %||% 0
      
      x$resources$rations$cur <- min(max_r, cur + gain)
      x$status$foraged_today <- TRUE
      
      state$char <- x
      
      log_safe(paste0("🌿 Foraged ", gain, " rations"), TRUE, "green")
    })
    
    # ----------------------------
    # WATER (WITH TOASTS)
    # ----------------------------
    observeEvent(input$add_water_btn, {
      x <- validate_character(state$char)
      max_w <- x$resources$water$max %||% 5
      cur   <- x$resources$water$cur %||% 0
      
      x$resources$water$cur <- min(max_w, cur + 1)
      state$char <- x
      
      log_safe("💧 Gained water", TRUE, "blue")
    })
    
    observeEvent(input$consume_water_btn, {
      x <- validate_character(state$char)
      session$sendCustomMessage(session$ns("play_rest_sfx"), list(name = "drink"))
      
      cur <- x$resources$water$cur %||% 0
      
      if (cur <= 0) {
        log_safe("⚠️ No water available", TRUE, "gold")
        return()
      }
      
      x$resources$water$cur <- cur - 1
      x$status$drank_today <- TRUE
      
      state$char <- x
      
      log_safe("💧 You drink water. You feel refreshed.", TRUE, "blue")
    })
    
    observeEvent(input$refill_btn, {
      x <- validate_character(state$char)
      session$sendCustomMessage(session$ns("play_rest_sfx"), list(name = "refill"))
      
      max_w <- x$resources$water$max %||% 5
      x$resources$water$cur <- max_w
      
      state$char <- x
      
      log_safe("🏞️ Water refilled to max", TRUE, "blue")
    })
    
  })
}