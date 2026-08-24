library(shiny)

debugCombatUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    
    title = "Combat",
    
    value = "debug_combat",
    
    tagList(
      
      tags$link(rel = "stylesheet", type = "text/css", href = "css/combat.css"),
      
      tags$script(src = paste0("js/combat2d_simple.js?v=", as.integer(Sys.time()))),
      tags$script(type="module",src=paste0("js/combat3d_lean.js?v=",as.integer(Sys.time()))),
      
      div(
        
        class = "combat-wrap",
        
        div(
          class = "combat-card",
          div(
            class = "combat-compact-header",
            uiOutput(ns("header_ui"))
          ),
          div(
            class = "combat-command-bar",
            div(
              class = "combat-command-actions",
              div(id=ns("combat_movement_group"),class="combat-action-group",span(class="combat-action-label","Movement"),uiOutput(ns("dash_button_ui")),uiOutput(ns("phase_move_ui"))),
              div(id=ns("combat_actions_group"),class="combat-action-group",span(class="combat-action-label","Actions"),actionButton(ns("open_standard_actions"),"Combat Actions",class="btn btn-default"),uiOutput(ns("level_two_actions_ui")),uiOutput(ns("level_three_actions_ui")),uiOutput(ns("class_actions_ui")),uiOutput(ns("rogue_combat_ui"))),
              div(class="combat-action-group",span(class="combat-action-label","Glyphs"),uiOutput(ns("combat_runes_ui")),uiOutput(ns("combat_wards_ui")))
            ),
            div(id=ns("combat_turn_group"),class="combat-command-turn",span(class="combat-action-label","Turn"),actionButton(ns("open_loot"),"Loot Defeated",class="btn btn-success"),actionButton(ns("override_action_budget"),"Override",class="btn btn-default"),actionButton(ns("end_turn"),"End Turn",class="btn btn-warning"))
          ),
          div(id=ns("combat_status_group"),class="combat-status-strip",uiOutput(ns("turn_actions_ui")),div(class="combat-turn-box",uiOutput(ns("turn_notice_ui")))
          )
        ),
        
        uiOutput(ns("combat_layout_ui"))
      )
    )
  )
}

debugCombatServer <- function(id, core, ctrl, add_log = NULL,
                              live_snapshot = NULL,
                              refresh_live_snapshot = NULL) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    
    source("server/combat_map_logic.R", local = FALSE)
    
    refresh_key <- reactiveVal(0)
    movement_paths <- reactiveVal(list())
    initiative_key <- reactiveVal(0L)
    bump_initiative <- function() initiative_key(isolate(initiative_key()) + 1L)
    
    positions_key <- reactiveVal(0)
    combat_key <- reactiveVal(0)
    events_key <- reactiveVal(0)
    
    map_ui_ready <- reactiveVal(FALSE)
    
    map_visual_key <- reactiveVal(0L)
    map_send_generation <- reactiveVal(0L)
    selected_target_id <- reactiveVal("")
    current_attack_is_opp <- reactiveVal(FALSE)
    current_attack_is_ready <- reactiveVal(FALSE)
    prompted_opportunity_events <- reactiveVal(character())
    
    bump_map_visual <- function() {
      map_visual_key(isolate(map_visual_key()) + 1L)
    }
    observeEvent(session$rootScope()$input$combat3d_lean_ready, {
      if (identical(input$map_render_mode %||% "2d", "3d")) later::later(bump_map_visual, 0.05)
    }, ignoreInit = FALSE)
    observeEvent(core$char_rev(),{bump_map_visual();if(is.function(refresh_live_snapshot))refresh_live_snapshot()},ignoreInit=TRUE)
    observeEvent(core$state$char,{bump_map_visual()},ignoreInit=TRUE)
    observeEvent(session$rootScope()$input$main_tabs,{if(identical(session$rootScope()$input$main_tabs,"debug_combat"))bump_map_visual()},ignoreInit=TRUE)

    
    bump_positions <- function() positions_key(isolate(positions_key()) + 1L)
    bump_combat    <- function() combat_key(isolate(combat_key()) + 1L)
    bump_events    <- function() events_key(isolate(events_key()) + 1L)
 
    
    bump_refresh <- function() {
      refresh_key(isolate(refresh_key()) + 1L)
      bump_positions()
      bump_combat()
      bump_events()
      bump_initiative()
      bump_map_visual()
      if (is.function(refresh_live_snapshot)) refresh_live_snapshot()
    }

    observeEvent(input$open_loot, {
      combat <- combat_tbl()
      if (!is.data.frame(combat) || !nrow(combat) ||
          !identical(as.character(combat$phase[1] %||% ""), "combat")) {
        showNotification("The DM must start combat first.", type = "warning")
        return()
      }
      enemies <- tryCatch(get_encounter_enemies(current_encounter_id()),error=function(e)data.frame())
      if(!nrow(enemies)){showNotification("There are no enemies to loot.",type="warning");return()}
      unlooted <- is.na(enemies$looted_by) | !nzchar(as.character(enemies$looted_by))
      available <- enemies[as.integer(enemies$hp_current)<=0L & unlooted,,drop=FALSE]
      if(!nrow(available)){showNotification("No defeated unlooted enemies.",type="warning");return()}
      showModal(modalDialog(title="Loot defeated enemy",selectInput(session$ns("loot_enemy_id"),"Enemy",choices=setNames(as.character(available$enemy_uuid),as.character(available$name))),
        p("Items go directly into Inventory or Armoury. Gold goes into your purse."),footer=tagList(modalButton("Cancel"),actionButton(session$ns("confirm_loot"),"Take loot",class="btn btn-success"))))
    },ignoreInit=TRUE)

    observeEvent(input$confirm_loot, {
      result<-claim_defeated_enemy_loot(current_encounter_id(),input$loot_enemy_id,core$state$char_id)
      if(is.null(result)){removeModal();showNotification("That enemy cannot be looted or was already claimed.",type="error");return()}
      char<-validate_character(core$state$char); items<-inventory_normalize(char$inventory$items)
      if(length(result$loot)) for(item in result$loot) items<-rbind(items,enemy_loot_to_inventory_row(item))
      char$inventory$items<-inventory_normalize(items); char$inventory$gold<-as.numeric(char$inventory$gold%||%0)+as.numeric(result$gold%||%0); core$state$char<-char
      save_character_to_db(char,core$state$char_id); removeModal(); bump_refresh()
      showNotification(paste0("Looted ",result$name,": ",length(result$loot)," item(s) and ",result$gold," gold."),type="message",duration=7)
    },ignoreInit=TRUE)

    snapshot_data <- reactive({
      if (!is.function(live_snapshot)) return(empty_player_live_snapshot())
      live_snapshot()
    })

    decode_effect_payload <- function(value) {
      if (is.list(value) && !is.data.frame(value)) return(value)
      text <- as.character(value %||% "")
      if (!nzchar(text)) return(list())
      tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) list())
    }

    active_effects <- reactive({
      effects <- snapshot_data()$effects %||% data.frame()
      if (!is.data.frame(effects) || !nrow(effects)) return(data.frame())
      round_number <- suppressWarnings(as.integer(snapshot_data()$combat$round_number[1] %||% 1L))
      if ("ends_round" %in% names(effects)) {
        keep <- is.na(effects$ends_round) | as.integer(effects$ends_round) >= round_number
        effects <- effects[keep, , drop = FALSE]
      }
      effects
    })

    actor_conditions <- function(actor_id) {
      effects <- active_effects()
      if (!is.data.frame(effects) || !nrow(effects)) return(character())
      rows <- effects[
        as.character(effects$effect_type %||% "") == "condition" &
          as.character(effects$target_actor_id %||% "") == as.character(actor_id),
        , drop = FALSE
      ]
      if (!nrow(rows)) return(character())
      conditions <- unique(vapply(seq_len(nrow(rows)), function(i) {
        as.character(decode_effect_payload(rows$payload[[i]])$condition %||% "")
      }, character(1)))
      if (identical(as.character(actor_id), as.character(core$state$char_id %||% "")) &&
          isTRUE(core$state$char$status$raging %||% FALSE) &&
          character_has_feature(core$state$char, "mindless_rage")) {
        conditions <- setdiff(conditions, c("charmed", "frightened"))
      }
      conditions
    }

    tile_in_spell_area <- function(x, y, spell_id) {
      effects <- active_effects()
      if (!is.data.frame(effects) || !nrow(effects)) return(FALSE)
      rows <- effects[as.character(effects$spell_id %||% "") == spell_id &
                        as.character(effects$effect_type %||% "") == "area", , drop = FALSE]
      if (!nrow(rows)) return(FALSE)
      any(vapply(seq_len(nrow(rows)), function(i) {
        dx <- abs(as.integer(x) - as.integer(rows$center_x[i]))
        dy <- abs(as.integer(y) - as.integer(rows$center_y[i]))
        !is.na(dx) && !is.na(dy) && max(dx, dy) * 5L <= as.integer(rows$radius_ft[i] %||% 0L)
      }, logical(1)))
    }

    rain_damage_modifier <- function(target_id, damage_type) {
      actors <- encounter_actors_tbl()
      target <- actors[as.character(actors$actor_id) == as.character(target_id), , drop = FALSE]
      if (!nrow(target)) return(0L)
      effects <- active_effects()
      rows <- effects[as.character(effects$spell_id %||% "") == "calling_rain" &
                        as.character(effects$effect_type %||% "") == "area", , drop = FALSE]
      if (!nrow(rows)) return(0L)
      for (i in seq_len(nrow(rows))) {
        dx <- abs(as.integer(target$x[1]) - as.integer(rows$center_x[i]))
        dy <- abs(as.integer(target$y[1]) - as.integer(rows$center_y[i]))
        if (is.na(dx) || is.na(dy) || max(dx, dy) * 5L > as.integer(rows$radius_ft[i])) next
        payload <- decode_effect_payload(rows$payload[[i]])
        upgrade <- payload$upgrade %||% list()
        type <- tolower(as.character(damage_type %||% ""))
        if (identical(type, "fire")) return(as.integer(upgrade$fire_modifier %||% -4L))
        if (type %in% c("cold", "lightning")) return(as.integer(upgrade$cold_lightning_modifier %||% 2L))
      }
      0L
    }
    
    pending_attack <- reactiveVal(NULL)
    turn_move_ft <- reactiveVal(0L)
    pending_move <- reactiveVal(NULL)
    movement_dash <- reactiveVal(FALSE)
    dash_action_spent <- reactiveVal(FALSE)
    movement_phase <- reactiveVal(FALSE)
    turn_budget <- reactiveVal(new_turn_action_budget())
    cunning_mode <- reactiveVal("")
    reckless_active <- reactiveVal(FALSE)
    manoeuvre_active <- reactiveVal("")
    player_reaction_available <- reactiveVal(TRUE)
    sneak_attack_used <- reactiveVal(FALSE)
    current_attack_mode <- reactiveVal("action")
    offhand_ready <- reactiveVal(NULL)

    player_has_feature <- function(feature_id) {
      char <- core$state$char
      if (is.null(char)) return(FALSE)
      any(vapply(get_unlocked_class_features(char), function(feature) {
        identical(as.character(feature$id %||% ""), feature_id)
      }, logical(1)))
    }

    character_has_feature <- function(char, feature_id) {
      any(vapply(get_unlocked_class_features(char), function(feature) {
        identical(as.character(feature$id %||% ""), feature_id)
      }, logical(1)))
    }

    current_turn_key <- reactive({
      combat <- combat_tbl()
      paste(
        current_encounter_id(),
        as.character(combat$round_number[1] %||% 0L),
        as.character(active_actor_id() %||% ""),
        sep = "::"
      )
    })

    bloodlust_turn_handled <- reactiveVal("")
    perform_forced_bloodlust_bite <- function(reason = "bloodlust") {
      char <- validate_character(core$state$char)
      actors <- encounter_actors_tbl()
      caster_id <- as.character(core$state$char_id %||% "")
      caster <- actors[as.character(actors$actor_id) == caster_id, , drop=FALSE]
      candidates <- actors[as.character(actors$actor_id) != caster_id &
        as.character(actors$actor_type) %in% c("player","enemy") &
        as.integer(actors$current_hp %||% 0L) > 0L, , drop=FALSE]
      if (!nrow(caster) || !nrow(candidates)) {
        log_safe("🩸 Bloodlust surges, but there is nobody close enough to bite.")
        return(FALSE)
      }
      candidates$distance <- pmax(abs(as.integer(candidates$x)-as.integer(caster$x[1])),
                                  abs(as.integer(candidates$y)-as.integer(caster$y[1]))) * 5L
      adjacent <- candidates[!is.na(candidates$distance) & candidates$distance <= 5L,,drop=FALSE]
      if (!nrow(adjacent)) {
        log_safe("🩸 Bloodlust demands a bite, but no creature is adjacent.")
        return(FALSE)
      }
      adjacent$enemy_priority <- as.integer(as.character(adjacent$actor_type)!="enemy")
      adjacent <- adjacent[order(adjacent$distance,adjacent$enemy_priority,as.character(adjacent$actor_id)),,drop=FALSE]
      target <- adjacent[1,,drop=FALSE]
      target_id <- as.character(target$actor_id[1]); target_type <- as.character(target$actor_type[1])
      target_char <- load_actor_for_combat(target_id,target_type)
      attack_roll <- sample.int(20L,1L)
      attack_total <- attack_roll + get_character_ability_mod(char,"str") + get_character_prof_bonus(char)
      target_ac <- get_effective_actor_ac(target_id,target_type,target_char)
      hit <- attack_roll == 20L || attack_total >= target_ac
      damage <- 0L
      if (hit) {
        damage <- roll_dice_expr("1d8")$total + get_character_ability_mod(char,"str")
        if (attack_roll == 20L) damage <- damage + roll_dice_expr("1d8")$total
        damage <- max(0L,as.integer(damage))
        if (target_type=="player") damage_player_in_encounter(current_encounter_id(),target_id,damage)
        else damage_encounter_enemy(current_encounter_id(),target_id,damage)
        addiction <- (char$resources$blood%||%list())$addiction%||%list()
        addiction$current_day_intake <- as.numeric(addiction$current_day_intake%||%0)+0.5
        char$resources$blood$addiction <- addiction
        core$state$char <- char
      }
      log_game_event(current_encounter_id(),"bloodlust_bite","player",caster_id,target_id,
        payload=list(reason=reason,target_name=as.character(target$display_name[1]),attack_roll=attack_roll,
                     attack_total=attack_total,target_ac=target_ac,hit=hit,damage=damage))
      log_safe(paste0("🩸 Bloodlust forces a bite at ",target$display_name[1],": ",
        attack_total," vs AC ",target_ac,if(hit)paste0(" — ",damage," damage.")else" — miss."))
      bump_refresh(); TRUE
    }

    observe({
      key <- current_turn_key()
      if (!identical(as.character(turn_budget()$key %||% ""), key)) {
        turn_budget(new_turn_action_budget(key))
        cunning_mode("")
        reckless_active(FALSE)
        manoeuvre_active("")
        sneak_attack_used(FALSE)
        current_attack_mode("action")
        offhand_ready(NULL)
        movement_dash(FALSE)
        dash_action_spent(FALSE)
        updateCheckboxInput(session, "dash_move", value = FALSE)
        if (identical(as.character(active_actor_id() %||% ""), as.character(core$state$char_id %||% ""))) {
          player_reaction_available(TRUE)
          if (!identical(bloodlust_turn_handled(),key) && bloodlust_bite_required(core$state$char,start_of_turn=TRUE)) {
            bloodlust_turn_handled(key)
            if (perform_forced_bloodlust_bite("stage_4_start_of_turn")) spend_action_safe("action","Forced Bloodlust Bite")
          }
        }
      }
    })

    spend_action_safe <- function(action_type, label) {
      combat <- combat_tbl()
      if (!is.data.frame(combat) || !nrow(combat) ||
          !identical(as.character(combat$phase[1] %||% ""), "combat")) {
        log_safe("⚠️ The DM must start combat before combat actions can be used.")
        return(FALSE)
      }
      updated <- spend_turn_action(turn_budget(), action_type)
      if (is.null(updated)) {
        log_safe(paste0("⚠️ No ", gsub("_", " ", action_type), " remains for ", label, "."))
        return(FALSE)
      }
      turn_budget(updated)
      TRUE
    }

    spend_attack_safe <- function(char, label = "Attack") {
      combat <- combat_tbl()
      if (!is.data.frame(combat) || !nrow(combat) ||
          !identical(as.character(combat$phase[1] %||% ""), "combat")) {
        log_safe("⚠️ The DM must start combat before attacks can be used.")
        return(FALSE)
      }
      updated <- spend_attack_from_budget(turn_budget(), attacks_per_attack_action(char))
      if (is.null(updated)) {
        log_safe(paste0("⚠️ No attack remains for ", label, "."))
        return(FALSE)
      }
      turn_budget(updated)
      TRUE
    }

    combat_runes <- reactive({
      inv<-inventory_normalize(validate_character(core$state$char)$inventory$items);keep<-inv$type=="glyph"&vapply(inv$meta,function(m)identical(as.character((m%||%list())$glyph_type%||%""),"rune")&&identical(as.character((m%||%list())$status%||%""),"ready"),logical(1));inv[keep,,drop=FALSE]
    })
    rune_targeting<-reactiveVal(NULL)
    ward_targeting<-reactiveVal(NULL)
    output$combat_runes_ui<-renderUI({r<-combat_runes();if(!nrow(r))return(tags$button(type="button",class="btn btn-default",disabled="disabled",title="Complete a rune project in Glyphs first","Runes (0)"));actionButton(session$ns("open_combat_rune"),paste0("Runes (",nrow(r),")"),class="btn btn-danger")})
    observeEvent(input$open_combat_rune,{
      if(!isTRUE(is_players_turn()))return();r<-combat_runes();if(!nrow(r))return();showModal(modalDialog(title="Choose a crafted rune",selectInput(session$ns("combat_rune_id"),"Available rune",choices=setNames(vapply(r$meta,function(m)as.character(m$glyph_id),character(1)),vapply(seq_len(nrow(r)),function(i){m<-r$meta[[i]];paste0(r$name[[i]]," · ",m$damage," ",m$damage_type," · ",m$area_ft,"ft · ",m$duration_rounds%||%1L," round(s) · ",m$rank)},character(1)))),p("Next, click a map tile or token within 60 ft and with a clear throwing line. Releasing the rune uses your bonus action and its area may affect allies."),footer=tagList(modalButton("Cancel"),actionButton(session$ns("confirm_combat_rune"),"Choose Centre on Map",class="btn btn-danger"))))
    },ignoreInit=TRUE)
    observeEvent(input$confirm_combat_rune,{
      req(input$combat_rune_id);rune_targeting(list(glyph_id=as.integer(input$combat_rune_id),range_ft=60L));removeModal();showNotification("Click a point within 60 ft with a clear throwing line. No action is spent until a valid point is chosen.",type="message",duration=12)
    },ignoreInit=TRUE)
    release_rune_at_map_point<-function(x,y,target_id=NULL){pending<-rune_targeting();if(is.null(pending))return(FALSE);self<-get_actor_row(as.character(core$state$char_id),"player");if(!nrow(self))return(TRUE);geometry<-combat_attack_geometry(map_tiles_rv(),self$x[[1L]],self$y[[1L]],x,y,pending$range_ft%||%60L,pending$range_ft%||%60L,map_id());if(!isTRUE(geometry$in_range)){showNotification(paste0("That point is ",geometry$distance_ft," ft away; runes can be thrown up to 60 ft."),type="warning",duration=12);return(TRUE)};if(!isTRUE(geometry$line_clear)){showNotification("A wall or sight-blocking obstacle blocks the rune's throwing line.",type="warning",duration=12);return(TRUE)};rune_targeting(NULL);if(!spend_action_safe("bonus_action","Release Rune"))return(TRUE);res<-release_crafted_rune_at(as.character(core$state$char_id),pending$glyph_id,current_encounter_id(),x,y,target_id);if(length(res$error%||%character())){showNotification(res$error,type="error",duration=12);return(TRUE)};if(!is.null(res$character))core$state$char<-validate_character(res$character);if(is.function(core$bump_char_rev))core$bump_char_rev();refresh_key(refresh_key()+1L);events_key(events_key()+1L);bump_map_visual();if(isTRUE(res$unstable))showNotification(paste("Rune instability!",res$instability_damage,"damage."),type="error",duration=12)else showNotification(paste0(res$damage_total," ",res$damage_type," damage; affected: ",paste(res$affected,collapse=", "),if(res$glyph$active_duration_rounds[[1L]]>1)paste0(". The area persists for ",res$glyph$active_duration_rounds[[1L]]," rounds.")else""),type="message",duration=12);TRUE}

    combat_wards<-reactive({inv<-inventory_normalize(validate_character(core$state$char)$inventory$items);keep<-inv$type=="glyph"&vapply(inv$meta,function(m)identical(as.character((m%||%list())$glyph_type%||%""),"ward")&&identical(as.character((m%||%list())$status%||%""),"ready"),logical(1));inv[keep,,drop=FALSE]})
    output$combat_wards_ui<-renderUI({w<-combat_wards();zones<-get_active_glyph_zones(current_encounter_id());zones<-zones[zones$glyph_type=="ward",,drop=FALSE];tagList(if(nrow(w))actionButton(session$ns("open_combat_ward"),paste0("Wards (",nrow(w),")"),class="btn btn-info")else tags$button(type="button",class="btn btn-default",disabled="disabled",title="Complete a ward project in Glyphs first","Wards (0)"),if(nrow(zones))actionButton(session$ns("open_disrupt_ward"),"Disrupt Ward",class="btn btn-default")else NULL)})
    observeEvent(input$open_combat_ward,{if(!isTRUE(is_players_turn()))return();w<-combat_wards();if(!nrow(w))return();showModal(modalDialog(title="Place a crafted ward",selectInput(session$ns("combat_ward_id"),"Available ward",choices=setNames(vapply(w$meta,function(m)as.character(m$glyph_id),character(1)),vapply(seq_len(nrow(w)),function(i){m<-w$meta[[i]];paste0(w$name[[i]]," · ",m$area_ft,"ft · resists ",paste(m$resistance_types,collapse=", ")," · Arcane Score ",m$arcane_score)},character(1)))),p(if(isTRUE(is_exploration_phase()))"Place this prepared ward before combat begins. It persists until disrupted."else"Placing the ward uses your action. It then persists until disrupted."),footer=tagList(modalButton("Cancel"),actionButton(session$ns("confirm_combat_ward"),"Choose Centre on Map",class="btn btn-info"))))},ignoreInit=TRUE)
    observeEvent(input$confirm_combat_ward,{req(input$combat_ward_id);ward_targeting(list(glyph_id=as.integer(input$combat_ward_id)));removeModal();showNotification("Click a map tile or token to place the ward. No action is spent until you click.",type="message",duration=10)},ignoreInit=TRUE)
    place_ward_at_map_point<-function(x,y){pending<-ward_targeting();if(is.null(pending))return(FALSE);ward_targeting(NULL);if(!isTRUE(is_exploration_phase())&&!spend_action_safe("action","Place Ward"))return(TRUE);res<-place_crafted_ward_at(as.character(core$state$char_id),pending$glyph_id,current_encounter_id(),x,y);if(length(res$error%||%character())){showNotification(res$error,type="error");return(TRUE)};core$state$char<-validate_character(res$character);if(is.function(core$bump_char_rev))core$bump_char_rev();refresh_key(refresh_key()+1L);events_key(events_key()+1L);bump_map_visual();showNotification(paste0("Ward placed: ",res$area_ft,"ft radius; resistance to ",paste(res$resistance_types,collapse=", "),"."),type="message",duration=10);TRUE}
    observeEvent(input$open_disrupt_ward,{if(!isTRUE(is_players_turn()))return();z<-get_active_glyph_zones(current_encounter_id());z<-z[z$glyph_type=="ward",,drop=FALSE];if(!nrow(z))return(showNotification("No active ward remains.",type="warning"));showModal(modalDialog(title="Disrupt a ward",selectInput(session$ns("disrupt_ward_id"),"Active ward",choices=setNames(z$id,paste0(z$name," · Arcane Score ",z$arcane_score))),p("This uses your action and rolls Arcana against the ward's Arcane Score."),footer=tagList(modalButton("Cancel"),actionButton(session$ns("confirm_disrupt_ward"),"Roll Arcana",class="btn btn-danger"))))},ignoreInit=TRUE)
    observeEvent(input$confirm_disrupt_ward,{if(!spend_action_safe("action","Disrupt Ward"))return();removeModal();bonus<-glyph_arcana_bonus(validate_character(core$state$char));res<-disrupt_glyph_ward(current_encounter_id(),input$disrupt_ward_id,"player",core$state$char_id,bonus);if(length(res$error%||%character()))return(showNotification(res$error,type="error"));refresh_key(refresh_key()+1L);events_key(events_key()+1L);bump_map_visual();showNotification(paste0("Arcana ",res$total," vs ",res$arcane_score,": ",if(res$success)paste(res$name,"breaks.")else"the ward holds."),type=if(res$success)"message"else"warning",duration=10)},ignoreInit=TRUE)

    output$turn_actions_ui <- renderUI({
      budget <- turn_budget()
      div(
        class = "combat-pill",
        paste0("Actions ", budget$actions, " • Bonus ", budget$bonus_actions,
               " • Reaction ", if (isTRUE(player_reaction_available())) 1L else 0L,
               if (as.integer(budget$attack_chain %||% 0L) + as.integer(budget$bonus_attacks %||% 0L) > 0L) {
                 paste0(" • Attacks ", as.integer(budget$attack_chain %||% 0L) + as.integer(budget$bonus_attacks %||% 0L))
               } else "")
      )
    })

    observeEvent(input$override_action_budget, {
      if (!isTRUE(is_players_turn())) {
        log_safe("⚠️ Action override is only available on your own turn.")
        return()
      }
      showModal(modalDialog(
        title = "Override action limit?",
        p("This grants one extra action for the current turn. Use it to recover from a mistaken click while combat actions are being tested."),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_action_override"), "Grant +1 Action", class = "btn btn-warning"))
      ))
    }, ignoreInit = TRUE)
    observeEvent(input$confirm_action_override, {
      removeModal()
      turn_budget(grant_turn_action(turn_budget(), 1L))
      log_game_event(current_encounter_id(), "action_override", "player", as.character(core$state$char_id %||% ""), payload = list(actions_added = 1L))
      log_safe("🛠️ Action override granted: +1 action this turn.")
    }, ignoreInit = TRUE)

    output$dash_button_ui <- renderUI({
      actionButton(session$ns("arm_dash"), if (isTRUE(movement_dash())) "Dash Armed ✓" else "Dash / Sprint",
                   class = if (isTRUE(movement_dash())) "btn btn-success" else "btn btn-default")
    })

    observeEvent(input$arm_dash, {
      combat <- combat_tbl()
      if (!is.data.frame(combat) || !nrow(combat) ||
          !identical(as.character(combat$phase[1] %||% ""), "combat")) {
        log_safe("⚠️ The DM must start combat before Dash can be used.")
        return()
      }
      if (isTRUE(movement_dash()) && !isTRUE(dash_action_spent())) {
        movement_dash(FALSE)
        return()
      }
      if (isTRUE(movement_dash())) {
        log_safe("⚠️ Dash has already been committed this turn.")
        return()
      }
      showModal(modalDialog(
        title = "Use Dash / Sprint?",
        p("Cost: 1 action, charged only when you move beyond normal speed. Sindre: 0."),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_dash"), "Arm Dash", class = "btn btn-primary"))
      ))
    }, ignoreInit = TRUE)
    observeEvent(input$confirm_dash, {
      removeModal()
      movement_dash(TRUE)
      log_safe("🏃 Dash armed. It can be cancelled freely until extra movement is used.")
    }, ignoreInit = TRUE)

    output$level_two_actions_ui <- renderUI({
      buttons <- list()
      if (player_has_feature("cunning_action")) {
        buttons <- c(buttons, list(actionButton(session$ns("open_cunning_action"), "Cunning Action", class = "btn btn-default")))
      }
      if (player_has_feature("action_surge")) {
        char <- validate_character(core$state$char)
        action <- list(usage = list(key = "action_surge", recharge = "short_rest"))
        buttons <- c(buttons, list(actionButton(
          session$ns("use_action_surge"), "Action Surge",
          class = "btn btn-default", disabled = if (!class_action_use_available(char, action)) "disabled" else NULL
        )))
      }
      if (player_has_feature("reckless_attack")) {
        buttons <- c(buttons, list(actionButton(
          session$ns("toggle_reckless"),
          if (isTRUE(reckless_active())) "Reckless ✓" else "Reckless Attack",
          class = if (isTRUE(reckless_active())) "btn btn-danger" else "btn btn-default"
        )))
      }
      if (player_has_feature("detect_undead")) {
        buttons <- c(buttons, list(actionButton(
          session$ns("use_detect_undead"), "Detect Undead", class = "btn btn-default"
        )))
      }
      natural_spells <- Filter(function(spell) {
        identical(as.character(spell$class %||% ""), "Hanianol Sorcerer") &&
          as.integer(spell$level %||% 0L) == 2L
      }, get_unlocked_class_spells(core$state$char))
      if (length(natural_spells)) {
        buttons <- c(buttons, list(actionButton(
          session$ns("open_natural_magic"), "Natural Magic", class = "btn btn-success"
        )))
      }
      if (!length(buttons)) return(NULL)
      tagList(buttons)
    })

    observeEvent(input$open_cunning_action, {
      if (!isTRUE(is_players_turn())) return()
      engaged <- is_currently_engaged()
      showModal(modalDialog(
        title = "Cunning Action",
        p("Choose how to spend your bonus action."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("cunning_dash"), "Dash"),
          actionButton(session$ns("cunning_disengage"), "Disengage", disabled=if(!engaged)"disabled"else NULL),
          actionButton(session$ns("cunning_hide"), "Hide")
        )
      ))
    }, ignoreInit = TRUE)

    use_cunning_action <- function(mode) {
      if (identical(tolower(mode), "disengage") && !is_currently_engaged()) {
        log_safe("Disengage is unavailable because no living enemy currently threatens an adjacent space.")
        return()
      }
      if (identical(tolower(mode), "hide")) return(attempt_hide("bonus_action", "Cunning Action: Hide"))
      if (!spend_action_safe("bonus_action", paste("Cunning Action:", mode))) return()
      cunning_mode(tolower(mode))
      if (identical(tolower(mode), "dash")) {
        movement_dash(TRUE)
        dash_action_spent(TRUE)
        updateCheckboxInput(session, "dash_move", value = TRUE)
      }
      removeModal()
      log_safe(paste0("🗡️ Cunning Action: ", mode, "."))
    }
    observeEvent(input$cunning_dash, use_cunning_action("Dash"), ignoreInit = TRUE)
    observeEvent(input$cunning_disengage, use_cunning_action("Disengage"), ignoreInit = TRUE)
    observeEvent(input$cunning_hide, use_cunning_action("Hide"), ignoreInit = TRUE)

    current_round_number <- function() {
      value <- suppressWarnings(as.integer(combat_tbl()$round_number[1] %||% 1L))
      if (is.na(value) || value < 1L) 1L else value
    }

    apply_combat_condition <- function(condition, target_id, target_type = "player", source = "standard_action",
                                       ends_round = current_round_number() + 1L, payload = list()) {
      target_char <- tryCatch(load_actor_for_combat(target_id, target_type), error = function(e) NULL)
      if (!is.null(target_char)) {
        target_char <- apply_unlocked_class_effects(validate_character(target_char))
        immunities <- unique(c(tolower(as.character(target_char$combat_profile$condition_immunities %||% character())),equipped_magical_traits(target_char)$condition_immunities))
        if (tolower(as.character(condition)) %in% immunities) {
          log_safe(paste0("🛡️ ", get_actor_display_name(target_id), " is immune to ", condition, "."))
          return(FALSE)
        }
      }
      payload$condition <- condition
      created <- create_encounter_effect(
        current_encounter_id(), "player", as.character(core$state$char_id %||% ""),
        source, "condition", payload = payload,
        target_actor_type = target_type, target_actor_id = as.character(target_id),
        starts_round = current_round_number(), ends_round = ends_round
      )
      if (is.data.frame(created) && nrow(created)) bump_refresh()
      is.data.frame(created) && nrow(created) > 0L
    }

    living_adjacent_enemies <- function(actor_id=as.character(core$state$char_id%||%"")) {
      actors<-encounter_actors_tbl();self<-actors[as.character(actors$actor_id)==actor_id,,drop=FALSE]
      if(!nrow(self))return(data.frame())
      enemies<-actors[as.character(actors$actor_type)=="enemy",,drop=FALSE]
      hp_col<-intersect(c("hp_current","current_hp","hp"),names(enemies))
      if(length(hp_col))enemies<-enemies[suppressWarnings(as.numeric(enemies[[hp_col[[1L]]]]))>0,,drop=FALSE]
      if(!nrow(enemies))return(enemies)
      enemies[vapply(seq_len(nrow(enemies)),function(i)is_adjacent_5ft(self$x[[1L]],self$y[[1L]],enemies$x[[i]],enemies$y[[i]]),logical(1)),,drop=FALSE]
    }
    is_currently_engaged <- function() nrow(living_adjacent_enemies())>0L

    enemy_passive_perception <- function(enemy_id) {
      enemy<-tryCatch(load_actor_for_combat(enemy_id,"enemy"),error=function(e)NULL)
      if(is.null(enemy))return(10L)
      explicit<-suppressWarnings(as.integer(enemy$combat_profile$passive_perception%||%NA))
      if(!is.na(explicit))return(explicit)
      wis<-suppressWarnings(as.integer(enemy$abilities$wis%||%enemy$abilities$int%||%10L));if(is.na(wis))wis<-10L
      10L+floor((wis-10L)/2L)
    }
    hide_context <- function(x=NULL,y=NULL) {
      actors<-encounter_actors_tbl();cid<-as.character(core$state$char_id%||%"");self<-actors[as.character(actors$actor_id)==cid,,drop=FALSE]
      if(!nrow(self))return(NULL);if(is.null(x))x<-self$x[[1L]];if(is.null(y))y<-self$y[[1L]]
      tiles<-map_tiles_rv();tile<-get_tile_row(tiles,x,y,map_id());terrain<-if(nrow(tile))as.character(tile$terrain[[1L]]%||%"grass")else"grass";light<-if(nrow(tile))as.character(tile$light[[1L]]%||%"full")else"full"
      neighbours<-tiles[as.integer(tiles$map_id)==as.integer(map_id())&abs(as.integer(tiles$x)-x)<=1L&abs(as.integer(tiles$y)-y)<=1L,,drop=FALSE]
      adjacent_wall<-any(tolower(as.character(neighbours$terrain%||%""))=="wall"|as.logical(neighbours$blocks_vision%||%FALSE),na.rm=TRUE)
      enemies<-actors[as.character(actors$actor_type)=="enemy",,drop=FALSE];hp_col<-intersect(c("hp_current","current_hp","hp"),names(enemies));if(length(hp_col))enemies<-enemies[suppressWarnings(as.numeric(enemies[[hp_col[[1L]]]]))>0,,drop=FALSE]
      los<-if(nrow(enemies))vapply(seq_len(nrow(enemies)),function(i)isTRUE(combat_attack_geometry(tiles,enemies$x[[i]],enemies$y[[i]],x,y,1000L,1000L,map_id())$line_clear),logical(1))else FALSE
      passive<-if(nrow(enemies))max(vapply(as.character(enemies$actor_id),enemy_passive_perception,integer(1)),na.rm=TRUE)else 10L
      list(dc=combat_hide_dc(passive,terrain,light,adjacent_wall,any(los)),terrain=terrain,light=light,adjacent_wall=adjacent_wall,enemy_has_los=any(los),passive=passive)
    }
    attempt_hide <- function(action_type=NULL,label="Hide",movement_recheck=FALSE,x=NULL,y=NULL) {
      if(!isTRUE(movement_recheck)&&!isTRUE(is_players_turn()))return(log_safe("Hide can only be attempted on your turn."))
      context<-hide_context(x,y);if(is.null(context))return(log_safe("Your position could not be assessed for hiding."))
      if(!isTRUE(movement_recheck)&&!spend_action_safe(action_type,label))return(FALSE)
      char<-validate_character(core$state$char);dex_mod<-floor((as.integer(char$abilities$dex%||%10L)-10L)/2L);rank<-as.character(char$prof$skills$stealth%||%"None");pb<-character_proficiency_bonus(char);prof<-if(rank=="Expertise")2L*pb else if(rank=="Proficient")pb else 0L
      roll<-sample.int(20L,1L);total<-roll+dex_mod+prof;cid<-as.character(core$state$char_id);end_encounter_condition(current_encounter_id(),cid,"hidden")
      success<-total>=context$dc
      if(success)apply_combat_condition("hidden",cid,"player",if(movement_recheck)"hidden_movement"else"hide",payload=list(stealth_total=total,hide_dc=context$dc,terrain=context$terrain,light=context$light))else bump_refresh()
      removeModal();log_game_event(current_encounter_id(),"hide_check","player",cid,payload=list(roll=roll,total=total,dc=context$dc,success=success,movement_recheck=movement_recheck,terrain=context$terrain,light=context$light,enemy_has_los=context$enemy_has_los))
      log_safe(paste0(if(success)"🥷 Hidden"else"👁️ Spotted",if(movement_recheck)" after moving"else"",": Stealth ",total," vs DC ",context$dc," (",context$terrain,", ",context$light," light)."));success
    }

    use_standard_action <- function(mode) {
      if (!isTRUE(is_players_turn())) {
        log_safe("⚠️ Standard actions can only be taken on your turn.")
        return()
      }
      mode <- tolower(as.character(mode))
      cid <- as.character(core$state$char_id %||% "")
      if (mode == "dash") {
        movement_dash(TRUE)
        updateCheckboxInput(session, "dash_move", value = TRUE)
        removeModal()
        log_safe("🏃 Dash armed. Your action is spent only if you move beyond normal speed.")
        return()
      }
      if(mode=="disengage"&&!is_currently_engaged()){log_safe("Disengage is unavailable because no living enemy currently threatens an adjacent space.");return()}
      if(mode=="hide")return(attempt_hide("action","Hide"))
      if (!spend_action_safe("action", tools::toTitleCase(mode))) return()
      if (mode == "disengage") cunning_mode("disengage")
      if (mode == "dodge") apply_combat_condition("dodging", cid, "player", "dodge")
      removeModal()
      log_game_event(current_encounter_id(), "standard_action", "player", cid,
                     payload = list(action = mode))
      log_safe(paste0("⚔️ ", tools::toTitleCase(mode), " action used."))
    }

    observeEvent(input$open_standard_actions, {
      engaged<-is_currently_engaged()
      showModal(modalDialog(
        title = "Combat Actions",
        p("Choose an action. Select a map target first for Help or Grapple."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("standard_dash"), "Dash"),
          actionButton(session$ns("standard_disengage"), "Disengage",disabled=if(!engaged)"disabled"else NULL),
          actionButton(session$ns("standard_hide"), "Hide"),
          actionButton(session$ns("standard_dodge"), "Dodge"),
          actionButton(session$ns("standard_help"), "Help"),
          actionButton(session$ns("standard_grapple"), "Grapple"),
          actionButton(session$ns("standard_escape_grapple"), "Escape Grapple"),
          actionButton(session$ns("standard_ready"), "Ready")
        ), easyClose = TRUE
      ))
    }, ignoreInit = TRUE)
    observeEvent(input$standard_dash, use_standard_action("dash"), ignoreInit = TRUE)
    observeEvent(input$standard_disengage, use_standard_action("disengage"), ignoreInit = TRUE)
    observeEvent(input$standard_hide, use_standard_action("hide"), ignoreInit = TRUE)
    observeEvent(input$standard_dodge, use_standard_action("dodge"), ignoreInit = TRUE)

    observeEvent(input$standard_help, {
      target_id <- as.character(selected_target_id() %||% "")
      target_type <- get_actor_type_by_id(target_id)
      actors <- encounter_actors_tbl()
      self <- actors[as.character(actors$actor_id) == as.character(core$state$char_id %||% ""), , drop = FALSE]
      target <- actors[as.character(actors$actor_id) == target_id, , drop = FALSE]
      adjacent <- nrow(self) && nrow(target) && is_adjacent_5ft(self$x[1], self$y[1], target$x[1], target$y[1])
      if (!nzchar(target_id) || identical(target_type, "player") || !isTRUE(adjacent)) {
        log_safe("⚠️ Combat Help requires a selected adjacent enemy to distract.")
        return()
      }
      if (!spend_action_safe("action", "Help")) return()
      apply_combat_condition("helped_against", target_id, target_type, "help")
      removeModal()
      log_safe(paste0("🤝 ", get_actor_display_name(target_id), " is distracted; the next allied attack has advantage."))
    }, ignoreInit = TRUE)

    observeEvent(input$standard_grapple, {
      target_id <- as.character(selected_target_id() %||% "")
      target_type <- get_actor_type_by_id(target_id)
      actors <- encounter_actors_tbl()
      self <- actors[as.character(actors$actor_id) == as.character(core$state$char_id %||% ""), , drop = FALSE]
      target <- actors[as.character(actors$actor_id) == target_id, , drop = FALSE]
      adjacent <- nrow(self) && nrow(target) && is_adjacent_5ft(self$x[1], self$y[1], target$x[1], target$y[1])
      if (!nzchar(target_id) || identical(target_type, "player") || !isTRUE(adjacent)) {
        log_safe("⚠️ Grapple requires a selected adjacent enemy.")
        return()
      }
      if (!spend_attack_safe(core$state$char, "Grapple")) return()
      str_mod <- floor((as.integer(core$state$char$abilities$str %||% 10L) - 10L) / 2L)
      proficiency <- character_proficiency_bonus(core$state$char)
      attacker_total <- sample.int(20L, 1L) + str_mod + proficiency
      defender_total <- sample.int(20L, 1L) + 2L
      success <- attacker_total >= defender_total
      if (success) apply_combat_condition("grappled", target_id, target_type, "grapple", ends_round = NULL,
                                          payload = list(grappler_id = as.character(core$state$char_id)))
      removeModal()
      log_safe(paste0(if (success) "🤼 Grapple succeeds" else "⚠️ Grapple fails",
                      " (", attacker_total, " vs ", defender_total, ")."))
    }, ignoreInit = TRUE)

    observeEvent(input$standard_escape_grapple, {
      cid <- as.character(core$state$char_id %||% "")
      if (!"grappled" %in% actor_conditions(cid)) {
        log_safe("⚠️ You are not grappled.")
        return()
      }
      if (!spend_action_safe("action", "Escape Grapple")) return()
      str_mod <- floor((as.integer(core$state$char$abilities$str %||% 10L) - 10L) / 2L)
      dex_mod <- floor((as.integer(core$state$char$abilities$dex %||% 10L) - 10L) / 2L)
      escape_total <- sample.int(20L, 1L) + max(str_mod, dex_mod) + character_proficiency_bonus(core$state$char)
      hold_total <- sample.int(20L, 1L) + 2L
      escaped <- escape_total >= hold_total && end_encounter_condition(current_encounter_id(), cid, "grappled")
      removeModal()
      if (escaped) bump_refresh()
      log_safe(paste0(if (escaped) "🤼 You escape the grapple" else "⚠️ The grapple holds",
                      " (", escape_total, " vs ", hold_total, ")."))
    }, ignoreInit = TRUE)

    observeEvent(input$standard_ready, {
      showModal(modalDialog(
        title = "Ready an Action",
        textInput(session$ns("ready_trigger"), "Trigger", placeholder = "When the bandit enters the doorway…"),
        textInput(session$ns("ready_response"), "Response", placeholder = "…I attack with my sword."),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_ready"), "Ready Action"))
      ))
    }, ignoreInit = TRUE)
    observeEvent(input$confirm_ready, {
      trigger <- trimws(as.character(input$ready_trigger %||% ""))
      response <- trimws(as.character(input$ready_response %||% ""))
      if (!nzchar(trigger) || !nzchar(response)) {
        log_safe("⚠️ A readied action needs both a trigger and response.")
        return()
      }
      if (!spend_action_safe("action", "Ready")) return()
      apply_combat_condition("readied", as.character(core$state$char_id), "player", "ready")
      log_game_event(current_encounter_id(), "ready", "player", as.character(core$state$char_id),
                     payload = list(trigger = trigger, response = response))
      removeModal()
      log_safe(paste0("⏱️ Readied: ", trigger, " → ", response, " (uses your reaction when triggered)."))
    }, ignoreInit = TRUE)

    observeEvent(input$use_action_surge, {
      if (!isTRUE(is_players_turn())) return()
      char <- validate_character(core$state$char)
      action <- list(usage = list(key = "action_surge", recharge = "short_rest"))
      if (!class_action_use_available(char, action)) {
        log_safe("⚠️ Action Surge has already been used and needs a rest.")
        return()
      }
      core$state$char <- mark_class_action_used(char, action)
      turn_budget(grant_turn_action(turn_budget(), 1L))
      log_safe("⚡ Action Surge grants one additional action.")
    }, ignoreInit = TRUE)

    observeEvent(input$toggle_reckless, {
      if (!isTRUE(is_players_turn())) return()
      reckless_active(!isTRUE(reckless_active()))
      log_safe(if (isTRUE(reckless_active())) "🔥 Reckless Attack enabled." else "Reckless Attack cancelled.")
    }, ignoreInit = TRUE)

    observeEvent(input$use_detect_undead, {
      if (!isTRUE(is_players_turn())) return()
      char <- validate_character(core$state$char)
      current <- suppressWarnings(as.integer(char$resources$sindre$cur %||% 0L))
      if (is.na(current)) current <- 0L
      if (current < 10L) {
        log_safe("⚠️ Not enough Sindre for Detect Undead.")
        return()
      }
      if (!spend_action_safe("action", "Detect Undead")) return()
      char$resources$sindre$cur <- current - 10L
      core$state$char <- char

      enemies <- snapshot_data()$enemies %||% data.frame()
      found <- character()
      if (is.data.frame(enemies) && nrow(enemies)) {
        text <- tolower(paste(
          enemies$name %||% "", enemies$creature_type %||% "",
          enemies$template_key %||% "", enemies$notes %||% ""
        ))
        undead_words <- "undead|skeleton|zombie|ghoul|ghost|wight|wraith|vampire|lich|revenant"
        undead_rows <- enemies[grepl(undead_words, text, perl = TRUE), , drop = FALSE]
        if (nrow(undead_rows)) {
          actors <- encounter_actors_tbl()
          self_row <- actors[as.character(actors$actor_id) == as.character(core$state$char_id %||% ""), , drop = FALSE]
          in_range <- rep(TRUE, nrow(undead_rows))
          if (nrow(self_row) && all(c("x", "y") %in% names(actors))) {
            enemy_ids <- as.character(undead_rows$enemy_uuid %||% undead_rows$actor_id %||% "")
            in_range <- vapply(enemy_ids, function(enemy_id) {
              row <- actors[as.character(actors$actor_id) == enemy_id, , drop = FALSE]
              if (!nrow(row)) return(TRUE)
              dx <- abs(as.integer(row$x[1]) - as.integer(self_row$x[1]))
              dy <- abs(as.integer(row$y[1]) - as.integer(self_row$y[1]))
              max(dx, dy, na.rm = TRUE) <= 12L
            }, logical(1))
          }
          found <- as.character(undead_rows$name[in_range] %||% character())
        }
      }
      message <- if (length(found)) {
        paste0("💀 Undead sensed within the encounter: ", paste(unique(found), collapse = ", "), ".")
      } else {
        "💀 No undead presence is sensed within 60 feet."
      }
      log_safe(message)
      log_game_event(
        encounter_id = current_encounter_id(), event_type = "ability",
        actor_type = "player", actor_id = as.character(core$state$char_id %||% ""),
        payload = list(ability_name = "Detect Undead", detected = unique(found), resource_cost = 10L)
      )
    }, ignoreInit = TRUE)

    rage_is_active <- reactive({
      isTRUE(core$state$char$status$raging %||% FALSE)
    })

    selected_totem <- reactive({
      character_level_choice(core$state$char, "Barbarian", 3L, "spirit_totem")
    })

    selected_manoeuvres <- reactive({
      choices <- core$state$char$build$level_choices$Fighter[["3"]] %||% list()
      unique(unname(as.character(unlist(
        choices[grepl("^battle_master_manoeuvre_", names(choices))]
      ))))
    })

    output$level_three_actions_ui <- renderUI({
      char <- validate_character(core$state$char)
      buttons <- list()
      if (player_has_feature("rage")) {
        max_uses <- barbarian_rage_maximum(char)
        remaining <- class_resource_remaining(char, "rage", max_uses)
        buttons <- c(buttons, list(actionButton(
          session$ns("toggle_rage"),
          if (isTRUE(rage_is_active())) "End Rage" else paste0("Rage (", remaining, ")"),
          class = if (isTRUE(rage_is_active())) "btn btn-danger" else "btn btn-default"
        )))
      }
      if (player_has_feature("frenzy") && isTRUE(rage_is_active())) {
        buttons <- c(buttons, list(actionButton(session$ns("use_frenzy"), "Frenzy Attack", class = "btn btn-danger")))
      }
      if (player_has_feature("combat_superiority")) {
        remaining <- class_resource_remaining(char, "superiority_dice", 4L)
        buttons <- c(buttons, list(actionButton(
          session$ns("open_manoeuvre"), paste0("Manoeuvre d8 (", remaining, ")"), class = "btn btn-default"
        )))
      }
      if (identical(selected_totem(), "Eagle") && isTRUE(rage_is_active())) {
        buttons <- c(buttons, list(actionButton(session$ns("totem_eagle_dash"), "Eagle Dash", class = "btn btn-default")))
      }
      if (player_has_feature("wild_insight")) {
        buttons <- c(buttons, list(actionButton(session$ns("wild_insight"), "Wild Insight", class = "btn btn-default")))
      }
      if (player_has_feature("fast_hands")) {
        buttons <- c(buttons, list(actionButton(session$ns("fast_hands"), "Fast Hands", class = "btn btn-default")))
      }
      if (player_has_feature("predator")) {
        action <- list(usage = list(key = "predator", recharge = "short_rest"))
        buttons <- c(buttons, list(actionButton(
          session$ns("use_predator"), "Predator",
          class = "btn btn-danger", disabled = if (!class_action_use_available(char, action)) "disabled" else NULL
        )))
      }
      if (player_has_feature("mislead")) {
        buttons <- c(buttons, list(actionButton(session$ns("use_mislead"), "Mislead (25 Sindre)", class = "btn btn-default")))
      }
      if (!length(buttons)) return(NULL)
      tagList(buttons)
    })

    observeEvent(input$toggle_rage, {
      if (!isTRUE(is_players_turn())) return()
      char <- validate_character(core$state$char)
      char$status <- char$status %||% list()
      if (isTRUE(char$status$raging %||% FALSE)) {
        char$status$raging <- FALSE
        core$state$char <- char
        log_safe("🧘 Rage ended.")
        return()
      }
      maximum <- barbarian_rage_maximum(char)
      updated <- spend_class_resource(char, "rage", maximum, "long_rest")
      if (is.null(updated)) {
        log_safe("⚠️ No Rage uses remain. Take a long rest to recover them.")
        return()
      }
      if (!spend_action_safe("bonus_action", "Rage")) return()
      updated$status <- updated$status %||% list()
      updated$status$raging <- TRUE
      core$state$char <- updated
      log_safe("🔥 Rage begins: +2 Strength weapon damage and physical resistance.")
    }, ignoreInit = TRUE)

    observeEvent(input$use_frenzy, {
      if (!isTRUE(is_players_turn()) || !isTRUE(rage_is_active())) return()
      if (!spend_action_safe("bonus_action", "Frenzy Attack")) return()
      turn_budget(grant_bonus_attack(turn_budget(), 1L))
      log_safe("🔥 Frenzy grants one additional weapon attack this turn.")
    }, ignoreInit = TRUE)

    observeEvent(input$totem_eagle_dash, {
      if (!isTRUE(is_players_turn()) || !isTRUE(rage_is_active())) return()
      if (!spend_action_safe("bonus_action", "Eagle Totem Dash")) return()
      movement_dash(TRUE)
      dash_action_spent(TRUE)
      updateCheckboxInput(session, "dash_move", value = TRUE)
      log_safe("🦅 Eagle Totem: Dash activated as a bonus action.")
    }, ignoreInit = TRUE)

    observeEvent(input$open_manoeuvre, {
      if (!isTRUE(is_players_turn())) return()
      manoeuvres <- selected_manoeuvres()
      if (!length(manoeuvres)) {
        log_safe("⚠️ No Battle Master manoeuvres have been selected in the Level tab.")
        return()
      }
      showModal(modalDialog(
        title = "Combat Superiority",
        selectInput(session$ns("manoeuvre_choice"), "Manoeuvre", choices = manoeuvres),
        p("The superiority die applies to your next attack this turn."),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_manoeuvre"), "Ready Manoeuvre", class = "btn btn-primary"))
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_manoeuvre, {
      char <- validate_character(core$state$char)
      updated <- spend_class_resource(char, "superiority_dice", 4L, "short_rest")
      if (is.null(updated)) {
        removeModal()
        log_safe("⚠️ No superiority dice remain.")
        return()
      }
      selected <- as.character(input$manoeuvre_choice %||% "")
      if (!selected %in% selected_manoeuvres()) return()
      core$state$char <- updated
      manoeuvre_active(selected)
      removeModal()
      log_safe(paste0("⚔️ ", selected, " readied for the next attack."))
    }, ignoreInit = TRUE)

    observeEvent(input$wild_insight, {
      if (!isTRUE(is_players_turn())) return()
      if (!spend_action_safe("bonus_action", "Wild Insight")) return()
      roll <- sample.int(100L, 1L)
      log_safe(paste0("🔮 Wild Insight rolls ", roll, " on the Wild Magic table. Resolve that numbered result in Magic."))
    }, ignoreInit = TRUE)

    observeEvent(input$fast_hands, {
      if (!isTRUE(is_players_turn())) return()
      showModal(modalDialog(
        title = "Fast Hands",
        selectInput(session$ns("fast_hands_choice"), "Bonus action", choices = c(
          "Use an Object", "Sleight of Hand", "Disarm a simple trap", "Open a lock"
        )),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_fast_hands"), "Use Bonus Action", class = "btn btn-primary"))
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_fast_hands, {
      choice <- as.character(input$fast_hands_choice %||% "Use an Object")
      if (!spend_action_safe("bonus_action", paste("Fast Hands:", choice))) return()
      removeModal()
      log_safe(paste0("🖐️ Fast Hands: ", choice, ". Resolve the selected object, tool or check."))
    }, ignoreInit = TRUE)

    observeEvent(input$use_predator, {
      if (!isTRUE(is_players_turn())) return()
      char <- validate_character(core$state$char)
      action <- list(usage = list(key = "predator", recharge = "short_rest"))
      if (!class_action_use_available(char, action) || !spend_action_safe("action", "Predator")) return()
      actors <- encounter_actors_tbl()
      caster_id <- as.character(core$state$char_id %||% "")
      caster <- actors[as.character(actors$actor_id) == caster_id, , drop = FALSE]
      enemies <- snapshot_data()$enemies %||% data.frame()
      dc <- 8L + character_proficiency_bonus(char) + floor((as.integer(char$abilities$bld_str %||% 10L) - 10L) / 2L)
      frightened <- character()
      if (nrow(caster)) for (i in seq_len(nrow(actors))) {
        actor <- actors[i, , drop = FALSE]
        if (!identical(as.character(actor$actor_type[1]), "enemy")) next
        distance <- max(abs(as.integer(actor$x[1]) - as.integer(caster$x[1])),
                        abs(as.integer(actor$y[1]) - as.integer(caster$y[1]))) * 5L
        if (is.na(distance) || distance > 15L) next
        enemy_id <- as.character(actor$actor_id[1])
        enemy <- enemies[as.character(enemies$enemy_uuid %||% "") == enemy_id, , drop = FALSE]
        save_mod <- if (nrow(enemy) && "wis_save" %in% names(enemy)) as.integer(enemy$wis_save[1] %||% 0L) else 0L
        if (sample.int(20L, 1L) + save_mod < dc) {
          round_number <- as.integer(combat_tbl()$round_number[1] %||% 1L)
          create_encounter_effect(
            current_encounter_id(), "player", caster_id, "predator", "condition",
            payload = list(condition = "frightened", save_dc = dc),
            target_actor_type = "enemy", target_actor_id = enemy_id,
            starts_round = round_number, ends_round = round_number + 1L
          )
          frightened <- c(frightened, as.character(actor$display_name[1] %||% enemy_id))
        }
      }
      core$state$char <- mark_class_action_used(char, action)
      log_safe(if (length(frightened)) paste0("🐺 Predator frightens: ", paste(frightened, collapse = ", "), ".") else "🐺 Predator: no nearby enemy was frightened.")
      bump_refresh()
    }, ignoreInit = TRUE)

    observeEvent(input$use_mislead, {
      showModal(modalDialog(
        title = "Cast Mislead?",
        p("Cost: 1 action and 25 Sindre. Creates a controllable illusory double and requires concentration."),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_mislead"), "Cast Mislead", class = "btn btn-primary"))
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_mislead, {
      removeModal()
      if (!isTRUE(is_players_turn())) return()
      char <- validate_character(core$state$char)
      sindre <- as.integer(char$resources$sindre$cur %||% 0L)
      if (sindre < 25L) {
        log_safe("⚠️ Not enough Sindre for Mislead.")
        return()
      }
      if (!spend_action_safe("action", "Mislead")) return()
      round_number <- as.integer(combat_tbl()$round_number[1] %||% 1L)
      caster_id <- as.character(core$state$char_id %||% "")
      actors <- encounter_actors_tbl()
      caster <- actors[as.character(actors$actor_id) == caster_id, , drop = FALSE]
      if (!nrow(caster)) {
        log_safe("⚠️ Mislead needs your character placed on the map.")
        return()
      }
      illusion <- create_encounter_summon(
        current_encounter_id(), caster_id, paste0(char$meta$name %||% "Player", "'s Double"),
        max_cr = "illusion", hp_max = 1L, ac = 10L, movement_speed = base_speed_ft(),
        expires_round = round_number + 10L
      )
      if (is.null(illusion) || !nrow(illusion)) {
        log_safe("⚠️ Mislead could not create its double.")
        return()
      }
      illusion_id <- as.character(illusion$summon_uuid[1])
      upsert_encounter_actor_position(current_encounter_id(), "summon", illusion_id,
                                      as.integer(caster$x[1]), as.integer(caster$y[1]))
      set_actor_turn_order(current_encounter_id(), illusion_id, "summon",
                           as.integer(caster$initiative[1] %||% 0L), as.integer(caster$turn_order[1] %||% 1L))
      created <- create_encounter_effect(
        current_encounter_id(), "player", caster_id, "mislead", "summon",
        payload = list(illusory_double = TRUE, double_id = illusion_id),
        target_actor_type = "summon", target_actor_id = illusion_id,
        starts_round = round_number, ends_round = round_number + 10L, concentration = TRUE
      )
      if (is.null(created)) {
        log_safe("⚠️ Mislead could not be stored.")
        return()
      }
      char$resources$sindre$cur <- sindre - 25L
      core$state$char <- char
      apply_combat_condition("invisible", caster_id, "player", "mislead_invisibility",
                             ends_round = round_number + 10L)
      log_safe("🪞 Mislead creates a controllable double on your initiative; you become invisible while concentrating.")
      bump_refresh()
    }, ignoreInit = TRUE)

    natural_magic_spells <- reactive({
      spells <- get_unlocked_class_spells(core$state$char)
      Filter(function(spell) {
        identical(as.character(spell$class %||% ""), "Hanianol Sorcerer") &&
          as.integer(spell$level %||% 0L) == 2L
      }, spells)
    })

    at_mandred_convergence <- reactive({
      if (!player_has_feature("seer")) return(FALSE)
      actors <- encounter_actors_tbl()
      caster_id <- as.character(core$state$char_id %||% "")
      caster <- actors[as.character(actors$actor_id) == caster_id, , drop = FALSE]
      if (!nrow(caster)) return(FALSE)
      tile <- get_tile_row(map_tiles_rv(), as.integer(caster$x[1]), as.integer(caster$y[1]), map_id())
      nrow(tile) > 0L && identical(tolower(as.character(tile$terrain[1] %||% "")), "mandred_convergence")
    })

    observeEvent(input$open_natural_magic, {
      if (!isTRUE(is_players_turn())) return()
      spells <- natural_magic_spells()
      if (!length(spells)) return()
      choices <- stats::setNames(names(spells), vapply(spells, function(spell) spell$name, character(1)))
      showModal(modalDialog(
        title = "Cast Natural Magic",
        selectInput(session$ns("natural_spell_id"), "Spell", choices = choices),
        uiOutput(session$ns("natural_spell_preview_ui")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            session$ns("cast_natural_spell"),
            if (isTRUE(at_mandred_convergence())) "Cast — 10 Sindre (Convergence)" else "Cast — 20 Sindre",
            class = "btn btn-success"
          )
        ), easyClose = TRUE
      ))
    }, ignoreInit = TRUE)

    output$natural_spell_preview_ui <- renderUI({
      spell <- natural_magic_spells()[[as.character(input$natural_spell_id %||% "")]]
      if (is.null(spell)) return(NULL)
      div(class = "confirm-box", tags$strong(spell$name), tags$p(spell$description),
          tags$p(tags$strong("Concentration: "), if (isTRUE(spell$concentration)) "Yes" else "No"))
    })

    observeEvent(input$cast_natural_spell, {
      if (!isTRUE(is_players_turn())) return()
      spell_id <- as.character(input$natural_spell_id %||% "")
      spell <- natural_magic_spells()[[spell_id]]
      if (is.null(spell)) return()
      char <- validate_character(core$state$char)
      sindre <- suppressWarnings(as.integer(char$resources$sindre$cur %||% 0L))
      if (is.na(sindre)) sindre <- 0L
      spell_cost <- as.integer(spell$cost %||% 20L)
      convergence <- isTRUE(at_mandred_convergence())
      if (convergence) spell_cost <- max(1L, ceiling(spell_cost / 2))
      if (sindre < spell_cost) {
        log_safe("⚠️ Not enough Sindre for Natural Magic.")
        return()
      }
      classes <- normalise_character_classes(char)
      hanianol_level <- sum(vapply(classes, function(entry) {
        if (identical(as.character(entry$class %||% ""), "Hanianol Sorcerer")) as.integer(entry$level %||% 0L) else 0L
      }, integer(1)))
      spell <- scale_class_spell(spell, hanianol_level)
      eid <- current_encounter_id()
      round_number <- as.integer(combat_tbl()$round_number[1] %||% 1L)
      caster_id <- as.character(core$state$char_id %||% "")
      actors <- encounter_actors_tbl()
      caster <- actors[as.character(actors$actor_id) == caster_id, , drop = FALSE]
      center_x <- if (nrow(caster)) as.integer(caster$x[1] %||% NA) else NA_integer_
      center_y <- if (nrow(caster)) as.integer(caster$y[1] %||% NA) else NA_integer_

      if (spell_id %in% c("grasping_vines", "calling_rain")) {
        target_id <- as.character(selected_target_id() %||% "")
        target <- actors[as.character(actors$actor_id) == target_id, , drop = FALSE]
        if (!nrow(target) || is.na(as.integer(target$x[1])) || is.na(as.integer(target$y[1]))) {
          log_safe("⚠️ Select an actor on the map to centre this area spell, then cast again.")
          return()
        }
        center_x <- as.integer(target$x[1]); center_y <- as.integer(target$y[1])
      }

      if (!spend_action_safe("action", spell$name)) return()

      end_actor_concentration(eid, caster_id)
      radius <- as.integer(spell$target$size_ft %||% 0L)
      duration_rounds <- if (identical(spell$duration, "1_hour")) 600L else if (identical(spell$duration, "30_minutes")) 300L else 10L
      payload <- list(
        name = spell$name, description = spell$description,
        effects = spell$effects %||% list(), upgrade = spell$resolved_upgrade %||% list(),
        save_dc = class_spell_save_dc(char, spell)
      )

      created <- NULL
      if (identical(spell_id, "call_beast")) {
        max_cr <- if (hanianol_level >= 15L) "2" else if (hanianol_level >= 11L) "1" else "1/2"
        beast_stats <- if (hanianol_level >= 15L) c(hp = 35L, ac = 14L) else if (hanianol_level >= 11L) c(hp = 22L, ac = 13L) else c(hp = 12L, ac = 12L)
        summon <- create_encounter_summon(
          eid, caster_id, paste0(core$state$char$meta$name %||% "Hanianol", "'s Beast"),
          max_cr = max_cr, hp_max = beast_stats[["hp"]], ac = beast_stats[["ac"]],
          expires_round = round_number + duration_rounds
        )
        if (!is.null(summon) && nrow(summon)) {
          upsert_encounter_actor_position(eid, "summon", as.character(summon$summon_uuid[1]), center_x, center_y)
          created <- create_encounter_effect(
            eid, "player", caster_id, spell_id, "summon", payload,
            target_actor_type = "summon", target_actor_id = as.character(summon$summon_uuid[1]),
            starts_round = round_number, ends_round = round_number + duration_rounds,
            concentration = TRUE
          )
        }
      } else {
        created <- create_encounter_effect(
          eid, "player", caster_id, spell_id,
          if (identical(spell_id, "wasting_sickness")) "condition_aura" else "area",
          payload, center_x = center_x, center_y = center_y, radius_ft = radius,
          starts_round = round_number, ends_round = round_number + duration_rounds,
          concentration = TRUE
        )
      }

      if (is.null(created)) {
        log_safe("⚠️ Natural Magic could not be stored. Apply database migration 002 first.")
        return()
      }

      affected_names <- character()
      if (spell_id %in% c("grasping_vines", "wasting_sickness")) {
        enemies <- snapshot_data()$enemies %||% data.frame()
        affected_actors <- if (identical(spell_id, "wasting_sickness")) {
          actors[as.character(actors$actor_id) != caster_id, , drop = FALSE]
        } else {
          actors[as.character(actors$actor_type) == "enemy", , drop = FALSE]
        }
        save_ability <- as.character(spell$resolution$ability %||% "con")
        save_dc <- class_spell_save_dc(char, spell)
        condition <- if (identical(spell_id, "grasping_vines")) "restrained" else "poisoned"
        for (i in seq_len(nrow(affected_actors))) {
          actor <- affected_actors[i, , drop = FALSE]
          dx <- abs(as.integer(actor$x[1]) - center_x)
          dy <- abs(as.integer(actor$y[1]) - center_y)
          if (is.na(dx) || is.na(dy) || max(dx, dy) * 5L > radius) next
          target_id <- as.character(actor$actor_id[1])
          target_type <- as.character(actor$actor_type[1] %||% "enemy")
          save_mod <- 0L
          if (identical(target_type, "player")) {
            target_char <- tryCatch(load_character_from_db(target_id), error = function(e) NULL)
            if (!is.null(target_char)) {
              save_mod <- get_character_ability_mod(target_char, save_ability)
              if (isTRUE(target_char$prof$saves[[save_ability]] %||% FALSE)) {
                save_mod <- save_mod + get_character_prof_bonus(target_char)
              }
            }
          } else if (identical(target_type, "enemy")) {
            enemy <- enemies[as.character(enemies$enemy_uuid %||% "") == target_id, , drop = FALSE]
            if (nrow(enemy)) {
              abilities <- decode_effect_payload(enemy$abilities[[1]] %||% list())
              score <- suppressWarnings(as.integer(abilities[[save_ability]] %||% 10L))
              if (!is.na(score)) save_mod <- floor((score - 10L) / 2L)
            }
          }
          save_disadvantage <- convergence || isTRUE(spell$resolved_upgrade$save_disadvantage %||% FALSE)
          rolls <- sample.int(20L, if (save_disadvantage) 2L else 1L)
          save_roll <- if (length(rolls) > 1L) min(rolls) else rolls[[1L]]
          if (save_roll + save_mod < save_dc) {
            create_encounter_effect(
              eid, "player", caster_id, spell_id, "condition",
              payload = list(condition = condition, save_ability = save_ability,
                             save_dc = save_dc, repeat_save = spell$resolved_upgrade$repeat_save %||%
                               spell$effects[[length(spell$effects)]]$repeat_save %||% "end_of_turn"),
              target_actor_type = target_type, target_actor_id = target_id,
              starts_round = round_number, ends_round = round_number + 10L,
              concentration = TRUE
            )
            affected_names <- c(affected_names, as.character(actor$display_name[1]))
          }
        }
      }
      char$resources$sindre$cur <- sindre - spell_cost
      core$state$char <- char
      log_game_event(
        encounter_id = eid, event_type = "spell", actor_type = "player", actor_id = caster_id,
        payload = list(spell_id = spell_id, spell_name = spell$name, level = hanianol_level,
                       center_x = center_x, center_y = center_y, radius_ft = radius,
                       resource_cost = spell_cost, convergence = convergence)
      )
      removeModal()
      log_safe(paste0(
        "🌿 ", spell$name, " is now active.",
        if (length(affected_names)) paste0(" Affected: ", paste(affected_names, collapse = ", "), ".") else ""
      ))
      bump_refresh()
    }, ignoreInit = TRUE)
    
    
   
    
    is_heart_eater <- reactive({
      char <- isolate(core$state$char)
      
      if (is.null(char)) return(FALSE)
      
      path_txt <- tryCatch(
        tolower(as.character(char$build$path %||% "")),
        error = function(e) ""
      )
      
      grepl("heart eater", path_txt, fixed = TRUE)
    })
    
    can_pass_through_tile <- function(tile, phase = FALSE) {
      if (isTRUE(phase) && isTRUE(is_heart_eater())) return(TRUE)
      !isTRUE(tile$blocks_movement)
    }
    
    can_land_on_tile <- function(tile) {
      !nzchar(as.character(tile$occupant_id %||% ""))
    }
    
    base_speed_ft <- function() {
      
      speed_ft <- suppressWarnings(as.integer(
        
        core$speed %||%
          
          core$state$speed %||%
          
          core$state$char$combat$speed_ft %||%
          
          core$state$char$combat_profile$speed_ft %||%
          
          30L
        
      ))
      
      if (length(speed_ft) < 1 || is.na(speed_ft) || speed_ft < 0L) speed_ft <- 30L
      char <- core$state$char
      if (!is.null(char) && player_has_feature("fast_movement")) {
        items <- char$inventory$items %||% data.frame()
        heavy <- FALSE
        if (is.data.frame(items) && nrow(items) && all(c("type", "equipped") %in% names(items))) {
          armour <- items[as.character(items$type) == "armor" & as.logical(items$equipped), , drop = FALSE]
          if (nrow(armour) && "meta" %in% names(armour)) {
            heavy <- any(vapply(armour$meta, function(meta) {
              identical(tolower(as.character((meta %||% list())$type %||% "")), "heavy")
            }, logical(1)))
          }
        }
        if (!heavy) speed_ft <- speed_ft + 10L
      }
      as.integer(speed_ft)
      
    }
    
    movement_allowance_ft <- function() {
      movement_actor<-if(isTRUE(is_exploration_phase()))as.character(core$state$char_id%||%"")else active_actor_id()
      if (any(c("restrained", "grappled") %in% actor_conditions(movement_actor))) return(0L)
      
      base <- base_speed_ft()
      
      if (isTRUE(movement_dash())) {
        
        base * 2L
        
      } else {
        
        base
        
      }
      
    }
    
    can_use_phase <- function() {
      !is.null(core$state$char) && (isTRUE(is_heart_eater()) || player_has_feature("second_story_work"))
    }
    
    known_ac_rv <- reactiveVal(data.frame(
      target_id = character(),
      lower = integer(),
      upper = integer(),
      stringsAsFactors = FALSE
    ))
    
    map_id <- reactiveVal(1L)
    
    map_fullscreen <- reactiveVal(FALSE)
    map_true3d_mode <- reactiveVal(TRUE)
    
    map_tiles_rv <- reactive({
      mid <- map_id()
      if (is.null(mid) || is.na(mid) || mid < 1) {
        return(data.frame())
      }
      
      tiles <- tryCatch(
        get_map_tiles(mid),
        error = function(e) data.frame()
      )
      
      if (!is.data.frame(tiles)) data.frame() else tiles
    })
    
    model_profile_cache <- new.env(parent = emptyenv())
    add_3d_models_to_render_df <- function(render_df) {
      if (!is.data.frame(render_df) || nrow(render_df) == 0) return(render_df)
      
      model_cols <- c(
        "model_base",
        "model_hair",
        "model_body",
        "model_arms",
        "model_legs",
        "model_feet",
        "model_headgear",
        "model_accessory",
        "hair_color"
      )
      
      for (col in model_cols) {
        if (!col %in% names(render_df)) {
          render_df[[col]] <- rep("", nrow(render_df))
        }
      }
      
      safe_chr1 <- function(x, default = "") {
        if (is.null(x)) return(default)
        x <- unlist(x, use.names = FALSE)
        if (length(x) < 1) return(default)
        x <- as.character(x[1])
        if (is.na(x)) default else x
      }
      
      player_rows <- which(
        !is.na(render_df$occupant_id) &
          nzchar(as.character(render_df$occupant_id)) &
          as.character(render_df$occupant_type) == "player"
      )
      
      for (i in player_rows) {
        actor_id <- as.character(render_df$occupant_id[i])
        
        char_obj <- if (exists(actor_id, envir=model_profile_cache, inherits=FALSE)) get(actor_id, envir=model_profile_cache) else {
          loaded <- if (identical(actor_id, as.character(core$state$char_id %||% ""))) core$state$char else tryCatch(load_character_from_db(actor_id), error = function(e) NULL)
          assign(actor_id, loaded, envir=model_profile_cache); loaded
        }
        
        if (is.null(char_obj)) next
        
        char_obj <- validate_character(char_obj)
        char3d <- char_obj$character_3d %||% list()
        
        render_df$model_base[i]      <- safe_chr1(char3d$base_model)
        render_df$model_hair[i]      <- safe_chr1(char3d$hair_model)
        render_df$model_body[i]      <- safe_chr1(char3d$body_model)
        render_df$model_arms[i]      <- safe_chr1(char3d$arms_model)
        render_df$model_legs[i]      <- safe_chr1(char3d$legs_model)
        render_df$model_feet[i]      <- safe_chr1(char3d$feet_model)
        render_df$model_headgear[i]  <- safe_chr1(char3d$headgear_model)
        render_df$model_accessory[i] <- safe_chr1(char3d$accessory_model)
        render_df$hair_color[i]      <- safe_chr1(char3d$hair_color, "#3b2416")
      }
      
      render_df
    }
    
   
    
    map_occupants_rv <- reactive({
      pos <- positions_tbl()
      eid <- current_encounter_id()
      mid <- map_id()
      
      new_occ <- empty_map_occupants()
      
      if (is.na(eid) || !is.data.frame(pos) || nrow(pos) == 0) {
        return(new_occ)
      }
      
      for (i in seq_len(nrow(pos))) {
        actor_id <- as.character(pos$actor_id[i] %||% "")
        actor_type <- as.character(pos$actor_type[i] %||% "player")
        x <- suppressWarnings(as.integer(pos$x[i] %||% NA))
        y <- suppressWarnings(as.integer(pos$y[i] %||% NA))
        
        if (!nzchar(actor_id) || is.na(x) || is.na(y)) next
        
        new_occ <- set_actor_position_local(
          occupants = new_occ,
          actor_id = actor_id,
          x = x,
          y = y,
          map_id = mid,
          encounter_id = eid,
          actor_type = actor_type
        )
      }
      
      new_occ
    })
    

    
    log_safe <- function(msg) {
      if (is.function(add_log)) {
        try(add_log(msg, toast = TRUE), silent = TRUE)
      } else {
        message(msg)
      }
    }
    is_exploration_phase <- reactive({combat<-combat_tbl();is.data.frame(combat)&&nrow(combat)>0L&&identical(tolower(as.character(combat$phase[[1L]]%||%"")),"exploration")})
    observe({exploring<-isTRUE(is_exploration_phase());for(id in c("combat_movement_group","combat_actions_group","combat_turn_group","combat_status_group","combat_runes_ui"))shinyjs::toggle(id=session$ns(id),condition=!exploring)})
    
    is_players_turn <- reactive({
      cid <- as.character(core$state$char_id %||% "")
      if (!nzchar(cid)) return(FALSE)
      
      combat <- combat_tbl()
      if (!is.data.frame(combat) || nrow(combat) == 0) return(FALSE)
      if(isTRUE(is_exploration_phase())){
        actors<-encounter_actors_tbl();return(nrow(actors[as.character(actors$actor_id)==cid&as.character(actors$actor_type)=="player",,drop=FALSE])>0L)
      }
      if (!identical(as.character(combat$phase[1] %||% ""), "combat")) return(FALSE)
      
      active_id <- as.character(combat$active_actor_id[1] %||% "")
      active_type <- as.character(combat$active_actor_type[1] %||% "")
      
      if (identical(active_type, "player") && identical(active_id, cid)) return(TRUE)
      if (identical(active_type, "summon")) {
        actors <- encounter_actors_tbl()
        owned <- actors[as.character(actors$actor_id) == active_id, , drop = FALSE]
        return(nrow(owned) > 0L && identical(as.character(owned$owner_actor_id[1] %||% ""), cid))
      }
      FALSE
    })
    
    output$combat_3d_static <- renderUI({
      
      if (!isTRUE(map_true3d_mode())) return(NULL)
      
      div(
        class = "combat-section-title",
        "3D Map",
        
        tags$div(
          id = session$ns("combat_3d_canvas"),
          style = "
        width:100%;
        height:620px;
        border-radius:14px;
        overflow:hidden;
        background:#111;
      "
        )
      )
    })
    
    observeEvent(input$dash_move, {
      requested <- isTRUE(input$dash_move)
      movement_dash(requested)
    }, ignoreInit = FALSE)
    
    output$phase_move_ui <- renderUI({
      if (!isTRUE(can_use_phase())) return(NULL)
      
      checkboxInput(
        session$ns("phase_move"),
        if (isTRUE(is_heart_eater())) "Shadow Step through obstacles" else "Climb / vault obstacles",
        value = FALSE
      )
    })
    
    observeEvent(input$phase_move, {
      movement_phase(isTRUE(input$phase_move) && isTRUE(can_use_phase()))
    }, ignoreInit = FALSE)
    
  
    
    output$turn_notice_ui <- renderUI({
      cid <- as.character(core$state$char_id %||% "")
      if (!nzchar(cid)) return(NULL)
      
      if (isTRUE(is_players_turn())) {
        div(
          class = "combat-pill",
          style = "background: rgba(225,245,225,0.96); border-color: rgba(90,150,90,0.75);",
          "✅ It is your turn"
        )
      } else {
        div(
          class = "combat-pill",
          style = "background: rgba(255,245,230,0.96); border-color: rgba(191,167,111,0.75);",
          "⏳ Not your turn — only opportunity attacks allowed"
        )
      }
    })
    
    # --------------------------------------------------
    # Current encounter
    # --------------------------------------------------
    current_encounter_id <- reactive({
      snapshot <- snapshot_data()
      session_row <- snapshot$session %||% data.frame()
      eid <- NA_integer_

      if (is.data.frame(session_row) && nrow(session_row) > 0 &&
          "active_encounter_id" %in% names(session_row)) {
        eid <- suppressWarnings(as.integer(session_row$active_encounter_id[1] %||% NA))
      }

      if (is.na(eid)) {
        eid <- suppressWarnings(as.integer(core$state$active_encounter_id %||% NA))
      }
      if (is.na(eid) || eid < 1) return(NA_integer_)
      eid
    })
    
    

    


    observeEvent(input$map_fullscreen_exit, {
      map_fullscreen(FALSE)
    }, ignoreInit = TRUE)
    
    encounter_tbl <- reactive({
      refresh_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())

      cached <- snapshot_data()$encounter %||% data.frame()
      if (is.data.frame(cached) && nrow(cached) > 0 &&
          identical(as.integer(cached$id[1] %||% NA), as.integer(eid))) {
        return(cached)
      }
      
      df <- tryCatch(get_encounter(eid), error = function(e) data.frame())
      if (!is.data.frame(df)) data.frame() else df
    })
    
    current_session_id <- reactive({
      enc <- encounter_tbl()
      if (!is.data.frame(enc) || nrow(enc) == 0) return(NA_integer_)
      
      sid <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
      if (is.na(sid) || sid < 1) return(NA_integer_)
      sid
    })
    
    players_tbl <- reactive({
      refresh_key()
      sid <- current_session_id()
      if (is.na(sid)) return(data.frame())
      
      df <- snapshot_data()$players %||% data.frame()
      if (!is.data.frame(df)) data.frame() else df
    })
    
    positions_tbl <- reactive({
      positions_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- snapshot_data()$positions %||% data.frame()
      if (!is.data.frame(df)) data.frame() else df
    })
    
    combat_tbl <- reactive({
      combat_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- snapshot_data()$combat %||% data.frame()
      if (!is.data.frame(df)) data.frame() else df
    })
    observeEvent({x<-combat_tbl();if(!nrow(x))NULL else c(current_encounter_id(),as.integer(x$round_number[[1L]]%||%1L))},{x<-combat_tbl();ticks<-tick_active_rune_zones(current_encounter_id(),as.integer(x$round_number[[1L]]%||%1L));if(length(ticks)){refresh_key(refresh_key()+1L);events_key(events_key()+1L);bump_map_visual()}},ignoreInit=FALSE)
    
    events_tbl <- reactive({
      events_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- snapshot_data()$events %||% data.frame()
      if (!is.data.frame(df)) data.frame() else df
    })

    observe({
      ev<-events_tbl();cid<-as.character(core$state$char_id%||%"");if(!is.null(session$userData$pending_merchant_id)||!is.null(session$userData$pending_trade_id)||!is.null(session$userData$pending_note_id)||!is.null(session$userData$pending_opportunity)||!nrow(ev)||!nzchar(cid)||!"event_type"%in%names(ev))return();rows<-ev[as.character(ev$event_type)=="opportunity_available",,drop=FALSE];if(!nrow(rows))return()
      for(i in seq_len(nrow(rows))){event_id<-as.character(rows$id[i]%||%paste0(rows$created_at[i],rows$actor_id[i]));if(event_id%in%prompted_opportunity_events())next;payload<-decode_effect_payload(rows$payload[[i]]);if(!cid%in%as.character(unlist(payload$attacker_ids%||%list())))next
        prompted_opportunity_events(unique(c(prompted_opportunity_events(),event_id)));enemy_id<-as.character(rows$actor_id[i]);session$userData$opportunity_target<-enemy_id;session$userData$pending_opportunity<-TRUE
        showModal(modalDialog(title="Opportunity attack",p(paste0(get_actor_display_name(enemy_id)," moved out of your reach without Disengaging.")),p("Use your reaction to make an opportunity attack?"),footer=tagList(actionButton(session$ns("decline_opportunity"),"Let them go"),actionButton(session$ns("take_opportunity"),"Use reaction",class="btn btn-danger")))) ;break
      }
    })
    observeEvent(input$decline_opportunity,{session$userData$opportunity_target<-NULL;session$userData$pending_opportunity<-NULL;removeModal()},ignoreInit=TRUE)
    observeEvent(input$take_opportunity, {
      target <- as.character(session$userData$opportunity_target %||% "")
      session$userData$pending_opportunity <- NULL
      removeModal()
      if (!nzchar(target)) return()
      log_safe(paste0("⚔️ Opportunity attack accepted against ", get_actor_display_name(target), "."))
      open_attack_flow(target, verified_opportunity = TRUE)
    }, ignoreInit = TRUE)
    
    encounter_actors_tbl <- reactive({
      initiative_key()
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      df <- tryCatch(
        build_snapshot_encounter_actors(snapshot_data()),
        error = function(e) data.frame()
      )
      if (!is.data.frame(df)) data.frame() else df
    })
    
    # --------------------------------------------------
    # Map hydration
    # --------------------------------------------------
    observeEvent(encounter_tbl(), {
      enc <- encounter_tbl()
      if (!is.data.frame(enc) || nrow(enc) == 0) return()
      
      mid <- suppressWarnings(as.integer(enc$map_id[1] %||% 1L))
      if (is.na(mid) || mid < 1) mid <- 1L
      map_id(mid)
    }, ignoreInit = FALSE)
    
    

    
    observeEvent(input$apply_admin, {
      eid <- current_encounter_id()
      combat <- combat_tbl()
      
      if (is.na(eid) || nrow(combat) == 0) {
        log_safe("No active combat.")
        return()
      }
      
      actor_id <- as.character(combat$active_actor_id[1] %||% "")
      actor_type <- as.character(combat$active_actor_type[1] %||% "")
      
      amt <- suppressWarnings(as.integer(input$admin_amount %||% 0))
      if (is.na(amt) || amt <= 0) {
        log_safe("Invalid amount.")
        return()
      }
      
      mode <- as.character(input$admin_type %||% "damage")
      
      result <- NULL
      
      if (mode == "damage") {
        result <- if (actor_type == "player") {
          damage_player_in_encounter(eid, actor_id, amt)
        } else {
          tryCatch(
            damage_encounter_enemy(eid, actor_id, amt),
            error = function(e) NULL
          )
        }
      } else if (mode == "heal") {
        result <- if (actor_type == "player") {
          heal_session_player(
            session_id = current_session_id(),
            character_id = actor_id,
            amount = amt
          )
        } else {
          tryCatch(
            heal_encounter_enemy(eid, actor_id, amt),
            error = function(e) NULL
          )
        }
      }
      
      if (is.null(result)) {
        log_safe("Action failed.")
        return()
      }
      
      # Log it
      try(
        log_game_event(
          encounter_id = eid,
          event_type = ifelse(mode == "heal", "heal", "damage"),
          actor_type = "control",
          target_id = actor_id,
          payload = list(
            amount = amt,
            hp_before = result$hp_before,
            hp_after = result$hp_after
          )
        ),
        silent = TRUE
      )
      
      log_safe(paste0(
        ifelse(mode == "heal", "Healed ", "Damaged "),
        get_actor_display_name(actor_id),
        " for ", amt
      ))
      
      bump_positions()
      bump_events()
      bump_initiative()
      bump_map_visual()
    })
    
    

    
    # --------------------------------------------------
    # Actor helpers
    # --------------------------------------------------
    get_actor_row <- function(actor_id, actor_type = NULL) {
      actor_id <- as.character(actor_id %||% "")
      if (!nzchar(actor_id)) return(data.frame())
      
      actors <- encounter_actors_tbl()
      if (!is.data.frame(actors) || nrow(actors) == 0) return(data.frame())
      
      out <- actors[as.character(actors$actor_id) == actor_id, , drop = FALSE]
      
      if (!is.null(actor_type)) {
        out <- out[as.character(out$actor_type) == as.character(actor_type), , drop = FALSE]
      }
      
      out
    }
    
    get_actor_display_name <- function(actor_id, actor_type = NULL) {
      row <- get_actor_row(actor_id, actor_type)
      if (!is.data.frame(row) || nrow(row) == 0) return("Unknown")
      as.character(row$display_name[1] %||% row$name[1] %||% "Unknown")
    }
    
    get_actor_type_by_id <- function(actor_id) {
      row <- get_actor_row(actor_id)
      if (!is.data.frame(row) || nrow(row) == 0) return(NA_character_)
      as.character(row$actor_type[1] %||% NA_character_)
    }
    
    active_actor_id <- reactive({
      combat <- combat_tbl()
      if (!is.data.frame(combat) || nrow(combat) == 0) return(NULL)
      
      aid <- as.character(combat$active_actor_id[1] %||% "")
      if (!nzchar(aid)) return(NULL)
      aid
    })
    
    active_actor_row <- reactive({
      aid <- active_actor_id()
      if (is.null(aid)) return(data.frame())
      get_actor_row(aid)
    })
    
    active_position_row <- reactive({
      aid <- if(isTRUE(is_exploration_phase()))as.character(core$state$char_id%||%"")else active_actor_id()
      pos <- positions_tbl()
      
      if (is.null(aid) || !is.data.frame(pos) || nrow(pos) == 0) {
        return(data.frame())
      }
      
      pos[as.character(pos$actor_id) == as.character(aid), , drop = FALSE]
    })
    
    load_actor_for_combat <- function(actor_id, actor_type = NULL) {
      actor_id <- as.character(actor_id %||% "")
      actor_type <- as.character(actor_type %||% get_actor_type_by_id(actor_id) %||% "")
      
      if (!nzchar(actor_id) || !nzchar(actor_type)) return(NULL)
      
      if (identical(actor_type, "player")) {
        db_char <- tryCatch(load_character_from_db(actor_id), error = function(e) NULL)
        
        if (!is.null(db_char)) {
          return(db_char)
        }
        
        if (identical(as.character(core$state$char_id %||% ""), actor_id)) {
          return(core$state$char)
        }
        
        return(NULL)
      }
      
      if (identical(actor_type, "enemy")) {
        enemies <- tryCatch(get_encounter_enemies(current_encounter_id()), error = function(e) data.frame())
        if (!is.data.frame(enemies) || nrow(enemies) == 0) return(NULL)
        
        row <- enemies[as.character(enemies$enemy_uuid) == actor_id, , drop = FALSE]
        if (nrow(row) == 0) return(NULL)
        
        return(list(
          meta = list(name = as.character(row$name[1] %||% "Enemy"), race = "Enemy"),
          build = list(class = "Enemy", level = 1),
          abilities = enemy_db_json(row$abilities[[1]] %||% NULL, list(str=10,dex=10,con=10,int=10,cha=10,bld_str=10)),
          resources = list(
            hp = list(
              max = as.integer(row$hp_max[1] %||% 1),
              cur = as.integer(row$hp_current[1] %||% 0),
              temp = as.integer(row$temp_hp[1] %||% 0)
            ),
            sindre = list(cur = 0, total = 0, temp = 0)
          ),
          status = list(
            effects = character(0),
            exhaustion = 0,
            bloodlust = FALSE
          ),
          combat_profile = list(
            resistances = enemy_db_values(row$resistances[[1]] %||% NULL),
            immunities = enemy_db_values(row$immunities[[1]] %||% NULL),
            vulnerabilities = enemy_db_values(row$vulnerabilities[[1]] %||% NULL),
            condition_immunities = enemy_db_values(row$condition_immunities[[1]] %||% NULL),
            attacks = enemy_db_json(row$attacks[[1]] %||% NULL, list()),
            loot = enemy_db_json(row$loot[[1]] %||% NULL, list()),
            ac_override = as.integer(row$ac[1] %||% 10),
            speed_ft = as.integer(row$movement_speed[1] %||% row$speed_ft[1] %||% 30L),
            initiative_mod = as.integer(row$initiative_mod[1] %||% 0L),
            attack_bonus = as.integer(row$attack_bonus[1] %||% 2L),
            damage_expr = as.character(row$damage_expr[1] %||% "1d6"),
            damage_type = as.character(row$damage_type[1] %||% "slashing")
          )
        ))
      }
      
      NULL
    }
    
 
    get_effective_actor_ac <- function(actor_id, actor_type = NULL, actor_obj = NULL) {
      actor_type <- as.character(actor_type %||% get_actor_type_by_id(actor_id) %||% "")
      
      if (identical(actor_type, "enemy")) {
        enemies <- tryCatch(get_encounter_enemies(current_encounter_id()), error = function(e) data.frame())
        if (is.data.frame(enemies) && nrow(enemies) > 0) {
          row <- enemies[as.character(enemies$enemy_uuid) == as.character(actor_id), , drop = FALSE]
          if (nrow(row) > 0) {
            ac <- suppressWarnings(as.integer(row$ac[1] %||% NA))
            if (!is.na(ac)) return(ac)
          }
        }
      }
      
      if (!is.null(actor_obj)) {
        ac <- tryCatch(calc_auto_ac_for_char(actor_obj), error = function(e) NA_integer_)
        if (!is.na(ac)) return(ac)
      }
      
      10L
    }
    
    # --------------------------------------------------
    # HP / AC helpers
    # --------------------------------------------------
    damage_player_in_encounter <- function(encounter_id, character_id, amount) {
      enc <- tryCatch(get_encounter(encounter_id), error = function(e) data.frame())
      if (!is.data.frame(enc) || nrow(enc) == 0) return(NULL)
      
      session_id <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
      if (is.na(session_id) || session_id < 1) return(NULL)
      
      damage_session_player(
        session_id = session_id,
        character_id = character_id,
        amount = amount
      )
    }
    
    update_known_ac <- function(target_id, attack_total, is_hit) {
      df <- known_ac_rv()
      target_id <- as.character(target_id %||% "")
      if (!nzchar(target_id)) return(invisible(FALSE))
      
      idx <- which(df$target_id == target_id)
      
      if (length(idx) == 0) {
        df <- rbind(df, data.frame(
          target_id = target_id,
          lower = 1L,
          upper = 99L,
          stringsAsFactors = FALSE
        ))
        idx <- nrow(df)
      }
      
      if (isTRUE(is_hit)) {
        df$upper[idx] <- min(as.integer(df$upper[idx]), as.integer(attack_total))
      } else {
        df$lower[idx] <- max(as.integer(df$lower[idx]), as.integer(attack_total) + 1L)
      }
      
      known_ac_rv(df)
      invisible(TRUE)
    }
    
    format_known_ac <- function(target_id) {
      df <- known_ac_rv()
      row <- df[df$target_id == as.character(target_id), , drop = FALSE]
      if (nrow(row) == 0) return("AC ?")
      
      lo <- as.integer(row$lower[1] %||% 1L)
      hi <- as.integer(row$upper[1] %||% 99L)
      
      if (lo <= 1L && hi >= 99L) return("AC ?")
      if (lo > 1L && hi < 99L && lo <= hi) return(paste0("AC ", lo, "–", hi))
      if (lo > 1L && hi >= 99L) return(paste0("AC ≥ ", lo))
      if (lo <= 1L && hi < 99L) return(paste0("AC ≤ ", hi))
      "AC ?"
    }
    
    hp_bar_ui <- function(cur, temp = 0, maxv = NULL) {
      cur  <- suppressWarnings(as.integer(cur %||% 0))
      temp <- suppressWarnings(as.integer(temp %||% 0))
      maxv <- suppressWarnings(as.integer(maxv %||% max(cur, 1)))
      
      if (is.na(cur)) cur <- 0L
      if (is.na(temp)) temp <- 0L
      if (is.na(maxv) || maxv <= 0) maxv <- max(cur, 1L)
      
      cur <- max(0L, cur)
      temp <- max(0L, temp)
      
      base_pct <- max(0, min(100, round((cur / maxv) * 100)))
      temp_pct <- max(0, min(100, round(((cur + temp) / maxv) * 100)))
      
      tags$div(
        class = "mini-bar-wrap",
        tags$div(
          class = "mini-bar-label",
          sprintf(
            "HP %s/%s%s",
            cur,
            maxv,
            if (temp > 0) paste0(" +", temp, " temp") else ""
          )
        ),
        tags$div(
          class = "mini-bar",
          tags$div(
            class = "mini-fill hp",
            style = paste0("width:", base_pct, "%;")
          ),
          if (temp > 0) {
            tags$div(
              class = "mini-fill",
              style = paste0(
                "width:", temp_pct, "%;",
                "background: linear-gradient(90deg, rgba(220,235,255,0.42), rgba(145,205,255,0.82));"
              )
            )
          }
        )
      )
    }
    
    # --------------------------------------------------
    # Damage trait helpers
    # --------------------------------------------------
    normalize_damage_type <- function(x) {
      x <- tolower(trimws(as.character(x %||% "")))
      x[nzchar(x)]
    }
    
    get_character_damage_traits <- function(char) {
      char <- validate_character(char)
      
      prof1 <- char$combat_profile %||% list()
      prof2 <- char$combat %||% list()
      prof3 <- char$meta$combat_profile %||% list()
      
      get_vec <- function(name) {
        equipment<-equipped_magical_traits(char)
        out <- c(
          prof1[[name]] %||% character(0),
          prof2[[name]] %||% character(0),
          prof3[[name]] %||% character(0),
          equipment[[name]] %||% character(0)
        )
        unique(normalize_damage_type(out))
      }
      
      resistances <- get_vec("resistances")
      if (isTRUE(char$status$raging %||% FALSE) && character_has_feature(char, "rage")) {
        resistances <- unique(c(resistances, "bludgeoning", "piercing", "slashing"))
        if (identical(character_level_choice(char, "Barbarian", 3L, "spirit_totem"), "Bear")) {
          resistances <- unique(c(
            resistances,
            "acid", "cold", "fire", "force", "lightning", "necrotic", "poison", "radiant", "thunder"
          ))
        }
      }
      list(
        resistances = resistances,
        immunities = get_vec("immunities"),
        vulnerabilities = get_vec("vulnerabilities")
      )
    }

    add_active_ward_traits <- function(traits,target_id) {
      pos<-positions_tbl();row<-pos[as.character(pos$actor_id)==as.character(target_id),,drop=FALSE];if(!nrow(row))return(traits)
      ward<-tryCatch(ward_resistances_at_point(current_encounter_id(),row$x[[1L]],row$y[[1L]]),error=function(e)character())
      traits$resistances<-unique(c(traits$resistances%||%character(),ward));traits
    }
    
    get_class_level <- function(char, class_name) {
      char <- validate_character(char)
      class_name <- tolower(trimws(as.character(class_name %||% "")))
      if (!nzchar(class_name)) return(0L)
      
      clv <- char$build$class_levels %||% NULL
      if (is.list(clv) || is.vector(clv)) {
        nms <- names(clv) %||% character(0)
        idx <- which(tolower(nms) == class_name)
        if (length(idx) >= 1) {
          val <- suppressWarnings(as.integer(clv[[idx[1]]]))
          if (!is.na(val)) return(max(0L, val))
        }
      }
      
      cls <- char$build$classes %||% NULL
      if (is.list(cls)) {
        nms <- names(cls) %||% character(0)
        idx <- which(tolower(nms) == class_name)
        if (length(idx) >= 1) {
          ent <- cls[[idx[1]]]
          val <- suppressWarnings(as.integer(ent$level %||% ent$levels %||% ent))
          if (!is.na(val)) return(max(0L, val))
        }
      }
      
      mc <- char$build$multiclass %||% NULL
      if (is.list(mc)) {
        nms <- names(mc) %||% character(0)
        idx <- which(tolower(nms) == class_name)
        if (length(idx) >= 1) {
          ent <- mc[[idx[1]]]
          val <- suppressWarnings(as.integer(ent$level %||% ent$levels %||% ent))
          if (!is.na(val)) return(max(0L, val))
        }
      }
      
      main_class <- tolower(as.character(char$build$class %||% ""))
      lvl <- suppressWarnings(as.integer(char$build$level %||% 1))
      if (!is.na(lvl) && nzchar(main_class) && identical(main_class, class_name)) {
        return(max(0L, lvl))
      }
      
      0L
    }
    
    get_sneak_attack_expr <- function(char) {
      rogue_level <- get_class_level(char, "rogue")
      if (rogue_level < 1) return("")
      
      dice_n <- floor((rogue_level + 1) / 2)
      if (dice_n < 1) return("")
      paste0(dice_n, "d6")
    }

    weapon_name_key <- function(weapon_row) {
      tolower(trimws(as.character(weapon_row$name[1] %||% "")))
    }

    weapon_is_ranged <- function(weapon_row) {
      grepl("bow|crossbow|sling|dart|firearm|pistol|rifle", weapon_name_key(weapon_row))
    }

    weapon_is_finesse <- function(weapon_row) {
      grepl("dagger|rapier|shortsword|scimitar|whip", weapon_name_key(weapon_row)) ||
        identical(tolower(as.character(weapon_row$stat[1] %||% "")), "dex")
    }

    weapon_is_light_melee <- function(weapon_row) {
      !weapon_is_ranged(weapon_row) &&
        grepl("dagger|shortsword|scimitar|club|handaxe|hand axe|light hammer|sickle", weapon_name_key(weapon_row))
    }

    sneak_attack_eligibility <- function(attacker_char, weapon_row, attacker_id, target_id, adv_mode, is_hit) {
      expr <- get_sneak_attack_expr(attacker_char)
      if (!nzchar(expr)) return(list(eligible = FALSE, reason = "", expr = ""))
      if (isTRUE(sneak_attack_used())) {
        return(list(eligible = FALSE, reason = "Sneak Attack has already been used this turn.", expr = expr))
      }
      if (!weapon_is_finesse(weapon_row) && !weapon_is_ranged(weapon_row)) {
        return(list(eligible = FALSE, reason = "Sneak Attack requires a finesse or ranged weapon.", expr = expr))
      }
      if (identical(tolower(as.character(adv_mode %||% "normal")), "disadvantage")) {
        return(list(eligible = FALSE, reason = "Sneak Attack cannot be used while attacking with disadvantage.", expr = expr))
      }

      actors <- encounter_actors_tbl()
      target <- actors[as.character(actors$actor_id) == as.character(target_id), , drop = FALSE]
      allies <- actors[
        as.character(actors$actor_id) != as.character(attacker_id) &
          as.character(actors$actor_type %||% "") != "enemy",
        , drop = FALSE
      ]
      ally_near_target <- FALSE
      if (nrow(target) && nrow(allies)) {
        ally_near_target <- any(vapply(seq_len(nrow(allies)), function(i) {
          conditions <- actor_conditions(as.character(allies$actor_id[i]))
          active <- !any(c("unconscious", "incapacitated", "dead") %in% conditions)
          active && is_adjacent_5ft(allies$x[i], allies$y[i], target$x[1], target$y[1])
        }, logical(1)))
      }
      has_advantage <- identical(tolower(as.character(adv_mode %||% "normal")), "advantage")
      if (!has_advantage && !ally_near_target) {
        return(list(eligible = FALSE, reason = "Sneak Attack needs advantage or an active ally within 5 ft of the target.", expr = expr))
      }
      if (!isTRUE(is_hit)) {
        return(list(eligible = FALSE, reason = "Sneak Attack is ready, but this attack missed.", expr = expr))
      }
      list(eligible = TRUE, reason = "Eligible: finesse/ranged hit with advantage or an ally threatening the target.", expr = expr)
    }
    
    apply_damage_traits_to_parts <- function(parts, traits) {
      if (length(parts) == 0) return(list(parts = list(), total = 0L))
      
      res <- lapply(parts, function(part) {
        typ <- normalize_damage_type(part$type %||% "")
        material <- normalize_damage_type(part$material %||% "")
        keys <- unique(c(typ, material))
        raw <- as.integer(part$total %||% 0)
        adj <- raw
        rule <- "normal"
        
        if (length(keys) > 0 && any(keys %in% normalize_damage_type(traits$immunities))) {
          adj <- 0L
          rule <- "immune"
        } else if (length(keys) > 0 && any(keys %in% normalize_damage_type(traits$resistances))) {
          adj <- floor(raw / 2)
          rule <- "resistant"
        } else if (length(keys) > 0 && any(keys %in% normalize_damage_type(traits$vulnerabilities))) {
          adj <- raw * 2L
          rule <- "vulnerable"
        }
        
        c(part, list(adjusted_total = as.integer(adj), rule = rule))
      })
      
      total <- sum(vapply(res, function(x) as.integer(x$adjusted_total %||% 0L), integer(1)))
      list(parts = res, total = as.integer(total))
    }

    spellsword_traits <- function(traits, damage_type, attacker_char) {
      if (!character_has_feature(attacker_char, "spellsword")) return(traits)
      damage_type <- normalize_damage_type(damage_type)
      if (!length(damage_type)) return(traits)
      type <- damage_type[[1L]]
      immunities <- normalize_damage_type(traits$immunities)
      resistances <- normalize_damage_type(traits$resistances)
      if (type %in% immunities) {
        traits$immunities <- setdiff(immunities, type)
        traits$resistances <- unique(c(resistances, type))
      } else if (type %in% resistances) {
        traits$resistances <- setdiff(resistances, type)
      }
      traits
    }
    
    build_attack_preview <- function(attacker_char, target_char, weapon_row, attacker_name, target_name,
                                     attacker_id, target_id, attacker_type = "player", target_type = NULL,
                                     adv_override = NULL) {
      attacker_char <- validate_character(attacker_char)
      target_type <- as.character(target_type %||% get_actor_type_by_id(target_id) %||% "player")
      
      adv <- as.character(adv_override %||% weapon_row$adv[1] %||% "Normal")
      adv_norm <- tolower(as.character(adv %||% "normal"))
      
      if (adv_norm %in% c("advantage", "adv")) {
        attack_rolls <- sample.int(20, 2)
        attack_roll <- max(attack_rolls)
      } else if (adv_norm %in% c("disadvantage", "dis")) {
        attack_rolls <- sample.int(20, 2)
        attack_roll <- min(attack_rolls)
      } else {
        attack_rolls <- sample.int(20, 1)
        attack_roll <- attack_rolls[1]
      }
      

      
      attack_bonus <- get_weapon_hit_bonus(attacker_char, weapon_row)
      active_manoeuvre <- as.character(manoeuvre_active() %||% "")
      superiority_roll <- 0L
      if (identical(active_manoeuvre, "Precision Attack")) {
        superiority_roll <- sample.int(8L, 1L)
        attack_bonus <- attack_bonus + superiority_roll
      }
      attack_total <- as.integer(attack_roll + attack_bonus)
      target_ac <- get_effective_actor_ac(target_id, target_type, target_char)
      
      critical_threshold <- if (character_has_feature(attacker_char, "improved_critical")) 19L else 20L
      base_hit <- attack_total >= target_ac
      surprise_critical <- character_has_feature(attacker_char, "assassinate") &&
        "surprised" %in% actor_conditions(target_id) && base_hit
      is_crit <- attack_roll >= critical_threshold || surprise_critical
      is_hit <- is_crit || base_hit
      
      damage_parts <- list()

      roll_attack_damage <- function(expr) {
        first <- roll_dice_expr(expr)
        if (!isTRUE(is_crit)) return(first)
        extra <- roll_dice_expr(expr)
        list(
          rolls = c(first$rolls, extra$rolls),
          total = as.integer(first$total + sum(extra$rolls))
        )
      }
      
      if (isTRUE(is_hit)) {
        dmg1_expr <- as.character(weapon_row$damage1[1] %||% "")
        if (nzchar(dmg1_expr)) {
          d1 <- roll_attack_damage(dmg1_expr)
          damage_parts <- c(damage_parts, list(list(
            source = "Weapon",
            expr = dmg1_expr,
            type = as.character(weapon_row$dmg_type1[1] %||% ""),
            material = as.character(weapon_row$material[1] %||% weapon_row$weapon_material[1] %||% ""),
            rolls = d1$rolls,
            total = as.integer(d1$total)
          )))
        }
        equipment_damage_bonus <- suppressWarnings(as.integer(
          as.numeric(weapon_row$material_damage_modifier[1] %||% 0) +
            as.numeric(weapon_row$quality_damage_modifier[1] %||% 0)
        ))
        if (is.na(equipment_damage_bonus)) equipment_damage_bonus <- 0L
        if (equipment_damage_bonus != 0L) {
          damage_parts <- c(damage_parts, list(list(
            source = "Material & build quality",
            expr = sprintf("%+d", equipment_damage_bonus),
            type = as.character(weapon_row$dmg_type1[1] %||% ""),
            material = as.character(weapon_row$material[1] %||% ""),
            rolls = integer(), total = equipment_damage_bonus
          )))
        }
        
        dmg2_expr <- as.character(weapon_row$damage2[1] %||% "")
        if (nzchar(dmg2_expr)) {
          d2 <- roll_attack_damage(dmg2_expr)
          damage_parts <- c(damage_parts, list(list(
            source = "Weapon Extra",
            expr = dmg2_expr,
            type = as.character(weapon_row$dmg_type2[1] %||% ""),
            material = as.character(weapon_row$material[1] %||% weapon_row$weapon_material[1] %||% ""),
            rolls = d2$rolls,
            total = as.integer(d2$total)
          )))
        }
      }
      
      sneak_check <- sneak_attack_eligibility(
        attacker_char, weapon_row, attacker_id, target_id, adv, is_hit
      )
      sneak_expr <- sneak_check$expr
      sneak_part <- NULL
      if (isTRUE(sneak_check$eligible)) {
        sa <- roll_attack_damage(sneak_expr)
        sneak_part <- list(
          source = "Sneak Attack",
          expr = sneak_expr,
          type = as.character(weapon_row$dmg_type1[1] %||% "piercing"),
          rolls = sa$rolls,
          total = as.integer(sa$total)
        )
      }

      weapon_stat <- tolower(as.character(weapon_row$stat[1] %||% "str"))
      if (isTRUE(is_hit) && identical(weapon_stat, "str") &&
          isTRUE(attacker_char$status$raging %||% FALSE) && character_has_feature(attacker_char, "rage")) {
        barbarian_level <- sum(vapply(normalise_character_classes(attacker_char), function(entry) {
          if (identical(as.character(entry$class %||% ""), "Barbarian")) as.integer(entry$level %||% 0L) else 0L
        }, integer(1)))
        rage_bonus <- if (barbarian_level >= 16L) 4L else if (barbarian_level >= 9L) 3L else 2L
        damage_parts <- c(damage_parts, list(list(
          source = "Rage", expr = as.character(rage_bonus),
          type = as.character(weapon_row$dmg_type1[1] %||% ""),
          rolls = integer(), total = rage_bonus
        )))
      }
      if (isTRUE(is_hit) && nzchar(active_manoeuvre) && !identical(active_manoeuvre, "Precision Attack")) {
        die <- sample.int(8L, 1L)
        damage_parts <- c(damage_parts, list(list(
          source = active_manoeuvre, expr = "1d8",
          type = as.character(weapon_row$dmg_type1[1] %||% ""), rolls = die, total = die
        )))
      }
      
      list(
        attacker_id = as.character(attacker_id),
        attacker_type = as.character(attacker_type %||% "player"),
        target_id = as.character(target_id),
        target_type = as.character(target_type),
        attacker_name = as.character(attacker_name),
        target_name = as.character(target_name),
        weapon_id = as.character(weapon_row$id[1] %||% ""),
        weapon_name = as.character(weapon_row$name[1] %||% "Weapon"),
        attack_roll = as.integer(attack_roll),
        attack_rolls = as.integer(attack_rolls),
        attack_adv_mode = adv,
        attack_bonus = as.integer(attack_bonus),
        superiority_manoeuvre = active_manoeuvre,
        superiority_roll = superiority_roll,
        attack_total = as.integer(attack_total),
        target_ac = as.integer(target_ac),
        is_hit = isTRUE(is_hit),
        is_crit = isTRUE(is_crit),
        base_parts = damage_parts,
        sneak_available = !is.null(sneak_part),
        sneak_reason = as.character(sneak_check$reason %||% ""),
        sneak_expr = as.character(sneak_expr %||% ""),
        sneak_part = sneak_part,
        target_traits = add_active_ward_traits(get_character_damage_traits(target_char),target_id),
        primary_damage_type = as.character(weapon_row$dmg_type1[1] %||% "")
      )
    }
    
    compute_final_attack <- function(preview, apply_sneak = FALSE, manual_bonus = 0L, manual_type = "") {
      if (is.null(preview)) {
        return(list(parts = list(), adjusted = list(parts = list(), total = 0L), raw_total = 0L))
      }
      
      parts <- preview$base_parts %||% list()
      
      if (isTRUE(apply_sneak) && isTRUE(preview$sneak_available) && !is.null(preview$sneak_part)) {
        parts <- c(parts, list(preview$sneak_part))
      }
      
      manual_bonus <- suppressWarnings(as.integer(manual_bonus %||% 0))
      if (is.na(manual_bonus) || manual_bonus < 0) manual_bonus <- 0L
      
      if (manual_bonus > 0L) {
        use_type <- as.character(manual_type %||% "")
        if (!nzchar(use_type) || identical(use_type, "same_as_primary")) {
          use_type <- preview$primary_damage_type %||% ""
        }
        if (identical(use_type, "untyped")) {
          use_type <- ""
        }
        
        parts <- c(parts, list(list(
          source = "Manual Bonus",
          expr = paste0(manual_bonus),
          type = use_type,
          rolls = integer(0),
          total = as.integer(manual_bonus)
        )))
      }

      if (!is.null(preview$target_id)) {
        parts <- lapply(parts, function(part) {
          modifier <- rain_damage_modifier(preview$target_id, part$type %||% "")
          part$total <- max(0L, as.integer(part$total %||% 0L) + modifier)
          if (modifier != 0L) {
            part$source <- paste0(
              part$source %||% "Damage", " [Rain ",
              if (modifier > 0L) "+" else "", modifier, "]"
            )
          }
          part
        })
      }
      
      raw_total <- sum(vapply(parts, function(x) as.integer(x$total %||% 0L), integer(1)))
      adjusted <- apply_damage_traits_to_parts(parts, preview$target_traits)
      
      list(
        parts = parts,
        adjusted = adjusted,
        raw_total = as.integer(raw_total)
      )
    }
    
    # --------------------------------------------------
    # Attack confirm modal
    # --------------------------------------------------
    output$attack_confirm_ui <- renderUI({
      preview <- pending_attack()
      if (is.null(preview)) return(NULL)
      
      apply_sneak <- isTRUE(input$final_apply_sneak %||% FALSE)
      manual_bonus <- suppressWarnings(as.integer(input$final_bonus_damage %||% 0))
      manual_type <- as.character(input$final_bonus_type %||% "same_as_primary")
      
      calc <- compute_final_attack(
        preview = preview,
        apply_sneak = apply_sneak,
        manual_bonus = manual_bonus,
        manual_type = manual_type
      )
      
      traits <- preview$target_traits %||% list(
        resistances = character(0),
        immunities = character(0),
        vulnerabilities = character(0)
      )
      
      part_ui <- if (length(calc$adjusted$parts) == 0) {
        div(class = "damage-part", span("No damage parts"), span("0"))
      } else {
        lapply(calc$adjusted$parts, function(part) {
          raw <- as.integer(part$total %||% 0L)
          adj <- as.integer(part$adjusted_total %||% 0L)
          rule <- as.character(part$rule %||% "normal")
          typ <- as.character(part$type %||% "")
          lbl <- paste0(
            part$source %||% "Damage",
            if (nzchar(typ)) paste0(" (", typ, ")") else ""
          )
          rhs <- if (identical(rule, "normal")) {
            as.character(adj)
          } else {
            paste0(raw, " → ", adj, " [", rule, "]")
          }
          
          div(class = "damage-part", span(lbl), span(rhs))
        })
      }
      
      tagList(
        div(
          class = "confirm-box",
          div(class = "combat-section-title", "Attack Preview"),
          div(
            class = "confirm-kv",
            div(class = "confirm-k", "Attacker"), div(preview$attacker_name),
            div(class = "confirm-k", "Target"), div(preview$target_name),
            div(class = "confirm-k", "Weapon"), div(preview$weapon_name),
            div(class = "confirm-k", "To Hit"), div(paste0(
              paste(preview$attack_rolls, collapse = "/"),
              if (!identical(preview$attack_adv_mode %||% "Normal", "Normal")) {
                paste0(" [", preview$attack_adv_mode %||% "Normal", "]")
              } else {
                ""
              },
              " → ", preview$attack_roll,
              " + ", preview$attack_bonus,
              " = ", preview$attack_total,
              " vs AC ", preview$target_ac
            )),
            div(class = "confirm-k", "Result"), div(
              if (isTRUE(preview$is_hit)) {
                if (isTRUE(preview$is_crit)) "Critical Hit" else "Hit"
              } else {
                "Miss"
              }
            )
          )
        ),
        
        if (isTRUE(preview$is_hit)) {
          tagList(
            if (identical(as.character(preview$target_id %||% ""), as.character(core$state$char_id %||% "")) &&
                player_has_feature("uncanny_dodge")) {
              div(
                class = "confirm-box",
                checkboxInput(
                  session$ns("use_uncanny_dodge"),
                  "Use reaction: Uncanny Dodge (halve final damage)",
                  value = FALSE
                )
              )
            },
            div(
              class = "confirm-box",
              div(class = "combat-section-title", "Modify Damage"),
              if (nzchar(as.character(preview$sneak_expr %||% ""))) {
                tagList(
                  tags$p(
                    class = if (isTRUE(preview$sneak_available)) "text-success" else "text-muted",
                    paste0("Sneak Attack ", preview$sneak_expr, ": ", preview$sneak_reason %||% "")
                  ),
                  if (isTRUE(preview$sneak_available) && !isTRUE(sneak_attack_used())) {
                    checkboxInput(
                      session$ns("final_apply_sneak"),
                      paste0("Apply Sneak Attack (", preview$sneak_part$expr %||% "", ")"),
                      value = TRUE
                    )
                  }
                )
              },
              fluidRow(
                column(
                  6,
                  numericInput(
                    session$ns("final_bonus_damage"),
                    "Manual bonus damage",
                    value = 0,
                    min = 0,
                    step = 1
                  )
                ),
                column(
                  6,
                  selectInput(
                    session$ns("final_bonus_type"),
                    "Bonus damage type",
                    choices = c(
                      "Same as primary" = "same_as_primary",
                      "Untyped" = "untyped",
                      unique(c(
                        preview$primary_damage_type %||% "",
                        normalize_damage_type(traits$resistances),
                        normalize_damage_type(traits$immunities),
                        normalize_damage_type(traits$vulnerabilities)
                      ))
                    ),
                    selected = "same_as_primary"
                  )
                )
              )
            ),
            
            div(
              class = "confirm-box",
              div(class = "combat-section-title", "Target Defences"),
              div(
                class = "confirm-kv",
                div(class = "confirm-k", "Resistances"),
                div(if (length(traits$resistances)) paste(traits$resistances, collapse = ", ") else "—"),
                div(class = "confirm-k", "Immunities"),
                div(if (length(traits$immunities)) paste(traits$immunities, collapse = ", ") else "—"),
                div(class = "confirm-k", "Vulnerabilities"),
                div(if (length(traits$vulnerabilities)) paste(traits$vulnerabilities, collapse = ", ") else "—")
              )
            ),
            
            div(
              class = "confirm-box",
              div(class = "combat-section-title", "Damage Breakdown"),
              part_ui,
              tags$hr(),
              div(class = "damage-part", span(tags$strong("Raw total")), span(tags$strong(calc$raw_total))),
              div(class = "damage-part", span(tags$strong("Adjusted total")), span(tags$strong(calc$adjusted$total))),
              div(class = "confirm-note", "You can still override the final number before applying.")
            )
          )
        } else {
          div(
            class = "confirm-box",
            div(class = "combat-section-title", "Damage"),
            div("Missed attacks default to 0 damage, but you can override if needed.")
          )
        }
      )
    })

    observeEvent(
      list(input$final_apply_sneak, input$final_bonus_damage, input$final_bonus_type),
      {
        preview <- pending_attack()
        if (is.null(preview) || !isTRUE(preview$is_hit)) return()
        apply_sneak <- isTRUE(input$final_apply_sneak %||% FALSE) && !isTRUE(sneak_attack_used())
        calc <- compute_final_attack(
          preview,
          apply_sneak = apply_sneak,
          manual_bonus = input$final_bonus_damage %||% 0L,
          manual_type = input$final_bonus_type %||% "same_as_primary"
        )
        updateNumericInput(session, "final_damage_override", value = calc$adjusted$total)
      },
      ignoreInit = TRUE
    )
    
    # --------------------------------------------------
    # Event formatting
    # --------------------------------------------------
    
    safe_event_payload <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.list(x)) return(x)
      
      if (is.character(x) && length(x) == 1 && nzchar(x)) {
        return(tryCatch(
          jsonlite::fromJSON(x, simplifyVector = FALSE),
          error = function(e) NULL
        ))
      }
      
      NULL
    }
    

    
    # --------------------------------------------------
    # Header
    # --------------------------------------------------
    output$header_ui <- renderUI({
      enc <- encounter_tbl()
      combat <- combat_tbl()
      active <- active_actor_row()
      
      enc_name <- if (is.data.frame(enc) && nrow(enc) > 0) {
        as.character(enc$name[1] %||% paste("encounter", current_encounter_id()))
      } else {
        paste("encounter", current_encounter_id())
      }
      
      round_txt <- if (is.data.frame(combat) && nrow(combat) > 0) {
        as.character(combat$round_number[1] %||% "—")
      } else {
        "—"
      }
      
      active_name <- if (is.data.frame(active) && nrow(active) > 0) {
        as.character(active$display_name[1] %||% "Unknown")
      } else {
        "No active actor"
      }
      
      div(
        class = "combat-compact-summary",
        tags$strong("⚔️ ", enc_name),
        tags$span(paste("Round", round_txt)),
        tags$span(paste("Turn:", active_name)),
        tags$span(paste("Moved:", turn_move_ft(), "ft"))
      )
    })
    
    actor_short_label <- function(actor_id, actor_type, occ_name) {
      actors <- encounter_actors_tbl()
      if (!is.data.frame(actors) || nrow(actors) == 0) {
        return(substr(occ_name, 1, 2))
      }
      
      row <- actors[
        as.character(actors$actor_id) == as.character(actor_id),
        ,
        drop = FALSE
      ]
      
      ord <- suppressWarnings(as.integer(row$turn_order[1] %||% NA))
      prefix <- if (identical(actor_type, "player")) "P" else "E"
      
      if (!is.na(ord)) return(paste0(prefix, ord))
      paste0(prefix, substr(occ_name, 1, 1))
    }
    
    # --------------------------------------------------
    # Map UI
    # --------------------------------------------------
    output$map_ui <- renderUI({
      map_ui_ready(TRUE)
      mode<-input$map_render_mode%||%"2d"
      tags$div(
        id = session$ns("combat_3d_shell"),
        class = if(identical(mode,"3d"))"combat-3d-shell"else"combat-2d-shell",
        
        tags$div(
          class = "combat-map-toolbar",
          radioButtons(session$ns("map_render_mode"),NULL,c("2D"="2d","Lean 3D"="3d"),selected=mode,inline=TRUE),
          if(identical(mode,"2d"))actionButton(session$ns("map_zoom_out"), "− Zoom", class = "btn btn-default")else NULL,
          if(identical(mode,"2d"))actionButton(session$ns("map_zoom_in"), "+ Zoom", class = "btn btn-default")else NULL,
          if(identical(mode,"3d"))selectInput(session$ns("map_3d_quality"),NULL,c("Low"="low","Balanced"="balanced","Decorative"="decorative"),selected=input$map_3d_quality%||%"balanced",width="135px")else NULL,
          actionButton(
            session$ns("map_3d_fullscreen"),
            "Fullscreen Map",
            class = "btn btn-default"
          )
        ),
        
        tags$div(
          id = session$ns("combat_3d_canvas"),
          class = if(identical(mode,"3d"))"combat-3d-canvas"else"combat-2d-canvas"
        )
      )
    })
    observeEvent(input$map_render_mode,{later::later(bump_map_visual,.15)},ignoreInit=TRUE)
    observeEvent(input$map_3d_quality,{if(identical(input$map_render_mode%||%"2d","3d"))later::later(bump_map_visual,.15)},ignoreInit=TRUE)
    
    
    observe({
      
      req(map_true3d_mode())
      req(map_ui_ready())
      map_visual_key()
      tiles_now <- map_tiles_rv()
      
      if (!is.data.frame(tiles_now) || nrow(tiles_now) == 0) {
        return()
      }
      
      occ <- tryCatch(
        map_occupants_rv(),
        error = function(e) empty_map_occupants()
      )
      
      render_df <- tryCatch(
        build_map_render_df(
          tiles = tiles_now,
          occupants = occ,
          map_id = map_id()
        ),
        error = function(e) {
          log_safe(paste("⚠️ Map render failed:", conditionMessage(e)))
          data.frame()
        }
      )
      
      
      # Personal GLTF models remain disabled; the lean renderer uses procedural miniatures.
      
      actors_lookup <- encounter_actors_tbl()
      
      if (!"occupant_name" %in% names(render_df)) {
        render_df$occupant_name <- ""
      }
      render_df$occupant_current_hp <- NA_integer_
      render_df$occupant_max_hp <- NA_integer_
      render_df$occupant_temp_hp <- 0L
      render_df$occupant_conditions <- ""
      
      if (is.data.frame(actors_lookup) && nrow(actors_lookup) > 0) {
        for (i in seq_len(nrow(render_df))) {
          oid <- as.character(render_df$occupant_id[i] %||% "")
          if (!nzchar(oid)) next
          
          row <- actors_lookup[
            as.character(actors_lookup$actor_id) == oid,
            ,
            drop = FALSE
          ]
          
          if (nrow(row) > 0) {
            render_df$occupant_name[i] <- as.character(
              row$display_name[1] %||% row$name[1] %||% oid
            )
            render_df$occupant_current_hp[i] <- suppressWarnings(as.integer(row$current_hp[1] %||% row$hp_current[1] %||% NA))
            render_df$occupant_max_hp[i] <- suppressWarnings(as.integer(row$max_hp[1] %||% row$hp_max[1] %||% NA))
            render_df$occupant_temp_hp[i] <- suppressWarnings(as.integer(row$temp_hp[1] %||% 0L))
            render_df$occupant_conditions[i] <- paste(actor_conditions(oid), collapse = ", ")
          }
        }
      }
      
      render_df$is_reachable <- FALSE
      render_df$is_pending_move <- FALSE
      render_df$is_active_actor <- FALSE
      render_df$is_self_actor <- !is.na(render_df$occupant_id) &
        as.character(render_df$occupant_id) == as.character(core$state$char_id %||% "")
      
      combat <- tryCatch(combat_tbl(), error = function(e) data.frame())
      
      if (is.data.frame(combat) && nrow(combat) > 0) {
        
        active_id <- active_actor_id()
        
     
        
        render_df$is_reachable <- FALSE
        
        pm <- pending_move()
        
        if (!is.null(pm)) {
          path <- pm$path %||% data.frame(x = pm$x, y = pm$y)
          path_keys <- if (is.data.frame(path) && nrow(path)) paste(path$x, path$y, sep = ",") else character()
          render_df$is_pending_move <- paste(render_df$x, render_df$y, sep = ",") %in% path_keys
          render_df$move_path_step <- match(paste(render_df$x, render_df$y, sep = ","), path_keys) - 1L
        }
        
        render_df$is_active_actor <-
          !is.na(render_df$occupant_id) &
          as.character(render_df$occupant_id) == as.character(active_id %||% "")
      }

      zone_df <- tryCatch(
        get_active_glyph_zones(current_encounter_id(), if(nrow(combat)) as.integer(combat$round_number[[1L]] %||% 1L) else NULL),
        error = function(e) data.frame()
      )
      if (nrow(zone_df)) zone_df <- zone_df[,c("id","glyph_type","name","rank","center_x","center_y","area_ft","colour","tooltip"),drop=FALSE]
      render_df$zone_tooltip <- ""
      if (nrow(zone_df)) for (i in seq_len(nrow(render_df))) {
        inside <- sqrt((zone_df$center_x-as.numeric(render_df$x[[i]]))^2+(zone_df$center_y-as.numeric(render_df$y[[i]]))^2) <= zone_df$area_ft/5
        if (any(inside)) render_df$zone_tooltip[[i]] <- paste(zone_df$tooltip[inside],collapse="\n\n")
      }
      
    
      
      generation<-isolate(map_send_generation())+1L
      map_send_generation(generation)
      later::later(function() {
        if(!identical(isolate(map_send_generation()),generation))return()
        mode<-isolate(input$map_render_mode%||%"2d")
        session$sendCustomMessage(
          if(identical(mode,"3d"))"combat3d-lean-init"else"combat3d-init",
          list(
            containerId = session$ns("combat_3d_canvas"),
            mapData = jsonlite::toJSON(
              render_df,
              dataframe = "rows",
              auto_unbox = TRUE,
              null = "null"
            ),
            zones = jsonlite::toJSON(zone_df, dataframe = "rows", auto_unbox = TRUE, null = "null"),
            quality=isolate(input$map_3d_quality%||%"balanced"),
            inputIds = list(
              move = session$ns("move_to_tile"),
              target = session$ns("map_target_click")
            )
          )
        )
      }, delay = 0.1)
    })
    
  
    
    empty_reachable <- function() {
      data.frame(
        x = integer(),
        y = integer(),
        move_cost_ft = integer(),
        stringsAsFactors = FALSE
      )
    }
    
    movement_range_tiles <- reactive({
      
      if (!isTRUE(is_players_turn())) {
        return(empty_reachable())
      }
      
      speed_ft <- suppressWarnings(as.integer(movement_allowance_ft()))
      if (length(speed_ft) < 1 || is.na(speed_ft) || speed_ft < 0L) {
        speed_ft <- 30L
      }
      
      used_ft <- suppressWarnings(as.integer(turn_move_ft() %||% 0L))
      if (length(used_ft) < 1 || is.na(used_ft) || used_ft < 0L) {
        used_ft <- 0L
      }
      
      remaining_ft <- max(0L, speed_ft - used_ft)
      
      if (remaining_ft < 5L) {
        return(empty_reachable())
      }
      
      pos <- active_position_row()
      
      if (!is.data.frame(pos) || nrow(pos) < 1) {
        return(empty_reachable())
      }
      
      start_x <- suppressWarnings(as.integer(pos$x[1] %||% NA))
      start_y <- suppressWarnings(as.integer(pos$y[1] %||% NA))
      
      if (length(start_x) < 1 || length(start_y) < 1 || is.na(start_x) || is.na(start_y)) {
        return(empty_reachable())
      }
      
      combat <- combat_tbl()
      
      if (!is.data.frame(combat) || nrow(combat) < 1) {
        return(empty_reachable())
      }
      
      actor_id <- if(isTRUE(is_exploration_phase()))as.character(core$state$char_id%||%"")else as.character(combat$active_actor_id[1] %||% "")
      actor_type <- if(isTRUE(is_exploration_phase()))"player"else as.character(combat$active_actor_type[1] %||% "player")
      
      if (!nzchar(actor_id)) {
        return(empty_reachable())
      }
      
      occ <- map_occupants_rv()
      tiles <- map_tiles_rv()
      
      if (!is.data.frame(tiles) || nrow(tiles) < 1) {
        return(empty_reachable())
      }
      
      can_phase <- identical(actor_type, "player") &&
        isTRUE(input$phase_move) &&
        isTRUE(can_use_phase())
      
      dirs <- expand.grid(dx = -1:1, dy = -1:1)
      dirs <- dirs[!(dirs$dx == 0 & dirs$dy == 0), , drop = FALSE]
      
      start_key <- paste(start_x, start_y, sep = ",")
      
      frontier <- data.frame(
        x = start_x,
        y = start_y,
        cost = 0L,
        path = I(list(data.frame(x = start_x, y = start_y))),
        stringsAsFactors = FALSE
      )
      
      best <- data.frame(
        key = start_key,
        cost = 0L,
        stringsAsFactors = FALSE
      )
      
      paths <- list()
      
      while (is.data.frame(frontier) && nrow(frontier) > 0) {
        
        idx <- which.min(frontier$cost)
        if (length(idx) < 1 || is.na(idx)) break
        
        cur <- frontier[idx, , drop = FALSE]
        frontier <- frontier[-idx, , drop = FALSE]
        
        cur_x <- suppressWarnings(as.integer(cur$x[1] %||% NA))
        cur_y <- suppressWarnings(as.integer(cur$y[1] %||% NA))
        cur_cost <- suppressWarnings(as.integer(cur$cost[1] %||% 0L))
        
        if (
          length(cur_x) < 1 || length(cur_y) < 1 || length(cur_cost) < 1 ||
          is.na(cur_x) || is.na(cur_y) || is.na(cur_cost)
        ) {
          next
        }
        
        for (i in seq_len(nrow(dirs))) {
          
          nx <- suppressWarnings(as.integer(cur_x + dirs$dx[i]))
          ny <- suppressWarnings(as.integer(cur_y + dirs$dy[i]))
          
          if (length(nx) < 1 || length(ny) < 1 || is.na(nx) || is.na(ny)) {
            next
          }
          
          nkey <- paste(nx, ny, sep = ",")
          
          # First reject anything not actually on the map.
          # This prevents can_enter_tile() from receiving missing/off-map tiles.
          tile_row <- tryCatch(
            get_tile_row(
              tiles = tiles,
              x = nx,
              y = ny,
              map_id = map_id()
            ),
            error = function(e) data.frame()
          )
          
          if (!is.data.frame(tile_row) || nrow(tile_row) < 1) {
            next
          }
          
          move_check <- tryCatch(
            can_enter_tile(
              tiles = tiles,
              occupants = occ,
              x = nx,
              y = ny,
              map_id = map_id(),
              exclude_actor_id = actor_id
            ),
            error = function(e) {
              list(
                ok = FALSE,
                reason = "error",
                move_cost = 1
              )
            }
          )
          
          ok <- isTRUE(move_check$ok)
          
          if (!ok) {
            
            if (isTRUE(can_phase)) {
              
              move_check$ok <- TRUE
              move_check$reason <- "phase"
              move_check$move_cost <- suppressWarnings(
                as.numeric(tile_row$move_cost[1] %||% 1)
              )
              
            } else {
              next
            }
          }
          
          move_cost <- suppressWarnings(as.numeric(move_check$move_cost %||% 1))
          
          if (length(move_cost) < 1 || is.na(move_cost) || move_cost <= 0) {
            move_cost <- 1
          }
          if (tile_in_spell_area(nx, ny, "grasping_vines")) move_cost <- move_cost * 2
          
          step_ft <- as.integer(round(move_cost * 5))
          
          if (length(step_ft) < 1 || is.na(step_ft) || step_ft < 5L) {
            step_ft <- 5L
          }
          
          new_cost <- as.integer(cur_cost + step_ft)
          
          if (length(new_cost) < 1 || is.na(new_cost)) {
            next
          }
          
          if (new_cost > remaining_ft) {
            next
          }
          
          old_best <- best[best$key == nkey, , drop = FALSE]
          
          if (
            is.data.frame(old_best) &&
            nrow(old_best) > 0 &&
            length(old_best$cost[1]) > 0 &&
            !is.na(old_best$cost[1]) &&
            old_best$cost[1] <= new_cost
          ) {
            next
          }
          
          cur_path <- cur$path[[1]]
          
          if (!is.data.frame(cur_path) || nrow(cur_path) < 1) {
            cur_path <- data.frame(x = cur_x, y = cur_y)
          }
          
          new_path <- rbind(
            cur_path,
            data.frame(x = nx, y = ny)
          )
          
          best <- best[best$key != nkey, , drop = FALSE]
          
          best <- rbind(
            best,
            data.frame(
              key = nkey,
              cost = new_cost,
              stringsAsFactors = FALSE
            )
          )
          
          paths[[nkey]] <- list(
            cost = new_cost,
            path = new_path
          )
          
          frontier <- rbind(
            frontier,
            data.frame(
              x = nx,
              y = ny,
              cost = new_cost,
              path = I(list(new_path)),
              stringsAsFactors = FALSE
            )
          )
        }
      }
      
      out <- best[best$key != start_key, , drop = FALSE]
      
      if (!is.data.frame(out) || nrow(out) < 1) {
        movement_paths(list())
        return(empty_reachable())
      }
      
      split_keys <- strsplit(out$key, ",", fixed = TRUE)
      valid <- vapply(split_keys, length, integer(1)) == 2L
      
      if (!any(valid)) {
        movement_paths(list())
        return(empty_reachable())
      }
      
      out <- out[valid, , drop = FALSE]
      split_keys <- split_keys[valid]
      
      xy <- do.call(rbind, split_keys)
      
      result <- data.frame(
        x = suppressWarnings(as.integer(xy[, 1])),
        y = suppressWarnings(as.integer(xy[, 2])),
        move_cost_ft = suppressWarnings(as.integer(out$cost)),
        stringsAsFactors = FALSE
      )
      
      result <- result[
        !is.na(result$x) &
          !is.na(result$y) &
          !is.na(result$move_cost_ft),
        ,
        drop = FALSE
      ]
      
      if (!is.data.frame(result) || nrow(result) < 1) {
        movement_paths(list())
        return(empty_reachable())
      }
      
      # Heart Eater phase may pass through occupied spaces,
      # but no actor may end movement on an occupied space.
      if (
        is.data.frame(occ) &&
        nrow(occ) > 0 &&
        all(c("x", "y") %in% names(occ))
      ) {
        occupied_keys <- paste(occ$x, occ$y, sep = ",")
        
        result <- result[
          !paste(result$x, result$y, sep = ",") %in% occupied_keys,
          ,
          drop = FALSE
        ]
      }
      
      valid_keys <- paste(result$x, result$y, sep = ",")
      paths <- paths[names(paths) %in% valid_keys]
      
      movement_paths(paths)
      
      result
    })
    
    attack_range_tiles <- reactive({
      if (!isTRUE(is_players_turn())) {
        return(data.frame(x = integer(), y = integer()))
      }
      
      pos <- active_position_row()
      if (!is.data.frame(pos) || nrow(pos) == 0) {
        return(data.frame(x = integer(), y = integer()))
      }
      
      start_x <- as.integer(pos$x[1] %||% NA)
      start_y <- as.integer(pos$y[1] %||% NA)
      if (is.na(start_x) || is.na(start_y)) {
        return(data.frame(x = integer(), y = integer()))
      }
      
      # Pass 1: melee range only.
      # Later we can pull real weapon range from selected weapon/equipped weapon.
      range_ft <- 5L
      steps <- max(1L, floor(range_ft / 5L))
      
      out <- expand.grid(dx = -steps:steps, dy = -steps:steps)
      out <- out[!(out$dx == 0 & out$dy == 0), , drop = FALSE]
      out <- out[pmax(abs(out$dx), abs(out$dy)) <= steps, , drop = FALSE]
      
      data.frame(
        x = start_x + out$dx,
        y = start_y + out$dy
      )
    })
    
    
    move_actor_to_tile <- function(target_x, target_y, forced_cost_ft = NULL, forced_path = NULL) {
      
      eid <- current_encounter_id()
      combat <- combat_tbl()
      cid <- as.character(core$state$char_id %||% "")
      
      if (!nzchar(cid) || !isTRUE(is_players_turn())) {
        log_safe("⚠️ You can only move your own character on your turn.")
        return(FALSE)
      }
      
      if (is.na(eid) || !is.data.frame(combat) || nrow(combat) < 1) {
        log_safe("⚠️ No active combat actor.")
        return(FALSE)
      }
      
      actor_id <- if(isTRUE(is_exploration_phase()))cid else as.character(combat$active_actor_id[1] %||% "")
      actor_type <- if(isTRUE(is_exploration_phase()))"player"else as.character(combat$active_actor_type[1] %||% "player")
      
      if (!identical(actor_id, cid)) {
        actor_row <- encounter_actors_tbl()
        actor_row <- actor_row[as.character(actor_row$actor_id) == actor_id, , drop = FALSE]
        owns_summon <- identical(actor_type, "summon") && nrow(actor_row) > 0L &&
          identical(as.character(actor_row$owner_actor_id[1] %||% ""), cid)
        if (!isTRUE(owns_summon)) {
          log_safe("⚠️ You can only move your character or a summon you control.")
          return(FALSE)
        }
      }
      
      pos <- active_position_row()
      
      if (!is.data.frame(pos) || nrow(pos) < 1) {
        log_safe("⚠️ Active actor has no map position.")
        return(FALSE)
      }
      
      old_x <- suppressWarnings(as.integer(pos$x[1] %||% NA))
      old_y <- suppressWarnings(as.integer(pos$y[1] %||% NA))
      
      if (is.na(old_x) || is.na(old_y)) {
        return(FALSE)
      }
      
      target_x <- suppressWarnings(as.integer(target_x))
      target_y <- suppressWarnings(as.integer(target_y))
      
      if (is.na(target_x) || is.na(target_y)) {
        return(FALSE)
      }
      
      if (identical(old_x, target_x) && identical(old_y, target_y)) {
        return(FALSE)
      }
      
      if (tile_is_occupied(
        occupants = map_occupants_rv(),
        x = target_x,
        y = target_y,
        map_id = map_id(),
        exclude_actor_id = actor_id
      )) {
        log_safe("⚠️ You cannot end movement in an occupied space.")
        return(FALSE)
      }
      
      if (!is.null(forced_cost_ft)) {
        
        total_ft <- suppressWarnings(as.integer(forced_cost_ft))
        chosen_path <- forced_path
        
      } else {
        
        reachable <- movement_range_tiles()
        
        if (!is.data.frame(reachable) || nrow(reachable) < 1) {
          log_safe("⚠️ No reachable movement tiles.")
          return(FALSE)
        }
        
        target_row <- reachable[
          reachable$x == target_x &
            reachable$y == target_y,
          ,
          drop = FALSE
        ]
        
        if (!is.data.frame(target_row) || nrow(target_row) < 1) {
          log_safe("⚠️ Destination unreachable.")
          return(FALSE)
        }
        
        total_ft <- suppressWarnings(as.integer(target_row$move_cost_ft[1] %||% NA))
        chosen_path <- movement_paths()[[paste(target_x, target_y, sep = ",")]]$path %||% NULL
      }
      
      if (length(total_ft) < 1 || is.na(total_ft) || total_ft < 0L) {
        log_safe("⚠️ Could not calculate movement cost.")
        return(FALSE)
      }
      
      remaining_ft <- suppressWarnings(
        as.integer(movement_allowance_ft() - as.integer(turn_move_ft() %||% 0L))
      )
      
      if (length(remaining_ft) < 1 || is.na(remaining_ft)) {
        remaining_ft <- 0L
      }
      
      if (total_ft > remaining_ft) {
        log_safe(paste0(
          "⚠️ Not enough movement. Needs ",
          total_ft,
          " ft; you have ",
          remaining_ft,
          " ft."
        ))
        return(FALSE)
      }

      projected_move <- as.integer(turn_move_ft() %||% 0L) + total_ft
      if (isTRUE(movement_dash()) && projected_move > base_speed_ft() && !isTRUE(dash_action_spent())) {
        if (!spend_action_safe("action", "Dash")) {
          log_safe("⚠️ You need an available action to move beyond your normal speed.")
          return(FALSE)
        }
        dash_action_spent(TRUE)
        log_safe("🏃 Dash committed as you exceed normal movement.")
      }
      
      phased_any <- isTRUE(movement_phase()) && isTRUE(can_use_phase())
      shadow_step_used <- isTRUE(phased_any) && isTRUE(is_heart_eater())
      was_hidden <- "hidden" %in% actor_conditions(actor_id)
      
      ok <- upsert_encounter_actor_position(
        encounter_id = eid,
        actor_type = actor_type,
        actor_id = actor_id,
        x = target_x,
        y = target_y
      )
      
      if (!isTRUE(ok)) {
        log_safe("⚠️ Could not move active actor.")
        return(FALSE)
      }

      if (!is.data.frame(chosen_path) || nrow(chosen_path) < 2L) {
        chosen_path <- data.frame(x = c(old_x, target_x), y = c(old_y, target_y))
      }
      rune_entries <- list()
      for (path_i in 2:nrow(chosen_path)) {
        entries <- tryCatch(trigger_rune_zone_entry(
          eid, actor_type, actor_id,
          chosen_path$x[[path_i - 1L]], chosen_path$y[[path_i - 1L]],
          chosen_path$x[[path_i]], chosen_path$y[[path_i]],
          as.integer(combat$round_number[[1L]] %||% 1L)
        ), error = function(e) { log_safe(paste("⚠️ Rune zone entry check failed:", e$message)); list() })
        rune_entries <- c(rune_entries, entries)
      }
      if (length(rune_entries)) {
        for (entry in rune_entries) log_safe(paste0("✧ ", entry$name, " strikes for ", entry$damage, " ", entry$damage_type, " damage on entry."))
        refresh_key(refresh_key() + 1L)
        events_key(events_key() + 1L)
      }

      if (isTRUE(was_hidden)) attempt_hide(movement_recheck=TRUE,x=target_x,y=target_y)

      if (!identical(cunning_mode(), "disengage")) {
        # Shadow Step is a teleport: only creatures threatening the departure
        # square may react. Creatures between origin and destination are never
        # treated as having been passed in melee.
        attacker_parts <- if (isTRUE(shadow_step_used)) {
          list(tryCatch(
            get_opportunity_attackers(eid, actor_id, actor_type, old_x, old_y, target_x, target_y),
            error = function(e) data.frame()
          ))
        } else {
          lapply(2:nrow(chosen_path), function(path_i) tryCatch(
            get_opportunity_attackers(
              eid, actor_id, actor_type,
              chosen_path$x[[path_i - 1L]], chosen_path$y[[path_i - 1L]],
              chosen_path$x[[path_i]], chosen_path$y[[path_i]]
            ), error = function(e) data.frame()
          ))
        }
        attacker_parts <- Filter(function(x) is.data.frame(x) && nrow(x), attacker_parts)
        attackers <- if (length(attacker_parts)) do.call(rbind, attacker_parts) else data.frame()
        if (nrow(attackers)) attackers <- attackers[!duplicated(as.character(attackers$actor_id)), , drop = FALSE]
        if (is.data.frame(attackers) && nrow(attackers)) {
          attacker_names <- unique(as.character(attackers$display_name %||% attackers$name %||% "Enemy"))
          log_game_event(
            eid, "opportunity_available", actor_type, actor_id,
            payload = list(attacker_ids = as.list(as.character(attackers$actor_id)),
                           attacker_names = as.list(attacker_names))
          )
          log_safe(paste0("⚔️ Opportunity attack available to: ", paste(attacker_names, collapse = ", "), "."))
        }
      }
      
      log_game_event(
        encounter_id = eid,
        event_type = "move",
        actor_type = actor_type,
        actor_id = actor_id,
        payload = list(
          from = list(x = old_x, y = old_y),
          to = list(x = target_x, y = target_y),
          move_cost_ft = total_ft,
          phased = phased_any,
          shadow_step = shadow_step_used,
          dash = isTRUE(movement_dash()),
          path = lapply(seq_len(nrow(chosen_path)), function(i) list(x = chosen_path$x[[i]], y = chosen_path$y[[i]])),
          atomic_click_move = TRUE
        )
      )
      
      if(isTRUE(is_exploration_phase()))turn_move_ft(0L)else turn_move_ft(as.integer(turn_move_ft()+total_ft))
      
      pending_move(NULL)
      
      log_safe(paste0(
        if (phased_any) "👻 " else "🧭 ",
        "Moved to (", target_x, ", ", target_y, "). Cost: ", total_ft, " ft."
      ))
      
      bump_positions()
      bump_map_visual()
      
      TRUE
    }
    # --------------------------------------------------
    # Initiative
    # --------------------------------------------------
    
    observeEvent(input$move_to_tile, {
      
      target_x <- suppressWarnings(as.integer(input$move_to_tile$x %||% NA))
      target_y <- suppressWarnings(as.integer(input$move_to_tile$y %||% NA))
      
      if (is.na(target_x) || is.na(target_y)) return()
      if(place_ward_at_map_point(target_x,target_y))return()
      if(release_rune_at_map_point(target_x,target_y))return()
      
      pm <- pending_move()
      
      # Second click on same tile confirms
      if (
        !is.null(pm) &&
        identical(as.integer(pm$x), target_x) &&
        identical(as.integer(pm$y), target_y)
      ) {
        move_actor_to_tile(
          target_x,
          target_y,
          forced_cost_ft = pm$cost_ft,
          forced_path = pm$path
        )
        
        pending_move(NULL)
        return()
      }
      
      pos <- active_position_row()
      
      if (!is.data.frame(pos) || nrow(pos) < 1) {
        log_safe("⚠️ Active actor has no map position.")
        return()
      }
      
      old_x <- suppressWarnings(as.integer(pos$x[1] %||% NA))
      old_y <- suppressWarnings(as.integer(pos$y[1] %||% NA))
      
      if (is.na(old_x) || is.na(old_y)) return()
      
      speed_ft <- movement_allowance_ft()
      used_ft <- as.integer(turn_move_ft() %||% 0L)
      remaining_ft <- max(0L, speed_ft - used_ft)
      is_shadow_step <- isTRUE(movement_phase()) && isTRUE(can_use_phase()) && isTRUE(is_heart_eater())
      if (isTRUE(is_shadow_step)) {
        destination <- get_tile_row(map_tiles_rv(), target_x, target_y, map_id = map_id())
        occupied <- tile_is_occupied(
          map_occupants_rv(), target_x, target_y, map_id = map_id(),
          exclude_actor_id = active_actor_id()
        )
        teleport_cost <- combat_grid_distance_ft(old_x, old_y, target_x, target_y)
        path_result <- list(
          ok = nrow(destination) > 0L && !isTRUE(occupied) && teleport_cost <= remaining_ft,
          reason = if (isTRUE(occupied)) "occupied" else if (!nrow(destination)) "out_of_bounds" else if (teleport_cost > remaining_ft) "unreachable" else "ok",
          cost_ft = teleport_cost,
          path = data.frame(x = c(old_x, target_x), y = c(old_y, target_y), step = 0:1)
        )
      } else {
        path_result <- combat_grid_shortest_path(
          map_tiles_rv(), map_occupants_rv(), old_x, old_y, target_x, target_y,
          map_id = map_id(), exclude_actor_id = active_actor_id(),
          allow_blocked = isTRUE(movement_phase()) && isTRUE(can_use_phase()),
          max_cost_ft = remaining_ft
        )
      }
      if (!isTRUE(path_result$ok)) {
        log_safe(if (identical(path_result$reason, "occupied")) "⚠️ You cannot end movement in an occupied space." else "⚠️ No legal path reaches that tile within your remaining movement.")
        pending_move(NULL)
        return()
      }
      cost_ft <- as.integer(path_result$cost_ft)
      
      if (cost_ft > remaining_ft) {
        log_safe(paste0(
          "⚠️ Destination too far. Needs ",
          cost_ft,
          " ft; you have ",
          remaining_ft,
          " ft."
        ))
        return()
      }
      
      if (tile_is_occupied(
        occupants = map_occupants_rv(),
        x = target_x,
        y = target_y,
        map_id = map_id(),
        exclude_actor_id = active_actor_id()
      )) {
        log_safe("⚠️ You cannot end movement in an occupied space.")
        return()
      }
      
      pending_move(list(
        x = target_x,
        y = target_y,
        cost_ft = cost_ft,
        path = path_result$path,
        dash = isTRUE(movement_dash()),
        phase = isTRUE(movement_phase()) && isTRUE(can_use_phase()),
        shadow_step = isTRUE(is_shadow_step)
      ))
      
      log_safe(paste0(
        "🟨 Move preview: (",
        target_x,
        ", ",
        target_y,
        ") — ",
        cost_ft,
        " ft. Click again to confirm."
      ))
      
      bump_map_visual()
      
    }, ignoreInit = TRUE)
    

    

    
    class_combat_actions <- reactive({
      char <- core$state$char
      if (is.null(char)) return(list())
      actions <- get_unlocked_combat_actions(char)
      classes <- normalise_character_classes(char)
      class_levels <- stats::setNames(
        vapply(classes, function(entry) as.integer(entry$level %||% 0L), integer(1)),
        vapply(classes, function(entry) as.character(entry$class %||% ""), character(1))
      )
      spells <- get_unlocked_class_spells(char)
      offensive <- Filter(function(spell) {
        as.integer(spell$level %||% 0L) %in% c(5L, 6L) && is.list(spell$damage)
      }, spells)
      if (length(offensive)) {
        spell_actions <- lapply(offensive, function(spell) {
          level <- as.integer(class_levels[[as.character(spell$class)]] %||% spell$level)
          spell <- scale_class_spell(spell, level)
          dice <- as.character(spell$resolved_upgrade$damage_dice %||% spell$damage$dice %||% "1d8")
          list(
            id = paste0("spell_", gsub("[^a-z0-9]+", "_", tolower(spell$name))),
            name = spell$name, desc = spell$description,
            action = list(
              name = spell$name,
              target = if (identical(as.character(spell$target$type %||% ""), "area")) "area_self" else "enemy",
              radius_ft = as.integer(spell$target$size_ft %||% 0L),
              action_type = spell$action_type,
              damage = list(mode = "dice", value = dice, type = spell$damage$type),
              resource = list(name = "sindre", cost = spell$cost),
              resolution = spell$resolution, effects = spell$effects %||% list()
            ),
            spell = spell
          )
        })
        actions <- c(actions, spell_actions)
      }
      actions
    })

    output$class_actions_ui <- renderUI({
      actions <- class_combat_actions()
      if (!length(actions)) return(NULL)
      actionButton(
        session$ns("open_class_action"),
        paste0("Abilities (", length(actions), ")"),
        class = "btn btn-primary"
      )
    })

    output$rogue_combat_ui <- renderUI({
      expr <- get_sneak_attack_expr(core$state$char)
      if (!nzchar(expr)) return(NULL)
      ready <- offhand_ready()
      tagList(
        div(
          class = "combat-pill",
          paste0("Sneak Attack ", expr, " • ", if (isTRUE(sneak_attack_used())) "Used" else "Ready")
        ),
        if (!is.null(ready) && isTRUE(is_players_turn())) {
          actionButton(session$ns("use_offhand_attack"), "Off-hand Attack (Bonus)", class = "btn btn-primary")
        }
      )
    })

    observeEvent(input$use_offhand_attack, {
      ready <- offhand_ready()
      if (is.null(ready) || !isTRUE(is_players_turn())) {
        log_safe("⚠️ Make a light-weapon attack first before using an off-hand attack.")
        return()
      }
      if (as.integer(turn_budget()$bonus_actions %||% 0L) < 1L) {
        log_safe("⚠️ No bonus action remains for an off-hand attack.")
        return()
      }
      open_attack_flow(as.character(ready$target_id), attack_mode = "offhand")
    }, ignoreInit = TRUE)

    observeEvent(input$open_class_action, {
      if (!isTRUE(is_players_turn())) {
        log_safe("⚠️ Combat abilities can only be used on your turn.")
        return()
      }

      actions <- class_combat_actions()
      if (!length(actions)) return()
      actors <- encounter_actors_tbl()
      targets <- actors[as.character(actors$actor_type %||% "") == "enemy", , drop = FALSE]
      needs_enemy <- any(vapply(actions, function(feature) {
        identical(as.character(feature$action$target %||% "enemy"), "enemy")
      }, logical(1)))
      has_self_action <- any(vapply(actions, function(feature) {
        identical(as.character(feature$action$target %||% "enemy"), "self")
      }, logical(1)))
      if (needs_enemy && !has_self_action && (!is.data.frame(targets) || nrow(targets) == 0L)) {
        log_safe("⚠️ There are no enemy targets in this encounter.")
        return()
      }

      action_choices <- stats::setNames(
        as.character(seq_along(actions)),
        vapply(actions, function(feature) as.character(feature$action$name %||% feature$name), character(1))
      )
      showModal(modalDialog(
        title = "Use Combat Ability",
        selectInput(session$ns("class_action_index"), "Ability", choices = action_choices),
        uiOutput(session$ns("class_action_target_ui")),
        uiOutput(session$ns("class_action_preview_ui")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_class_action"), "Use Ability", class = "btn btn-danger")
        ),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)

    output$class_action_target_ui <- renderUI({
      actions <- class_combat_actions()
      idx <- suppressWarnings(as.integer(input$class_action_index %||% 1L))
      if (is.na(idx) || idx < 1L || idx > length(actions)) return(NULL)
      action <- actions[[idx]]$action
      required_condition<-tolower(as.character(action$required_target_condition%||%""))
      if (identical(as.character(action$target %||% "enemy"), "self")) {
        return(tags$p(class = "confirm-note", "Target: Self"))
      }
      if (identical(as.character(action$target %||% "enemy"), "area_self")) {
        return(tags$p(class = "confirm-note", paste0("Target: enemies within ", action$radius_ft %||% 0L, " feet")))
      }
      actors <- encounter_actors_tbl()
      targets <- actors[as.character(actors$actor_type %||% "") == "enemy", , drop = FALSE]
      if(nzchar(required_condition)&&nrow(targets))targets<-targets[vapply(as.character(targets$actor_id),function(id)required_condition%in%tolower(actor_conditions(id)),logical(1)),,drop=FALSE]
      if (!is.data.frame(targets) || nrow(targets) == 0L) {
        message <- if (nzchar(required_condition)) {
          paste0("No ", required_condition, " enemy targets are available.")
        } else {
          "No enemy targets are available."
        }
        return(tags$p(class = "confirm-note", message))
      }
      selectInput(
        session$ns("class_action_target"), "Target",
        choices = stats::setNames(as.character(targets$actor_id), as.character(targets$display_name %||% targets$actor_id))
      )
    })

    output$class_action_preview_ui <- renderUI({
      actions <- class_combat_actions()
      idx <- suppressWarnings(as.integer(input$class_action_index %||% 1L))
      if (is.na(idx) || idx < 1L || idx > length(actions)) return(NULL)
      feature <- actions[[idx]]
      action <- feature$action
      resource <- action$resource %||% list()
      div(
        class = "confirm-box",
        tags$strong(action$name %||% feature$name),
        tags$p(feature$desc),
        if (length(resource)) tags$p(
          tags$strong("Cost: "), resource$cost %||% 0L, " ", tools::toTitleCase(resource$name %||% "resource")
        ),
        if (nzchar(action$note %||% "")) tags$p(class = "confirm-note", action$note),
        if (nzchar(as.character(action$required_target_condition %||% ""))) {
          tags$p(
            class = "confirm-note",
            paste("Requires target condition:", tools::toTitleCase(action$required_target_condition))
          )
        },
        if(!is.null(action$range_ft))tags$p(class="confirm-note",paste0("Range: ",action$range_ft," ft"))
      )
    })

    observeEvent(input$confirm_class_action, {
      req(isTRUE(is_players_turn()))
      actions <- class_combat_actions()
      idx <- suppressWarnings(as.integer(input$class_action_index %||% NA))
      if (is.na(idx) || idx < 1L || idx > length(actions)) return()

      feature <- actions[[idx]]
      action <- feature$action
      target_mode <- as.character(action$target %||% "enemy")
      target_id <- if (target_mode %in% c("self", "area_self")) {
        as.character(core$state$char_id %||% "self")
      } else as.character(input$class_action_target %||% "")
      if (!nzchar(target_id)) return()
      required_condition<-tolower(as.character(action$required_target_condition%||%""));if(nzchar(required_condition)&&!required_condition%in%tolower(actor_conditions(target_id))){log_safe(paste0("⚠️ ",action$name%||%feature$name," requires the target to be ",required_condition,"."));return()}
      if(target_mode=="enemy"&&!is.null(action$range_ft)){
        self<-get_actor_row(as.character(core$state$char_id),"player");target<-get_actor_row(target_id,"enemy")
        if(!nrow(self)||!nrow(target))return(log_safe("⚠️ The combatants' map positions are unavailable."))
        geometry<-combat_attack_geometry(map_tiles_rv(),self$x[[1L]],self$y[[1L]],target$x[[1L]],target$y[[1L]],action$range_ft,action$long_range_ft%||%action$range_ft,map_id())
        if(!isTRUE(geometry$in_range))return(log_safe(paste0("⚠️ ",action$name%||%feature$name," is out of range (",geometry$distance_ft," ft; maximum ",action$long_range_ft%||%action$range_ft," ft).")))
        if(!isTRUE(geometry$line_clear))return(log_safe(paste0("⚠️ A wall blocks ",action$name%||%feature$name,".")))
      }
      damage <- action$damage %||% list()

      char <- validate_character(core$state$char)
      if (!class_action_use_available(char, action)) {
        log_safe(paste0("⚠️ ", action$name %||% feature$name, " has already been used and needs a rest."))
        return()
      }
      resource <- action$resource %||% list()
      if (length(resource) && identical(as.character(resource$name %||% ""), "sindre")) {
        cost <- suppressWarnings(as.integer(resource$cost %||% 0L))
        available <- suppressWarnings(as.integer(char$resources$sindre$cur %||% 0L))
        if (is.na(cost)) cost <- 0L
        if (is.na(available)) available <- 0L
        if (available < cost) {
          log_safe("⚠️ Not enough Sindre for that ability.")
          return()
        }
        char$resources$sindre$cur <- available - cost
      }
      action_type <- as.character(action$action_type %||% "action")
      if (!spend_action_safe(action_type, action$name %||% feature$name)) return()

      if (identical(target_mode, "self") && is.list(action$healing)) {
        healing <- resolve_class_action_healing(action, char)
        char <- mark_class_action_used(char, action)
        core$state$char <- char
        result <- apply_healing_to_state(core$state, healing)
        if (is.null(result)) {
          log_safe("⚠️ The healing ability could not be applied.")
          return()
        }
        ability_name <- as.character(action$name %||% feature$name)
        gained <- as.integer(result$hp_after %||% 0L) - as.integer(result$hp_before %||% 0L)
        log_game_event(
          encounter_id = current_encounter_id(), event_type = "ability",
          actor_type = "player", actor_id = as.character(core$state$char_id %||% ""),
          target_id = target_id,
          payload = list(ability_name = ability_name, healing_rolled = healing, healing = gained)
        )
        removeModal()
        log_safe(paste0("✨ ", ability_name, " restores ", gained, " HP."))
        bump_refresh()
        return()
      }

      if (identical(target_mode, "area_self")) {
        actors <- encounter_actors_tbl()
        caster <- actors[as.character(actors$actor_id) == target_id, , drop = FALSE]
        targets <- actors[as.character(actors$actor_type) == "enemy", , drop = FALSE]
        radius <- as.integer(action$radius_ft %||% 0L)
        affected <- character()
        total_damage <- 0L
        enemies <- snapshot_data()$enemies %||% data.frame()
        if (nrow(caster) && nrow(targets)) for (i in seq_len(nrow(targets))) {
          row <- targets[i, , drop = FALSE]
          distance <- max(abs(as.integer(row$x[1]) - as.integer(caster$x[1])),
                          abs(as.integer(row$y[1]) - as.integer(caster$y[1]))) * 5L
          if (is.na(distance) || distance > radius) next
          enemy_id <- as.character(row$actor_id[1])
          max_hp <- as.integer(row$hp_max[1] %||% row$max_hp[1] %||% 1L)
          rolled <- resolve_class_action_damage(action, max_hp, char)
          amount <- rolled$amount
          ability <- as.character(action$resolution$ability %||% "con")
          enemy <- enemies[as.character(enemies$enemy_uuid %||% "") == enemy_id, , drop = FALSE]
          save_col <- paste0(ability, "_save")
          save_mod <- if (nrow(enemy) && save_col %in% names(enemy)) as.integer(enemy[[save_col]][1] %||% 0L) else 0L
          dc <- class_spell_save_dc(char, feature$spell)
          if (sample.int(20L, 1L) + save_mod >= dc) amount <- floor(amount / 2L)
          target_char <- load_actor_for_combat(enemy_id, "enemy")
          traits <- spellsword_traits(get_character_damage_traits(target_char), rolled$damage_type, char)
          adjusted <- apply_damage_traits_to_parts(list(list(total = amount, type = rolled$damage_type)), traits)
          dealt <- as.integer(adjusted$total %||% amount)
          damage_encounter_enemy(current_encounter_id(), enemy_id, dealt)
          affected <- c(affected, as.character(row$display_name[1] %||% enemy_id))
          total_damage <- total_damage + dealt
        }
        core$state$char <- mark_class_action_used(char, action)
        removeModal()
        log_safe(if (length(affected)) {
          paste0("✨ ", action$name, " hits ", paste(affected, collapse = ", "), " for ", total_damage, " total damage.")
        } else paste0("✨ ", action$name, " finds no enemy within range."))
        bump_refresh()
        return()
      }

      target_row <- get_actor_row(target_id, "enemy")
      if (!is.data.frame(target_row) || nrow(target_row) == 0L) {
        log_safe("⚠️ That target is no longer available.")
        removeModal()
        return()
      }

      max_hp <- suppressWarnings(as.integer(target_row$hp_max[1] %||% target_row$max_hp[1] %||% 1L))
      resolved_damage <- resolve_class_action_damage(action, max_hp, char)
      raw_damage <- resolved_damage$amount

      save_succeeded <- FALSE
      resolution_type <- as.character(action$resolution$type %||% "")
      if (identical(resolution_type, "spell_attack")) {
        spell_attack <- sample.int(20L, 1L) + character_proficiency_bonus(char) +
          floor((as.integer(char$abilities$bld_str %||% 10L) - 10L) / 2L)
        target_ac <- as.integer(target_row$ac[1] %||% 10L)
        if (spell_attack < target_ac) raw_damage <- 0L
        log_safe(paste0("🎲 Spell attack ", spell_attack, " vs AC ", target_ac,
                        if (raw_damage > 0L) " — hit." else " — miss."))
      } else if (is.list(action$resolution) && identical(resolution_type, "saving_throw")) {
        ability <- as.character(action$resolution$ability %||% "con")
        enemies <- snapshot_data()$enemies %||% data.frame()
        enemy <- enemies[as.character(enemies$enemy_uuid %||% "") == target_id, , drop = FALSE]
        save_col <- paste0(ability, "_save")
        save_mod <- if (nrow(enemy) && save_col %in% names(enemy)) as.integer(enemy[[save_col]][1] %||% 0L) else 0L
        save_roll <- sample.int(20L, 1L)
        save_dc <- class_spell_save_dc(char, feature$spell %||% list(class = feature$class %||% ""))
        save_succeeded <- save_roll + save_mod >= save_dc
        if (save_succeeded && grepl("half_damage", as.character(action$resolution$on_success %||% ""), fixed = TRUE)) {
          raw_damage <- floor(raw_damage / 2L)
        }
        log_safe(paste0("🎲 ", get_actor_display_name(target_id, "enemy"), " rolls ", save_roll + save_mod,
                        " vs spell DC ", save_dc, if (save_succeeded) " — success." else " — failure."))
      }

      target_char <- load_actor_for_combat(target_id, "enemy")
      traits <- get_character_damage_traits(target_char)
      traits <- spellsword_traits(traits, resolved_damage$damage_type, char)
      adjusted <- apply_damage_traits_to_parts(
        list(list(total = raw_damage, type = resolved_damage$damage_type)),
        traits
      )
      final_damage <- as.integer(adjusted$total %||% raw_damage)
      eid <- current_encounter_id()
      result <- tryCatch(
        damage_encounter_enemy(eid, target_id, final_damage),
        error = function(e) NULL
      )
      if (is.null(result)) {
        log_safe("⚠️ The ability could not be applied.")
        return()
      }

      if (!save_succeeded && raw_damage > 0L && length(action$effects %||% list())) {
        round_number <- as.integer(combat_tbl()$round_number[1] %||% 1L)
        for (effect in action$effects) {
          condition <- switch(
            as.character(effect$type %||% ""),
            speed_modifier = "slowed", healing_block = "healing_blocked",
            condition = as.character(effect$value %||% ""),
            condition_save = {
              ability <- as.character(effect$save %||% "con")
              enemies <- snapshot_data()$enemies %||% data.frame()
              enemy <- enemies[as.character(enemies$enemy_uuid %||% "") == target_id, , drop = FALSE]
              save_col <- paste0(ability, "_save")
              save_mod <- if (nrow(enemy) && save_col %in% names(enemy)) as.integer(enemy[[save_col]][1] %||% 0L) else 0L
              dc <- class_spell_save_dc(char, feature$spell)
              if (sample.int(20L, 1L) + save_mod < dc) as.character(effect$value %||% "") else ""
            },
            ""
          )
          if (!nzchar(condition)) next
          create_encounter_effect(
            current_encounter_id(), "player", as.character(core$state$char_id %||% ""),
            as.character(feature$id %||% "class_spell"), "condition",
            payload = list(condition = condition, speed_modifier = effect$value_ft %||% NULL),
            target_actor_type = "enemy", target_actor_id = target_id,
            starts_round = round_number, ends_round = round_number + 1L
          )
        }
      }

      core$state$char <- mark_class_action_used(char, action)
      ability_name <- as.character(action$name %||% feature$name)
      target_name <- get_actor_display_name(target_id, "enemy")
      log_game_event(
        encounter_id = eid,
        event_type = "ability",
        actor_type = "player",
        actor_id = as.character(core$state$char_id %||% ""),
        target_id = target_id,
        payload = list(
          ability_name = ability_name,
          target_name = target_name,
          raw_damage = raw_damage,
          damage = final_damage,
          damage_type = resolved_damage$damage_type,
          resource_cost = resource$cost %||% 0L
        )
      )
      removeModal()
      log_safe(paste0("✨ ", ability_name, " deals ", final_damage, " damage to ", target_name, "."))
      bump_refresh()
    }, ignoreInit = TRUE)

    output$initiative_ui <- renderUI({
      actors <- encounter_actors_tbl()
      aid <- active_actor_id()
      
      if (!is.data.frame(actors) || nrow(actors) == 0) {
        return(tagList(
          div(class = "combat-section-title", "Initiative"),
          div("No actors in this encounter.")
        ))
      }
      
      if ("turn_order" %in% names(actors)) {
        actors <- actors[order(actors$turn_order, na.last = TRUE), , drop = FALSE]
      }
      
      rows <- lapply(seq_len(nrow(actors)), function(i) {
        row <- actors[i, , drop = FALSE]
        
        cid <- as.character(row$actor_id[1] %||% "")
        ctype <- as.character(row$actor_type[1] %||% "actor")
        nm <- as.character(row$display_name[1] %||% "Unknown")
        turn_order <- as.character(row$turn_order[1] %||% "—")
        initiative <- as.character(row$initiative[1] %||% "—")
        cur_hp  <- suppressWarnings(as.integer(row$current_hp[1] %||% row$hp_current[1] %||% 0))
        temp_hp <- suppressWarnings(as.integer(row$temp_hp[1] %||% 0))
        max_hp  <- suppressWarnings(as.integer(row$hp_max[1] %||% row$max_hp[1] %||% NA))
        
        if (is.na(cur_hp)) cur_hp <- 0L
        if (is.na(temp_hp)) temp_hp <- 0L
        
        if (is.na(max_hp) || max_hp < 1L) {
          max_hp <- max(cur_hp, 1L)
        }
        is_active <- isTRUE(row$is_active[1] %||% FALSE)
        
        ac_txt <- if (identical(ctype, "player")) {
          char_obj <- load_actor_for_combat(cid, "player")
          ac_val <- tryCatch(calc_auto_ac_for_char(char_obj), error = function(e) NA_integer_)
          if (!is.na(ac_val)) paste0("AC ", ac_val) else "AC ?"
        } else {
          format_known_ac(cid)
        }
        
        div(
          class = paste("initiative-row", if (!is.null(aid) && identical(cid, aid)) "active"),
          div(
            class = "initiative-top",
            div(
              div(class = "initiative-name", nm),
              div(class = "initiative-sub", paste("Order", turn_order, "• Initiative", initiative, "•", ctype))
            )
          ),
          div(
            class = "initiative-tags",
            div(class = "initiative-tag", if (is_active) "Active" else "Inactive"),
            div(class = "initiative-tag", ac_txt)
          ),
          hp_bar_ui(cur_hp, temp_hp, maxv = max_hp)
        )
      })
      
      tagList(
        div(class = "combat-section-title", "Initiative"),
        div(class = "initiative-list", rows)
      )
    })
    
    

    
    observeEvent(input$map_fullscreen_exit, {
      map_fullscreen(FALSE)
      
      later::later(function() {
        session$sendCustomMessage(
          "combat3d-resize",
          list(containerId = session$ns("combat_3d_canvas"))
        )
      }, delay = 0.15)
    }, ignoreInit = TRUE)
    
    
    output$combat_layout_ui <- renderUI({
      combat <- combat_tbl()
      phase<-if(is.data.frame(combat)&&nrow(combat))tolower(as.character(combat$phase[[1L]]%||%""))else""
      if (!phase%in%c("combat","exploration")) {
        return(div(class = "combat-card", style = "padding:32px;text-align:center;",
                   tags$h3("No active combat"),
                   tags$p("The DM will start an encounter when combat begins.")))
      }
      tagList(if(phase=="exploration")div(class="combat-card",style="padding:12px 16px;margin-bottom:10px;",tags$strong("Exploration map"),tags$p(style="margin:3px 0 0;","Move your own token and place prepared wards. Enemies and combat actions remain hidden until the DM starts combat."))else NULL,div(
        class = "combat-play-layout",
        
        div(
          class = "combat-card combat-map-card combat-map-card-large",
          uiOutput(session$ns("map_ui"))
        ),
        
        div(
          class = "combat-card combat-log-card combat-log-card-bottom",
          uiOutput(session$ns("log_ui"))
        )
      ))
    })
    
    
    format_event_text_fast <- function(ev_row, actor_name_lookup) {
      typ <- as.character(ev_row$event_type[1] %||% "event")
      actor_id <- as.character(ev_row$actor_id[1] %||% "")
      target_id <- as.character(ev_row$target_id[1] %||% "")
      
      payload <- NULL
      if ("payload" %in% names(ev_row)) {
        payload <- safe_event_payload(ev_row$payload[[1]])
      }
      
      actor_name <- if (nzchar(actor_id)) actor_name_lookup(actor_id) else "Actor"
      target_name <- if (nzchar(target_id)) actor_name_lookup(target_id) else "Target"
      
      if (typ == "enter_combat") {
        return("Combat begins.")
      }
      
      if (typ == "initiative") {
        if (is.list(payload)) {
          return(paste0(
            "Initiative rolled. ",
            payload$top_actor %||% "Someone",
            " leads on ",
            payload$top_score %||% "?",
            "."
          ))
        }
        
        return("Initiative rolled.")
      }

      if (typ == "ability") {
        if (is.list(payload)) {
          return(paste0(
            actor_name, " uses ", payload$ability_name %||% "an ability",
            " on ", payload$target_name %||% target_name,
            " for ", payload$damage %||% 0, " ",
            payload$damage_type %||% "", " damage."
          ))
        }
        return(paste0(actor_name, " uses an ability."))
      }
      
      if (typ == "move") {
        if (is.list(payload) && !is.null(payload$to)) {
          tx <- payload$to$x %||% "?"
          ty <- payload$to$y %||% "?"
          cost <- payload$move_cost_ft %||% "?"
          
          prefix <- if (isTRUE(payload$phased %||% FALSE)) {
            paste0(actor_name, " phases")
          } else if (isTRUE(payload$dash %||% FALSE)) {
            paste0(actor_name, " dashes")
          } else {
            paste0(actor_name, " moves")
          }
          
          return(paste0(
            prefix,
            " to (", tx, ", ", ty, ").",
            " Cost: ", cost, " ft."
          ))
        }
        
        return(paste0(actor_name, " moves."))
      }
      
      if (typ == "heal") {
        if (is.list(payload) && !is.null(payload$amount)) {
          amt <- payload$amount %||% "?"
          before <- payload$hp_before %||% "?"
          after <- payload$hp_after %||% "?"
          
          return(paste0(
            target_name,
            " heals ",
            amt,
            " HP (",
            before,
            " → ",
            after,
            ")."
          ))
        }
        
        return(paste0(target_name, " is healed."))
      }
      
      if (typ == "damage") {
        if (is.list(payload) && !is.null(payload$amount)) {
          amt <- payload$amount %||% "?"
          before <- payload$hp_before %||% "?"
          after <- payload$hp_after %||% "?"
          
          return(paste0(
            target_name,
            " takes ",
            amt,
            " damage (HP ",
            before,
            " → ",
            after,
            ")."
          ))
        }
        
        return("Damage was dealt.")
      }
      
      if (typ == "defeated") {
        return(paste0(target_name, " is defeated."))
      }

      if (typ %in% c("condition_added", "condition_removed")) {
        condition <- as.character(payload$condition %||% "condition")
        verb <- if (identical(typ, "condition_added")) "gains" else "loses"
        return(paste(target_name, verb, condition, "."))
      }

      if (typ == "opportunity_available") {
        names <- as.character(unlist(payload$attacker_names %||% list()))
        who <- if (length(names) && any(nzchar(names))) paste(names[nzchar(names)], collapse = ", ") else "an adjacent player"
        return(paste0(actor_name, " leaves melee reach without Disengaging — opportunity attack available to ", who, "."))
      }
      
      if (typ == "attack") {
        if (is.list(payload)) {
          wpn <- payload$weapon_name %||% "Weapon"
          
          opp_txt <- if (isTRUE(payload$is_opportunity_attack %||% FALSE)) {
            " makes an opportunity attack and"
          } else {
            ""
          }
          
          crit_txt <- if (isTRUE(payload$is_crit %||% FALSE)) {
            " critically hits "
          } else {
            " hits "
          }
          
          if (isTRUE(payload$is_hit %||% FALSE)) {
            dmg <- payload$final_damage %||% payload$damage_total %||% "?"
            extra <- if (isTRUE(payload$used_sneak_attack %||% FALSE)) " + Sneak Attack" else ""
            
            return(paste0(
              actor_name,
              opp_txt,
              crit_txt,
              target_name,
              " with ",
              wpn,
              extra,
              " for ",
              dmg,
              " damage."
            ))
          }
          
          return(paste0(
            actor_name,
            opp_txt,
            " misses ",
            target_name,
            " with ",
            wpn,
            " (",
            payload$attack_total %||% "?",
            " vs AC ",
            payload$target_ac %||% "?",
            ")."
          ))
        }
        
        return(paste0(actor_name, " attacks ", target_name, "."))
      }
      
      if (typ == "end_turn") {
        if (is.list(payload)) {
          from_name <- if (nzchar(as.character(payload$from_actor_id %||% ""))) {
            actor_name_lookup(payload$from_actor_id)
          } else {
            actor_name
          }
          
          to_name <- if (nzchar(as.character(payload$to_actor_id %||% ""))) {
            actor_name_lookup(payload$to_actor_id)
          } else {
            "Next actor"
          }
          
          return(paste0(from_name, " ends turn. ", to_name, " acts next."))
        }
        
        return("Turn advanced.")
      }
      
      typ
    }
    
    # --------------------------------------------------
    # Attack flow
    # --------------------------------------------------
    
    resolve_attack_adv_mode <- function(mode = "auto", attacker_id = "", target_id = "") {
      mode <- as.character(mode %||% "auto")
      
      if (identical(mode, "advantage")) return("Advantage")
      if (identical(mode, "disadvantage")) return("Disadvantage")
      if (identical(mode, "normal")) return("Normal")

      if (identical(cunning_mode(), "hide")) return("Advantage")

      attacker_effects <- actor_conditions(attacker_id)
      target_effects <- actor_conditions(target_id)
      has_advantage <- any(c("restrained", "hidden", "invisible") %in% attacker_effects) ||
        "helped_against" %in% target_effects
      has_disadvantage <- any(c("restrained", "poisoned") %in% attacker_effects) ||
        any(c("dodging", "hidden", "invisible") %in% target_effects)
      if (has_advantage && has_disadvantage) return("Normal")
      if (has_advantage) return("Advantage")
      if (has_disadvantage) return("Disadvantage")
      
      # Future automatic rules go here:
      # - target has cover -> "Disadvantage"
      # - attacker invisible -> "Advantage"
      # - prone/ranged/etc.
      "Normal"
    }
    
    open_attack_flow <- function(target_id, attack_mode = "action", verified_opportunity = FALSE) {
      combat_now <- combat_tbl()
      if (!is.data.frame(combat_now) || !nrow(combat_now) ||
          !identical(as.character(combat_now$phase[1] %||% ""), "combat")) {
        log_safe("⚠️ The DM must start combat before attacks can be made.")
        return()
      }
      attack_mode <- if (identical(as.character(attack_mode), "offhand")) "offhand" else "action"
      current_attack_mode(attack_mode)
      
      self_id <- as.character(core$state$char_id %||% "")
      is_reaction_attack <- !isTRUE(is_players_turn())
      is_ready <- is_reaction_attack && "readied" %in% actor_conditions(self_id)
      is_opp <- is_reaction_attack && !is_ready
      current_attack_is_opp(isTRUE(is_opp))
      current_attack_is_ready(isTRUE(is_ready))
      
      if (isTRUE(is_reaction_attack)) {
        if (!isTRUE(player_reaction_available())) {
          log_safe("⚠️ Your reaction has already been used this round.")
          return()
        }
        actors <- encounter_actors_tbl()
        self <- actors[as.character(actors$actor_id) == self_id, , drop = FALSE]
        target <- actors[as.character(actors$actor_id) == as.character(target_id), , drop = FALSE]
        adjacent <- nrow(self) && nrow(target) && is_adjacent_5ft(self$x[1], self$y[1], target$x[1], target$y[1])
        if (isTRUE(is_opp) && !isTRUE(verified_opportunity) && !isTRUE(adjacent)) {
          log_safe("⚠️ An opportunity attack requires the target to be within your reach.")
          return()
        }
        log_safe(if (isTRUE(is_ready)) "⏱️ Resolving your readied attack as a reaction."
                 else "⚠️ This is not your turn. This will be treated as an opportunity attack.")
      }
      selected_target_id(as.character(target_id))
      
      attacker_id <- if (isTRUE(is_reaction_attack)) self_id else active_actor_id()
      attacker_type <- if (isTRUE(is_reaction_attack)) "player" else as.character(combat_tbl()$active_actor_type[1] %||% "")
      
      
      if (is.null(attacker_id) || !nzchar(as.character(attacker_id))) {
        log_safe("⚠️ No active attacker.")
        return()
      }
      
      if (!nzchar(target_id)) {
        log_safe("⚠️ Choose a target first.")
        return()
      }
      
      attacker_name <- get_actor_display_name(attacker_id)
      if (!nzchar(attacker_name)) attacker_name <- "Attacker"
      
      if (identical(attacker_type, "player")) {
        attacker_char <- load_actor_for_combat(attacker_id, "player")
        if (is.null(attacker_char)) {
          log_safe("⚠️ Could not load attacker.")
          return()
        }
        
        weapons <- get_equipped_weapons_for_combat(attacker_char)
        if (identical(attack_mode, "offhand")) {
          ready <- offhand_ready()
          allowed <- as.character(ready$weapon_ids %||% character())
          weapons <- weapons[as.character(weapons$id) %in% allowed, , drop = FALSE]
        }
        if (!is.data.frame(weapons) || nrow(weapons) == 0) {
          log_safe(if (identical(attack_mode, "offhand")) "⚠️ No eligible light off-hand weapon is equipped."
                   else "⚠️ No equipped weapons available.")
          return()
        }
        
        labels <- vapply(seq_len(nrow(weapons)), function(i) {
          w <- weapons[i, , drop = FALSE]
          hit_bonus <- get_weapon_hit_bonus(attacker_char, w)
          
          paste0(
            w$name[1],
            " • Hit ",
            ifelse(hit_bonus >= 0, "+", ""),
            hit_bonus,
            " • ",
            w$damage1[1],
            if (nzchar(w$damage2[1])) paste0(" + ", w$damage2[1]) else ""
          )
        }, character(1))
        
        weapon_choices <- stats::setNames(as.character(weapons$id), labels)
        
        showModal(modalDialog(
          title = paste0(if (identical(attack_mode, "offhand")) "Choose off-hand weapon — " else "Choose weapon — ", attacker_name),
          radioButtons(session$ns("attack_weapon_id"), "Equipped weapons", choices = weapon_choices),
          radioButtons(
            session$ns("attack_adv_mode"),
            "Attack roll",
            choices = c(
              "Auto" = "auto",
              "Normal" = "normal",
              "Advantage" = "advantage",
              "Disadvantage" = "disadvantage"
            ),
            selected = "auto",
            inline = TRUE
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_attack"), if (identical(attack_mode, "offhand")) "Roll Bonus Attack" else "Roll Attack", class = "btn btn-danger")
          ),
          easyClose = TRUE
        ))
        
      } else if (identical(attacker_type, "enemy")) {
        attacker_char <- load_actor_for_combat(attacker_id, "enemy")
        target_type <- get_actor_type_by_id(target_id)
        target_char <- load_actor_for_combat(target_id, target_type)
        
        if (is.null(attacker_char) || is.null(target_char)) {
          log_safe("⚠️ Could not load combatants.")
          return()
        }
        
        preview <- build_enemy_attack_preview(
          attacker_char = attacker_char,
          target_char = target_char,
          attacker_name = attacker_name,
          target_name = get_actor_display_name(target_id),
          attacker_id = attacker_id,
          target_id = target_id,
          attacker_type = "enemy",
          target_type = target_type
        )
        
        pending_attack(preview)
        
        base_proposed <- if (isTRUE(preview$is_hit)) {
          compute_final_attack(preview)$adjusted$total
        } else {
          0L
        }
        
        showModal(modalDialog(
          title = "Confirm Attack",
          uiOutput(session$ns("attack_confirm_ui")),
          numericInput(
            session$ns("final_damage_override"),
            "Final damage to apply",
            value = base_proposed,
            min = 0,
            step = 1
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("apply_attack_final"), "Apply Result", class = "btn btn-danger")
          ),
          easyClose = TRUE,
          size = "m"
        ))
        
      } else {
        log_safe("⚠️ Unsupported attacker type.")
      }
    }
    
    pending_target <- reactiveVal(NULL)
    
    observeEvent(input$map_target_click, {
      
      actor_id <- as.character(input$map_target_click$actor_id %||% "")
      actor_type <- as.character(input$map_target_click$actor_type %||% "")
      target_x<-suppressWarnings(as.integer(input$map_target_click$x%||%NA));target_y<-suppressWarnings(as.integer(input$map_target_click$y%||%NA));if(!is.na(target_x)&&!is.na(target_y)&&place_ward_at_map_point(target_x,target_y))return();if(!is.na(target_x)&&!is.na(target_y)&&release_rune_at_map_point(target_x,target_y,actor_id))return()
      
      if (!nzchar(actor_id)) return()
      
      combat <- combat_tbl()
      active_id <- as.character(combat$active_actor_id[1] %||% "")
      
      if (identical(actor_id, active_id)) return()
      
      old <- pending_target()
      
      if (!is.null(old) && identical(as.character(old$actor_id), actor_id)) {
        pending_target(NULL)
        open_attack_flow(actor_id)
        return()
      }
      
      pending_target(list(
        actor_id = actor_id,
        actor_type = actor_type
      ))
      
      selected_target_id(actor_id)
      
      log_safe(paste0(
        "🎯 Target selected: ",
        get_actor_display_name(actor_id),
        ". Click again to attack."
      ))
      
    }, ignoreInit = TRUE)
    
    build_enemy_attack_preview <- function(attacker_char, target_char, attacker_name, target_name,
                                           attacker_id, target_id, attacker_type = "enemy", target_type = NULL) {
      target_type <- as.character(target_type %||% get_actor_type_by_id(target_id) %||% "player")
      
      target_is_reckless_player <- isTRUE(reckless_active()) &&
        identical(as.character(target_id), as.character(core$state$char_id %||% ""))
      roll_obj <- roll_attack_d20(adv = if (target_is_reckless_player) "Adv" else "Normal")
      attack_roll <- as.integer(roll_obj$roll)
      attack_bonus <- as.integer(attacker_char$combat_profile$attack_bonus %||% 2L)
      attack_total <- as.integer(attack_roll + attack_bonus)
      target_ac <- get_effective_actor_ac(target_id, target_type, target_char)
      
      is_crit <- identical(attack_roll, 20L)
      is_hit <- is_crit || (attack_total >= target_ac)
      
      damage_parts <- list()
      if (isTRUE(is_hit)) {
        dmg_expr <- as.character(attacker_char$combat_profile$damage_expr %||% "1d6")
        dmg_type <- as.character(attacker_char$combat_profile$damage_type %||% "slashing")
        dr <- roll_dice_expr(dmg_expr)
        if (isTRUE(is_crit)) {
          extra <- roll_dice_expr(dmg_expr)
          dr$rolls <- c(dr$rolls, extra$rolls)
          dr$total <- as.integer(dr$total + sum(extra$rolls))
        }
        
        damage_parts <- list(list(
          source = "Enemy Attack",
          expr = dmg_expr,
          type = dmg_type,
          rolls = dr$rolls,
          total = as.integer(dr$total)
        ))
      }
      
      list(
        attacker_id = as.character(attacker_id),
        attacker_type = as.character(attacker_type),
        target_id = as.character(target_id),
        target_type = as.character(target_type),
        attacker_name = as.character(attacker_name),
        target_name = as.character(target_name),
        weapon_id = "",
        weapon_name = "Natural / Simple Attack",
        attack_roll = attack_roll,
        attack_rolls = attack_rolls,
        attack_bonus = as.integer(attack_bonus),
        attack_total = as.integer(attack_total),
        target_ac = as.integer(target_ac),
        is_hit = isTRUE(is_hit),
        is_crit = isTRUE(is_crit),
        base_parts = damage_parts,
        sneak_available = FALSE,
        sneak_part = NULL,
        target_traits = add_active_ward_traits(get_character_damage_traits(target_char),target_id),
        primary_damage_type = as.character(attacker_char$combat_profile$damage_type %||% "")
      )
    }
    
    
    
    observeEvent(input$confirm_attack, {
      removeModal()
      
      reaction_attack <- isTRUE(current_attack_is_opp()) || isTRUE(current_attack_is_ready())
      attacker_id <- if (isTRUE(reaction_attack)) {
        as.character(core$state$char_id %||% "")
      } else {
        active_actor_id()
      }
      target_id <- as.character(selected_target_id() %||% "")
      weapon_id <- as.character(input$attack_weapon_id %||% "")
      
      if (is.null(attacker_id) || !nzchar(as.character(attacker_id))) {
        log_safe("⚠️ No active attacker.")
        return()
      }
      
      if (!nzchar(target_id)) {
        log_safe("⚠️ No target selected.")
        return()
      }
      
      if (!nzchar(weapon_id)) {
        log_safe("⚠️ No weapon selected.")
        return()
      }
      
      target_type <- get_actor_type_by_id(target_id)
      attacker_char <- load_actor_for_combat(attacker_id, "player")
      target_char <- load_actor_for_combat(target_id, target_type)
      
      if (is.null(attacker_char) || is.null(target_char)) {
        log_safe("⚠️ Could not load combatants.")
        return()
      }
      attack_mode <- as.character(current_attack_mode() %||% "action")
      
      weapons <- get_equipped_weapons_for_combat(attacker_char)
      weapon_row <- weapons[as.character(weapons$id) == weapon_id, , drop = FALSE]
      
      if (!is.data.frame(weapon_row) || nrow(weapon_row) == 0) {
        log_safe("⚠️ Could not find selected weapon.")
        return()
      }

      attacker_row <- get_actor_row(attacker_id, "player")
      target_row_for_range <- get_actor_row(target_id, target_type)
      attack_geometry <- NULL
      # An opportunity attack resolves at the boundary square where the target
      # left reach, before its final map position is committed.
      if (!isTRUE(current_attack_is_opp()) && nrow(attacker_row) && nrow(target_row_for_range)) {
        attack_geometry <- combat_attack_geometry(
          map_tiles_rv(), attacker_row$x[[1L]], attacker_row$y[[1L]],
          target_row_for_range$x[[1L]], target_row_for_range$y[[1L]],
          weapon_row$range_ft[[1L]] %||% 5L,
          weapon_row$long_range_ft[[1L]] %||% weapon_row$range_ft[[1L]] %||% 5L,
          map_id = map_id()
        )
        if (!isTRUE(attack_geometry$in_range)) {
          log_safe(paste0("⚠️ Target is ", attack_geometry$distance_ft, " ft away; ", weapon_row$name[[1L]], " cannot reach that far."))
          return()
        }
        if (!isTRUE(attack_geometry$line_clear)) {
          log_safe("⚠️ A wall or other sight-blocking obstacle blocks this attack.")
          return()
        }
      }
      
      attacker_name <- get_actor_display_name(attacker_id)
      target_name <- get_actor_display_name(target_id)
      
      if (!nzchar(attacker_name)) attacker_name <- "Attacker"
      if (!nzchar(target_name)) target_name <- "Target"
      
      adv_mode <- resolve_attack_adv_mode(
        mode = input$attack_adv_mode %||% "auto",
        attacker_id = attacker_id,
        target_id = target_id
      )
      if (character_has_feature(attacker_char, "assassinate") &&
          as.integer(combat_tbl()$round_number[1] %||% 1L) == 1L) {
        target_row <- get_actor_row(target_id, target_type)
        current_order <- as.integer(combat_tbl()$current_turn_order[1] %||% 0L)
        target_order <- if (nrow(target_row)) as.integer(target_row$turn_order[1] %||% 0L) else 0L
        if (target_order > current_order) adv_mode <- "Advantage"
      }
      weapon_stat <- tolower(as.character(weapon_row$stat[1] %||% "str"))
      if (isTRUE(reckless_active()) && identical(weapon_stat, "str") &&
          identical(as.character(input$attack_adv_mode %||% "auto"), "auto")) {
        adv_mode <- "Advantage"
      }
      if (isTRUE(attacker_char$status$raging %||% FALSE) &&
          identical(character_level_choice(attacker_char, "Barbarian", 3L, "spirit_totem"), "Wolf") &&
          identical(weapon_stat, "str")) {
        adv_mode <- "Advantage"
      }
      if (!is.null(attack_geometry) && !isTRUE(attack_geometry$normal_range)) adv_mode <- "Disadvantage"
      
      preview <- build_attack_preview(
        attacker_char = attacker_char,
        target_char = target_char,
        weapon_row = weapon_row,
        attacker_name = attacker_name,
        target_name = target_name,
        attacker_id = attacker_id,
        target_id = target_id,
        adv_override = adv_mode
      )

      preview$attack_mode <- attack_mode
      if (identical(attack_mode, "action") && !isTRUE(reaction_attack) && weapon_is_light_melee(weapon_row)) {
        other_light <- weapons[
          as.character(weapons$physical_id%||%weapons$id) != as.character(weapon_row$physical_id[[1L]]%||%weapon_id) &
            vapply(seq_len(nrow(weapons)), function(i) weapon_is_light_melee(weapons[i, , drop = FALSE]), logical(1)),
          , drop = FALSE
        ]
        preview$offhand_weapon_ids <- as.character(other_light$id %||% character())
      }
      
      preview$is_opportunity_attack <- isTRUE(current_attack_is_opp())
      preview$is_readied_attack <- isTRUE(current_attack_is_ready())
      
      pending_attack(preview)
      if (nzchar(as.character(preview$superiority_manoeuvre %||% ""))) manoeuvre_active("")
      
      base_proposed <- if (isTRUE(preview$is_hit)) {
        compute_final_attack(preview, apply_sneak = isTRUE(preview$sneak_available))$adjusted$total
      } else {
        0L
      }
      
      showModal(modalDialog(
        title = "Confirm Attack",
        uiOutput(session$ns("attack_confirm_ui")),
        numericInput(
          session$ns("final_damage_override"),
          "Final damage to apply",
          value = base_proposed,
          min = 0,
          step = 1
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("apply_attack_final"), "Apply Result", class = "btn btn-danger")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$apply_attack_final, {
      preview <- pending_attack()
      if (is.null(preview)) {
        removeModal()
        return()
      }

      reaction_attack <- isTRUE(preview$is_opportunity_attack) || isTRUE(preview$is_readied_attack)
      if (isTRUE(reaction_attack)) {
        if (!isTRUE(player_reaction_available())) {
          log_safe("⚠️ Your reaction is no longer available; the attack was not applied.")
          return()
        }
        player_reaction_available(FALSE)
      } else {
        attacker_char <- load_actor_for_combat(preview$attacker_id, "player")
        spent <- if (identical(as.character(preview$attack_mode %||% "action"), "offhand")) {
          spend_action_safe("bonus_action", "Off-hand attack")
        } else {
          spend_attack_safe(attacker_char, "Attack")
        }
        if (!isTRUE(spent)) return()
      }

      if (identical(as.character(preview$attack_mode %||% "action"), "offhand")) {
        offhand_ready(NULL)
      } else if (length(preview$offhand_weapon_ids %||% character())) {
        offhand_ready(list(target_id = preview$target_id, weapon_ids = preview$offhand_weapon_ids))
        log_safe("🗡️ Off-hand attack available: spend your bonus action with another light weapon.")
      } else {
        offhand_ready(NULL)
      }

      removeModal()
      
      apply_sneak <- isTRUE(input$final_apply_sneak %||% FALSE)
      if (isTRUE(sneak_attack_used())) apply_sneak <- FALSE
      manual_bonus <- suppressWarnings(as.integer(input$final_bonus_damage %||% 0))
      manual_type <- as.character(input$final_bonus_type %||% "same_as_primary")
      final_damage <- suppressWarnings(as.integer(input$final_damage_override %||% 0))
      if (is.na(final_damage) || final_damage < 0) final_damage <- 0L
      used_uncanny_dodge <- FALSE
      if (isTRUE(input$use_uncanny_dodge %||% FALSE) &&
          identical(as.character(preview$target_id %||% ""), as.character(core$state$char_id %||% "")) &&
          player_has_feature("uncanny_dodge")) {
        if (isTRUE(player_reaction_available())) {
          player_reaction_available(FALSE)
          final_damage <- floor(final_damage / 2L)
          used_uncanny_dodge <- TRUE
          log_safe("🌀 Uncanny Dodge halves the incoming damage.")
        } else {
          log_safe("⚠️ Uncanny Dodge was selected, but no reaction remains.")
        }
      }
      
      calc <- compute_final_attack(
        preview = preview,
        apply_sneak = apply_sneak,
        manual_bonus = manual_bonus,
        manual_type = manual_type
      )
      if (identical(cunning_mode(), "hide")) cunning_mode("")
      end_encounter_condition(current_encounter_id(), preview$attacker_id, "hidden")
      end_encounter_condition(current_encounter_id(), preview$target_id, "helped_against")
      if (isTRUE(preview$is_readied_attack)) {
        end_encounter_condition(current_encounter_id(), preview$attacker_id, "readied")
        current_attack_is_ready(FALSE)
      }
      if (isTRUE(apply_sneak)) sneak_attack_used(TRUE)
      
      eid <- current_encounter_id()
      
      update_known_ac(
        target_id = preview$target_id,
        attack_total = preview$attack_total,
        is_hit = preview$is_hit
      )
      
      log_game_event(
        encounter_id = eid,
        event_type = "attack",
        actor_type = as.character(preview$attacker_type %||% "player"),
        actor_id = as.character(preview$attacker_id),
        target_id = as.character(preview$target_id),
        payload = list(
          attacker_name = preview$attacker_name,
          target_name = preview$target_name,
          target_type = preview$target_type,
          weapon_name = preview$weapon_name,
          is_opportunity_attack = isTRUE(preview$is_opportunity_attack),
          is_readied_attack = isTRUE(preview$is_readied_attack),
          attack_roll = preview$attack_roll,
          attack_rolls = as.list(preview$attack_rolls),
          attack_bonus = preview$attack_bonus,
          attack_total = preview$attack_total,
          target_ac = preview$target_ac,
          is_hit = preview$is_hit,
          is_crit = preview$is_crit,
          damage_total = calc$adjusted$total,
          final_damage = final_damage,
          used_uncanny_dodge = used_uncanny_dodge,
          used_sneak_attack = isTRUE(apply_sneak),
          raw_damage_total = calc$raw_total,
          damage_parts = lapply(calc$adjusted$parts, function(x) {
            list(
              source = x$source %||% "",
              type = x$type %||% "",
              total = x$total %||% 0,
              adjusted_total = x$adjusted_total %||% 0,
              rule = x$rule %||% "normal"
            )
          })
        )
      )
      
      if (isTRUE(preview$is_hit) && final_damage > 0L) {
        res <- if (identical(preview$target_type, "player")) {
          damage_player_in_encounter(
            encounter_id = eid,
            character_id = preview$target_id,
            amount = final_damage
          )
        } else {
          tryCatch(
            damage_encounter_enemy(
              encounter_id = eid,
              enemy_uuid = preview$target_id,
              amount = final_damage
            ),
            error = function(e) NULL
          )
        }
        
        if (!is.null(res)) {
          log_game_event(
            encounter_id = eid,
            event_type = "damage",
            actor_type = as.character(preview$attacker_type %||% "player"),
            actor_id = as.character(preview$attacker_id),
            target_id = as.character(preview$target_id),
            payload = list(
              amount = final_damage,
              hp_before = res$hp_before %||% NA,
              hp_after = res$hp_after %||% NA,
              temp_before = res$temp_before %||% NA,
              temp_after = res$temp_after %||% NA
            )
          )
        } else {
          log_safe("⚠️ Attack landed, but damage could not be applied.")
        }
      }
      
      ac_hint <- if (identical(preview$target_type, "enemy")) {
        format_known_ac(preview$target_id)
      } else {
        paste0("AC ", preview$target_ac)
      }
      
      if (isTRUE(preview$is_hit)) {
        log_safe(paste0(
          "🗡️ ", preview$attacker_name, " hits ", preview$target_name,
          " with ", preview$weapon_name,
          if (isTRUE(apply_sneak)) " + Sneak Attack" else "",
          " for ", final_damage, " damage. ",
          "(", preview$attack_total, " vs ", ac_hint, ")"
        ))
      } else {
        log_safe(paste0(
          "🛡️ ", preview$attacker_name, " misses ", preview$target_name,
          " with ", preview$weapon_name,
          " (", preview$attack_total, " vs ", ac_hint, ")."
        ))
      }
      if (bloodlust_bite_required(core$state$char, preview$attack_roll, FALSE)) {
        perform_forced_bloodlust_bite("stage_3_natural_1")
      }
      
      pending_attack(NULL)
      bump_positions()
      bump_events()
      bump_initiative()
      bump_map_visual()
    }, ignoreInit = TRUE)
    
  
    # --------------------------------------------------
    # Combat log
    # --------------------------------------------------
    output$log_ui <- renderUI({
      ev <- events_tbl()
      
      if (!is.data.frame(ev) || nrow(ev) == 0) {
        return(tagList(
          div(class = "combat-section-title", "Combat Log"),
          div("No recent events.")
        ))
      }
      
      if ("created_at" %in% names(ev)) {
        ord <- order(ev$created_at, decreasing = TRUE, na.last = TRUE)
        ev <- ev[ord, , drop = FALSE]
      }
      
      actors_for_log <- isolate(encounter_actors_tbl())
      
      actor_name_lookup <- function(actor_id) {
        actor_id <- as.character(actor_id %||% "")
        if (!nzchar(actor_id)) return("Actor")
        
        if (is.data.frame(actors_for_log) && nrow(actors_for_log) > 0) {
          row <- actors_for_log[
            as.character(actors_for_log$actor_id) == actor_id,
            ,
            drop = FALSE
          ]
          
          if (nrow(row) > 0) {
            return(as.character(row$display_name[1] %||% row$name[1] %||% "Unknown"))
          }
        }
        
        "Unknown"
      }
      
      items <- lapply(seq_len(nrow(ev)), function(i) {
        row <- ev[i, , drop = FALSE]
        
        typ <- as.character(row$event_type[1] %||% "event")
        body <- format_event_text_fast(row, actor_name_lookup)
        ts <- if ("created_at" %in% names(row)) as.character(row$created_at[1] %||% "") else ""
        
        div(
          class = "combat-log-item",
          div(class = "combat-log-type", typ),
          div(class = "combat-log-body", body),
          if (nzchar(ts)) div(class = "combat-log-time", ts)
        )
      })
      
      tagList(
        div(class = "combat-section-title", "Combat Log"),
        div(class = "combat-log", items)
      )
    })
    
    # --------------------------------------------------
    # Refresh / bind / combat flow
    # --------------------------------------------------
    observeEvent(input$refresh, {
      bump_refresh()
    }, ignoreInit = TRUE)
    
    end_current_turn <- function(forced = FALSE) {
      eid <- current_encounter_id()
      if (is.na(eid)) {
        log_safe("⚠️ Choose an encounter first.")
        return(FALSE)
      }
      combat <- combat_tbl()
      if (!is.data.frame(combat) || !nrow(combat) ||
          !identical(as.character(combat$phase[1] %||% ""), "combat")) {
        log_safe("⚠️ The DM must start combat and roll initiative first.")
        return(FALSE)
      }
      ok <- advance_turn(eid)
      if (isTRUE(ok)) {
        turn_move_ft(0L)
        log_safe(if (isTRUE(forced)) "⏭️ Turn advanced out of turn by confirmation." else "⏭️ Turn advanced.")
      } else {
        log_safe("⚠️ Could not advance turn.")
      }
      bump_combat()
      bump_positions()
      bump_events()
      bump_initiative()
      bump_map_visual()
      isTRUE(ok)
    }

    observeEvent(input$end_turn, {
      if (!isTRUE(is_players_turn())) {
        active_name <- get_actor_display_name(active_actor_id())
        showModal(modalDialog(
          title = "End somebody else's turn?",
          p(paste0("It is currently ", active_name %||% "another actor", "'s turn. This may skip an enemy or a player.")),
          p("Only continue if the DM wants to advance for an absent player or completed enemy."),
          footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_force_end_turn"), "Advance Anyway", class = "btn btn-warning"))
        ))
        return()
      }
      end_current_turn(FALSE)
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_force_end_turn, {
      removeModal()
      end_current_turn(TRUE)
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------
    # Movement
    # --------------------------------------------------
 
    
 
    
    #Thing thing
    last_positions_sig <- reactiveVal("")
    last_combat_sig <- reactiveVal("")
    last_events_sig <- reactiveVal("")
    
    df_sig <- function(df, cols = NULL) {
      if (!is.data.frame(df) || nrow(df) == 0) return("empty")
      
      if (!is.null(cols)) {
        cols <- intersect(cols, names(df))
        df <- df[, cols, drop = FALSE]
      }
      
      paste(utils::capture.output(str(df)), collapse = "|")
    }
    
    polling_busy <- reactiveVal(FALSE)
    
    observe({
      snapshot <- snapshot_data()
      
      if (isTRUE(isolate(polling_busy()))) return()
      
      polling_busy(TRUE)
      
      tryCatch({
        
        eid <- isolate(current_encounter_id())
        if (is.na(eid)) return()
        
        pos_now <- snapshot$positions %||% data.frame()
        pos_sig <- df_sig(pos_now, c("actor_type", "actor_id", "x", "y", "updated_at"))
        
        if (!identical(pos_sig, isolate(last_positions_sig()))) {
          last_positions_sig(pos_sig)
          bump_positions()
          bump_map_visual()
        }
        
        combat_now <- snapshot$combat %||% data.frame()
        combat_sig <- df_sig(
          combat_now,
          c("round_number", "current_turn_order", "active_actor_type", "active_actor_id", "phase", "updated_at")
        )
        
        if (!identical(combat_sig, isolate(last_combat_sig()))) {
          last_combat_sig(combat_sig)
          bump_combat()
          bump_initiative()
          bump_map_visual()
        }
        
        events_now <- snapshot$events %||% data.frame()
        
        if (is.data.frame(events_now) && nrow(events_now) > 0 && "event_type" %in% names(events_now)) {
          events_now <- events_now[
            !as.character(events_now$event_type %||% "") %in% c("move"),
            ,
            drop = FALSE
          ]
        }
        
        events_sig <- df_sig(events_now, c("id", "created_at", "event_type", "actor_id", "target_id"))
        
        if (!identical(events_sig, isolate(last_events_sig()))) {
          last_events_sig(events_sig)
          bump_events()
        }
        
      }, error = function(e) {
        message("Combat polling failed: ", conditionMessage(e))
      }, finally = {
        polling_busy(FALSE)
      })
    })
    # --------------------------------------------------
    # Damage test
    # --------------------------------------------------
    
    get_player_attackers_for_reaction <- function() {
      actors <- encounter_actors_tbl()
      if (!is.data.frame(actors) || nrow(actors) == 0) return(data.frame())
      
      actors[
        as.character(actors$actor_type %||% "") == "player" &
          as.logical(actors$is_active %||% TRUE),
        ,
        drop = FALSE
      ]
    }

  })
}
