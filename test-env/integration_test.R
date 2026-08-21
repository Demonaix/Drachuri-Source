project_dir <- normalizePath(Sys.getenv("DND_PROJECT_DIR"))
player_dir <- file.path(project_dir, "DND APP Drachuri Edition Player_v2")

setwd(player_dir)
source("global.R")
source("session_db.R")
source("plug/skills_data.R")
source("server/combat_map_logic.R")

on.exit(close_db_pool(), add = TRUE)
stopifnot(is_db_available())

started <- proc.time()[["elapsed"]]
snapshot <- get_player_live_snapshot(1L, "1001")
elapsed_ms <- round((proc.time()[["elapsed"]] - started) * 1000)
payload_kib <- round(length(serialize(snapshot, NULL, version = 2)) / 1024, 1)

stopifnot(nrow(snapshot$session) == 1L)
stopifnot(nrow(snapshot$players) == 10L)
stopifnot(nrow(snapshot$self_player) == 1L)
stopifnot(nrow(snapshot$encounter) == 1L)
stopifnot(nrow(snapshot$enemies) == 1L)
stopifnot(nrow(snapshot$positions) == 11L)
stopifnot(nrow(snapshot$combat) == 1L)

membership <- get_active_session_for_character("1001")
stopifnot(nrow(membership) == 1L)
stopifnot(membership$session_id[[1L]] == 1L)
stopifnot(membership$active_encounter_id[[1L]] == 1L)

# Party rests share one day cycle. A character cannot create the next day
# while any active party member has not completed the current rest.
rest_one<-begin_session_long_rest(1L,1001L,1L)
stopifnot(is.list(rest_one),rest_one$can_apply,rest_one$day_number==2L)
progress_one<-complete_session_long_rest(rest_one$cycle_id,1001L)
stopifnot(progress_one$completed[[1L]]==1L,progress_one$active[[1L]]==10L)
rest_repeat<-begin_session_long_rest(1L,1001L,2L)
stopifnot(!rest_repeat$can_apply,rest_repeat$cycle_id==rest_one$cycle_id)
rest_two<-begin_session_long_rest(1L,1002L,1L)
stopifnot(rest_two$can_apply,rest_two$cycle_id==rest_one$cycle_id,rest_two$day_number==2L)
for(cid in 1002:1010)complete_session_long_rest(rest_one$cycle_id,cid)
rest_next<-begin_session_long_rest(1L,1001L,2L)
stopifnot(rest_next$can_apply,rest_next$day_number==3L,rest_next$cycle_id!=rest_one$cycle_id)

supplies<-get_session_supplies(1L,list(wood=3L,wood_max=10L,water=3L,water_max=5L,rations=3L,rations_max=5L))
stopifnot(supplies$wood[[1L]]==3L,supplies$water[[1L]]==3L,supplies$rations[[1L]]==3L)
spent_wood<-adjust_session_supply(1L,"wood",-1L)
stopifnot(spent_wood$applied,spent_wood$after==2L,get_session_supplies(1L)$wood[[1L]]==2L)
for(i in 1:5)adjust_session_supply(1L,"water",-1L)
stopifnot(get_session_supplies(1L)$water[[1L]]==0L,!adjust_session_supply(1L,"water",-1L)$applied)
refilled<-adjust_session_supply(1L,"water",fill=TRUE)
stopifnot(refilled$after==5L,get_session_supplies(1L)$water[[1L]]==5L)

gather_request<-create_camp_gather_request(1L,50L,"1001","rations",3L,"1002")
stopifnot(is.data.frame(gather_request),nrow(gather_request)==1L)
pending_gather<-get_pending_camp_gather_requests("1002")
stopifnot(any(pending_gather$id==gather_request$id[[1L]]))
gather_result<-resolve_camp_gather_request(gather_request$id[[1L]],"1002",TRUE,5L)
stopifnot(gather_result$status=="resolved",gather_result$assisted,gather_result$amount>=0L,gather_result$amount<=4L)
gathered_char<-load_character_from_db("1001")
if(gather_result$amount>0L){stopifnot(is.list(gather_result$reward),nzchar(gather_result$reward$name),any(vapply(gathered_char$inventory$items$meta,function(m)identical(as.character((m%||%list())$category%||%""),"food")&&isTRUE((m%||%list())$foraged),logical(1))))}else stopifnot(is.null(gather_result$reward))
stopifnot(is.null(create_camp_gather_request(1L,50L,"1001","wood",0L,NULL)))
decline_request<-create_camp_gather_request(1L,50L,"1003","water",1L,"1004")
stopifnot(resolve_camp_gather_request(decline_request$id[[1L]],"1004",FALSE,0L)$status=="declined")
retry_request<-create_camp_gather_request(1L,50L,"1003","water",1L,NULL)
stopifnot(is.data.frame(retry_request),nrow(retry_request)==1L)

history_check<-create_party_skill_check(1L,"History","int","Ancient city walls","1001",3L,"party")
stopifnot(is.data.frame(history_check),nrow(history_check)==1L)
pending_checks<-get_pending_party_skill_checks(1L,"1002")
stopifnot(any(pending_checks$id==history_check$id[[1L]]))
history_result<-resolve_party_skill_check(history_check$id[[1L]],"1002",5L)
stopifnot(history_result$assisted,history_result$final_total==max(history_result$requester_roll,history_result$helper_roll))
stopifnot(is.null(resolve_party_skill_check(history_check$id[[1L]],"1003",1L)))
recent_checks<-get_recent_party_skill_results(1L)
stopifnot(any(recent_checks$id==history_check$id[[1L]]),recent_checks$final_total[recent_checks$id==history_check$id[[1L]]]==history_result$final_total)
solo_check<-create_party_skill_check(1L,"Medicine","bld_str","Treat the wound","1003",2L,"solo")
solo_result<-resolve_party_skill_check(solo_check$id[[1L]],"1003",NULL)
stopifnot(!solo_result$assisted,is.na(solo_result$helper_roll))

character <- load_character_from_db(1001L)
stopifnot(is.list(character))
stopifnot(is.integer(character_skill_modifier(character,"History",SKILLS_LIST)))
character$journal$log <- c(character$journal$log, "Multiplayer regression save marker")
stopifnot(identical(save_character_to_db(character, 1001L), "1001"))
reloaded_character <- load_character_from_db(1001L)
stopifnot(identical(
  tail(reloaded_character$journal$log, 1L),
  "Multiplayer regression save marker"
))

# Equipment provenance: only explicitly migrated weapons prompt; fresh weapons
# are filled manually and do not enter the migration queue.
migrated_weapon <- enemy_loot_to_inventory_row(
  enemy_loot_catalog()$dagger, id = "qa_migrated_weapon"
)
reloaded_character$inventory$items <- inventory_normalize(rbind(
  inventory_normalize(reloaded_character$inventory$items), migrated_weapon
))
stopifnot(identical(save_character_to_db(reloaded_character, 1001L), "1001"))
con <- get_db_connection()
DBI::dbExecute(con, paste(
  "UPDATE character_inventory_items SET source='character_backfill',needs_provenance_roll=TRUE",
  "WHERE character_id=$1 AND instance_id=$2"
), params=list("1001", "qa_migrated_weapon"))
release_db_connection(con)
pending_make <- pending_equipment_assignments("1001")
stopifnot(any(pending_make$instance_id == "qa_migrated_weapon"))
rolled_make <- roll_equipment_assignment("1001", "qa_migrated_weapon")
stopifnot(!is.null(rolled_make), nzchar(rolled_make$material[[1L]]), nzchar(rolled_make$build_quality[[1L]]))

fresh_weapon <- enemy_loot_to_inventory_row(
  enemy_loot_catalog()$dagger, id = "qa_fresh_manual_weapon"
)
fresh_weapon$meta[[1L]]$material <- "Steel"
fresh_weapon$meta[[1L]]$build_quality <- "Bog-Standard"
with_fresh_weapon <- load_character_from_db(1001L)
with_fresh_weapon$inventory$items <- inventory_normalize(rbind(
  inventory_normalize(with_fresh_weapon$inventory$items), fresh_weapon
))
stopifnot(identical(save_character_to_db(with_fresh_weapon, 1001L), "1001"))
stopifnot(!"qa_fresh_manual_weapon" %in% pending_equipment_assignments("1001")$instance_id)

damage <- damage_session_player(1L, "1002", 3L)
stopifnot(identical(damage$hp_before, 60L))
stopifnot(identical(damage$hp_after, 57L))

stopifnot(isTRUE(advance_turn(1L)))
after <- get_player_live_snapshot(1L, "1002")
stopifnot(identical(after$self_player$current_hp[[1L]], 57L))
stopifnot(identical(after$combat$current_turn_order[[1L]], 2L))
stopifnot(identical(after$combat$active_actor_id[[1L]], "1002"))

stopifnot(isTRUE(upsert_encounter_actor_position(1L, "player", "1001", 4L, 5L)))
player_one_after <- get_player_live_snapshot(1L, "1001")
player_two_after <- get_player_live_snapshot(1L, "1002")
position_one <- player_one_after$positions[
  player_one_after$positions$actor_type == "player" &
    player_one_after$positions$actor_id == "1001",
  , drop = FALSE
]
position_two_view <- player_two_after$positions[
  player_two_after$positions$actor_type == "player" &
    player_two_after$positions$actor_id == "1001",
  , drop = FALSE
]
stopifnot(nrow(position_one) == 1L, position_one$x[[1L]] == 4L, position_one$y[[1L]] == 5L)
stopifnot(
  nrow(position_two_view) == 1L,
  position_two_view$x[[1L]] == 4L,
  position_two_view$y[[1L]] == 5L
)
stopifnot(identical(player_one_after$combat$active_actor_id[[1L]], "1002"))
stopifnot(identical(player_two_after$self_player$current_hp[[1L]], 57L))

# Simulate a dropped/restarted client-side pool and verify a new checkout sees
# the same shared state without restarting PostgreSQL.
close_db_pool()
stopifnot(is_db_available())
reconnected <- get_player_live_snapshot(1L, "1002")
stopifnot(identical(reconnected$self_player$current_hp[[1L]], 57L))
stopifnot(identical(reconnected$combat$active_actor_id[[1L]], "1002"))

# The seeded enemy and player 1003 deliberately share turn order 3. Both must
# receive a turn instead of the enemy being collapsed out of the sequence.
stopifnot(isTRUE(advance_turn(1L)))
enemy_turn <- get_player_live_snapshot(1L, "1002")
stopifnot(identical(enemy_turn$combat$active_actor_type[[1L]], "enemy"))
stopifnot(isTRUE(advance_turn(1L)))
after_tie <- get_player_live_snapshot(1L, "1002")
stopifnot(identical(after_tie$combat$active_actor_id[[1L]], "1003"))

grapple <- create_encounter_effect(
  1L, "enemy", as.character(enemy_turn$combat$active_actor_id[[1L]]),
  "grapple", "condition", payload = list(condition = "grappled"),
  target_actor_type = "player", target_actor_id = "1002", starts_round = 1L
)
stopifnot(is.data.frame(grapple), nrow(grapple) == 1L)
stopifnot(isTRUE(end_encounter_condition(1L, "1002", "grappled")))
cleared <- get_player_live_snapshot(1L, "1002")$effects
if (is.data.frame(cleared) && nrow(cleared)) {
  stopifnot(!any(as.character(cleared$target_actor_id) == "1002" & grepl("grappled", as.character(cleared$payload))))
}

double <- create_encounter_summon(1L, "1002", "QA Illusory Double", max_cr = "illusion", hp_max = 1L, ac = 10L)
stopifnot(is.data.frame(double), nrow(double) == 1L)
double_id <- as.character(double$summon_uuid[[1L]])
stopifnot(isTRUE(set_actor_turn_order(1L, double_id, "summon", 12L, 12L)))
summons <- get_encounter_summons(1L)
stopifnot(any(as.character(summons$summon_uuid) == double_id & summons$turn_order == 12L))

generated_enemy <- resolve_enemy_blueprint("Bandit", c("Speedy", "Boss", "Fae"))
stopifnot(generated_enemy$movement_speed == 40L, generated_enemy$abilities[["dex"]] == 18L)
stopifnot(generated_enemy$armor_id=="leather",any(vapply(generated_enemy$loot,function(x)identical(x$name,"Leather Armour"),logical(1))))
stopifnot(generated_enemy$hp_max == 24L, "iron" %in% generated_enemy$vulnerabilities)
stopifnot(any(vapply(generated_enemy$attacks, function(x) identical(x$name, "Shortsword"), logical(1))))
stopifnot(any(vapply(generated_enemy$loot, function(x) identical(x$name, "Shortsword"), logical(1))))

reinforcement_id <- add_encounter_enemy(
  1L, "QA Reinforcement", hp_max = 14L, ac = 13L, movement_speed = 30L,
  attack_bonus = 3L, damage_expr = "1d6+1", damage_type = "slashing",
  template_key = "qa_generated", enemy_type = generated_enemy$enemy_type,
  characteristics = generated_enemy$characteristics, abilities = generated_enemy$abilities,
  attacks = generated_enemy$attacks, loot = generated_enemy$loot,
  vulnerabilities = generated_enemy$vulnerabilities, condition_immunities = c("frightened"), gold_min=7L, gold_max=7L
)
stopifnot(is.character(reinforcement_id), nzchar(reinforcement_id))
stopifnot(isTRUE(upsert_encounter_actor_position(1L, "enemy", reinforcement_id, 7L, 7L)))
reinforcement <- get_encounter_enemies(1L)
reinforcement <- reinforcement[as.character(reinforcement$enemy_uuid) == reinforcement_id,,drop=FALSE]
stopifnot(nrow(reinforcement) == 1L, reinforcement$enemy_type[[1L]] == "Bandit")
stopifnot("iron" %in% enemy_db_values(reinforcement$vulnerabilities[[1L]]))
stopifnot("frightened" %in% enemy_db_values(reinforcement$condition_immunities[[1L]]))
con <- get_db_connection(); DBI::dbExecute(con,"UPDATE encounter_enemies SET hp_current=0 WHERE enemy_uuid=$1::uuid",params=list(reinforcement_id)); release_db_connection(con)
claimed <- claim_defeated_enemy_loot(1L,reinforcement_id,"1001")
stopifnot(is.list(claimed),claimed$gold==7L,length(claimed$loot)>=1L)
stopifnot(is.null(claim_defeated_enemy_loot(1L,reinforcement_id,"1002")))
weapon_drop <- Filter(function(x) identical(x$type,"weapon"),claimed$loot)[[1L]]
loot_row <- enemy_loot_to_inventory_row(weapon_drop,"qa_loot")
stopifnot(loot_row$type[[1L]]=="weapon",is.list(loot_row$meta[[1L]]))

trader<-load_character_from_db("1001");trader$inventory$items<-inventory_normalize(rbind(trader$inventory$items,loot_row));trader$inventory$gold<-20;save_character_to_db(trader,"1001")
offer<-create_trade_offer(1L,"1001","1002","item",item_id="qa_loot")
stopifnot(is.list(offer),!"qa_loot"%in%offer$sender$inventory$items$id)
pending<-get_pending_trade_offers("1002");stopifnot(nrow(pending)==1L,pending$summary[[1]]!="")
accepted<-resolve_trade_offer(offer$offer_id,"1002",TRUE);stopifnot(accepted$status=="accepted")
received<-accepted$character$inventory$items;received_meta<-Filter(function(x)nzchar(as.character(x$material%||%""))&&nzchar(as.character(x$build_quality%||%"")),received$meta);stopifnot(any(received$name==loot_row$name),length(received_meta)>=1L)
gold_offer<-create_trade_offer(1L,"1001","1002","gold",gold_amount=5L);stopifnot(gold_offer$sender$inventory$gold==15)
declined<-resolve_trade_offer(gold_offer$offer_id,"1002",FALSE);stopifnot(declined$status=="declined",declined$character$inventory$gold==20)
accepted_gold<-create_trade_offer(1L,"1001","1002","gold",gold_amount=5L);stopifnot(accepted_gold$sender$inventory$gold==15)
accepted_gold_result<-resolve_trade_offer(accepted_gold$offer_id,"1002",TRUE);stopifnot(accepted_gold_result$status=="accepted",accepted_gold_result$character$inventory$gold==5)

qa_apple<-list(id="qa_apple",catalogue_id="qa_apple",name="QA Apple",type="consumable",desc="Fresh test fruit.",value=2,weight=.2,qty=2,meta=list(category="consumable"))
merchant<-create_merchant(1L,"QA Grocer","moderate","food","fair",50,list(qa_apple));stopifnot(nrow(merchant)==1L)
stopifnot(invite_players_to_merchant(merchant$id[[1L]],c("1001","1002")))
merchant_invites<-get_pending_merchant_invitations("1002");stopifnot(nrow(merchant_invites)==1L,merchant_invites$name[[1L]]=="QA Grocer")
merchant_bundle<-get_merchant_bundle(merchant$id[[1L]]);stock_id<-merchant_bundle$stock$id[[1L]]
bought<-merchant_trade(merchant$id[[1L]],"1002","buy",stock_id=stock_id);stopifnot(is.list(bought),any(bought$character$inventory$items$name=="QA Apple"))
bought_id<-bought$character$inventory$items$id[match("QA Apple",bought$character$inventory$items$name)]
sold<-merchant_trade(merchant$id[[1L]],"1002","sell",player_item_id=bought_id);stopifnot(is.list(sold),!bought_id%in%sold$character$inventory$items$id)
merchant_tx<-DBI::dbGetQuery(con,"SELECT direction,haggle_roll,haggle_dc FROM merchant_transactions WHERE merchant_id=$1 ORDER BY id",params=list(merchant$id[[1L]]));stopifnot(identical(as.character(merchant_tx$direction),c("buy","sell")))
quote1<-create_merchant_quote(merchant$id[[1L]],"1002","buy",stock_id=stock_id);stopifnot(nrow(quote1)==1L,quote1$status[[1L]]=="pending")
stopifnot(reject_merchant_quote(quote1$id[[1L]],"1002"))
quote2<-create_merchant_quote(merchant$id[[1L]],"1002","buy",stock_id=stock_id);stopifnot(quote2$haggle_dc[[1L]]==15L)
quoted_buy<-merchant_trade(merchant$id[[1L]],"1002",quote_id=quote2$id[[1L]]);stopifnot(is.list(quoted_buy),any(quoted_buy$character$inventory$items$name=="QA Apple"))
quote_status<-DBI::dbGetQuery(con,"SELECT status FROM merchant_quotes WHERE id=$1",params=list(quote2$id[[1L]]));stopifnot(quote_status$status[[1L]]=="accepted")

critical_buyer<-load_character_from_db("1002");critical_buyer$inventory$gold<-100;save_character_to_db(critical_buyer,"1002")
critical_stock<-qa_apple;critical_stock$qty<-2L;critical_merchant<-create_merchant(1L,"QA Critical Trader","rich","food","fair",100,list(critical_stock));critical_bundle<-get_merchant_bundle(critical_merchant$id[[1L]])
critical_quote<-create_merchant_quote(critical_merchant$id[[1L]],"1002","buy",stock_id=critical_bundle$stock$id[[1L]],natural_roll_override=1L)
stopifnot(nrow(critical_quote)==1L,critical_quote$natural_roll[[1L]]==1L,critical_quote$final_price[[1L]]==2*critical_quote$base_value[[1L]])
critical_buy<-merchant_trade(critical_merchant$id[[1L]],"1002",quote_id=critical_quote$id[[1L]]);stopifnot(is.list(critical_buy),critical_buy$natural_roll==1L)
con<-get_db_connection();DBI::dbExecute(con,"UPDATE merchants SET status='closed' WHERE id=$1",params=list(critical_merchant$id[[1L]]));release_db_connection(con)
closed_quote<-create_merchant_quote(critical_merchant$id[[1L]],"1002","buy",stock_id=critical_bundle$stock$id[[1L]])
stopifnot(merchant_action_failed(closed_quote),grepl("no longer open",merchant_error_message(closed_quote),fixed=TRUE))

stopifnot(identical(get_session_fire(1L),FALSE),isTRUE(set_session_fire(1L,TRUE)),identical(get_session_fire(1L),TRUE))
rest_cycle<-begin_session_long_rest(1L,"1001",1L);stopifnot(!is.null(rest_cycle),isTRUE(rest_cycle$can_apply))
rest_progress<-complete_session_long_rest(rest_cycle$cycle_id,"1001","half");stopifnot(rest_progress$completed[[1L]]==1L)
rest_saved<-DBI::dbGetQuery(con,"SELECT rest_outcome FROM session_rest_completions WHERE rest_cycle_id=$1 AND character_id=$2",params=list(rest_cycle$cycle_id,"1001"));stopifnot(rest_saved$rest_outcome[[1L]]=="half")
note_id<-send_private_note(1L,"1001","1002","Meet me beside the old standing stone.")
stopifnot(!is.null(note_id));notes<-get_private_notes("1002",TRUE);stopifnot(any(notes$id==note_id),grepl("QA Rogue",notes$sender_name[notes$id==note_id],fixed=TRUE))
stopifnot(mark_private_note(note_id,"1002","read"));stopifnot(!note_id%in%get_private_notes("1002",TRUE)$id)
reply_id<-send_private_note(1L,"1002","1001","I will be there.",reply_to_id=note_id);stopifnot(!is.null(reply_id));stopifnot(mark_private_note(note_id,"1002","acknowledged"))

blood_event<-record_blood_consumption("1001",1L,3L,"blood","QA Stag",1.5,12)
heart_event<-record_blood_consumption("1001",1L,3L,"heart","QA Bandit",1,25)
stopifnot(nrow(blood_event)==1L,nrow(heart_event)==1L)
blood_history<-get_blood_consumption_history("1001",10L)
stopifnot(nrow(blood_history)==2L,all(blood_history$campaign_day==3L))
stopifnot(setequal(as.character(blood_history$consumption_type),c("blood","heart")))
con<-get_db_connection()
DBI::dbExecute(con,"INSERT INTO items(id,name,item_type,category) VALUES('qa_mundane','QA Rope','item','mundane_loot')")
category_rows<-DBI::dbGetQuery(con,"SELECT category FROM items WHERE category='mundane_loot' LIMIT 1")
mundane_count<-DBI::dbGetQuery(con,"SELECT count(*) AS n FROM items WHERE id LIKE 'mundane\\_%' ESCAPE '\\'")$n[[1L]]
food_summary<-DBI::dbGetQuery(con,"SELECT count(*) AS n,min(ration_value) AS min_rations,max(ration_value) AS max_rations,min(shelf_life_days) AS min_shelf,max(shelf_life_days) AS max_shelf FROM items WHERE id LIKE 'food\\_%' ESCAPE '\\'")
release_db_connection(con)
stopifnot(nrow(category_rows)==1L,mundane_count==100L,food_summary$n[[1L]]==100L,food_summary$min_rations[[1L]]>=1L,food_summary$max_rations[[1L]]>food_summary$min_rations[[1L]],food_summary$max_shelf[[1L]]>food_summary$min_shelf[[1L]])

stopifnot(isTRUE(end_encounter_combat(1L)))
ended_snapshot <- get_player_live_snapshot(1L, "1002")
ended_combat <- get_combat_state(1L)
stopifnot(identical(as.character(ended_combat$phase[[1L]]), "ended"))
stopifnot(is.na(ended_snapshot$session$active_encounter_id[[1L]]))

cat("PASS: character save, sync, HP, movement, turns, enemy generation/traits, and reconnection\n")
cat("Snapshot:", elapsed_ms, "ms,", payload_kib, "KiB\n")

stopifnot(length(.drachuri_db$checked_out) == 0L)
close_db_pool()
stopifnot(is.null(.drachuri_db$pool))
