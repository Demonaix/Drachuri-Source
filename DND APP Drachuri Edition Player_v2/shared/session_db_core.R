# ============================================================
# DB connection helpers (offline-safe)
# ============================================================

safe_db_disconnect <- function(con) {
  release_db_connection(con)
}

is_db_available <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch({
    DBI::dbGetQuery(con, "SELECT 1 AS ok")
    TRUE
  }, error = function(e) {
    message("DB availability check failed: ", e$message)
    FALSE
  })
}

claim_defeated_enemy_loot <- function(encounter_id, enemy_uuid, character_id) {
  con <- get_db_connection(); if (is.null(con)) return(NULL); on.exit(release_db_connection(con),add=TRUE)
  out <- tryCatch(DBI::dbWithTransaction(con, {
    row <- DBI::dbGetQuery(con,"SELECT enemy_uuid,name,hp_current,loot,gold_min,gold_max,looted_by FROM encounter_enemies WHERE encounter_id=$1 AND enemy_uuid=$2::uuid FOR UPDATE",params=list(as.integer(encounter_id),as.character(enemy_uuid)))
    already_looted <- !is.na(row$looted_by[1]) && nzchar(as.character(row$looted_by[1]))
    if(!nrow(row)||as.integer(row$hp_current[1])>0L||already_looted) {
      NULL
    } else {
      DBI::dbExecute(con,"UPDATE encounter_enemies SET looted_by=$3,looted_at=now(),updated_at=now() WHERE encounter_id=$1 AND enemy_uuid=$2::uuid",params=list(as.integer(encounter_id),as.character(enemy_uuid),as.character(character_id)))
      lo<-as.integer(row$gold_min[1]%||%0L); hi<-as.integer(row$gold_max[1]%||%lo); if(is.na(lo))lo<-0L;if(is.na(hi)||hi<lo)hi<-lo
      list(name=as.character(row$name[1]%||%"Enemy"),loot=enemy_db_json(row$loot[[1]]%||%NULL,list()),gold=if(hi>lo)sample.int(hi-lo+1L,1L)+lo-1L else lo)
    }
  }),error=function(e){message("claim_defeated_enemy_loot failed: ",e$message);NULL}); out
}

create_trade_offer <- function(session_id,sender_id,recipient_id,kind,item_id=NULL,gold_amount=0L) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{
    ids<-sort(c(as.character(sender_id),as.character(recipient_id))); rows<-DBI::dbGetQuery(con,"SELECT id,state_blob FROM character_blobs WHERE id::text=ANY($1::text[]) ORDER BY id FOR UPDATE",params=list(enemy_pg_array(ids)));if(nrow(rows)!=2L)stop("Both characters must exist.")
    sender_row<-rows[as.character(rows$id)==as.character(sender_id),,drop=FALSE]; sender<-validate_character(unserialize(sender_row$state_blob[[1]]));sender<-hydrate_character_inventory_relational(con,sender,as.character(sender_id)); item_blob<-NULL; summary<-list(kind=kind)
    if(identical(kind,"gold")){amount<-as.integer(gold_amount);if(is.na(amount)||amount<1L||sender$inventory$gold<amount)stop("Not enough gold.");sender$inventory$gold<-sender$inventory$gold-amount;summary$gold<-amount
    }else{inv<-inventory_normalize(sender$inventory$items);idx<-match(as.character(item_id),inv$id);if(is.na(idx))stop("Item is no longer available.");item<-inv[idx,,drop=FALSE];inv<-inv[-idx,,drop=FALSE];sender$inventory$items<-inventory_normalize(inv);item_blob<-serialize(item,NULL);summary<-list(kind="item",name=item$name[[1]],type=item$type[[1]],qty=item$qty[[1]],weight=item$weight[[1]],value=item$value[[1]],desc=item$desc[[1]],meta=item$meta[[1]])}
    DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,char_name=$2,updated_at=now() WHERE id::text=$3",params=list(list(serialize(sender,NULL)),sender$meta$name,as.character(sender_id)))
    sync_character_inventory_relational(con,sender,as.character(sender_id))
    offer<-DBI::dbGetQuery(con,"INSERT INTO trade_offers(session_id,sender_character_id,recipient_character_id,offer_kind,item_blob,gold_amount,summary) VALUES($1,$2,$3,$4,$5,$6,$7::jsonb) RETURNING id",params=list(as.integer(session_id),as.character(sender_id),as.character(recipient_id),kind,list(item_blob%||%raw()),as.integer(gold_amount),enemy_json(summary)))
    list(offer_id=offer$id[[1]],sender=sender)
  }),error=function(e){message("create_trade_offer failed: ",e$message);structure(NULL,error=e$message)})
}

consume_trade_sender_update <- function(character_id) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{rows<-DBI::dbGetQuery(con,"SELECT id,status FROM trade_offers WHERE sender_character_id=$1 AND status<>'pending' AND sender_seen=FALSE ORDER BY resolved_at FOR UPDATE",params=list(as.character(character_id)));if(!nrow(rows))NULL else {DBI::dbExecute(con,"UPDATE trade_offers SET sender_seen=TRUE WHERE id=ANY($1::bigint[])",params=list(enemy_pg_array(rows$id)));blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1",params=list(as.character(character_id)));list(character=unserialize(blob$state_blob[[1]]),statuses=rows$status)}}),error=function(e)NULL)
}

get_pending_trade_offers <- function(character_id) {
  con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbGetQuery(con,"SELECT t.*,cb.char_name AS sender_name FROM trade_offers t LEFT JOIN character_blobs cb ON cb.id::text=t.sender_character_id WHERE t.recipient_character_id=$1 AND t.status='pending' ORDER BY t.created_at",params=list(as.character(character_id))),error=function(e)data.frame())
}

resolve_trade_offer <- function(offer_id,recipient_id,accept=TRUE) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{
    offer<-DBI::dbGetQuery(con,"SELECT * FROM trade_offers WHERE id=$1 FOR UPDATE",params=list(as.integer(offer_id)));if(!nrow(offer)||offer$status[[1]]!="pending"||offer$recipient_character_id[[1]]!=as.character(recipient_id))stop("Offer is no longer pending.")
    target_id<-if(isTRUE(accept))as.character(recipient_id) else as.character(offer$sender_character_id[[1]]); row<-DBI::dbGetQuery(con,"SELECT id,state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(target_id));char<-validate_character(unserialize(row$state_blob[[1]]));char<-hydrate_character_inventory_relational(con,char,target_id)
    if(offer$offer_kind[[1]]=="gold")char$inventory$gold<-char$inventory$gold+as.integer(offer$gold_amount[[1]]) else {item<-unserialize(offer$item_blob[[1]]);item$id[[1]]<-paste0("trade_",offer$id[[1]],"_",sample(1000:9999,1));item$equipped[[1]]<-FALSE;item$edit[[1]]<-FALSE;char$inventory$items<-inventory_normalize(rbind(inventory_normalize(char$inventory$items),item))}
    DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,char_name=$2,updated_at=now() WHERE id::text=$3",params=list(list(serialize(char,NULL)),char$meta$name,target_id));sync_character_inventory_relational(con,char,target_id);status<-if(isTRUE(accept))"accepted" else "declined";DBI::dbExecute(con,"UPDATE trade_offers SET status=$2,resolved_at=now() WHERE id=$1",params=list(as.integer(offer_id),status));list(character=char,status=status)
  }),error=function(e){message("resolve_trade_offer failed: ",e$message);NULL})
}

send_private_note <- function(session_id,sender_id,recipient_id,body,reply_to_id=NULL) {
  body<-trimws(as.character(body%||%""));if(!nzchar(body)||nchar(body)>4000L)return(NULL)
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbGetQuery(con,"INSERT INTO private_notes(session_id,sender_character_id,recipient_character_id,body,reply_to_id) VALUES($1,$2,$3,$4,$5) RETURNING id",params=list(as.integer(session_id),as.character(sender_id),as.character(recipient_id),body,suppressWarnings(as.integer(reply_to_id%||%NA))))$id[[1]],error=function(e){message("send_private_note failed: ",e$message);NULL})
}

get_private_notes <- function(character_id,unread_only=FALSE) {
  con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE)
  extra<-if(isTRUE(unread_only))" AND n.status='sent'" else ""
  tryCatch(DBI::dbGetQuery(con,paste0("SELECT n.*,s.char_name AS sender_name,r.char_name AS recipient_name FROM private_notes n LEFT JOIN character_blobs s ON s.id::text=n.sender_character_id LEFT JOIN character_blobs r ON r.id::text=n.recipient_character_id WHERE n.recipient_character_id=$1",extra," ORDER BY n.created_at DESC LIMIT 50"),params=list(as.character(character_id))),error=function(e)data.frame())
}

mark_private_note <- function(note_id,recipient_id,status=c("read","acknowledged")) {
  status<-match.arg(status);con<-get_db_connection();if(is.null(con))return(FALSE);on.exit(release_db_connection(con),add=TRUE)
  set_sql<-if(status=="read")"status=$3,read_at=COALESCE(read_at,now())" else "status=$3,acknowledged_at=now(),read_at=COALESCE(read_at,now())"
  tryCatch({DBI::dbExecute(con,paste0("UPDATE private_notes SET ",set_sql," WHERE id=$1 AND recipient_character_id=$2"),params=list(as.integer(note_id),as.character(recipient_id),status));TRUE},error=function(e){message("mark_private_note failed: ",e$message);FALSE})
}

create_merchant <- function(session_id,name,wealth_class="moderate",specialty="general",temperament="fair",gold=50,stock=list(),pricing_style="standard") {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  wealth_class<-match.arg(as.character(wealth_class),c("poor","moderate","rich"));specialty<-match.arg(as.character(specialty),c("general","provisions","food","weapons","armour","hunter","apothecary","arcane","outfitter","luxury","smith","materials","livestock"));temperament<-match.arg(as.character(temperament),c("hard","fair","generous"));pricing_style<-match.arg(as.character(pricing_style),c("cheap","standard","expensive","very_expensive"))
  tryCatch(DBI::dbWithTransaction(con,{m<-DBI::dbGetQuery(con,"INSERT INTO merchants(session_id,name,wealth_class,specialty,temperament,gold,pricing_style) VALUES($1,$2,$3,$4,$5,$6,$7) RETURNING *",params=list(as.integer(session_id),trimws(as.character(name)),wealth_class,specialty,temperament,max(0,as.numeric(gold)),pricing_style))
    for(item in stock){qty<-max(1L,as.integer(item$qty%||%1L));base<-max(0,as.numeric(item$value%||%0));DBI::dbExecute(con,"INSERT INTO merchant_stock(merchant_id,catalogue_id,item_json,quantity,base_value) VALUES($1,$2,$3::jsonb,$4,$5)",params=list(m$id[[1L]],as.character(item$catalogue_id%||%item$id%||%""),enemy_json(item),qty,base))};m[1,,drop=FALSE]
  }),error=function(e){message("create_merchant failed: ",e$message);NULL})
}

queue_character_refresh <- function(character_id,update_kind="inventory",message="Your character was updated.") {
  con<-get_db_connection();if(is.null(con))return(FALSE);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbExecute(con,"INSERT INTO character_refresh_notifications(character_id,update_kind,message) VALUES($1,$2,$3)",params=list(as.character(character_id),as.character(update_kind),as.character(message)))>0L,error=function(e){message("queue_character_refresh failed: ",e$message);FALSE})
}

consume_character_refresh <- function(character_id) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{updates<-DBI::dbGetQuery(con,"SELECT id,update_kind,message FROM character_refresh_notifications WHERE character_id=$1 AND seen=FALSE ORDER BY created_at FOR UPDATE",params=list(as.character(character_id)));if(!nrow(updates))NULL else{DBI::dbExecute(con,"UPDATE character_refresh_notifications SET seen=TRUE,seen_at=now() WHERE id=ANY($1::bigint[])",params=list(enemy_pg_array(updates$id)));row<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1",params=list(as.character(character_id)));if(!nrow(row))NULL else{char<-validate_character(unserialize(row$state_blob[[1L]]));char<-hydrate_character_inventory_relational(con,char,as.character(character_id));list(character=char,messages=as.character(updates$message),kinds=as.character(updates$update_kind))}}}),error=function(e){message("consume_character_refresh failed: ",e$message);NULL})
}

list_session_merchants <- function(session_id,open_only=FALSE) {
  con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE);extra<-if(isTRUE(open_only))" AND status='open'"else""
  tryCatch(DBI::dbGetQuery(con,paste0("SELECT * FROM merchants WHERE session_id=$1",extra," ORDER BY created_at DESC"),params=list(as.integer(session_id))),error=function(e)data.frame())
}

get_merchant_bundle <- function(merchant_id) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  tryCatch({m<-DBI::dbGetQuery(con,"SELECT * FROM merchants WHERE id=$1",params=list(as.integer(merchant_id)));if(!nrow(m))return(NULL);s<-DBI::dbGetQuery(con,"SELECT * FROM merchant_stock WHERE merchant_id=$1 AND quantity>0 ORDER BY id",params=list(as.integer(merchant_id)));list(merchant=m[1,,drop=FALSE],stock=s)},error=function(e)NULL)
}

merchant_error_message <- function(result,fallback="The merchant action could not be completed.") {
  msg<-attr(result,"error",exact=TRUE)
  if(is.null(msg)||!nzchar(as.character(msg)))fallback else as.character(msg)
}
merchant_action_failed <- function(result) is.null(result)||!is.null(attr(result,"error",exact=TRUE))

invite_players_to_merchant <- function(merchant_id,character_ids) {
  con<-get_db_connection();if(is.null(con))return(FALSE);on.exit(release_db_connection(con),add=TRUE);ids<-unique(as.character(character_ids));ids<-ids[nzchar(ids)];if(!length(ids))return(FALSE)
  tryCatch({for(cid in ids)DBI::dbExecute(con,"INSERT INTO merchant_invitations(merchant_id,character_id,status) VALUES($1,$2,'pending') ON CONFLICT(merchant_id,character_id) DO UPDATE SET status='pending',updated_at=now()",params=list(as.integer(merchant_id),cid));TRUE},error=function(e){message("invite_players_to_merchant failed: ",e$message);FALSE})
}

get_pending_merchant_invitations <- function(character_id,pending_only=TRUE) {
  con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE)
  status_sql<-if(isTRUE(pending_only))"i.status='pending'"else"i.status IN ('pending','opened')"
  tryCatch(DBI::dbGetQuery(con,paste0("SELECT i.*,m.name,m.wealth_class,m.specialty,m.temperament,m.gold FROM merchant_invitations i JOIN merchants m ON m.id=i.merchant_id WHERE i.character_id=$1 AND ",status_sql," AND m.status='open' ORDER BY i.created_at"),params=list(as.character(character_id))),error=function(e)data.frame())
}

mark_merchant_invitation <- function(merchant_id,character_id,status=c("opened","dismissed")) {
  status<-match.arg(status);con<-get_db_connection();if(is.null(con))return(FALSE);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbExecute(con,"UPDATE merchant_invitations SET status=$3,updated_at=now() WHERE merchant_id=$1 AND character_id=$2",params=list(as.integer(merchant_id),as.character(character_id),status))>0L,error=function(e)FALSE)
}

create_merchant_quote <- function(merchant_id,character_id,direction=c("buy","sell"),stock_id=NULL,player_item_id=NULL,natural_roll_override=NULL) {
  direction<-match.arg(direction);con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{merchant<-DBI::dbGetQuery(con,"SELECT * FROM merchants WHERE id=$1 AND status='open'",params=list(as.integer(merchant_id)));if(!nrow(merchant))stop("Merchant is no longer open.");row<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1",params=list(as.character(character_id)));if(!nrow(row))stop("Character not found.");char<-validate_character(unserialize(row$state_blob[[1L]]));char<-hydrate_character_inventory_relational(con,char,as.character(character_id));inv<-inventory_normalize(char$inventory$items)
    if(direction=="buy"){stock<-DBI::dbGetQuery(con,"SELECT * FROM merchant_stock WHERE id=$1 AND merchant_id=$2 AND quantity>0",params=list(as.integer(stock_id),as.integer(merchant_id)));if(!nrow(stock))stop("That item has sold out.");item<-enemy_db_json(stock$item_json[[1L]],list());item_name<-as.character(item$name%||%"Item");base<-as.numeric(stock$base_value[[1L]])
    }else{sale_id<-as.character(player_item_id);if(startsWith(sale_id,"animal:")){stable_id<-suppressWarnings(as.integer(sub("^animal:","",sale_id)));stable_animal<-DBI::dbGetQuery(con,"SELECT COALESCE(NULLIF(ca.custom_name,''),a.name) AS name,a.gold_value FROM character_animals ca JOIN animals a ON a.id=ca.animal_id WHERE ca.id=$1 AND ca.character_id=$2",params=list(stable_id,as.character(character_id)));if(!nrow(stable_animal))stop("That animal is no longer in your stable.");item_name<-as.character(stable_animal$name[[1L]]);base<-as.numeric(stable_animal$gold_value[[1L]])}else{idx<-match(sale_id,inv$id);if(is.na(idx))stop("That item is no longer in your inventory.");if(isTRUE(as.logical(inv$equipped[[idx]])))stop("Unequip that item before selling it.");item_name<-as.character(inv$name[[idx]]);base<-as.numeric(inv$value[[idx]])}}
    invite<-DBI::dbGetQuery(con,"SELECT haggle_rejections FROM merchant_invitations WHERE merchant_id=$1 AND character_id=$2",params=list(as.integer(merchant_id),as.character(character_id)));rejections<-if(nrow(invite))as.integer(invite$haggle_rejections[[1L]])else 0L;natural_roll<-suppressWarnings(as.integer(natural_roll_override%||%sample.int(20L,1L)));if(is.na(natural_roll)||natural_roll<1L||natural_roll>20L)stop("Natural roll override must be between 1 and 20.");roll<-natural_roll+character_skill_modifier(char,"Persuasion");terms<-merchant_haggle_terms(base,direction,merchant$temperament[[1L]],roll,dc_penalty=2L*rejections,pricing_style=as.character(merchant$pricing_style[[1L]]%||%"standard"));if(direction=="buy"&&natural_roll==1L){terms$price<-max(if(base>0)1 else 0,round(base*2));terms$success<-FALSE};if(direction=="buy"&&natural_roll==20L){terms$price<-max(if(base>0)1 else 0,round(base*.5));terms$success<-TRUE}
    DBI::dbGetQuery(con,"INSERT INTO merchant_quotes(merchant_id,character_id,direction,stock_id,player_item_id,item_name,base_value,final_price,haggle_roll,haggle_dc,haggle_success,natural_roll) VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12) RETURNING *",params=list(as.integer(merchant_id),as.character(character_id),direction,suppressWarnings(as.integer(stock_id%||%NA)),as.character(player_item_id%||%NA_character_),item_name,terms$base,terms$price,terms$roll,terms$dc,terms$success,natural_roll))[1,,drop=FALSE]
  }),error=function(e){message("create_merchant_quote failed: ",e$message);structure(list(),error=e$message)})
}

reject_merchant_quote <- function(quote_id,character_id) {
  con<-get_db_connection();if(is.null(con))return(FALSE);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{q<-DBI::dbGetQuery(con,"UPDATE merchant_quotes SET status='rejected',resolved_at=now() WHERE id=$1 AND character_id=$2 AND status='pending' RETURNING merchant_id",params=list(as.integer(quote_id),as.character(character_id)));if(!nrow(q))return(FALSE);DBI::dbExecute(con,"UPDATE merchant_invitations SET haggle_rejections=haggle_rejections+1,updated_at=now() WHERE merchant_id=$1 AND character_id=$2",params=list(q$merchant_id[[1L]],as.character(character_id)));TRUE}),error=function(e)FALSE)
}

merchant_trade <- function(merchant_id,character_id,direction=c("buy","sell"),stock_id=NULL,player_item_id=NULL,quote_id=NULL) {
  direction<-match.arg(direction);con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{merchant<-DBI::dbGetQuery(con,"SELECT * FROM merchants WHERE id=$1 AND status='open' FOR UPDATE",params=list(as.integer(merchant_id)));if(!nrow(merchant))stop("Merchant is no longer open.")
    row<-DBI::dbGetQuery(con,"SELECT id,state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(as.character(character_id)));if(!nrow(row))stop("Character not found.");char<-validate_character(unserialize(row$state_blob[[1L]]));char<-hydrate_character_inventory_relational(con,char,as.character(character_id));inv<-inventory_normalize(char$inventory$items)
    quote<-NULL;if(!is.null(quote_id)){quote<-DBI::dbGetQuery(con,"SELECT * FROM merchant_quotes WHERE id=$1 AND merchant_id=$2 AND character_id=$3 AND status='pending' FOR UPDATE",params=list(as.integer(quote_id),as.integer(merchant_id),as.character(character_id)));if(!nrow(quote))stop("That quote is no longer available.");direction<-as.character(quote$direction[[1L]]);stock_id<-quote$stock_id[[1L]];player_item_id<-quote$player_item_id[[1L]]}
    skill_bonus<-character_skill_modifier(char,"Persuasion");haggle_roll<-sample.int(20L,1L)+skill_bonus
    if(direction=="buy"){
      stock<-DBI::dbGetQuery(con,"SELECT * FROM merchant_stock WHERE id=$1 AND merchant_id=$2 AND quantity>0 FOR UPDATE",params=list(as.integer(stock_id),as.integer(merchant_id)));if(!nrow(stock))stop("That item has sold out.");terms<-if(!is.null(quote))list(base=as.numeric(quote$base_value[[1L]]),price=as.numeric(quote$final_price[[1L]]),roll=as.integer(quote$haggle_roll[[1L]]),dc=as.integer(quote$haggle_dc[[1L]]),success=isTRUE(quote$haggle_success[[1L]]))else merchant_haggle_terms(stock$base_value[[1L]],"buy",merchant$temperament[[1L]],haggle_roll,pricing_style=as.character(merchant$pricing_style[[1L]]%||%"standard"));if(as.numeric(char$inventory$gold)<terms$price)stop(paste0("You need ",terms$price," gold but only have ",as.numeric(char$inventory$gold),"."))
      item<-enemy_db_json(stock$item_json[[1L]],list());item_name<-as.character(item$name%||%"Item");is_animal<-identical(tolower(as.character(item$type%||%"")),"animal")
      if(is_animal){animal_id<-as.character(item$catalogue_id%||%item$id%||%"");animal<-DBI::dbGetQuery(con,"SELECT id,max_hp FROM animals WHERE id=$1",params=list(animal_id));if(!nrow(animal))stop("That animal is no longer in the livestock catalogue.");DBI::dbExecute(con,"INSERT INTO character_animals(character_id,animal_id,current_hp) VALUES($1,$2,$3)",params=list(as.character(character_id),animal_id,as.integer(animal$max_hp[[1L]])))}else{item$id<-paste0("merchant_",merchant_id,"_",stock$id[[1L]],"_",sample(1000:9999,1));item$qty<-1;item$equipped<-FALSE;item$edit<-FALSE;newrow<-enemy_loot_to_inventory_row(item,id=item$id);char$inventory$items<-inventory_normalize(rbind(inv,newrow))};char$inventory$gold<-as.numeric(char$inventory$gold)-terms$price;DBI::dbExecute(con,"UPDATE merchant_stock SET quantity=quantity-1 WHERE id=$1",params=list(stock$id[[1L]]));DBI::dbExecute(con,"UPDATE merchants SET gold=gold+$2,updated_at=now() WHERE id=$1",params=list(as.integer(merchant_id),terms$price))
    }else{
      sale_id<-as.character(player_item_id);selling_animal<-startsWith(sale_id,"animal:")
      if(selling_animal){stable_id<-suppressWarnings(as.integer(sub("^animal:","",sale_id)));sold_animal<-DBI::dbGetQuery(con,"SELECT ca.id,ca.animal_id,COALESCE(NULLIF(ca.custom_name,''),a.name) AS display_name,a.* FROM character_animals ca JOIN animals a ON a.id=ca.animal_id WHERE ca.id=$1 AND ca.character_id=$2 FOR UPDATE",params=list(stable_id,as.character(character_id)));if(!nrow(sold_animal))stop("That animal is no longer in your stable.");base_value<-as.numeric(sold_animal$gold_value[[1L]]);item_name<-as.character(sold_animal$display_name[[1L]]);item<-list(id=as.character(sold_animal$animal_id[[1L]]),catalogue_id=as.character(sold_animal$animal_id[[1L]]),name=as.character(sold_animal$name[[1L]]),type="animal",desc=as.character(sold_animal$description[[1L]]),value=base_value,weight=0,qty=1L,meta=list(category="animal",species=as.character(sold_animal$species[[1L]]),speed=as.integer(sold_animal$speed[[1L]]),armour_class=as.integer(sold_animal$armour_class[[1L]]),max_hp=as.integer(sold_animal$max_hp[[1L]]),mountable=isTRUE(sold_animal$mountable[[1L]])))}else{idx<-match(sale_id,inv$id);if(is.na(idx))stop("That item is no longer in your inventory.");sold<-inv[idx,,drop=FALSE];base_value<-as.numeric(sold$value[[1L]]);item_name<-as.character(sold$name[[1L]]);item<-as.list(sold[1,,drop=FALSE]);item$meta<-sold$meta[[1L]];item$qty<-1L}
      terms<-if(!is.null(quote))list(base=as.numeric(quote$base_value[[1L]]),price=as.numeric(quote$final_price[[1L]]),roll=as.integer(quote$haggle_roll[[1L]]),dc=as.integer(quote$haggle_dc[[1L]]),success=isTRUE(quote$haggle_success[[1L]]))else merchant_haggle_terms(base_value,"sell",merchant$temperament[[1L]],haggle_roll);if(as.numeric(merchant$gold[[1L]])<terms$price)stop(paste0("The merchant needs ",terms$price," gold but only has ",as.numeric(merchant$gold[[1L]]),"."))
      if(selling_animal)DBI::dbExecute(con,"DELETE FROM character_animals WHERE id=$1 AND character_id=$2",params=list(stable_id,as.character(character_id)))else{if(as.numeric(sold$qty[[1L]])>1){inv$qty[[idx]]<-as.numeric(inv$qty[[idx]])-1}else inv<-inv[-idx,,drop=FALSE];char$inventory$items<-inventory_normalize(inv)};char$inventory$gold<-as.numeric(char$inventory$gold)+terms$price;DBI::dbExecute(con,"UPDATE merchants SET gold=gold-$2,updated_at=now() WHERE id=$1",params=list(as.integer(merchant_id),terms$price));DBI::dbExecute(con,"INSERT INTO merchant_stock(merchant_id,catalogue_id,item_json,quantity,base_value) VALUES($1,$2,$3::jsonb,1,$4)",params=list(as.integer(merchant_id),as.character(item$catalogue_id%||%item$id%||%""),enemy_json(item),max(0,base_value)))
    }
    natural_roll<-if(!is.null(quote)&&"natural_roll"%in%names(quote))as.integer(quote$natural_roll[[1L]])else NA_integer_;DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,char_name=$2,updated_at=now() WHERE id::text=$3",params=list(list(serialize(char,NULL)),char$meta$name,as.character(character_id)));sync_character_inventory_relational(con,char,as.character(character_id));DBI::dbExecute(con,"INSERT INTO merchant_transactions(merchant_id,character_id,direction,item_name,base_value,final_price,haggle_roll,haggle_dc,haggle_success,natural_roll) VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10)",params=list(as.integer(merchant_id),as.character(character_id),direction,item_name,terms$base,terms$price,terms$roll,terms$dc,terms$success,natural_roll));if(!is.null(quote)){DBI::dbExecute(con,"UPDATE merchant_quotes SET status='accepted',resolved_at=now() WHERE id=$1",params=list(as.integer(quote_id)));DBI::dbExecute(con,"UPDATE merchant_invitations SET haggle_rejections=0,updated_at=now() WHERE merchant_id=$1 AND character_id=$2",params=list(as.integer(merchant_id),as.character(character_id)))};list(character=char,item_name=item_name,direction=direction,terms=terms,natural_roll=natural_roll,is_animal=exists("is_animal")&&isTRUE(is_animal),merchant_gold=if(direction=="buy")as.numeric(merchant$gold[[1L]])+terms$price else as.numeric(merchant$gold[[1L]])-terms$price)
  }),error=function(e){message("merchant_trade failed: ",e$message);structure(list(),error=e$message)})
}

# ============================================================
# READ HELPERS
# ============================================================



get_encounter_enemies <- function(encounter_id) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  on.exit(release_db_connection(con), add = TRUE)
  
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  if (is.na(encounter_id) || encounter_id < 1) return(data.frame())
  
  res <- tryCatch(
    DBI::dbGetQuery(
      con,
      "
           SELECT
        id,
        encounter_id,
        enemy_uuid,
        name,
        template_key,
        hp_max,
        hp_current,
        temp_hp,
        ac,
        initiative,
        turn_order,
        is_active,
        movement_speed,
        enemy_type,
        characteristics,
        abilities,
        attacks,
        loot,
        resistances,
        immunities,
        vulnerabilities,
        condition_immunities,
        gold_min,
        gold_max,
        looted_by,
        looted_at,
        attack_bonus,
        damage_expr,
        damage_type,
        notes,
        created_at,
        updated_at
      FROM encounter_enemies
      WHERE encounter_id = $1
      ORDER BY created_at ASC, id ASC
      ",
      params = list(encounter_id)
    ),
    error = function(e) {
      message("get_encounter_enemies failed: ", e$message)
      data.frame()
    }
  )
  
  if (!is.data.frame(res) || nrow(res) == 0) return(data.frame())
  
  int_cols <- c(
    "id", "encounter_id", "hp_max", "hp_current", "temp_hp",
    "ac", "initiative", "turn_order", "movement_speed", "attack_bonus"
  )
  for (nm in intersect(int_cols, names(res))) {
    res[[nm]] <- suppressWarnings(as.integer(res[[nm]]))
  }
  
  if ("is_active" %in% names(res)) {
    res$is_active <- as.logical(res$is_active)
  }
  
  res$enemy_uuid   <- as.character(res$enemy_uuid %||% "")
  res$name         <- as.character(res$name %||% "")
  res$template_key <- as.character(res$template_key %||% "")
  res$damage_expr  <- as.character(res$damage_expr %||% "")
  res$damage_type  <- as.character(res$damage_type %||% "")
  res$notes        <- as.character(res$notes %||% "")
  
  res
}

get_game_session <- function(session_id) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM game_sessions
      WHERE id = $1
      ",
      params = list(as.integer(session_id))
    ),
    error = function(e) {
      message("get_game_session failed: ", e$message)
      data.frame()
    }
  )
}

get_session_players <- function(session_id) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        sp.*,
        cb.char_name
      FROM session_players sp
      LEFT JOIN character_blobs cb
        ON cb.id = sp.character_id
      WHERE sp.session_id = $1
      ORDER BY sp.turn_order NULLS LAST, sp.id
      ",
      params = list(as.integer(session_id))
    ),
    error = function(e) {
      message("get_session_players failed: ", e$message)
      data.frame()
    }
  )
}

get_active_session_for_character <- function(character_id) {
  character_id <- as.character(character_id %||% "")
  if (!nzchar(character_id)) return(data.frame())

  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(safe_db_disconnect(con), add = TRUE)

  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        gs.id AS session_id,
        gs.active_encounter_id,
        gs.active_map_id,
        gs.mode,
        gs.status
      FROM session_players sp
      INNER JOIN game_sessions gs
        ON gs.id = sp.session_id
      WHERE sp.character_id = $1
        AND sp.is_active = TRUE
        AND gs.status = 'active'
      ORDER BY gs.updated_at DESC NULLS LAST, gs.id DESC
      LIMIT 1
      ",
      params = list(character_id)
    ),
    error = function(e) {
      message("get_active_session_for_character failed: ", e$message)
      data.frame()
    }
  )
}

get_combat_state <- function(encounter_id) {
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  if (is.na(encounter_id) || encounter_id < 1) return(data.frame())
  
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM combat_state
      WHERE encounter_id = $1
      ORDER BY updated_at DESC, id DESC
      LIMIT 1
      ",
      params = list(encounter_id)
    ),
    error = function(e) {
      message("get_combat_state failed: ", e$message)
      data.frame()
    }
  )
}

get_game_events <- function(session_id, limit = 50) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM game_events
      WHERE session_id = $1
      ORDER BY created_at DESC
      LIMIT $2
      ",
      params = list(as.integer(session_id), as.integer(limit))
    ),
    error = function(e) {
      message("get_game_events failed: ", e$message)
      data.frame()
    }
  )
}

# ============================================================
# WRITE HELPERS
# ============================================================


set_session_hp <- function(session_id, character_id, current_hp, temp_hp = 0) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      UPDATE session_players
      SET current_hp = $1,
          temp_hp = $2,
          updated_at = NOW()
      WHERE session_id = $3
        AND character_id = $4
      ",
      params = list(
        as.integer(current_hp),
        as.integer(temp_hp),
        as.integer(session_id),
        as.character(character_id)
      )
    )
    TRUE
  }, error = function(e) {
    message("set_session_hp failed: ", e$message)
    FALSE
  })
}

set_combat_state <- function(
    encounter_id,
    round_number = 1L,
    current_turn_order = 1L,
    active_actor_id = NULL,
    active_actor_type = "player",
    phase = "combat"
) {
  `%||%` <- get("%||%", inherits = TRUE)
  
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  round_number <- suppressWarnings(as.integer(round_number))
  current_turn_order <- suppressWarnings(as.integer(current_turn_order))
  active_actor_id <- as.character(active_actor_id %||% "")
  active_actor_type <- as.character(active_actor_type %||% "player")
  phase <- as.character(phase %||% "combat")
  
  if (is.na(encounter_id) || encounter_id < 1) return(FALSE)
  if (is.na(round_number) || round_number < 1) round_number <- 1L
  if (is.na(current_turn_order) || current_turn_order < 1) current_turn_order <- 1L
  if (!nzchar(active_actor_id)) return(FALSE)
  if (!nzchar(active_actor_type)) active_actor_type <- "player"
  if (!nzchar(phase)) phase <- "combat"
  
  enc <- tryCatch(get_encounter(encounter_id), error = function(e) data.frame())
  if (!is.data.frame(enc) || nrow(enc) == 0) {
    message("set_combat_state failed: encounter not found for encounter_id=", encounter_id)
    return(FALSE)
  }
  
  session_id <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
  if (is.na(session_id) || session_id < 1) {
    message("set_combat_state failed: encounter has invalid session_id")
    return(FALSE)
  }
  
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch({
    existing <- DBI::dbGetQuery(
      con,
      "
      SELECT id
      FROM combat_state
      WHERE encounter_id = $1
      ORDER BY id DESC
      LIMIT 1
      ",
      params = list(encounter_id)
    )
    
    if (is.data.frame(existing) && nrow(existing) > 0) {
      DBI::dbExecute(
        con,
        "
        UPDATE combat_state
        SET session_id = $2,
            round_number = $3,
            current_turn_order = $4,
            active_actor_type = $5,
            active_actor_id = $6,
            phase = $7,
            updated_at = NOW()
        WHERE encounter_id = $1
        ",
        params = list(
          encounter_id,
          session_id,
          round_number,
          current_turn_order,
          active_actor_type,
          active_actor_id,
          phase
        )
      )
    } else {
      DBI::dbExecute(
        con,
        "
        INSERT INTO combat_state (
          session_id,
          encounter_id,
          round_number,
          current_turn_order,
          active_actor_type,
          active_actor_id,
          phase
        )
        VALUES ($1, $2, $3, $4, $5, $6, $7)
        ",
        params = list(
          session_id,
          encounter_id,
          round_number,
          current_turn_order,
          active_actor_type,
          active_actor_id,
          phase
        )
      )
    }
    
    TRUE
  }, error = function(e) {
    message("set_combat_state failed: ", e$message)
    FALSE
  })
}

library(DBI)
library(RPostgres)



is_adjacent_5ft <- function(x1, y1, x2, y2) {
  dx <- abs(as.integer(x1) - as.integer(x2))
  dy <- abs(as.integer(y1) - as.integer(y2))
  max(dx, dy) == 1L
}

get_opportunity_attackers <- function(encounter_id, mover_id, mover_type, old_x, old_y, new_x, new_y) {
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  mover_id <- as.character(mover_id %||% "")
  mover_type <- as.character(mover_type %||% "")
  old_x <- suppressWarnings(as.integer(old_x))
  old_y <- suppressWarnings(as.integer(old_y))
  new_x <- suppressWarnings(as.integer(new_x))
  new_y <- suppressWarnings(as.integer(new_y))
  
  if (is.na(encounter_id) || encounter_id < 1) return(data.frame())
  if (!nzchar(mover_id) || !nzchar(mover_type)) return(data.frame())
  if (any(is.na(c(old_x, old_y, new_x, new_y)))) return(data.frame())
  
  actors <- tryCatch(get_encounter_actors(encounter_id), error = function(e) data.frame())
  pos <- tryCatch(get_encounter_positions(encounter_id), error = function(e) data.frame())
  
  if (!is.data.frame(actors) || nrow(actors) == 0) return(data.frame())
  if (!is.data.frame(pos) || nrow(pos) == 0) return(data.frame())
  
  # normalise actor columns
  if (!"actor_id" %in% names(actors)) actors$actor_id <- character()
  if (!"actor_type" %in% names(actors)) actors$actor_type <- character()
  
  actors$actor_id <- as.character(actors$actor_id)
  actors$actor_type <- as.character(actors$actor_type)
  # get_encounter_actors may already contain coordinates. Drop them before the
  # authoritative positions join so merge() does not create x.x/x.y columns.
  actors$x <- NULL
  actors$y <- NULL
  
  # normalise position columns
  keep_cols <- intersect(c("actor_id", "actor_type", "x", "y"), names(pos))
  pos <- pos[, keep_cols, drop = FALSE]
  
  if (!all(c("actor_id", "actor_type", "x", "y") %in% names(pos))) return(data.frame())
  
  pos$actor_id <- as.character(pos$actor_id)
  pos$actor_type <- as.character(pos$actor_type)
  pos$x <- suppressWarnings(as.integer(pos$x))
  pos$y <- suppressWarnings(as.integer(pos$y))
  
  pos <- pos[!is.na(pos$x) & !is.na(pos$y), , drop = FALSE]
  if (nrow(pos) == 0) return(data.frame())
  
  merged <- merge(
    actors,
    pos,
    by = c("actor_id", "actor_type"),
    all = FALSE
  )
  
  if (!is.data.frame(merged) || nrow(merged) == 0) return(data.frame())
  
  merged$actor_id <- as.character(merged$actor_id)
  merged$actor_type <- as.character(merged$actor_type)
  merged$x <- suppressWarnings(as.integer(merged$x))
  merged$y <- suppressWarnings(as.integer(merged$y))
  
  others <- merged[
    merged$actor_id != mover_id,
    ,
    drop = FALSE
  ]
  
  if (nrow(others) == 0) return(data.frame())
  
  was_adj <- vapply(
    seq_len(nrow(others)),
    function(i) {
      isTRUE(is_adjacent_5ft(
        x1 = others$x[i],
        y1 = others$y[i],
        x2 = old_x,
        y2 = old_y
      ))
    },
    logical(1)
  )
  
  now_adj <- vapply(
    seq_len(nrow(others)),
    function(i) {
      isTRUE(is_adjacent_5ft(
        x1 = others$x[i],
        y1 = others$y[i],
        x2 = new_x,
        y2 = new_y
      ))
    },
    logical(1)
  )
  
  out <- others[was_adj & !now_adj, , drop = FALSE]
  if (nrow(out) == 0) return(data.frame())
  
  if (identical(mover_type, "enemy")) {
    out <- out[out$actor_type == "player", , drop = FALSE]
  } else if (identical(mover_type, "player")) {
    out <- out[out$actor_type == "enemy", , drop = FALSE]
  }
  
  out
}

log_game_event <- function(encounter_id,
                           event_type,
                           actor_type = NULL,
                           actor_id = NULL,
                           target_id = NULL,
                           payload = list()) {
  
  enc <- tryCatch(get_encounter(encounter_id), error = function(e) data.frame())
  if (!is.data.frame(enc) || nrow(enc) == 0) return(FALSE)
  
  session_id <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
  if (is.na(session_id) || session_id < 1) return(FALSE)
  
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  safe_scalar_chr <- function(x) {
    if (is.null(x)) return(NA_character_)
    x <- as.character(x)
    if (length(x) < 1) return(NA_character_)
    x[1]
  }
  
  actor_type <- safe_scalar_chr(actor_type)
  actor_id   <- safe_scalar_chr(actor_id)
  target_id  <- safe_scalar_chr(target_id)
  
  payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      INSERT INTO game_events (
        session_id, encounter_id, event_type, actor_type, actor_id, target_id, payload
      )
      VALUES ($1, $2, $3, $4, $5, $6, $7::jsonb)
      ",
      params = list(
        session_id,
        as.integer(encounter_id),
        as.character(event_type),
        actor_type,
        actor_id,
        target_id,
        as.character(payload_json)
      )
    )
    TRUE
  }, error = function(e) {
    message("log_game_event failed: ", e$message)
    FALSE
  })
}


heal_session_player <- function(session_id, character_id, amount) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch({
    row <- DBI::dbGetQuery(
      con,
      "
      SELECT current_hp, temp_hp
      FROM session_players
      WHERE session_id = $1
        AND character_id = $2
      ",
      params = list(
        as.integer(session_id),
        as.character(character_id)
      )
    )
    
    if (nrow(row) == 0) return(NULL)
    
    old_hp <- as.integer(row$current_hp[1] %||% 0)
    heal   <- max(0L, as.integer(amount %||% 0))
    new_hp <- old_hp + heal
    
    DBI::dbExecute(
      con,
      "
      UPDATE session_players
      SET current_hp = $1,
          updated_at = NOW()
      WHERE session_id = $2
        AND character_id = $3
      ",
      params = list(
        new_hp,
        as.integer(session_id),
        as.character(character_id)
      )
    )
    
    list(
      hp_before = old_hp,
      hp_after = new_hp,
      amount = heal
    )
  }, error = function(e) {
    message("heal_session_player failed: ", e$message)
    NULL
  })
}



empty_map_occupants <- function() {
  data.frame(
    map_id = integer(),
    encounter_id = integer(),
    actor_type = character(),
    actor_id = character(),
    x = integer(),
    y = integer(),
    stringsAsFactors = FALSE
  )
}

set_actor_position_local <- function(occupants, actor_id, x, y,
                                     map_id = 1L, encounter_id = 1L,
                                     actor_type = "player") {
  occupants <- occupants %||% empty_map_occupants()
  
  actor_id <- as.character(actor_id %||% "")
  if (!nzchar(actor_id)) return(occupants)
  
  idx <- which(
    as.character(occupants$actor_id) == actor_id &
      as.integer(occupants$map_id) == as.integer(map_id) &
      as.integer(occupants$encounter_id) == as.integer(encounter_id)
  )
  
  if (length(idx) > 0) {
    occupants$x[idx[1]] <- as.integer(x)
    occupants$y[idx[1]] <- as.integer(y)
    occupants$actor_type[idx[1]] <- as.character(actor_type)
  } else {
    occupants <- rbind(
      occupants,
      data.frame(
        map_id = as.integer(map_id),
        encounter_id = as.integer(encounter_id),
        actor_type = as.character(actor_type),
        actor_id = actor_id,
        x = as.integer(x),
        y = as.integer(y),
        stringsAsFactors = FALSE
      )
    )
  }
  
  occupants
}

get_actor_position <- function(occupants, actor_id, map_id = NULL, encounter_id = NULL) {
  occupants <- occupants %||% empty_map_occupants()
  actor_id <- as.character(actor_id %||% "")
  if (!nzchar(actor_id) || !nrow(occupants)) return(occupants[0, , drop = FALSE])
  
  out <- occupants[as.character(occupants$actor_id) == actor_id, , drop = FALSE]
  
  if (!is.null(map_id)) {
    out <- out[as.integer(out$map_id) == as.integer(map_id), , drop = FALSE]
  }
  
  if (!is.null(encounter_id)) {
    out <- out[as.integer(out$encounter_id) == as.integer(encounter_id), , drop = FALSE]
  }
  
  out
}
remove_encounter_enemy <- function(enemy_uuid) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  
  on.exit(release_db_connection(con), add = TRUE)
  
  enemy_uuid <- as.character(enemy_uuid %||% "")
  if (!nzchar(enemy_uuid)) return(FALSE)
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      DELETE FROM encounter_enemies
      WHERE enemy_uuid = $1
      ",
      params = list(enemy_uuid)
    )
    TRUE
  }, error = function(e) {
    message("remove_encounter_enemy failed: ", e$message)
    FALSE
  })
}


get_session_encounters <- function(session_id) {
  session_id <- suppressWarnings(as.integer(session_id))
  if (is.na(session_id) || session_id < 1) return(data.frame())
  
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        id AS encounter_id,
        session_id,
        name,
        map_id,
        status,
        created_at,
        updated_at
      FROM encounters
      WHERE session_id = $1
      ORDER BY updated_at DESC NULLS LAST, id DESC
      ",
      params = list(session_id)
    ),
    error = function(e) {
      message("get_session_encounters failed: ", e$message)
      data.frame()
    }
  )
}


get_encounter <- function(encounter_id) {
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  if (is.na(encounter_id) || encounter_id < 1) return(data.frame())
  
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        id AS encounter_id,
        session_id,
        name,
        map_id,
        status,
        created_at,
        updated_at
      FROM encounters
      WHERE id = $1
      LIMIT 1
      ",
      params = list(encounter_id)
    ),
    error = function(e) {
      message("get_encounter failed: ", e$message)
      data.frame()
    }
  )
}

set_encounter_map <- function(encounter_id, map_id) {
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  map_id <- suppressWarnings(as.integer(map_id))
  
  if (is.na(encounter_id) || encounter_id < 1) return(FALSE)
  if (is.na(map_id) || map_id < 1) map_id <- 1L
  
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      UPDATE encounters
      SET map_id = $2,
          updated_at = NOW()
      WHERE id = $1
      ",
      params = list(encounter_id, map_id)
    )
    TRUE
  }, error = function(e) {
    message("set_encounter_map failed: ", e$message)
    FALSE
  })
}

set_encounter_status <- function(encounter_id, status = "active") {
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  status <- as.character(status %||% "active")
  
  if (is.na(encounter_id) || encounter_id < 1) return(FALSE)
  
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      UPDATE encounters
      SET status = $2,
          updated_at = NOW()
      WHERE id = $1
      ",
      params = list(encounter_id, status)
    )
    TRUE
  }, error = function(e) {
    message("set_encounter_status failed: ", e$message)
    FALSE
  })
}
upsert_encounter_actor_position <- function(encounter_id, actor_type, actor_id, x, y) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  
  on.exit(release_db_connection(con), add = TRUE)
  
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  actor_type <- as.character(actor_type %||% "")
  actor_id <- as.character(actor_id %||% "")
  x <- suppressWarnings(as.integer(x))
  y <- suppressWarnings(as.integer(y))
  
  if (is.na(encounter_id) || encounter_id < 1) return(FALSE)
  if (!nzchar(actor_type) || !nzchar(actor_id)) return(FALSE)
  if (is.na(x) || is.na(y)) return(FALSE)
  
  tryCatch({
    existing <- DBI::dbGetQuery(
      con,
      "
      SELECT id
      FROM encounter_positions
      WHERE encounter_id = $1
        AND actor_type = $2
        AND actor_id = $3
      ",
      params = list(encounter_id, actor_type, actor_id)
    )
    
    if (nrow(existing) > 0) {
      DBI::dbExecute(
        con,
        "
        UPDATE encounter_positions
        SET x = $1,
            y = $2,
            updated_at = NOW()
        WHERE encounter_id = $3
          AND actor_type = $4
          AND actor_id = $5
        ",
        params = list(x, y, encounter_id, actor_type, actor_id)
      )
    } else {
      DBI::dbExecute(
        con,
        "
        INSERT INTO encounter_positions (
          encounter_id,
          actor_type,
          actor_id,
          x,
          y,
          updated_at
        )
        VALUES ($1, $2, $3, $4, $5, NOW())
        ",
        params = list(encounter_id, actor_type, actor_id, x, y)
      )
    }
    
    TRUE
  }, error = function(e) {
    message("upsert_encounter_actor_position failed: ", e$message)
    FALSE
  })
}

get_encounter_positions <- function(encounter_id) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  on.exit(release_db_connection(con), add = TRUE)
  
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  if (is.na(encounter_id) || encounter_id < 1) return(data.frame())
  
  res <- tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        id,
        encounter_id,
        actor_type,
        actor_id,
        x,
        y,
        updated_at
      FROM encounter_positions
      WHERE encounter_id = $1
      ORDER BY id ASC
      ",
      params = list(encounter_id)
    ),
    error = function(e) {
      message("get_encounter_positions failed: ", e$message)
      data.frame()
    }
  )
  
  if (!is.data.frame(res) || nrow(res) == 0) return(data.frame())
  
  for (nm in intersect(c("id", "encounter_id", "x", "y"), names(res))) {
    res[[nm]] <- suppressWarnings(as.integer(res[[nm]]))
  }
  res$actor_type <- as.character(res$actor_type %||% "")
  res$actor_id <- as.character(res$actor_id %||% "")
  
  res
}

calculate_hp_damage <- function(current_hp, temp_hp, amount) {
  old_hp <- max(0L, suppressWarnings(as.integer(current_hp %||% 0L)))
  old_temp <- max(0L, suppressWarnings(as.integer(temp_hp %||% 0L)))
  damage <- max(0L, suppressWarnings(as.integer(amount %||% 0L)))

  temp_absorb <- min(old_temp, damage)
  remaining <- damage - temp_absorb

  list(
    hp_before = old_hp,
    hp_after = max(0L, old_hp - remaining),
    temp_before = old_temp,
    temp_after = old_temp - temp_absorb,
    amount = damage
  )
}

damage_session_player <- function(session_id, character_id, amount) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch({
    row <- DBI::dbGetQuery(
      con,
      "
      SELECT current_hp, temp_hp
      FROM session_players
      WHERE session_id = $1
        AND character_id = $2
      ",
      params = list(
        as.integer(session_id),
        as.character(character_id)
      )
    )
    
    if (nrow(row) == 0) return(NULL)
    
    result <- calculate_hp_damage(
      row$current_hp[1] %||% 0,
      row$temp_hp[1] %||% 0,
      amount
    )
    
    DBI::dbExecute(
      con,
      "
      UPDATE session_players
      SET current_hp = $1,
          temp_hp = $2,
          updated_at = NOW()
      WHERE session_id = $3
        AND character_id = $4
      ",
      params = list(
        result$hp_after,
        result$temp_after,
        as.integer(session_id),
        as.character(character_id)
      )
    )
    
    result
  }, error = function(e) {
    message("damage_session_player failed: ", e$message)
    NULL
  })
}

next_combat_turn <- function(actors, combat) {
  if (!is.data.frame(actors) || nrow(actors) == 0) return(FALSE)

  if ("is_active" %in% names(actors)) actors <- actors[is.na(actors$is_active) | actors$is_active %in% TRUE, , drop=FALSE]
  hp <- suppressWarnings(as.integer(actors$current_hp %||% actors$hp_current %||% NA_integer_))
  defeated_non_players <- as.character(actors$actor_type %||% "") %in% c("enemy","summon") & !is.na(hp) & hp <= 0L
  actors <- actors[!defeated_non_players, , drop=FALSE]
  if (!nrow(actors)) return(FALSE)
  
  actors <- actors[order(actors$turn_order, actors$actor_type, actors$actor_id, na.last = TRUE), , drop = FALSE]
  actors <- actors[!is.na(actors$turn_order), , drop = FALSE]
  if (nrow(actors) == 0) return(FALSE)
  
  current_order <- suppressWarnings(as.integer(combat$current_turn_order[1] %||% NA))
  current_round <- suppressWarnings(as.integer(combat$round_number[1] %||% 1L))
  
  if (is.na(current_round) || current_round < 1) current_round <- 1L
  
  current_actor_id <- as.character(combat$active_actor_id[1] %||% "")
  current_actor_type <- as.character(combat$active_actor_type[1] %||% "")
  idx <- which(
    as.character(actors$actor_id) == current_actor_id &
      as.character(actors$actor_type) == current_actor_type
  )[1]
  if (!length(idx) || is.na(idx)) {
    idx <- which(as.integer(actors$turn_order) == current_order)[1]
  }
  if (!length(idx) || is.na(idx)) idx <- 0L

  if (idx >= nrow(actors)) {
    next_actor <- actors[1, , drop = FALSE]
    next_round <- current_round + 1L
  } else {
    next_actor <- actors[idx + 1L, , drop = FALSE]
    next_round <- current_round
  }

  list(
    round_number = as.integer(next_round),
    turn_order = as.integer(next_actor$turn_order[1]),
    actor_type = as.character(next_actor$actor_type[1] %||% "player"),
    actor_id = as.character(next_actor$actor_id[1] %||% "")
  )
}

advance_turn <- function(encounter_id) {
  actors <- get_encounter_actors(encounter_id)
  combat <- get_combat_state(encounter_id)
  next_turn <- next_combat_turn(actors, combat)
  if (identical(next_turn, FALSE)) return(FALSE)
  
  ok1 <- set_combat_state(
    encounter_id = encounter_id,
    round_number = next_turn$round_number,
    current_turn_order = next_turn$turn_order,
    active_actor_type = next_turn$actor_type,
    active_actor_id = next_turn$actor_id,
    phase = "combat"
  )
  
  ok2 <- log_game_event(
    encounter_id = encounter_id,
    event_type = "end_turn",
    actor_type = "system",
    payload = list(
      next_turn_order = next_turn$turn_order,
      next_actor_id = next_turn$actor_id,
      next_actor_type = next_turn$actor_type,
      round_number = next_turn$round_number
    )
  )
  
  isTRUE(ok1) && isTRUE(ok2)
}

end_encounter_combat <- function(encounter_id) {
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  enc <- tryCatch(get_encounter(encounter_id), error = function(e) data.frame())
  if (is.na(encounter_id) || !is.data.frame(enc) || !nrow(enc)) return(FALSE)
  session_id <- suppressWarnings(as.integer(enc$session_id[1] %||% NA_integer_))
  if (is.na(session_id)) return(FALSE)
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(release_db_connection(con), add = TRUE)
  tryCatch({
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, "UPDATE encounters SET status = 'completed', updated_at = NOW() WHERE id = $1", list(encounter_id))
      DBI::dbExecute(con, "UPDATE combat_state SET phase = 'ended', active_actor_type = NULL, active_actor_id = NULL, updated_at = NOW() WHERE encounter_id = $1", list(encounter_id))
      DBI::dbExecute(con, "UPDATE game_sessions SET mode = 'exploration', active_encounter_id = NULL, active_actor_type = NULL, active_actor_id = NULL, updated_at = NOW() WHERE id = $1", list(session_id))
      log_game_event(encounter_id, "end_combat", "control", payload = list(round_number = NA_integer_))
    })
    TRUE
  }, error = function(e) {
    message("end_encounter_combat failed: ", e$message)
    FALSE
  })
}


heal_encounter_enemy <- function(encounter_id, enemy_uuid, amount) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  enemy_uuid <- as.character(enemy_uuid)
  amount <- suppressWarnings(as.integer(amount))
  if (is.na(amount) || amount <= 0) return(NULL)
  
  row <- tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT hp_current, hp_max
      FROM encounter_enemies
      WHERE encounter_id = $1 AND enemy_uuid = $2
      ",
      params = list(encounter_id, enemy_uuid)
    ),
    error = function(e) NULL
  )
  
  if (is.null(row) || nrow(row) == 0) return(NULL)
  
  hp_before <- as.integer(row$hp_current[1])
  hp_max <- as.integer(row$hp_max[1])
  
  hp_after <- min(hp_before + amount, hp_max)
  
  ok <- tryCatch(
    DBI::dbExecute(
      con,
      "
      UPDATE encounter_enemies
      SET hp_current = $3, updated_at = NOW()
      WHERE encounter_id = $1 AND enemy_uuid = $2
      ",
      params = list(encounter_id, enemy_uuid, hp_after)
    ),
    error = function(e) NULL
  )
  
  if (is.null(ok)) return(NULL)
  
  list(
    amount = amount,
    hp_before = hp_before,
    hp_after = hp_after,
    temp_before = 0,
    temp_after = 0
  )
}

create_encounter <- function(session_id, name, map_id = NULL, status = "setup") {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  
  on.exit(release_db_connection(con), add = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  map_id <- suppressWarnings(as.integer(map_id))
  name <- as.character(name %||% "")
  status <- as.character(status %||% "setup")
  
  if (is.na(session_id) || session_id < 1) return(NULL)
  if (!nzchar(name)) return(NULL)
  if (is.na(map_id)) map_id <- NA_integer_
  
  res <- tryCatch(
    DBI::dbGetQuery(
      con,
      "
      INSERT INTO encounters (
        session_id,
        name,
        map_id,
        status,
        created_at,
        updated_at
      )
      VALUES ($1, $2, $3, $4, NOW(), NOW())
      RETURNING id
      ",
      params = list(session_id, name, map_id, status)
    ),
    error = function(e) {
      message("create_encounter failed: ", e$message)
      NULL
    }
  )
  
  if (is.null(res) || nrow(res) == 0) return(NULL)
  as.integer(res$id[1])
}

set_active_encounter <- function(session_id, encounter_id) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  
  on.exit(release_db_connection(con), add = TRUE)
  
  session_id <- suppressWarnings(as.integer(session_id))
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  
  if (is.na(session_id) || session_id < 1) return(FALSE)
  if (is.na(encounter_id) || encounter_id < 1) return(FALSE)
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      UPDATE game_sessions
      SET active_encounter_id = $1,
          updated_at = NOW()
      WHERE id = $2
      ",
      params = list(encounter_id, session_id)
    )
    TRUE
  }, error = function(e) {
    message("set_active_encounter failed: ", e$message)
    FALSE
  })
}

# ============================================================
# COMBINED VIEW
# ============================================================

empty_player_live_snapshot <- function() {
  list(
    session = data.frame(),
    self_player = data.frame(),
    players = data.frame(),
    encounter = data.frame(),
    positions = data.frame(),
    combat = data.frame(),
    events = data.frame(),
    enemies = data.frame(),
    effects = data.frame(),
    summons = data.frame(),
    fetched_at = as.POSIXct(NA)
  )
}

build_snapshot_encounter_actors <- function(snapshot) {
  players <- snapshot$players %||% data.frame()
  enemies <- snapshot$enemies %||% data.frame()
  summons <- snapshot$summons %||% data.frame()
  positions <- snapshot$positions %||% data.frame()

  player_df <- data.frame(
    actor_id = as.character(players$character_id %||% rep(NA_character_, nrow(players))),
    actor_type = rep("player", nrow(players)),
    display_name = as.character(players$display_name %||% players$char_name %||% rep(NA_character_, nrow(players))),
    current_hp = suppressWarnings(as.integer(players$current_hp %||% rep(NA_integer_, nrow(players)))),
    hp_current = suppressWarnings(as.integer(players$current_hp %||% rep(NA_integer_, nrow(players)))),
    hp_max = suppressWarnings(as.integer(players$max_hp %||% players$hp_max %||% rep(NA_integer_, nrow(players)))),
    max_hp = suppressWarnings(as.integer(players$max_hp %||% players$hp_max %||% rep(NA_integer_, nrow(players)))),
    temp_hp = suppressWarnings(as.integer(players$temp_hp %||% rep(NA_integer_, nrow(players)))),
    initiative = suppressWarnings(as.integer(players$initiative %||% rep(NA_integer_, nrow(players)))),
    turn_order = suppressWarnings(as.integer(players$turn_order %||% rep(NA_integer_, nrow(players)))),
    is_active = as.logical(players$is_active %||% rep(TRUE, nrow(players))),
    stringsAsFactors = FALSE
  )

  enemy_df <- data.frame(
    actor_id = as.character(enemies$enemy_uuid %||% rep(NA_character_, nrow(enemies))),
    actor_type = rep("enemy", nrow(enemies)),
    display_name = as.character(enemies$name %||% rep(NA_character_, nrow(enemies))),
    current_hp = suppressWarnings(as.integer(enemies$hp_current %||% rep(NA_integer_, nrow(enemies)))),
    hp_current = suppressWarnings(as.integer(enemies$hp_current %||% rep(NA_integer_, nrow(enemies)))),
    hp_max = suppressWarnings(as.integer(enemies$hp_max %||% rep(NA_integer_, nrow(enemies)))),
    max_hp = suppressWarnings(as.integer(enemies$hp_max %||% rep(NA_integer_, nrow(enemies)))),
    temp_hp = suppressWarnings(as.integer(enemies$temp_hp %||% rep(NA_integer_, nrow(enemies)))),
    initiative = suppressWarnings(as.integer(enemies$initiative %||% rep(NA_integer_, nrow(enemies)))),
    turn_order = suppressWarnings(as.integer(enemies$turn_order %||% rep(NA_integer_, nrow(enemies)))),
    is_active = as.logical(enemies$is_active %||% rep(TRUE, nrow(enemies))),
    ac = suppressWarnings(as.integer(enemies$ac %||% rep(NA_integer_, nrow(enemies)))),
    movement_speed = suppressWarnings(as.integer(enemies$movement_speed %||% rep(NA_integer_, nrow(enemies)))),
    stringsAsFactors = FALSE
  )

  summon_df <- data.frame(
    actor_id = as.character(summons$summon_uuid %||% rep(NA_character_, nrow(summons))),
    actor_type = rep("summon", nrow(summons)),
    display_name = as.character(summons$name %||% rep("Summoned Beast", nrow(summons))),
    current_hp = suppressWarnings(as.integer(summons$hp_current %||% rep(NA_integer_, nrow(summons)))),
    hp_current = suppressWarnings(as.integer(summons$hp_current %||% rep(NA_integer_, nrow(summons)))),
    hp_max = suppressWarnings(as.integer(summons$hp_max %||% rep(NA_integer_, nrow(summons)))),
    max_hp = suppressWarnings(as.integer(summons$hp_max %||% rep(NA_integer_, nrow(summons)))),
    temp_hp = suppressWarnings(as.integer(summons$temp_hp %||% rep(0L, nrow(summons)))),
    initiative = suppressWarnings(as.integer(summons$initiative %||% rep(NA_integer_, nrow(summons)))),
    turn_order = suppressWarnings(as.integer(summons$turn_order %||% rep(NA_integer_, nrow(summons)))),
    is_active = as.logical(summons$is_active %||% rep(TRUE, nrow(summons))),
    stringsAsFactors = FALSE
  )

  all_columns <- Reduce(union, list(names(player_df), names(enemy_df), names(summon_df)))
  add_columns <- function(df) {
    for (name in setdiff(all_columns, names(df))) df[[name]] <- rep(NA, nrow(df))
    df[, all_columns, drop = FALSE]
  }
  actors <- rbind(add_columns(player_df), add_columns(enemy_df), add_columns(summon_df))
  if (nrow(actors) == 0) return(actors)

  actors$x <- NA_integer_
  actors$y <- NA_integer_
  if (is.data.frame(positions) && nrow(positions) > 0) {
    for (i in seq_len(nrow(actors))) {
      hit <- positions[
        as.character(positions$actor_id) == actors$actor_id[i] &
          as.character(positions$actor_type) == actors$actor_type[i],
        ,
        drop = FALSE
      ]
      if (nrow(hit) > 0) {
        actors$x[i] <- suppressWarnings(as.integer(hit$x[1] %||% NA))
        actors$y[i] <- suppressWarnings(as.integer(hit$y[1] %||% NA))
      }
    }
  }
  actors
}

record_blood_consumption <- function(character_id, session_id = NULL, campaign_day = 1L,
                                     consumption_type = "blood", source = "Unknown source",
                                     quantity = 1, sindre_value = 0) {
  con <- get_db_connection(); if (is.null(con)) return(NULL); on.exit(release_db_connection(con), add = TRUE)
  sid <- suppressWarnings(as.integer(session_id %||% NA_integer_))
  if (is.na(sid) || sid < 1L) sid <- NA_integer_
  tryCatch(DBI::dbGetQuery(con, paste(
    "INSERT INTO blood_consumption_events(character_id,session_id,campaign_day,consumption_type,source,quantity,sindre_value)",
    "VALUES($1,$2,$3,$4,$5,$6,$7) RETURNING *"
  ), params = list(as.character(character_id), sid,
    max(1L, as.integer(campaign_day %||% 1L)), as.character(consumption_type),
    as.character(source %||% "Unknown source"), as.numeric(quantity), as.numeric(sindre_value))),
  error = function(e) { message("record_blood_consumption failed: ", e$message); NULL })
}

get_blood_consumption_history <- function(character_id, limit = 30L) {
  con <- get_db_connection(); if (is.null(con)) return(data.frame()); on.exit(release_db_connection(con), add = TRUE)
  tryCatch(DBI::dbGetQuery(con, paste(
    "SELECT id,campaign_day,consumption_type,source,quantity,sindre_value,created_at",
    "FROM blood_consumption_events WHERE character_id=$1 ORDER BY campaign_day DESC,created_at DESC LIMIT $2"
  ), params = list(as.character(character_id), max(1L, as.integer(limit)))),
  error = function(e) data.frame())
}

filter_player_snapshot_visibility <- function(snapshot) {
  enemies<-snapshot$enemies%||%data.frame();effects<-snapshot$effects%||%data.frame();combat<-snapshot$combat%||%data.frame()
  if(!is.data.frame(enemies)||!nrow(enemies))return(snapshot)
  hidden_ids<-character()
  phase<-if(is.data.frame(combat)&&nrow(combat))tolower(as.character(combat$phase[[1L]]%||%""))else""
  if(phase=="exploration")hidden_ids<-as.character(enemies$enemy_uuid%||%character())
  if(is.data.frame(effects)&&nrow(effects)){
    for(i in seq_len(nrow(effects))){
      if(as.character(effects$target_actor_type[[i]]%||%"")!="enemy")next
      payload<-effects$payload[[i]]%||%list();if(!is.list(payload))payload<-tryCatch(jsonlite::fromJSON(as.character(payload),simplifyVector=FALSE),error=function(e)list())
      condition<-tolower(as.character(payload$condition%||%""));if(condition%in%c("hidden","invisible"))hidden_ids<-c(hidden_ids,as.character(effects$target_actor_id[[i]]%||%""))
    }
  }
  hidden_ids<-unique(hidden_ids[nzchar(hidden_ids)]);if(!length(hidden_ids))return(snapshot)
  snapshot$enemies<-enemies[!as.character(enemies$enemy_uuid)%in%hidden_ids,,drop=FALSE]
  positions<-snapshot$positions%||%data.frame();if(is.data.frame(positions)&&nrow(positions))snapshot$positions<-positions[!(as.character(positions$actor_type)=="enemy"&as.character(positions$actor_id)%in%hidden_ids),,drop=FALSE]
  events<-snapshot$events%||%data.frame();if(is.data.frame(events)&&nrow(events)){actor_hidden<-as.character(events$actor_id%||%"")%in%hidden_ids;target_hidden<-as.character(events$target_id%||%"")%in%hidden_ids;snapshot$events<-events[!actor_hidden&!target_hidden,,drop=FALSE]}
  snapshot$effects<-effects[!(as.character(effects$target_actor_type%||%"")=="enemy"&as.character(effects$target_actor_id%||%"")%in%hidden_ids),,drop=FALSE]
  snapshot
}

get_player_live_snapshot <- function(session_id, character_id = NULL,
                                     encounter_id = NULL, event_limit = 20L,
                                     connection_factory = get_db_connection,
                                     release_connection = safe_db_disconnect,
                                     query_function = DBI::dbGetQuery) {
  session_id <- suppressWarnings(as.integer(session_id))
  character_id <- as.character(character_id %||% "")
  encounter_id <- suppressWarnings(as.integer(encounter_id %||% NA))
  event_limit <- suppressWarnings(as.integer(event_limit))

  if (is.na(session_id) || session_id < 1) return(empty_player_live_snapshot())
  if (is.na(event_limit) || event_limit < 1) event_limit <- 20L

  con <- connection_factory()
  if (is.null(con)) return(empty_player_live_snapshot())
  on.exit(release_connection(con), add = TRUE)

  query <- function(sql, params = list()) {
    tryCatch(
      query_function(con, sql, params = params),
      error = function(e) {
        message("Live snapshot query failed: ", e$message)
        data.frame()
      }
    )
  }

  session_row <- query(
    "SELECT * FROM game_sessions WHERE id = $1",
    list(session_id)
  )

  players <- query(
    paste(
      "SELECT sp.*, cb.char_name",
      "FROM session_players sp",
      "LEFT JOIN character_blobs cb ON cb.id = sp.character_id",
      "WHERE sp.session_id = $1",
      "ORDER BY sp.turn_order NULLS LAST, sp.id"
    ),
    list(session_id)
  )

  if (is.na(encounter_id) || encounter_id < 1) {
    if (is.data.frame(session_row) && nrow(session_row) > 0 &&
        "active_encounter_id" %in% names(session_row)) {
      encounter_id <- suppressWarnings(as.integer(session_row$active_encounter_id[1] %||% NA))
    }
  }

  encounter <- data.frame()
  positions <- data.frame()
  combat <- data.frame()
  events <- data.frame()
  enemies <- data.frame()
  effects <- data.frame()
  summons <- data.frame()

  if (!is.na(encounter_id) && encounter_id > 0) {
    encounter <- query(
      "SELECT * FROM encounters WHERE id = $1",
      list(encounter_id)
    )
    positions <- query(
      paste(
        "SELECT * FROM encounter_positions",
        "WHERE encounter_id = $1",
        "ORDER BY actor_type, actor_id"
      ),
      list(encounter_id)
    )
    combat <- query(
      paste(
        "SELECT * FROM combat_state",
        "WHERE encounter_id = $1",
        "ORDER BY updated_at DESC, id DESC LIMIT 1"
      ),
      list(encounter_id)
    )
    events <- query(
      paste(
        "SELECT * FROM game_events",
        "WHERE encounter_id = $1",
        "ORDER BY created_at DESC LIMIT $2"
      ),
      list(encounter_id, event_limit)
    )
    enemies <- query(
      paste(
        "SELECT * FROM encounter_enemies",
        "WHERE encounter_id = $1 AND is_active = TRUE",
        "ORDER BY turn_order NULLS LAST, id"
      ),
      list(encounter_id)
    )
    effects <- query(
      paste("SELECT * FROM encounter_effects", "WHERE encounter_id = $1 AND is_active = TRUE", "ORDER BY created_at, id"),
      list(encounter_id)
    )
    summons <- query(
      paste("SELECT * FROM encounter_summons", "WHERE encounter_id = $1 AND is_active = TRUE", "ORDER BY turn_order NULLS LAST, id"),
      list(encounter_id)
    )
  }

  self_player <- data.frame()
  if (is.data.frame(players) && nrow(players) > 0 && nzchar(character_id)) {
    self_player <- players[
      as.character(players$character_id) == character_id,
      ,
      drop = FALSE
    ]
  }

  filter_player_snapshot_visibility(list(
    session = session_row,
    self_player = self_player,
    players = players,
    encounter = encounter,
    positions = positions,
    combat = combat,
    events = events,
    enemies = enemies,
    effects = effects,
    summons = summons,
    fetched_at = Sys.time()
  ))
}

create_encounter_effect <- function(encounter_id, source_actor_type, source_actor_id,
                                    spell_id, effect_type, payload = list(),
                                    target_actor_type = NULL, target_actor_id = NULL,
                                    center_x = NULL, center_y = NULL, radius_ft = NULL,
                                    starts_round = 1L, ends_round = NULL,
                                    concentration = FALSE) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(release_db_connection(con), add = TRUE)
  nullable_character <- function(value) {
    value <- as.character(value %||% character())
    if (!length(value) || is.na(value[1]) || !nzchar(value[1])) NA_character_ else value[1]
  }
  nullable_integer <- function(value) {
    value <- suppressWarnings(as.integer(value %||% integer()))
    if (!length(value) || is.na(value[1])) NA_integer_ else value[1]
  }
  tryCatch(DBI::dbGetQuery(con, paste(
    "INSERT INTO encounter_effects (encounter_id, source_actor_type, source_actor_id,",
    "spell_id, effect_type, target_actor_type, target_actor_id, center_x, center_y,",
    "radius_ft, starts_round, ends_round, concentration, payload)",
    "VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14::jsonb) RETURNING *"
  ), params = list(
    as.integer(encounter_id), as.character(source_actor_type), as.character(source_actor_id),
    as.character(spell_id), as.character(effect_type), nullable_character(target_actor_type),
    nullable_character(target_actor_id), nullable_integer(center_x), nullable_integer(center_y),
    nullable_integer(radius_ft), as.integer(starts_round), nullable_integer(ends_round),
    isTRUE(concentration), jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  )), error = function(e) {
    message("create_encounter_effect failed: ", e$message)
    NULL
  })
}

end_encounter_condition <- function(encounter_id, target_actor_id, condition) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(release_db_connection(con), add = TRUE)
  tryCatch({
    changed <- DBI::dbExecute(con, paste(
      "UPDATE encounter_effects SET is_active = FALSE, updated_at = NOW()",
      "WHERE encounter_id = $1 AND target_actor_id = $2",
      "AND effect_type = 'condition' AND payload->>'condition' = $3 AND is_active = TRUE"
    ), params = list(as.integer(encounter_id), as.character(target_actor_id), as.character(condition)))
    changed > 0L
  }, error = function(e) {
    message("end_encounter_condition failed: ", e$message)
    FALSE
  })
}

begin_session_long_rest <- function(session_id,character_id,current_day) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  session_id<-suppressWarnings(as.integer(session_id));character_id<-as.character(character_id%||%"");current_day<-suppressWarnings(as.integer(current_day%||%1L));if(is.na(current_day)||current_day<1L)current_day<-1L
  if(is.na(session_id)||!nzchar(character_id))return(NULL)
  tryCatch(DBI::dbWithTransaction(con,{
    DBI::dbGetQuery(con,"SELECT pg_advisory_xact_lock($1)",params=list(session_id))
    latest<-DBI::dbGetQuery(con,"SELECT * FROM session_rest_cycles WHERE session_id=$1 AND rest_type='long_rest' ORDER BY day_number DESC LIMIT 1",params=list(session_id))
    active<-DBI::dbGetQuery(con,"SELECT count(*)::integer AS n FROM session_players WHERE session_id=$1 AND is_active=TRUE",params=list(session_id))$n[[1L]]
    completed<-if(nrow(latest))DBI::dbGetQuery(con,"SELECT count(*)::integer AS n FROM session_rest_completions WHERE rest_cycle_id=$1",params=list(latest$id[[1L]]))$n[[1L]]else 0L
    if(!nrow(latest)||completed>=max(1L,active)){day_number<-max(current_day+1L,if(nrow(latest))as.integer(latest$day_number[[1L]])+1L else 2L);latest<-DBI::dbGetQuery(con,"INSERT INTO session_rest_cycles(session_id,day_number,initiated_by) VALUES($1,$2,$3) ON CONFLICT(session_id,day_number,rest_type) DO UPDATE SET session_id=EXCLUDED.session_id RETURNING *",params=list(session_id,day_number,character_id));completed<-0L}
    already<-DBI::dbGetQuery(con,"SELECT 1 FROM session_rest_completions WHERE rest_cycle_id=$1 AND character_id=$2",params=list(latest$id[[1L]],character_id))
    list(cycle_id=as.integer(latest$id[[1L]]),day_number=as.integer(latest$day_number[[1L]]),can_apply=!nrow(already),completed=as.integer(completed),active=max(1L,as.integer(active)))
  }),error=function(e){message("begin_session_long_rest failed: ",e$message);NULL})
}

complete_session_long_rest <- function(cycle_id,character_id,outcome="full") {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  outcome<-match.arg(as.character(outcome),c("full","half","skip"))
  tryCatch({DBI::dbExecute(con,"INSERT INTO session_rest_completions(rest_cycle_id,character_id,rest_outcome) VALUES($1,$2,$3) ON CONFLICT DO NOTHING",params=list(as.integer(cycle_id),as.character(character_id),outcome));DBI::dbGetQuery(con,paste("SELECT count(*)::integer AS completed,(SELECT count(*)::integer FROM session_players sp JOIN session_rest_cycles rc ON rc.session_id=sp.session_id WHERE rc.id=$1 AND sp.is_active=TRUE) AS active FROM session_rest_completions WHERE rest_cycle_id=$1"),params=list(as.integer(cycle_id)))[1,,drop=FALSE]},error=function(e){message("complete_session_long_rest failed: ",e$message);NULL})
}

get_session_fire <- function(session_id,defaults=list()) {
  row<-get_session_supplies(session_id,defaults)
  if(is.null(row)||!"has_fire"%in%names(row))return(FALSE)
  isTRUE(row$has_fire[[1L]])
}

set_session_fire <- function(session_id,lit=TRUE,defaults=list()) {
  get_session_supplies(session_id,defaults)
  con<-get_db_connection();if(is.null(con))return(FALSE);on.exit(release_db_connection(con),add=TRUE)
  tryCatch({DBI::dbExecute(con,"UPDATE session_supplies SET has_fire=$2,updated_at=now() WHERE session_id=$1",params=list(as.integer(session_id),isTRUE(lit)));TRUE},error=function(e){message("set_session_fire failed: ",e$message);FALSE})
}

get_session_supplies <- function(session_id,defaults=list()) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  value<-function(name,fallback){x<-suppressWarnings(as.integer(defaults[[name]]%||%fallback));if(is.na(x)||x<0L)fallback else x}
  tryCatch(DBI::dbGetQuery(con,paste("INSERT INTO session_supplies(session_id,wood,wood_max,water,water_max,rations,rations_max)","VALUES($1,$2,$3,$4,$5,$6,$7) ON CONFLICT(session_id) DO UPDATE SET session_id=EXCLUDED.session_id RETURNING *"),params=list(as.integer(session_id),value("wood",3L),value("wood_max",10L),value("water",3L),value("water_max",5L),value("rations",3L),value("rations_max",5L)))[1,,drop=FALSE],error=function(e){message("get_session_supplies failed: ",e$message);NULL})
}

adjust_session_supply <- function(session_id,resource,amount=0L,fill=FALSE,defaults=list()) {
  resource<-match.arg(as.character(resource),c("wood","water","rations"));row<-get_session_supplies(session_id,defaults);if(is.null(row))return(NULL)
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{locked<-DBI::dbGetQuery(con,"SELECT * FROM session_supplies WHERE session_id=$1 FOR UPDATE",params=list(as.integer(session_id)));before<-as.integer(locked[[resource]][[1L]]);maximum<-as.integer(locked[[paste0(resource,"_max")]][[1L]]);after<-if(isTRUE(fill))maximum else max(0L,min(maximum,before+as.integer(amount)));sql<-paste0("UPDATE session_supplies SET ",resource,"=$2,updated_at=now() WHERE session_id=$1 RETURNING *");updated<-DBI::dbGetQuery(con,sql,params=list(as.integer(session_id),after))[1,,drop=FALSE];list(row=updated,before=before,after=after,applied=!identical(before,after))}),error=function(e){message("adjust_session_supply failed: ",e$message);NULL})
}

create_camp_gather_request <- function(session_id,day_number,requester_id,resource,requester_bonus,helper_id=NULL) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE);helper_id<-as.character(helper_id%||%"");if(!nzchar(helper_id))helper_id<-NA_character_
  tryCatch({row<-DBI::dbGetQuery(con,paste("INSERT INTO camp_gather_requests(session_id,day_number,resource,requester_character_id,helper_character_id,requester_bonus)","SELECT $1,$2,$3,$4,$5,$6 WHERE NOT EXISTS(SELECT 1 FROM camp_gather_actions WHERE session_id=$1 AND day_number=$2 AND character_id=$4) RETURNING *"),params=list(as.integer(session_id),as.integer(day_number),as.character(resource),as.character(requester_id),helper_id,as.integer(requester_bonus)));if(!nrow(row))NULL else row[1,,drop=FALSE]},error=function(e){message("create_camp_gather_request failed: ",e$message);NULL})
}

get_pending_camp_gather_requests <- function(helper_id) {
  con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbGetQuery(con,paste("SELECT r.*,COALESCE(sp.display_name,cb.char_name,r.requester_character_id) AS requester_name FROM camp_gather_requests r","LEFT JOIN session_players sp ON sp.session_id=r.session_id AND sp.character_id::text=r.requester_character_id LEFT JOIN character_blobs cb ON cb.id::text=r.requester_character_id","WHERE r.helper_character_id=$1 AND r.status='pending' ORDER BY r.created_at"),params=list(as.character(helper_id))),error=function(e)data.frame())
}

get_finished_camp_gather_requests <- function(requester_id) {
  con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbGetQuery(con,"SELECT * FROM camp_gather_requests WHERE requester_character_id=$1 AND status IN ('resolved','declined') ORDER BY resolved_at DESC LIMIT 20",params=list(as.character(requester_id))),error=function(e)data.frame())
}

resolve_camp_gather_request <- function(request_id,responder_id,accept=TRUE,helper_bonus=0L) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  result<-tryCatch(DBI::dbWithTransaction(con,{
    r<-DBI::dbGetQuery(con,"SELECT * FROM camp_gather_requests WHERE id=$1 FOR UPDATE",params=list(as.integer(request_id)))
    if(!nrow(r)||r$status[[1L]]!="pending")stop("Gathering request is no longer pending.")
    responder_id<-as.character(responder_id);helper<-as.character(r$helper_character_id[[1L]]%||%"")
    if(nzchar(helper)&&helper!=responder_id)stop("This gathering request belongs to another helper.")
    if(!isTRUE(accept)){
      DBI::dbExecute(con,"UPDATE camp_gather_requests SET status='declined',resolved_at=now() WHERE id=$1",params=list(as.integer(request_id)))
      list(status="declined")
    }else{
      actors<-unique(c(as.character(r$requester_character_id[[1L]]),if(nzchar(helper))helper))
      used<-DBI::dbGetQuery(con,"SELECT character_id FROM camp_gather_actions WHERE session_id=$1 AND day_number=$2 AND character_id=ANY($3::text[])",params=list(r$session_id[[1L]],r$day_number[[1L]],paste0("{",paste(actors,collapse=","),"}")))
      if(nrow(used))stop("A participant has already used today's camp gathering action.")
      requester_roll<-sample.int(20L,1L)+as.integer(r$requester_bonus[[1L]]);helper_roll<-if(nzchar(helper))sample.int(20L,1L)+as.integer(helper_bonus)else NA_integer_;total<-max(c(requester_roll,helper_roll),na.rm=TRUE);amount<-camp_gathering_yield(total);resource<-as.character(r$resource[[1L]]);reward<-if(resource=="rations")camp_foraging_reward(total)else NULL
      for(actor in actors)DBI::dbExecute(con,"INSERT INTO camp_gather_actions(session_id,day_number,character_id,resource,request_id) VALUES($1,$2,$3,$4,$5)",params=list(r$session_id[[1L]],r$day_number[[1L]],actor,r$resource[[1L]],r$id[[1L]]))
      if(resource!="rations"){DBI::dbExecute(con,"INSERT INTO session_supplies(session_id) VALUES($1) ON CONFLICT DO NOTHING",params=list(r$session_id[[1L]]));sql<-paste0("UPDATE session_supplies SET ",resource,"=LEAST(",resource,"_max,",resource,"+$2),updated_at=now() WHERE session_id=$1");DBI::dbExecute(con,sql,params=list(r$session_id[[1L]],amount))}
      DBI::dbExecute(con,"UPDATE camp_gather_requests SET status='resolved',helper_bonus=$2,requester_roll=$3,helper_roll=$4,result_total=$5,yield_amount=$6,reward_json=$7::jsonb,resolved_at=now() WHERE id=$1",params=list(r$id[[1L]],if(nzchar(helper))as.integer(helper_bonus)else NA_integer_,requester_roll,helper_roll,total,amount,enemy_json(reward%||%list())))
      list(status="resolved",resource=resource,amount=amount,total=total,requester_roll=requester_roll,helper_roll=helper_roll,assisted=nzchar(helper),requester_id=as.character(r$requester_character_id[[1L]]),day_number=as.integer(r$day_number[[1L]]),reward=reward)
    }
  }),error=function(e){message("resolve_camp_gather_request failed: ",e$message);NULL})
  if(!is.null(result)&&identical(result$status,"resolved")&&identical(result$resource,"rations")&&result$amount>0L){
    granted<-tryCatch(DBI::dbWithTransaction(con,{cid<-result$requester_id;blob<-DBI::dbGetQuery(con,"SELECT state_blob FROM character_blobs WHERE id::text=$1 FOR UPDATE",params=list(cid));if(!nrow(blob))stop("Foraging character not found.");char<-validate_character(unserialize(blob$state_blob[[1L]]));char<-hydrate_character_inventory_relational(con,char,cid);char<-add_foraged_food(char,result$reward,result$day_number);DBI::dbExecute(con,"UPDATE character_blobs SET state_blob=$1,char_name=$2,updated_at=now() WHERE id::text=$3",params=list(list(serialize(char,NULL)),char$meta$name,cid));sync_character_inventory_relational(con,char,cid);TRUE}),error=function(e){message("foraged food grant failed: ",e$message);FALSE})
    if(!isTRUE(granted))return(NULL)
  }
  result
}

create_party_skill_check <- function(session_id,skill,ability,context,requester_id,requester_modifier,scope="party") {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE);scope<-match.arg(scope,c("solo","party"))
  tryCatch(DBI::dbGetQuery(con,"INSERT INTO party_skill_checks(session_id,skill,ability,context,requester_character_id,requester_modifier,scope) VALUES($1,$2,$3,$4,$5,$6,$7) RETURNING *",params=list(as.integer(session_id),as.character(skill),as.character(ability),trimws(as.character(context%||%"")),as.character(requester_id),as.integer(requester_modifier),scope))[1,,drop=FALSE],error=function(e){message("create_party_skill_check failed: ",e$message);NULL})
}

get_pending_party_skill_checks <- function(session_id,character_id) {
  con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbGetQuery(con,paste("SELECT c.*,COALESCE(sp.display_name,cb.char_name,c.requester_character_id) AS requester_name FROM party_skill_checks c","LEFT JOIN session_players sp ON sp.session_id=c.session_id AND sp.character_id::text=c.requester_character_id LEFT JOIN character_blobs cb ON cb.id::text=c.requester_character_id","WHERE c.session_id=$1 AND c.scope='party' AND c.status='pending' AND c.requester_character_id<>$2 ORDER BY c.created_at"),params=list(as.integer(session_id),as.character(character_id))),error=function(e)data.frame())
}

resolve_party_skill_check <- function(check_id,responder_id,helper_modifier=NULL) {
  con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbWithTransaction(con,{row<-DBI::dbGetQuery(con,"SELECT * FROM party_skill_checks WHERE id=$1 FOR UPDATE",params=list(as.integer(check_id)));if(!nrow(row)||row$status[[1L]]!="pending")stop("This check has already resolved.");solo<-row$scope[[1L]]=="solo";if(!solo&&as.character(row$requester_character_id[[1L]])==as.character(responder_id))stop("The requester cannot assist their own check.");lead_roll<-sample.int(20L,1L)+as.integer(row$requester_modifier[[1L]]);helper_roll<-if(solo)NA_integer_ else sample.int(20L,1L)+as.integer(helper_modifier%||%0L);total<-max(c(lead_roll,helper_roll),na.rm=TRUE);DBI::dbExecute(con,"UPDATE party_skill_checks SET helper_character_id=$2,helper_modifier=$3,requester_roll=$4,helper_roll=$5,final_total=$6,status='resolved',resolved_at=now() WHERE id=$1",params=list(row$id[[1L]],if(solo)NA_character_ else as.character(responder_id),if(solo)NA_integer_ else as.integer(helper_modifier),lead_roll,helper_roll,total));list(id=as.integer(row$id[[1L]]),skill=row$skill[[1L]],context=row$context[[1L]],requester_roll=lead_roll,helper_roll=helper_roll,final_total=total,assisted=!solo)}),error=function(e){message("resolve_party_skill_check failed: ",e$message);NULL})
}

get_recent_party_skill_results <- function(session_id) {
  con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbGetQuery(con,paste("SELECT c.*,COALESCE(rp.display_name,rc.char_name,c.requester_character_id) AS requester_name,COALESCE(hp.display_name,hc.char_name,c.helper_character_id) AS helper_name FROM party_skill_checks c","LEFT JOIN session_players rp ON rp.session_id=c.session_id AND rp.character_id::text=c.requester_character_id LEFT JOIN character_blobs rc ON rc.id::text=c.requester_character_id","LEFT JOIN session_players hp ON hp.session_id=c.session_id AND hp.character_id::text=c.helper_character_id LEFT JOIN character_blobs hc ON hc.id::text=c.helper_character_id","WHERE c.session_id=$1 AND c.status='resolved' ORDER BY c.resolved_at DESC LIMIT 20"),params=list(as.integer(session_id))),error=function(e)data.frame())
}

session_notification_dedupe_key <- function(session_id,message,created_at=Sys.time()) {
  bucket<-floor(as.numeric(as.POSIXct(created_at))/5);paste(as.integer(session_id),bucket,enc2utf8(as.character(message%||%"")),sep="|")
}

publish_session_notification <- function(session_id,message,source_character_id=NULL,source_name="Player",notification_type="message",dedupe_key=NULL) {
  sid<-suppressWarnings(as.integer(session_id));msg<-trimws(as.character(message%||%""));if(is.na(sid)||sid<1L||!nzchar(msg))return(FALSE)
  key<-as.character(dedupe_key%||%session_notification_dedupe_key(sid,msg));con<-get_db_connection();if(is.null(con))return(FALSE);on.exit(release_db_connection(con),add=TRUE)
  tryCatch({DBI::dbExecute(con,paste("INSERT INTO session_notifications(session_id,source_character_id,source_name,message,notification_type,dedupe_key)","VALUES($1,$2,$3,$4,$5,$6) ON CONFLICT(dedupe_key) DO NOTHING"),params=list(sid,as.character(source_character_id%||%NA_character_),as.character(source_name%||%"Player"),msg,as.character(notification_type%||%"message"),key));TRUE},error=function(e)FALSE)
}

get_session_notifications_after <- function(session_id,after_id=0L,limit=50L) {
  sid<-suppressWarnings(as.integer(session_id));after<-suppressWarnings(as.integer(after_id%||%0L));if(is.na(sid)||sid<1L)return(data.frame());if(is.na(after))after<-0L
  con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE)
  tryCatch(DBI::dbGetQuery(con,"SELECT * FROM session_notifications WHERE session_id=$1 AND id>$2 ORDER BY id LIMIT $3",params=list(sid,after,max(1L,as.integer(limit)))),error=function(e)data.frame())
}

get_active_encounter_conditions <- function(encounter_id, target_actor_id = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(release_db_connection(con), add = TRUE)
  target_actor_id <- as.character(target_actor_id %||% "")
  sql <- paste(
    "SELECT *, payload->>'condition' AS condition FROM encounter_effects",
    "WHERE encounter_id = $1 AND effect_type = 'condition' AND is_active = TRUE"
  )
  params <- list(as.integer(encounter_id))
  if (nzchar(target_actor_id)) {
    sql <- paste(sql, "AND target_actor_id = $2")
    params <- c(params, list(target_actor_id))
  }
  tryCatch(DBI::dbGetQuery(con, sql, params = params), error = function(e) {
    message("get_active_encounter_conditions failed: ", e$message)
    data.frame()
  })
}

end_actor_concentration <- function(encounter_id, source_actor_id) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(release_db_connection(con), add = TRUE)
  tryCatch({
    DBI::dbExecute(con, paste(
      "UPDATE encounter_summons SET is_active = FALSE, updated_at = NOW()",
      "WHERE encounter_id = $1 AND summon_uuid::text IN (",
      "SELECT target_actor_id FROM encounter_effects",
      "WHERE encounter_id = $1 AND source_actor_id = $2",
      "AND concentration = TRUE AND effect_type = 'summon' AND is_active = TRUE)"
    ), params = list(as.integer(encounter_id), as.character(source_actor_id)))
    DBI::dbExecute(con, paste(
      "UPDATE encounter_effects SET is_active = FALSE, updated_at = NOW()",
      "WHERE encounter_id = $1 AND source_actor_id = $2",
      "AND concentration = TRUE AND is_active = TRUE"
    ), params = list(as.integer(encounter_id), as.character(source_actor_id)))
    TRUE
  }, error = function(e) FALSE)
}

create_encounter_summon <- function(encounter_id, owner_actor_id, name,
                                    max_cr = "1/2", hp_max = 10L, ac = 12L,
                                    movement_speed = 30L, expires_round = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(release_db_connection(con), add = TRUE)
  expires_round <- suppressWarnings(as.integer(expires_round %||% NA_integer_))
  tryCatch(DBI::dbGetQuery(con, paste(
    "INSERT INTO encounter_summons (encounter_id, owner_actor_id, name, max_cr,",
    "hp_max, hp_current, ac, movement_speed, expires_round)",
    "VALUES ($1,$2,$3,$4,$5,$5,$6,$7,$8) RETURNING *"
  ), params = list(
    as.integer(encounter_id), as.character(owner_actor_id), as.character(name),
    as.character(max_cr), as.integer(hp_max), as.integer(ac),
    as.integer(movement_speed), expires_round
  )), error = function(e) {
    message("create_encounter_summon failed: ", e$message)
    NULL
  })
}

get_encounter_summons <- function(encounter_id) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(release_db_connection(con), add = TRUE)
  tryCatch(DBI::dbGetQuery(
    con,
    "SELECT * FROM encounter_summons WHERE encounter_id = $1 AND is_active = TRUE ORDER BY id",
    params = list(as.integer(encounter_id))
  ), error = function(e) data.frame())
}

get_session_overview <- function(session_id) {
  sess <- get_game_session(session_id)
  
  active_encounter_id <- NA_integer_
  if (is.data.frame(sess) && nrow(sess) > 0 && "active_encounter_id" %in% names(sess)) {
    active_encounter_id <- suppressWarnings(as.integer(sess$active_encounter_id[1] %||% NA))
  }
  
  combat <- data.frame()
  events <- data.frame()
  
  if (!is.na(active_encounter_id) && active_encounter_id > 0) {
    combat <- tryCatch(get_combat_state(active_encounter_id), error = function(e) data.frame())
    events <- tryCatch(get_encounter_events(active_encounter_id, limit = 20), error = function(e) data.frame())
  }
  
  list(
    session = sess,
    players = get_session_players(session_id),
    positions = get_player_positions(session_id),
    combat = combat,
    events = events
  )
}

get_encounter_events <- function(encounter_id, limit = 50) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  limit <- suppressWarnings(as.integer(limit))
  
  if (is.na(encounter_id) || encounter_id < 1) return(data.frame())
  if (is.na(limit) || limit < 1) limit <- 50L
  
  tryCatch(
    DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM game_events
      WHERE encounter_id = $1
      ORDER BY created_at DESC
      LIMIT $2
      ",
      params = list(encounter_id, limit)
    ),
    error = function(e) {
      message("get_encounter_events failed: ", e$message)
      data.frame()
    }
  )
}

remove_encounter_actor_position <- function(encounter_id, actor_type, actor_id) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      DELETE FROM encounter_positions
      WHERE encounter_id = $1
        AND actor_type = $2
        AND actor_id = $3
      ",
      params = list(
        as.integer(encounter_id),
        as.character(actor_type),
        as.character(actor_id)
      )
    )
    TRUE
  }, error = function(e) {
    message("remove_encounter_actor_position failed: ", e$message)
    FALSE
  })
}

damage_encounter_enemy <- function(encounter_id, enemy_uuid, amount) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  enemy_uuid <- as.character(enemy_uuid %||% "")
  amount <- suppressWarnings(as.integer(amount %||% 0))
  
  if (is.na(encounter_id) || encounter_id < 1) return(NULL)
  if (!nzchar(enemy_uuid)) return(NULL)
  if (is.na(amount) || amount < 0) amount <- 0L
  
  tryCatch({
    row <- DBI::dbGetQuery(
      con,
      "
      SELECT hp_current, temp_hp
      FROM encounter_enemies
      WHERE encounter_id = $1
        AND enemy_uuid = $2
      ",
      params = list(
        encounter_id,
        enemy_uuid
      )
    )
    
    if (!is.data.frame(row) || nrow(row) == 0) return(NULL)
    
    old_hp   <- suppressWarnings(as.integer(row$hp_current[1] %||% 0L))
    old_temp <- suppressWarnings(as.integer(row$temp_hp[1] %||% 0L))
    dmg      <- suppressWarnings(as.integer(amount %||% 0L))
    
    if (is.na(old_hp) || old_hp < 0) old_hp <- 0L
    if (is.na(old_temp) || old_temp < 0) old_temp <- 0L
    if (is.na(dmg) || dmg < 0) dmg <- 0L
    
    temp_absorb <- min(old_temp, dmg)
    new_temp <- old_temp - temp_absorb
    remaining <- dmg - temp_absorb
    new_hp <- max(0L, old_hp - remaining)
    
    DBI::dbExecute(
      con,
      "
      UPDATE encounter_enemies
      SET hp_current = $1,
          temp_hp = $2,
          updated_at = NOW()
      WHERE encounter_id = $3
        AND enemy_uuid = $4
      ",
      params = list(
        new_hp,
        new_temp,
        encounter_id,
        enemy_uuid
      )
    )
    
    list(
      hp_before = old_hp,
      hp_after = new_hp,
      temp_before = old_temp,
      temp_after = new_temp,
      amount = dmg
    )
  }, error = function(e) {
    message("damage_encounter_enemy failed: ", e$message)
    NULL
  })
}

set_encounter_enemy_hp <- function(encounter_id, enemy_uuid, current_hp, temp_hp = 0) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  enemy_uuid <- as.character(enemy_uuid %||% "")
  current_hp <- suppressWarnings(as.integer(current_hp))
  temp_hp <- suppressWarnings(as.integer(temp_hp))
  
  if (is.na(encounter_id) || encounter_id < 1) return(FALSE)
  if (!nzchar(enemy_uuid)) return(FALSE)
  if (is.na(current_hp) || current_hp < 0) current_hp <- 0L
  if (is.na(temp_hp) || temp_hp < 0) temp_hp <- 0L
  
  tryCatch({
    DBI::dbExecute(
      con,
      "
      UPDATE encounter_enemies
      SET hp_current = $1,
          temp_hp = $2,
          updated_at = NOW()
      WHERE encounter_id = $3
        AND enemy_uuid = $4
      ",
      params = list(
        current_hp,
        temp_hp,
        encounter_id,
        enemy_uuid
      )
    )
    TRUE
  }, error = function(e) {
    message("set_encounter_enemy_hp failed: ", e$message)
    FALSE
  })
}
 
roll_encounter_initiative <- function(encounter_id, core_state = NULL) {
  actors <- get_encounter_actors(encounter_id)
  if (!is.data.frame(actors) || nrow(actors) == 0) return(data.frame())
  
  if ("is_active" %in% names(actors)) {
    actors <- actors[actors$is_active %in% TRUE, , drop = FALSE]
  }
  if (nrow(actors) == 0) return(data.frame())
  
  out <- lapply(seq_len(nrow(actors)), function(i) {
    row <- actors[i, , drop = FALSE]
    
    actor_id   <- as.character(row$actor_id[1] %||% "")
    actor_type <- as.character(row$actor_type[1] %||% "player")
    nm         <- as.character(row$display_name[1] %||% "Unknown")
    
    init_bonus <- 0L
    
    if (identical(actor_type, "player")) {
      fallback_char <- NULL
      
      if (!is.null(core_state) &&
          identical(as.character(core_state$char_id %||% ""), actor_id)) {
        fallback_char <- core_state$char
      }
      
      init_bonus <- suppressWarnings(
        as.integer(get_character_initiative_bonus(
          actor_id,
          fallback_char = fallback_char
        ))
      )
      if (is.na(init_bonus)) init_bonus <- 0L
    }
    
    if (identical(actor_type, "enemy")) {
      init_bonus <- 0L
    }
    
    roll  <- sample(1:20, 1)
    total <- as.integer(roll + init_bonus)
    
    data.frame(
      actor_id = actor_id,
      actor_type = actor_type,
      display_name = nm,
      initiative_roll = as.integer(roll),
      initiative_bonus = as.integer(init_bonus),
      initiative_total = as.integer(total),
      stringsAsFactors = FALSE
    )
  })
  
  init_df <- do.call(rbind, out)
  if (!is.data.frame(init_df) || nrow(init_df) == 0) return(data.frame())
  
  init_df <- init_df[order(
    -init_df$initiative_total,
    -init_df$initiative_bonus,
    init_df$display_name,
    init_df$actor_id
  ), , drop = FALSE]
  
  init_df$turn_order <- seq_len(nrow(init_df))
  
  write_ok <- logical(nrow(init_df))
  
  for (i in seq_len(nrow(init_df))) {
    write_ok[i] <- isTRUE(
      set_actor_turn_order(
        encounter_id = encounter_id,
        actor_id = as.character(init_df$actor_id[i]),
        actor_type = as.character(init_df$actor_type[i]),
        initiative = as.integer(init_df$initiative_total[i]),
        turn_order = as.integer(init_df$turn_order[i])
      )
    )
  }
  
  init_df$write_ok <- write_ok
  init_df
}

start_encounter_combat <- function(encounter_id, core_state = NULL) {
  actors <- get_encounter_actors(encounter_id)
  if (!is.data.frame(actors) || nrow(actors) == 0) return(data.frame())
  
  if ("is_active" %in% names(actors)) {
    actors <- actors[actors$is_active %in% TRUE, , drop = FALSE]
  }
  if (nrow(actors) == 0) return(data.frame())
  
  # Starting combat is the initiative trigger. Always roll afresh so stale
  # turn orders from a previous combat cannot leak into a new round one.
  actors <- roll_encounter_initiative(
    encounter_id = encounter_id,
    core_state = core_state
  )
  if (!is.data.frame(actors) || nrow(actors) == 0) return(data.frame())
  
  first_actor <- actors[1, , drop = FALSE]
  
  ok <- set_combat_state(
    encounter_id = encounter_id,
    round_number = 1L,
    current_turn_order = as.integer(first_actor$turn_order[1] %||% 1L),
    active_actor_id = as.character(first_actor$actor_id[1] %||% ""),
    active_actor_type = as.character(first_actor$actor_type[1] %||% "player"),
    phase = "combat"
  )
  
  if (!isTRUE(ok)) return(data.frame())
  
  log_game_event(
    encounter_id = encounter_id,
    event_type = "enter_combat",
    actor_type = "system",
    payload = list(
      round_number = 1L,
      active_actor_id = as.character(first_actor$actor_id[1] %||% ""),
      active_actor_type = as.character(first_actor$actor_type[1] %||% "player")
    )
  )
  
  actors
}

set_actor_turn_order <- function(encounter_id, actor_id, actor_type = NULL, initiative, turn_order) {
  con <- get_db_connection()
  if (is.null(con)) return(FALSE)
  on.exit(safe_db_disconnect(con), add = TRUE)
  
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  actor_id <- as.character(actor_id %||% "")
  actor_type <- as.character(actor_type %||% "")
  initiative <- suppressWarnings(as.integer(initiative))
  turn_order <- suppressWarnings(as.integer(turn_order))
  
  if (is.na(encounter_id) || encounter_id < 1) return(FALSE)
  if (!nzchar(actor_id)) return(FALSE)
  if (!nzchar(actor_type)) return(FALSE)
  if (is.na(initiative)) initiative <- 0L
  if (is.na(turn_order) || turn_order < 1) turn_order <- 1L
  
  enc <- tryCatch(get_encounter(encounter_id), error = function(e) data.frame())
  if (!is.data.frame(enc) || nrow(enc) == 0) return(FALSE)
  
  session_id <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
  if (is.na(session_id) || session_id < 1) return(FALSE)
  
  tryCatch({
    if (identical(actor_type, "player")) {
      DBI::dbExecute(
        con,
        "
        UPDATE session_players
        SET initiative = $1,
            turn_order = $2,
            updated_at = NOW()
        WHERE session_id = $3
          AND character_id = $4
        ",
        params = list(initiative, turn_order, session_id, actor_id)
      )
    } else if (identical(actor_type, "enemy")) {
      DBI::dbExecute(
        con,
        "
        UPDATE encounter_enemies
        SET initiative = $1,
            turn_order = $2,
            updated_at = NOW()
        WHERE encounter_id = $3
          AND enemy_uuid = $4
        ",
        params = list(initiative, turn_order, encounter_id, actor_id)
      )
    } else if (identical(actor_type, "summon")) {
      DBI::dbExecute(
        con,
        "UPDATE encounter_summons SET initiative = $1, turn_order = $2, updated_at = NOW() WHERE encounter_id = $3 AND summon_uuid = $4",
        params = list(initiative, turn_order, encounter_id, actor_id)
      )
    } else {
      return(FALSE)
    }
    
    TRUE
  }, error = function(e) {
    message("set_actor_turn_order failed: ", e$message)
    FALSE
  })
}


get_encounter_actors <- function(encounter_id) {
  encounter_id <- suppressWarnings(as.integer(encounter_id))
  if (is.na(encounter_id) || encounter_id < 1) return(data.frame())
  
  enc <- tryCatch(
    get_encounter(encounter_id),
    error = function(e) data.frame()
  )
  if (!is.data.frame(enc) || nrow(enc) == 0) return(data.frame())
  
  session_id <- suppressWarnings(as.integer(enc$session_id[1] %||% NA))
  if (is.na(session_id) || session_id < 1) return(data.frame())
  
  players <- tryCatch(
    get_session_players(session_id),
    error = function(e) data.frame()
  )
  
  enemies <- tryCatch(
    get_encounter_enemies(encounter_id),
    error = function(e) data.frame()
  )

  summons <- tryCatch(
    get_encounter_summons(encounter_id),
    error = function(e) data.frame()
  )
  
  positions <- tryCatch(
    get_encounter_positions(encounter_id),
    error = function(e) data.frame()
  )

  # Return a vector with exactly one value per row. Using scalar fallbacks in a
  # data.frame constructor breaks whenever an actor category has zero rows.
  actor_column <- function(df, candidates, default = NA) {
    n <- nrow(df)
    if (!n) return(rep(default, 0L))
    for (candidate in candidates) {
      if (!candidate %in% names(df)) next
      value <- df[[candidate]]
      if (length(value) == n) return(value)
    }
    rep(default, n)
  }
  
  player_df <- data.frame(
    actor_id = as.character(actor_column(players, "character_id", "")),
    actor_type = rep("player", nrow(players)),
    display_name = as.character(actor_column(players, c("display_name", "char_name"), "Unknown")),
    
    current_hp = suppressWarnings(as.integer(actor_column(players, c("current_hp", "hp_current"), NA))),
    hp_current = suppressWarnings(as.integer(actor_column(players, c("current_hp", "hp_current"), NA))),
    hp_max = suppressWarnings(as.integer(actor_column(players, c("max_hp", "hp_max"), NA))),
    max_hp = suppressWarnings(as.integer(actor_column(players, c("max_hp", "hp_max"), NA))),
    
    temp_hp = suppressWarnings(as.integer(actor_column(players, "temp_hp", 0))),
    initiative = suppressWarnings(as.integer(actor_column(players, "initiative", NA))),
    turn_order = suppressWarnings(as.integer(actor_column(players, "turn_order", NA))),
    is_active = as.logical(actor_column(players, "is_active", TRUE)),
    stringsAsFactors = FALSE
  )
  enemy_df <- data.frame(
    actor_id = as.character(actor_column(enemies, "enemy_uuid", "")),
    actor_type = rep("enemy", nrow(enemies)),
    display_name = as.character(actor_column(enemies, "name", "Enemy")),
    
    current_hp = suppressWarnings(as.integer(actor_column(enemies, "hp_current", NA))),
    hp_current = suppressWarnings(as.integer(actor_column(enemies, "hp_current", NA))),
    hp_max = suppressWarnings(as.integer(actor_column(enemies, "hp_max", NA))),
    max_hp = suppressWarnings(as.integer(actor_column(enemies, "hp_max", NA))),
    
    temp_hp = suppressWarnings(as.integer(actor_column(enemies, "temp_hp", 0))),
    initiative = suppressWarnings(as.integer(actor_column(enemies, "initiative", NA))),
    turn_order = suppressWarnings(as.integer(actor_column(enemies, "turn_order", NA))),
    is_active = as.logical(actor_column(enemies, "is_active", TRUE)),
    ac = suppressWarnings(as.integer(actor_column(enemies, "ac", NA))),
    movement_speed = suppressWarnings(as.integer(actor_column(enemies, c("movement_speed", "speed_ft"), NA))),
    stringsAsFactors = FALSE
  )
  summon_df <- data.frame(
    actor_id = as.character(actor_column(summons, "summon_uuid", "")),
    actor_type = rep("summon", nrow(summons)),
    display_name = as.character(actor_column(summons, "name", "Summoned Beast")),
    current_hp = suppressWarnings(as.integer(actor_column(summons, "hp_current", NA))),
    hp_current = suppressWarnings(as.integer(actor_column(summons, "hp_current", NA))),
    hp_max = suppressWarnings(as.integer(actor_column(summons, "hp_max", NA))),
    max_hp = suppressWarnings(as.integer(actor_column(summons, "hp_max", NA))),
    temp_hp = suppressWarnings(as.integer(actor_column(summons, "temp_hp", 0))),
    initiative = suppressWarnings(as.integer(actor_column(summons, "initiative", NA))),
    turn_order = suppressWarnings(as.integer(actor_column(summons, "turn_order", NA))),
    is_active = as.logical(actor_column(summons, "is_active", TRUE)),
    ac = suppressWarnings(as.integer(actor_column(summons, "ac", NA))),
    movement_speed = suppressWarnings(as.integer(actor_column(summons, c("movement_speed", "speed_ft"), NA))),
    owner_actor_id = as.character(actor_column(summons, "owner_actor_id", "")),
    stringsAsFactors = FALSE
  )
  
  # --------------------------------------------------
  # Ensure both have identical columns
  # --------------------------------------------------
  
  all_cols <- Reduce(union, list(names(player_df), names(enemy_df), names(summon_df)))
  
  add_missing_cols <- function(df, cols) {
    missing <- setdiff(cols, names(df))
    n <- nrow(df)
    
    for (m in missing) {
      df[[m]] <- rep(NA, n)
    }
    
    df[, cols, drop = FALSE]
  }
  
  player_df <- add_missing_cols(player_df, all_cols)
  enemy_df  <- add_missing_cols(enemy_df, all_cols)
  summon_df <- add_missing_cols(summon_df, all_cols)
  
  actors <- rbind(player_df, enemy_df, summon_df)
  if (!is.data.frame(actors) || nrow(actors) == 0) return(data.frame())
  
  if (is.data.frame(positions) && nrow(positions) > 0) {
    positions$actor_id <- as.character(positions$actor_id %||% "")
    positions$actor_type <- as.character(positions$actor_type %||% "")
    
    actors$x <- NA_integer_
    actors$y <- NA_integer_
    
    for (i in seq_len(nrow(actors))) {
      hit <- positions[
        as.character(positions$actor_id) == as.character(actors$actor_id[i]) &
          as.character(positions$actor_type) == as.character(actors$actor_type[i]),
        ,
        drop = FALSE
      ]
      
      if (nrow(hit) > 0) {
        actors$x[i] <- suppressWarnings(as.integer(hit$x[1] %||% NA))
        actors$y[i] <- suppressWarnings(as.integer(hit$y[1] %||% NA))
      }
    }
  }
  
  actors
}
