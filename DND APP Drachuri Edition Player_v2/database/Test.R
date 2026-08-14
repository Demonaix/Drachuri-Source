library(DBI)

get_session_overview <- function(session_id) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  
  session_row <- dbGetQuery(
    con,
    "
    SELECT *
    FROM game_sessions
    WHERE id = $1
    ",
    params = list(session_id)
  )
  
  players <- dbGetQuery(
    con,
    "
    SELECT sp.*, cb.char_name
    FROM session_players sp
    LEFT JOIN character_blobs cb
      ON cb.id = sp.character_id
    WHERE sp.session_id = $1
    ORDER BY sp.turn_order NULLS LAST, sp.id
    ",
    params = list(session_id)
  )
  
  positions <- dbGetQuery(
    con,
    "
    SELECT *
    FROM player_positions
    WHERE session_id = $1
    ORDER BY updated_at DESC
    ",
    params = list(session_id)
  )
  
  list(
    session = session_row,
    players = players,
    positions = positions
  )
}

move_player_in_session <- function(session_id, character_id, x, y) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbExecute(
    con,
    "
    UPDATE player_positions
    SET x = $1, y = $2, updated_at = NOW()
    WHERE session_id = $3
      AND character_id = $4
    ",
    params = list(x, y, session_id, character_id)
  )
}