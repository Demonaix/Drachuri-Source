library(DBI)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(), host = Sys.getenv("SUPABASE_HOST"),
  port = as.integer(Sys.getenv("SUPABASE_PORT")), dbname = Sys.getenv("SUPABASE_DBNAME"),
  user = Sys.getenv("SUPABASE_USER"), password = Sys.getenv("SUPABASE_DB_PASSWORD")
)
on.exit(dbDisconnect(con), add = TRUE)

map_name <- "3D Visual Test — Forest Crossing"
existing <- dbGetQuery(con, "select id from maps where name=$1 order by id limit 1", params = list(map_name))
if (nrow(existing)) {
  map_id <- as.integer(existing$id[[1L]])
  dbExecute(con, "update maps set width=14,height=12 where id=$1", params = list(map_id))
  dbExecute(con, "delete from map_tiles where map_id=$1", params = list(map_id))
} else {
  map_id <- as.integer(dbGetQuery(con, "select coalesce(max(id),0)+1 as id from maps")$id[[1L]])
  dbExecute(con, "insert into maps(id,name,width,height) values($1,$2,14,12)", params = list(map_id, map_name))
  dbExecute(con, "select setval(pg_get_serial_sequence('maps','id'),$1,true)", params = list(map_id))
}

tiles <- expand.grid(x = 1:14, y = 1:12)
tiles$terrain <- "grass"
tiles$terrain[tiles$y %in% c(6L,7L)] <- "road"
tiles$terrain[tiles$x <= 4L & !tiles$y %in% c(6L,7L)] <- "forest"
tiles$terrain[tiles$x >= 10L & tiles$y >= 9L] <- "forest"
tiles$terrain[tiles$x %in% c(7L,8L) & tiles$y >= 9L] <- "ravine"
tiles$terrain[tiles$x >= 12L & tiles$y <= 3L] <- "water"
tiles$terrain[tiles$x == 11L & tiles$y <= 4L] <- "swamp"
tiles$terrain[tiles$x >= 12L & tiles$y == 4L] <- "sand"
tiles$terrain[tiles$x %in% 9:11 & tiles$y %in% 2:5] <- "stone"
tiles$terrain[tiles$x == 11L & tiles$y %in% 2:5] <- "wall"
tiles$terrain[tiles$x == 9L & tiles$y %in% c(2L,5L)] <- "wall"

dbBegin(con)
tryCatch({
  for (i in seq_len(nrow(tiles))) dbExecute(con, "insert into map_tiles(map_id,x,y,terrain) values($1,$2,$3,$4)", params = list(map_id, tiles$x[[i]], tiles$y[[i]], tiles$terrain[[i]]))
  active <- dbGetQuery(con, "select active_encounter_id from game_sessions where active_encounter_id is not null order by id limit 1")
  if (nrow(active)) {
    encounter_id <- active$active_encounter_id[[1L]]
    dbExecute(con, "update encounters set map_id=$1 where id=$2", params = list(map_id, encounter_id))
    dbExecute(con, "update game_sessions set active_map_id=$1 where active_encounter_id=$2", params = list(map_id, encounter_id))
  }
  dbCommit(con)
}, error = function(e) { dbRollback(con); stop(e) })

cat("Visual test map ready: ", map_name, " (#", map_id, ")\n", sep = "")
