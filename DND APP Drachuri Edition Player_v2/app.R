source("env.R")

message(Sys.getenv("SUPABASE_HOST"))
message(Sys.getenv("SUPABASE_USER"))
message(Sys.getenv("SUPABASE_PORT"))

source("global.R")
register_db_pool_shutdown()

test_con <- get_db_connection()
if (is.null(test_con)) {
  message("❌ Database connection failed at startup")
} else {
  message("✅ Database connection succeeded at startup")
  # get_db_connection() normally returns a checked-out pooled connection.
  # Disconnecting it directly corrupts the pool's replacement connection
  # metadata (notably the port); always return it through the shared helper.
  release_db_connection(test_con)
}

source("ui.R")
source("server.R")


shinyApp(ui = ui_player, server = server_player)
