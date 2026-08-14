source("env.R")
source("global.R")
register_db_pool_shutdown()
source("ui.R")
source("server.R")
source("server/combat_map_logic.R", local = FALSE)
source("session_db.R") 


shinyApp(ui = ui_control, server = server_control)
