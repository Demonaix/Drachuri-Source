# Both applications use the same session and encounter database API.
control_app_dir <- Sys.getenv("DRACHURI_APP_DIR", unset = "")
if (!nzchar(control_app_dir)) control_app_dir <- getwd()
control_app_dir <- normalizePath(control_app_dir, mustWork = TRUE)
player_app_dir <- normalizePath(
  file.path(dirname(control_app_dir), "DND APP Drachuri Edition Player_v2"),
  mustWork = TRUE
)
source(file.path(player_app_dir, "shared", "session_db_core.R"), local = FALSE)
