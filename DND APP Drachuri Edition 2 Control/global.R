# Both applications must execute exactly the same game and character rules.
# Keep the canonical file inside the distributable player app. Use the launcher's
# explicit app path so Shiny reloads do not depend on the current working folder.
control_app_dir <- Sys.getenv("DRACHURI_APP_DIR", unset = "")
if (!nzchar(control_app_dir)) control_app_dir <- getwd()
control_app_dir <- normalizePath(control_app_dir, mustWork = TRUE)
if (!identical(normalizePath(getwd(), mustWork = TRUE), control_app_dir)) {
  setwd(control_app_dir)
}
player_app_dir <- normalizePath(
  file.path(dirname(control_app_dir), "DND APP Drachuri Edition Player_v2"),
  mustWork = TRUE
)
source(
  file.path(player_app_dir, "shared", "global_core.R"),
  local = FALSE
)
source(
  file.path(player_app_dir, "shared", "enemy_generator_core.R"),
  local = FALSE
)
source(
  file.path(player_app_dir, "shared", "relational_inventory_core.R"),
  local = FALSE
)

# global.R is loaded for both Shiny application layouts. Keep the session and
# encounter helpers available even when runApp() bypasses app.R in favour of
# the legacy ui.R/server.R entry points.
source(
  file.path(player_app_dir, "shared", "session_db_core.R"),
  local = FALSE
)
source(
  file.path(player_app_dir, "shared", "story_core.R"),
  local = FALSE
)

# Encounter setup and live combat both depend on the canonical map helpers.
# app.R also sources this file, but runApp() can bypass app.R for this project.
source("server/combat_map_logic.R", local = FALSE)
