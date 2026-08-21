# Both applications must execute exactly the same game and character rules.
# Keep the canonical file inside the distributable player app.
source(
  "../DND APP Drachuri Edition Player_v2/shared/global_core.R",
  local = FALSE
)
source(
  "../DND APP Drachuri Edition Player_v2/shared/enemy_generator_core.R",
  local = FALSE
)
source(
  "../DND APP Drachuri Edition Player_v2/shared/relational_inventory_core.R",
  local = FALSE
)

# global.R is loaded for both Shiny application layouts. Keep the session and
# encounter helpers available even when runApp() bypasses app.R in favour of
# the legacy ui.R/server.R entry points.
source(
  "../DND APP Drachuri Edition Player_v2/shared/session_db_core.R",
  local = FALSE
)

# Encounter setup and live combat both depend on the canonical map helpers.
# app.R also sources this file, but runApp() can bypass app.R for this project.
source("server/combat_map_logic.R", local = FALSE)
