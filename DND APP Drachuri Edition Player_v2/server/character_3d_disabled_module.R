character3DTabUI <- function(id) {
  shiny::tabPanel(
    "Combat Markers",
    value = "character_3d",
    shiny::div(
      class = "magic-card",
      shiny::h4("Combat Marker Workshop is currently unavailable"),
      shiny::p("This module will return after its performance work is complete.")
    )
  )
}

character3DTabServer <- function(id, state, restoring, add_log, char_rev = NULL) {
  shiny::moduleServer(id, function(input, output, session) {})
}
