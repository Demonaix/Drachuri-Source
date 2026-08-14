runeCraftingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "card",
        div(class = "title", "Rune Crafting"),
        div(class = "desc", "Create minor, major, arcane, or cursed runes."),
        
        numericInput(ns("level"), "Character Level", value = 1, min = 1),
        numericInput(ns("arcana"), "Arcana Skill", value = 0, min = 0),
        
        uiOutput(ns("rune_type_ui")),
        uiOutput(ns("material_ui")),
        
        hr(),
        uiOutput(ns("rune_summary"))
    )
  )
}

runeCraftingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    available_types <- reactive({
      get_available_rune_types(input$level %||% 1)
    })
    
    output$rune_type_ui <- renderUI({
      selectInput(
        ns("rune_type"),
        "Rune Type",
        choices = available_types()
      )
    })
    
    output$material_ui <- renderUI({
      req(input$rune_type)
      
      selectInput(
        ns("material"),
        "Blank Material",
        choices = get_available_rune_materials(input$rune_type)
      )
    })
    
    rune_rule <- reactive({
      req(input$rune_type, input$material)
      get_rune_rule(
        rune_type = input$rune_type,
        material = input$material,
        arcana_skill = input$arcana %||% 0
      )
    })
    
    output$rune_summary <- renderUI({
      rule <- rune_rule()
      req(rule)
      
      tagList(
        h4(paste(rule$rune_type, rule$material, "Rune")),
        tags$ul(
          tags$li(strong("Required blank: "), rule$required_blank),
          tags$li(strong("Required tools: "), rule$required_tools),
          tags$li(strong("Crafting time: "), rule$crafting_time),
          tags$li(strong("Cost: "), rule$cost),
          tags$li(strong("Active time: "), paste0(rule$active_time_sec, " seconds / ", rule$active_time_rounds, " round(s)")),
          tags$li(strong("Arcane score: "), rule$arcane_score),
          tags$li(strong("Instability: "), paste(rule$instability_damage, "damage in", rule$instability_radius_ft, "ft radius")),
          tags$li(strong("Usage: "), rule$usage),
          tags$li(strong("Dodge counter: "), rule$counter_dodge),
          tags$li(strong("Disrupt counter: "), rule$counter_disrupt),
          tags$li(strong("Near-success disrupt: "), rule$disrupt_near_success)
        )
      )
    })
  })
}