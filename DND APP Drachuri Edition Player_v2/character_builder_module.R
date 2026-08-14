library(shiny)

characterBuilderUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$h3("Character Builder"),
    
    selectInput(
      ns("base_model"),
      "Base character",
      choices = c(
        "Female" = "models/universal_base/Base Characters/Godot - UE/Superhero_Female_FullBody.gltf",
        "Male"   = "models/universal_base/Base Characters/Godot - UE/Superhero_Male_FullBody.gltf"
      )
    ),
    
    selectInput(
      ns("hair_model"),
      "Hair",
      choices = c(
        "None" = "",
        "Long" = "models/universal_base/Hairstyles/Rigged to Head Bone/glTF (Godot -Unreal)/Hair_Long.gltf",
        "Buns" = "models/universal_base/Hairstyles/Rigged to Head Bone/glTF (Godot -Unreal)/Hair_Buns.gltf",
        "Buzzed" = "models/universal_base/Hairstyles/Rigged to Head Bone/glTF (Godot -Unreal)/Hair_Buzzed.gltf",
        "Beard" = "models/universal_base/Hairstyles/Rigged to Head Bone/glTF (Godot -Unreal)/Hair_Beard.gltf"
      )
    ),
    
    actionButton(ns("load_character"), "Load character"),
    
    tags$hr(),
    
    tags$div(
      id = ns("character_preview"),
      style = "width: 100%; height: 500px; border: 1px solid #ccc;"
    )
  )
}


characterBuilderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$load_character, {
      session$sendCustomMessage(
        "loadCharacterPreview",
        list(
          containerId = session$ns("character_preview"),
          baseModel = input$base_model,
          hairModel = input$hair_model
        )
      )
    })
    
  })
}


ui <- fluidPage(
  tags$head(
    tags$script(type = "importmap", HTML('
    {
      "imports": {
        "three": "https://cdn.jsdelivr.net/npm/three@0.160.0/build/three.module.js",
        "three/addons/": "https://cdn.jsdelivr.net/npm/three@0.160.0/examples/jsm/"
      }
    }
  ')),
    
    tags$script(
      type = "module",
      src = "js/character-preview.js"
    )
  ),
  
  characterBuilderUI("builder")
)


server <- function(input, output, session) {
  characterBuilderServer("builder")
}

shinyApp(ui, server)