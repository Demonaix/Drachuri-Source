library(shiny)

worldMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      "#", ns("wrap"), "{position:fixed;right:22px;bottom:378px;z-index:1040}",
      "#", ns("open"), "{width:58px;height:58px;border-radius:50%;font-size:24px;background:#40566b;color:#fff4cf;border:2px solid #d7b96d;box-shadow:0 5px 18px rgba(0,0,0,.4)}",
      "#", ns("overlay"), "{display:none;position:fixed;inset:0;z-index:2000010;background:rgba(10,8,5,.96);padding:18px;box-sizing:border-box}",
      "#", ns("frame"), "{width:100%;height:100%;position:relative;display:flex;align-items:center;justify-content:center;overflow:hidden;border:1px solid rgba(215,185,109,.7);border-radius:14px;box-shadow:inset 0 0 70px #000}",
      "#", ns("image"), "{display:block;max-width:100%;max-height:100%;width:auto;height:auto;object-fit:contain;filter:drop-shadow(0 10px 30px #000)}",
      "#", ns("title"), "{position:absolute;left:22px;bottom:18px;z-index:2;padding:8px 14px;border:1px solid rgba(215,185,109,.62);border-radius:999px;background:rgba(28,19,11,.82);color:#f6e8c1;font:700 17px Cinzel,Georgia,serif;letter-spacing:.05em}",
      "#", ns("close"), "{position:absolute;right:20px;top:18px;z-index:3;width:48px;height:48px;border-radius:50%;font-size:23px;background:rgba(78,43,29,.92);color:#fff4cf;border:2px solid #d7b96d;box-shadow:0 5px 18px #0008}",
      "@media(max-width:700px){#",ns("overlay"),"{padding:6px}#",ns("title"),"{left:12px;bottom:10px;font-size:13px}#",ns("close"),"{right:10px;top:10px;width:42px;height:42px}}"
    ))),
    div(
      id = ns("wrap"),
      actionButton(ns("open"), "🗺", title = "Map of Annwn", `aria-label` = "Open map of Annwn")
    ),
    div(
      id = ns("overlay"),
      div(
        id = ns("frame"),
        div(id = ns("title"), "Map of Annwn"),
        actionButton(ns("close"), "×", title = "Close map", `aria-label` = "Close map"),
        tags$img(id = ns("image"), src = "assets/textures/map_annwn_world.jpg", alt = "Map of Annwn")
      )
    )
  )
}

worldMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$open, {
      shinyjs::show(id = "overlay", anim = FALSE)
    }, ignoreInit = TRUE)
    observeEvent(input$close, {
      shinyjs::hide(id = "overlay", anim = FALSE)
    }, ignoreInit = TRUE)
  })
}
