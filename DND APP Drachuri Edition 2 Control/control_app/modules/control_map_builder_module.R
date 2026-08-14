library(shiny)

controlMapBuilderUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    tags$link(rel = "stylesheet", type = "text/css", href = "css/combat.css"),
   # tags$script(type = "module", src = paste0("js/combat3d.js?v=", as.integer(Sys.time()))),
   tags$script(src = paste0("js/mapBuilder2d.js?v=", as.integer(Sys.time()))),
    
    tags$style(HTML("
  .map-builder-wrap{
    display:flex;
    flex-direction:column;
    gap:12px;
  }

  .map-builder-toolbar{
    display:flex;
    flex-wrap:wrap;
    gap:10px;
    align-items:flex-end;
  }

  .map-builder-grid-wrap{
    overflow:auto;
    max-width:100%;
    width:100%;
    max-height:70vh;
    padding:8px;
    border:1px solid rgba(191,167,111,0.45);
    border-radius:12px;
    background:rgba(255,255,245,0.8);
    box-sizing:border-box;
  }

  .map-builder-grid{
    display:grid;
    gap:0;
    width:max-content;
  }

  .map-builder-cell{
    position:relative;
    width:24px;
    height:24px;
    min-width:24px;
    min-height:24px;
    border:1px solid rgba(80,60,30,0.22);
    cursor:pointer;
    box-sizing:border-box;
    display:block;
  }

  .map-builder-cell:hover{
    outline:2px solid rgba(220,140,60,0.55);
    outline-offset:-2px;
    z-index:2;
  }

  .map-builder-cell.selected{
    outline:2px solid rgba(220,140,60,0.95);
    outline-offset:-2px;
    z-index:3;
  }

  .map-builder-token{
    position:absolute;
    left:4px;
    top:4px;
    width:14px;
    height:14px;
    border-radius:999px;
    display:flex;
    align-items:center;
    justify-content:center;
    font-size:9px;
    font-weight:900;
    color:white;
    z-index:3;
  }

  .map-builder-legend{
    display:flex;
    flex-wrap:wrap;
    gap:8px;
    margin-top:8px;
  }

  .map-builder-legend-pill{
    padding:5px 9px;
    border-radius:999px;
    border:1px solid rgba(191,167,111,0.55);
    background:rgba(255,255,245,0.92);
    font-size:11px;
    font-weight:800;
  }

  .map-builder-side{
    display:grid;
    grid-template-columns: 1.2fr 1fr;
    gap:12px;
  }

  @media (max-width: 1000px){
    .map-builder-side{
      grid-template-columns:1fr;
    }
  }
  
  .map-builder-cell {

  position: relative;

}

.map-builder-cell::after {

  content: '';

  position: absolute;

  inset: 0;

  background: var(--map-builder-overlay, transparent);

  pointer-events: none;

  z-index: 1;

}
")),
    
    div(
      class = "control-card",
      div(class = "control-section-title", "🗺️ Map Builder"),
      
      div(
        class = "map-builder-wrap",
        
        div(
          class = "control-card",
          div(class = "control-section-title", "Map Setup"),
          
          div(
            class = "map-builder-toolbar",
            selectInput(ns("map_select"), "Existing Map", choices = c(), width = "220px"),
            actionButton(ns("load_selected_map"), "Load Map", class = "btn btn-primary"),
            actionButton(ns("refresh_maps"), "Refresh Maps", class = "btn btn-default"),
            textInput(ns("new_map_name"), "New Map Name", value = "", width = "220px"),
            numericInput(ns("map_width"), "Width", value = 10, min = 1, max = 50, width = "110px"),
            numericInput(ns("map_height"), "Height", value = 10, min = 1, max = 50, width = "110px"),
            actionButton(ns("new_map"), "Create New Map", class = "btn btn-primary"),
            actionButton(ns("load_from_encounter"), "Use Encounter Map ID", class = "btn btn-default"),
            actionButton(ns("clear_map"), "Clear Paint", class = "btn btn-warning"),
            actionButton(ns("refresh_3d_preview"), "Refresh 3D Preview", class = "btn btn-default")
          )
        ),
        
        div(
          class = "map-builder-side",
          
          div(
            class = "control-card",
            div(class = "control-section-title", "Paint Tools"),
            
            div(
              class = "map-builder-toolbar",
              selectInput(
                ns("paint_terrain"),
                "Terrain",
                choices = c("grass", "stone", "forest", "swamp", "water", "wall", "road"),
                selected = "grass",
                width = "150px"
              ),
              selectInput(
                ns("paint_light"),
                "Light",
                choices = c("full", "dim", "dark"),
                selected = "full",
                width = "130px"
              ),
              selectInput(
                ns("paint_fog"),
                "Fog",
                choices = c("Visible" = 0, "Hidden" = 1),
                selected = 0,
                width = "120px"
              ),
              numericInput(ns("paint_move_cost"), "Move Cost", value = 1, min = 1, step = 1, width = "120px"),
              checkboxInput(ns("paint_blocks_movement"), "Blocks Movement", value = FALSE),
              checkboxInput(ns("paint_blocks_vision"), "Blocks Vision", value = FALSE)
            ),
            
            div(
              class = "map-builder-toolbar",
              actionButton(ns("paint_mode_apply"), "Paint Selected Tile", class = "btn btn-success"),
              actionButton(ns("paint_mode_fill"), "Fill Whole Map", class = "btn btn-default"),
              actionButton(ns("save_map"), "Save Map", class = "btn btn-success")
            ),
            
            tags$hr(),
            
            div(class = "control-section-title", "Selected Tile"),
            uiOutput(ns("selected_tile_ui"))
          ),
          
          div(
            class = "control-card",
            div(class = "control-section-title", "Legend"),
            div(
              class = "map-builder-legend",
              div(class = "map-builder-legend-pill", "Blue token = player"),
              div(class = "map-builder-legend-pill", "Red token = enemy"),
              div(class = "map-builder-legend-pill", "Dark overlay = low light / fog"),
              div(class = "map-builder-legend-pill", "Inner border = blocks movement")
            ),
            tags$hr(),
            div(class = "control-section-title", "Map Preview"),
            radioButtons(
              ns("preview_mode"),
              "Preview Mode",
              choices = c("2D" = "2d", "3D" = "3d"),
              selected = "2d",
              inline = TRUE
            ),
            uiOutput(ns("map_preview_ui"))
          )
        )
      )
    )
  )
}

controlMapBuilderServer <- function(id, ctrl, session_tbl = NULL, players_tbl = NULL, positions_tbl = NULL, bump_refresh) {
  moduleServer(id, function(input, output, session) {
    
    `%||%` <- get("%||%", inherits = TRUE)
    ns <- session$ns
    
    timer <- function(label, expr) {
      t0 <- Sys.time()
      on.exit({
        dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3)
        cat("[TIMER]", label, dt, "sec\n")
      }, add = TRUE)
      force(expr)
    }
    
    list_existing_maps <- function() {
      con <- get_db_connection()
      if (is.null(con)) return(data.frame())
      on.exit(release_db_connection(con), add = TRUE)
      
      tryCatch(
        DBI::dbGetQuery(
          con,
          "
      select
        m.id as map_id,
        m.name as map_name,
        m.width,
        m.height,
        count(t.*) as tile_count
      from maps m
      left join map_tiles t
        on t.map_id = m.id
      group by m.id, m.name, m.width, m.height
      order by lower(m.name), m.id
      "
        ),
        error = function(e) {
          message('list_existing_maps failed: ', e$message)
          data.frame()
        }
      )
    }
    
    create_map_record <- function(name, width, height) {
      con <- get_db_connection()
      if (is.null(con)) return(NA_integer_)
      on.exit(release_db_connection(con), add = TRUE)
      
      name <- trimws(as.character(name %||% ""))
      if (!nzchar(name)) name <- "Untitled Map"
      
      tryCatch({
        res <- DBI::dbGetQuery(
          con,
          "
      insert into maps (name, width, height, created_at, updated_at)
      values ($1, $2, $3, current_timestamp, current_timestamp)
      returning id
      ",
          params = list(name, as.integer(width), as.integer(height))
        )
        
        as.integer(res$id[1])
      }, error = function(e) {
        message("create_map_record failed: ", e$message)
        NA_integer_
      })
    }
    
    isolate({
      if (is.null(ctrl$map_tiles_store)) {
        ctrl$map_tiles_store <- list()
      }
    })
    
    selected_x <- reactiveVal(1L)
    selected_y <- reactiveVal(1L)
    tiles_cache <- reactiveVal(data.frame())
    
    maps_rv <- reactiveVal(data.frame())
    
    map_render_key <- reactiveVal(0L)
    
    bump_map_render <- function() {
      map_render_key(isolate(map_render_key()) + 1L)
    }
    

  
    
    load_maps <- function() {
      maps <- list_existing_maps()
      if (!is.data.frame(maps)) maps <- data.frame()
      maps_rv(maps)
    }
    
    current_map_id <- reactive({
      mid <- suppressWarnings(as.integer(input$map_select %||% ctrl$map_id %||% NA))
      if (is.na(mid) || mid < 1) return(NA_integer_)
      mid
    })
    
    current_encounter_id <- reactive({
      eid <- suppressWarnings(as.integer(ctrl$encounter_id %||% NA))
      if (is.na(eid) || eid < 1) return(NA_integer_)
      eid
    })
    
    observeEvent(TRUE, {
  load_maps()
}, once = TRUE)

observeEvent(input$refresh_maps, {
  load_maps()
}, ignoreInit = TRUE)

observe({
  maps <- maps_rv()
  
  if (!is.data.frame(maps) || nrow(maps) == 0) {
    updateSelectInput(session, "map_select", choices = c("No maps yet" = ""))
    return()
  }
  

  
  ids <- as.character(maps$map_id)
  labels <- paste0(
    maps$map_name,
    " • ",
    maps$width,
    "x",
    maps$height,
    " • ID ",
    maps$map_id,
    " • ",
    maps$tile_count,
    " tiles"
  )
  selected <- as.character(ctrl$map_id %||% input$map_select %||% "")
  if (!nzchar(selected) || !selected %in% ids) {
    selected <- ids[1]
  }
  
  updateSelectInput(
    session,
    "map_select",
    choices = stats::setNames(ids, labels),
    selected = selected
  )
})


    
load_map_tiles <- function(mid) {
  mid <- suppressWarnings(as.integer(mid))
  if (is.na(mid) || mid < 1) {
    tiles_cache(data.frame())
    return(FALSE)
  }
  
  showNotification(paste0("Loading map #", mid, "…"), type = "message", duration = 2)
  
  tiles <- tryCatch(
    get_map_tiles(mid),
    error = function(e) {
      message("get_map_tiles failed: ", e$message)
      data.frame()
    }
  )
  
  if (!is.data.frame(tiles)) tiles <- data.frame()
  
  ctrl$map_id <- mid
  tiles_cache(tiles)
  bump_map_render()
  
  
  if (nrow(tiles) > 0) {
    b <- get_map_bounds(tiles)
    updateNumericInput(session, "map_width", value = b$width)
    updateNumericInput(session, "map_height", value = b$height)
  }
  
  showNotification(paste0("Loaded map #", mid), type = "message", duration = 4)
  TRUE
}

observeEvent(input$load_selected_map, {
  mid <- suppressWarnings(as.integer(input$map_select %||% NA))
  if (is.na(mid) || mid < 1) {
    showNotification("Choose a map first.", type = "error")
    return()
  }
  
  load_map_tiles(mid)
}, ignoreInit = TRUE)
    
    current_tiles <- reactive({
      tiles_cache()
    })
    
    observe({
      tiles <- current_tiles()
      if (!is.data.frame(tiles) || nrow(tiles) == 0) return()
      
      b <- get_map_bounds(tiles)
      
      updateNumericInput(session, "map_width", value = b$width)
      updateNumericInput(session, "map_height", value = b$height)
    })
    
    current_positions <- reactive({
      eid <- current_encounter_id()
      if (is.na(eid)) return(data.frame())
      
      if (!is.null(positions_tbl) && is.reactive(positions_tbl)) {
        df <- tryCatch(positions_tbl(), error = function(e) data.frame())
      } else {
        df <- tryCatch(get_encounter_positions(eid), error = function(e) data.frame())
      }
      
      if (!is.data.frame(df)) data.frame() else df
    })
    
    observeEvent(input$load_from_encounter, {
      eid <- current_encounter_id()
      if (is.na(eid)) {
        showNotification("No active encounter selected.", type = "error")
        return()
      }
      
      enc <- tryCatch(get_encounter(eid), error = function(e) data.frame())
      if (!is.data.frame(enc) || nrow(enc) == 0) {
        showNotification("Could not load encounter.", type = "error")
        return()
      }
      
      mid <- suppressWarnings(as.integer(enc$map_id[1] %||% 1L))
      if (is.na(mid) || mid < 1) mid <- 1L
      
      ctrl$map_id <- mid
      load_maps()
      updateSelectInput(session, "map_select", selected = as.character(mid))
      load_map_tiles(mid)
      
      showNotification(paste0("Using map ID ", mid, "."), type = "message")
    }, ignoreInit = TRUE)
    
    observeEvent(input$new_map, {
      w <- suppressWarnings(as.integer(input$map_width %||% 10L))
      h <- suppressWarnings(as.integer(input$map_height %||% 10L))
      
      if (is.na(w) || w < 1) w <- 10L
      if (is.na(h) || h < 1) h <- 10L
      
      map_name <- trimws(as.character(input$new_map_name %||% ""))
      if (!nzchar(map_name)) {
        map_name <- paste0("Map ", format(Sys.time(), "%Y-%m-%d %H:%M"))
      }
      
      mid <- create_map_record(map_name, w, h)
      
      if (is.na(mid) || mid < 1) {
        showNotification("Failed to create map record.", type = "error")
        return()
      }
      
      tiles <- create_square_map_tiles(
        map_id = mid,
        width = w,
        height = h,
        default_terrain = "grass",
        default_fog = 0L,
        default_light = "full",
        default_move_cost = 1,
        default_blocks_movement = FALSE,
        default_blocks_vision = FALSE
      )
      
      ok <- set_shared_map_tiles(ctrl, mid, tiles)
      
      if (!isTRUE(ok)) {
        showNotification("Map record created, but tiles failed to save.", type = "error")
        return()
      }
      
      selected_x(1L)
      selected_y(1L)
      ctrl$map_id <- mid
      tiles_cache(tiles)
      bump_map_render()
      
      load_maps()
      updateSelectInput(session, "map_select", selected = as.character(mid))
      
      if (is.function(bump_refresh)) bump_refresh()
      
      showNotification(
        paste0("Created map: ", map_name, " (#", mid, ")"),
        type = "message"
      )
    }, ignoreInit = TRUE)

    
    observeEvent(input$clear_map, {
      tiles <- tiles_cache()
      
      if (!is.data.frame(tiles) || nrow(tiles) == 0) {
        showNotification("No map to clear.", type = "error")
        return()
      }
      
      tiles$terrain <- "grass"
      tiles$fog <- 0L
      tiles$light <- "full"
      tiles$move_cost <- 1
      tiles$blocks_movement <- FALSE
      tiles$blocks_vision <- FALSE
      
      # instant local update
      tiles_cache(tiles)
      bump_map_render()
      
      showNotification("Applied brush locally. Click Save Map to persist.", type = "message")
      
    })
    
    observeEvent(
      list(
        input$paint_terrain,
        input$paint_light,
        input$paint_fog,
        input$paint_move_cost,
        input$paint_blocks_movement,
        input$paint_blocks_vision
      ),
      {
        session$sendCustomMessage(
          "mapbuilder2d-brush",
          list(
            terrain = as.character(input$paint_terrain %||% "grass"),
            light = as.character(input$paint_light %||% "full"),
            fog = suppressWarnings(as.integer(input$paint_fog %||% 0L)),
            blocks_movement = isTRUE(input$paint_blocks_movement),
            blocks_vision = isTRUE(input$paint_blocks_vision),
            move_cost = suppressWarnings(as.numeric(input$paint_move_cost %||% 1))
          )
        )
      },
      ignoreInit = FALSE
    )

    apply_brush_to_tile <- function(x, y) {
      tiles <- tiles_cache()
      if (!is.data.frame(tiles) || nrow(tiles) == 0) return(FALSE)
      
      updated <- set_tile_values(
        tiles = tiles,
        x = x,
        y = y,
        map_id = current_map_id(),
        updates = list(
          terrain = as.character(input$paint_terrain %||% "grass"),
          fog = suppressWarnings(as.integer(input$paint_fog %||% 0L)),
          light = as.character(input$paint_light %||% "full"),
          move_cost = suppressWarnings(as.numeric(input$paint_move_cost %||% 1)),
          blocks_movement = isTRUE(input$paint_blocks_movement),
          blocks_vision = isTRUE(input$paint_blocks_vision)
        )
      )
      
      tiles_cache(updated)
      
      session$sendCustomMessage(
        "mapbuilder2d-paint-tile",
        list(
          containerId = session$ns("map_builder_canvas"),
          x = as.integer(x),
          y = as.integer(y),
          terrain = as.character(input$paint_terrain %||% "grass"),
          light = as.character(input$paint_light %||% "full"),
          fog = suppressWarnings(as.integer(input$paint_fog %||% 0L)),
          blocks_movement = isTRUE(input$paint_blocks_movement)
        )
      )
      
      TRUE
    }
    
    observeEvent(input$save_map, {
      tiles <- tiles_cache()
      mid <- current_map_id()
      
      if (!is.data.frame(tiles) || nrow(tiles) == 0) {
        showNotification("No map to save.", type = "error")
        return()
      }
      
      ok <- set_shared_map_tiles(ctrl, mid, tiles)
      
      if (!isTRUE(ok)) {
        showNotification("Failed to save map.", type = "error")
        return()
      }
      
      ctrl$map_id <- mid
      load_maps()
      updateSelectInput(session, "map_select", selected = as.character(mid))
      if (is.function(bump_refresh)) bump_refresh()
      
      showNotification(paste0("Saved map ", mid, "."), type = "message")
    }, ignoreInit = TRUE)
    

    
    observeEvent(input$paint_mode_apply, {
      ok <- apply_brush_to_tile(selected_x(), selected_y())
      if (!isTRUE(ok)) {
        showNotification("Failed to paint tile.", type = "error")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$paint_mode_fill, {
      tiles <- tiles_cache()
      
      if (!is.data.frame(tiles) || nrow(tiles) == 0) {
        showNotification("No map loaded.", type = "error")
        return()
      }
      
      new_terrain <- as.character(input$paint_terrain %||% "grass")
      new_fog <- suppressWarnings(as.integer(input$paint_fog %||% 0L))
      new_light <- as.character(input$paint_light %||% "full")
      new_move_cost <- suppressWarnings(as.numeric(input$paint_move_cost %||% 1))
      new_blocks_movement <- isTRUE(input$paint_blocks_movement)
      new_blocks_vision <- isTRUE(input$paint_blocks_vision)
      
      if (is.na(new_fog)) new_fog <- 0L
      if (is.na(new_move_cost) || new_move_cost < 1) new_move_cost <- 1
      
      tiles$terrain <- new_terrain
      tiles$fog <- new_fog
      tiles$light <- new_light
      tiles$move_cost <- new_move_cost
      tiles$blocks_movement <- new_blocks_movement
      tiles$blocks_vision <- new_blocks_vision
      
      # instant local update
      tiles_cache(tiles)
      bump_map_render()
      
      showNotification("Applied brush locally. Click Save Map to persist.", type = "message")
    }, ignoreInit = TRUE)
    
    output$selected_tile_ui <- renderUI({
      tiles <- current_tiles()
      row <- get_tile_row(
        tiles = tiles,
        x = selected_x(),
        y = selected_y(),
        map_id = current_map_id()
      )
      
      if (!is.data.frame(row) || nrow(row) == 0) {
        return(tags$em("No tile selected yet."))
      }
      
      div(
        class = "battlefield-kv",
        div(class = "battlefield-k", "Coords"), div(paste0("(", row$x[1], ", ", row$y[1], ")")),
        div(class = "battlefield-k", "Terrain"), div(as.character(row$terrain[1] %||% "grass")),
        div(class = "battlefield-k", "Light"), div(as.character(row$light[1] %||% "full")),
        div(class = "battlefield-k", "Fog"), div(ifelse(as.integer(row$fog[1] %||% 0L) == 1L, "Hidden", "Visible")),
        div(class = "battlefield-k", "Move Cost"), div(as.character(row$move_cost[1] %||% 1)),
        div(class = "battlefield-k", "Blocks Move"), div(ifelse(isTRUE(row$blocks_movement[1]), "Yes", "No")),
        div(class = "battlefield-k", "Blocks Vision"), div(ifelse(isTRUE(row$blocks_vision[1]), "Yes", "No"))
      )
    })

    
    observeEvent(input$map_builder_tile_click, {
      info <- input$map_builder_tile_click
      
      tx <- suppressWarnings(as.integer(info$x))
      ty <- suppressWarnings(as.integer(info$y))
      if (is.na(tx) || is.na(ty)) return()
      
      selected_x(tx)
      selected_y(ty)
      
      session$sendCustomMessage(
        "mapbuilder2d-select",
        list(
          containerId = session$ns("map_builder_canvas"),
          x = tx,
          y = ty
        )
      )
    }, ignoreInit = TRUE)
    
    
    observeEvent(input$map_builder_tile_paint, {
      info <- input$map_builder_tile_paint
      painted <- info$tiles
      
      if (is.null(painted) || length(painted) == 0) return()
      
      tiles <- tiles_cache()
      if (!is.data.frame(tiles) || nrow(tiles) == 0) return()
      
      for (p in painted) {
        tx <- suppressWarnings(as.integer(p$x))
        ty <- suppressWarnings(as.integer(p$y))
        if (is.na(tx) || is.na(ty)) next
        
        tiles <- set_tile_values(
          tiles = tiles,
          x = tx,
          y = ty,
          map_id = current_map_id(),
          updates = list(
            terrain = as.character(input$paint_terrain %||% "grass"),
            fog = suppressWarnings(as.integer(input$paint_fog %||% 0L)),
            light = as.character(input$paint_light %||% "full"),
            move_cost = suppressWarnings(as.numeric(input$paint_move_cost %||% 1)),
            blocks_movement = isTRUE(input$paint_blocks_movement),
            blocks_vision = isTRUE(input$paint_blocks_vision)
          )
        )
      }
      
      tiles_cache(tiles)
    }, ignoreInit = TRUE)
    
  
    
    observe({
      req(input$preview_mode == "2d")
      map_render_key()
      
      timer("2d render observer", {
        tiles <- isolate(current_tiles())
        if (!is.data.frame(tiles) || nrow(tiles) == 0) return()
        
        session$sendCustomMessage(
          "mapbuilder2d-render",
          list(
            containerId = session$ns("map_builder_canvas"),
            tiles = jsonlite::toJSON(
              tiles,
              dataframe = "rows",
              auto_unbox = TRUE,
              null = "null"
            ),
            selected = list(
              x = isolate(selected_x()),
              y = isolate(selected_y())
            ),
            inputIds = list(
              tileClick = session$ns("map_builder_tile_click"),
              tilePaint = session$ns("map_builder_tile_paint")
            ),
            brush = isolate(list(
              terrain = as.character(input$paint_terrain %||% "grass"),
              light = as.character(input$paint_light %||% "full"),
              fog = suppressWarnings(as.integer(input$paint_fog %||% 0L)),
              blocks_movement = isTRUE(input$paint_blocks_movement)
            ))
          )
        )
      })
    })
    
    output$map_preview_ui <- renderUI({
      mode <- input$preview_mode %||% "2d"
      
      if (identical(mode, "3d")) {
        session$onFlushed(function() {
          tiles <- isolate(current_tiles())
          if (!is.data.frame(tiles) || nrow(tiles) == 0) return()
          
          render_df <- build_map_render_df(
            tiles = tiles,
            occupants = empty_map_occupants(),
            map_id = isolate(current_map_id())
          )
          
          session$sendCustomMessage(
            "combat3d-init",
            list(
              containerId = session$ns("combat_3d_container"),
              mapData = jsonlite::toJSON(
                render_df,
                dataframe = "rows",
                auto_unbox = TRUE,
                null = "null"
              ),
              inputIds = list()
            )
          )
        }, once = TRUE)
        
        tagList(
          div(
            id = session$ns("combat_3d_shell"),
            class = "combat-3d-shell",
            style = "height:620px; min-height:620px;",
            div(
              id = session$ns("combat_3d_container"),
              class = "combat-3d-container",
              style = "height:100%; width:100%;"
            )
          )
        )
      } else {
        session$onFlushed(function() {
          bump_map_render()
        }, once = TRUE)
        
        div(
          id = session$ns("map_builder_canvas"),
          class = "map-builder-grid-wrap",
          style = "min-height:500px;"
        )
      }
    })
    
    observeEvent(input$refresh_3d_preview, {
      req(input$preview_mode == "3d")
      
      tiles <- isolate(current_tiles())
      if (!is.data.frame(tiles) || nrow(tiles) == 0) return()
      
      render_df <- build_map_render_df(
        tiles = tiles,
        occupants = empty_map_occupants(),
        map_id = isolate(current_map_id())
      )
      
      session$sendCustomMessage(
        "combat3d-init",
        list(
          containerId = session$ns("combat_3d_container"),
          mapData = jsonlite::toJSON(
            render_df,
            dataframe = "rows",
            auto_unbox = TRUE,
            null = "null"
          ),
          inputIds = list()
        )
      )
    }, ignoreInit = TRUE)
  })
}