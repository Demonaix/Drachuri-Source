library(shiny)

control_map_presets <- function() c(
  "Tavern / Inn"="tavern","Prison / Cells"="prison","Forest"="forest",
  "Dungeon"="dungeon","Cave"="cave","Swamp"="swamp",
  "Road / Crossroads"="road","Ruins"="ruins","Ravine / Gorge"="ravine",
  "River Crossing"="river","Coast / Beach"="coast"
)

generate_control_map_tiles <- function(map_id,width,height,preset="forest",seed=1L,density=35) {
  width<-max(1L,as.integer(width));height<-max(1L,as.integer(height));density<-max(0,min(100,as.numeric(density)))
  set.seed(as.integer(seed%||%1L));indoor<-preset%in%c("tavern","prison","dungeon")
  tiles<-create_square_map_tiles(map_id,width,height,default_terrain=if(indoor)"stone"else if(preset=="cave")"stone"else"grass",default_light=if(preset%in%c("dungeon","cave"))"dark"else if(indoor)"dim"else"full")
  at<-function(x=NULL,y=NULL){keep<-rep(TRUE,nrow(tiles));if(!is.null(x))keep<-keep&tiles$x%in%x;if(!is.null(y))keep<-keep&tiles$y%in%y;keep}
  paint<-function(idx,terrain,light=NULL){tiles$terrain[idx]<<-terrain;props<-switch(terrain,
    wall=list(1,TRUE,TRUE),ravine=list(1,TRUE,FALSE),water=list(2,FALSE,FALSE),forest=list(2,FALSE,TRUE),swamp=list(2,FALSE,FALSE),sand=list(1.5,FALSE,FALSE),
    table=list(1,TRUE,FALSE),bar=list(1,TRUE,FALSE),crate=list(1,TRUE,FALSE),barrel=list(1,TRUE,FALSE),shelf=list(1,TRUE,TRUE),
    chair=list(2,FALSE,FALSE),bench=list(2,FALSE,FALSE),bed=list(2,FALSE,FALSE),rubble=list(2,FALSE,FALSE),campfire=list(2,FALSE,FALSE),torch=list(1,FALSE,FALSE),brazier=list(2,FALSE,FALSE),
    list(1,FALSE,FALSE));tiles$move_cost[idx]<<-props[[1]];tiles$blocks_movement[idx]<<-props[[2]];tiles$blocks_vision[idx]<<-props[[3]];if(!is.null(light))tiles$light[idx]<<-light}
  perimeter<-function(){paint(at(c(1L,width),NULL)|at(NULL,c(1L,height)),"wall")}
  door<-function(x=ceiling(width/2),y=1L){paint(at(x,y),"stone",if(indoor)"dim"else"full")}
  sample_open<-function(prob){which(stats::runif(nrow(tiles))<prob & tiles$x>1L & tiles$x<width & tiles$y>1L & tiles$y<height & !as.logical(tiles$blocks_movement))}
  if(preset=="tavern"){
    perimeter();door();if(width>=7L&&height>=6L){bar_y<-height-2L;paint(at(seq(max(3L,ceiling(width*.55)),width-2L),bar_y),"bar");for(x in seq(3L,width-2L,by=3L))for(y in seq(3L,max(3L,height-3L),by=3L))if(stats::runif(1)<density/100){paint(at(x,y),"table");if(x+1L<width)paint(at(x+1L,y),"chair")};paint(at(2L,unique(pmax(2L,pmin(height-1L,c(3L,height-2L))))),"torch")}
  }else if(preset=="prison"){
    perimeter();door();if(width>=6L){for(x in seq(4L,width-2L,by=4L)){paint(at(x,seq(2L,height-1L)),"wall");for(y in unique(pmax(2L,pmin(height-1L,c(ceiling(height/3),ceiling(2*height/3))))))paint(at(x,y),"stone","dim");if(x>2L)paint(at(x-1L,height-1L),"bed")}}
  }else if(preset=="forest"){
    paint(sample_open(density/100),"forest");road_x<-pmax(1L,pmin(width,round(width/2+sin(seq_len(height)/2)*pmax(1,width/8))));for(y in seq_len(height))paint(at(unique(pmax(1L,pmin(width,c(road_x[y]-1L,road_x[y])))),y),"road")
  }else if(preset=="dungeon"){
    perimeter();door();if(width>=7L)for(x in seq(5L,width-2L,by=5L)){paint(at(x,seq(2L,height-1L)),"wall");paint(at(x,max(2L,min(height-1L,sample(2:max(2L,height-1L),1)))),"stone","dim")};if(height>=7L)for(y in seq(5L,height-2L,by=5L)){paint(at(seq(2L,width-1L),y),"wall");paint(at(max(2L,min(width-1L,sample(2:max(2L,width-1L),1))),y),"stone","dim")};paint(sample_open(density/500),"barrel");paint(sample_open(density/700),"brazier")
  }else if(preset=="cave"){
    paint(sample_open(density/130),"wall");paint(sample_open(density/300),"rubble");if(width>=8L&&height>=8L)paint(at(sample(2:(width-1L),max(1L,round(width/8))),sample(2:(height-1L),max(1L,round(height/8)))),"ravine")
  }else if(preset=="swamp"){
    paint(sample_open(density/100),"swamp");paint(sample_open(density/260),"water");path_x<-ceiling(width/2);paint(at(unique(pmax(1L,pmin(width,c(path_x-1L,path_x)))),NULL),"road")
  }else if(preset=="road"){
    cx<-ceiling(width/2);cy<-ceiling(height/2);paint(at(unique(pmax(1L,pmin(width,c(cx-1L,cx)))),NULL),"road");paint(at(NULL,unique(pmax(1L,pmin(height,c(cy-1L,cy))))),"road");paint(sample_open(density/170),"forest")
  }else if(preset=="ruins"){
    paint(sample_open(density/180),"stone");paint(sample_open(density/220),"rubble");for(i in seq_len(max(1L,round(density/12)))){x<-sample(seq_len(width),1);y<-sample(seq_len(height),1);len<-sample(2:max(2L,min(6L,max(width,height))),1);if(stats::runif(1)<.5)paint(at(seq(x,min(width,x+len-1L)),y),"wall")else paint(at(x,seq(y,min(height,y+len-1L))),"wall")}
  }else if(preset=="ravine"){
    paint(sample_open(density/260),"stone");centre<-round(width/2+sin(seq_len(height)/2.4+seed)*pmax(1,width/7));bridge_y<-max(1L,min(height,round(height*.55)));for(y in seq_len(height)){xs<-unique(pmax(1L,pmin(width,c(centre[y]-1L,centre[y]))));paint(at(xs,y),if(y%in%c(bridge_y,bridge_y+1L))"road"else"ravine")}
  }else if(preset=="river"){
    paint(sample_open(density/220),"forest");centre<-round(width/2+sin(seq_len(height)/2.8+seed)*pmax(1,width/8));bridge_y<-max(1L,min(height,round(height*.55)));for(y in seq_len(height)){xs<-unique(pmax(1L,pmin(width,c(centre[y]-1L,centre[y],centre[y]+1L))));paint(at(xs,y),if(y%in%c(bridge_y,bridge_y+1L))"road"else"water")}
  }else if(preset=="coast"){
    shoreline<-round(width*.62+sin(seq_len(height)/2.6+seed)*pmax(1,width/14));for(y in seq_len(height)){edge<-max(2L,min(width-1L,shoreline[y]));paint(at(seq(max(1L,edge-1L),min(width,edge+1L)),y),"sand");if(edge+2L<=width)paint(at(seq(edge+2L,width),y),"water")};inland<-which(tiles$terrain=="grass"&stats::runif(nrow(tiles))<density/280);paint(inland,"forest")
  }
  tiles
}

place_generated_map_objects<-function(map_id,tiles,preset,session_id){
  if(is.na(suppressWarnings(as.integer(session_id)))||!preset%in%c("prison","dungeon","tavern"))return(0L)
  candidates<-data.frame()
  if(preset%in%c("prison","dungeon")){
    walls<-tiles[tolower(tiles$terrain)=="wall",,drop=FALSE];cols<-as.integer(names(which(table(walls$x)>=2L)))
    for(x in cols)for(y in 2:(max(tiles$y)-1L)){here<-tiles[tiles$x==x&tiles$y==y,,drop=FALSE];up<-tiles[tiles$x==x&tiles$y==y-1L,,drop=FALSE];down<-tiles[tiles$x==x&tiles$y==y+1L,,drop=FALSE];if(nrow(here)&&tolower(here$terrain[[1L]])!="wall"&&nrow(up)&&nrow(down)&&tolower(up$terrain[[1L]])=="wall"&&tolower(down$terrain[[1L]])=="wall")candidates<-rbind(candidates,data.frame(x=x,y=y))}
  }
  if(!nrow(candidates)&&preset=="tavern"){edge<-tiles[tiles$y==min(tiles$y)&tolower(tiles$terrain)!="wall",,drop=FALSE];if(nrow(edge))candidates<-edge[1,c("x","y"),drop=FALSE]}
  if(!nrow(candidates))return(0L);candidates<-unique(candidates);count<-0L
  for(i in seq_len(nrow(candidates))){kind<-if(preset=="prison")"gate"else"door";difficulty<-if(preset=="tavern")"standard"else if(preset=="prison")"hard"else"standard";lock<-create_chest(session_id,paste(tools::toTitleCase(kind),paste0("(",candidates$x[[i]],", ",candidates$y[[i]],")")),difficulty,list(),preset!="tavern",lock_kind=kind);if(!is.null(lock)&&isTRUE(place_map_object(map_id,candidates$x[[i]],candidates$y[[i]],kind,lock$id[[1L]])))count<-count+1L}
  count
}

controlMapBuilderUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    tags$link(rel = "stylesheet", type = "text/css", href = "css/combat.css"),
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
.map-builder-cell.object-door::before,.map-builder-cell.object-gate::before,.map-builder-cell.object-chest::before{position:absolute;inset:1px;z-index:4;display:flex;align-items:center;justify-content:center;font-size:15px;text-shadow:0 1px 2px #fff;pointer-events:none}
.map-builder-cell.object-door::before{content:'🚪'}.map-builder-cell.object-gate::before{content:'▥'}.map-builder-cell.object-chest::before{content:'▣'}.map-builder-cell.object-unlocked::before{opacity:.55}
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
          ),
          tags$hr(),
          div(class="control-section-title","Autogenerate a new map"),
          p(class="control-mini","Creates a new editable map. It never overwrites the currently selected map."),
          div(class="map-builder-toolbar",
              selectInput(ns("generator_preset"),"Map type",choices=control_map_presets(),selected="tavern",width="190px"),
              numericInput(ns("generator_seed"),"Variation seed",value=1,min=1,step=1,width="125px"),
              sliderInput(ns("generator_density"),"Feature density",min=10,max=80,value=35,step=5,width="220px"),
              actionButton(ns("generate_map"),"Generate New Map",class="btn btn-success"))
        ),
        
        div(
          class = "map-builder-side",
          
          div(
            class = "control-card",
            div(class = "control-section-title", "Paint Tools"),
            radioButtons(
              ns("builder_tool_mode"),
              "Active tool",
              choices = c("Select tile" = "select", "Paint terrain" = "paint", "Place object" = "object"),
              selected = "select",
              inline = TRUE
            ),
            
            div(
              class = "map-builder-toolbar",
              selectInput(
                ns("paint_terrain"),
                "Terrain",
                choices = list(
                  "Ground"=c("Grass"="grass","Sand"="sand","Stone"="stone","Forest"="forest","Swamp"="swamp","Water"="water","Wall"="wall","Ravine"="ravine","Road"="road","Mandred convergence"="mandred_convergence"),
                  "Common clutter"=c("Table"="table","Bar counter"="bar","Chair"="chair","Bench"="bench","Crate"="crate","Barrel"="barrel","Bed"="bed","Shelf"="shelf","Rubble"="rubble","Campfire"="campfire","Torch"="torch","Brazier"="brazier")
                ),
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
              actionButton(ns("arm_paint_tool"), "Arm Paint Brush", class = "btn btn-warning"),
              actionButton(ns("select_only_tool"), "Disarm Tools", class = "btn btn-default"),
              actionButton(ns("paint_mode_apply"), "Paint Selected Tile", class = "btn btn-success"),
              actionButton(ns("paint_mode_fill"), "Fill Whole Map", class = "btn btn-default"),
              actionButton(ns("save_map"), "Save Map", class = "btn btn-success")
            ),
            
            tags$hr(),
            
            div(class = "control-section-title", "Selected Tile"),
            uiOutput(ns("selected_tile_ui")),
            tags$hr(),
            div(class="control-section-title","Map Object"),
            div(class="map-builder-toolbar",
              selectInput(ns("object_type"),"Object",c("Door"="door","Gate"="gate","Existing chest"="chest"),width="150px"),
              conditionalPanel(
                condition = "input.object_type !== 'chest'",
                selectInput(ns("object_difficulty"),"New door/gate lock",c("Unlocked"="unlocked","Easy"="easy","Standard"="standard","Hard"="hard","Master"="master"),width="180px"),
                ns = ns
              ),
              conditionalPanel(
                condition = "input.object_type === 'chest'",
                selectInput(ns("object_chest_id"),"Chest",choices=character(),width="210px"),
                tags$small("This uses the lock already saved with the chest."),
                ns = ns
              ),
              actionButton(ns("arm_object_tool"),"Arm Object Placement",class="btn btn-warning"),
              actionButton(ns("place_object"),"Place on Selected Tile",class="btn btn-primary"),
              actionButton(ns("remove_object"),"Remove Object",class="btn btn-danger"))
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
            selectInput(ns("preview_3d_quality"), "3D Detail", choices = c("Low"="low", "Balanced"="balanced", "Decorative"="decorative"), selected = "decorative", width = "180px"),
            uiOutput(ns("map_preview_ui"))
          )
        )
      )
    )
  )
}

controlMapBuilderServer <- function(id, ctrl, session_tbl = NULL, players_tbl = NULL, positions_tbl = NULL, bump_refresh) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$paint_terrain, {
      defaults <- list(
        water=c(2,0,0), ravine=c(1,1,0), table=c(1,1,0), bar=c(1,1,0), crate=c(1,1,0), barrel=c(1,1,0), shelf=c(1,1,1),
        chair=c(2,0,0), bench=c(2,0,0), bed=c(2,0,0), rubble=c(2,0,0), campfire=c(2,0,0), torch=c(1,0,0), brazier=c(2,0,0)
      )
      d <- defaults[[as.character(input$paint_terrain %||% "")]]
      if (is.null(d)) return()
      updateNumericInput(session, "paint_move_cost", value = as.numeric(d[[1L]]))
      updateCheckboxInput(session, "paint_blocks_movement", value = isTRUE(as.logical(as.numeric(d[[2L]]))))
      updateCheckboxInput(session, "paint_blocks_vision", value = isTRUE(as.logical(as.numeric(d[[3L]]))))
    }, ignoreInit = TRUE)
    
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

    as_editor_tiles <- function(tiles) {
      if (is.data.frame(tiles) && "terrain_blocks_movement" %in% names(tiles)) {
        tiles$blocks_movement <- as.logical(tiles$terrain_blocks_movement)
      }
      tiles
    }
    

  
    
    load_maps <- function() {
      maps <- list_existing_maps()
      if (!is.data.frame(maps)) maps <- data.frame()
      maps_rv(maps)
    }
    
    loaded_map_id <- reactiveVal(NA_integer_)

    current_map_id <- reactive({
      mid <- suppressWarnings(as.integer(loaded_map_id()))
      if (is.na(mid) || mid < 1) mid <- suppressWarnings(as.integer(input$map_select %||% ctrl$map_id %||% NA))
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
  loaded_map_id(mid)
  tiles_cache(as_editor_tiles(tiles))
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

    observeEvent(input$generate_map, {
      w<-max(1L,suppressWarnings(as.integer(input$map_width%||%10L)));h<-max(1L,suppressWarnings(as.integer(input$map_height%||%10L)))
      preset<-as.character(input$generator_preset%||%"forest");seed<-suppressWarnings(as.integer(input$generator_seed%||%1L));if(is.na(seed))seed<-1L
      label<-names(control_map_presets())[match(preset,control_map_presets())]%||%tools::toTitleCase(preset)
      map_name<-trimws(as.character(input$new_map_name%||%""));if(!nzchar(map_name))map_name<-paste0(label," ",format(Sys.time(),"%Y-%m-%d %H:%M"))
      mid<-create_map_record(map_name,w,h);if(is.na(mid)||mid<1L){showNotification("Failed to create generated map record.",type="error");return()}
      tiles<-tryCatch(generate_control_map_tiles(mid,w,h,preset,seed,input$generator_density%||%35),error=function(e){showNotification(paste("Map generation failed:",conditionMessage(e)),type="error",duration=12);data.frame()})
      if(!nrow(tiles)||!isTRUE(set_shared_map_tiles(ctrl,mid,tiles))){showNotification("The map record was created, but its generated tiles could not be saved.",type="error");return()}
      placed_objects<-place_generated_map_objects(mid,tiles,preset,suppressWarnings(as.integer(ctrl$session_id%||%ctrl$active_session_id%||%NA)))
      refreshed_tiles<-tryCatch(as_editor_tiles(get_map_tiles(mid)),error=function(e)tiles)
      if(!is.data.frame(refreshed_tiles)||!nrow(refreshed_tiles))refreshed_tiles<-tiles
      selected_x(1L);selected_y(1L);ctrl$map_id<-mid;loaded_map_id(mid);tiles_cache(refreshed_tiles);bump_map_render();load_maps();updateSelectInput(session,"map_select",selected=as.character(mid));if(is.function(bump_refresh))bump_refresh()
      showNotification(paste0("Generated ",label,": ",map_name," (#",mid,") using seed ",seed,if(placed_objects>0)paste0(" with ",placed_objects," contextual door/gate",if(placed_objects==1)""else"s")else"","."),type="message",duration=8)
    },ignoreInit=TRUE)

    
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

    chest_choice_signature <- reactiveVal(NULL)
    observe({
      sid<-suppressWarnings(as.integer(ctrl$session_id%||%ctrl$active_session_id%||%NA));rows<-if(is.na(sid))data.frame()else list_session_chests(sid,TRUE,portable_only=TRUE)
      choices<-if(nrow(rows))setNames(as.character(rows$id),paste0(rows$name," · ",ifelse(rows$locked,tools::toTitleCase(rows$difficulty),"Unlocked")))else character()
      signature<-paste(names(choices),choices,collapse="|")
      if(identical(signature,chest_choice_signature()))return()
      chest_choice_signature(signature)
      selected<-as.character(isolate(input$object_chest_id)%||%"")
      if(!selected%in%unname(choices))selected<-if(length(choices))unname(choices[[1L]])else character()
      updateSelectInput(session,"object_chest_id",choices=choices,selected=selected)
    })
    observeEvent(input$arm_object_tool,{
      updateRadioButtons(session,"builder_tool_mode",selected="object")
      showNotification("Object placement armed. Click a tile, then place the selected object.",type="message")
    },ignoreInit=TRUE)
    observeEvent(input$arm_paint_tool,{
      updateRadioButtons(session,"builder_tool_mode",selected="paint")
      showNotification("Paint brush armed. Click or drag across the map to paint.",type="message")
    },ignoreInit=TRUE)
    observeEvent(input$select_only_tool,{
      updateRadioButtons(session,"builder_tool_mode",selected="select")
      showNotification("Map tools disarmed. Clicking now only selects tiles.",type="message")
    },ignoreInit=TRUE)
    refresh_current_map_tiles <- function(mid){
      db_tiles<-tryCatch(get_map_tiles(mid),error=function(e){message("get_map_tiles refresh failed: ",conditionMessage(e));data.frame()})
      current<-tiles_cache()
      if(!is.data.frame(db_tiles)||!nrow(db_tiles)||!is.data.frame(current)||!nrow(current))return(FALSE)
      object_cols<-intersect(c("object_type","object_id","object_chest_id","object_locked","object_name","effective_blocks_movement"),names(db_tiles))
      keys<-paste(current$x,current$y,sep=",");db_keys<-paste(db_tiles$x,db_tiles$y,sep=",");idx<-match(keys,db_keys)
      for(nm in object_cols){vals<-db_tiles[[nm]][idx];if(is.character(vals))vals[is.na(vals)]<-"";current[[nm]]<-vals}
      tiles_cache(current);bump_map_render();invisible(TRUE)
    }
    observeEvent(input$place_object,{
      if(!identical(as.character(input$builder_tool_mode%||%"select"),"object"))return(showNotification("Choose Place object as the active tool first.",type="warning"))
      mid<-current_map_id();x<-selected_x();y<-selected_y();typ<-as.character(input$object_type%||%"door");sid<-suppressWarnings(as.integer(ctrl$session_id%||%ctrl$active_session_id%||%NA))
      if(is.na(mid)||is.na(x)||is.na(y)||is.na(sid))return(showNotification("Select a session, map and tile first.",type="error"))
      chest_id<-suppressWarnings(as.integer(input$object_chest_id%||%NA))
      if(typ%in%c("door","gate")){
        difficulty<-as.character(input$object_difficulty%||%"standard");lock<-create_chest(sid,paste(tools::toTitleCase(typ),paste0("(",x,", ",y,")")),if(difficulty=="unlocked")"standard"else difficulty,list(),difficulty!="unlocked",lock_kind=typ)
        if(is.null(lock))return(showNotification("The lock record could not be created.",type="error"));chest_id<-as.integer(lock$id[[1L]])
      }
      if(is.na(chest_id))return(showNotification("Choose the specific chest to place.",type="error"))
      if(!isTRUE(place_map_object(mid,x,y,typ,chest_id)))return(showNotification("Object could not be placed.",type="error"))
      refresh_current_map_tiles(mid);showNotification(paste(tools::toTitleCase(typ),"placed."))
    },ignoreInit=TRUE)
    observeEvent(input$remove_object,{mid<-current_map_id();x<-selected_x();y<-selected_y();if(isTRUE(remove_map_object(mid,x,y))){refresh_current_map_tiles(mid);showNotification("Map object removed.")}else showNotification("There is no object on that tile.",type="warning")},ignoreInit=TRUE)
    
    
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
      input$builder_tool_mode
      
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
            toolMode = isolate(as.character(input$builder_tool_mode %||% "select")),
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
    
    send_3d_preview <- function(delay = 0) {
      later::later(function() {
        if (!identical(isolate(input$preview_mode %||% "2d"), "3d")) return()
        tiles <- isolate(current_tiles())
        if (!is.data.frame(tiles) || nrow(tiles) == 0) return()
        render_df <- build_map_render_df(tiles = tiles, occupants = empty_map_occupants(), map_id = isolate(current_map_id()))
        session$sendCustomMessage("combat3d-lean-init", list(
          containerId = session$ns("combat_3d_container"),
          mapData = jsonlite::toJSON(render_df, dataframe = "rows", auto_unbox = TRUE, null = "null"),
          inputIds = list(), quality = isolate(input$preview_3d_quality %||% "decorative")
        ))
      }, delay = delay)
    }

    observeEvent(session$rootScope()$input$combat3d_lean_ready, {
      if (identical(input$preview_mode %||% "2d", "3d")) send_3d_preview(0.05)
    }, ignoreInit = FALSE)

    observeEvent(input$preview_mode, {
      if (identical(input$preview_mode %||% "2d", "3d")) send_3d_preview(0.15)
    }, ignoreInit = TRUE)

    observeEvent(input$preview_3d_quality, {
      if (identical(input$preview_mode %||% "2d", "3d")) send_3d_preview(0.05)
    }, ignoreInit = TRUE)

    output$map_preview_ui <- renderUI({
      mode <- input$preview_mode %||% "2d"
      
      if (identical(mode, "3d")) {
        session$onFlushed(function() {
          send_3d_preview(0.05)
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
      send_3d_preview()
    }, ignoreInit = TRUE)
  })
}
