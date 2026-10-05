# Define server logic
server <- function(input, output, session) {

  ################################################################################################
  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })
  ################################################################################################
  ################################################################################################
  # Maintenance
  app_paused <- reactiveFileReader(
    intervalMillis = 30000,   # check every 30 seconds (runs for every open session)
    session = session,
    filePath = "www/pause_flag.txt",
    readFunc = function(path) file.exists(path)
  )
  
  observe({
    if (app_paused()) {
      
      showModal(modalDialog(
        title = "Application temporarily unavailable",
        "The application is currently on hold for maintenance.",
        footer = NULL,
        easyClose = FALSE
      ))
      
      shinyjs::disable(selector = "body")
    }
  })
  ################################################################################################
  
  layers <- callModule(reactiveLayersModule, id = "reactiveLayersModule")


  reactiveValsList <- list(
    subunit_names = reactiveVal(NULL),
    mapCache = reactiveVal(0),
    sppListCache = reactiveVal(NULL),
    sppSelectCache = reactiveVal(NULL),
    bcrCache = reactiveVal(NULL),
    bcrPred =  reactiveVal(NULL),
    inserted_ids = reactiveVal(character()),
    data_ready = reactiveVal(FALSE),
    band = reactiveVal(NULL),
    sppOnMap = reactiveVal(NULL)
  )

  observe({
    if (input$tabs == "data") {
      shinyjs::show("explore_module-band")
      shinyjs::show("explore_module-dwdNMoutput")
      shinyjs::show("explore_module-speciesboxes")
    } else if (input$tabs == "popstats") {
      shinyjs::hide("explore_module-band")
      shinyjs::show("explore_module-speciesboxes")
      shinyjs::show("explore_module-dwdNMoutput")
    }
  })
  # Help Component
 # help_modules <- c("data", "dist")
  #lapply(help_modules, function(module) {
  #  btn_id <- paste0(module, "Help")
  #  observeEvent(input[[btn_id]], updateTabsetPanel(session, "main", "Module Guidance"))
  #})

  ######################## #
  ### MAPPING LOGIC ####
  ######################## #
  # Initialize Leaflet map centered on Canada
  output$myMap <- renderLeaflet({
    
    leaflet() %>%
      addMapPane(name = "ground", zIndex=380) %>%
      addMapPane(name = "overlay", zIndex=420) %>%
      #add listener
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        map.on('baselayerchange', function(e) {
          Shiny.setInputValue('active_raster', e.name, {priority: 'event'});
        });
      }
    ") %>%
      addProviderTiles("Esri.WorldGrayCanvas", group="baseMap") %>%
      leafem::addMouseCoordinates() %>%
      # Fit bounds to Canada's extent
      fitBounds(lng1 = -141.0, lat1 = 42, lng2 = -52.0, lat2 = 70) %>%
      addLayersControl(position = "topright",
                       options = layersControlOptions(collapsed = FALSE))
  })

  # Create map proxy for updates
  myMap <- leafletProxy("myMap", session)

  # Proxy calls sent before the map is rendered are dropped by leaflet.js,
  # so flag when the map exists (it reports its bounds once drawn)
  mapReady <- reactiveVal(FALSE)
  observeEvent(input$myMap_bounds, mapReady(TRUE), once = TRUE)

  # The "Species occurrence" tab only exists for the Area of occurrence analysis
  observe({
    req(input$tabs)
    if (input$tabs == "popstats" && identical(input$`pop_module-popAnalysis`, "popArea")) {
      showTab("centerPanel", "occView", select = TRUE)
    } else {
      hideTab("centerPanel", "occView")
      updateTabsetPanel(session, "centerPanel", selected = "mapView")
    }
  })

  ########################## #
  ########################## #
  ### ACCESS THE DATA   ###
  ########################## #
  ########################## #
  # Modules are started the first time their tab is opened, then only once per session:
  # calling callModule() again would register a duplicate set of observers
  modules_started <- character()

  # Provide species UI
  observeEvent(input$tabs, {
    req(input$tabs == "data", !"data" %in% modules_started)
    modules_started <<- c(modules_started, "data")

    callModule(
      exploreSERVER, "explore_module",
      spp_list = spp_tbl,
      layers = layers,
      myMap = myMap,
      reactiveVals = reactiveValsList,  # Pass the entire list
      mapReady = mapReady
    )
    
  })
  
  # Single place drawing the species legend: follows the layer shown on the map
  # (active_raster, set when the user switches layer) and the selected band.
  # A new run or a band change re-triggers it through sppSelectCache / band.
  observe({
    cache <- reactiveValsList$sppSelectCache()
    req(cache, reactiveValsList$band())

    selected <- input$active_raster
    if (is.null(selected) || !selected %in% names(cache)) selected <- reactiveValsList$sppOnMap()
    req(selected %in% names(cache))

    band_index <- as.numeric(reactiveValsList$band())
    legend_title <- if (band_index == 1) "Mean Density (males/ha)" else "Variation in density"

    myMap %>% .raster_legend(cache[[selected]][[band_index]], legend_title)
  })
  
  
  ################################################################################################
  # Observe on tabs
  ################################################################################################
  
  ########################### #
  ########################### #
  ### POPULATION ESTIMATES  ###
  ########################### #
  ########################### #
  
  observeEvent(input$tabs, {
    req(input$tabs == "popstats")
    
    # Need to run getLayerNM first
    if (!isTRUE(reactiveValsList$data_ready())) {
      showModal(modalDialog(
        title = "Data not ready",
        "Please run the Model Access tool before using the Population Distribution tool",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)  # stop here
    }

    req(!"popstats" %in% modules_started)
    modules_started <<- c(modules_started, "popstats")

    callModule(
      popSERVER, "pop_module",
      layers = layers,
      myMap = myMap,
      reactiveVals = reactiveValsList  # Pass the entire list
    )
    
  })
  
  ############################ #
  ############################ #
  ### PREDICTORS IMPORTANCE  ###
  ############################ #
  ############################ #
  observeEvent(input$tabs, {
    req(input$tabs == "pred", !"pred" %in% modules_started)
    modules_started <<- c(modules_started, "pred")

    callModule(
      predSERVER, "pred_module",
      spp_list = spp_tbl,
      layers = layers,
      reactiveVals = reactiveValsList  # Pass the entire list
    )
    
  })
}





