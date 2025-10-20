# Define server logic
server <- function(input, output, session) {

  ################################################################################################
  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })
  ################################################################################################
  
  layers <- callModule(reactiveLayersModule, id = "reactiveLayersModule")

  # tab and module-level reactives
  module <- reactive({
    input$tabs
  })

  reactiveValsList <- list(
    insertedTabs = reactiveVal(c()),
    subunit_names = reactiveVal(NULL),
    selected_subunits = reactiveValues(selected = character()),
    mapCache = reactiveVal(0),
    sppListCache = reactiveVal(NULL),
    sppDisplay = reactiveVal(NULL),
    sppSelectCache = reactiveVal(NULL),
    bcrCache = reactiveVal(NULL),
    inserted_ids = reactiveVal(character()),
    data_ready = reactiveVal(FALSE),
    pop_module_out = reactiveVal(NULL)
  )
  
  ######################## #
  ### GUIDANCE TEXT ####
  ######################## #
  # UI for component guidance text
  output$gtext_module <- renderUI({
    req(input$tabs)
    file <- file.path('Rmd', glue("gtext_{input$tabs}.Rmd"))
    if (!file.exists(file)) return()
    includeMarkdown(file)
  })

  # Control right Panel
  output$rightPanel <- renderUI({
    if (input$tabs == "data") {
      tabsetPanel(
        tabPanel("Download",
                 bandUI("explore_module"),
                 sppUI("explore_module"),
                 dwdUI("explore_module")
        )
      )
    } else if(input$tabs == "popstats"){
      tabsetPanel(
        tabPanel("Download",
                 br(),
                 br(),
                 sppUI("explore_module"),
                 dwdUI("explore_module")
        )
      )
    }
  })
  
  observe({
    if (input$tabs == "data") {
      shinyjs::show("explore_module-band")
      shinyjs::show("explore_module-bandDef")
      shinyjs::show("explore_module-dwdNMoutput")
      shinyjs::show("explore_module-speciesboxes")
    } else if (input$tabs == "popstats") {
      shinyjs::hide("explore_module-band")
      shinyjs::show("explore_module-speciesboxes")
      shinyjs::show("explore_module-dwdNMoutput")
      shinyjs::show("explore_module-bandDef")
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
  # Initialize map
  # Initialize Leaflet map centered on Canada
  output$myMap <- renderLeaflet({
    
    leaflet() %>%
      addMapPane(name = "ground", zIndex=380) %>%
      addMapPane(name = "overlay", zIndex=420) %>%
      addProviderTiles("CartoDB.Positron", group="baseMap") %>%
      leafem::addMouseCoordinates() %>%
      # Fit bounds to Canada's extent
      fitBounds(lng1 = -141.0, lat1 = 42, lng2 = -52.0, lat2 = 70) %>%
      addLayersControl(position = "topright",
                       options = layersControlOptions(collapsed = FALSE))
  })

  # Create map proxy for updates
  myMap <- leafletProxy("myMap", session)

  ########################## #
  ########################## #
  ### ACCESS THE DATA   ###
  ########################## #
  ########################## #
  # Provide species UI
  observeEvent(input$tabs, {
    req(input$tabs == "data")
    
    callModule(
      exploreSERVER, "explore_module",
      spp_list = spp_tbl,
      layers = layers,
      myMap = myMap,
      reactiveVals = reactiveValsList  # Pass the entire list
    )
    
  })
  
  ################################################################################################
  # Observe on tabs
  ################################################################################################
  observe({
    req(input$`pop_module-popAnalysis`)
    if (input$`pop_module-popAnalysis` == "popArea") {
      # switch away if user tries to view Population table
      updateTabsetPanel(session, "centerPanel", selected = "Species occurrence")
    }else{
      updateTabsetPanel(session, "centerPanel", selected = "MapView")
    }
  })
  
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
        "Please run Access the data before proceeding into Population Distribution",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)  # stop here
    }
    
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
    req(input$tabs == "pred")
    
    # Need to run getLayerNM first
    if (!isTRUE(reactiveValsList$data_ready())) {
      showModal(modalDialog(
        title = "Data not ready",
        "Please run Access the data before proceeding into Population Distribution",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)  # stop here
    }
    
    callModule(
      predSERVER, "pred_module",
      layers = layers,
      reactiveVals = reactiveValsList  # Pass the entire list
    )
    
  })
}





