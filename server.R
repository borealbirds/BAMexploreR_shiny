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
    sppSelectCache = reactiveVal(NULL),
    bcrCache = reactiveVal(NULL),
    inserted_ids = reactiveVal(character())
  )
  
  ######################## #
  ### GUIDANCE TEXT ####
  ######################## #
  # UI for component guidance text
  output$gtext_module <- renderUI({
    file <- file.path('Rmd', glue("gtext_{module()}.Rmd"))
    if (!file.exists(file)) return()
    includeMarkdown(file)
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
  ### EXPLORE THE DATA   ###
  ########################## #
  ########################## #
  ############################################################################################################ #
  ### RUN Explore MODULE   ##
  # 1.
  # 2.
  ############################################################################################################ #
  # Provide species UI
  observeEvent(input$tabs, {
    req(input$tabs == "data")
    
    callModule(
      exploreSERVER, "explore_module",
      spp_list = spp_list,
      layers = layers,
      myMap = myMap,
      reactiveVals = reactiveValsList  # Pass the entire list
    )
    
  })
}





