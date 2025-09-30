popUI <- function(id, opt) {
  ns <- NS(id)
  fluidPage(
    tags$style(HTML(sprintf("
      #%s {
        color: white !important;;
        font-size: 14px;
      }
    ", ns("md_text")))),
    
    div(
      id = ns("md_text"),
      includeMarkdown("./Rmd/text_intro_tab.md")
    ),
    br(), 
    radioButtons(ns("popAnalysis"), "Select the type of analysis",
                 choices = c("Population size estimation" = "popSize",
                             "Population area of occurence" = "popArea"), selected = "popSize"),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'popArea'", ns("popAnalysis")),
      selectizeInput(ns("sppCache"), "Select a species:", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Start typing to search...",maxItems = 1,maxOptions = 999, closeOnSelect = FALSE)),
      selectInput(ns("quantileType"), "Select the type of analysis",
                  choices = c("Use Lorenzo quantile" = 'Lorenzo',
                              "Set the quantile value" = 'custom'),
                  selected = 'Lorenzo')),
    conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'popArea' && input['%s'] == 'custom'",
        ns("popAnalysis"), ns("quantileType")
      ),
      div(style = "color: white !important;", sliderInput(ns("quantile"), " ", min = 0, max = 1, value = 0.8, step = 0.05))
    )
  )
}

popTable <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf("
      #%s table.dataTable,
      #%s table.dataTable th,
      #%s table.dataTable td {
        color: white !important;
      }
    ", ns("popSizeTbl"), ns("popSizeTbl"),ns("popSizeTbl")))),
    #tabsetPanel(
      tabPanel("Population size estimation", DT::dataTableOutput(ns("popSizeTbl")))
    #)
  )
}

popOccUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Species occurrence",  # This will appear as a tab
    tags$style(HTML(sprintf("
      #%s table.dataTable,
      #%s table.dataTable th,
      #%s table.dataTable td {
        color: white !important;
      }
    ", ns("popOccTbl"), ns("popOccTbl"), ns("popOccTbl")))),
    br(),
    plotOutput(ns("popOccPlot"), height = "700px"),
    br(),
    DT::dataTableOutput(ns("popOccTbl"))
  )
}

popSERVER <- function(input, output, session, layers, myMapProxy, reactiveVals) {
  
  ns <- session$ns
  
  sppMap <- reactiveVals$sppSelectCache 
  spplist <- sppMap()
  sppName_origin <- names(sppMap())
  sppMapname <- reactiveVals$inserted_ids
  sppNames <- sppMapname()
  
  names(spplist) <- sppNames
  sppMap(spplist)
  #####################################
  ## observe on sppMap
  observeEvent(sppMapname(), {
    updateSelectizeInput(session, "sppCache", choices = sppMapname(), server = TRUE)
  }, ignoreNULL = TRUE)
  
  
  observe({
    reactiveVals$pop_module_out(input$popAnalysis)
  })
  ############

  output$popPanel <- renderUI({
    req(input$popAnalysis)
    
    if (input$popAnalysis == "popArea") {
      popOccUI("pop_module")  # returns a tabPanel
    } else {
      NULL  # tab doesn’t exist
    }
  })
  # Render kable table into UI
  pop_aoi_result <- bam_pop_size(spplist)
  
  output$popSizeTbl <- DT::renderDataTable({
    pop_aoi_result
  }, options = list(dom = 't'), rownames = FALSE)
  
  
  quantile <- reactive({
    req(input$quantileType)
    if(input$quantileType == "custom"){
      input$quantile 
    }
  })
  
  occRasters <- reactive({
    req(sppMap())
    if(input$quantileType == "Lorenzo"){
      pop_aoi_result <- bam_occurrence(sppMap())
    } else {
      pop_aoi_result <- bam_occurrence(sppMap(), quantile = quantile())
    }
  })

  
  output$popOccPlot <- renderPlot({
    req(input$sppCache, occRasters())
    req(input$popAnalysis=="popArea")
    
    rpop <- occRasters()$occurrence_rasters[[input$sppCache]]
    rpop_pj <- terra::project(rpop, "EPSG:4326")  
    terra::plot(rpop_pj, main = input$sppCache, col = c("white", "darkgreen"))
  })
  
  output$popOccTbl <- DT::renderDataTable({
    req(input$popAnalysis=="popArea")
    r <- occRasters()$occurrence_summary %>%
      filter(species == input$sppCache)
  }, options = list(dom = 't'), rownames = FALSE)
  
  output$predPlot <- renderPlot({
    req(input$sppDisplay)
    
    selected_name <- strsplit(input$sppCache, "_")[[1]][1]
    display_col <- sppDisplay() 
    
    species_code <- spplist %>%
      filter(!!sym(display_col) == selected_name) %>%
      pull(speciesCode)
    
    rpop <- occRasters()$occurrence_rasters[[input$sppCache]]
    terra::plot(rpop, main = input$sppCache, col = c("white", "darkgreen"))
  })
  
  
  
}