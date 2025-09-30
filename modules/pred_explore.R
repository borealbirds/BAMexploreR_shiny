predUI <- function(id, opt) {
  ns <- NS(id)
  fluidPage(
    tags$style(HTML(sprintf("
      #%s {
        color: white !important;;
        font-size: 14px;
      }
    ", ns("md1_text")))),
    
    div(
      id = ns("md1_text"),
      includeMarkdown("./Rmd/text_intro_tab.md")
    ),
    br(), 
    selectizeInput(ns("sppPred"), "Select a species:", choices = NULL, multiple = TRUE,
                     options = list(placeholder = "Start typing to search...",maxItems = 1,maxOptions = 999, closeOnSelect = FALSE)),
    selectInput(ns("group"), "Type of grouping", choices = c("species" = 'spp',
                                                       "BCR" = 'bcr',
                                                       "predictor" = 'var',
                                                       "predictor class" = 'var_class'),
    selected = 'bcr')
  )
}

barchartUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Prediction",  
    plotOutput(ns("predbarchart"), height = "700px")
  )
}


predSERVER <- function(input, output, session, layers, myMapProxy, reactiveVals) {
  
  ns <- session$ns
  
  #sppMap <- reactiveVals$sppSelectCache 
  #spplist <- sppMap()
  #sppName_origin <- names(sppMap())
  display_col <- reactiveVals$sppDisplay()
  sppMapname <- reactiveVals$inserted_ids
  sppNames <- sppMapname()
  
  #names(spplist) <- sppNames
  #sppMap(spplist)
  bcr <- reactiveVals$bcrCache()
  
  
  allSpp <- bam_spp_list(version = layers$version_reactive())
  selected_name <- strsplit(sppNames, "_")[[1]][1]
  spp_vec <- reactive(
    c(selected_name, setdiff(allSpp, selected_name)) 
  )
  #####################################
  ## observe on sppMap
  observeEvent(spp_vec(), {
    updateSelectizeInput(session, "sppPred", choices = spp_vec(), server = TRUE)
  }, ignoreNULL = TRUE)
  
  
  output$predbarchart <- renderPlot({
    req(input$sppPred)
    spp_name <- input$sppPred
     
    
    species_name <- spp_list %>%
      filter(!!sym(display_col) == spp_name) %>%
      pull(speciesCode)
    
    p <- bam_predictor_importance(species = species_name, bcr = bcr, group = input$group, version = layers$version_reactive(), plot=TRUE)
    p + ggtitle(paste("Predictor importance for", spp_name, "using", input$group)) +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  })
  
  
  
} 
  