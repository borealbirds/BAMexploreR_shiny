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
      includeMarkdown("./Rmd/pred_tab.md")
    ),
    br(), 
    radioButtons(ns("predAnalysis"), "Select the type of analysis",
                 choices = c("Display mean relative predictor importance" = "predImpo",
                             "Display proportion of model predictors importance" = "predChart"), selected = "predImpo"),
    br(),
    br(), 
    selectInput(ns("versionSelPred"), "Choose version:", choices = c("Version 4" ="v4", "Version 5" = "v5"), selected = "v5"),
    radioButtons(ns("sppDisplayPred"), "Display species using: ",
                 choices = c("Species Code" = "speciesCode",
                             "Common Name" = "commonName",
                             "Scientific Name" = "scientificName"),
                 selected = "commonName", inline = TRUE),
    selectizeInput(ns("sppPred"), "Select a species:", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Start typing to search...",maxOptions = 999, closeOnSelect = FALSE)),
    uiOutput(ns("bcrCheckboxesPred")),
    actionButton(ns("getPlot"), "Visualize the data", icon = icon(name = "fas fa-crow", lib = "font-awesome"), style="width:250px")
  )
}

barchartUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Prediction",  
    plotOutput(ns("predbarchart"), height = "700px")
  )
}

axisUI  <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = sprintf("input['%s'] == 'predImpo'", ns("predAnalysis")),
      selectInput(ns("group"), "Type of grouping", choices = c("species" = 'spp',
                                                             "BCR" = 'bcr'),
                  selected = 'bcr')
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'predChart'", ns("predAnalysis")),
      selectInput(ns("Xgroup"), "Type of grouping for X axis", choices = c("Species" = 'spp',
                                                                         "BCR" = 'bcr',
                                                                         "Predictor" = "predictor",
                                                                         "Predictor class" = "predictor_class"),
                  selected = 'bcr'),
      selectInput(ns("Ygroup"), "Type of grouping for Y axis", choices = c("species" = 'spp',
                                                                         "BCR" = 'bcr',
                                                                         "Predictor" = "predictor",
                                                                         "Predictor class" = "predictor_class"),
                  selected = 'spp')
    
    )
  )
}

predDwdUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$style(type="text/css", "#downloadData {background-color:white;color: black}"),
    div(style = "margin-top: 40px;",
        downloadButton(ns("predDwdoutput"), "Download predictors importance tables"),
    )
  )
}

predSERVER <- function(input, output, session, layers, myMapProxy, reactiveVals) {
  
  ns <- session$ns
  
  display_col <- reactiveVals$sppDisplay()
  sppMapname <- reactiveVals$inserted_ids
  sppNames <- sppMapname()
  bcr <- reactiveVals$bcrPred

  observeEvent(input$versionSelPred,{
    allSpp <- bam_spp_list(version = input$versionSelPred, type = input$sppDisplayPred)
    updateSelectizeInput(session, "sppPred", choices = allSpp, server = TRUE)
    
    bcr_list <- if (input$versionSelPred == "v5") bcrv5.map$subunit_ui else bcrv4.map$subunit_ui
    
    reactiveVals$bcrPred(bcr_list)
    
    output$bcrCheckboxesPred <- renderUI({
      checkbox_list <- lapply(bcr_list, function(name) {
        div(checkboxInput(inputId = ns(name), label = name, value = FALSE))
      })
      
      div(class = "checkbox-grid", do.call(tagList, checkbox_list))  # Wrap in a styled div
    })
  })
  
  selected_bcr <- reactive({
    req(bcr())  
    bcr()[map_lgl(bcr(), ~ input[[.x]] %||% FALSE)]
  })
  
  #####################################
  ## observe on sppMap
  observeEvent(input$getPlot,{
    output$predbarchart <- renderPlot({
      req(input$sppPred)
      
      spp_name <- input$sppPred
      
      species_name <- spp_tbl %>%
        filter(!!sym(input$sppDisplayPred) %in% spp_name) %>%
        pull(speciesCode)
      
      if(input$predAnalysis =="predImpo"){
        p <- bam_predictor_importance(species = species_name, bcr = selected_bcr(), group = input$group, version = input$versionSelPred, plot=TRUE)
        p + ggtitle(paste("Predictor importance using ", input$group)) +
          theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
      } else{
        if(input$Xgroup == input$Ygroup){
          showModal(modalDialog(
            title = "XY grouping are the same",
            "Please change one of the axis in order to create the barchart",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(NULL)  # stop here
        }
        
        p <- bam_predictor_barchart(species = species_name, bcr = selected_bcr(), group = c(input$Xgroup, input$Ygroup), version = input$versionSelPred, plot=TRUE)
        p + ggtitle(paste("Proportion of model predictors importance using ", input$Xgroup, " and ", input$Ygroup)) +
          theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
      }
      
    })
  })
  
  
  ###########################################################
  ###########################################################
  # Download
  ###########################################################
  ###########################################################
  output$predDwdoutput <- downloadHandler(
    filename = function() {
      paste0("myplot_", Sys.Date(), ".png")
    },
    content = function(file) {
      
      # Get selected species (checkboxes checked)
      spp_names <- names(reactiveVals$sppSelectCache())
      selected <- reactiveVals$sppSelectCache()[spp_names %in% names(input) &
                                                  purrr::map_lgl(spp_names, ~ input[[.x]] %||% FALSE)]
      
      # ---- Proceed with download ----
      tiff_files <- c()
      
      for (species_code in names(selected)) {
        raster_obj <- occRasters()$occurrence_rasters[[species_code]]
        tiff_name <- paste0(species_code, "_occurrence.tif")
        writeRaster(raster_obj, file.path(tempdir(), tiff_name), overwrite = TRUE)
        tiff_files <- c(tiff_files, tiff_name)
      }
    },
    contentType = "image/png"  # Optional, sets correct MIME type
  )
  
  
} 
  