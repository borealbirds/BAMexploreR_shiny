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
      includeMarkdown("./Rmd/popstats_tab.md")
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

popDwdUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$style(type="text/css", "#downloadData {background-color:white;color: black}"),
    div(style = "margin-top: 40px;",
       downloadButton(ns("popDwdoutput"), "Download population estimates tables"),
    )
  )
}

popSppUI  <- function(id) {
  ns <- NS(id)
  uiOutput(ns("popsppboxes"))
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
  #Renser UI
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
  
  # Render right panel checkboxInput
  output$popsppboxes <- renderUI({
    req(reactiveVals$inserted_ids())
    
    checkbox_list <- lapply(reactiveVals$inserted_ids(), function(id) {
      checkboxInput(inputId = ns(id),
                    label = tags$span(class = "dynamic-checkbox-label", id),
                    value = FALSE)
    })
    
    tagList(div("Select the species for which you want to download population size and occurence tables:",
                    style = "color: white !important; font-size:14px; font-weight: bold; margin-top: 50px; margin-bottom: 30px;"),
            checkbox_list
            )
  })
  
  ###################################
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
  
  observe({
    req(reactiveVals$sppSelectCache())
    
    spp_names <- names(reactiveVals$sppSelectCache())
    selected <- reactiveVals$sppSelectCache()[spp_names %in% names(input) & 
                                                purrr::map_lgl(spp_names, ~ input[[.x]] %||% FALSE)]
    if (length(selected) == 0) {
      shinyjs::disable("popDwdoutput")  # Disable button
    } else {
      shinyjs::enable("popDwdoutput")   # Enable button
    }
  })
  
  ###########################################################
  ###########################################################
  # Download
  ###########################################################
  ###########################################################
  output$popDwdoutput <- downloadHandler(
    filename = function() { "BAM_Tables_output.zip" },
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
      
      occ_csv <- "BAM_species_occurrence.csv"
      occ_out <- occRasters()$occurrence_summary %>% filter(species %in% names(selected))
      readr::write_csv(occ_out, occ_csv)
      
      popsize_csv <- "BAM_species_popsize.csv"
      pop_out <- pop_aoi_result %>% filter(species %in% names(selected))
      readr::write_csv(pop_out, popsize_csv)
      
      all_files <- c(tiff_files, popsize_csv, occ_csv)
      
      # Create a ZIP file
      setwd(tempdir())
      zip::zip(zipfile = "BAM_population_estimates.zip", files = all_files)
      
      # Copy ZIP file to chosen location
      file.copy("BAM_population_estimates.zip", file, overwrite = TRUE)
      
      # Clean up temporary files
      unlink(c(tiff_files,"BAM_population_estimates.zip"), force = TRUE)
    },
    contentType = "application/zip"
  )
  
}