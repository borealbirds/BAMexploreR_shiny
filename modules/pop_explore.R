popUI <- function(id, opt) {
  ns <- NS(id)
  fluidPage(
    tags$style(HTML(sprintf("
      #%s {
        color: white !important;;
        font-size: 14px;
      }
    ", ns("md_text")))),
    br(), 
    radioButtons(ns("popAnalysis"), "Select the type of analysis:",
                 choices = c("Population size estimation" = "popSize",
                             "Area of occurence" = "popArea"), selected = "popSize"),
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
    h4("Population size estimation", style = "color: white !important; margin-top: 20px;"),
    DT::dataTableOutput(ns("popSizeTbl"))
  )
}

popOccUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML(sprintf("
      #%s table.dataTable,
      #%s table.dataTable th,
      #%s table.dataTable td {
        color: white !important;
      }
    ", ns("popOccTbl"), ns("popOccTbl"), ns("popOccTbl")))),
    h4("Area of occurrence", style = "color: white !important; margin-top: 20px;"),
    plotOutput(ns("popOccPlot"), height = "500px"),
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
       downloadButton(ns("popDwdoutput"), "Download selected estimates"),
    )
  )
}

popSppUI  <- function(id) {
  ns <- NS(id)
  uiOutput(ns("popsppboxes"))
}

popSERVER <- function(input, output, session, layers, myMapProxy, reactiveVals) {
  
  ns <- session$ns
  
  # The module starts once per session, so read the data module's results reactively
  # (sppSelectCache is already named like inserted_ids: <species>_<version>[_<year>])
  sppMap <- reactiveVals$sppSelectCache
  sppMapname <- reactiveVals$inserted_ids
  #####################################
  ## observe on sppMap
  observeEvent(sppMapname(), {
    updateSelectizeInput(session, "sppCache", choices = sppMapname(), server = TRUE)
  }, ignoreNULL = TRUE)
  
  
  # Render kable table into UI
  pop_aoi_result <- reactive({
    req(sppMap())
    bam_pop_size(sppMap())
  })

  output$popSizeTbl <- DT::renderDataTable({
    pop_aoi_result()
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

    # BCR delineation of the model version, in the raster CRS, without the mosaic polygons
    bcr <- if (grepl("_v4", input$sppCache)) BCRNMv4 else BCRNMv5
    bcr <- bcr[!bcr$bcr %in% c("Canada", "Alaska", "Lower48"), ]
    bcr <- terra::crop(terra::project(bcr, terra::crs(rpop)), terra::ext(rpop))
    bcr_sel <- bcr[bcr$bcr %in% reactiveVals$bcrCache(), ]

    terra::plot(rpop, main = input$sppCache, col = c("white", "darkgreen"))
    terra::lines(bcr, col = "grey50", lwd = 1)
    if (nrow(bcr_sel) > 0) terra::lines(bcr_sel, col = "black", lwd = 2)
  })
  
  output$popOccTbl <- DT::renderDataTable({
    req(input$popAnalysis=="popArea")
    
    r <- occRasters()$occurrence_summary %>%
      filter(species == input$sppCache)
  }, options = list(dom = 't'), rownames = FALSE)
  
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
      pop_out <- pop_aoi_result() %>% filter(species %in% names(selected))
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