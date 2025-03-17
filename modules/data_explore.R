######################## #
### GPKG            ####
######################## #
# gpkg UI
exploreUI <- function(id, opt) {
  ns <- NS(id)
  fluidPage(
    selectInput(ns("versionSel"), "Choose version:", choices = c("Version 4" ="v4", "Version 5" = "v5"), selected = "v5"),
    radioButtons(ns("sppDisplay"), "Display species using: ",
                 choices = c("Species Code" = "speciesCode",
                             "Common Name" = "commonName",
                             "Scientific Name" = "scientificName"),
                 selected = "commonName", inline = TRUE),
    selectizeInput(ns("sppSelect"), "Select a species:",
                  choices = NULL,  multiple = TRUE,
                  options = list(placeholder = "Start typing to search...",
                                 maxOptions = 10, closeOnSelect = FALSE)),
   div(style = "color: white !important; margin: 15px; margin-top: 10px; font-size:13px;font-weight: bold", "  --  Or  --"),
   selectInput(ns("sppGroup"), label = div(style = "font-size:13px;margin-top: -10px;", "Select a group of species"), choices = c("Please select", spp.grp), selected = "Please select"),
   div(style = "color: white !important; margin-top: 50px; font-size:13px;", "Select the data extent"),
   uiOutput(ns("bcrCheckboxes")),
   uiOutput(ns("yrSelect")),
   actionButton(ns("getLayerNM"), "Visualize the data", icon = icon(name = "fas fa-crow", lib = "font-awesome"), style="width:250px"),
  )
}

bandUI  <- function(id) {
  ns <- NS(id)
  div(style = "margin-top: 40px;", hidden(selectInput(ns("band"), label = "Select band:", 
              choices = c("Mean", "CV", "Mean extrapolation", 
                          "Km to nearest point count", "Not sure"), 
              selected = "Mean"
  )))
}

dwdUI  <- function(id) {
  ns <- NS(id)
  tags$style(type="text/css", "#downloadData {background-color:white;color: black}")
  div(style = "margin-top: 40px;", hidden(downloadButton(ns("dwdNMoutput"), "Download species map")))
}

sppUI  <- function(id) {
  ns <- NS(id)
  uiOutput(ns("speciesboxes"))
}

# GPKG server module
#exploreSERVER <- function(input, output, session, spp_list, layers, myMapProxy, subunit_names, selected_subunits, mapCache, sppListCache, bcrCache) {
exploreSERVER <- function(input, output, session, spp_list, layers, myMapProxy, reactiveVals) {
  
  ns <- session$ns
  pop = ~paste("BCR:", subunit_ui)

  # Access reactive values from the list
  subunit_names <- reactiveVals$subunit_names
  selected_subunits <- reactiveVals$selected_subunits
  mapCache <- reactiveVals$mapCache
  sppListCache <- reactiveVals$sppListCache
  sppSelectCache <- reactiveVals$sppSelectCache
  bcrCache <- reactiveVals$bcrCache
  
  observeEvent(input$versionSel,{
    bcr_map <- if (input$versionSel == "v5") bcrv5.map else bcrv4.map
    layers$version_reactive(input$versionSel)
    layers$bcr_reactive(bcr_map)

    # Extract unique values from the bcr_subunit column
    sub_names <- unique(bcr_map$subunit_ui)
    subnames <- c("mosaic", sub_names)
    
    #subunit_names(subnames)
    reactiveVals$subunit_names(subnames)
    
    output$bcrCheckboxes <- renderUI({
      req(subunit_names())  # Ensure it's not NULL
      
      checkbox_list <- lapply(subunit_names(), function(name) {
        div(checkboxInput(inputId = ns(name), label = name, value = name %in% bcrCache()))
      })
      
      div(class = "checkbox-grid", do.call(tagList, checkbox_list))  # Wrap in a styled div
    })
    
    
    if(input$versionSel == "v5"){
      output$yrSelect <- renderUI({
        selectInput(ns("modYr"), label = div(style = "font-size:13px;margin-top: -10px;", "Select the year"), choices = model.year, selected = "2020")
      })
    }else{
      output$yrSelect <- renderUI({
        NULL  # This will remove the selectInput if versionSel is "v4"
      })
    }
  }, ignoreInit = FALSE)

  observeEvent(layers$bcr_reactive(),{
    req(layers$bcr_reactive())
    req(input$getLayerNM[1]==0)
    bcr_map <- layers$bcr_reactive()
    
    ncat <- if(input$versionSel == "v5") 32 else 16 
    custom_palette <- RColorBrewer::brewer.pal(12, "Set3")  # Generate 12 colors from the Set3 palette
    custom_palette <- rep(custom_palette, length.out = ncat)  # Repeat the palette to get 25 colors
    pal <- colorFactor(palette = custom_palette, domain = bcr_map$subunit_ui)
    
    myMapProxy %>%
      clearGroup("BCR") %>%
      fitBounds(lng1 = -141.0, lat1 = 42, lng2 = -52.0, lat2 = 70) %>%
      addPolygons(data=bcr_map, color='black', fillColor = pal(bcr_map$subunit_ui), fillOpacity = 0.8, weight=2, layerId = bcr_map$subunit_ui, popup = pop, group="BCR", options = leafletOptions(pane = "ground")) %>%
      addLegend(pal = pal, values = bcr_map$subunit_ui, opacity = 1, title = "BCR Subunit", position = "bottomright", group = "BCR", layerId = "legend_custom") %>%
      addLayersControl(position = "topright",
                       overlayGroups = c("BCR"),
                       options = layersControlOptions(collapsed = FALSE))
  }, once= TRUE)
  
  
  selected_subunits <- reactive({
    req(subunit_names())  # Ensure subunit names exist
    
    selected <- subunit_names()[map_lgl(subunit_names(), ~ input[[.x]] %||% FALSE)]
    selected
  })
  
  observe({
    req(layers$bcr_reactive(), selected_subunits())  # Ensure both are available
    req(mapCache() != input$getLayerNM[1])
    
    bcr_map <- layers$bcr_reactive()
    selected_units <- subset(bcr_map, bcr_map$subunit_ui %in% selected_subunits())
    
    if(input$mosaic){
      selected_units <- bcr_map
    }
    
    myMapProxy %>% 
      clearGroup("highlighted") %>%  # Clear only highlighted group
      addPolygons(data = selected_units, fillColor = "red", fillOpacity = 0.7, weight = 2, color = "black", group = "highlighted", options = leafletOptions(pane = "overlay"))
  })
  
  # Update the Leaflet map when bcr_reactive() changes
  observe({
    req(spp_list)
    req(input$sppDisplay)
    
    sppURL <- sppList(input$versionSel, input$sppDisplay)
    sppOnline <- spp_list %>%
      filter(.data[[input$sppDisplay]] %in% sppURL)
      
    species_choices <- switch(input$sppDisplay,
                              "speciesCode" = sppOnline$speciesCode,
                              "commonName" = sppOnline$commonName,
                              "scientificName" = sppOnline$scientificName)
    
    if(!is.null(sppListCache())){
      sppCache <- sppOnline %>%
        filter(speciesCode %in% sppListCache())
    }else{
      sppCache <- NULL
    }
    
    species_selected <- switch(input$sppDisplay,
                              "speciesCode" = sppCache$speciesCode,
                              "commonName" = sppCache$commonName,
                              "scientificName" = sppCache$scientificName)

    updateSelectizeInput(session, "sppSelect", choices = species_choices, selected = species_selected, server = TRUE)
  })

  observeEvent(input$sppGroup, {
    req(spp_list)
    req(input$sppGroup !="Please select")
    
    sppURL <- sppList(input$versionSel, input$sppDisplay)
    sppOnline <- spp_list %>%
      filter(.data[[input$sppDisplay]] %in% sppURL)
    
    spp_listsub <- sppOnline %>%
      filter(.data[[input$sppGroup]] ==1)
    species_choices <- switch(input$sppDisplay,
                              "speciesCode" = sppOnline$speciesCode,
                              "commonName" = sppOnline$commonName,
                              "scientificName" = sppOnline$scientificName)
    species_selected <- switch(input$sppDisplay,
                              "speciesCode" = spp_listsub$speciesCode,
                              "commonName" = spp_listsub$commonName,
                              "scientificName" = spp_listsub$scientificName)
    updateSelectizeInput(session, "sppSelect", choices = species_choices, selected  = species_selected, server = TRUE)
  })
  
  observeEvent(input$getLayerNM,{
    req(layers$bcr_reactive(), selected_subunits(), input$sppSelect)  # Ensure both are available
    bcr_map <- layers$bcr_reactive()
    pop = ~paste("BCR:", subunit_ui)
    
    req(mapCache() != input$getLayerNM[1])

    #mapCache(input$getLayerNM[1])
    reactiveVals$mapCache(input$getLayerNM[1])
    # show pop-up ...
    showModal(modalDialog(
      title = "Please wait",
      "Processing display",
      easyClose = TRUE,
      footer = NULL))
    
    spp_listsub <- spp_list %>%
      filter(!!sym(input$sppDisplay) %in% input$sppSelect) %>%
      pull(speciesCode)
    #sppListCache(spp_listsub)
    reactiveVals$sppListCache(spp_listsub)
    
    yearSelect <- if(input$versionSel == "v5") input$modYr else NULL
    
    sppMap <- getlayerNM(spp_listsub, input$versionSel, destfile = tempdir(), ext= selected_subunits(), year = yearSelect, lazyload= FALSE, bcrNM=layers$bcr_reactive())
    reactiveVals$sppSelectCache(sppMap)
    sppMap_layer <- lapply(sppMap, function(x) x[[1]])
    #bcrCache(selected_subunits())
    reactiveVals$bcrCache(selected_subunits())
    
    myMapProxy %>%
      clearImages() %>%
      clearControls() %>%
      clearGroup("BCR") %>%
      clearGroup("highlighted") %>%
      removeControl("legend_custom") %>%
      add_species_layers(., sppMap_layer) %>%
      addPolygons(data=bcr_map, color='black', fillColor = "white", fillOpacity = 0.05, weight=2, layerId = bcr_map$subunit_ui, popup = ~subunit_ui, group="BCR", options = leafletOptions(pane = "overlay"))

    removeModal()
    
    # Add band selection in v5 to UI
    if(input$versionSel == "v5"){
      shinyjs::show("band")
    }
    
    # Add species checkbox to UI
    output$speciesboxes <- renderUI({
      req(sppMap)  # Ensure it's not NULL
      
      checkbox_list <- lapply(spp_listsub, function(name) {
        checkboxInput(inputId = ns(name), label = name, value = FALSE)  # Default unchecked
      })
      
      div(class = "checkbox-grid", do.call(tagList, checkbox_list))  # Wrap checkboxes
    })
    
    # Add download button to UI
    shinyjs::show("dwdNMoutput")
  }, ignoreNULL = TRUE)
  
  # Update map when band is selected
  observeEvent(input$band, {
    req(reactiveVals$sppSelectCache())
    req(input$versionSel == "v5")
    
    # Determine which layer to extract based on input$band
    band_index <- switch(input$band,
                          "Mean" = 1,
                          "CV" = 2,
                          "Mean extrapolation" = 3,
                          "Km to nearest point count" = 4,
                           5)   
    # Extract the selected layer for each species map
    sppMap_layer <- lapply(reactiveVals$sppSelectCache(), function(x) x[[band_index]])
    
    # Update the map
    myMapProxy %>%
      clearImages() %>%
      clearControls() %>%
      clearGroup("Species Data") %>%  # Clear the previous band layer
      add_species_layers(., sppMap_layer)  # Add the new selected band layer
  })
  
  
  ###########################################################
  ###########################################################
  # Download
  ###########################################################
  ###########################################################
  output$dwdNMoutput <- downloadHandler(
    filename = function() { "species_tiffs.zip" },
    content = function(file) {
      # Ensure at least one species is selected
      req(reactiveVals$sppSelectCache)
      
      final_spp <- reactive({
         
        # Extract names from the list of SpatRast objects
        spp_names <- names(reactiveVals$sppSelectCache())
        
        # Filter based on checked checkboxes
        selected <- reactiveVals$sppSelectCache()[spp_names %in% names(input) & map_lgl(spp_names, ~ input[[.x]] %||% FALSE)]
        
        selected
      })
      
      # Create a temporary directory
      tiff_files <- c()
      
      for (species in final_spp()) {
        # Store the file path
        tiff_files <- c(tiff_files, terra::sources(species))
      }
      
      # Create a ZIP file
      zip_filename <- file.path(tempdir(), "BAM_NM_output.zip")
      zip(zipfile = zip_filename, files = tiff_files, flags = "-j")  # `-j` removes directory structure
      
      # Copy ZIP file to the provided download location
      file.copy(zip_filename, file, overwrite = TRUE)
    }
  )
  
}
