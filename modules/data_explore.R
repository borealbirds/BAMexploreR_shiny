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
                                 maxItems = 12,
                                 maxOptions = 999, closeOnSelect = FALSE)),
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
  div(style = "margin-top: 40px;", hidden(selectInput(ns("band"), label = "Select band *:", 
              choices = c("mean", "coefficient of variation"), 
              selected = "mean"
  )))
}

#dwdUI  <- function(id) {
#  ns <- NS(id)
#  tags$style(type="text/css", "#downloadData {background-color:white;color: black}")
#  div(style = "margin-top: 40px;", hidden(downloadButton(ns("dwdNMoutput"), "Download species map")),
#      tags$div(
#        style = "font-size: 0.85em; color: white !important; font-weight: normal;",
        
        # Title in italics
#        tags$br(),
#        tags$br(),
#        tags$em("* Band definition"),
        
        # Definitions
#        tags$p(
#          tags$strong("mean"),
#          " are regions of the study area ...",
#          tags$br(),
#          tags$strong("coefficient of variation"),
#          " are regions within ..."
#        )
#      )
#  )
#}

dwdUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$style(type="text/css", "#downloadData {background-color:white;color: black}"),
    div(style = "margin-top: 40px;",
        hidden(downloadButton(ns("dwdNMoutput"), "Download species map")),
        
        # Wrap your text in an id so we can show it later
        hidden(
          div(
            id = ns("bandDef"),
            style = "font-size: 0.85em; color: white !important; font-weight: normal;",
            tags$br(),
            tags$br(),
            tags$em("* Band definition"),
            tags$p(
              tags$strong("mean"),
              " is mean density prediction across 32 independent model runs measured in male birds per hectare",
              tags$br(),
              tags$strong("coefficient of variation"),
              " is the variation in density across model runs"
            )
          )
        )
    )
  )
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
  inserted_ids <- reactiveVals$inserted_ids 
  
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
    #req(mapCache() != input$getLayerNM[1])
    
    bcr_map <- layers$bcr_reactive() 
    selected_units <- subset(bcr_map, bcr_map$subunit_ui %in% selected_subunits())
    
    if(input$mosaic){
      selected_units <- bcr_map
    }
    
    bbox_vals <- terra::ext(bcr_map)
    map_bounds <- c(bbox_vals$xmin, bbox_vals$ymin, bbox_vals$xmax, bbox_vals$ymax)
    
    myMapProxy %>% 
      clearGroup("highlighted") %>%  # Clear only highlighted group
      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>%
      addPolygons(data = selected_units, fillColor = "red", fillOpacity = 0.7, weight = 2, color = "black", group = "highlighted", options = leafletOptions(pane = "overlay"))
  })
  
  # Update the Leaflet map when bcr_reactive() changes
  observe({
    req(spp_list)
    req(input$sppDisplay)
    
    sppURL <- bam_spp_list(input$versionSel, input$sppDisplay)
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
    
    sppURL <- bam_spp_list(input$versionSel, input$sppDisplay)
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
    if(is.null(selected_subunits()) || is.null(input$sppSelect)){
        # show pop-up ...
        showModal(modalDialog(
          title = "You must select both a species and a data extent before proceeding.",
          easyClose = TRUE,
          footer = modalButton("OK")))
      }
      
    req(layers$bcr_reactive(), selected_subunits(), input$sppSelect)  # Ensure both are available
  
    bcr_map <- layers$bcr_reactive() %>% st_as_sf()
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
    
    sppMap <- bam_get_layer(spp_listsub, input$versionSel, destfile = tempdir(), crop_ext= NULL, year = yearSelect, bcrNM=selected_subunits())
    reactiveVals$sppSelectCache(sppMap)
    sppMap_layer <- lapply(sppMap, function(x) x[[1]])
    reactiveVals$bcrCache(selected_subunits())
    
    bbox_vals <- terra::ext(bcr_map)
    map_bounds <- c(bbox_vals$xmin, bbox_vals$ymin, bbox_vals$xmax, bbox_vals$ymax)
    
    myMapProxy %>%
      #clearImages() %>%
      clearControls() %>%
      clearGroup("BCR") %>%
      clearGroup("highlighted") %>%
      removeControl("legend_custom") %>%
      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>%
      add_species_layers(., sppMap_layer, input$sppDisplay, input$versionSel, input$modYr, input$band, band_index)  %>%
      addPolygons(data=bcr_map, color='black', fillColor = "white", fillOpacity = 0.05, weight=2, layerId = bcr_map$subunit_ui, popup = ~subunit_ui, group="BCR", options = leafletOptions(pane = "overlay"))

    removeModal()
    
    # Add band selection in v5 to UI
    if(input$versionSel == "v5"){
      shinyjs::show("band")
    }
    
    new_ids <- vapply(input$sppSelect, function(name) {
      if (input$versionSel == "v5") {
        paste(name, input$versionSel, input$modYr, sep = "_")
      } else {
        paste(name, input$versionSel, sep = "_")
      }
    }, character(1))
    
    # Add new_ids
    updated_ids <- unique(c(inserted_ids(), new_ids))
    reactiveVals$inserted_ids(updated_ids)
    
    output$speciesboxes <- renderUI({
      req(reactiveVals$inserted_ids())
      
      checkbox_list <- lapply(reactiveVals$inserted_ids(), function(id) {
        checkboxInput(inputId = ns(id),
                      label = tags$span(class = "dynamic-checkbox-label", id),
                      value = FALSE)
      })
      
      tagList(checkbox_list)
    })
   
    # Add download button to UI
    shinyjs::show("dwdNMoutput")
    shinyjs::show("bandDef") 
  }, ignoreNULL = TRUE)
  
  # Update map when band is selected
  observeEvent(input$band, {
    req(reactiveVals$sppSelectCache())
    req(input$versionSel == "v5")
    
    # Determine which layer to extract based on input$band
    band_index <- switch(input$band,
                          "mean" = 1,
                          2)   
    # Extract the selected layer for each species map
    sppMap_layer <- lapply(reactiveVals$sppSelectCache(), function(x) x[[band_index]])
    # Update the map
    myMapProxy %>%
      clearImages() %>%
      clearControls() %>%
      clearGroup("Species Data") %>%  # Clear the previous band layer
      add_species_layers(., sppMap_layer, input$sppDisplay, input$versionSel, input$modYr, input$band, band_index)  # Add the new selected band layer
  })
  
  observe({
    spp_names <- names(reactiveVals$sppSelectCache())
    species_choices <- switch(
      input$sppDisplay,
      "speciesCode"   = spp_list$speciesCode,
      "commonName"    = spp_list$commonName,
      "scientificName"= spp_list$scientificName
    )
    display_names <- if (input$versionSel == "v5")
      paste(species_choices[match(spp_names, spp_list$speciesCode)], input$versionSel, input$modYr, sep = "_")
    else
      paste(species_choices[match(spp_names, spp_list$speciesCode)], input$versionSel, sep = "_")
    
    selected <- reactiveVals$sppSelectCache()[display_names %in% names(input) & 
                                                purrr::map_lgl(display_names, ~ input[[.x]] %||% FALSE)]
    
    if (length(selected) == 0) {
      shinyjs::disable("dwdNMoutput")  # Disable button
    } else {
      shinyjs::enable("dwdNMoutput")   # Enable button
    }
  })
  
  ###########################################################
  ###########################################################
  # Download
  ###########################################################
  ###########################################################
  output$dwdNMoutput <- downloadHandler(
    filename = function() { "BAM_NM_output.zip" },
    content = function(file) {
      # Get selected species (checkboxes checked)
      spp_names <- names(reactiveVals$sppSelectCache())
      species_choices <- switch(
        input$sppDisplay,
        "speciesCode"   = spp_list$speciesCode,
        "commonName"    = spp_list$commonName,
        "scientificName"= spp_list$scientificName
      )
      display_names <- if (input$versionSel == "v5")
        paste(species_choices[match(spp_names, spp_list$speciesCode)], input$versionSel, input$modYr, sep = "_")
      else
        paste(species_choices[match(spp_names, spp_list$speciesCode)], input$versionSel, sep = "_")
      
      selected <- reactiveVals$sppSelectCache()[display_names %in% names(input) &
                                                  purrr::map_lgl(display_names, ~ input[[.x]] %||% FALSE)]
      
      # ---- Proceed with download ----
      tiff_files <- c()
      
      for (species_code in names(selected)) {
        raster_obj <- selected[[species_code]]
        
        display_names <- if (input$versionSel == "v5")
          paste(species_code, input$versionSel, input$modYr, sep = "_")
        else
          paste(species_code, input$versionSel, sep = "_")
        
        tiff_path <- file.path(tempdir(), paste0(display_names, ".tif"))
        writeRaster(raster_obj, tiff_path, overwrite = TRUE)
        tiff_files <- c(tiff_files, tiff_path)
      }
      
      # Create a ZIP file
      zip_filename <- file.path(tempdir(), "BAM_NM_output.zip")
      zip(zipfile = zip_filename, files = tiff_files, flags = "-j")
      
      # Copy ZIP file to chosen location
      file.copy(zip_filename, file, overwrite = TRUE)
      
      # Clean up temporary files
      unlink(c(tiff_files, zip_filename), force = TRUE)
    }
  )
}
