predUI <- function(id, opt) {
  ns <- NS(id)
  fluidPage(
    tags$style(HTML(sprintf("
      #%s {
        color: white !important;;
        font-size: 14px;
      }
    ", ns("md1_text")))),
    br(), 
    radioButtons(ns("predAnalysis"), "Select the summary type:",
                 choices = c("Dot-and-whisker" = "predImpo",
                             "Barplot" = "predChart"), selected = "predImpo"),
    selectInput(ns("versionSelPred"), "Choose version:", choices = c("Version 4" ="v4", "Version 5" = "v5"), selected = "v5"),
    radioButtons(ns("sppDisplayPred"), "Display species using: ",
                 choices = c("Species Code" = "speciesCode",
                             "Common Name" = "commonName",
                             "Scientific Name" = "scientificName"),
                 selected = "commonName", inline = TRUE),
    selectizeInput(ns("sppPred"), "Select a species:", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Start typing to search...",maxOptions = 999, closeOnSelect = FALSE)),
    div(style = "color: white !important; margin-top: 20px; font-size:13px;", "Select the model extent:"),
    uiOutput(ns("bcrCheckboxesPred"))
  )
}

barchartUI <- function(id) {
  ns <- NS(id)
  
  tagList(
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
                  selected = 'predictor')

    ),
    # Below the grouping choices so the user sets them before plotting
    div(style = "margin-top: 20px;",
        actionButton(ns("getPlot"), "Visualize results", icon = icon(name = "fas fa-crow", lib = "font-awesome"), style="width:250px"))
  )
}

predDwdUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$style(type="text/css", "#downloadData {background-color:white;color: black}"),
    div(style = "margin-top: 40px;",
        downloadButton(ns("predDwdoutput"), "Download selected summaries"),
    )
  )
}

predSERVER <- function(input, output, session, spp_list, layers, myMapProxy, reactiveVals) {
  
  ns <- session$ns
  
  sppMapname <- reactiveVals$inserted_ids
  bcrCache <- reactiveVals$bcrCache
  sppNames <- sppMapname()
  bcr <- reactiveVals$bcrPred

  observeEvent(input$versionSelPred,{
    
    data <- .load_predictor_importance(input$versionSelPred)
    available_spp <- unique(data$spp)
    allSpp <- spp_tbl %>%
      filter(speciesCode %in% available_spp) %>%
      pull(!!sym(input$sppDisplayPred))
    updateSelectizeInput(session, "sppPred", choices = allSpp, server = TRUE)
    
    bcr_list <- if (input$versionSelPred == "v5") bcrv5.map$bcr else bcrv4.map$bcr
    
    reactiveVals$bcrPred(bcr_list)
  })

  # All BCRs of the version are listed; once species are selected, BCRs not available for them are disabled
  output$bcrCheckboxesPred <- renderUI({
    req(bcr())

    # birdlist only holds v5 BCRs, so availability filtering applies to v5 only
    if (length(input$sppPred) > 0 && input$versionSelPred == "v5") {
      sppSelect <- spp_list %>%
        filter(!!sym(input$sppDisplayPred) %in% input$sppPred) %>%
        pull(speciesCode)
      sppSelect <- intersect(sppSelect, names(birdlist))
      valid_bcr <- birdlist$bcr[apply(birdlist[, sppSelect, drop = FALSE], 1, all)]
    } else {
      valid_bcr <- bcr()
    }

    checkbox_list <- lapply(bcr(), function(name) {
      cb <- checkboxInput(
        inputId = ns(name),
        label = name,
        value = isTRUE(isolate(input[[name]])) && name %in% valid_bcr  # keep ticks that are still valid
      )

      # Disable if species/BCR combination is FALSE
      if (!name %in% valid_bcr) {
        cb <- shinyjs::disabled(cb)
        cb <- shiny::tagAppendAttributes(cb, class = "disabled-bcr")
      }
      cb
    })

    div(class = "checkbox-grid", do.call(tagList, checkbox_list))
  })
  
  selected_bcr <- reactive({
    req(bcr())  
    bcr()[map_lgl(bcr(), ~ input[[.x]] %||% FALSE)]
  })
  
  #####################################
  ## Plot, built only when "Visualize results" is clicked
  pred_plot <- reactiveVal(NULL)

  output$predbarchart <- renderPlot({
    req(pred_plot())
    pred_plot()
  })

  show_notice <- function(title, msg) {
    showModal(modalDialog(title = title, msg, easyClose = TRUE, footer = modalButton("OK")))
  }

  observeEvent(input$getPlot,{
    if (length(input$sppPred) == 0) {
      show_notice("No species selected", "Please select at least one species.")
      return()
    }

    species_name <- spp_tbl %>%
      filter(!!sym(input$sppDisplayPred) %in% input$sppPred) %>%
      pull(speciesCode)

    if (input$predAnalysis == "predChart") {
      # bam_predictor_barchart() needs one of spp/bcr paired with one of predictor/predictor_class
      axes <- c(input$Xgroup, input$Ygroup)
      msg <- if (axes[1] == axes[2]) {
        "X and Y axes use the same grouping. Please choose two different groupings."
      } else if (all(c("spp", "bcr") %in% axes)) {
        "Species and BCR cannot be combined: relative influence is normalised within each species x BCR model. Pair Species or BCR with Predictor or Predictor class."
      } else if (all(c("predictor", "predictor_class") %in% axes)) {
        "Predictor and Predictor class cannot be combined: each predictor belongs to a single class. Pair one of them with Species or BCR."
      }
      if (!is.null(msg)) {
        show_notice("Invalid axis combination", msg)
        return()
      }
    }

    # Any other error from the functions is shown in a modal rather than in the plot area
    p <- tryCatch({
      if (input$predAnalysis == "predImpo") {
        bam_predictor_importance(species = species_name, bcr = selected_bcr(), group = input$group, version = input$versionSelPred, plot = TRUE) +
          ggtitle(paste("Predictor importance using ", input$group))
      } else {
        bam_predictor_barchart(species = species_name, bcr = selected_bcr(), groups = c(input$Xgroup, input$Ygroup), version = input$versionSelPred, plot = TRUE) +
          ggtitle(paste("Proportion of model predictors importance using ", input$Xgroup, " and ", input$Ygroup))
      }
    }, error = function(e) {
      show_notice("Unable to build the plot", conditionMessage(e))
      NULL
    })

    req(p)
    pred_plot(p + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))
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
  