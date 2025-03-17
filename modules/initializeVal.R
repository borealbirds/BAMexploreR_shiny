# Define a module to manage the reactive values
reactiveLayersModule <- function(input, output, session, layers) {
  layers <- reactiveValues(bcr_reactive = reactiveVal(NULL),
                           selected_bcr = reactiveVal(NULL),
                           version_reactive = reactiveVal("v5"),
                           spp_reactive = reactiveVal(NULL)
  )

  return(layers)
}


