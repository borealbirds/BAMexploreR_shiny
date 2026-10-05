# Define a module to manage the reactive values
reactiveLayersModule <- function(input, output, session, layers) {
  layers <- reactiveValues(bcr_reactive = reactiveVal(NULL))

  return(layers)
}


