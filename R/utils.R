add_species_layers <- function(map, sppMap, nameDisplay) {

  # Define a color palette (same for all species)
  pal <- colorNumeric(palette = brewer.pal(9, "YlGnBu"), domain = unlist(lapply(sppMap, function(x) values(x[[1]]))), na.color = "transparent")
  bbox_vals <- NULL
  added_layers <- c()
  
  # Loop through species and add raster layers
  for (spp_name in names(sppMap)) {
    raster_layer <- sppMap[[spp_name]]  # Extract the mean layer
    raster_layer <- terra::project(raster_layer, "EPSG:4326")  # Reproject to WGS84
    
    if (is.null(bbox_vals)) {  # Initialize bounding box
      bbox <- terra::ext(raster_layer)
      bbox_vals <- as.vector(bbox)
    }
    
    sp_name <- spp_list %>%
      filter(speciesCode == spp_name) %>%
      pull(!!sym(nameDisplay))
    
    map <- map %>%
      addRasterImage(raster_layer, colors = pal, group = sp_name, layerId = sp_name) %>%
      addLayersControl(position = "topright",
                       baseGroups = c(added_layers, sp_name),
                       options = layersControlOptions(collapsed = FALSE))
    
    #Track layer order (latest added on top)
    added_layers <- c(added_layers, sp_name)
  }
  #browser()
  # Add legend once (after all species layers are added)
  map <- map %>%
    setView(lng = mean(c(bbox_vals[1], bbox_vals[2])), lat = mean(c(bbox_vals[3], bbox_vals[4])), zoom = 5)
  
  # Add legend once (after all species layers are added)
  map <- map %>%
    addLegend(pal = pal, values = unlist(lapply(sppMap, function(x) values(x[[1]]))),
              title = "Species Abundance", position = "bottomright", opacity = 1)
  
  return(map)
}