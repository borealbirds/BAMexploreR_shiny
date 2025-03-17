add_species_layers <- function(map, sppMap) {

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
    
    map <- map %>%
      addRasterImage(raster_layer, colors = pal, group = spp_name, layerId = spp_name) %>%
      addLayersControl(position = "topright",
                       baseGroups = c(spp_name, added_layers),
                       options = layersControlOptions(collapsed = FALSE))
    
    #Track layer order (latest added on top)
    added_layers <- c(spp_name, added_layers)
  }
  #browser()
  # Add legend once (after all species layers are added)
  map <- map %>%
    setView(lng = mean(c(bbox_vals[1], bbox_vals[2])), lat = mean(c(bbox_vals[3], bbox_vals[4])), zoom = 5)
  
  # Add legend once (after all species layers are added)
  map <- map %>%
    addLegend(pal = pal, values = unlist(lapply(sppMap, function(x) values(x[[1]]))),
              title = "Species Abundance", position = "bottomright", opacity = 1)
  
#  # Add layer control
#  map <- map %>%
#    addLayersControl(position = "topright",
#                     #overlayGroups = c("TEWA", "MOBL", "BHVI", "BBWO"),
#                     overlayGroups = layer_order,
#                     options = layersControlOptions(collapsed = FALSE)) %>%
#    leaflet::groupOptions(layer_order)  # Enforce correct drawing order
  
  return(map)
}