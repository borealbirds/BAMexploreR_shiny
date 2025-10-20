add_species_layers <- function(map, sppMap, nameDisplay, versionSel, modYr, band_label, band_index) {
  
  # Define a color palette (same for all species)
  pal <- colorNumeric(palette = brewer.pal(9, "YlGnBu"), domain = unlist(lapply(sppMap, function(x) values(x[[1]]))), na.color = "transparent")
  added_layers <- c()
  
  # Loop through species and add raster layers
  for (spp_name in names(sppMap)) {
    raster_layer <- sppMap[[spp_name]]  # Extract the mean layer
    raster_layer <- terra::project(raster_layer, "EPSG:4326")  # Reproject to WGS84

    target_extent <- ext(-177.9919, 30.65155, -18.12997, 81.60892)
    r_ext <- ext(raster_layer)
    if (xmin(r_ext) < xmin(target_extent) ||
        xmax(r_ext) > xmax(target_extent) ||
        ymin(r_ext) < ymin(target_extent) ||
        ymax(r_ext) > ymax(target_extent)) {
      
      # Crop raster
      raster_layer <- crop(raster_layer, target_extent)
    } 
    
    # get extent after crop
    r_ext <- ext(raster_layer)
    
    options(leaflet.maxbytes = 1e8)  

    map <- map %>%
      addRasterImage(raster_layer, colors = pal, group = spp_name, layerId = spp_name) %>%
      addLayersControl(position = "topright",
                       baseGroups = c(added_layers, spp_name),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      fitBounds(lng1 = xmin(r_ext*0.8), lat1 = ymin(r_ext*0.8), lng2 = xmax(r_ext*0.8), lat2 = ymax(r_ext*0.8))
    
    #Track layer order (latest added on top)
    added_layers <- c(added_layers, spp_name)
  }

  # Dynamically set legend title
  legend_title <- switch(
    band_label,
    "mean" = "Mean Density (males/ha)",
    "coefficient of variation" = "Variation in density",
    band_label
  )
  
  # Add legend once (after all species layers are added)
  map <- map %>%
    addLegend(
      pal = pal,
      values = unlist(lapply(sppMap, function(x) values(x))),
      title = legend_title,
      position = "bottomright",
      opacity = 1
    )
  
  return(map)
}