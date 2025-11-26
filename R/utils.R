add_species_layers <- function(map, sppMap, nameDisplay, versionSel, modYr, band_label, band_index) {
  
  added_layers <- c()
  
  # Loop through species and add raster layers
  for (spp_name in names(sppMap)) {
    
    raster_layer <- sppMap[[spp_name]]  # Extract the mean layer
    repeat {
      if (ncell(raster_layer)*4 < 4e6) break
      raster_layer <- terra::aggregate(raster_layer, fact = 2, fun = mean)
    }
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
    #browser()
    # Define a color palette (same for all species)
    #pal <- colorNumeric(palette = brewer.pal(9, "YlGnBu"), domain =  values(raster_layer[[1]]), na.color = "transparent")
    
    
    #Option 1
    #vals <- values(raster_layer)
    #vals <- vals[!is.na(vals)]
    
    # Define your custom palette
    #my_colors <- c(
    #  '#f9ffaf', '#edef5c', '#bbdf5f', '#61c074', 
    #  '#34af7c', '#008c80', '#007a7c', '#255668'
    #)
    
    # Compute range and breaks
    #rng <- range(vals, na.rm = TRUE)
    #breaks <- seq(rng[1], rng[2], length.out = length(my_colors) + 1)
    #
    #1pal <- colorBin(
    #  palette = my_colors,
    #  domain = vals,
    #  bins = breaks,
    #  na.color = "transparent"
    #)
    
    #Option 2
    raster_layer[raster_layer ==0] <- NA
    vals <- values(raster_layer)
    rng_trim <- quantile(vals, probs = c(0.0025, 0.9975), na.rm = TRUE)
    my_colors <- c(
      '#f9ffaf', '#edef5c', '#bbdf5f', '#61c074', 
      '#34af7c', '#008c80', '#007a7c', '#255668'
    )
    pal <- colorBin(
      palette = my_colors,
      domain = vals,
      bins = seq(rng_trim[1], rng_trim[2], length.out = length(my_colors) + 1),
      na.color = "transparent"
    )
    # get extent after crop
    r_ext <- ext(raster_layer)
    
    raster_layer[raster_layer < rng_trim[1] | raster_layer > rng_trim[2]] <- NA
    map <- map %>%
      addRasterImage(raster_layer, colors = pal, group = spp_name, layerId = spp_name) %>%
      #addRasterImage(raster_layer, colors = pal, group = spp_name, layerId = spp_name) %>%
      addLayersControl(position = "topright",
                       baseGroups = c(added_layers, spp_name),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      fitBounds(lng1 = xmin(r_ext*0.8), lat1 = ymin(r_ext*0.8), lng2 = xmax(r_ext*0.8), lat2 = ymax(r_ext*0.8))
    
    #Track layer order (latest added on top)
    added_layers <- c(added_layers, spp_name)
  }

  return(map)
}