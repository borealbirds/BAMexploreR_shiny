##################################################################################
#' Retrieve list of species
#'
#' The function produce a character vector of species available to download. To
#' retrieve available species, the function derive the list using the version, and type of output
#'
#' @param version character; Indicate the version of the National Model requested. Each version of the
#'        National Model has its url access provided within the package.
#' @param type character; type of output provided in the list, either \code{"speciesCode"}, \code{"commonName"} or \code{"scientificName"}.Default is \code{"speciesCode"}.
#' @param guild character; Specifies the guild to filter the species list, based on the classification used in The State of Canada’s Birds Report (Birds Canada, 2024).
#'              By providing a guild (e.g., "Forest Birds"), the function will return only the species available within that specific category.
#'              Accepted guild names include \code{"Forest_Birds"}, \code{"Grassland_Birds"}, \code{"Waterfowl"}, and others as defined in the report.
#'
#' @return Vector of species name
#'
#' @importFrom httr GET content
#' @importFrom dplyr pull mutate filter
#' @importFrom stringr str_sub
#' @importFrom tidyselect all_of
#'
#' @docType methods
#' @author Melina Houle
#' @rdname bam_spp_list
#' @export
#' @examples

#' speciesList <- bam_spp_list("v4", "speciesCode")
bam_spp_list <- function(version, type = "speciesCode", guild = NULL) {
  spdt <- spp_tbl
  
  if (!version %in% c("v4", "v5")) {
    stop("Invalid version argument. Must be either 'v4' or 'v5'.")
  }
  
  if (!type %in% c("speciesCode", "commonName", "scientificName")) {
    stop("Invalid type argument. Must be one of 'speciesCode', 'commonName' or 'scientificName'.")
  }
  
  if(is.null(guild)){
    spcode <- spdt %>% dplyr::pull("speciesCode")
  }else if(any(!(guild %in% guild_opt))){
    print("Guild is invalid")
  }else{
    spcode <- spdt %>%
      dplyr::filter(dplyr::if_any(tidyselect::all_of(guild), ~ . == 1)) %>%  # Use if_any to check across multiple columns
      dplyr::pull("speciesCode")  # Extract species code
  }
  
  url <- version.url$url[version.url$version == version]
  response <- httr::GET(url)
  content_text <- httr::content(response, "text")
  if (httr::status_code(response) == 200) {
    if(version == "v4"){
      # Use regular expressions to parse
      tiff_files <- regmatches(content_text, gregexpr('href="([^"]+\\.tif)"', content_text))
      tiff_files <- unlist(tiff_files)
      tiff_files <- gsub('href="|/"', '', tiff_files)
      spList <- tiff_files %>%
        stringr::str_sub(start = 6, end = 9) %>%
        .[.%in%spcode]
    } else if(version == "v5"){
      # Use regular expressions to parse
      subdirs <- regmatches(content_text, gregexpr('href="([^"]+/)"', content_text))
      subdirs <- unlist(subdirs)
      spList <- gsub('href="|/"', '', subdirs) %>%
        .[!(. %in% "/data")]
    } else {
      print("You must specify either v4 or v5")
    }
  } else {
    # Return an error message if the request failed
    return(paste("Error:", httr::status_code(response)))
  }
  
  # Extract species list
  if(type=="speciesCode"){
    sp <- spdt %>%
      dplyr::filter(speciesCode %in% spList,
                    speciesCode %in% spcode) %>%
      dplyr::pull(speciesCode)
  }else if(type=="commonName") {
    sp <- spdt %>%
      dplyr::filter(speciesCode %in% spList,
                    speciesCode %in% spcode) %>%
      dplyr::pull(commonName)
  }else if(type=="scientificName") {
    sp <- spdt %>%
      dplyr::filter(speciesCode %in% spList,
                    speciesCode %in% spcode) %>%
      dplyr::pull(scientificName)
  }
  sp <- unique(sp)
  return(sp)
}


##################################################################################
#' Map the boundaries of BCR subunits for a specified version.
#'
#' @param version A \code{character} specifying which version of the National Model to use. Valid options are "v4" or "v5".
#' @param ext A \code{SpatVector} or a \code{SpatRaster} used to define an area of interest.
#'
#' @return Map illustrating the BCR and overlap extent if provided.
#'
#' @import tmap
#' @importFrom terra vect crs project
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tmap tm_shape tm_polygons tm_layout tm_text tm_add_legend tmap_mode
#' @importFrom sf st_as_sf st_intersects
#' @docType methods
#' @author Melina Houle
#' @rdname bam_map_bcr
#' @export
#' @examples
#' subUnit<- bam_map_bcr("v5")
bam_map_bcr <- function(version, ext = NULL) {
  tmap::tmap_mode("plot")
  
  if (!version %in% c("v4", "v5")) {
    stop("Invalid version argument. Must be either 'v4' or 'v5'.")
  }
  
  # Need SpatVector or SpatRaster and projection
  if (missing(ext)) {
    if(version == "v4"){
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
    }else {
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv5_5072.shp", package = "BAMexploreR"))
    }
  }else {
    if(!inherits(ext, "SpatVector") && !inherits(ext, "SpatRaster")){
      stop("You need to provide a SpatRast or a SpatVect")
    }else{
      if (nchar(terra::crs(ext)) == 0) {
        stop("CRS is missing or empty.")
      }
    }
  }
  
  if(version == "v4"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
    ncat <-16
  }else if(version == "v5"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv5_5072.shp", package = "BAMexploreR"))
    ncat <-33
  }else{
    stop("Model version doesn't exist.")
  }
  
  if (length(terra::intersect(base_bcr, ext)) == 0) {
    warning("The provided extent does not intersect with any BCR sub-units.")
  }
  
  label_sf <- sf::st_as_sf(data.frame(
    X = base_bcr$X,
    Y = base_bcr$Y,
    label = base_bcr$subunit_ui,
    stringsAsFactors = FALSE
  ), coords = c("X", "Y"), crs = terra::crs(base_bcr))
  
  # Ensure both SpatVect objects are in the same CRS
  if (terra::crs(ext) != terra::crs(base_bcr)) {
    ext <- terra::project(ext, terra::crs(base_bcr))
  }
  # Convert SpatVect objects to sf objects for use with tmap
  base_sf <- sf::st_as_sf(base_bcr)
  user_sf <- sf::st_as_sf(ext)
  
  # Create the tmap
  # Generate a larger palette and subset it to get exactly 25 colors
  custom_palette <- RColorBrewer::brewer.pal(12, "Set3")  # Generate 12 colors from the Set3 palette
  custom_palette <- rep(custom_palette, length.out = ncat)  # Repeat the palette to get 25 colors
  
  tmap <- tmap::tm_shape(base_sf) +
    tmap::tm_polygons(fill = "subunit_ui",
                      fill.scale = tm_scale_categorical(values = custom_palette),
                      col = "black", col_alpha = 0.5,
                      fill.legend = NULL,
                      id = "subunit_ui") +
    tmap::tm_add_legend(type = "polygons",  # Updated from "fill"
                        labels = unique(base_sf$subunit_ui),
                        title = "BCR subunit",
                        fill = custom_palette[seq_along(unique(base_sf$subunit_ui))]) +  # Use `fill` instead of `col`
    
    tmap::tm_layout(legend.outside = TRUE, legend.is.portrait = FALSE, legend.stack = "horizontal")
  
  if(!missing(ext)){
    tmap <- tmap +
      tmap::tm_shape(user_sf) +
      tmap::tm_polygons(fill = NA, fill_alpha = 0, col = "black", col_alpha = 1, lwd = 3, fill.legend = NULL) +
      tmap::tm_add_legend(type = "polygons", labels = "User AOI", fill = NA, col = "black") + # Add legend item for user_sf
      tmap::tm_layout(legend.outside = TRUE, legend.stack = "vertical")
  }
  
  # Return the results as a list
  return(tmap)
}

##################################################################################
#' Retrieve the list of BCR overlaid by the study area
#'
#' @param version A \code{character} specifying which version of the BAM landbird model to use. Valid options are "v4" or "v5".
#' @param ext A \code{SpatVector} or \code{SpatRaster} used to define the extent for the cropping. If \code{NULL}, the mosaic
#'  will be used as default.
#'
#' @return Vector of bcr that overlay the study area.
#'
#' @importFrom terra vect crs project
#' @importFrom sf st_as_sf st_intersects
#' @docType methods
#' @author Melina Houle
#' @rdname bam_get_bcr
#' @export
#' @examples
#' subUnit<- bam_get_bcr("v5")
bam_get_bcr <- function(version, ext = NULL) {
  
  if (!version %in% c("v4", "v5")) {
    stop("Invalid version argument. Must be either 'v4' or 'v5'.")
  }
  
  # Need output path
  if (missing(ext)) {
    if(version == "v4"){
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
    }else if (version == "v5"){
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv5_5072.shp", package = "BAMexploreR"))
    }else{
      stop("The version is not recognised by the function. BAM National Models are only available for v4 and v5.")
    }
  }
  
  # Need SpatVector or SpatRaster and projection
  if(!inherits(ext, "SpatVector") && !inherits(ext, "SpatRaster")){
    stop("You need to provide a SpatRast or a SpatVect")
  }else{
    if (nchar(terra::crs(ext)) == 0) {
      stop("CRS is missing or empty.")
    }
  }
  
  if(version == "v4"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
  }else if(version == "v5"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv5_5072.shp", package = "BAMexploreR"))
  }else{
    stop("Model version doesn't exist.")
  }
  
  # Convert SpatVect objects to sf objects for use with tmap
  base_sf <- sf::st_as_sf(base_bcr)
  
  # Find intersections
  if(!missing(ext)){
    # Ensure both SpatVect objects are in the same CRS
    if (terra::crs(ext) != terra::crs(base_bcr)) {
      ext <- terra::project(ext, terra::crs(base_bcr))
    }
    user_sf <- sf::st_as_sf(ext)
    intersected <- sf::st_intersects(base_sf, user_sf, sparse = FALSE)
    intersected_subUnits <- base_sf$subunit_ui[apply(intersected, 1, any)]
  }else{
    intersected_subUnits <-base_sf$subunit_ui
  }
  
  # Return the results as a list
  return(intersected_subUnits)
}

##################################################################################
#' Download BAM species specific Landbird Models density maps based on list of species
#'
#' @param spList A \code{vector} of species to be downloaded.

#' @param version A \code{character} specifying which version of the BAM Landbird Models to use. Valid options are "v4" or "v5".
#'
#' @param destfile A \code{character} indicating output path where the downloaded file is saved.
#'
#' @param crop_ext A \code{SpatVector} or A \code{SpatRaster} used to define the extent for the cropping.
#' Or downloading valid BCR polygons from list, type: \code{bam_map_bcr("v4")} or \code{bam_map_bcr("v5")}
#'
#' @param year A \code{character} specifying the year for which the density map were generated. Only in v5.
#' Valid options are "1985", "1990", "1995", "2000", "2005", "2010", "2015" and "2020". Default value is "2020".
#'
#' @param bcrNM A \code{vector} representing the BCR subunit name according to model version. Default is "mosaic".
#'
#' @return A list of \code{SpatRaster} objects. In addition to returning these objects,
#' the function also downloads raster files to the directory specified by \code{destfile},
#' as a side-effect.
#'
#' @examples
#' bird <- bam_get_layer("TEWA", "v4", tempfile())
#'
#' bird <- bam_get_layer("TEWA", "v4", destfile = tempdir())
#'
#' @author Melina Houle
#' @docType methods
#' @rdname bam_get_layer
#' @export
#'
#' @importFrom dplyr pull
#' @importFrom magrittr %>%
#' @importFrom httr GET content
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom stringr str_sub
#' @importFrom terra vect rast project crop values crs writeRaster same.crs expanse
#' @importFrom stats setNames
#'
bam_get_layer <- function(spList, version, destfile, crop_ext = NULL,  year = NULL, bcrNM= "mosaic") {
  # Valid Model versions
  if (!version %in% c("v4", "v5")) {
    stop("Model version doesn't exist.")
  }
  
  # Need output path
  if (missing(destfile)) {
    stop("You must provide an output path to store downloaded rasters.")
  }
  
  if (is.null(year)){
    if(version == "v5"){
      year <- c("2020")
    }
  }
  
  if (!is.null(bcrNM)){
    if (!is.character(bcrNM)) {
      stop("bcrNM` must be a character vector representing valid BCR codes (e.g., 'can5', 'can80'). You provided an object of class: ", class(bcrNM)[1])
    }
    if(version == "v5"){
      valid_bcrs <- c("mosaic", "can3", "can5", "can9", "can10", "can11", "can12", "can13", "can14",
                      "can40", "can41", "can42", "can60", "can61", "can70", "can71", "can72", "can80",
                      "can81", "can82", "usa2", "usa5", "usa9", "usa10", "usa11", "usa12", "usa13",
                      "usa14", "usa23", "usa28", "usa30", "usa40", "usa43", "usa41423")
      if (!all(bcrNM %in% valid_bcrs)) {
        stop("Invalid bcr value(s) provided: ", paste(setdiff(bcrNM, valid_bcrs), collapse = ", "))
      }
    }else{
      valid_bcrs <- c("mosaic", "can4", "can5", "can9", "can10", "can11", "can12", "can13", "can14",
                      "can60", "can61", "can70", "can71", "can80", "can81", "can82", "can83")
      if (!all(bcrNM %in% valid_bcrs)) {
        stop("Invalid bcr value(s) provided: ", paste(setdiff(bcrNM, valid_bcrs), collapse = ", "))
      }
    }
  }
  
  # Need CRS
  if (!is.null(crop_ext)){
    if(inherits(crop_ext, "SpatVector") || inherits(crop_ext, "SpatRaster") ) {
      if (nchar(crs(crop_ext)) == 0) {
        stop("CRS of crop_ext is missing or empty.")
      }else{
        if (crs(crop_ext, describe = TRUE)$code != 5072) {
          crop_ext <- terra::project(crop_ext, "EPSG:5072")
        }
      }
    }else{
      stop("crop_ext need to be a SpatVector  or a SpatRaster")
    }
  }
  
  #cwd <- getwd()
  if (!file.exists(destfile)) {
    dir.create(destfile, showWarnings = FALSE)
  }
  
  # Check crop_ext area
  if(!is.null(crop_ext)){
    crop_area <- expanse(crop_ext, unit="km")
    if(sum(crop_area) < 100){
      warning(sprintf("The BAM density models are predicted to a resolution of 1 km2. Your area of interest is only %.2f km2. Please consider whether these models are appropriate for your application.", crop_area))
    }
  }
  
  spv <- bam_spp_list(version, "speciesCode")
  
  # Check if provided species list is in the available species codes. Display erroneous
  uspecies <- spList[!spList %in% spv]
  if (length(uspecies) > 0) {
    message("The following species aren't available for processing: ",
            paste(uspecies, collapse = ", "))
  }
  
  # Create valid species vector
  spList <- spList[spList %in% spv]
  
  outList <- list()
  
  batch_download <- function(species_code, version, year = NULL, crop_ext, bcrNM = "mosaic") {
    message("Downloading data for ", species_code, " from version ", version)
    
    # get file name and URL
    get_file_info <- function() {
      if (version == "v4") {
        file_name <- paste0("pred-", species_code, "-CAN-Mean.tif")
        file_url <- file.path(url, file_name)
      } else if (version == "v5") {
        region <- ifelse(length(bcrNM) == 1, bcrNM, "mosaic")
        file_name <- paste0(species_code, "_", region, "_", year, ".tif")
        file_url <- file.path(url, species_code, region, file_name)
      }
      list(name = file_name, url = file_url)
    }
    
    # download raster to a file
    download_raster <- function(file_url, to_temp = TRUE) {
      target_file <- if (to_temp) tempfile(fileext = ".tif") else file.path(destfile, basename(file_url))
      writeBin(content(GET(file_url), "raw"), target_file)
      rast(target_file)
    }
    
    #crop raster to extent
    crop_raster <- function(r, ext) {
      r_proj <- terra::project(ext, r)
      terra::crop(r, r_proj, snap = "near", mask = TRUE)
    }
    
    # Get file info
    url <- version.url$url[version.url$version == version]
    file_info <- get_file_info()
    file_name <- file_info$name
    file_url  <- file_info$url
    
    # create output name
    out_name <- paste0(tools::file_path_sans_ext(file_name), ".tif")
    #browser()
    # Main raster loading
    if (inherits(crop_ext, c("SpatVector", "SpatRaster"))) {
      tiff_data <- download_raster(file_url, to_temp = TRUE)
      
      tiff_data <- if (inherits(crop_ext, "SpatVector")) {
        crop_raster(tiff_data, crop_ext)
      } else {
        crop_raster(tiff_data, project(crop_ext, tiff_data, align_only = TRUE))
      }
      
      out_name <- sub("\\.tif?$", "_clip.tif", file_name)
      
    } else if ("mosaic" %in% bcrNM) {
      tiff_data <- download_raster(file_url, to_temp = (version == "v4"))
      
    } else if(length(bcrNM)>1 || (length(bcrNM) == 1 && version == "v4")){
      tiff_data <- download_raster(file_url, to_temp = (version == "v4"))
      
      extent <- file.path(
        "www", "data","5072",
        ifelse(version == "v4", "BAM_BCRNMv4_5072.shp", "BAM_BCRNMv5_5072.shp")
      ) %>% vect()
      extent <- extent[extent$subunit_ui %in% bcrNM, ]
      tiff_data <- crop_raster(tiff_data, extent)
      
      if (version == "v4"){
        out_name <- paste0(species_code, "-CAN-Mean_BCRclip.tif")
      }else{
        out_name <- paste0(species_code, "-mosaic-", year, "-BCRclip.tif")
      }
    }else{
      tiff_data <- download_raster(file_url, to_temp = TRUE)
    }
    
    if (!terra::same.crs(tiff_data, "EPSG:5072"))
      tiff_data <- terra::project(tiff_data, "EPSG:5072")
    
    terra::writeRaster(tiff_data, file.path(destfile, out_name), overwrite = TRUE)
    return(setNames(list(tiff_data), species_code))
  }
  
  # Perform batch download for species in the list
  for (s in spList) {
    if(version == "v4"){
      outspp <- batch_download(species = s, year = NULL, version = version, crop_ext, bcrNM)
      outList <- append(outList, outspp)
    }else{
      for (y in year) {#v5
        outspp <- batch_download(species = s, year = y, version = version, crop_ext, bcrNM)
        outList <- append(outList, outspp)
      }
    }
  }
  
  #Delete temp file
  temp_file <- tempfile(fileext = ".tif")
  on.exit({
    if (file.exists(temp_file)) file.remove(temp_file)
  })
  # Return the results as a list
  #setwd(cwd)
  return(outList)
  
}
