##################################################################################
#' Retrieve list of species
#'
#' The function produce a character vector of species available to download. To
#' retrieve available species, the function derive the list using the version, and type of output
#'
#' @param version character; Indicate the version of the National Model requested. Each version of the
#'        National Model has its url access provided within the package.
#' @param type character; type of output provided in the list, either "speciesCode", "commonName" or "scientificName".
#' @param guild character; Specifies the guild to filter the species list, based on the classification used in The State of Canada’s Birds Report (Birds Canada, 2024).
#'              By providing a guild (e.g., "Forest Birds"), the function will return only the species available within that specific category.
#'              Accepted guild names include "Forest_Birds", "Grassland_Birds", "Waterfowl", and others as defined in the report.
#'
#' @return Vector of species name
#'
#' @importFrom httr GET content
#' @importFrom dplyr pull mutate filter
#' @importFrom stringr str_sub
#' @docType methods
#' @author Melina Houle
#' @rdname sppList
#' @export
#' @examples

#' speciesList <- sppList("v4", "mean", "species_code", guild)
sppList <- function(version, type, guild = NULL) {
  spdt <- spp_List
  
  if(is.null(guild)){
    spcode <- spdt %>% dplyr::pull("speciesCode")
  }else if(any(!(guild %in% guild_opt))){
    print("Guild is invalid")
  }else{
    spcode <- spdt %>%
      dplyr::filter(if_any(all_of(guild), ~ . == 1)) %>%  # Use if_any to check across multiple columns
      dplyr::pull("speciesCode")  # Extract species code
  }
  
  url <- version.url$url[version.url$version == version]
  response <- httr::GET(url)
  content_text <- httr::content(response, "text")
  if (httr::status_code(response) == 200) {
    if(version == "v4" || version == "v4_demo"){
      # Use regular expressions to parse
      tiff_files <- regmatches(content_text, gregexpr('href="([^"]+\\.tif)"', content_text))
      tiff_files <- unlist(tiff_files)
      tiff_files <- gsub('href="|/"', '', tiff_files)
      spList <- tiff_files %>%
        stringr::str_sub(start = 6, end = 9) %>%
        .[.%in%spcode]
      #return(spList)
    } else if(version == "v5" || version == "v5_demo"){
      # Use regular expressions to parse
      subdirs <- regmatches(content_text, gregexpr('href="([^"]+/)"', content_text))
      subdirs <- unlist(subdirs)
      spList <- gsub('href="|/"', '', subdirs) %>%
        .[!(. %in% "/data")]
      #return(spList)
    } else {
      print("You must specified either v4 or v5")
    }
  } else {
    # Return an error message if the request failed
    return(paste("Error:", httr::status_code(response)))
  }
  
  # Extract species list
  if(type=="speciesCode"){
    sp <-spList
  }else if(type=="commonName") {
    sp <- spdt %>%
      dplyr::filter(speciesCode %in% spList) %>%
      dplyr::pull(commonName)
  }else if(type=="scientificName") {
    sp <- spdt %>%
      dplyr::filter(speciesCode %in% spList) %>%
      dplyr::pull(scientificName)
  }
  sp <- unique(sp)
  return(sp)
}


##################################################################################
#' Retrieve list of BCR overlaid by the study area
#'
#' @param version character; Indicate the version of the National Model requested. Each version of the
#'        National Model has its url access provided within the package.
#' @param ext SpatVector, SpatExtent, or SpatRaster used to define the extent for the cropping.
#'
#' @return List containing vector of bcr that overlay the study area and a map illustrating the overlap.
#'
#' @import tmap
#' @importFrom terra vect crs project
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tmap tm_shape tm_polygons tm_layout tm_text tm_add_legend tmap_mode
#' @importFrom sf st_as_sf st_intersects
#' @docType methods
#' @author Melina Houle
#' @rdname mapBCR
#' @export
#' @examples
#' subUnit<- mapBCR("v5")
mapBCR <- function(version, ext) {
  add_sf <- TRUE
  tmap::tmap_mode("plot")
  # Need output path
  if (missing(version)) {
    stop("You must specified either v4 or v5")
  }

  # Need output path
  if (missing(ext)) {
    if(version == "v4" || version == "v4_demo" ){
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR"))
      add_sf <- FALSE
    }else if (version == "v5" || version == "v5_demo" ){
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv5_LAEA.shp", package = "BAMexploreR"))
      add_sf <- FALSE
    }else{
      stop("The version is not recognised by the function. BAM National Models are only available for v4 and v5.")
    }
  }

  # Need SpatVector or SpatRaster and projection
  if(!class(ext)[1] %in% c("SpatVector", "SpatRaster")){
    stop("You need to provide a SpatRast or a SpatVect")
  }else{
    if (nchar(terra::crs(ext)) == 0) {
      stop("CRS is missing or empty.")
    }
  }

  if(version == "v4" || version == "v4_demo"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR"))
    ncat <-16
  }else if(version == "v5" || version == "v5_demo"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv5_LAEA.shp", package = "BAMexploreR"))
    ncat <-32
  }else{
    stop("Model version doesn't exist.")
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

  # Find intersections
  if(add_sf){
    intersected <- sf::st_intersects(base_sf, user_sf, sparse = FALSE)
    intersected_subUnits <- base_sf$subunit_ui[apply(intersected, 1, any)]
  }else{
    intersected_subUnits <-base_sf$subunit_ui
  }
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

  if(add_sf){
    tmap <- tmap +
      tmap::tm_shape(user_sf) +
      tmap::tm_polygons(fill = NA, fill_alpha = 0, col = "red", col_alpha = 1, lwd = 2, fill.legend = NULL) +
      tmap::tm_add_legend(type = "polygons", labels = "User AOI", fill = NA, col = "red") + # Add legend item for user_sf
      tmap::tm_layout(legend.outside = TRUE, legend.stack = "vertical")
  }

  # Return the results as a list
  return(list(subUnits = intersected_subUnits, map = tmap))
}


##################################################################################
#' Download National Model species specific output map raster based on list of species
#'
#' @param spList character. A vector of species to be downloaded.

#' @param version character. Represents the version of the national model output to be downloaded.
#'
#' @param destfile character. Indicate output path where the downloaded file is saved.
#'
#' @param ext SpatVector, SpatExtent, or SpatRaster used to define the extent for the cropping.
#' Or downloading valid BCR polygons from list, type: mapBCR("v4") or mapBCR("v5")
#'
#' @param year character; Specify the year for which the density map were generated. Only in v5.
#'
#' @param lazyload logical; Use the object provided in the package. Default is TRUE.
#'
#' @param bcrNM spatVect; spatVect representing the BCR.

#' @return Invoked for its side-effect of downloading files to the \code{destfile/} directory.
#'
#' @importFrom dplyr pull
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom stringr str_sub
#' @importFrom terra vect rast project crop values
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname getlayerNM
#' @examples
#' bird <- getlayerNM("BAOR", "v4", "mean",  tempfile())
#'
#' bird <- getlayerNM("BAOR", "v4", destfile = tempdir(), layer = "mean", crop = FALSE)
#'
#' bird <- getlayerNM("BAOR", "v4", destfile = ".", "mean", crop = FALSE, ext = NULL)
#'
#'
getlayerNM <- function(spList, version, destfile, ext = NULL,  year = NULL, lazyload = TRUE, bcrNM= NULL) {
  # Valid Model versions
  
  if (!version %in% c("v4", "v4_demo", "v5", "v5_demo")) {
    stop("Model version doesn't exist.")
  }
  
  # Need output path
  if (missing(destfile)) {
    stop("You must provide an output path to store downloaded rasters.")
  }
  
  if (is.null(year)){
    if(version == "v5" || version == "v5_demo"){
      year <- c("2020")
    }
  }
  # Need CRS
  if (!is.null(ext)){
    if(class(ext) == "SpatVect" | class(ext) == "SpatRast") {
      if (nchar(crs(ext)) == 0) {
        stop("CRS of cropping element is missing or empty.")
      }
    } else if(class(ext) == "character"){
      if(length(ext)>1){
        if(lazyload){
          if(version == "v4" || version == "v4_demo"){
            base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR"))
          }else{
            base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv5_LAEA.shp", package = "BAMexploreR"))
          }
        }else{
          base_bcr <- bcrNM
        }
        sel_bcr <- subset(base_bcr, base_bcr$subunit_ui %in% ext)
        ext <- terra::union(sel_bcr)
      }
    }else{
      ext <- ext
    }
  }
  
  cwd <- getwd()
  if (!file.exists(destfile)) {
    dir.create(destfile, showWarnings = FALSE)
  }
  
  if (!missing(destfile)) {
    setwd(destfile)
    on.exit(setwd(cwd))
  }
  
  # Set url based on version
  url <- version.url$url[version.url$version == version]
  response <- httr::GET(url)
  content_text <- httr::content(response, "text")
  # Create list of available species
  if(version == "v4" || version == "v4_demo"){
    tiff_files <- regmatches(content_text, gregexpr('href="([^"]+\\.tif)"', content_text))
    tiff_files <- unlist(tiff_files)
    tiff_files <- gsub('href="|/"', '', tiff_files)
    spv <- tiff_files %>%
      stringr::str_sub(start = 6, end = 9)
  }else if(version == "v5" || version == "v5_demo"){
    subdirs <- regmatches(content_text, gregexpr('href="([^"]+/)"', content_text))
    subdirs <- unlist(subdirs)
    spv <- gsub('href="|/"', '', subdirs) %>%
      .[!(. %in% "/data")]
    flevel1 <- paste0(subdirs, "/")
  }
  
  # Check if provided species list is in the available species codes. Display erroneous
  uspecies <- spList[!spList %in% spv]
  if (length(uspecies) > 0) {
    tryCatch({
      warning(paste0("The following species aren't available for processing: ", paste(uspecies, collapse = ", ")))
    }, warning = function(w) {
      message(w$message)
    })
  }
  
  # Create valid species vector
  spList <- spList[spList %in% spv]
  
  outList <- list()
  # Batch download function
  batch_download <- function(species_code, version, ext) {
    cat(paste0("Downloading data for ", species_code, " from version ", version), "\n")
    if(version=="v4_demo"){
      version<- "v4"
    }else if(version=="v5_demo"){
      version<- "v5"
    }
    
    if(class(ext)[1]=="SpatVector" || class(ext)[1]=="SpatRaster"|| is.null(ext)){
      temp_file <- tempfile(fileext = ".tif")
      if(version == "v5"){
        region <- "mosaic"
        # Create a temporary file
        file_name <- paste0(species_code, "_", region, "_", year, ".tiff")
        file_url <- file.path(url, species_code, region, file_name)
      } else if(version == "v4"){
        file_name <- paste0("pred-", species_code, "-CAN-Mean.tif")
        file_url <- file.path(url, file_name)
      }
      writeBin(content(GET(file_url), "raw"), temp_file)
      tiff_data <- rast(temp_file)
      
      if(class(ext)[1] == "SpatVector"){
        tiff_data <- tiff_data %>%
          crop(project(ext, tiff_data), snap="near", mask=TRUE)
        if(tools::file_ext(file_name) == "tif"){
          out_name <- sub("(\\.tif)$", "_clip\\1", file_name)
        }else{
          out_name <- sub("(\\.tiff)$", "_clip\\1", file_name)
        }
      }else if(class(ext)[1] == "SpatRaster"){
        tiff_data <- tiff_data %>%
          crop(project(ext, tiff_data, align_only = TRUE), snap="near", mask=TRUE)
        out_name <- sub("(\\.tif)$", "_clip\\1", file_name)
      }else{
        out_name <- paste0(species_code, "_", region, "_", year, ".tiff")
      }
      writeRaster(tiff_data, file.path(destfile, out_name), overwrite=TRUE)
      outList <- c(outList, setNames(list(tiff_data), species_code))
      
      # Delete the temporary file
      file.remove(temp_file)
      rm(tiff_data)
      return(outList)
    }else if(class(ext) == "character"){
      if(version == "v4"){
        extent <- system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR")  %>%
          vect()
        extent <- subset(extent, extent$subunit_ui == ext)
        
        # Create a temporary file
        file_name <- paste0("pred-", species_code, "-CAN-Mean.tif")
        file_url <- paste(url, file_name, sep= "/")
        temp_file <- tempfile(fileext = ".tif")
        
        writeBin(content(GET(file_url), "raw"), temp_file)
        tiff_data <- rast(temp_file)
        tiff_data <- tiff_data %>%
          crop(project(extent, tiff_data), snap="near", mask=TRUE)
        
        # Construct output file name and save raster
        out_name <- file.path(destfile, paste0(tools::file_path_sans_ext(file_name), "_", ext, ".tif"))
        writeRaster(tiff_data, out_name, overwrite = TRUE)
        
        # Store result and clean up
        outList[[species_code]] <- tiff_data
        file.remove(temp_file)
        rm(tiff_data)
      }else if (version == "v5"){
        file_name <- paste0(species_code, "_", ext, "_", year, ".tiff")
        file_url <- paste(url, species_code, ext, file_name, sep= "/")
        writeBin(content(GET(file_url), "raw"), file.path(destfile, file_name))
        tiff_data <- rast(file.path(destfile, file_name))
        outList <- c(outList, setNames(list(tiff_data), species_code))
      }
    }
    return(outList)
  }
  
  # Perform batch download for species in the list
  for (s in spList) {
    outList <- batch_download(s, version, ext)
  }
  
  # Return the results as a list
  return(outList)
  
}
