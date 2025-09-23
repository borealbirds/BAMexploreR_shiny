

### From file: bam_get_bcr.R ###

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


### From file: bam_get_layer.R ###

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
                      "can81", "can82")
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

  allowed_years <- c("2000", "2005", "2010", "2015", "2020")
  if(version == "v5"){
    if (!all(year %in% allowed_years)) {
      stop("Invalid year: must be one of ", paste(allowed_years, collapse = ", "))
    }
  }

  valid_species <- bam_spp_list(version = version )
  if (!all(spList %in% valid_species)) {
    stop("Invalid species in spList: must be in bam_spp_list()")
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
    } else {
      tiff_data <- download_raster(file_url, to_temp = (version == "v5"))
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


### From file: bam_map_bcr.R ###

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
    ncat <-19
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


### From file: bam_occurrence.R ###

##################################################################################
#' Estimate a presence/absence threshold using a Lorenz curve
#'
#' @description This function fits a Lorenz curve to a raster's density
#' values using \code{opticut::lorenz()}.
#' The Lorenz curve is not plotted. See package \code{opticut} for
#' inspecting Lorenz curves. Instead, the plot generated by \code{plot=TRUE} is the user's
#' input density raster re-coded to presence (1) / absence (0) values.
#'
#'
#' @param raster_list A raster of the "Area of Interest". Defined and created via Melina's function(s).
#'
#' @param quantile Default is \code{"by_lorenz"}, and the  optimum threshold is estimated via
#' \code{opticut::lorenz()} as the pixel density when the slope of the tangent of the Lorenz function is 1.
#' If a custom threshold is preferred, set this argument using a \code{numeric} between 0 and 1.
#' This value indicates a cumulative proportion of pixels, above which all raster pixel values are assigned as "presence".
#' Raster pixel values below the quantile are assigned "absent". E.g. By setting \code{quantile=0.8},
#' the threshold density for separating presence versus absence is whatever pixel value accumulates 80% of the total
#' pixels from the raster.
#'
#' @param plot Default is \code{TRUE} for visualizing occurrence patterns.
#'
#' @return A list with:
#' \describe{
#'   \item{raster}{A \code{SpatRaster} with binary presence (1) / absence (0) values.}
#'   \item{occurrence_summary}{A \code{data.frame} with estimated area of occupancy
#'   in km², before and after thresholding.}
#'   }
#'
#' @details \code{opticut::lorenz()} generates a Lorenz plot where the x-axis represents the cumulative
#' proportion of area (sorted by bird density, \code{p}) and the y-axis represents the cumulative
#' proportion of the raster's total population (\code{L}). If bird density were uniform across
#' space, the plot would follow a line with a slope of 1. In reality birds are not distributed evenly
#' on the landscape. \code{lorenz()} estimates a pixel density threshold separating "presence" from "absence".
#' The threshold is defined as the point on the Lorenz curve where the tangent has a slope of 1 and \code{p - L}
#' is maximized. See \code{?opticut::lorenz} for details.
#' Note that in some cases, the Lorenz-derived threshold is similar to the mean density, however, they
#' estimate different ecological quantities and are not inherently equal.
#'
#'
#'
#'
#' @importFrom opticut lorenz iquantile
#' @importFrom terra rast values setValues
#' @importFrom dplyr arrange bind_rows
#' @importFrom purrr map imap map_dbl
#' @importFrom tibble tibble
#' @export
#' @examples
#'
#' # download v4 rasters for Tennessee Warbler and Ovenbird
#' rasters <- bam_get_layer(c("TEWA", "OVEN"), "v4", destfile=tempdir())
#'
#' # visualize core habitat
#' bam_occurrence(rasters)
#'
#' # set custom threshold and compare core habitat
#' bam_occurrence(rasters, quantile=0.8)
#'
#' # analyse core habitat in a custom area, using v5 predictions for Tennessee Warbler
#' aoi_sf <- vect(system.file("extdata", "vignette_poly_5072.shp", package = "BAMexploreR"))
#' rasterv5 <- bam_get_layer("TEWA", "v5",  crop_ext = aoi_sf, destfile = tempdir(), year = "2020")
#' bam_occurrence(rasterv5)
#'
#'

bam_occurrence <- function(raster_list, quantile="by_lorenz", plot=TRUE){

  # check for valid input
  stopifnot(is.list(raster_list))
  stopifnot(all(purrr::map_lgl(raster_list, ~ inherits(.x, "SpatRaster"))))

  # extract the mean if the SpatRaster has a "mean" layer
  extract_mean_layer <- function(raster_i){

    if ("mean" %in% names(raster_i)){

       return(raster_i[["mean"]])

    } else return(raster_i)
  }

  raster_list <- lapply(raster_list, extract_mean_layer)


  # define thresholding function for a single raster
  estimate_threshold <- function(raster_i, spp){

    # retrieve density per pixel, retain this vector with NAs to preserve pixel positions
    dpp <- terra::values(raster_i, na.rm=FALSE)

    # remove NAs for `lorenz()`
    dpp_no_nas <- dpp[!is.na(dpp)]

    # estimate threshold from a Lorenz curve
    lorenz_fit <- opticut::lorenz(dpp_no_nas)

    # apply threshold
    if (quantile == "by_lorenz"){

      # locate the pixel value where the tangent approaches 1:1
      t_pixel <- summary(lorenz_fit)["t"]

      # "x" is bird density when "t_pixel" approaches 1:1
      # ("p" is the proportion of pixels, "L" is the proportion of birds)
      optimum_threshold <- lorenz_fit[t_pixel, "x"]
      names(optimum_threshold) <- "optimum threshold"

    } else {

      # `L` for ordered cumulative abundance quantiles (versus non-cumulative)
      # `threshold` partitions "1 minus threshold" proportion of values as presence (1) and the rest ("threshold") as absence (0)
      # e.g. for `threshold=0.8` the densest 20% of values are assigned as presence (1) and the rest as absence (0)
      optimum_threshold <- quantile(lorenz_fit, probs = quantile, type = "L")
      names(optimum_threshold) <- "optimum threshold"

   } # finish finding optimum threshold

    # create binarized density raster based on the current threshold
    # note: need to preserve NA positions from the original raster to
    # maintain raster range between input and output
    classify_pixels <- function(dpp, threshold) {
      ifelse(dpp >= threshold, 1, ifelse(!is.na(dpp), 0, NA))
    }

    pixels_binary <- classify_pixels(dpp, optimum_threshold)

    # write the binary values into a raster object
    binary_raster <- terra::setValues(raster_i, pixels_binary)

    # mask the original raster with above threshold values
    threshold_raster <- terra::setValues(raster_i, ifelse(dpp >= optimum_threshold, dpp, 0))

    # count occurrence pixels before and after thresholding
    n_pixels <- terra::global(raster_i >= 0, "sum", na.rm = TRUE)[1,1]
    n_core_pixels <- terra::global(binary_raster == 1, "sum", na.rm = TRUE)[1,1]

    # get pixel area in square kilometers
    pixel_area_km2 <- (terra::res(raster_i)[1] * terra::res(raster_i)[2]) / 1e6  # convert square meters to square km

    # total area of core habitat in km2
    og_area_km2 <- n_pixels * pixel_area_km2
    core_area_km2 <- n_core_pixels * pixel_area_km2


    # plot occurrence map
    if (plot==TRUE){
      #terra::plot(binary_raster, main=paste("density threshold =", round(optimum_threshold, digits = 4), paste("\n", spp)))
      terra::plot(binary_raster, main=paste0(spp, "\n", "density threshold = ", round(optimum_threshold, 4)),
                  type="classes", col = c("#CCCCCC", "#666666"),levels = c("Absent", "Present"), legend = TRUE)
    }

    return(list(occurrence_raster = binary_raster,
                threshold_raster = threshold_raster,
                threshold = optimum_threshold,
                og_area_km2 = og_area_km2,
                core_area_km2 = core_area_km2))

  } # close thresholding function


  # apply threshold estimate to user's list of rasters
  output_list <- purrr::imap(raster_list, estimate_threshold)

  # extract other area data from list output
  thresholds <- purrr::map_dbl(output_list, "threshold")
  og_area <- tibble::tibble(species = names(output_list), type="no_threshold", area_km2=purrr::map_dbl(output_list, "og_area_km2"))
  threshold_area <- tibble::tibble(species = names(output_list), type="with_threshold", area_km2=purrr::map_dbl(output_list, "core_area_km2"))

  occurrence_summary <-
    bind_rows(og_area, threshold_area) |>
    arrange(species)

  return(list(
    occurrence_rasters = purrr::map(output_list, "occurrence_raster"),
    occurrence_summary = occurrence_summary))

}



### From file: bam_pop_size.R ###

##################################################################################
#' Estimate population size from a density raster
#'
#' @description This function is essentially a wrapper around \code{terra::values},
#' but with an adjustment for converting pixel values from males/ha to males/pixel.
#' It also provides a basic summary of the input raster.
#'
#' @param raster_list A list of \code{SpatRaster}s. See \code{bam_get_layer()} for accessing BAM's raster data.
#' @param crop_ext SpatVector used to define the extent for the cropping and grouping of population estimates.
#' @param group Optional character value of column in SpatVector used for grouping population estimates.
#'
#' @return A \code{tibble} with six columns: \code{group}, \code{total_pop}, \code{mean_density} (per pixel), \code{sd_density}, \code{n_cells}, \code{species}
#'
#'
#' @importFrom dplyr left_join mutate row_number rename group_by ungroup summarize all_of n filter
#' @importFrom purrr imap
#' @importFrom stats sd
#' @importFrom tidyselect everything
#' @importFrom terra aggregate
#'
#' @export
#' @examples
#' # download rasters for Tennessee Warbler and Ovenbird
#' rasters <- bam_get_layer(c("TEWA", "OVEN"), "v4", destfile=tempdir())
#'
#' # get summaries of population size
#' bam_pop_size(rasters) # 111 million and 3.5 million, respectively

bam_pop_size <- function(raster_list, crop_ext= NULL, group = NULL){
  # check for valid input
  stopifnot(is.list(raster_list))
  stopifnot(all(purrr::map_lgl(raster_list, ~ inherits(.x, "SpatRaster"))))

  raster_list_mean <- map(raster_list, function(r) {
    lyr_names <- trimws(names(r))
    if ("mean" %in% lyr_names) {
      r[["mean"]]
    } else {
      r  # or return r if you want to keep it as-is
    }
  })

  # Reproject crop_ext
  cat("Aligning projections\n")
  if (!is.null(crop_ext)){
    if(inherits(crop_ext, "SpatVector") || inherits(crop_ext, "SpatRaster") ) {
      if (nchar(crs(crop_ext)) == 0) {
        stop("CRS of crop_ext is missing or empty.")
      }else{
        crop_ext <- terra::project(crop_ext, "EPSG:5072")
      }
    }else{
      stop("crop_ext need to be a SpatVector  or a SpatRaster")
    }
  }

  # Check crop_ext area
  if(!is.null(crop_ext)){
    crop_area <- expanse(crop_ext, unit="km")
    if(sum(crop_area) < 100){
      warning(sprintf("The BAM density models are predicted to a resolution of 1 km2. Your area of interest is only %.2f km2. Please consider whether these models are appropriate for your application.", crop_area))
    }
  }

  # Aggregate crop_ext by grouping variable
  crop_ext_grp <- if(!is.null(group)){
    terra::aggregate(crop_ext, by = group)
  } else {crop_ext}

  # define crop function
  crop_raster <- function(r, ext) {
    terra::crop(r, ext, snap = "near", mask = TRUE)
  }

  pop_estimate <- function(raster_i, crop_ext_grp, group, spp) {
    # ——————————————————————————————
    # 1) handle NULL crop_ext_grp by extracting all pixels
    # ——————————————————————————————
    if (is.null(crop_ext_grp)) {
      # Create a data.frame with one row per pixel:
      values <- terra::values(raster_i, mat = FALSE)
      # data.frame with a dummy 'ID' column so the rest of your code works:
      pixel_values <- data.frame(ID = seq_along(values), mean = values)

    } else {
      # normal case
      pixel_values <- terra::extract(raster_i, crop_ext_grp)
      value_col <- setdiff(names(pixel_values), "ID")

      # rename it to "density"
      pixel_values <- pixel_values %>%
        dplyr::rename(mean = all_of(value_col))
    }

    # ——————————————————————————————
    # 2) the rest of your code stays unchanged
    # ——————————————————————————————
    if (!is.na(group)) {
      group_summary <- dplyr::left_join(pixel_values,
                                        as.data.frame(crop_ext_grp) |>
                                          dplyr::mutate(ID = row_number()),
                                        by = "ID") |>
        dplyr::rename(density = mean) |>
        dplyr::filter(!is.na(density)) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
        dplyr::summarize(total_pop = sum(density)*100,
                         mean_density = round(mean(density), 3),
                         sd_density = round(sd(density), 3),
                         n_cells = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::mutate(species = spp) |>
        dplyr::select(species, tidyselect::everything())

      #rename the group column
      colnames(group_summary) <- c("group", colnames(group_summary[2:ncol(group_summary)]))
    } else {
      group_summary <- pixel_values |>
        dplyr::rename(density = mean) |>
        dplyr::filter(!is.na(density)) |>
        dplyr::summarise(
          total_pop   = sum(density) * 100,
          mean_density = round(mean(density), 3),
          sd_density   = round(sd(density), 3),
          n_cells      = n()
        ) |>
        dplyr::mutate(group   = NA,
                      species = spp) |>
        dplyr::select(species, group, tidyselect::everything())
    }

    return(group_summary)
  }

  # crop (this will speed things up for small AOIs)
  if (is.null(crop_ext)) {
    crop_list <- raster_list_mean
  } else {
    crop_list <- purrr::imap(raster_list_mean, ~ crop_raster(.x, crop_ext_grp))
  }

  # calculate
  cat("Calculating population size - be patient!\n")
  if(!is.null(group)){
    list_of_summaries <- purrr::imap_dfr(crop_list, ~pop_estimate(
      raster_i = .x,
      spp = .y,
      crop_ext_grp = crop_ext_grp,
      group = group))
  } else {
    list_of_summaries <- purrr::imap_dfr(crop_list, ~pop_estimate(
      raster_i = .x,
      spp = .y,
      crop_ext_grp = crop_ext_grp,
      group = NA))
  }

  # output
  return(list_of_summaries)
}



### From file: bam_predictor_barchart.R ###

##################################################################################
#' Visualize Predictor Importance as Stacked Bar Charts
#'
#' Creates stacked bar plots showing the proportion of model predictors importance
#' in predicting bird abundance.
#'
#'
#'
#' @param species A \code{character} specifying the species to filter by. The default is \code{"all"}, which includes all species in the dataset.
#' See \code{data(spp_tbl)} for available species and their spelling.
#'
#' @param bcr A \code{character} specifying the model subregions, or Bird Conservation Regions (BCRs) to filter by.
#' The default is \code{"all"}, which includes all BCRs in the dataset.
#'
#' @param groups A \code{character} of two grouping variables for summarising predictor importance.
#' The first group element is plotted on the x-axis as bins each containing a stacked bar,
#' while the second group element is shown by fill colours in the stacked bars.
#' Valid strings are any two of: \code{"spp"} (species), \code{"bcr"} (BCR; model subregion), \code{"predictor"} (model predictor), or \code{"predictor_class"} (model predictor class).
#' Please see the examples below for a visualization.
#'
#' @param version A \code{character}. Defaults to \code{"v5"}. Loads BAM's predictor importance data,
#' a \code{data.frame} containing predictor importance values, with mean predictor importance as
#' rows and columns \code{bcr}, \code{species}, \code{predictor_clas}, \code{n_boot}, \code{mean_rel_inf}, and \code{sd_rel_inf}.
#' \code{"v4"} is also possible but not fully supported for all functions in the first release of this package.
#'
#' @param plot A \code{logical} indicating whether to plot the results (\code{TRUE}) or return the processed data (\code{FALSE}).
#'
#' @param colours A \code{character} vector of hex codes for the colours to use in the ggplot (optional).
#' If \code{NULL}, default colours are used.
#'
#' @return A stacked bar chart with the first group element plotted on the x-axis as bins each containing a stacked bar, and the second group element is shown by fill colours in the stacked bars.  If plot = FALSE the processed data is returned as a data.frame.
#'
#' @details Stacked bars can be grouped by species, predictor class, or
#' Bird Conservation Region (BCR). For example, grouping by species and predictor class creates
#' a plot where a stacked bar is created for each species, and each bar is split into the proportion
#' of predictor importance that each of nine predictor classes contributed, pooled across the specified BCRs.
#'
#'
#' @importFrom rlang syms
#' @importFrom dplyr filter summarise group_by left_join mutate
#' @importFrom ggplot2 ggplot aes geom_bar theme theme_classic element_text scale_fill_manual labs
#'
#' @export
#'
#' @examples

#' # Compare predictor importance (binned by predictor class) for all species in all BCRs
#' bam_predictor_barchart(species = "all", bcr = "all",  groups = c("spp", "predictor_class"))
#'
#' # Compare predictor importance (binned by predictor class) in the Prairies (BCRs 11, 6-1, 6-0)
#' # to the Pacific Coast across (BCR 5) all species
#' prairies_to_coast <- c("can11", "can60", "can61", "can5")
#' bam_predictor_barchart(species = "all", bcr = prairies_to_coast, groups=c("bcr", "predictor_class"))
#'
#' # Compare predictor importance (binned by predictor class) for four
#' # warbler species in BCR14
#' warblers <- c("CAWA", "BAWW", "BTNW", "BLBW")
#' bam_predictor_barchart(species = warblers, bcr = "can14", groups = c("spp", "predictor_class"))
#'
#' # Compare predictor importance for a single warbler species
#' # relative to the total influence that predictor had across all warblers.
#' bam_predictor_barchart(species = warblers, bcr = "can14", groups = c("predictor", "spp"))


bam_predictor_barchart <- function(species = "all", bcr = "all",  groups = c("spp", "predictor_class"), version ="v5", plot = TRUE, colours = NULL){

  if (!version %in% c("v4", "v5")) {
    stop("Invalid version argument. Must be either 'v4' or 'v5'.")
  }

  # load bam_predictor_importance_v* from data folder
  #load(system.file("R/sysdata.rda", package = "BAMexploreR"))
  if (version == "v5") {
    #data("bam_predictor_importance_v5", package = "BAMexploreR")
    data <- bam_predictor_importance_v5
  } else {
    #data("bam_predictor_importance_v4", package = "BAMexploreR")
    data <- bam_predictor_importance_v4
  }

  # convert user specified species to FLBCs
  if (!identical(species, "all")){
    species <- standardize_species_names(species_input = species, spp_tbl = BAMexploreR:::spp_tbl)
  }

  # check if user specified species are in `data`
  if (!all(species %in% unique(data$spp)) && !identical(species, "all")) {
    stop(paste("The following species are not in `data`:",
               paste(setdiff(species, unique(data$spp)), collapse = ", ")))
  }

  # check if user specified BCRs are in `data`
  if (!all(bcr %in% unique(data$bcr)) && !identical(bcr, "all")) {
    stop(paste("The following BCR(s) are not in `data`:",
               paste(setdiff(bcr, unique(data$bcr)), collapse = ", ")))
  }

  # filter for user-specified species
  if (!identical(species, "all")) {
    data <- filter(data, spp %in% species)
  }

  # filter for user-specified BCRs
  # use .env because `bcr` is also a column name in `data`
  if (!identical(bcr, "all")) {
    data <- dplyr::filter(data, bcr %in% .env$bcr)
  }

  # ensure groups are specified correctly
  if (is.null(groups) || length(groups) < 2) {
    stop("The 'groups' parameter must be a character vector with at least two elements.")
  }

  # check groups exist in data
  if (!all(groups %in% colnames(data))) {
    stop("One or more elements in `groups` are not valid column names in `data`.")
  }


  # for dplyr::group_by
  group_syms <- rlang::syms(groups)


  # sum predictor importance across for every permutation of group1 and group2
  rel_inf_sum <-
    data |>
    drop_na() |>
    group_by(!!!group_syms) |>
    summarise(sum_influence = sum(mean_rel_inf), .groups="keep")


  # sum of predictor importance for each of group1 (all group2 sums are amalgamated into group1 bins)
  group1_sum <-
    rel_inf_sum |>
    group_by(!!group_syms[[1]])  |>
    summarise(sum_group1 = sum(sum_influence), .groups="keep")

  # get the %contribution of group2 predictors to overall predictor importance for a given group1
  proportion_inf <-
    rel_inf_sum |>
    left_join(x = _, group1_sum, by=groups[1]) |>
    mutate(prop = sum_influence/sum_group1)


  if (plot) {
    p <- ggplot2::ggplot(proportion_inf, ggplot2::aes(x = !!group_syms[[1]], y = prop, fill = !!group_syms[[2]])) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(x = groups[1], y = "Proportion of Influence", fill = groups[2])

    if (!is.null(colours)) {
      p <- p + ggplot2::scale_fill_manual(values = colours)
    }

    return(p)
  } else {
    return(proportion_inf)
  }
}




### From file: bam_predictor_importance.R ###

##################################################################################
#' Plot Side-by-Side Variation in Predictor Importance by Predictor Class
#'
#' @description Plot mean relative predictor importance (with bootstrap variation)
#' by predictor class, with options to group by BCR (Bird Conservation Region) or species.
#'
#' @param species A \code{character} specifying the species to filter by. The default is \code{"all"}, which includes all species in the dataset.
#'
#' @param bcr A \code{character} specifying the Bird Conservation Regions (BCRs) to filter by. The default is \code{"all"}, which includes all BCRs in the dataset.
#'
#' @param group A \code{character} specifying the grouping variable for summarizing predictor importance.
#' Valid strings are \code{"spp"} (species), or \code{"bcr"} (BCR).
#'
#' @param version A \code{character}. Defaults to \code{"v5"}. Loads BAM's predictor importance data,
#' a \code{data.frame} containing predictor importance values, with mean predictor importance as
#' rows and columns \code{bcr}, \code{species}, \code{predictor_class}, \code{n_boot}, \code{mean_rel_inf}, and \code{sd_rel_inf}.
#' \code{"v4"} is also possible but not fully supported for all functions in the first release of this package.
#'
#' @param plot A \code{logical} indicating whether to plot the results (\code{TRUE}) or return the processed data (\code{FALSE}).
#'
#' @param colours A \code{character} vector of hex codes for the colours to use in the ggplot (optional).
#' If \code{NULL}, default colours are used.
#'
#' @return A ggplot displaying percent predictor importance by predictor class, grouped by the \code{group} argument.
#' Percent importance is used to allow comparisions across groups that have
#' differing total predictor importance.
#' If \code{plot = FALSE} the processed data is returned as a \code{data.frame}.
#'
#' @details Bootstrap variation (per species x BCR) is propagated by
#' taking the root sum square of standard deviation values.
#'
#' @importFrom dplyr group_by filter summarise left_join mutate
#' @importFrom rlang syms
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point labs theme theme_classic element_text position_dodge scale_colour_manual
#' @importFrom tidyr drop_na
#'
#' @export
#' @examples
#'
#'
#' # Example of plotting predictor importance for Townsend's Solitaire across all BCRs.
#' # This is a species with relatively high bootstrap variance.
#' bam_predictor_importance(species = "TOSO")
#'
#' # Example of plotting predictor importance for two warbler species from three BCRs,
#' # using custom colours:
#' bam_predictor_importance(species = c("BAWW", "CAWA"), group = "spp",
#' bcr = c("can12", "can13", "can14"),  colours = c("#1f78b4", "#33a02c"))
#'
#'
#'
##################################################################################

bam_predictor_importance <- function(species = "all", bcr = "all", group = "spp", version = "v5", plot = TRUE, colours = NULL) {

  # validate data version
  if (!version %in% c("v4", "v5")) {
    stop("Invalid `version`. Use 'v4' or 'v5'")
  }

  # load bam_predictor_importance_v* from data folder
  #load(system.file("R/sysdata.rda", package = "BAMexploreR"))
  if (version == "v5") {
    #data("bam_predictor_importance_v5", package = "BAMexploreR")
    data <- bam_predictor_importance_v5
  } else {
    #data("bam_predictor_importance_v4", package = "BAMexploreR")
    data <- bam_predictor_importance_v4
  }

  # convert user specified species to FLBCs
  if (!identical(species, "all")){
    species <- standardize_species_names(species_input = species, spp_tbl = BAMexploreR:::spp_tbl)
  }

  # check if user specified species are in `data`
  if (!all(species %in% unique(data$spp)) && !identical(species, "all")) {
    stop(paste("The following species are not in `data`:",
               paste(setdiff(species, unique(data$spp)), collapse = ", ")))
  }

  # check if user specified BCRs are in `data`
  if (!all(bcr %in% unique(data$bcr)) && !identical(bcr, "all")) {
    stop(paste("The following BCR(s) are not in `data`:",
               paste(setdiff(bcr, unique(data$bcr)), collapse = ", ")))
  }

  # check if user specified `group` is in `data`
  if (is.null(group) || !group %in% colnames(data)) {
    stop("Please specify a valid `group` column from the data.")
  }


  # filter for user-specified species
  if (!identical(species, "all")) {
    data <- filter(data, spp %in% species)
  }

  # filter for user-specified BCRs
  if (!identical(bcr, "all")) {
    data <- dplyr::filter(data, bcr %in% !!bcr)
  }

  # check if user specified `colours` match the number of levels in `group`.
  if (!is.null(colours)) {
    n_groups <- length(unique(data[[group]]))
    if (length(colours) != n_groups) {
      stop(paste("The length of `colours` does not match the number of levels in `group`", n_groups))
    }
  }


  # convert characters to symbols for dplyr::group_by
  group_sym <- rlang::syms(unique(c(group, "predictor_class")))


  # group by user-specified group
  # then, sum rel.inf by the grouped predictor per predictor class
  # convert std. dev. back to variance, sum, and take sqrt()
  cov_importance_grouped <-
    data |>
    group_by(!!!group_sym) |> # !!! evaluates a list of expressions
    filter(!is.na(predictor_class)) |>
    summarise(sum_inf = sum(mean_rel_inf), sd_inf = sd(mean_rel_inf),
              pooled_sd = sqrt(sum(sd_rel_inf^2)),
              .groups = "keep")

  # group by user-specified group,
  # then, sum rel.inf from all predictor_classes
  group1_sum <-
    cov_importance_grouped |>
    group_by(!!group_sym[[1]]) |>
    summarise(sum_all_groups = sum(sum_inf), .groups = "keep")

  # calculate the percent of predictor importance
  # sd_percent_inf is the uncertainty of the percent influence of a given predictor_class
  percent_importance <-
    cov_importance_grouped |>
    left_join(group1_sum, by = group) |>
    mutate(percent_inf = 100 * sum_inf / sum_all_groups,
           sd_percent_inf = 100 * pooled_sd / sum_all_groups)


  if (plot) {
    p <- ggplot(percent_importance, aes(x = predictor_class, y = percent_inf,
                                        fill = !!sym(group), colour = !!sym(group))) +
      geom_point(position = position_dodge(width = 0.75), alpha = 0.7, size = 2.5) +
      geom_errorbar(aes(ymax = percent_inf + sd_percent_inf, ymin = percent_inf - sd_percent_inf),
                    position = position_dodge(width = 0.75), width = 0, linewidth = 0.75) +
      labs(x = "Predictor Class", y = "Relative Importance (%)",
           title = paste("Predictor importance by", group)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # Apply custom colours only if provided
    if (!is.null(colours)) {
      group_levels <- unique(data[[group]])
      names(colours) <- group_levels
      p <- p + scale_colour_manual(values = colours) +
        scale_fill_manual(values = colours)
    }

    return(p)

  } else {

    return(percent_importance)

  }
}











### From file: bam_spp_list.R ###

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


### From file: data-doc.R ###

#' Species Group Data
#'
#' This dataset contains a vector of species groups used in analysis.
#' The groups include categories such as "Waterfowl", "Birds of Prey", and "Shorebirds".
#'
#' @format A character vector with 12 elements:
#' \describe{
#'   \item{COSEWIC}{The conservation status of species.}
#'   \item{Cavity_Birds}{Species that nest in cavities.}
#'   \item{Waterfowl}{Bird species that primarily live on or near water.}
#'   \item{Marine_Birds}{Bird species that live in marine environments.}
#'   \item{Shorebirds}{Bird species typically found along shorelines.}
#'   \item{Wetland_Birds}{Bird species that inhabit wetlands.}
#'   \item{Birds_of_Prey}{Raptors or predatory birds.}
#'   \item{Forest_Birds}{Bird species that live in forested areas.}
#'   \item{Grassland_Birds}{Bird species that inhabit grasslands.}
#'   \item{Aerial_Insectivores}{Birds that feed on insects while flying.}
#'   \item{Arctic_Birds}{Bird species found in Arctic regions.}
#'   \item{Long_Distance_Migrants}{Birds that migrate long distances between breeding and wintering grounds.}
#' }
#'
#' @source The data was derived from internal project datasets and species grouping systems.
#' @keywords datasets
#' @examples
#' data(guild_opt)
#' head(guild_opt)
#'
#' @docType data
"guild_opt"


### From file: predictor_metadata.R ###

#' Table of variable metadata for BAM density models
#'
#' This dataset lists all variables used in the BAM landbird density models for version 4 and version 5
#' The table contains information on the definitions of each variable and the source
#' For version 5, the table also contains further details on the source, as well as some of the methods used to extract each variable and build the raster layers for model prediction
#'
#' @format A data frame with 326 rows and 12 columns:
#' \describe{
#'   \item{version}{BAM landbird density model version}
#'   \item{variable}{Variable name used in the model objects and output}
#'   \item{definition}{A description of the variable}
#'   \item{category}{Grouping variable used in some of the package functions}
#'   \item{source}{The original data source for the variable}
#'   \item{provider}{The producer of the variable}
#'   \item{citation}{Citation for the data source}
#'   \item{covariate_extraction}{The scale that the covariate was extracted from the native resolution of the datasource; numerical values indicate the buffer radius used for extraction}
#'   \item{prediction_resolution}{The resolution that the variable was calculated at for model prediciton; "1km" indicates the mean or mode within 1 km, "5x5" indicates a moving focal window over the 1km layer}
#'   \item{years}{The years of data available for the source variable}
#'   \item{temporal_matchings}{How the years of data were temporally matched to the bird survey data}
#' }
#' @keywords internal
#'
#' @docType data
"predictor_metadata"


### From file: spp_tbl-doc.R ###

#' Table of BAM species
#'
#' This dataset lists all species from which BAM landbird density models were generated. We applied species groups categories used by
#' The State of the Canada's birds who classify species according to broad biomes and groups of species that are known to have distinct and noteworthy trends.
#' The same species can be included in more than one group, but only species that are truly representative of a given group are included in each
#' (Birds Canada and Environment and Climate Change Canada. 2024. The State of Canada’s Birds Report. Accessed from NatureCounts. DOI: 10.71842/8bab-ks08)
#'
#' @format A data frame with 143 rows and 16 columns:
#' \describe{
#'   \item{speciesCode}{AOU code used by WildTrax.}
#'   \item{commonName}{Common name of the bird.}
#'   \item{order}{Taxonomic order of the species.}
#'   \item{scientificName}{Scientific name of the species.}
#'   \item{COSEWIC}{Binary value (0 or 1) indicating whether the species is listed under COSEWIC (1 = listed, 0 = not listed).}
#'   \item{Cavity}{Binary value (0 or 1) indicating whether the species is classified as a cavity-nesting bird (1 = yes, 0 = no).}
#'   \item{Waterfowl}{Binary value (0 or 1) indicating whether the species is classified as waterfowl (1 = yes, 0 = no).}
#'   \item{Marine_Birds}{Binary value (0 or 1) indicating whether the species is classified as a marine bird (1 = yes, 0 = no).}
#'   \item{Shorebirds}{Binary value (0 or 1) indicating whether the species is classified as a shorebird (1 = yes, 0 = no).}
#'   \item{Wetland_Birds}{Binary value (0 or 1) indicating whether the species is classified as a wetland bird (1 = yes, 0 = no).}
#'   \item{Birds_of_Prey}{Binary value (0 or 1) indicating whether the species is classified as a bird of prey (1 = yes, 0 = no).}
#'   \item{Forest_Birds}{Binary value (0 or 1) indicating whether the species is classified as a forest bird (1 = yes, 0 = no).}
#'   \item{Grassland_Birds}{Binary value (0 or 1) indicating whether the species is classified as a grassland bird (1 = yes, 0 = no).}
#'   \item{Aerial_Insectivores}{Binary value (0 or 1) indicating whether the species is classified as an aerial insectivore (1 = yes, 0 = no).}
#'   \item{Arctic_Birds}{Binary value (0 or 1) indicating whether the species is classified as an Arctic bird (1 = yes, 0 = no).}
#'   \item{Long_Distance_Migrants}{Binary value (0 or 1) indicating whether the species is classified as a long-distance migrant (1 = yes, 0 = no).}
#' }
#' @keywords internal
#'
#' @docType data
"spp_tbl"


### From file: standardize_species_names.R ###

#' Standardize user's species inputs to 4-letter bird codes
#' 
#' Internal helper function. Converts species names (common, scientific, or FLBCs)
#' to FLBCs using spp_tbl.
#' 
#' @param species_input A character vector of species names or codes.
#' @param spp_tbl A lookup table containing speciesCode, commonName, and scientificName.
#'
#' @return A character vector of species codes (same length as input).
#' @noRd

standardize_species_names <- function(species_input, spp_tbl) {
  
  # convert to lowercase for case-insensitive matching
  species_input_lower <- tolower(species_input)
  
  # also make lookup columns lowercase
  spp_tbl <- 
    spp_tbl |> 
    mutate(
      speciesCode_lower = tolower(speciesCode),
      commonName_lower = tolower(commonName),
      scientificName_lower = tolower(scientificName)
    )
  
  # convert users' species to FLBCs
  matched_codes <- 
    purrr::map_chr(species_input_lower, function(sp) {
      
    if (sp %in% spp_tbl$speciesCode_lower) {
      
      spp_tbl$speciesCode[match(sp, spp_tbl$speciesCode_lower)]
      
    } else if (sp %in% spp_tbl$commonName_lower) {
      
      spp_tbl$speciesCode[match(sp, spp_tbl$commonName_lower)]
      
    } else if (sp %in% spp_tbl$scientificName_lower) {
      
      spp_tbl$speciesCode[match(sp, spp_tbl$scientificName_lower)]
      
    } else {
      warning(paste0(sp, "not found in spp_tbl. Returning NA."))
      NA_character_
    }
  }) # close map_chr()

  return(matched_codes)
  
} # close function


### From file: utils.R ###

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", ".env","predictor_class", "mean_rel_inf", "sd_rel_inf", "species", "spp", "sum_inf", "sum_all_groups",
                           "pooled_sd", "percent_inf", "sym", "sd_percent_inf", "guild_opt", "speciesCode", "commonName",
                           "scientificName", "sum_influence", "sum_group1", "prop", "density", "spp_tbl"))
}
