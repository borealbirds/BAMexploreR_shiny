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
