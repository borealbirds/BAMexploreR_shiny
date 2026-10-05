# Downsample, reproject to WGS84 and crop a model raster for leaflet display; zeros become NA
.display_raster <- function(r) {
  while (ncell(r) * 4 >= 4e6) {
    r <- terra::aggregate(r, fact = 2, fun = mean)
  }
  r <- terra::project(r, "EPSG:4326")

  target_extent <- ext(-177.9919, 30.65155, -18.12997, 81.60892)
  r_ext <- ext(r)
  if (xmin(r_ext) < xmin(target_extent) || xmax(r_ext) > xmax(target_extent) ||
      ymin(r_ext) < ymin(target_extent) || ymax(r_ext) > ymax(target_extent)) {
    r <- crop(r, target_extent)
  }
  r[r == 0] <- NA
  r
}

# Density palette shared by the map layers and their legend: 8 bins over the 0.25-99.75% quantiles
.density_palette <- function(vals) {
  rng_trim <- unname(quantile(vals, probs = c(0.0025, 0.9975), na.rm = TRUE))
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
  list(pal = pal, range = rng_trim)
}

# Replace the map legend with the one of raster r
.raster_legend <- function(map, r, title) {
  vals <- values(.display_raster(r))
  map %>%
    clearControls() %>%
    addLegend(
      pal = .density_palette(vals)$pal,
      values = vals,
      title = title,
      position = "bottomright",
      opacity = 1,
      labFormat = labelFormat(digits = 4)
    )
}

.add_species_layers <- function(map, sppMap) {

  added_layers <- c()

  # Loop through species and add raster layers
  for (spp_name in names(sppMap)) {

    raster_layer <- .display_raster(sppMap[[spp_name]])
    vals <- values(raster_layer)
    palette <- .density_palette(vals)
    pal <- palette$pal
    rng_trim <- palette$range

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

# Use matrix to check species availabilities per bcr
.filter_species_by_bcr  <- function(birdlist, spList, bcrNM) {
  valid_sp <- intersect(spList, names(birdlist))
  
  # subset birdlist for selected BCRs
  subset <- birdlist[birdlist$bcr %in% bcrNM, c("bcr", valid_sp), drop = FALSE]
  
  mat <- as.matrix(subset[valid_sp])
  keep <- colSums(mat) > 0
  valid_sp[keep]
}


#' Load and prepare a predictor-importance dataset
#'
#' Internal helper shared by \code{bam_predictor_importance()} and
#' \code{bam_predictor_barchart()}.
#'
#' Relative influence is normalised by \code{gbm::summary.gbm()} to sum to 100
#' within a single model, i.e. within a species x BCR x bootstrap. The v5 export
#' preserves this: every species x BCR sums to exactly 100. The v4 export was
#' truncated to predictors with \code{rel.inf >= 1}, so its species x BCR totals
#' range from ~1 to ~88 and are not comparable to one another. For v4 we rescale
#' each species x BCR back to 100 so that units are at least internally
#' consistent, and warn that composition remains distorted.
#'
#' @param version A \code{character}, either \code{"v4"} or \code{"v5"}.
#'
#' @return A \code{data.frame} of predictor importance whose \code{mean_rel_inf}
#'   sums to 100 within each species x BCR.
#'
#' @importFrom dplyr group_by mutate ungroup select
#' @noRd
.load_predictor_importance <- function(version) {
  
  if (version == "v5") {
    return(bam_predictor_importance_v5)
  }
  
  .warn_once(
    "v4_truncated",
    "Version 'v4' predictor importance retains only predictors with a relative ",
    "influence >= 1 (a median of 2 predictors per species x BCR). Each species x ",
    "BCR has been rescaled to sum to 100, but the truncation still biases ",
    "composition toward predictor classes made up of few, strongly influential ",
    "predictors. Treat cross-species and cross-BCR comparisons as indicative only; ",
    "use version = 'v5' where possible."
  )
  
  # rescale each species x BCR to sum to 100, carrying sd_rel_inf on the same scale
  bam_predictor_importance_v4 |>
    dplyr::group_by(spp, bcr) |>
    dplyr::mutate(
      .scale       = 100 / sum(mean_rel_inf, na.rm = TRUE),
      mean_rel_inf = mean_rel_inf * .scale,
      sd_rel_inf   = sd_rel_inf * .scale
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.scale)
}


#' Bootstrap-level predictor-class shares, derived on demand
#'
#' Correct uncertainty for a predictor class requires summing relative influence
#' within that class \emph{inside each bootstrap} before taking any variance,
#' because relative influence is compositional: predictors within one model are
#' negatively correlated by construction. The species x BCR summary datasets have
#' already averaged over bootstraps and cannot support this.
#'
#' \code{bam_predictor_boot_v5} is the single source of truth: it holds
#' \code{rel.inf} for every species x BCR x bootstrap x predictor, and
#' \code{bam_predictor_importance_v5} is derived from it. This helper performs the
#' other derivation, rolling predictors up to their class within each bootstrap.
#' The result is memoised, since collapsing ~1.7 million rows is wasted work on
#' the second and subsequent call in a session.
#'
#' @param version A \code{character}, either \code{"v4"} or \code{"v5"}.
#'
#' @return A \code{data.frame} with columns \code{spp}, \code{bcr}, \code{boot},
#'   \code{predictor_class} and \code{share}, or \code{NULL} when no
#'   bootstrap-level dataset is shipped for this version (v4 has none).
#'
#' @importFrom dplyr filter group_by summarise arrange
#' @noRd
.predictor_class_boot <- function(version) {
  
  key <- paste0("class_boot_", version)
  if (!is.null(.bam_cache[[key]])) return(.bam_cache[[key]])
  
  raw <- .get_dataset(paste0("bam_predictor_boot_", version))
  if (is.null(raw)) return(NULL)
  
  # `rel.inf` sums to 100 within a species x BCR x bootstrap, so dividing the
  # class total by 100 gives that class's share of the model in that bootstrap.
  # Predictors with no class would silently deflate the shares, so drop them
  # first; the shipped v5 data has none.
  out <-
    raw |>
    dplyr::filter(!is.na(predictor_class)) |>
    dplyr::group_by(spp, bcr, boot, predictor_class) |>
    dplyr::summarise(share = sum(rel.inf) / 100, .groups = "drop") |>
    dplyr::arrange(spp, bcr, boot, predictor_class) |>
    as.data.frame()
  
  .bam_cache[[key]] <- out
  out
}

.bam_cache <- new.env(parent = emptyenv())

#' Fetch a shipped dataset by name
#'
#' LazyData puts shipped datasets in the package namespace on a normal install,
#' but \code{devtools::load_all()} and a plain \code{data()} load can place them
#' elsewhere, so search both and return \code{NULL} rather than erroring when the
#' dataset is not shipped at all (as for v4 bootstrap data).
#'
#' @param nm A \code{character} dataset name.
#' @return The dataset, or \code{NULL}.
#' @noRd
.get_dataset <- function(nm) {
  out <- tryCatch(
    get(nm, envir = asNamespace("BAMexploreR")),
    error = function(e) tryCatch(get(nm), error = function(e) NULL)
  )
  if (is.null(out) || !is.data.frame(out)) NULL else out
}