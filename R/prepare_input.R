#' @title Prepare input for running 3-PG Model
#' @description Checks and prepares all input tables for use in \code{\link{run_3PG}}.
#' For detailed descriptions, see Forrester (2020).
#'
#' @param site A data frame containing site-level data. It must contain exactly one row with the following columns:
#' \itemize{
#'   \item \code{latitude}: Site latitude in the WGS84 coordinate system (degrees, range: [-90, 90]).
#'   \item \code{elevation}: Site elevation (meters above sea level, range: [0, 4000]).
#'   \item \code{soil_class}: Soil class as per 3PG User Manual Table 27:
#'     \itemize{
#'       \item 1: Clay
#'       \item 2: Clay loam
#'       \item 3: Loam
#'       \item 4: Loamy sand
#'       \item 5: Sand
#'       \item 6: Sandy clay
#'       \item 7: Sandy clay loam
#'       \item 8: Sandy loam
#'       \item 9: Silt
#'       \item 10: Silty clay
#'       \item 11: Silty clay loam
#'       \item 12: Silty loam
#'       \item 0: Uses c\eqn{\theta} and n\eqn{\theta} provided in the "parameters" input
#'     }
#'   \item \code{asw_i}: Initial available soil water (mm, must be >= 0).
#'   \item \code{asw_min}: Minimum available soil water (mm, must be >= 0).
#'   \item \code{asw_max}: Maximum available soil water (mm, must be >= 0).
#'   \item \code{from}: Start of simulation period (year-month, e.g., "2000-01").
#'   \item \code{to}: End of simulation period (year-month, e.g., "2009-12"). The simulation includes the entire month of December 2009.
#' }
#' @param species A data frame containing species-level information. Each row corresponds to one species or cohort.
#' Required columns:
#' \itemize{
#'   \item \code{species}: Species or cohort ID/name. Must match species names in \code{thinning}, \code{parameters}, and \code{sizeDist}.
#'   \item \code{planted}: Planting date in "year-month" format (e.g., "2000-01").
#'   \item \code{fertility}: Soil fertility, ranging from 0 to 1.
#'   \item \code{stems_n}: Number of trees per hectare.
#'   \item \code{biom_stem}: Stem biomass (Mg/ha).
#'   \item \code{biom_root}: Root biomass (Mg/ha).
#'   \item \code{biom_foliage}: Initial foliage biomass (Mg/ha). For leafless periods, provide spring foliage biomass.
#' }
#' Optional columns (required if `mort_model = 2`):
#' \itemize{
#'   \item \code{lt_fN}: Long-term soil nutrition modifier.
#'   \item \code{lt_fT}: Long-term temperature modifier.
#'   \item \code{lt_fPhys}: Long-term vapour pressure deficit and soil modifier.
#' }
#' @param climate A data frame containing monthly climate data. The table must include the following columns:
#' \itemize{
#'   \item \code{year}: Year of observation (only required for subsetting) (numeric).
#'   \item \code{month}: Month of observation (only required for subsetting) (numeric).
#'   \item \code{tmp_min}: Monthly mean daily minimum temperature (°C).
#'   \item \code{tmp_max}: Monthly mean daily maximum temperature (°C).
#'   \item \code{tmp_ave}: Monthly mean daily average temperature (°C) (optional).
#'   \item \code{prcp}: Monthly rainfall (mm month\eqn{-1}).
#'   \item \code{srad}: Monthly mean daily solar radiation (MJ m\eqn{^{-2}} d\eqn{^{-1}}).
#'   \item \code{frost_days}: Frost days per month (d month\eqn{-1}).
#'   \item \code{co2}: Monthly mean atmospheric CO2 (ppm), required if \code{calculate_d13c = 1} (optional).
#'   \item \code{d13catm}: Monthly mean isotopic composition of air (‰), required if \code{calculate_d13c = 1} (optional).
#' }
#' @param thinning A data frame containing thinning information. If no thinning is required, set to \code{NULL}. The following columns are required:
#' \itemize{
#'   \item \code{species}: Species or cohort ID/name.
#'   \item \code{age}: Age (years) at which thinning is performed (numeric).
#'   \item \code{stems_n}: Number of trees remaining after thinning (numeric).
#'   \item \code{stem}: Type of thinning (above/below) applied to stems (numeric, default is 1).
#'   \item \code{root}: Type of thinning (above/below) applied to roots (numeric, default is 1).
#'   \item \code{foliage}: Type of thinning (above/below) applied to foliage (numeric, default is 1).
#'   \item \code{biom_prop_retained}: Proportion of aboveground biomass retained after thinning.
#' }
#' @param defoliation A data frame containing defoliation information. If no defoliation is required, set to \code{NULL}. The following columns are required:
#' \itemize{
#'   \item \code{species}: Species or cohort ID/name.
#'   \item \code{age}: Age (years) at which defoliation occurs (numeric).
#'   \item \code{stem_retained}: Proportion of stem mass retained after defoliation (0 to 1).
#'   \item \code{foliage_retained}: Proportion of foliage mass retained after defoliation (0 to 1).
#'   \item \code{root_retained}: Proportion of root mass retained after defoliation (0 to 1).
#'   \item \code{stem}: Fraction of average tree stem mass of killed trees (numeric, default 1).
#'   \item \code{def_recover_t}: Time (months) to recover from defoliation (numeric).
#'   \item \code{prop_carbs}: Proportion of pre-defoliation non-structural carbohydrates used to regenerate foliage (0 to 1).
#'   \item \code{prop_npp}: Proportion of new photosynthate allocated to foliage (0 to 1).
#' }
#' @param parameters A data frame with parameters to modify. Columns must include:
#' \itemize{
#'   \item \code{parameter}: Name of the parameter.
#'   \item Additional columns corresponding to species/cohort names.
#' }
#' @param size_dist A data frame with size distribution values. Required columns:
#' \itemize{
#'   \item \code{parameter}: Name of the parameter.
#'   \item Additional columns corresponding to species/cohort names.
#' }
#' @param settings A list of model settings. Defaults:
#' \itemize{
#'   \item \code{light_model}: 1 3-PGpjs (default), 2: 3-PGmix.
#'   \item \code{transp_model}: 1 3-PGpjs (default), 2: 3-PGmix.
#'   \item \code{phys_model}: 1 3-PGpjs (default), min of f_vpd and f_sw; '2' - 3-PGmix, f_vpd x f_sw
#'   \item \code{height_model}: 1 exponential (default); 2 Michajlow; 3 Näslund
#'   \item \code{crown_width_model}: 1 exponential (default); 2 Michajlow; 3 Näslund
#'   \item \code{calculate_d13c}: 0 (default: no), 1: yes.
#'   \item \code{mort_model}: 1 3-PGpjs (default); 2 self-thinning with modifiers; 3 deltaN and deltaB with modifiers
#' }
#'
#' @return A list with 7 elements: site, species, climate, thinning, parameters, size_dist, and settings.
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_parameters}}, \code{\link{prepare_sizeDist}}, \code{\link{prepare_thinning}}, \code{\link{prepare_climate}}, \code{\link{prepare_site}}
#'
#' @example inst/examples/prepare_input-help.R
#'
#' @references
#' Forrester, D. I., 2020. 3-PG User Manual. Swiss Federal Institute for Forest, Snow and Landscape Research WSL, Birmensdorf, Switzerland. 70 p. Available at the following web site: \url{https://sites.google.com/site/davidforresterssite/home/projects/3PGmix/3pgmixdownload}
#'
#'Sands, P. J., 2010. 3PGpjs user manual. Available at the following web site: \url{https://3pg.sites.olt.ubc.ca/files/2014/04/3PGpjs_UserManual.pdf}
#'
#' @export
#'
prepare_input <- function(
  site,
  species,
  climate,
  thinning = NULL,
  defoliation = NULL,
  parameters = NULL,
  size_dist = NULL,
  settings = NULL
){

  # Settings
  set_def = list(
    light_model = 1, transp_model = 1, phys_model = 1,
    height_model = 1, crown_width_model = 1, calculate_d13c = 0, # correct_bias = 0, =
    mort_model = 1#, manag_model = 1
    )
  set_def[names(settings)] <- settings

  # Prepare each table
  # Site
  site = prepare_site(site = site)

  # Species
  #if( set_def['mort_model'] == 2 && anyNA(species[, c( "lt_fN","lt_fT","lt_fPhys" )]) ){            !20251114
  #  stop('Long-term modifiers (lt_fN, lt_fT, lt_fPhys) must contain non-NA values.')
  #}
  species = prepare_species(species = species)


  # Climate
  if( set_def['calculate_d13c'] == 1 ){
    if( !all( c("co2","d13catm") %in% colnames(climate) ) ){
      stop('Please provide forcing data for co2 and d13catm in climate, if calculate_d13c = 1')
    }
  }

  climate = prepare_climate(climate = climate, from = site$from, to = site$to)

  # Thinning
  thinning = prepare_thinning( thinning = thinning, sp_names = species$species)

  # Defoliation
  defoliation = prepare_defoliation( defoliation = defoliation, sp_names = species$species)

  # Parameters
  parameters = prepare_parameters( parameters = parameters, sp_names = species$species)

  # Size distribution
  #if( set_def['correct_bias'] == 1 & is.null(size_dist) ){
  #  stop('Please provide size_dist table or change the setting to size_dist = 0')
  #}
  size_dist = prepare_sizeDist( size_dist = size_dist, sp_names = species$species)


  # return the checked output
  out <- list( site = site, species = species, climate = climate, thinning = thinning,
               defoliation = defoliation,
               parameters = parameters, size_dist = size_dist, settings = set_def)

  return( out )
}
