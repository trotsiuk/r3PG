#' @title Prepare input for running 3-PG Model
#' @description Checks and prepares all input tables for use in \code{\link{run_3PG}}.
#' For detailed descriptions, see Forrester (2020).
#'
#' @param site A data frame containing site-level data. It must contain exactly one row with the following columns:
#' \itemize{
#'   \item \code{latitude}: Site latitude in the WGS84 coordinate system (degrees, range: [-90, 90]).
#'   \item \code{altitude}: Site altitude (meters above sea level, range: [0, 4000]).
#'   \item \code{soil_class}: Soil class as per 3PGpjs User Manual Table 2:
#'     \itemize{
#'       \item 1: Sandy
#'       \item 2: Sandy loam
#'       \item 3: Clay loam
#'       \item 4: Clay
#'       \item 0: No effect of available soil water on production
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
#'   \item \code{foliage}: Type of thinning (above/below) applied to foliage (numeric, default is 1).
#'   \item \code{root}: Type of thinning (above/below) applied to roots (numeric, default is 1).
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
#'   \item \code{light_model}: 1 (default: 3-PGpjs), 2: 3-PGmix.
#'   \item \code{transp_model}: 1 (default: 3-PGpjs), 2: 3-PGmix.
#'   \item \code{phys_model}: 1 (default: 3-PGpjs), 2: 3-PGmix.
#'   \item \code{height_model}: 1 (default: linear), 2: non-linear.
#'   \item \code{correct_bias}: 0 (default: no), 1: yes.
#'   \item \code{calculate_d13c}: 0 (default: no), 1: yes.
#'   \item \code{mort_model}: 1 (default: 3-PGpjs), 2: 3-PGmix.
#'   \item \code{manag_model}: 1 (default: 3-PGpjs thinning based on the number of trees), 2: 3-PGmix based on the biomass proportion.
#' }
#'
#' @return A list with 7 elements: site, species, climate, thinning, parameters, size_dist, and settings.
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_parameters}}, \code{\link{prepare_sizeDist}}, \code{\link{prepare_thinning}}, \code{\link{prepare_climate}}, \code{\link{prepare_site}}
#'
#' @example inst/examples/prepare_input-help.R
#'
#' @references
#' Forrester, D. I., 2020. 3-PG User Manual. Swiss Federal Institute for Forest, Snow and Landscape Research WSL, Birmensdorf, Switzerland. 70 p. Available at the following web site: \url{http://sites.google.com/site/davidforresterssite/home/projects/3PGmix/3pgmixdownload}
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
  parameters = NULL,
  size_dist = NULL,
  settings = NULL
){

  # Settings
  set_def = list(
    light_model = 1, transp_model = 1, phys_model = 1,
    height_model = 1, correct_bias = 0, calculate_d13c = 0,
    mort_model = 1, manag_model = 1
    )
  set_def[names(settings)] <- settings

  # Prepare each table
  # Site
  site = prepare_site(site = site)

  # Species
  if( set_def['mort_model'] == 2 && anyNA(species[, c( "lt_fN","lt_fT","lt_fPhys" )]) ){
    stop('Long-term modifiers (lt_fN, lt_fT, lt_fPhys) must contain non-NA values.')
  }
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

  # Parameters
  parameters = prepare_parameters( parameters = parameters, sp_names = species$species)

  # Size distribution
  if( set_def['correct_bias'] == 1 & is.null(size_dist) ){
    stop('Please provide size_dist table or change the setting to size_dist = 0')
  }
  size_dist = prepare_sizeDist( size_dist = size_dist, sp_names = species$species)


  # return the checked output
  out <- list( site = site, species = species, climate = climate, thinning = thinning, parameters = parameters, size_dist = size_dist, settings = set_def)

  return( out )
}
