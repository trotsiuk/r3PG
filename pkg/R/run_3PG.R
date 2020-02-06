#' Define the function to run 3PGN in R
#'
#' @param siteInputs a \code{data frame} or \code{matrix} containing the information about site level data in the following order:
#' \itemize{
#' \item latitude: Site latitude in the WGS84 coordinate system
#' \item soil_class: \code{integer} representing the soil class, according to table ...
#' \item asw_i: initial soil water
#' \item asw_min: minimum soil water
#' \item asw_max: maximum soil water
#' \item year_i: \code{integer} year when the simulation will start
#' \item momth_i: \code{integer} month when simulation will start, first month in the output file
#' \item altitude: altitude of the site, m a.s.l.
#' }
#' @param speciesInputs a \code{data frame} or \code{matrix} containing the information about species level data. Each row corresponds to one species/layer. The following order apply:
#' \itemize{
#' \item species: \code{integer} species id
#' \item year_p: \code{integer} year when the species was planted (from this we calculate species age)
#' \item month_p: \code{integer} month when species was planted. Assumption is that species was planted in the end of this month. E.g. if species is planted in January 2000, then in 31 April 2000 it will be 3 month.
#' \item fertility: soil fertility for a given species. Range from 0 to 1.
#' \item biom_foliage_i: initial foliage biomass (T/ha). If this is a leafless period provide a foliage biomass in spring.
#' \item biom_root_i: root biomass for a given species.
#' \item biom_stem_i: stem biomass for a given species
#' \item n_trees: number of trees per ha.
#' }
#' @param forcingInputs  a \code{data frame} or \code{matrix} containing the information about climatic forcing data. First month shall corresponds to \code{year_i month_i}:
#' \itemize{
#' \item tmp_min:
#' \item tmp_max:
#' \item prcp:
#' \item sol_rad:
#' \item frost_days:
#' \item co2:
#' \item d13catm:
#' }
#' @param parameterInputs parameters level data
#' @param biasInputs bial level data
#' @param settings list with all possible settings of the model. All provided as \code{integer}
#' \itemize{
#' \item light_model: `1` - 3PGmix; `2` - 3PGpjs
#' \item water_balance: `1` - 3PGmix; `2` - 3PGpjs
#' \item phys_model:  `1` - 3PGmix; `2` - 3PGpjs
#' \item correct_bias: `0` - no; `1` - 3PGmix
#' \item calculate_d13c: `0` - no; `1` - 3PGmix
#' }
#'
#' @details This is the model
#'
#'
#' @export
#' @useDynLib r3PGmix
#'
run_3PG <- function(
  siteInputs,
  speciesInputs,
  forcingInputs,
  parameterInputs,
  biasInputs,
  settings = list(light_model = 1L, water_balance = 1L, phys_model = 1L, correct_bias = 0L, calculate_d13c = 0L)
){

  # replace default settings
  set_def = list(light_model = 1L, water_balance = 1L, phys_model = 1L, correct_bias = 0L, calculate_d13c = 0L)
  set_def[names(settings)] <- settings

  n_sp = as.integer( nrow(speciesInputs) )
  n_m = as.integer( nrow(forcingInputs) )

  out <- .Call('s_3PG_c',
    siteInputs = as.matrix( siteInputs, nrow = 1, ncol = 8),
    speciesInputs = as.matrix( speciesInputs, nrow = n_sp, ncol = 8),
    forcingInputs = as.matrix( forcingInputs, nrow = n_m, ncol = 7),
    parameterInputs = as.matrix( parameterInputs, nrow = 65, ncol = n_sp),
    biasInputs = as.matrix( biasInputs, nrow = 47, ncol = n_sp),
    n_sp = n_sp,
    n_m = n_m,
    settings = unlist(set_def))

  return(out)

}


.onUnload <- function(libpath) {
  library.dynam.unload("r3PGmix", libpath)
}