#' Define the function to run 3PG in R
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
#' \item Altitude: altitude of the site, m a.s.l.
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
#' @param managementInputs a \code{data frame} or \code{matrix} containing the information about management. The following order apply:
#' \itemize{
#' \item species: \code{integer} species id
#' \item age: \code{real} age at which thinning is done
#' \item n_trees: \code{integer} number of trees remaining after thinning.
#' \item foliage_type: Type of thinning (above/below). Default is 1.
#' \item root_type: Type of thinning (above/below). Default is 1.
#' \item stem_type: Type of thinning (above/below). Default is 1.
#' }
#' @param parameterInputs parameters level data
#' @param biasInputs bial level data
#' @param settings list with all possible settings of the model. All provided as \code{integer}
#' \itemize{
#' \item light_model: `1` - 3PGpjs; `2` - 3PGmix
#' \item transp_model: `1` - 3PGpjs; `2` - 3PGmix
#' \item phys_model:  `1` - 3PGpjs; `2` - 3PGmix
#' \item correct_bias: `0` - no; `1` - 3PGmix
#' \item calculate_d13c: `0` - no; `1` - 3PGmix
#' }
#'
#' @details This is the model
#'
#' @example inst/examples/run_3PGHelp.R
#' @export
#' @useDynLib r3PGmix
#'
run_3PG <- function(
  siteInputs,
  speciesInputs,
  forcingInputs,
  managementInputs = NULL,
  parameterInputs,
  biasInputs,
  settings = list(light_model = 1, transp_model = 1, phys_model = 1, correct_bias = 0, calculate_d13c = 0)
){

  # replace default settings
  set_def = list(light_model = 1, transp_model = 1, phys_model = 1, correct_bias = 0, calculate_d13c = 0)
  set_def[names(settings)] <- settings
  set_def <- as.integer( unlist(set_def) )

  n_sp = as.integer( nrow(speciesInputs) )
  n_m = as.integer( nrow(forcingInputs) )

  # Prepare thinning
  if( is.null(managementInputs) ) {
    t_t = 0L
    n_man = 1L
    thin_mat = array(NA_real_, dim = c(1,5,n_sp))
  } else {
    t_t = as.integer( as.vector( table(managementInputs[,1]) ) )
    n_man = as.integer( max(t_t) )

    thin_mat <- merge( data.frame(species = rep(1:n_sp, each = n_man), thin_n = rep(1:n_man, times = n_sp)),
      cbind(data.frame(thin_n = sequence(t_t)), managementInputs), by=c('species', 'thin_n'), all = T)

    thin_mat <- simplify2array(by(thin_mat[,3:7], thin_mat[,1], as.matrix))
  }


  f_out <- .Call('s_3PG_c',
    siteInputs = as.matrix( siteInputs, nrow = 1, ncol = 8),
    speciesInputs = as.matrix( speciesInputs, nrow = n_sp, ncol = 8),
    forcingInputs = as.matrix( forcingInputs, nrow = n_m, ncol = 7),
    managementInputs = thin_mat,
    parameterInputs = as.matrix( parameterInputs, nrow = 65, ncol = n_sp),
    biasInputs = as.matrix( biasInputs, nrow = 47, ncol = n_sp),
    n_sp = n_sp,
    n_m = n_m,
    n_man = n_man,
    t_t = t_t,
    settings = set_def)

  out <- list(
    site = siteInputs,
    species = speciesInputs,
    sim = f_out
  )
  return(out)

}


.onUnload <- function(libpath) {
  library.dynam.unload("r3PGmix", libpath)
}