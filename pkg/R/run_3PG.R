#' @title Runs a 3-PG model simulation
#'
#' @description Runs the 3-PGpjs (monospecific, evenaged and evergreen forests) or 3-PGmix (deciduous, uneven-aged or mixed-species forests) model. For more details on parameters and structure of input visit \code{\link{prepare_input}}.
#'
#' @param site table as described in \code{\link{prepare_input}} containing the information about site conditions.
#' @param species table as described in \code{\link{prepare_input}} containing the information about species level data. Each row corresponds to one species/cohort.
#' @param climate  table as described in \code{\link{prepare_input}} containing the information about monthly values for climatic data. See also \code{\link{prepare_climate}}
#' @param thinning table as described in \code{\link{prepare_input}} containing the information about thinnings. See also \code{\link{prepare_thinning}}
#' @param parameters table as described in \code{\link{prepare_input}} containing the information about parameters to be modified. See also \code{\link{prepare_parameters}}
#' @param size_dist table as described in \code{\link{prepare_input}} containing the information about size distributions. See also \code{\link{prepare_sizeDist}}
#' @param settings a list as described in \code{\link{prepare_input}} with settings for the model.
#' @param check_input \code{logical} if the input shall be checked for consistency. It will call \code{\link{prepare_input}} function.
#' @param df_out \code{logical} if the output shall be long data.frame (TRUE) the 4-dimensional array (FALSE).
#'
#' @details `r3PG` provides an implementation of the Physiological Processes Predicting Growth \href{https://3pg.forestry.ubc.ca}{3-PG} model, which simulates forest growth and productivity. The `r3PG` serves as a flexible and easy-to-use interface for the `3-PGpjs` (monospecific, evenaged and evergreen forests) and the `3-PGmix` (deciduous, uneven-aged or mixed-species forests) model written in `Fortran`. The package, allows for fast and easy interaction with the model, and `Fortran` re-implementation facilitates computationally intensive sensitivity analysis and calibration. The user can flexibly switch between various options and submodules, to use the original `3-PGpjs` model version for monospecific, even-aged and evergreen forests and the `3-PGmix` model, which can also simulate multi-cohort stands (e.g. mixtures, uneven-aged) that contain deciduous species.
#'
#' This implementation of 3-PG includes several major variants / modifications of the model in particular the ability to switch between 3-PGpjs (the more classic model version for monospecific stands) vs. 3-PGmix (a version for mixed stands), as well as options for bias corrections and \eqn{\delta^13 C} calculations (see parameters).
#'
#' @note The \code{run_3PG} also checks the quality of input data. When names, or structures are not consistent with requirements it will return an error. Turn this off to optimize for speed.
#'
#' @return either a 4-dimentional array or a data.frame, depending on the parameter \code{df_out}. More details on the output is \code{\link{i_output}}
#'
#' @seealso \code{\link{prepare_input}}, \code{\link{prepare_parameters}}, \code{\link{prepare_sizeDist}}, \code{\link{prepare_thinning}}, \code{\link{prepare_climate}}
#'
#' @example inst/examples/run_3PG-help.R
#'
#' @references
#' Forrester, D. I., 2020. 3-PG User Manual. Swiss Federal Institute for Forest, Snow and Landscape Research WSL, Birmensdorf, Switzerland. 70 p. Available at the following web site: \url{http://sites.google.com/site/davidforresterssite/home/projects/3PGmix/3pgmixdownload}
#'
#' Forrester, D. I., & Tang, X. (2016). Analysing the spatial and temporal dynamics of species interactions in mixed-species forests and the effects of stand density using the 3-PG model. Ecological Modelling, 319, 233–254. \doi{10.1016/j.ecolmodel.2015.07.010}
#'
#'Landsberg, J. J., & Waring, R. H., 1997. A generalised model of forest productivity using simplified concepts of radiation-use efficiency, carbon balance and partitioning. Forest Ecology and Management, 95(3), 209–228. \doi{10.1016/S0378-1127(97)00026-1}
#'
#'Sands, P. J., 2010. 3PGpjs user manual. Available at the following web site: \url{http://3pg.sites.olt.ubc.ca/files/2014/04/3PGpjs_UserManual.pdf}
#'
#' @export
#'
#' @useDynLib r3PG
#'
run_3PG <- function(
  site,
  species,
  climate,
  thinning = NULL,
  parameters = NULL,
  size_dist = NULL,
  settings = NULL,
  check_input = TRUE,
  df_out = TRUE
){

  thinn_null <- is.null(thinning)

  # Check and prepare input if required
  if( check_input ){

    input_checked = prepare_input(site = site, species = species, climate = climate,
      thinning = thinning, parameters = parameters, size_dist = size_dist,
      settings = settings)

    # extract output from the list
    site = input_checked$site
    species = input_checked$species
    climate = input_checked$climate
    thinning = input_checked$thinning
    parameters = input_checked$parameters
    size_dist = input_checked$size_dist
    settings = input_checked$settings

  }

  # Make small adjustments to the input and tranform it to matrix

  # site
  from = as.Date(paste(site$from,"-01",sep=""))
  site$year_i = as.numeric(format(from,'%Y'))
  site$month_i = as.numeric(format(from,'%m'))
  site = site[,c('latitude', 'altitude', 'soil_class', 'asw_i', 'asw_min', 'asw_max', 'year_i', 'month_i')]
  site = as.matrix( site, nrow = 1, ncol = 8)

  # species
  n_sp = dim( species )[1]
  sp_names = species$species
  planted = as.Date(paste(species$planted,"-01",sep=""))
  species$year_p = as.numeric(format(planted,'%Y'))
  species$month_p = as.numeric(format(planted,'%m'))
  species = species[,c('year_p', 'month_p', 'fertility', 'stems_n', 'biom_stem', 'biom_root', 'biom_foliage')]
  species = as.matrix( species, nrow = n_sp, ncol = 7)

  # climate
  n_m = dim(climate)[1]
  climate = climate[,c('tmp_min', 'tmp_max', 'tmp_ave', 'prcp', 'srad', 'frost_days', 'vpd_day', 'co2', 'd13catm')]
  climate = as.matrix( climate, nrow = n_m, ncol = 9)

  # thinning
  n_man = dim(thinning)[1]
  if( thinn_null ){
    t_t = 1L
    }else{
      if( dim(thinning)[3] == 1 ){
        t_t = as.integer( length(thinning[,1,]))
      }else{
        t_t = as.integer( colSums(!is.na(thinning[,1,])) )
      }
    }

  # Parameters
  parameters = as.matrix( parameters[,-1], nrow = 82, ncol = n_sp)

  # Size distribution
  size_dist = as.matrix( size_dist[,-1], nrow = 30, ncol = n_sp)

  # Settings
  settings <- as.integer( unlist(settings) )

  # Run the simulations
  r3PG_out = .Call('s_3PG_c',
    siteInputs = site,
    speciesInputs = species,
    forcingInputs = climate,
    managementInputs = thinning,
    parameterInputs = parameters,
    sizeDistInputs = size_dist,
    n_sp = n_sp,
    n_m = n_m,
    n_man = n_man,
    t_t = t_t,
    settings = settings)


  if( df_out ){
    r3PG_out = transf.out( sim = r3PG_out, sp_names = sp_names, year_i = site[7], month_i = site[8] )
  }

  return( r3PG_out )

}

.onUnload <- function(libpath) {
  library.dynam.unload("r3PG", libpath)
}


transf.out <- function( sim, sp_names, year_i, month_i ){

  # internal variables
  n_ob = dim(sim)[1]
  n_sp = dim(sim)[2]

  sim <- as.data.frame.table( sim, stringsAsFactors = F, responseName = 'value')

  if( month_i == 12){
    year_i = year_i + 1
    month_i = 0
  }

  sim$date <- seq( from = as.Date( paste(year_i, month_i+1, 01, sep = '-') ), by = "month", length.out = n_ob) - 1
  sim$species <- rep(sp_names, each = n_ob)
  sim$group <- rep( unique(var.default$variable_group), each = n_ob * n_sp)
  sim$variable <- rep( var.default$variable_name[order(var.default$variable_id)], each = n_ob * n_sp)

  sim <- sim[!sim$value %in% -9999,]

  sim <- sim[,c('date', 'species', 'group', 'variable', 'value')]

  return(sim)

}
