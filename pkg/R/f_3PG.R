#' @title Run 3-PG in FORTRAN
#' @description A minimal (speed-optimized) interface to run 3-PG in FORTRAN. Only use this function when optimizing for speed, otherwise use \code{\link{run_3PG}}
#' @param siteInputs \code{matrix} (dim=c(1,8)) containing the information about site level information
#' @param speciesInputs \code{matrix} (dim=c(n_sp,7)) containing the information about species level data. Each row corresponds to one species/layer.
#' @param forcingInputs  \code{matrix} (dim=c(n_m,9)) containing the information about climatic forcing data (tmp_min, tmp_max, tmp_ave, prcp, srad, frost_days, vpd_day, co3, d13catm).
#' @param managementInputs \code{matrix} (dim=c(n_man,5,n_sp)) containing the information about management.
#' @param parameterInputs \code{matrix} (dim=c(82,n_sp)) parameters level data.
#' @param sizeDistInputs \code{matrix} (dim=c(30,n_sp)) bial level data
#' @param n_sp \code{integer} number of species
#' @param n_m \code{integer} number of simulated month
#' @param n_man \code{integer} number of maximum management interventions per species, dimention of managementInputs matrix (min=1)
#' @param t_t \code{vector} number of management interactions for each species
#' @param settings \code{vector} of integers  with all possible settings for the model.
#' @param sp_names \code{character vector} of species names
#' @param df_out \code{logical} if the output shall be in the 4-dimentional array (FALSE) or long data.frame (TRUE)
#'
#' @details The purpose of this function is to provide a minimal interface to the 3-PG FORTRAN implementation, without tests and conversions that are performed in the normal \code{\link{run_3PG}} function. You should not use this function, unless you are optimizing for speed.
#'
#' If you use f_3PG, note that there is no check for input quality and consistency

#' @example inst/examples/f_3PGHelp.R
#'
#' @export
#'
#' @useDynLib r3PG
#'
f_3PG <- function(
  siteInputs,
  speciesInputs,
  forcingInputs,
  managementInputs,
  parameterInputs,
  sizeDistInputs,
  n_sp,
  n_m,
  n_man,
  t_t,
  settings = c(1L,1L,1L,0L,0L),
  sp_names,
  df_out = TRUE
){

  f_out <- .Call('s_3PG_c',
    siteInputs = siteInputs,
    speciesInputs = speciesInputs,
    forcingInputs = forcingInputs,
    managementInputs = managementInputs,
    parameterInputs = parameterInputs,
    sizeDistInputs = sizeDistInputs,
    n_sp = n_sp,
    n_m = n_m,
    n_man = n_man,
    t_t = t_t,
    settings = settings)

  sp_info <- data.frame(
    species = sp_names,
    year_i = siteInputs[6],
    month_i = siteInputs[7]
  )

  out <- list(
    sp_info = sp_info,
    sim = f_out
  )


  if( df_out ){
    out <- transf_out( out )
  }

  return(out)
}


.onUnload <- function(libpath) {
  library.dynam.unload("r3PG", libpath)
}
