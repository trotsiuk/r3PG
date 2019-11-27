#' Define the function to run 3PGN in R
#'
#' @param siteInputs site level data
#' @param speciesInputs species level data
#' @param forcingInputs forcing level data
#' @param parameterInputs parameters level data
#' @param biasInputs bial level data
#' @param settings settings for the model
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
  settings = list(fit_dbh_dist = 0L)
){

  n_sp = as.integer( nrow(speciesInputs) )
  n_m = as.integer( nrow(forcingInputs) )

  out <- .Call('s_3PG_c',
    siteInputs = as.matrix( siteInputs, nrow = 1, ncol = 8),
    speciesInputs = as.matrix( speciesInputs, nrow = n_sp, ncol = 8),
    forcingInputs = as.matrix( forcingInputs, nrow = n_m, ncol = 6),
    parameterInputs = as.matrix( parameterInputs, nrow = 65, ncol = n_sp),
    biasInputs = as.matrix( biasInputs, nrow = 47, ncol = n_sp),
    n_sp = n_sp,
    n_m = n_m,
    f_dbh_dist = settings$f_dbh_dist)

  return(out)

}


.onUnload <- function(libpath) {
  library.dynam.unload("r3PGmix", libpath)
}