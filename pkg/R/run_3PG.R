#' Define the function to run 3PGN in R
#'
#' @param siteInputs site level data
#' @param speciesInputs species level data
#' @param forcingInputs forcing level data
#' @param parameterInputs parameters level data
#' @param biasInputs bial level data
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
  biasInputs
){

  n_sp = as.integer( nrow(speciesInputs) )
  n_m = as.integer( nrow(forcingInputs) )

  out <- .Call('model_f_C',
    siteInputs = siteInputs,
    speciesInputs = speciesInputs,
    forcingInputs = forcingInputs,
    parameterInputs = parameterInputs,
    biasInputs = biasInputs,
    n_sp = n_sp,
    n_m = n_m)

  return(out)

}


.onUnload <- function(libpath) {
  library.dynam.unload("r3PGmix", libpath)
}