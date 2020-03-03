#' Define the function to run 3PG in R
#'
#' @description This is the base implementation of the 3-PG model in Fortran. This function shall be used if you are sure that the input is in correct form. We always recomend to use \code{\link{run_3PG}}
#'
#' @param siteInputs a \code{matrix} (dim=c(1,8)) containing the information about site level information
#' @param speciesInputs a \code{matrix} (dim=c(n_sp,7)) containing the information about species level data. Each row corresponds to one species/layer.
#' @param forcingInputs  a \code{matrix} (dim=c(n_m,7)) containing the information about climatic forcing data.
#' @param managementInputs a \code{matrix} (dim=c(n_man,5,n_sp)) containing the information about management.
#' @param parameterInputs a \code{matrix} (dim=c(82,n_sp)) parameters level data.
#' @param biasInputs a \code{matrix} (dim=c(30,n_sp)) bial level data
#' @param n_sp \code{integer} number of species
#' @param n_m \code{integer} number of simulated month
#' @param n_man \code{integer} number of management interventions, dimention of managementInputs matrix (min1)
#' @param t_t \code{vector} number of management interactions for each species
#' @param settings \code{vector} of integers  with all possible settings of the model.
#'
#' @details This is the model
#'
#' @example inst/examples/f_3PGHelp.R
#' @export
#' @useDynLib r3PGmix
#'
f_3PG <- function(
  siteInputs,
  speciesInputs,
  forcingInputs,
  managementInputs,
  parameterInputs,
  biasInputs,
  n_sp,
  n_m,
  n_man,
  t_t,
  settings = c(1L,1L,1L,0L,0L)
){

  f_out <- .Call('s_3PG_c',
    siteInputs = siteInputs,
    speciesInputs = speciesInputs,
    forcingInputs = forcingInputs,
    managementInputs = managementInputs,
    parameterInputs = parameterInputs,
    biasInputs = biasInputs,
    n_sp = n_sp,
    n_m = n_m,
    n_man = n_man,
    t_t = t_t,
    settings = settings)

  out <- list(
    site = siteInputs,
    species = speciesInputs,
    species_name = colnames(parameterInputs),
    sim = f_out
  )
  return(out)

}


.onUnload <- function(libpath) {
  library.dynam.unload("r3PGmix", libpath)
}
