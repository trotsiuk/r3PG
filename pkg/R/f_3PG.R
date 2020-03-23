#' @title The function to run the 3-PG in FORTRAN.
#' @description This is the base implementation of the 3-PG model in Fortran. This function shall be used if you are sure that the input is in correct form. We always recomend to use \code{\link{run_3PG}}
#' @param siteInputs \code{matrix} (dim=c(1,8)) containing the information about site level information
#' @param speciesInputs \code{matrix} (dim=c(n_sp,7)) containing the information about species level data. Each row corresponds to one species/layer.
#' @param forcingInputs  \code{matrix} (dim=c(n_m,7)) containing the information about climatic forcing data.
#' @param managementInputs \code{matrix} (dim=c(n_man,5,n_sp)) containing the information about management.
#' @param parameterInputs \code{matrix} (dim=c(82,n_sp)) parameters level data.
#' @param biasInputs \code{matrix} (dim=c(30,n_sp)) bial level data
#' @param n_sp \code{integer} number of species
#' @param n_m \code{integer} number of simulated month
#' @param n_man \code{integer} number of maximum management interventions per species, dimention of managementInputs matrix (min=1)
#' @param t_t \code{vector} number of management interactions for each species
#' @param settings \code{vector} of integers  with all possible settings for the model.
#' @param sp_names \code{character vector} of species names
#' @param df_out \code{logical} if the output shall be in the 4-dimentional array (FALSE) or long data.frame (TRUE)
#'
#' @details Thre is no check for input quality and consistency. Therefore, it is highly recomended to check that your input is in the correct form
#'
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
  biasInputs,
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
    biasInputs = biasInputs,
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
