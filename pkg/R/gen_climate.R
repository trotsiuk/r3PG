#' @title The function generate climate table.
#' @description This function replicate the average climate for the required number of years.
#' @param climate  a \code{data frame} or \code{matrix} containing the information about climatic forcing data. Shall have 12 rows, one for each month.
#' \itemize{
#' \item tmp_min: monthly mean daily minimum temperature (C).
#' \item tmp_max: monthly mean daily maximum temperature (C).
#' \item prcp: monthly rainfall (mm month-1).
#' \item srad: monthly mean daily solar radiation (MJ m-2 d-1).
#' \item frost_days: frost days per month (d month-1).
#' \item co2: required if calculate_d13c=1
#' \item d13catm: required if calculate_d13c=1
#' }
#' @param n_years for how many years to repeat the climate
#' @param month_i first month of the simulations.
#'
#' @details This function shall be used in case user provide only average climate and want to repeate it for each year of simualtion.
#'
#' @example inst/examples/f_3PGHelp.R
#'
#' @export
#'
gen_climate <- function(
  climate,
  n_years = 10,
  month_i = 1
){

  if( !identical(c("tmp_min","tmp_max","prcp","srad","frost_days"), colnames(climate)[1:5]) ){
    stop( 'First five columns names of the forcingInputs table shall correspond to: tmp_min,tmp_max,prcp,srad,frost_days' )
  }

  out = do.call("rbind", replicate(n_years, climate, simplify = FALSE))

  if( month_i > 1 ){
    out = out[-c(1:(month_i-1)),]
  }

  return( out )
}


