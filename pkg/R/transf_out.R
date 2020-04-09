#' @title The function to tranform the output to the long format
#'
#' @description This function tranform the output of the \code{\link{run_3PG}} or \code{\link{f_3PG}} functions into the long format dataframe. This allow easy and fast exploration or manipulation of the output data.
#'
#' @param model a 2-dimentinal list as an output of \code{\link{run_3PG}} or \code{\link{f_3PG}} function.
#'
#' @details Function transform the model simulation from 4-dimentional array to long format dataframe with the following columns: \code{date; species; group; variable; value}.
#'
#' @example inst/examples/run_3PGHelp.R
#'
#' @seealso \code{\link{run_3PG}}
#'
#' @export
#'
transf_out <- function( model ){

  # simulations
  species_name <- model[['sp_info']]$species
  year_i <- model[['sp_info']]$year_i[1]
  month_i <- model[['sp_info']]$month_i[1]

  out <- model[['sim']]

  # internal variables
  n_ob = dim(out)[1]
  n_sp = dim(out)[2]

  out <- as.data.frame.table( out, stringsAsFactors = F, responseName = 'value')

  out$date <- seq( as.Date( paste(year_i, month_i+1, 01, sep = '-') ), by = "month", length.out = n_ob) - 1
  out$species <- rep(species_name, each = n_ob)
  out$group <- rep( unique(var.default$variable_group), each = n_ob * n_sp)
  out$variable <- rep( var.default$variable_name[order(var.default$variable_id)], each = n_ob * n_sp)

  out <- out[!out$value %in% -9999,]

  out <- out[,c('date', 'species', 'group', 'variable', 'value')]

  return(out)

}
