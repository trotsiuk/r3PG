#' Tranform the output of the fortran 3PG to the long format
#'
#' @param model a list as an output of `run_3PG`
#'
#' @details This is transforming the model output to long format
#'
#' @example inst/examples/run_3PGHelp.R
#' @export
#'
transf_out <- function( model ){

  # simulations
  site <- as.data.frame( model[['site']] )
  species_name <- model[['species_name']]
  out <- model[['sim']]

  # internal variables
  n_ob = dim(out)[1]
  n_sp = dim(out)[2]

  out <- as.data.frame.table( out, stringsAsFactors = F, responseName = 'value')

  out$date <- seq( as.Date( paste(site$year_i, site$month_i+1, 01, sep = '-') ), by = "month", length.out = n_ob) - 1
  out$species <- rep(species_name, each = n_ob)
  out$group <- rep( unique(var_names.default$variable_group), each = n_ob * n_sp)
  out$variable <- rep( var_names.default$variable_name[order(var_names.default$variable_id)], each = n_ob * n_sp)

  out <- out[!out$value %in% -9999,]

  out <- out[,c('date', 'species', 'group', 'variable', 'value')]

  return(out)

}
