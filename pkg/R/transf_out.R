#' Define the function to run 3PGN in R
#'
#' @param out the 4 dimentional array as output of `run_3PG`
#' @param day_start the first date of the simulation, in date format
#'
#' @details This is transforming the model output to long format
#'
#'
#' @export
#'

transf_out <- function( out, day_start = as.Date('2010-01-31') ){

  # internal variables
  n_ob = dim(out)[1]
  n_sp = dim(out)[2]

  out <- as.data.frame.table( out, stringsAsFactors = F, responseName = 'value')

  out$date <- seq( ceiling_date( day_start, "month"), by = "month", length.out = n_ob) - 1
  out$species <- rep( paste0('sp_', 1:n_sp), each = n_ob)
  out$group <- rep( unique(var_names$variable_group), each = n_ob * n_sp)
  out$variable <- rep( var_names$variable_name[order(var_names$variable_id)], each = n_ob * n_sp)

  out <- out[!out$value %in% -9999,]

  out <- out[,c('date', 'species', 'group', 'variable', 'value')]

  return(out)

}
