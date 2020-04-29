#' @title Prepare parameters table
#' @description Prepares the parameters table, by either replicating the defaults or replicating defaults for each of the species.
#'
#' @param parameters table containing the information about parameters to be modified. Values that are not provided are replaced by defaults.
#' \itemize{
#' \item parameter: name of the parameter, must be consistent in naming with \code{\link{i_parameters}}
#' \item species: each column must correspond to species/cohort id/name, as defined in \code{species} table
#' }
#' @param sp_names names of the species / cohorts used for the simulations.The `sp_names` must be identical to those from \code{species} table.
#'
#' @details This function prepares the parameter table for \code{\link{run_3PG}}
#'
#' @return a data.frame with 47 rows and columns corresponding to each species.
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_input}}, \code{\link{prepare_sizeDist}}, \code{\link{prepare_thinning}}, \code{\link{prepare_climate}}
#'
#' @example inst/examples/prepare_parameters-help.R
#'
#' @export
#'
prepare_parameters <- function(
  parameters = NULL,
  sp_names = c('Fagus sylvatica', 'Pinus sylvestris')
){

  if( any( is.null(sp_names), is.na(sp_names), length(sp_names)==0L) ){
    stop( 'sp_names must be provided according to the species table.' )
  }

  # Prepare parameters
  parameters_out = param.default['parameter']

  parameters_out[sp_names] <- NA_real_

  parameters_out[sp_names] <- param.default$default


  if( !is.null(parameters) ){

    if( !identical( c("parameter"), colnames(parameters)[1]) ){
      stop( 'First column name of the parameters table must correspond to: parameter' )
    }

    if( !all( parameters$parameter %in% param.default$parameter) ){
      stop( paste0('Parameter input table must contains only parameters presend in: ', paste(param.default$parameter, collapse = ','),'. Check `param_info`` for more details.' ))
    }

    sp_names_replace = sp_names[sp_names %in% colnames(parameters)]
    parameters_out[match(parameters$parameter, parameters_out$parameter), sp_names_replace] <- parameters[,sp_names_replace]
  }

  return( parameters_out )
}
