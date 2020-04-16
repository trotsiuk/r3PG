#' @title Prepare parameters table
#' @description This function prepares the parameters table, by either replicating the defaults or replicating defaults for each of the species.
#'
#' @param parameters table containing the information about parameters to be modified. Values that are not provided are replaced by defaults.
#' \itemize{
#' \item parameter: name of the parameter, must be consistent in naming with \code{\link{i_parameters}}
#' \item species: each column must correspond to species/cohort id/name, as defined in \code{species} table
#' }
#' @param sp_names names of the species / cohorsts used for the simulations.The `sp_names` must be identical to those from \code{species} table.
#'
#' @details This function prepares the parameter table for the model.
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

    if( !identical( sp_names, colnames(parameters)[-1]) ){
      stop( 'Names or order of species in parameters does not correspond to names or order in sp_names' )
    }

    parameters_out[match(parameters$parameter, parameters_out$parameter), ] <- parameters
  }

  return( parameters_out )
}
