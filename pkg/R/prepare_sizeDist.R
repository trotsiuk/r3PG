#' @title Prepare parameters table
#' @description This function prepares the parameters table, by either replicating the defaults or replicating defaults for each of the species.
#'
#' @param size_dist a \code{data frame} or \code{matrix} containing the size distribution parameters to be modified. Values that are not provided are replaced by defaults.
#' \itemize{
#' \item parameter: name of the parameter, shall be consistent in naming with \code{\link{param_info}}
#' \item species: each column correspond to species/cohort id/name, as defined in \code{speciesInputs} table
#' }
#' @param sp_names names of the species / cohorsts used for the simulations.The `sp_names` shall be identical to those from \code{species} table.
#'
#' @details This function prepares the parameter table for the model.
#'
#' @example inst/examples/prepare_sizeDist-help.R
#'
#' @export
#'
prepare_sizeDist <- function(
  size_dist = NULL,
  sp_names = c('Fagus sylvatica', 'Pinus sylvestris')
){

  if( any( is.null(sp_names), is.na(sp_names), length(sp_names)==0L) ){
    stop( 'sp_names shall be provided according to the species table.' )
  }

  # Prepare parameters
  size_dist_out = sizeDist.default['parameter']

  size_dist_out[sp_names] <- NA_real_

  size_dist_out[sp_names] <- as.numeric( sizeDist.default$default )


  if( !is.null(size_dist) ){

    if( !identical( c("parameter"), colnames(size_dist)[1]) ){
      stop( 'First column name of the parameters table shall correspond to: parameter' )
    }

    if( !all( size_dist$parameter %in% sizeDist.default$parameter) ){
      stop( paste0('size_dist input table shall contains only parameters presend in: ', paste(sizeDist.default$parameter, collapse = ','),'. Check `param_info`` for more details.' ))
    }

    if( !identical( sp_names, colnames(size_dist)[-1]) ){
      stop( 'Names or order of species in size_dist table does not correspond to names or order in sp_names' )
    }

    size_dist_out[match(size_dist$parameter, size_dist_out$parameter), ] <- size_dist
  }

  return( size_dist_out )
}
