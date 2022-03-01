#' @title Get parameter sets
#' @description Gets parameters sets for species from published studies
#'
#' @param df_out must be one of the following: overview, source, full, run.
#' 
#' @param sp_names names of the species. The `sp_names` must be either a scientific name
#' (Picea abies) or a scientific name with a integer (Picea abies 37) with df_out = run.
#' If unsure, consider using first the option df_out = overview with no species.
#' See details below for more information.
#' 
#' @details This function access the internal parameter database stored in the package. If 
#' df_out = overview, a simplified table with all existing parameter sets will be returned. 
#' A extended version including information related to the source can be obtained
#' with df_out = source. The complete version of this table (species,source, comments,
#' parameters) will be returned if df_out = full. 
#' Alternatively, with df_out = run, it is possible to
#' obtain only the parameters in the format required by \code{\link{run_3PG}}.
#' Passing species names with sp_names means selecting from the table
#' the desired species. Please note that one species might have more than one 
#' available parameter sets.
#' 
#' The parameter sets were obtained from published studies. Basic information about the
#' sets and the corresponding studies is provided in the table. For further information, 
#' please consider checking the original publication. 
#'
#' @return a data.frame with parameter sets for all available species, or only the
#'  requested species, if sp_names is not null.
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{i_parameters}}
#' 
#' @example inst/examples/get_parameters-help.R
#'
#' @export
#'
get_parameters <- function(
  df_out = 'overview',
  sp_names = NULL
){
  # Check input parameters
  if(!df_out %in% c('full', 'run', 'overview', 'source') ){
    stop( 'df_out must be in of the following: overview, source, full, run' )
  }
  
  # Obtain the required table
  parameters_out <- param.db[[df_out]]
  
  # Do the subset according to species
  if(!is.null(sp_names)){
    if(df_out == 'run'){
      # Check names
      sp_mask_1 <- sp_names %in% gsub( ' [0-9]+', '', colnames(parameters_out))
      sp_mask_2 <- sp_names %in% colnames(parameters_out)
      sp_mask <- Reduce(`|`, list(sp_mask_1, sp_mask_2))
      if (!any(sp_mask)){
        stop('sp_names not in parameters or invalid sp_names')
      }else if (!all(sp_mask) & any(sp_mask)){
        warning('Some sp_names are invalid or not in parameters')
        warning(paste(sp_names[!sp_mask], collapse = ', '))
      }
      sp_names <- sp_names[sp_mask]
      # Subset to existent species
      sp_mask_1 <- gsub( ' [0-9]+', '', colnames(parameters_out))  %in% sp_names
      sp_mask_2 <- colnames(parameters_out)  %in% sp_names
      sp_mask <- Reduce(`|`, list(sp_mask_1, sp_mask_2))
      parameters_out <- parameters_out[, c('parameter', colnames(parameters_out)[sp_mask])]

    }else{
      # Check names
      sp_mask <-sp_names %in% parameters_out[, 'species']
      if (!any(sp_mask)){
        stop('sp_names not in parameters or invalid sp_names')
      }else if (!all(sp_mask) & any(sp_mask)){
        warning('Some sp_names are invalid or not in parameters')
        warning(paste(sp_names[!sp_mask], collapse = ', '))
      }
      sp_names <- sp_names[sp_mask]
      # Subset to existent species
      sp_mask <- parameters_out[, 'species'] %in% sp_names
      parameters_out <- parameters_out[sp_mask, ] 
    }
  }
  return(parameters_out)
}
