#' @title Prepare parameters table
#' @description Prepares the parameters table for simulations by either replicating the defaults or updating them with user-provided values.
#'
#' @param parameters A data frame containing parameter values to be modified. Columns must include:
#' \itemize{
#'   \item \code{parameter}: Name of the parameter. Must match the names in \code{i_parameters}.
#'   \item Additional columns: Each column must correspond to a species or cohort name, as defined in the \code{species} table.
#' }
#' If \code{NULL}, all parameters will be set to default values.
#' @param sp_names A character vector of species or cohort names used for the simulations. This must match the names in the \code{species} table.
#'
#' @details
#' This function prepares the parameter table for \code{\link{run_3PG}}.
#' If no parameters are provided, default values from \code{i_parameters} are used. If parameters are provided,
#' the function ensures consistency, updates specified values, and retains defaults for unspecified parameters.
#'
#' @return A data frame with parameters as rows and one column for each species or cohort.
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

  if (any(is.null(sp_names), is.na(sp_names), length(sp_names) == 0L)) {
    stop("sp_names must be provided and correspond to the species table.")
  }

  # Initialize output with default parameters
  parameters_out = i_parameters['parameter']

  parameters_out[sp_names] <- NA_real_

  parameters_out[sp_names] <- i_parameters$default


  if( !is.null(parameters) ){

    # Validate input parameter table
    if (!identical("parameter", colnames(parameters)[1])) {
      stop("The first column name of the parameters table must be 'parameter'.")
    }
    if (!all(parameters$parameter %in% i_parameters$parameter)) {
      stop(
        "Parameter input table must only contain parameters present in `i_parameters`. ",
        "Check `param_info` for more details."
      )
    }

    sp_names_replace = sp_names[sp_names %in% colnames(parameters)]
    parameters_out[match(parameters$parameter, parameters_out$parameter), sp_names_replace] <- parameters[,sp_names_replace]
  }

  return( parameters_out )
}
