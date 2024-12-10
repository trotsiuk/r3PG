#' @title Prepare size distribution table
#' @description Prepares the size distribution table for simulations by either replicating defaults or updating them with user-provided values.
#'
#' @param size_dist A data frame containing size distribution values to be modified. Columns must include:
#' \itemize{
#'   \item \code{parameter}: Name of the parameter. Must match the names in \code{i_sizeDist}.
#'   \item Additional columns: Each column must correspond to a species or cohort name, as defined in the \code{species} table.
#' }
#' If \code{NULL}, all parameters will be set to default values.
#' @param sp_names A character vector of species or cohort names used for the simulations. This must match the names in the \code{species} table.
#'
#' @details
#' This function prepares the size distribution table for \code{\link{run_3PG}}.
#' If no size distribution values are provided, default values from \code{i_sizeDist} are used. If values are provided,
#' the function ensures consistency, updates specified values, and retains defaults for unspecified parameters.
#'
#' @return A data frame with rows for each parameter and one column for each species or cohort.
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_input}}, \code{\link{prepare_parameters}}, \code{\link{prepare_thinning}}, \code{\link{prepare_climate}}
#'
#' @example inst/examples/prepare_sizeDist-help.R
#'
#' @export
#'
prepare_sizeDist <- function(
  size_dist = NULL,
  sp_names = c('Fagus sylvatica', 'Pinus sylvestris')
){

  if (any(is.null(sp_names), is.na(sp_names), length(sp_names) == 0L)) {
    stop("sp_names must be provided and correspond to the species table.")
  }

  # Initialize output with default size distribution
  size_dist_out = i_sizeDist['parameter']

  size_dist_out[sp_names] <- NA_real_

  size_dist_out[sp_names] <- as.numeric( i_sizeDist$default )


  if( !is.null(size_dist) ){

    # Validate input size_dist table
    if (!identical("parameter", colnames(size_dist)[1])) {
      stop("The first column name of the size_dist table must be 'parameter'.")
    }
    if (!all(size_dist$parameter %in% i_sizeDist$parameter)) {
      stop(
        "size_dist input table must only contain parameters present in `i_sizeDist`. ",
        "Check `param_info` for more details."
      )
    }

    sp_names_replace = sp_names[sp_names %in% colnames(size_dist)]
    size_dist_out[match(size_dist$parameter, size_dist_out$parameter), sp_names_replace] <- size_dist[,sp_names_replace]

  }

  return( size_dist_out )
}
