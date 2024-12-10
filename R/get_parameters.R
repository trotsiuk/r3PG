#' @title Get parameter sets from published studies
#' @description Retrieves parameter sets for the 3-PG model from published studies, stored in the package's database.
#'
#' @param mode A character string specifying the desired output. Must be one of:
#' \itemize{
#'   \item \code{"full"}: Full parameter set with all details.
#'   \item \code{"overview"}: Simplified table with key information.
#'   \item \code{"source"}: Summary of sources and references.
#'   \item \code{"comments"}: Parameter details along with notes and source comments.
#'   \item \code{"parameters"}: Parameters formatted for \code{\link{run_3PG}}.
#'   \item \code{"sizeDist"}: Size distribution parameters formatted for \code{\link{run_3PG}}.
#' }
#' @param sp_names A character vector of species names. Can include:
#' \itemize{
#'   \item Scientific names (e.g., \code{"Picea abies"}).
#'   \item Scientific names with a parameter set ID (e.g., \code{"Picea abies 37"}).
#' }
#' If \code{NULL}, retrieves all species (except for \code{mode = "source"}).
#'
#' @details
#' This function accesses the parameter database stored in \code{\link{i_parameters_lit}}, which contains parameter sets from published studies.
#' \itemize{
#'   \item Use \code{mode = "overview"} to get a quick summary of available parameter sets.
#'   \item Use \code{mode = "parameters"} or \code{"sizeDist"} for formatted outputs ready for \code{\link{run_3PG}}.
#'   \item Use \code{sp_names} to filter results by species. Species names can optionally include the parameter set ID.
#' }
#' For detailed descriptions, consult the original publications referenced in \code{\link{i_parameters_lit}}.
#'
#' @return A data frame containing parameter sets based on the selected mode and species.
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{i_parameters_lit}}
#'
#' @example inst/examples/get_parameters-help.R
#'
#' @export
#'
get_parameters <- function(
  mode = 'overview',
  sp_names = NULL){

  # Validate input
  valid_modes <- c("full", "overview", "source", "comments", "parameters", "sizeDist")
  if (!mode %in% valid_modes) {
    stop("Invalid mode. Must be one of: ", paste(valid_modes, collapse = ", "))
  }

  # Select columns based on mode
  if (mode == "full") {
    parameters_out <- i_parameters_lit
  } else if (mode == "overview") {
    out_col <- c("parset_id", "species", "age", "type", "year", "region", "country", "source")
    parameters_out <- i_parameters_lit[, out_col]
  } else if (mode == "source") {
    out_col <- c("source", "source_full", "link")
    parameters_out <- i_parameters_lit[, out_col]
    parameters_out <- parameters_out[!duplicated(parameters_out), ]
  } else if (mode == "comments") {
    out_col <- c("parset_id", "species", "age", "type", "year", "region", "country", "notes", "source", "source_comments")
    parameters_out <- i_parameters_lit[, out_col]
  }else if(mode == "parameters"){
    out_col <- i_parameters$parameter
    out_col <- out_col[out_col %in% colnames( i_parameters_lit) ]
    parameters_out <- as.data.frame(cbind(out_col, t(i_parameters_lit[, out_col])))
    parameters_out[, 2:ncol(parameters_out)] <- apply(parameters_out[, 2:ncol(parameters_out)],2, function(x){as.numeric(x)})
    rownames(parameters_out) <- NULL
    colnames(parameters_out) <- c("parameter",
                                  paste(i_parameters_lit[["species"]],
                                        i_parameters_lit[["parset_id"]], sep = " "))

  }else if(mode == "sizeDist"){
    out_col <- i_sizeDist$parameter
    out_col <- out_col[out_col %in% colnames( i_parameters_lit) ]
    parameters_out <- as.data.frame(cbind(out_col, t(i_parameters_lit[, out_col])))
    parameters_out[, 2:ncol(parameters_out)] <- apply(parameters_out[, 2:ncol(parameters_out)],2, function(x){as.numeric(x)})
    rownames(parameters_out) <- NULL
    colnames(parameters_out) <- c("parameter",
                                  paste(i_parameters_lit[["species"]],
                                        i_parameters_lit[["parset_id"]], sep = " "))
  }


  # Filter by species
  if(!is.null(sp_names)){
    if(mode=="source"){
      stop("Filtering by species is not applicable for mode = 'source'.")
    }else if(mode %in% c("parameters", "sizeDist")){
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
