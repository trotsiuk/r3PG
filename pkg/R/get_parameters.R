#' @title Get parameter sets
#' @description Gets parameter sets from published studies with 3PG
#'
#' @param mode must be one of the following: full, overview, source, comments,
#'  parameters, sizeDist
#'
#' @param sp_names names of the species. The `sp_names` must be either a scientific name
#' (Picea abies). If passing parameters or sizeDist to mode, it also possible
#' to use a scientific name with a integer, which is the value of parset_id, e.g.
#' Picea abies 37.
#' If unsure, consider using first the option \code{mode = overview} with \code{sp_names = NULL}.
#' See details below for more information.
#'
#' @details This function access the parameter database stored in the package,
#' which is named \code{\link{i_parameters_lit}}, and also accessible without this function.
#' If \code{mode = overview}, a simplified table with 
#' all existing parameter sets will be returned.  A extended version including information
#' about to the source and remarks can be obtained with \code{mode = comments}.
#' A complete description of the source is returned with \code{mode = source}. 
#' The full  version of this table (species,source, comments,
#' parameters) will be returned if \code{mode = full}. Alternatively, with \code{mode = parameters} 
#' and \code{mode = sizeDist}, it is possible to obtain parameters in the format
#' required by \code{\link{run_3PG}}.
#' 
#' Passing species names with \code{sp_names} will selecting from the table
#' the desired species. Please note that one species might have more than one
#' available parameter sets, unless species names contain the parset_id value.
#' Please also note that \code{mode = source} is not compatible with \code{sp_names}.
#'
#' The parameter sets were obtained from published studies. For basic information about this 
#' dataset check in  \code{\link{i_parameters_lit}}. Relevant information about 
#' the parameter sets and the corresponding studies is provided in the table.
#' For further information,  please consider checking the original publications.
#'
#' @return a data frame with parameter sets for all available species, or only the
#'  requested species, if \code{sp_names} is not null.
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{i_parameters_list}}
#'
#' @example inst/examples/get_parameters-help.R
#'
#' @export
#'
get_parameters <- function(
  mode = 'overview',
  sp_names = NULL){
  
  # Check input parameters
  if(!mode %in% c('full', 'overview', 'source', 'comments', 'parameters',
                    'sizeDist') ){
    stop( 'mode must be in of the following: full, overview, source, 
          comments, parameters, sizeDist' )
  }
  
  # columns for each setup
  if(mode == "full"){
    parameters_out <- i_parameters_lit
  }else if(mode == "overview"){
    out_col <- c("parset_id", "species",  "age", "type", "year", "region",
                 "country","source")
    parameters_out <- i_parameters_lit[, out_col]
  }else if(mode == "source"){
    out_col <- c("source", "source_full", "link")
    parameters_out <- i_parameters_lit[, out_col]
    parameters_out <- parameters_out[!duplicated(parameters_out), ]
  }else if(mode == "comments"){
    out_col <- c("parset_id", "species","age", "type", "year", "region",
                 "country","notes", "source",  "source_comments")
    parameters_out <- i_parameters_lit[, out_col]
  }else if(mode == "parameters"){
    out_col <- i_parameters$parameter
    parameters_out <- as.data.frame(cbind(out_col, t(i_parameters_lit[, out_col])))
    parameters_out[, 2:ncol(parameters_out)] <- apply(parameters_out[, 2:ncol(parameters_out)],2, function(x){as.numeric(x)})
    rownames(parameters_out) <- NULL
    colnames(parameters_out) <- c("parameter",
                                  paste(i_parameters_lit[["species"]],
                                        i_parameters_lit[["parset_id"]], sep = " "))

  }else if(mode == "sizeDist"){    
    out_col <- i_sizeDist$parameter
    parameters_out <- as.data.frame(cbind(out_col, t(i_parameters_lit[, out_col])))
    parameters_out[, 2:ncol(parameters_out)] <- apply(parameters_out[, 2:ncol(parameters_out)],2, function(x){as.numeric(x)})
    rownames(parameters_out) <- NULL
    colnames(parameters_out) <- c("parameter",
                                  paste(i_parameters_lit[["species"]],
                                        i_parameters_lit[["parset_id"]], sep = " "))
  
  }
  # Do the subset according to species
  if(!is.null(sp_names)){
    if(mode=="source"){
      stop("Source can be used with sp_names")
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
