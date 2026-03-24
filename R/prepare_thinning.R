#' @title Check and prepare thinning information
#' @description Prepares the thinning table for simulations, ensuring data consistency and completeness.
#'
#' @param thinning A data frame containing thinning information. If no thinning is required, set to \code{NULL}. The following columns are required:
#' \itemize{
#'   \item \code{species}: Species or cohort ID/name.
#'   \item \code{age}: Age (years) at which thinning is performed (numeric).
#'   \item \code{stems_n}: Number of trees remaining after thinning (numeric). Or the Proportion of above ground biomass retained following the thinning event (0 to 1).
#'   \item \code{stem}: Type of thinning (above/below) applied to stems (numeric, default is 1).
#'   \item \code{foliage}: Type of thinning (above/below) applied to foliage (numeric, default is 1).
#'   \item \code{root}: Type of thinning (above/below) applied to roots (numeric, default is 1).
#'   \item \code{biom_prop_retained}: Proportion of aboveground biomass retained after thinning.
#' }
#' @param sp_names A character vector of species or cohort names used in the simulation. This must match the species names in the \code{species} table. Required even if \code{thinning = NULL}.
#'
#' @details
#' This function prepares the thinning table for \code{\link{run_3PG}}. If no thinning is specified (\code{thinning = NULL}),
#' it returns a 3-dimensional array of \code{NA} values. Otherwise, it validates the thinning data, converts species names to indices,
#' and formats the table as a 3-dimensional array.
#'
#' @return A 3-dimensional array where the third dimension corresponds to each species.
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_input}}, \code{\link{prepare_parameters}}, \code{\link{prepare_sizeDist}}, \code{\link{prepare_climate}}
#'
#' @example inst/examples/prepare_thinning-help.R
#'
#' @export
#'
prepare_thinning <- function(
  thinning = NULL,
  sp_names = c('Fagus sylvatica', 'Pinus sylvestris')
){

  if( any( is.null(sp_names), is.na(sp_names), length(sp_names)==0L) ){
    stop("sp_names must be provided and correspond to the species table.")
  }

  n_sp <- length(sp_names)
  sp_id <- 1:n_sp
  names(sp_id) <- sp_names

  if( is.null(thinning) ){

    thinning = array(NA_real_, dim = c(1, 6, n_sp)) #!20251114

  } else {

    required_cols <- c("species", "age", "stems_n", "stem", "root", "foliage")
    optional_cols <- c( "biom_prop_retained")

    # Check if all compulsory columns are present
    missing_cols <- setdiff(required_cols, colnames(thinning))
    if (length(missing_cols) > 0) {
      stop(paste("The 'thinning' table is missing the following compulsory columns:", paste(missing_cols, collapse = ", ")))
    }

    if( !any(thinning$species %in% sp_names) ){
      stop("species and sp_names does not match.")
    }

    # Add missing optional columns with NA values
    if (!all(optional_cols %in% colnames(thinning))) {
      missing_optional_cols <- setdiff(optional_cols, colnames(thinning))
      thinning[missing_optional_cols] <- NA_real_
    } 
        
    thinning <- data.frame( thinning )

    # check whether the thinning above/below are within plausible range
    if (any(thinning[ , c("stem", "root", "foliage")] < 0.1 | thinning[ , c("stem", "root", "foliage")] > 8)) {
      stop("Thinning values for stem, root, and foliage must be in the range [0.1, 8].")
    }

    # both supplied → ambiguous
    if (any(!is.na(thinning[, "stems_n"]) & !is.na(thinning[, "biom_prop_retained"]))) {                                                     #!20260123
      stop("Thinning input error: both 'stems_n' and 'biom_prop_retained' were supplied in the same thinning event. Please supply only one." #!20260123
      )
    }

    # neither supplied → undefined
    if (any(is.na(thinning[, "stems_n"]) & is.na(thinning[, "biom_prop_retained"]))) {                                                      #!20260123
      stop("Thinning input error: neither 'stems_n' nor 'biom_prop_retained' was supplied in a thinning event. Please supply exactly one."  #!20260123
      )
    }

    if (length(thinning[, "biom_prop_retained"]) > 0 && any(!is.na(thinning[, "biom_prop_retained"]))) {     #!20260123
      if (any(thinning[, "biom_prop_retained"] < 0 | thinning[, "biom_prop_retained"] > 1, na.rm = TRUE)) {  #!20260123
        stop("Thinning values for biom_prop_retained must be in the range [0, 1].")                          #!20260123
      }
    }

    thinning <- thinning[thinning$species %in% sp_names, ]
    thinning$species <- sp_id[thinning$species] # Map species names to indices

    # Preserve the user-provided row order within each species.
    # This is required when a cohort includes a coppice event (which resets age
    # to 0), so that post-coppice management events are not re-sorted ahead of
    # the coppice. For non-coppice cohorts the user naturally supplies events in
    # ascending age, so the result is identical to the previous age-based sort.
    thinning <- thinning[order(thinning$species), ]

    t_t = as.integer( as.vector( table(thinning[,1]) ) )
    n_man = as.integer( max(t_t) )

    thinning = merge(
      data.frame(species = rep(1:n_sp, each = n_man), thin_n = rep(1:n_man, times = n_sp)),
      cbind(data.frame(thin_n = sequence(t_t)), thinning),
      by=c('species', 'thin_n'),
      all = T
      )

    thinning = thinning[order(thinning$species, thinning$thin_n),]

    thinning = simplify2array(by(thinning[,3:8], thinning[,1], as.matrix)) #!20251114
      }

  if( n_sp > 1 ){
    dimnames(thinning)[[3]] <- sp_names
  }


  return( thinning )
}
