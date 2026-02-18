#' @title Check and prepare species data for consistency
#' @description Validates and prepares the species table for use in the 3PG model.
#' Ensures the input is consistent, with required columns and valid data ranges.
#'
#' @param species A data frame containing species-level information. Each row corresponds to one species or cohort.
#'
#' Required columns:
#' \itemize{
#'   \item \code{species}: Species or cohort ID/name. Must match species names in \code{thinning}, \code{parameters}, and \code{sizeDist}.
#'   \item \code{planted}: Planting date in "year-month" format (e.g., "2000-01").
#'   \item \code{fertility}: Soil fertility, ranging from 0 to 1.
#'   \item \code{stems_n}: Number of trees per hectare.
#'   \item \code{biom_stem}: Stem biomass (Mg/ha).
#'   \item \code{biom_root}: Root biomass (Mg/ha).
#'   \item \code{biom_foliage}: Initial foliage biomass (Mg/ha). For leafless periods, provide spring foliage biomass.
#' }
#'
#' @details This function is used to validate and prepare the species table for \code{\link{run_3PG}}.
#'
#' @return a data.frame with one row
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_input}}, \code{\link{prepare_parameters}}, \code{\link{prepare_sizeDist}}, \code{\link{prepare_thinning}}, \code{\link{prepare_site}}
#'
#' @example inst/examples/prepare_species-help.R
#'
#' @export
#'
prepare_species <- function(
  species
){

  # Ensure input is a data frame
  species = data.frame(species)

  # Check for non-empty input
  if (nrow(species) == 0) {
    stop("The 'species' table must not be empty.")
  }

  # Define required and optional columns
  cols_compulsory <- c("species","planted","fertility","stems_n","biom_stem","biom_root","biom_foliage")
  #cols_modifier <- c( "lt_fN","lt_fT","lt_fPhys" )

  # Check if all compulsory columns are present
  missing_cols <- setdiff(cols_compulsory, colnames(species))
  if (length(missing_cols) > 0) {
    stop(paste("The 'species' table is missing the following compulsory columns:", paste(missing_cols, collapse = ", ")))
  }

  # Validate compulsory columns
  if (anyNA(species[, cols_compulsory])) {
    stop("The 'species' table contains NA values in compulsory columns.")
  }

  if (any(species$fertility < 0 | species$fertility > 1)) {
    stop("The 'fertility' column must contain values between 0 and 1.")
  }

  if (any(species$stems_n < 0)) {
    stop("The 'stems_n' column must contain non-negative values.")
  }

  if (any(species$biom_stem < 0)) {
    stop("The 'biom_stem' column must contain non-negative values.")
  }

  if (any(species$biom_root < 0)) {
    stop("The 'biom_root' column must contain non-negative values.")
  }

  if (any(species$biom_foliage < 0)) {
    stop("The 'biom_foliage' column must contain non-negative values.")
  }

  if (any(species$biom_stem > 10000)) {
    warning("Some values in 'biom_stem' are greater than 10000. Please verify the input data.")
  }


  ## Handle optional columns
  #if (!all(cols_modifier %in% colnames(species))) {
#
  #  # Add missing optional columns with NA values
  #  missing_modifiers <- setdiff(cols_modifier, colnames(species))
  #  species[missing_modifiers] <- NA_real_
#
  #  } else {
#
  #  # Validate optional columns if present
  #  if (!all(is.na(species[, cols_modifier]))) {
#
  #    if (any(species[, cols_modifier] < 0, na.rm = TRUE)) {
  #      stop("Long-term modifiers (lt_fN, lt_fT, lt_fPhys) must contain non-negative values.")
  #    }
  #  }
  #}


  # Select and return the final table
  #final_columns <- c(cols_compulsory, cols_modifier)
  final_columns <- c(cols_compulsory)
  return(species[, final_columns, drop = FALSE])
}


