#' @title Check and prepare defoliation information
#' @description Prepares the defoliation table for simulations, ensuring data consistency and completeness.
#'
#' @param defoliation A data frame containing defoliation information. If no defoliation is required, set to \code{NULL}. The following columns are required:
#' \itemize{
#'   \item \code{species}: Species or cohort ID/name.
#'   \item \code{age}: Age (years) at which defoliation occurs (numeric).
#'   \item \code{stem_retained}: Proportion of stem mass retained after defoliation (0 to 1).
#'   \item \code{foliage_retained}: Proportion of foliage mass retained after defoliation (0 to 1).
#'   \item \code{root_retained}: Proportion of root mass retained after defoliation (0 to 1).
#'   \item \code{stem}: Fraction of average tree stem mass of killed trees (numeric, default 1).
#'   \item \code{t_recover}: Time (months) to recover from defoliation (numeric).
#'   \item \code{prop_carbs}: Proportion of pre-defoliation carbs used to regenerate foliage (0 to 1).
#'   \item \code{prop_npp}: Proportion of new photosynthate allocated to foliage (0 to 1).
#' }
#' @param sp_names A character vector of species or cohort names used in the simulation. Must match \code{species} names in the input.
#'
#' @return A 3-dimensional array of defoliation parameters, indexed by [event, parameter, species].
#' @export
prepare_defoliation <- function(defoliation = NULL,
                                sp_names = c('Fagus sylvatica', 'Pinus sylvestris')) {

  if (any(is.null(sp_names), is.na(sp_names), length(sp_names) == 0L)) {
    stop("sp_names must be provided and correspond to the species table.")
  }

  n_sp <- length(sp_names)
  sp_id <- 1:n_sp
  names(sp_id) <- sp_names

  required_cols <- c("species", "age", "stem_retained", "foliage_retained", "root_retained",
                     "stem", "t_recover", "prop_carbs", "prop_npp")

  if (is.null(defoliation)) {
    defoliation <- array(NA_real_, dim = c(1, length(required_cols) - 1, n_sp))  # drop species column
  } else {
    if (!identical(required_cols, colnames(defoliation))) {
      stop(paste("Column names of defoliation table must be:", paste(required_cols, collapse = ", ")))
    }

    defoliation <- data.frame(defoliation)
    defoliation <- defoliation[defoliation$species %in% sp_names, ]
    defoliation$species <- sp_id[defoliation$species]
    defoliation <- defoliation[order(defoliation$species, defoliation$age), ]

    t_t <- as.integer(as.vector(table(defoliation[, "species"])))
    n_def <- max(t_t)

    defoliation <- merge(
      data.frame(species = rep(1:n_sp, each = n_def), def_n = rep(1:n_def, times = n_sp)),
      cbind(data.frame(def_n = sequence(t_t)), defoliation),
      by = c("species", "def_n"),
      all = TRUE
    )

    defoliation <- defoliation[order(defoliation$species, defoliation$def_n), ]
    defoliation <- simplify2array(by(defoliation[, 3:10], defoliation[, 1], as.matrix))
  }

  if (n_sp > 1) {
    dimnames(defoliation)[[3]] <- sp_names
  }

  return(defoliation)
}