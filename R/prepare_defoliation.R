#' @title Check and prepare defoliation information
#' @description Prepares the defoliation table for simulations, ensuring data consistency and completeness.
#'
#' @param defoliation A data frame containing defoliation information. If no defoliation is required, set to \code{NULL}. The following columns are required:
#' \itemize{
#'   \item \code{species}: Species or cohort ID/name.
#'   \item \code{age}: Age (years) at which defoliation occurs (numeric).
#'   \item \code{def_type}: Defoliation type: 1 - prune; 2 - copice; 3 - epicormic; 4 - stand replacing
#'   \item \code{stem_retained}: Proportion of stem mass retained after defoliation (0 to 1).
#'   \item \code{foliage_retained}: Proportion of foliage mass retained after defoliation (0 to 1).
#'   \item \code{root_retained}: Proportion of root mass retained after defoliation (0 to 1).
#'   \item \code{stem}: Fraction of average tree stem mass of killed trees (numeric, default 1).
#'   \item \code{def_recover_t}: Time (months) to recover from defoliation (numeric). After this time growth is only from npp (not non-structural carbohydrates), and biomass partitioning returns to normal.
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

  required_cols <- c("species", "age", "def_type", "stem_retained", "foliage_retained", "root_retained",
                     "stem", "def_recover_t", "prop_carbs", "prop_npp")

  if (is.null(defoliation)) {

    defoliation <- array(NA_real_, dim = c(1, length(required_cols) - 1, n_sp))  # drop species column
#############    defoliation <- array(
#############      rep(
#############        c(
#############          age = NA_real_,
#############          def_type = 0,
#############          stem_retained = 1,
#############          foliage_retained = 1,
#############          root_retained = 1,
#############          stem = 1,
#############          def_recover_t = 0,
#############          prop_carbs = 0,
#############          prop_npp = 0
#############        ),
#############        times = n_sp
#############      ),
#############      dim = c(1, length(required_cols) - 1, n_sp),
#############      dimnames = list(
#############        def_n = 1,
#############        param = required_cols[-1],
#############        species = sp_names
#############      )
#############    )

  } else {

    if (!identical(required_cols, colnames(defoliation))) {
      stop(paste("Column names of defoliation table must be:", paste(required_cols, collapse = ", ")))
    }

    if( !any(defoliation$species %in% sp_names) ){
      stop("species and sp_names does not match.")
    }


    # pruning (def_type = 1)
    if (any(
      defoliation$def_type == 1 &
      (defoliation$stem_retained != 1 |
       defoliation$root_retained != 1 |
       defoliation$foliage_retained >= 1)
    )) {
      stop("Defoliation input error (pruning, def_type = 1): ",
           "stem_retained and root_retained must equal 1, ",
           "and foliage_retained must be < 1.")
    }

    # coppice (def_type = 2)
    if (any(
      defoliation$def_type == 2 &
      (defoliation$stem_retained != 0 |
       defoliation$root_retained <= 0 |
       defoliation$foliage_retained != 0)
    )) {
      stop("Defoliation input error (coppice, def_type = 2): ",
           "stem_retained must be 0, root_retained must be > 0, ",
           "and foliage_retained must equal 0.")
    }

    # epicormic (def_type = 3)
    if (any(
      defoliation$def_type == 3 &
      (defoliation$stem_retained <= 0 |
       defoliation$root_retained <= 0 |
       defoliation$stem_retained != defoliation$root_retained |
       defoliation$foliage_retained >= 1)
    )) {
      stop("Defoliation input error (epicormic, def_type = 3): ",
           "stem_retained must equal root_retained, both > 0, ",
           "and foliage_retained must be < 1.")
    }

    # stand replacing (def_type = 4)
    if (any(
      defoliation$def_type == 4 &
      (defoliation$stem_retained != 0 |
       defoliation$root_retained != 0 |
       defoliation$foliage_retained != 0)
    )) {
      stop("Defoliation input error (stand replacing, def_type = 4): ",
           "stem_retained, root_retained, and foliage_retained must all equal 0.")
    }


    # check whether the thinning above/below are within plausible range
    if (any(defoliation$stem < 0.2 | defoliation$stem > 5)) {
      stop("Defoliation values for stem must be in the range [0.2, 5].")
    }


    if (any(is.na(defoliation$def_recover_t) | defoliation$def_recover_t < 2)) {
      stop("Defoliation input error: 'def_recover_t' must be provided and >= 2 months for all defoliation events.")
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
    defoliation <- simplify2array(by(defoliation[, 3:11], defoliation[, 1], as.matrix))

  }

  if (n_sp > 1) {
    dimnames(defoliation)[[3]] <- sp_names
  }

  return(defoliation)
}