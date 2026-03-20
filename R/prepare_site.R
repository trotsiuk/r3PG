#' @title Check the site data for consistency
#' @description Validates and prepares the site table for use in the 3PG model. Ensures that the input information is consistent and formatted correctly.
#'
#' @param site A data frame containing site-level data. It must contain exactly one row with the following columns:
#' \itemize{
#'   \item \code{latitude}: Site latitude in the WGS84 coordinate system (degrees, range: [-90, 90]).
#'   \item \code{elevation}: Site elevation (meters above sea level, range: [0, 4000]).
#'   \item \code{soil_class}: Soil class as per 3PG User Manual Table 27:
#'     \itemize{
#'       \item 1: Clay
#'       \item 2: Clay loam
#'       \item 3: Loam
#'       \item 4: Loamy sand
#'       \item 5: Sand
#'       \item 6: Sandy clay
#'       \item 7: Sandy clay loam
#'       \item 8: Sandy loam
#'       \item 9: Silt
#'       \item 10: Silty clay
#'       \item 11: Silty clay loam
#'       \item 12: Silty loam
#'       \item 0: Uses cθ and nθ provided in the “parameters” input
#'     }
#'   \item \code{asw_i}: Initial available soil water (mm, must be >= 0).
#'   \item \code{asw_min}: Minimum available soil water (mm, must be >= 0).
#'   \item \code{asw_max}: Maximum available soil water (mm, must be >= 0).
#'   \item \code{from}: Start of simulation period (year-month, e.g., "2000-01").
#'   \item \code{to}: End of simulation period (year-month, e.g., "2009-12"). The simulation includes the entire month of December 2009.
#'   \item \code{lt_mod_mths}: Number of months considered to be long-term when calculating the long-term average modifier values for density-dependent mortality.
#'   \item \code{st_Power}: Power in self-thinning rule when mort_model = 2
#'   \item \code{st_Intercept}: Intercept for self-thinning rule (mort_model = 2)
#'   \item \code{st_fN}: Power in fertility modifier when (mort_model = 2)
#'   \item \code{st_fT}: Power in temperature modifier when (mort_model = 2)
#'   \item \code{st_fPhys}: Power in physmod modifier when (mort_model = 2)
#'   \item \code{beta0}: Constant for self-thinning when mort_model = 3
#'   \item \code{betaB}: Power for B when mort_model = 3
#'   \item \code{betaN}: Power in tree density when mort_model = 3
#'   \item \code{betafN}: Power in nutrition modifier when mort_model = 3
#'   \item \code{betafT}: Power in temperature modifier when mort_model = 3
#'   \item \code{betafPhys}: Power in physmod modifier when mort_model = 3
#'
#'
#' }
#'
#' @details This function validates the site table for \code{\link{run_3PG}} and ensures that all required fields are consistent.
#'
#' @return A validated and prepared data frame with one row.
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_input}}, \code{\link{prepare_parameters}}, \code{\link{prepare_sizeDist}}, \code{\link{prepare_thinning}}
#'
#' @example inst/examples/prepare_site-help.R
#'
#' @export
#'
prepare_site <- function(
    site
){

  site = data.frame(site)

  # Backward compatibility: legacy column name `altitude`
  if ("altitude" %in% colnames(site) && !"elevation" %in% colnames(site)) {
    warning(
      "Deprecated site column 'altitude' detected; converting to 'elevation'. ",
      "Please update your input data to use 'elevation'."
    )
    colnames(site)[colnames(site) == "altitude"] <- "elevation"
  }

  if (nrow(site) != 1) {
    stop("The 'site' table must contain exactly one row.")
  }

  required_cols <- c("latitude", "elevation", "soil_class", "asw_i", "asw_min", "asw_max", "from", "to")
  optional_cols <- c( "lt_mod_mths", "st_Power", "st_Intercept", "st_fN", "st_fT", "st_fPhys", "beta0", "betaB", "betaN", "betafN", "betafT", "betafPhys")


  #  if (!identical(required_cols, colnames(site))) {
  #    stop(paste(
  #      "The 'site' table must contain the following columns in order:",
  #      paste(required_cols, collapse = ", ")
  #    ))
  #  }

  # Check if all compulsory columns are present
  missing_cols <- setdiff(required_cols, colnames(site))
  if (length(missing_cols) > 0) {
    stop(paste("The 'site' table must contain the following columns in order:", paste(missing_cols, collapse = ", ")))
  }


  # Check for NA values
  if (anyNA(site)) {
    stop("The 'site' table must not contain NA values.")
  }

  # Validate simulation period
  from <- as.Date(paste0(site$from, "-01"))
  to <- as.Date(paste0(site$to, "-01"))

  if (any(is.na(c(from, to)))) {
    stop("The 'from' and 'to' columns must be in 'YYYY-MM' format.")
  }
  if (from >= to) {
    stop("The 'from' date must be earlier than the 'to' date.")
  }

  # Validate latitude and elevation
  if (site$latitude < -90 || site$latitude > 90) {
    stop("Latitude must be within the range [-90, 90].")
  }
  if (site$elevation < 0 || site$elevation > 4000) {
    stop("Elevation must be within the range [0, 4000].")
  }

  # Validate soil class
  if (!site$soil_class %in% 0:12) {
    stop("Soil class must be an integer between 0 and 12.")
  }

  # Validate soil water values
  if (site$asw_i < 0) stop("Initial available soil water ('asw_i') must be >= 0.")
  if (site$asw_min < 0) stop("Minimum available soil water ('asw_min') must be >= 0.")
  if (site$asw_max < 0) stop("Maximum available soil water ('asw_max') must be >= 0.")



  # Handle optional columns
  if (!all(optional_cols %in% colnames(site))) {

    # Add missing optional columns with NA values
    missing_modifiers <- setdiff(optional_cols, colnames(site))
    site[missing_modifiers] <- NA_real_

  } else {

    # Validate optional columns if present
    if (!all(is.na(site[, optional_cols]))) {
      if ("lt_mod_mths" %in% names(site)) {
        if (any(site$lt_mod_mths < 0, na.rm = TRUE)) {
          stop(
            "Number of months for long-term modifier calculations (lt_mod_mths) ",
            "must contain non-negative values."
          )
        }
      }
    }
  }

  # Return validated site table
  return(site[, c(required_cols,optional_cols), drop = FALSE])
}