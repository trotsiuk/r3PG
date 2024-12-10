#' @title Prepare climate data
#' @description Prepares the climate data for the simulation, either by replicating the average climate
#' for the required period or subsetting data from a longer time-series.
#'
#' @param climate A data frame containing monthly climate data. The table must include the following columns:
#' \itemize{
#'   \item \code{year}: Year of observation (only required for subsetting) (numeric).
#'   \item \code{month}: Month of observation (only required for subsetting) (numeric).
#'   \item \code{tmp_min}: Monthly mean daily minimum temperature (°C).
#'   \item \code{tmp_max}: Monthly mean daily maximum temperature (°C).
#'   \item \code{tmp_ave}: Monthly mean daily average temperature (°C) (optional).
#'   \item \code{prcp}: Monthly rainfall (mm month\eqn{-1}).
#'   \item \code{srad}: Monthly mean daily solar radiation (MJ m\eqn{^{-2}} d\eqn{^{-1}}).
#'   \item \code{frost_days}: Frost days per month (d month\eqn{-1}).
#'   \item \code{co2}: Monthly mean atmospheric CO2 (ppm), required if \code{calculate_d13c = 1} (optional).
#'   \item \code{d13catm}: Monthly mean isotopic composition of air (‰), required if \code{calculate_d13c = 1} (optional).
#' }
#' If the climate table contains exactly 12 rows, it will be replicated for the number of years and months specified
#' by \code{from} and \code{to}. Otherwise, it will be subsetted to the selected time period.
#'
#' @param from Start of the simulation period, provided as "YYYY-MM" (e.g., "2000-04").
#' @param to End of the simulation period, provided as "YYYY-MM" (e.g., "2010-11").
#' The simulation will include the full month specified in \code{to}.
#'
#' @details
#' If the climate data contains exactly 12 rows, it is treated as average monthly climate and replicated
#' for the desired period specified by \code{from} and \code{to}.
#' If the data contains more than 12 rows, it is assumed to be a time-series, and a subset of the data is extracted
#' for the specified period. In this case, \code{year} and \code{month} columns are required.
#'
#' @return A data frame with monthly climate data for the specified simulation period, including columns:
#' \code{year}, \code{month}, \code{tmp_min}, \code{tmp_max}, \code{tmp_ave}, \code{prcp},
#' \code{srad}, \code{frost_days}, \code{vpd_day}, \code{co2}, \code{d13catm}.
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_input}}, \code{\link{prepare_parameters}}, \code{\link{prepare_sizeDist}}, \code{\link{prepare_thinning}}
#'
#' @example inst/examples/prepare_climate-help.R
#'
#' @export
#'
prepare_climate <- function(
  climate,
  from = '2000-04',
  to = '2010-11'
){

  # make data.frame
  climate = data.frame(climate)

  # Check for required columns
  required_cols <- c("tmp_min", "tmp_max", "prcp", "srad", "frost_days")
  if (!all(required_cols %in% colnames(climate))) {
    stop("Climate table must include the following columns: tmp_min, tmp_max, prcp, srad, frost_days")
  }

  # Check for missing values
  if (anyNA(climate[required_cols])) {
    stop("Climate table must not contain NA values in required columns")
  }

  # Convert dates
  from <- as.Date(paste0(from, "-01"))
  to <- as.Date(paste0(to, "-01"))

  if (from >= to) {
    stop("The start date must be earlier than the end date")
  }

  # Replicate or subset the data
  if( dim(climate)[1] == 12 ){

    n_years <- as.numeric(format(to,'%Y')) - as.numeric(format(from,'%Y')) + 1
    month_i <- as.numeric(format(from,'%m'))
    month_e <- as.numeric(format(to,'%m'))

    climate = do.call("rbind", replicate(n_years, climate, simplify = FALSE))
    climate$year = rep( as.numeric(format(from,'%Y')):as.numeric(format(to,'%Y')), each = 12)
    climate$month = rep(1:12, times = n_years)

    if( month_i > 1 ){
      climate = climate[-c(1:(month_i-1)),]
    }

    if( month_e < 12 ){
      climate = climate[1:(nrow(climate)-(12-month_e)),]
    }

  } else {

    # Subset time-series data
    if (!all(c("year", "month") %in% colnames(climate))) {
      stop("Climate table must include 'year' and 'month' columns for subsetting")
    }

    climate$date <- as.Date(paste(climate$year, climate$month, "01", sep = "-"))

    if (from < min(climate$date) || to > max(climate$date)) {
      stop("Requested period is outside the available dates in the climate table")
    }

    climate = climate[climate$date >= from & climate$date <= to, ]

  }

  # Check for the number of frost days to limit
  daysInMonth <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  climate$frost_days <- pmin( climate$frost_days, daysInMonth[climate$month])


  # Calculate derived columns
  if (!"tmp_ave" %in% colnames(climate)) {
    climate$tmp_ave <- (climate$tmp_min + climate$tmp_max) / 2
  }

  if (!"vpd_day" %in% colnames(climate)) {
    climate$vpd_day <- get_vpd(climate$tmp_min, climate$tmp_max)
  }

  if (!"co2" %in% colnames(climate)) {
    climate$co2 <- 350
  }

  if (!"d13catm" %in% colnames(climate)) {
    climate$d13catm <- -7.1
  }

  # Select final table
  climate = climate[,c("year", "month",'tmp_min', 'tmp_max', 'tmp_ave', 'prcp', 'srad', 'frost_days', 'vpd_day', 'co2', 'd13catm')]

  clim_range( climate )

  return( climate )
}




get_vpd <- function(tmin, tmax){
  # internal function to calculate VPD if not available
  vpd_min = 6.10780 * exp(17.2690 * tmin / (237.30 + tmin))
  vpd_max = 6.10780 * exp(17.2690 * tmax / (237.30 + tmax))

  vpd_day = (vpd_max - vpd_min) / 2

  return(vpd_day)
}


clim_range <- function( climate ){
  # internal function to check whether climate data are within the plausible range

  # Temperature hard limit
  if( any( max(climate$tmp_min, climate$tmp_max, climate$tmp_ave) > 50,
           min(climate$tmp_min, climate$tmp_max, climate$tmp_ave) < -50) ){
    warning("Temperature is outside the limits (-50 to 50 C)!")
  }

  if (any(climate$tmp_max < climate$tmp_ave)) {
    stop("Average temperature is greater than maximum temperature!")
  }

  if (any(climate$tmp_ave < climate$tmp_min)) {
    stop("Minimum temperature is greater than average temperature!")
  }

  # Precipitation checks
  if (any(climate$prcp < 0)) {
    stop("Precipitation contains negative values.")
  }

  if (any(climate$prcp > 10000)) {
    warning("Precipitation is outside the plausible range (0 to 10000 mm)!")
  }

  # Solar radiation checks
  if (any(climate$srad < 0)) {
    stop("Solar radiation contains negative values.")
  }

  if (any(climate$srad > 100)) {
    warning("Solar radiation is outside the plausible range (0 to 100 MJ/m^2/day)!")
  }

  # Frost days checks
  if (any(climate$frost_days < 0)) {
    stop("Frost days contain negative values.")
  }

  if (any(climate$frost_days > 31)) {
    warning("Frost days are outside the plausible range (0 to 31 days)!")
  }

  # VPD checks
  if (any(climate$vpd_day < 0)) {
    stop("VPD contains negative values.")
  }

  if (any(climate$vpd_day > 40)) {
    warning("VPD is outside the plausible range (0 to 40 kPa)!")
  }

}