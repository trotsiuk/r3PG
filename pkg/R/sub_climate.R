#' @title The function to subset climate table.
#' @description This function prepare the climate table, by either replicating the average climate for the required number of years or subsetting.
#' @param climate  a \code{data frame} or \code{matrix} containing the information about climatic forcing data. In case you want to replicate average climate it shall have exactly 12, one for each month. In case one want to subset, it shall contain the period for subsetting and two additional columns: year and month.  Additionally can include: tmp_ave, c02, d13catm. The minimum required columns are listed below.
#' \itemize{
#' \item year: numeric, year of observation (only reguired for subsetting.
#' \item month: numeric, months of observation (only reguired for subsetting.
#' \item tmp_min: monthly mean daily minimum temperature (C).
#' \item tmp_max: monthly mean daily maximum temperature (C).
#' \item prcp: monthly rainfall (mm month-1).
#' \item srad: monthly mean daily solar radiation (MJ m-2 d-1).
#' \item frost_days: frost days per month (d month-1).
#' }
#' @param from from which date climate data shall be included. Shall be provided as character, in form of year-month. E.g. "2000-01"
#' @param to to which date climate data shall be included. Shall be provided as character, in form of year-month. E.g. "2009-12", will include December 2009 as last simulation month
#'
#' @details This function shall be used in case user provide only average climate and want to repeate it for each year of simualtion.
#'
#' @example inst/examples/sub_climateHelp.R
#'
#' @export
#'
sub_climate <- function(
  climate,
  from = '2000-04',
  to = '2010-11'
){

  if( !all(c("tmp_min","tmp_max","prcp","srad","frost_days") %in% colnames(climate)) ){
    stop( 'climate table shall include: tmp_min,tmp_max,prcp,srad,frost_days' )
  }

  from = as.Date(paste(from,"-01",sep=""))
  to = as.Date(paste(to,"-01",sep=""))

  if( from >= to ){
    stop( 'The start date is later than the end date' )
  }

  if( nrow(climate) == 12 ){

    n_years <- as.numeric(format(to,'%Y')) - as.numeric(format(from,'%Y'))
    month_i <- as.numeric(format(from,'%m'))
    month_e <- as.numeric(format(to,'%m'))

    out = do.call("rbind", replicate(n_years, climate, simplify = FALSE))

    if( month_i > 1 ){
      out = out[-c(1:(month_i-1)),]
    }

    if( month_e < 12 ){
      out = out[1:(nrow(out)-(12-month_e)),]
    }

  } else {

    col_select = intersect( c('tmp_min', 'tmp_max', 'tmp_ave', 'prcp', 'srad', 'frost_days', 'vpd_day', 'co2', 'd13catm'),
      colnames(climate))

    climate$date = as.Date( paste(climate$year, '-', climate$month, "-01",sep="") )

    out = climate[climate$date >= from & climate$date <= to, col_select]
  }

  return( out )
}


