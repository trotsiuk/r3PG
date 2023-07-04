#' @title Check the site data for consistency
#' @description Prepares the site table, by checking whether the input information is consistent.
#'
#' @param site table containing the information about site data. It shall contain exactly one row.
#' \itemize{
#'   \item latitude: site latitude in the WGS84 coordinate system.
#'   \item altitude: site altitude, m a.s.l.
#'   \item soil_class:  soil class, according to table 2 user manual of 3PGpjs. 1 - Sandy; 2 - Sandy loam; 3 - Clay loam; 4 - Clay; 0 - No effect of available soil water on production.
#'   \item asw_i: initial available soil water (mm).
#'   \item asw_max: minimum available soil water (mm).
#'   \item asw_min: maximum available soil water (mm).
#'   \item from: year and month indicating the start of simulation. Provided in form of year-month. E.g. "2000-01".
#'   \item to: year and month indicating the end of simulation. Provided in form of year-month. E.g. "2009-12", will include December 2009 as last simulation month.
#' }
#'
#' @details This function check the site table for \code{\link{run_3PG}}.
#'
#' @return a data.frame with one row
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

  if( nrow(site) != 1){
    stop( 'Site table shall contain exactly one row')
  }

  if( !identical( c("latitude","altitude","soil_class","asw_i","asw_min","asw_max","from","to"), colnames(site)) ){
    stop( 'Columns names of the site table must correspond to: latitude, altitude, soil_class, asw_i, asw_min, asw_max, from, to')
  }

  # Test for NA
  if( any( is.na(site) ) ){
    stop( "Climate table should not contain NAs" )
  }

  # prepare the time period
  from = as.Date(paste(site$from,"-01",sep=""))
  to = as.Date(paste(site$to,"-01",sep=""))

  if( any(is.na(from), is.na(to))) {
    stop('The simulation dates (from/to) are in the wrong format')
  }

  if( from >= to ){
    stop( 'The start date is later than the end date' )
  }

  if( site$latitude > 90 | site$latitude < -90 ){
    stop( 'Latitude shall be within a range of [-90:90]' )
  }

  if( site$altitude > 4000 | site$altitude < 0 ){
    stop( 'Altitude shall be within a range of [0:4000]' )
  }

  if( !site$soil_class %in% c(-1:4) ){
    stop( 'Soil class shall be within a range of [-1:4]' )
  }

  if( site$asw_i < 0 ){
    stop( 'ASW initial shall be greater than 0' )
  }

  if( site$asw_min < 0 ){
    stop( 'ASW minimum shall be greater than 0' )
  }

  if( site$asw_max < 0 ){
    stop( 'ASW maximum shall be greater than 0' )
  }

  # Select final table
  site = site[,c("latitude","altitude","soil_class","asw_i","asw_min","asw_max","from","to")]

  return( site )
}