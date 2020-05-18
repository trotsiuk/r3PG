#' Information about model outputs
#'
#' A dataset containing the list of output variables and their description.
#'
#' @format A data frame with 150 rows and 7 variables:
#' \describe{
#'   \item{group_id}{serial number of the group}
#'   \item{variable_id}{serial number of the variable}
#'   \item{variable_group}{group name to which variable belongs}
#'   \item{variable_name}{variable name as named in output}
#'   \item{description}{description of the variable}
#'   \item{unit}{unit of the variable}
#'   \item{variable_vba}{corresponding name of the variable as output from Excel version of 3-PGmix}
#' }
"i_output"


#' Information about parameters
#'
#' A dataset containing the parameters order and description.
#'
#' @format A data frame with 82 rows and 3 variables:
#' \describe{
#'   \item{parameter}{parameter name}
#'   \item{description}{description of the parameter}
#'   \item{unit}{unit}
#'   \item{default}{default value for E.globulus from original 3-PG}
#' }
"i_parameters"


#' Information about size distribution parameters
#'
#' A dataset containing the parameters order and description.
#'
#' @format A data frame with 30 rows and 3 variables:
#' \describe{
#'   \item{parameter}{parameter name}
#'   \item{description}{description of the parameter}
#'   \item{unit}{unit}
#'   \item{default}{default value equal to 0}
#' }
"i_sizeDist"


#' Site input
#'
#' Table containing the information about site conditions.
#'
#' @format A \code{data frame} with 1 rows and 8 variables:
#' \describe{
#'   \item{latitude}{site latitude in the WGS84 coordinate system}
#'   \item{altitude}{site altitude, m a.s.l.}
#'   \item{soil_class}{ soil class, according to table 2 user manual of 3PGpjs. 1 - Sandy; 2 - Sandy loam; 3 - Clay loam; 4 - Clay; 0 - No effect of available soil water on production}
#'   \item{asw_i}{initial available soil water (mm)}
#'   \item{asw_max}{minimum available soil water (mm)}
#'   \item{asw_min}{maximum available soil water (mm)}
#'   \item{from}{year and month indicating the start of simulation. Provided in form of year-month. E.g. "2000-01"}
#'   \item{to}{year and month indicating the end of simulation. Provided in form of year-month. E.g. "2009-12", will include December 2009 as last simulation month}
#' }
"d_site"


#' Species input
#'
#' Table containing the information about species level data. Each row corresponds to one species/cohort.
#'
#' @format A \code{data frame} with number of rows corresponding to each species/cohort and 8 variables:
#' \describe{
#'   \item{species}{species or cohort id/name. It must be consistent with species names in \code{\link{d_thinning}}, \code{\link{d_parameters}} and \code{\link{d_sizeDist}} tables.}
#'   \item{planted}{year and month indicating when the species was planted. Provided in form of year-month. E.g. "2000-01"}
#'   \item{fertility}{soil fertility for a given species. Range from 0 to 1}
#'   \item{stems_n}{number of trees per ha}
#'   \item{biom_stem}{stem biomass for a given species  (Mg/ha)}
#'   \item{biom_root}{root biomass for a given species  (Mg/ha)}
#'   \item{biom_foliage}{initial foliage biomass (Mg/ha). If this is a leafless period, provide the spring foliage biomass.}
#' }
"d_species"


#' Climate input
#'
#' Table containing the information about monthly values for climatic data.
#'
#' @format A \code{data frame} with 156 rows and 7 variables:
#' \describe{
#'   \item{year}{calendar year}
#'   \item{month}{month}
#'   \item{tmp_min}{monthly mean daily minimum temperature (C)}
#'   \item{tmp_max}{monthly mean daily maximum temperature (C)}
#'   \item{tmp_ave}{monthly mean daily average temperature (C). (optional)}
#'   \item{prcp}{monthly rainfall (mm month-1)}
#'   \item{srad}{monthly mean daily solar radiation (MJ m-2 d-1)}
#'   \item{frost_days}{frost days per month (d month-1)}
#'   \item{co2}{monthly mean atmospheric co2 (ppm), required if calculate_d13c=1 (optional)}
#'   \item{d13catm}{Monthly mean isotopic composition of air (‰), required if calculate_d13c=1 (optional)}
#' }
"d_climate"


#' Thinning input
#'
#' Table containing the information about thinnings
#'
#' @format A \code{data frame} with 3 rows and 6 variables:
#' \describe{
#'   \item{species}{species or cohort id/name. It must be consistent with species names in \code{\link{d_species}}, \code{\link{d_parameters}} and \code{\link{d_sizeDist}} tables.}
#'   \item{age}{age when thinning is performed}
#'   \item{stems_n}{number of trees remaining after thinning}
#'   \item{stem}{type of thinning (above/below). Default is 1}
#'   \item{root}{type of thinning (above/below). Default is 1}
#'   \item{foliage}{type of thinning (above/below). Default is 1}
#' }
"d_thinning"


#' Parameters input
#'
#' Table containing the information about parameters.
#'
#' @format A \code{data frame} with 65 rows and x variables:
#' \describe{
#'   \item{parameter}{name of the parameter, must be consistent in naming with \code{\link{i_parameters}}}
#'   \item{Fagus sylvatica}{parameter values for species 1}
#'   \item{Pinus sylvestris}{parameter values for species 2}
#' }
"d_parameters"


#' sizeDist input
#'
#' Table containing the information about size distribution.
#'
#' @format A data frame with 47 rows and x variables:
#' \describe{
#'   \item{parameter}{name of the parameter, must be consistent in naming with \code{\link{i_sizeDist}}}
#'   \item{Fagus sylvatica}{parameter values for species 1}
#'   \item{Pinus sylvestris}{parameter values for species 2}
#' }
"d_sizeDist"