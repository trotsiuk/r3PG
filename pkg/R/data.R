#' Information table
#'
#' A dataset containing the list of variables and their description.
#'
#' @format A data frame with 150 rows and 7 variables:
#' \describe{
#'   \item{group_id}{serial number of the group}
#'   \item{variable_id}{serial number of the variable}
#'   \item{variable_group}{group name to which variable belongs}
#'   \item{variable_name}{variable name as named in output}
#'   \item{description}{description of the variable}
#'   \item{unit}{unit of the variable}
#'   \item{variable_vba}{corresponding name of the variable in the VBA output}
#' }
"r3pg_info"


#' Site information
#'
#' Information about site conditions for the example data.
#'
#' @format A data frame with 1 rows and 8 variables:
#' \describe{
#'   \item{latitude}{site latitude in the WGS84 coordinate system}
#'   \item{soilClass}{\code{integer} representing the soil class, according to table ...}
#'   \item{iASW}{initial awailable soil water}
#'   \item{minASW}{minimum awailable soil water}
#'   \item{maxASW}{maximum awailable soil water}
#'   \item{iYear}{\code{integer} year when the simulation will start}
#'   \item{iMonth}{\code{integer} month when simulation will start, first month in the output file}
#'   \item{Altitude}{altitude of the site, m a.s.l.}
#' }
"site_eum"


#' Species information
#'
#' Containing the information about species level data. Each row corresponds to one species/layer.
#'
#' @format A data frame with x rows and 8 variables:
#' \describe{
#'   \item{species}{\code{integer} species id}
#'   \item{pYear}{\code{integer} year when the species was planted (from this we calculate species age)}
#'   \item{pMonth}{\code{integer} month when species was planted. Assumption is that species was planted in the end of this month. E.g. if species is planted in January 2000, then in 31 April 2000 it will be 3 month.}
#'   \item{fertilityRating}{soil fertility for a given species. Range from 0 to 1.}
#'   \item{iWF}{initial foliage biomass (T/ha). If this is a leafless period provide a foliage biomass in spring.}
#'   \item{iWR}{root biomass for a given species.}
#'   \item{iWS}{stem biomass for a given species}
#'   \item{iStocking}{number of trees per ha.}
#' }
"species_eum"


#' Climate forcing data
#'
#' Monthly values for climatic data
#'
#' @format A data frame with 156 rows and 7 variables:
#' \describe{
#'   \item{Tmin}{minimum monthly temperature}
#'   \item{Tmax}{maximum monthly temperature}
#'   \item{Rain}{total monthly precipittation}
#'   \item{sRad}{solar radiation}
#'   \item{fDays}{number of frost days}
#'   \item{co2}{atmospheric co2}
#'   \item{d13catm}{d13catm}
#' }
"climate_eum"


#' Parameters information
#'
#' Containing the information about parameters.
#'
#' @format A data frame with 65 rows and x variables:
#' \describe{
#'   \item{parameter}{name of the parameter}
#'   \item{sp1}{parameter values for species 1}
#'   \item{sp2}{parameter values for species 2}
#' }
"parameters_eum"


#' Bias information
#'
#' Containing the information about bias parameters.
#'
#' @format A data frame with 47 rows and x variables:
#' \describe{
#'   \item{parameter}{name of the parameter}
#'   \item{sp1}{parameter values for species 1}
#'   \item{sp2}{parameter values for species 2}
#' }
"bias_eum"
