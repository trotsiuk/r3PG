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
"output_info"

#' Parameters table information
#'
#' A dataset containing the parameters order and description.
#'
#' @format A data frame with 82 rows and 3 variables:
#' \describe{
#'   \item{parameter}{parameter name}
#'   \item{description}{description of the parameter}
#'   \item{unit}{unit}
#'   \item{default}{default value for E.globus from original 3-PG}
#' }
"param_info"

#' Bias table information
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
"bias_info"


#' Site information
#'
#' Information about site conditions for the example data.
#'
#' @format A data frame with 1 rows and 8 variables:
#' \describe{
#'   \item{latitude}{site latitude in the WGS84 coordinate system}
#'   \item{soil_class}{\code{integer} representing the soil class, according to table ...}
#'   \item{asw_i}{initial awailable soil water}
#'   \item{asw_max}{minimum awailable soil water}
#'   \item{asw_min}{maximum awailable soil water}
#'   \item{year_i}{\code{integer} year when the simulation will start}
#'   \item{month_i}{\code{integer} month when simulation will start, first month in the output file}
#'   \item{altitude}{altitude of the site, m a.s.l.}
#' }
"site_eum"


#' Species information
#'
#' Containing the information about species level data. Each row corresponds to one species/layer.
#'
#' @format A data frame with x rows and 8 variables:
#' \describe{
#'   \item{species}{\code{integer} species id}
#'   \item{year_p}{\code{integer} year when the species was planted (from this we calculate species age)}
#'   \item{month_p}{\code{integer} month when species was planted. Assumption is that species was planted in the end of this month. E.g. if species is planted in January 2000, then in 31 April 2000 it will be 3 month.}
#'   \item{fertility}{soil fertility for a given species. Range from 0 to 1.}
#'   \item{biom_foliage}{initial foliage biomass (T/ha). If this is a leafless period provide a foliage biomass in spring.}
#'   \item{biom_root}{root biomass for a given species.}
#'   \item{biom_stem}{stem biomass for a given species}
#'   \item{n_trees}{number of trees per ha.}
#' }
"species_eum"


#' Climate forcing data
#'
#' Monthly values for climatic data
#'
#' @format A data frame with 156 rows and 7 variables:
#' \describe{
#'   \item{tmp_min}{minimum monthly temperature}
#'   \item{tmp_max}{maximum monthly temperature}
#'   \item{prcp}{total monthly precipittation}
#'   \item{srad}{solar radiation}
#'   \item{frost_days}{number of frost days}
#'   \item{co2}{atmospheric co2}
#'   \item{d13catm}{d13catm}
#' }
"climate_eum"


#' Management information
#'
#' Information about management
#'
#' @format A data frame with 3 rows and 6 variables:
#' \describe{
#'   \item{species}{\code{integer} species id}
#'   \item{age}{age at which management is done}
#'   \item{n_trees}{number of trees remaining after management}
#'   \item{foliage}{type of thinning (above/below). Default is 1}
#'   \item{root}{type of thinning (above/below). Default is 1}
#'   \item{stem}{type of thinning (above/below). Default is 1}
#' }
"thinn_eum"


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
