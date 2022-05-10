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
#' @export
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
#' @export
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
#' @export
"i_sizeDist"


#' Information about literature parameters
#'
#' A dataset containing parameter sets from published studies with 3PG
#'
#' @format A data frame with 110 rows and  124 variables:
#' \describe{
#'   \item{parset_id}{id of the parameter set}
#'   \item{species}{species scientific name}
#'   \item{age}{whether the parameter set was was used in even or uneven stands.}
#'   \item{type}{whether the parameter set was used in monocultures or mixed stands.}
#'   \item{year}{year of publication}
#'   \item{region}{geographical region in which the parameters were tested. NAs values are allowed.}
#'   \item{country}{country or countries in which the parameters were tested.}
#'   \item{notes}{any relevant remark about how the parameters were processed. NAs are allowed.}
#'   \item{source}{short reference to publication}
#'   \item{source_comments}{any relevant comment about the parameters present in the publication. NAs are allowed.}
#'   \item{source_full}{full reference to publication}
#'   \item{link}{a link to the publication, e.g. doi}
#'   \item{pFS2}{Foliage:stem partitioning ratio - D = 2 cm}
#'   \item{pFS20}{Foliage:stem partitioning ratio - D = 20 cm}
#'   \item{aWS}{Constant in the stem mass v. diam. relationship}
#'   \item{nWS}{Power in the stem mass v. diam. relationship}
#'   \item{pRx}{Maximum fraction of NPP to roots}
#'   \item{pRn}{Minimum fraction of NPP to roots}
#'   \item{gammaF1}{Maximum litterfall rate}
#'   \item{gammaF0}{Litterfall rate at t = 0}
#'   \item{tgammaF}{Age at which litterfall rate has median value}
#'   \item{gammaR}{Average monthly root turnover rate}
#'   \item{leafgrow}{If deciduous, leaves are produced at end of this month}
#'   \item{leaffall}{If deciduous, leaves all fall at start of this month}
#'   \item{Tmin}{Minimum temperature for growth}
#'   \item{Topt}{Optimum temperature for growth}
#'   \item{Tmax}{Maximum temperature for growth}
#'   \item{kF}{Days production lost per frost day}
#'   \item{SWconst}{Moisture ratio deficit for fq = 0.5}
#'   \item{SWpower}{Power of moisture ratio deficit}
#'   \item{fCalpha700}{Assimilation enhancement factor at 700 ppm}
#'   \item{fCg700}{Canopy conductance enhancement factor at 700 ppm}
#'   \item{m0}{Value of m when FR = 0}
#'   \item{fN0}{Value of fNutr when FR = 0}
#'   \item{fNn}{Power of (1-FR) in fNutr}
#'   \item{MaxAge}{Maximum stand age used in age modifier}
#'   \item{nAge}{Power of relative age in function for fAge}
#'   \item{rAge}{Relative age to give fAge = 0.5}
#'   \item{gammaN1}{Mortality rate for large t}
#'   \item{gammaN0}{Seedling mortality rate (t = 0)}
#'   \item{tgammaN}{Age at which mortality rate has median value}
#'   \item{ngammaN}{Shape of mortality response}
#'   \item{wSx1000}{Max. stem mass per tree - 1000 trees/hectare}
#'   \item{thinPower}{Power in self-thinning rule}
#'   \item{mF}{Fraction mean single-tree foliage biomass lost per dead tree}
#'   \item{mR}{Fraction mean single-tree root biomass lost per dead tree}
#'   \item{mS}{Fraction mean single-tree stem biomass lost per dead tree}
#'   \item{SLA0}{Specific leaf area at age 0}
#'   \item{SLA1}{Specific leaf area for mature leaves}
#'   \item{tSLA}{Age at which specific leaf area = (SLA0+SLA1)/2}
#'   \item{k}{Extinction coefficient for absorption of PAR by canopy}
#'   \item{fullCanAge}{Age at canopy closure}
#'   \item{MaxIntcptn}{Maximum proportion of rainfall evaporated from canopy}
#'   \item{LAImaxIntcptn}{LAI for maximum rainfall interception}
#'   \item{cVPD}{LAI for 50\% reduction of VPD in canopy}
#'   \item{alphaCx}{Canopy quantum efficiency}
#'   \item{Y}{Ratio NPP/GPP}
#'   \item{MinCond}{Minimum canopy conductance}
#'   \item{MaxCond}{Maximum canopy conductance}
#'   \item{LAIgcx}{LAI for maximum canopy conductance}
#'   \item{CoeffCond}{Defines stomatal response to VPD}
#'   \item{BLcond}{Canopy boundary layer conductance}
#'   \item{RGcGw}{The ratio of diffusivities of CO2 and water vapour in air}
#'   \item{D13CTissueDif}{d13C difference of modelled tissue and new photosynthate}
#'   \item{aFracDiffu}{Fractionation against 13C in diffusion}
#'   \item{bFracRubi}{Enzymatic fractionation by Rubisco}
#'   \item{fracBB0}{Branch and bark fraction at age 0}
#'   \item{fracBB1}{Branch and bark fraction for mature stands}
#'   \item{tBB}{Age at which fracBB = (fracBB0+fracBB1)/2}
#'   \item{rhoMin}{Minimum basic density - for young trees}
#'   \item{rhoMax}{Maximum basic density - for older trees}
#'   \item{tRho}{Age at which rho = (rhoMin+rhoMax)/2}
#'   \item{aH}{Constant in the stem height relationship}
#'   \item{nHB}{Power of DBH in the stem height relationship}
#'   \item{nHC}{Power of competition in the stem height relationship}
#'   \item{aV}{Constant in the stem volume relationship}
#'   \item{nVB}{Power of DBH in the stem volume relationship}
#'   \item{nVH}{Power of height in the stem volume relationship}
#'   \item{nVBH}{Power of DBH^2 x height in the stem volume relationship}
#'   \item{crownshape}{Crown shape (1=cone, 2=ellipsoid, 3=half-ellipsoid, 4=rectangular)}
#'   \item{aK}{Constant in the crown diameter relationship}
#'   \item{nKB}{Power of DBH in the crown diameter relationship}
#'   \item{nKH}{Power of height in the crown diameter relationship}
#'   \item{nKC}{Power of competition in the crown diameter relationship}
#'   \item{nKrh}{Power of relative height in the crown diameter relationship}
#'   \item{aHL}{Constant in the LCL relationship}
#'   \item{nHLB}{Power of DBH in the LCL relationship}
#'   \item{nHLL}{Power of LAI in the LCL relationship}
#'   \item{nHLC}{Power of competition in the LCL relationship}
#'   \item{nHLrh}{Power of relative height in the LCL relationship}
#'   \item{Dscale0}{Constant in the relationship for Weibull scale parameter of D distribution}
#'   \item{DscaleB}{Slope of DBH in relationship for Weibull scale parameter of D distribution}
#'   \item{Dscalerh}{Slope of relative height in relationship for Weibull scale parameter of D distribution}
#'   \item{Dscalet}{Slope of age in relationship for Weibull scale parameter of D distribution}
#'   \item{DscaleC}{Slope of competition in relationship for Weibull scale parameter of D distribution}
#'   \item{Dshape0}{Constant in the relationship for Weibull shape parameter of D distribution}
#'   \item{DshapeB}{Slope of DBH in relationship for Weibull shape parameter of D distribution}
#'   \item{Dshaperh}{Slope of relative height in relationship for Weibull shape parameter of D distribution}
#'   \item{Dshapet}{Slope of age in relationship for Weibull shape parameter of D distribution}
#'   \item{DshapeC}{Slope of competition in relationship for Weibull shape parameter of D distribution}
#'   \item{Dlocation0}{Constant in the relationship for Weibull location parameter of D distribution}
#'   \item{DlocationB}{Slope of DBH in relationship for Weibull location parameter of D distribution}
#'   \item{Dlocationrh}{Slope of relative height in relationship for Weibull location parameter of D distribution}
#'   \item{Dlocationt}{Slope of age in relationship for Weibull location parameter of D distribution}
#'   \item{DlocationC}{Slope of competition in relationship for Weibull location parameter of D distribution}
#'   \item{wsscale0}{Constant in the relationship for Weibull scale parameter of ws distribution}
#'   \item{wsscaleB}{Slope of DBH in relationship for Weibull scale parameter of ws distribution}
#'   \item{wsscalerh}{Slope of relative height in relationship for Weibull scale parameter of ws distribution}
#'   \item{wsscalet}{Slope of age in relationship for Weibull scale parameter of ws distribution}
#'   \item{wsscaleC}{Slope of competition in relationship for Weibull scale parameter of ws distribution}
#'   \item{wsshape0}{Constant in the relationship for Weibull shape parameter of ws distribution}
#'   \item{wsshapeB}{Slope of DBH in relationship for Weibull shape parameter of ws distribution}
#'   \item{wsshaperh}{Slope of relative height in relationship for Weibull shape parameter of ws distribution}
#'   \item{wsshapet}{Slope of age in relationship for Weibull shape parameter of ws distribution}
#'   \item{wsshapeC}{Slope of competition in relationship for Weibull shape parameter of ws distribution}
#'   \item{wslocation0}{Constant in the relationship for Weibull location parameter of ws distribution}
#'   \item{wslocationB}{Slope of DBH in relationship for Weibull location parameter of ws distribution}
#'   \item{wslocationrh}{Slope of relative height in relationship for Weibull location parameter of ws distribution}
#'   \item{wslocationt}{Slope of age in relationship for Weibull location parameter of ws distribution}
#'   \item{wslocationC}{Slope of competition in relationship for Weibull location parameter of ws distribution}
#'   \item{Qa}{Intercept of net v. solar radiation relationship}
#'   \item{Qb}{Slope of net v. solar radiation relationship}
#'   \item{gDM_mol}{Molecular weight of dry matter}
#'   \item{molPAR_MJ}{Conversion of solar radiation to PAR}
#' }
#' @details Each row refers to an unique parameter set. 
#' The function \code{\link{get_parameters}} eases the use to this dataset. 
"i_parameters_lit"



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


