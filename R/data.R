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
#' @export
#' @details Each row refers to an unique parameter set.
#' The function \code{\link{get_parameters}} eases the use to this dataset.
"i_parameters_lit"


#' Example input data for r3PG simulations
#'
#' A named list containing all required input tables for a 3PG simulation.
#' These include site conditions, species characteristics, climate, model parameters,
#' size distributions, thinning schedules, and defoliation events.
#' Based on the EU mixfor dataset
#'
#' @format A named \code{list} with the following components:
#' \describe{
#'   \item{site}{A data frame with 1 row and 8 variables describing site conditions.}
#'   \item{species}{A data frame with rows corresponding to each species/cohort and variables such as initial biomass and modifiers.}
#'   \item{climate}{A data frame with monthly values of temperature, rainfall, radiation, frost days, etc.}
#'   \item{parameters}{A data frame containing species-specific physiological parameters.}
#'   \item{sizeDist}{A data frame describing the size distribution parameters for each species.}
#'   \item{thinning}{A data frame listing thinning interventions by species and age.}
#'   \item{defoliation}{A data frame listing defoliation events, including retained biomass fractions and recovery settings.}
#' }
#'
#' @examples
#' data(d_input)
#' str(d_input$climate)
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_input}}
"d_input"



#' Example input data for r3PG simulations
#'
#' A named list containing all required input tables for a 3PG simulation.
#' These include site conditions, species characteristics, climate, model parameters,
#' size distributions, thinning schedules, and defoliation events.
#' This data set is particularly tailored to test regeneration
#'
#' @format A named \code{list} with the following components:
#' \describe{
#'   \item{site}{A data frame with 1 row and 8 variables describing site conditions.}
#'   \item{species}{A data frame with rows corresponding to each species/cohort and variables such as initial biomass and modifiers.}
#'   \item{climate}{A data frame with monthly values of temperature, rainfall, radiation, frost days, etc.}
#'   \item{parameters}{A data frame containing species-specific physiological parameters.}
#'   \item{sizeDist}{A data frame describing the size distribution parameters for each species.}
#'   \item{thinning}{A data frame listing thinning interventions by species and age.}
#'   \item{defoliation}{A data frame listing defoliation events, including retained biomass fractions and recovery settings.}
#' }
#'
#' @examples
#' data(d_regeneration)
#' str(d_regeneration$climate)
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_input}}
"d_regeneration"


#' Example input data for r3PG simulations
#'
#' A named list containing all required input tables for a 3PG simulation.
#' These include site conditions, species characteristics, climate, model parameters,
#' size distributions, thinning schedules, and defoliation events.
#' This dataset is particularly tailored to test defoliation
#'
#' @format A named \code{list} with the following components:
#' \describe{
#'   \item{site}{A data frame with 1 row and 8 variables describing site conditions.}
#'   \item{species}{A data frame with rows corresponding to each species/cohort and variables such as initial biomass and modifiers.}
#'   \item{climate}{A data frame with monthly values of temperature, rainfall, radiation, frost days, etc.}
#'   \item{parameters}{A data frame containing species-specific physiological parameters.}
#'   \item{sizeDist}{A data frame describing the size distribution parameters for each species.}
#'   \item{thinning}{A data frame listing thinning interventions by species and age.}
#'   \item{defoliation}{A data frame listing defoliation events, including retained biomass fractions and recovery settings.}
#' }
#'
#' @examples
#' data(d_defoliation)
#' str(d_defoliation$climate)
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_input}}
"d_defoliation"
