#' @title The function to run the 3-PG in R
#' @description This function allows to run the 3-PGpjs or 3-PGmix model in R. It returns a model output as list, which can be later tranformed to the dataframe using \code{\link{transf_out}} function.
#' @param siteInputs a \code{data frame} or \code{matrix} containing the information about site level data in the following order:
#' \itemize{
#' \item latitude: site latitude in the WGS84 coordinate system
#' \item soil_class: soil class, according to table 2 of 3PGpjs user manual.
#' \item asw_i: initial soil water in mm.
#' \item asw_min: minimum soil water in mm.
#' \item asw_max: maximum soil water in mm.
#' \item year_i: initial year of the simulation.
#' \item momth_i: initial month of the simulation, first month in the output file.
#' \item altitude: altitude of the site, m a.s.l.
#' }
#' @param speciesInputs a \code{data frame} or \code{matrix} containing the information about species level data. Each row corresponds to one species/layer. The following order of the collumns apply:
#' \itemize{
#' \item species: species or cohort id/name. It shall be consistent with \code{parameterInputs} and \code{biasInputs} table.
#' \item year_p: year when the species was planted (it is used to calculate species age at the time of simulation).
#' \item month_p: month when species was planted, assuming end of the month. E.g. if species is planted in January 2000, then in 31 April 2000 it will be 3 month.
#' \item fertility: soil fertility for a given species. Range from 0 to 1.
#' \item biom_foliage: initial foliage biomass (T/ha). If this is a leafless period provide a foliage biomass in summer.
#' \item biom_root: root biomass for a given species.
#' \item biom_stem: stem biomass for a given species.
#' \item n_trees: number of trees per ha.
#' }
#' @param forcingInputs  a \code{data frame} or \code{matrix} containing the information about climatic forcing data. First month shall corresponds to \code{year_i month_i} in the \code{siteInputs} table:
#' \itemize{
#' \item tmp_min: monthly mean daily minimum temperature (C).
#' \item tmp_max: monthly mean daily maximum temperature (C).
#' \item prcp: monthly rainfall (mm month-1).
#' \item srad: monthly mean daily solar radiation (MJ m-2 d-1).
#' \item frost_days: frost days per month (d month-1).
#' \item co2: required if calculate_d13c=1
#' \item d13catm: required if calculate_d13c=1
#' }
#' @param managementInputs a \code{data frame} or \code{matrix} containing the information about management. The following order apply:
#' \itemize{
#' \item species: species or cohort id/name.
#' \item age: age at which management is done.
#' \item n_trees: number of trees remaining after management
#' \item foliage: type of management (above/below). Default is 1.
#' \item root: type of management (above/below). Default is 1.
#' \item stem: type of management (above/below). Default is 1.
#' }
#' @param parameterInputs a \code{data frame} or \code{matrix} containing the parameters to be modified:
#' \itemize{
#' \item parameter: name of the parameter, shall be consistent in naming with \code{\link{param_info}}
#' \item species: each column correspond to species/cohort id/name, as defined in \code{speciesInputs} table
#' }
#' @param biasInputs  a \code{data frame} or \code{matrix} containing the bias parameters to be modified:
#' \itemize{
#' \item parameter: name of the parameter, shall be consistent in naming with \code{\link{bias_info}}
#' \item species: each column shall correspond to species/cohort id/name, as defined in \code{speciesInputs} table
#' }
#' @param settings list with all possible settings of the model.
#' \itemize{
#' \item light_model: `1` - 3-PGpjs; `2` - 3-PGmix
#' \item transp_model: `1` - 3-PGpjs; `2` - 3-PGmix
#' \item phys_model:  `1` - 3-PGpjs; `2` - 3-PGmix
#' \item correct_bias: `0` - no; `1` - 3-PGmix
#' \item calculate_d13c: `0` - no; `1` - 3-PGmix
#' }
#' @param df_out \code{logical} if the output shall be in the 4-dimentional array (FALSE) or long data.frame (TRUE)
#'
#' @details The \code{run_3PG} also check for the quality of input data. In case that names, or structure is not consitent with requirenments it will return an error.
#'
#' @example inst/examples/run_3PGHelp.R
#'
#' @export
#'
run_3PG <- function(
  siteInputs,
  speciesInputs,
  forcingInputs,
  managementInputs = NULL,
  parameterInputs = NULL,
  biasInputs = NULL,
  settings = list(light_model = 1, transp_model = 1, phys_model = 1, correct_bias = 0, calculate_d13c = 0),
  df_out = TRUE
){

  #Check the input naming and structure
  chk_input()


  # replace default settings
  set_def = list(light_model = 1, transp_model = 1, phys_model = 1, correct_bias = 0, calculate_d13c = 0)
  set_def[names(settings)] <- settings
  set_def <- as.integer( unlist(set_def) )

  n_sp = as.integer( nrow(speciesInputs) )
  n_m = as.integer( nrow(forcingInputs) )

  # Climate
  if( set_def[5] == 1 ){
    if( !identical(c("co2","d13catm"), names(forcingInputs)[6:7]) ){
      stop('Please provide forcing data for co2 and d13catm in forcingInputs, if calculate_d13c = 1')
    }
  }else{
    if( !identical(c("co2"), names(forcingInputs)[6]) ){
      forcingInputs = cbind(forcingInputs[,1:5], matrix(350, ncol = 2, nrow = n_m))
    }
  }

  # Management
  if( is.null(managementInputs) ) {
    t_t = 0L
    n_man = 1L
    thin_mat = array(NA_real_, dim = c(1,5,n_sp))
  } else {

    sp_names <- 1:n_sp
    names( sp_names ) <- speciesInputs$species
    managementInputs[,1] = sp_names[managementInputs$species] # change sp names to integer

    t_t = as.integer( as.vector( table(managementInputs[,1]) ) )
    n_man = as.integer( max(t_t) )

    thin_mat <- merge( data.frame(species = rep(1:n_sp, each = n_man), thin_n = rep(1:n_man, times = n_sp)),
      cbind(data.frame(thin_n = sequence(t_t)), managementInputs), by=c('species', 'thin_n'), all = T)

    thin_mat <- thin_mat[order(thin_mat$species, thin_mat$thin_n),]

    thin_mat <- simplify2array(by(thin_mat[,3:7], thin_mat[,1], as.matrix))
  }

  # Replace the default parameters
  parameterReplaced = param.default['parameter']
  parameterReplaced[speciesInputs$species] <- NA_real_
  parameterReplaced[speciesInputs$species] <- param.default$default
  if( !is.null(parameterInputs) ){
    parameterReplaced[match(parameterInputs$parameter, parameterReplaced$parameter), ] <- parameterInputs
  }


  # Bias
  biasReplaced = bias.default['parameter']
  biasReplaced[speciesInputs$species] <- NA_real_
  biasReplaced[speciesInputs$species] <- as.numeric(bias.default$default)
  if( !is.null(biasInputs) ){
    biasReplaced[match(biasInputs$parameter, biasReplaced$parameter), ] <- biasInputs
  }


  # Run the model
  out <- f_3PG(
    siteInputs = as.matrix( siteInputs, nrow = 1, ncol = 8),
    speciesInputs = as.matrix( speciesInputs[,-1], nrow = n_sp, ncol = 7),
    forcingInputs = as.matrix( forcingInputs, nrow = n_m, ncol = 7),
    managementInputs = thin_mat,
    parameterInputs = as.matrix( parameterReplaced[,-1], nrow = 82, ncol = n_sp),
    biasInputs = as.matrix( biasReplaced[,-1], nrow = 30, ncol = n_sp),
    n_sp = n_sp,
    n_m = n_m,
    n_man = n_man,
    t_t = t_t,
    settings = set_def,
    sp_names = speciesInputs$species,
    df_out = df_out)

  return(out)

}

# Check the input data for consistency ------------------------------------
# The following checks of input data need to be performed
# If all required columns are provided
# If the parameters names are consistent with potential parameters

chk_input <- function(){
  eval.parent(quote({

    # Consistensy in order of the columns in the data
    if( !identical(c("latitude","soil_class","asw_i","asw_min","asw_max","year_i","month_i","altitude"), names(siteInputs)) ){
      stop( 'Columns names of the siteInputs table shall correspond to: latitude, soil_class, asw_i, asw_min, asw_max, year_i, month_i, altitude' )
    }

    if( !identical(c("species","year_p","month_p","fertility","biom_foliage","biom_root","biom_stem","n_trees"), names(speciesInputs)) ){
      stop( 'Columns names of the siteInputs table shall correspond to: species,year_p,month_p,fertility,biom_foliage,biom_root,biom_stem,n_trees' )
    }

    if( !identical(c("tmp_min","tmp_max","prcp","srad","frost_days"), names(forcingInputs)[1:5]) ){
      stop( 'First five columns names of the forcingInputs table shall correspond to: tmp_min,tmp_max,prcp,srad,frost_days' )
    }

    if( !is.null(managementInputs) ){
      if( !identical(c("species","age","n_trees","foliage","root","stem"), names(managementInputs)) ){
        stop( 'Columns names of the managementInputs table shall correspond to: species,age,n_trees,foliage,root,stem' )
      }
    }

    if( !is.null(parameterInputs) ){

      if( !identical(c("parameter"), names(parameterInputs)[1]) ){
        stop( 'First column name of the parameterInputs table shall correspond to: parameter' )
      }

      if( !all( parameterInputs$parameter %in% param.default$parameter) ){
        stop( paste0('Parameter input table shall contains only parameters presend in: ', paste(param.default$parameter, collapse = ','),'. Check `param_info`` for more details.' ))
      }

    }

    if( !is.null(biasInputs) ){

      if( !identical(c("parameter"), names(biasInputs)[1]) ){
        stop( 'First column name of the biasInputs table shall correspond to: parameter' )
      }

      if( !all( biasInputs$parameter %in% bias.default$parameter) ){
        stop( paste0('Bias input table shall contains only parameters presend in: ', paste(bias.default$parameter, collapse = ','),'. Check `param_info`` for more details.' ))
      }

    }


    # Consistency in namings of the species
    if( !is.null(parameterInputs) ){

      if( nrow(speciesInputs) != ncol(parameterInputs[,-1]) ){
        stop( 'Please provide parameterInputs for all of the species in the speciesInputs table' )
      }

      if( !identical(speciesInputs$species, colnames(parameterInputs[,-1])) ){
        stop( 'Names or order of species in speciesInputs does not correspond to names or order in parameterInputs' )
      }
    }

    if( !is.null(settings$correct_bias) ){
      if( settings$correct_bias == 1 ){

        if( is.null(biasInputs) ){
          stop( 'Please provide biasInputs table or change the setting to correct_bias = 0' )
        }

        if( nrow(speciesInputs) != ncol(biasInputs[,-1]) ){
          stop( 'Please provide biasInputs for all of the species in the speciesInputs table' )
        }

        if( !identical(speciesInputs$species, colnames(biasInputs[,-1])) ){
          stop( 'Names or order of species in speciesInputs does not correspond to names or order in biasInputs' )
        }
      }
    }


  }))
}
