#' @title Runs a 3-PG simulation
#'
#' @description This function runs the 3-PGpjs or 3-PGmix model
#'
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
#' \item species: species or cohort id/name. It shall be consistent with \code{parameterInputs} and \code{sizeDistInputs} table.
#' \item year_p: year when the species was planted (it is used to calculate species age at the time of simulation).
#' \item month_p: month when species was planted, assuming end of the month. E.g. if species is planted in January 2000, then in 31 April 2000 it will be 3 month.
#' \item fertility: soil fertility for a given species. Range from 0 to 1.
#' \item biom_foliage: initial foliage biomass (T/ha). If this is a leafless period provide a foliage biomass in summer.
#' \item biom_root: root biomass for a given species.
#' \item biom_stem: stem biomass for a given species.
#' \item n_trees: number of trees per ha.
#' }
#' @param forcingInputs  a \code{data frame} or \code{matrix} containing the information about climatic forcing data. First month shall corresponds to \code{year_i month_i} in the \code{siteInputs} table. The required variables are (tmp_min, tmp_max, prcp, srad, frost_days)
#' \itemize{
#' \item tmp_min: monthly mean daily minimum temperature (C).
#' \item tmp_max: monthly mean daily maximum temperature (C).
#' \item tmp_ave: monthly mean daily maximum temperature (C).
#' \item prcp: monthly rainfall (mm month-1).
#' \item srad: monthly mean daily solar radiation (MJ m-2 d-1).
#' \item frost_days: frost days per month (d month-1).
#' \item vpd_day: frost days per month (mbar).
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
#' @param parameterInputs a \code{data frame} or \code{matrix} containing the parameters to be modified. Values that are not provided are replaced by defaults.
#' \itemize{
#' \item parameter: name of the parameter, shall be consistent in naming with \code{\link{param_info}}
#' \item species: each column correspond to species/cohort id/name, as defined in \code{speciesInputs} table
#' }
#' @param sizeDistInputs  a \code{data frame} or \code{matrix} containing the size distribution parameters to be modified:
#' \itemize{
#' \item parameter: name of the parameter, shall be consistent in naming with \code{\link{sizeDist_info}}
#' \item species: each column shall correspond to species/cohort id/name, as defined in \code{speciesInputs} table
#' }
#' @param settings a list with settings for the model. Values that are not provided are replaced by defaults.
#' \itemize{
#' \item light_model: `1` - 3-PGpjs; `2` - 3-PGmix
#' \item transp_model: `1` - 3-PGpjs; `2` - 3-PGmix
#' \item phys_model:  `1` - 3-PGpjs; `2` - 3-PGmix
#' \item correct_sizeDist: `0` - no; `1` - yes
#' \item calculate_d13c: `0` - no; `1` - yes
#' }
#' @param df_out \code{logical} if the output shall be in the 4-dimentional array (FALSE) or long data.frame (TRUE)
#'
#' @details `r3PG` provides an implementation of the Physiological Processes Predicting Growth \href{https://3pg.forestry.ubc.ca}{3-PG} model (Landsberg & Waring, 1997), which simulate forest growth and productivity. The `r3PG` serves as a flexible and easy-to-use interface for the `3-PGpjs` (Sands, 2010) and the `3-PGmix` (Forrester & Tang, 2016) model written in `Fortran`. The package, allows for fast and easy interaction with the model, and `Fortran` re-implementation facilitates computationally intensive sensitivity analysis and calibration. The user can flexibly switch between various options and submodules, to use the original `3-PGpjs` model version for monospecific, even-aged and evergreen forests and the `3-PGmix` model, which can also simulate multi-cohort stands (e.g. mixtures, uneven-aged) that contain deciduous species.
#'
#' This implementation of 3pg includes several major variants / modifications of the model in particular the ability to switch between 3-PGpjs (the more classic model version for monospecific stands) vs. 3-PGmix (a version for mixed stands), as well as options for size distribution corrections and 13C calculations (see parameters).
#'
#' @note The \code{run_3PG} also check for the quality of input data. In case that names, or structure is not consitent with requirenments it will return an error.
#'
#' @return a 4-dimentional array or a data.frame, depending on the parameter df_out. The matrix can also be transformed via the \code{\link{transf_out}} function.
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
  sizeDistInputs = NULL,
  settings = list(light_model = 1, transp_model = 1, phys_model = 1, correct_sizeDist = 0, calculate_d13c = 0),
  df_out = TRUE
){

  #Check the input naming and structure
  chk_input()


  # replace default settings
  set_def = list(light_model = 1, transp_model = 1, phys_model = 1, correct_sizeDist = 0, calculate_d13c = 0)
  set_def[names(settings)] <- settings
  set_def <- as.integer( unlist(set_def) )

  n_sp = as.integer( nrow(speciesInputs) )
  n_m = as.integer( nrow(forcingInputs) )

  # Climate
  chk_climate()

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


  # sizeDist
  sizeDistReplaced = sizeDist.default['parameter']
  sizeDistReplaced[speciesInputs$species] <- NA_real_
  sizeDistReplaced[speciesInputs$species] <- as.numeric(sizeDist.default$default)
  if( !is.null(sizeDistInputs) ){
    sizeDistReplaced[match(sizeDistInputs$parameter, sizeDistReplaced$parameter), ] <- sizeDistInputs
  }


  # Run the model
  out <- f_3PG(
    siteInputs = as.matrix( siteInputs, nrow = 1, ncol = 8),
    speciesInputs = as.matrix( speciesInputs[,-1], nrow = n_sp, ncol = 7),
    forcingInputs = as.matrix( forcingInputs, nrow = n_m, ncol = 9),
    managementInputs = thin_mat,
    parameterInputs = as.matrix( parameterReplaced[,-1], nrow = 82, ncol = n_sp),
    sizeDistInputs = as.matrix( sizeDistReplaced[,-1], nrow = 30, ncol = n_sp),
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
    if( !identical(c("latitude","soil_class","asw_i","asw_min","asw_max","year_i","month_i","altitude"), colnames(siteInputs)) ){
      stop( 'Columns names of the siteInputs table shall correspond to: latitude, soil_class, asw_i, asw_min, asw_max, year_i, month_i, altitude' )
    }

    if( !identical(c("species","year_p","month_p","fertility","biom_foliage","biom_root","biom_stem","n_trees"), colnames(speciesInputs)) ){
      stop( 'Columns names of the siteInputs table shall correspond to: species,year_p,month_p,fertility,biom_foliage,biom_root,biom_stem,n_trees' )
    }

    if( !all(c("tmp_min","tmp_max","prcp","srad","frost_days") %in% colnames(forcingInputs)) ){
      stop( 'forcingInputs table shall include: tmp_min,tmp_max,prcp,srad,frost_days' )
    }

    if( !is.null(managementInputs) ){
      if( !identical(c("species","age","n_trees","foliage","root","stem"), colnames(managementInputs)) ){
        stop( 'Columns names of the managementInputs table shall correspond to: species,age,n_trees,foliage,root,stem' )
      }
    }

    if( !is.null(parameterInputs) ){

      if( !identical(c("parameter"), colnames(parameterInputs)[1]) ){
        stop( 'First column name of the parameterInputs table shall correspond to: parameter' )
      }

      if( !all( parameterInputs$parameter %in% param.default$parameter) ){
        stop( paste0('Parameter input table shall contains only parameters presend in: ', paste(param.default$parameter, collapse = ','),'. Check `param_info`` for more details.' ))
      }

    }

    if( !is.null(sizeDistInputs) ){

      if( !identical(c("parameter"), colnames(sizeDistInputs)[1]) ){
        stop( 'First column name of the sizeDistInputs table shall correspond to: parameter' )
      }

      if( !all( sizeDistInputs$parameter %in% sizeDist.default$parameter) ){
        stop( paste0('sizeDist input table shall contains only parameters presend in: ', paste(sizeDist.default$parameter, collapse = ','),'. Check `param_info`` for more details.' ))
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

    if( !is.null(settings$correct_sizeDist) ){
      if( settings$correct_sizeDist == 1 ){

        if( is.null(sizeDistInputs) ){
          stop( 'Please provide sizeDistInputs table or change the setting to correct_sizeDist = 0' )
        }

        if( nrow(speciesInputs) != ncol(sizeDistInputs[,-1]) ){
          stop( 'Please provide sizeDistInputs for all of the species in the speciesInputs table' )
        }

        if( !identical(speciesInputs$species, colnames(sizeDistInputs[,-1])) ){
          stop( 'Names or order of species in speciesInputs does not correspond to names or order in sizeDistInputs' )
        }
      }
    }


  }))
}


chk_climate <- function(){
  eval.parent(quote({

    if( any(is.na(forcingInputs)) ){
      stop( 'forcingInputs should not contain NA in any column' )
    }

    # add tmp_ave if missing
    if( !'tmp_ave' %in% colnames(forcingInputs) ){
      forcingInputs$tmp_ave = (forcingInputs$tmp_min + forcingInputs$tmp_max) / 2
    }

    # add VPD if missing
    if( !'vpd_day' %in% colnames(forcingInputs) ){

      vpd_max = 6.10780 * exp(17.2690 * forcingInputs$tmp_max / (237.30 + forcingInputs$tmp_max))
      vpd_min = 6.10780 * exp(17.2690 * forcingInputs$tmp_min / (237.30 + forcingInputs$tmp_min))

      forcingInputs$vpd_day = (vpd_max - vpd_min) / 2
    }

    # if
    if( set_def[5] == 1 ){
      if( !all(c("co2","d13catm") %in% colnames(forcingInputs)) ){
        stop('Please provide forcing data for co2 and d13catm in forcingInputs, if calculate_d13c = 1')
      }
    }else{
      if( !'co2' %in% colnames(forcingInputs) ){
        forcingInputs$co2 = 350
      }
      if( !'d13catm' %in% colnames(forcingInputs) ){
        forcingInputs$d13catm = -7.705222
      }

    }

    forcingInputs = forcingInputs[,c('tmp_min', 'tmp_max', 'tmp_ave', 'prcp', 'srad', 'frost_days', 'vpd_day', 'co2', 'd13catm')]

  }))
}
