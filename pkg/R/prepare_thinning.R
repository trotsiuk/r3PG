#' @title Check and prepare management information.
#' @description This function prepares the management table and ckeck for consistensy.
#'
#' @param thinning  a \code{data frame} or \code{matrix} containing the information about management. In case there is no management it shall be equall to \code{NULL}. The following columns are required:
#' \itemize{
#' \item species: species or cohort id/name.
#' \item age: age at which management is done.
#' \item n_trees: number of trees remaining after management
#' \item foliage: type of management (above/below). Default is 1.
#' \item root: type of management (above/below). Default is 1.
#' \item stem: type of management (above/below). Default is 1.
#' }
#' @param sp_names names of the species / cohorsts used for the simulations. This is required to account if `thinning=NULL` or if not all species are indicated in the `thinning` table. The `sp_names` shall be identical to those from \code{species} table.
#'
#' @details This function prepares the thinning table for the model.
#'
#' In case there is no management it will return empty 3-d array.
#'
#' In case there will be management it will return 3-d array, where one dimention correspond to each species.
#'
#' @example inst/examples/prepare_thinning-help.R
#'
#' @export
#'
prepare_thinning <- function(
  thinning = NULL,
  sp_names = c('Fagus sylvatica', 'Pinus sylvestris')
){

  if( any( is.null(sp_names), is.na(sp_names), length(sp_names)==0L) ){
    stop( 'sp_names shall be provided according to the species table.' )
  }

  n_sp = length(sp_names)
  sp_names <- setNames( 1:n_sp, sp_names)

  if( is.null(thinning) ){

    thinning = array(NA_real_, dim = c(1,5,n_sp))

  } else {

    if( !identical( c("species","age","n_trees","foliage","root","stem"), colnames(thinning) ) ){
      stop( 'Column names of the thinning table shall correspond to: species, age, n_trees, foliage, root,stem' )
    }

    thinning = data.frame( thinning )
    thinning$species = sp_names[thinning$species] # change sp names to integer

    t_t = as.integer( as.vector( table(thinning[,1]) ) )
    n_man = as.integer( max(t_t) )

    thinning = merge( data.frame(species = rep(1:n_sp, each = n_man), thin_n = rep(1:n_man, times = n_sp)),
      cbind(data.frame(thin_n = sequence(t_t)), thinning), by=c('species', 'thin_n'), all = T)

    thinning = thinning[order(thinning$species, thinning$thin_n),]

    thinning = simplify2array(by(thinning[,3:7], thinning[,1], as.matrix))
  }


  dimnames(thinning)[[3]] = names(sp_names)

  return( thinning )
}
