#' @title Check and prepare management information.
#' @description Prepares the management table and checks for consistency.
#'
#' @param thinning  table containing the information about thinnings. If there is no thinning, it must be \code{NULL}. The following columns are required:
#' \itemize{
#' \item species: species or cohort id/name.
#' \item age: age at which thinning is done.
#' \item stems_n: number of trees remaining after thinning
#' \item stem: type of thinning (above/below). Default is 1.
#' \item foliage: type of thinning (above/below). Default is 1.
#' \item root: type of thinning (above/below). Default is 1.
#' }
#' @param sp_names names of the species / cohorts used for the simulations. This is required whether `thinning=NULL` or if not all species are indicated in the `thinning` table. The `sp_names` must be identical to those from \code{species} table.
#'
#' @details This function prepares the thinning table for \code{\link{run_3PG}}.
#'
#' In case there is no thinning it will return empty 3-d array.
#'
#' In case there will be thinning it will return 3-d array, where one dimension correspond to each species.
#'
#' @return  a 3-dimentional array, where third dimention correspond to each species.
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_input}}, \code{\link{prepare_parameters}}, \code{\link{prepare_sizeDist}}, \code{\link{prepare_climate}}
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
    stop( 'sp_names must be provided according to the species table.' )
  }

  n_sp = length(sp_names)
  sp_id <- 1:n_sp
  names(sp_id) <- sp_names

  if( is.null(thinning) ){

    thinning = array(NA_real_, dim = c(1,5,n_sp))

  } else {

    if( !identical( c("species","age","stems_n","stem","root","foliage"), colnames(thinning) ) ){
      stop( 'Column names of the thinning table must correspond to: species, age, stems_n, stem, root, foliage' )
    }

    thinning = data.frame( thinning )

    # check whether the thinning above/below are within plausible range
    if( any(d_thinning[ c("stem","root","foliage") ] < 0 | d_thinning[ c("stem","root","foliage") ] > 5) ){
      stop( 'Thinning values for stem, root, foliage shall be in a range [0, 10]' )
    }

    thinning = thinning[thinning$species %in% sp_names, ]
    thinning$species = sp_id[thinning$species] # change sp names to integer
    thinning = thinning[order(thinning$species, thinning$age),] # order the age of the trees

    t_t = as.integer( as.vector( table(thinning[,1]) ) )
    n_man = as.integer( max(t_t) )

    thinning = merge( data.frame(species = rep(1:n_sp, each = n_man), thin_n = rep(1:n_man, times = n_sp)),
      cbind(data.frame(thin_n = sequence(t_t)), thinning), by=c('species', 'thin_n'), all = T)

    thinning = thinning[order(thinning$species, thinning$thin_n),]

    thinning = simplify2array(by(thinning[,3:7], thinning[,1], as.matrix))
  }

  if( n_sp > 1 ){
    dimnames(thinning)[[3]] = sp_names
  }


  return( thinning )
}
