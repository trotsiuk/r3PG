#' @title Check the species data for consistency
#' @description Prepares the species table, by checking whether the input information is consistent.
#'
#' @param species table containing the information about species level data. Each row corresponds to one species/cohort.
#' \itemize{
#' \item species: species or cohort id/name. It must be consistent with species names in \code{thinning}, \code{parameters} and \code{sizeDist} tables.
#' \item planted: year and month indicating when species was planted. Provided in form of year-month. E.g. "2000-01".
#' \item fertility: soil fertility for a given species. Range from 0 to 1.
#' \item stems_n: number of trees per ha.
#' \item biom_stem: stem biomass for a given species (Mg/ha).
#' \item biom_root: root biomass for a given species (Mg/ha).
#' \item biom_foliage: initial foliage biomass (Mg/ha). If this is a leafless period, provide the spring foliage biomass.
#' }
#'
#' @details This function check the species table for \code{\link{run_3PG}}.
#'
#' @return a data.frame with one row
#'
#' @seealso \code{\link{run_3PG}}, \code{\link{prepare_input}}, \code{\link{prepare_parameters}}, \code{\link{prepare_sizeDist}}, \code{\link{prepare_thinning}}, \code{\link{prepare_site}}
#'
#' @example inst/examples/prepare_species-help.R
#'
#' @export
#'
prepare_species <- function(
  species
){

  species = data.frame(species)

  if( !identical(c("species","planted","fertility","stems_n","biom_stem","biom_root","biom_foliage"), colnames(species)) ){
    stop( 'Columns names of the species table must correspond to: species, planted, fertility, stems_n, biom_stem, biom_root, biom_foliage' )
  }

  # Test for NA
  if( any( is.na(species) ) ){
    stop( "Climate table should not contain NAs" )
  }

  if( any(species$fertility > 1 | species$fertility < 0) ){
    stop( 'Fertility shall be within a range of [0:1]' )
  }

  if( any(species$stems_n < 0) ){
    stop( 'Stem number shall be greater than 0' )
  }

  if( any(species$biom_stem < 0) ){
    stop( 'Biomas stem shall be greater than 0' )
  }

  if( any(species$biom_root < 0) ){
    stop( 'Biomas root shall be greater than 0' )
  }

  if( any(species$biom_foliage < 0) ){
    stop( 'Biomas foliage shall be greater than 0' )
  }

  # Select final table
  species = species[,c("species","planted","fertility","stems_n","biom_stem","biom_root","biom_foliage")]

  return( species )
}