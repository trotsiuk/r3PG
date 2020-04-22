#' @name r3PG
#' @title Simulating Forest Growth using the 3-PG Process-Based Vegetation Model
#' @docType package
#' @description The r3PG package provides a flexible and easy-to-use interface for Fortran implementations of the 3-PGpjs and the 3-PGmix forest growth models. The user can flexibly switch between various options and submodules, to use the original 3-PGpjs model version for monospecific, even-aged and evergreen forests and the 3-PGmix model, which can also simulate multi-cohort stands (e.g. mixtures, uneven-aged) that contain deciduous species. The core function to run the model is \code{\link{run_3PG}}. For more background, please consult the vignette via vignette(package = "r3PG")
#' @seealso \code{\link{run_3PG}}
#' @example inst/examples/run_3PG-help.R
NULL
