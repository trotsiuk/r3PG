#' Script is designet to create internal data
#' Sometimes functions need pre-computed data tables, like in case of Variable names and group
#' If you put these in data/ they’ll also be available to package users, which is not appropriate.
#' Instead, you can save them in R/sysdata.rda. For example, two coloured related packages,
#' munsell and dichromat, use R/sysdata.rda to store large tables of colour data.
#' You can use usethis::use_data() to create this file with the argument internal = TRUE:
#'   x <- sample(1000)
#'   usethis::use_data(x, mtcars, internal = TRUE)
#'
#'

var.default <- read.table("data-raw/internal_data/var.default.csv", header = T, stringsAsFactors = F, sep = ",")

param.default <- read.table("data-raw/internal_data/param.default.csv", header = T, stringsAsFactors = F, sep = ",")

sizeDist.default <- read.table("data-raw/internal_data/sizeDist.default.csv", header = T, stringsAsFactors = F, sep = ",")



# Save the data -----------------------------------------------------------

usethis::use_data(var.default, param.default, sizeDist.default,
  internal = TRUE, overwrite = TRUE)



output_info <- var.default
param_info <- param.default
sizeDist_info <- sizeDist.default

usethis::use_data( output_info, param_info, sizeDist_info, internal = FALSE, overwrite = TRUE)
