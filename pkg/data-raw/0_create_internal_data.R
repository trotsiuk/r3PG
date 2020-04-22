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

library(dplyr)
library(readxl)

options(digits=16)

# internal data
var.default <- read_excel('data-raw/internal_data/info.default.xlsx', sheet = 'output')

param.default <- read_excel('data-raw/internal_data/info.default.xlsx', sheet = 'parameters')

sizeDist.default <- read_excel('data-raw/internal_data/info.default.xlsx', sheet = 'sizeDist')

usethis::use_data(var.default, param.default, sizeDist.default, internal = TRUE, overwrite = TRUE)


# visible data
i_output <- var.default
i_parameters <- param.default
i_sizeDist <- sizeDist.default

usethis::use_data( i_output, i_parameters, i_sizeDist, internal = FALSE, overwrite = TRUE)
