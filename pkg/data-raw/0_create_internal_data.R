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
var.default <- read_excel('data-raw/info.default.xlsx', sheet = 'output')

param.default <- read_excel('data-raw/info.default.xlsx', sheet = 'parameters')

sizeDist.default <- read_excel('data-raw/info.default.xlsx', sheet = 'sizeDist')

# Build the param.db in full format
param.db <- read_excel('data-raw/parameters_db.xlsx', sheet = 'Parameter_DB')
source.db <- read_excel('data-raw/parameters_db.xlsx', sheet = 'Source')
  # Keep only the columnsthat currently are necessary
source.db <- source.db[, colnames(source.db) %in% c('source', 'source_full', 'year', 'link', 'region', 'country')]
param.db <- param.db[, !colnames(param.db) %in% c('par_range_available')]
  # Merge and tidy the full version of db and overview
param.db_full <- merge(param.db, source.db, by.x = "source", by.y = "source")
column_full <- c(colnames(param.db)[1:7], colnames(source.db)[2:ncol(source.db)], colnames(param.db)[8:ncol(param.db)])
column_ov <- c('parset_id', 'species', 'age', 'type'  )#, 'year' 'region', 'country'
column_source <- c(column_ov, 'year', 'region', 'country', 'notes', 'source',
                   'source_comments', 'link', 'source_full')
param.db_full <- param.db_full[, column_full]
param.db_ov <- param.db_full[,column_ov]
param.db_source <- param.db_full[,column_source]
 # Create run ready table
param.db_run <- as.data.frame(cbind(colnames(param.db)[8:ncol(param.db)],
                                    t(param.db[, 8:ncol(param.db)])))
colnames(param.db_run) <- c("parameter", paste(unlist(param.db[, 'species']),
                                               unlist(param.db[, 'parset_id'])))
rownames(param.db_run) <-  NULL
param.db_run[, 2:ncol(param.db_run)] <- apply(param.db_run[, 2:ncol(param.db_run)],
                                              MARGIN = 2,
                                              FUN = function(x)as.numeric(x))
rm(param.db)

param.db <- list(full = param.db_full, overview = param.db_ov,
                 run = param.db_run, source = param.db_source)

usethis::use_data(var.default, param.default, sizeDist.default,
                  param.db, internal = TRUE, overwrite = TRUE)


# visible data
i_output <- var.default
i_parameters <- param.default
i_sizeDist <- sizeDist.default

usethis::use_data( i_output, i_parameters, i_sizeDist, internal = FALSE, overwrite = TRUE)
