#' Script is designed to create internal data
#' Sometimes functions need pre-computed data tables, like in case of Variable names and group
#' If you put these in data/ they’ll also be available to package users, which is not appropriate.
#' Instead, you can save them in R/sysdata.rda. For example, two coloured related packages,
#' munsell and dichromat, use R/sysdata.rda to store large tables of colour data.
#' You can use usethis::use_data() to create this file with the argument internal = TRUE:
#'   x <- sample(1000)
#'   usethis::use_data(x, mtcars, internal = TRUE)
#'
#'
options(digits=16)


# EU_MIXFOR ---------------------------------------------------------------
#' `EU MIXFOR`
d_site <- readxl::read_excel('data-raw/data.input.xlsx', sheet = 'site')
  
d_species <- readxl::read_excel('data-raw/data.input.xlsx', sheet = 'species')

d_climate <- readxl::read_excel('data-raw/data.input.xlsx', sheet = 'climate')

d_parameters <- readxl::read_excel('data-raw/data.input.xlsx', sheet = 'parameters')

d_sizeDist <- readxl::read_excel('data-raw/data.input.xlsx', sheet = 'sizeDist')

d_thinning <- readxl::read_excel('data-raw/data.input.xlsx', sheet = 'thinning')


#' `INFO data`
i_output <- readxl::read_excel('data-raw/data.default.xlsx', sheet = 'output')

i_parameters <- readxl::read_excel('data-raw/data.default.xlsx', sheet = 'parameters')

i_sizeDist <- readxl::read_excel('data-raw/data.default.xlsx', sheet = 'sizeDist')

#' `LITERATURE DATA`

param.db <- readxl::read_excel('data-raw/parameters_db.xlsx', sheet = 'Parameter_DB')
source.db <- readxl::read_excel('data-raw/parameters_db.xlsx', sheet = 'Source')
# Keep only the columnsthat currently are necessary
source.db <- source.db[, colnames(source.db) %in% c('source', 'source_full', 'year', 'link', 'region', 'country')]
param.db <- param.db[, !colnames(param.db) %in% c('par_range_available')]
# Merge and tidy the full version of db and overview
param.db_full <- merge(param.db, source.db, by.x = "source", by.y = "source")
column_full <- c("parset_id", "species",  "age", "type", "year", "region",
                 "country","notes",  "source","source_comments", "source_full",
                 "link", colnames(param.db)[8:ncol(param.db)] )
param.db_full <- param.db_full[, column_full]

i_parameters_lit <- param.db_full


#' `STORE the data`
usethis::use_data( d_site, d_species, d_climate, d_parameters, d_sizeDist, d_thinning, 
                   internal = FALSE, overwrite = TRUE)

usethis::use_data( i_output, i_parameters, i_sizeDist, i_parameters_lit,
                   internal = TRUE, overwrite = TRUE)
  
