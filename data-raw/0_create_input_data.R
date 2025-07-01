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
usethis::use_data( i_output, i_parameters, i_sizeDist, i_parameters_lit,
                   internal = TRUE, overwrite = TRUE)


# Default data ------------------------------------------------------------
#' `EU MIXFOR`
f_reg <- 'data-raw/data.input.xlsx'

d_site <- readxl::read_excel(f_reg, sheet = 'site')
d_species <- readxl::read_excel(f_reg, sheet = 'species')
d_climate <- readxl::read_excel(f_reg, sheet = 'climate')
d_parameters <- readxl::read_excel(f_reg, sheet = 'parameters')
d_sizeDist <- readxl::read_excel(f_reg, sheet = 'sizeDist')
d_thinning <- readxl::read_excel(f_reg, sheet = 'thinning')

# Create a named list using the actual variables
d_input <- list(
  site = d_site,
  species = d_species,
  climate = d_climate,
  parameters = d_parameters,
  sizeDist = d_sizeDist,
  thinning = d_thinning
)

# Save the list as a single data object
usethis::use_data(d_input, internal = FALSE, overwrite = TRUE)


# Regeneration data set ---------------------------------------------------
f_reg <- 'data-raw/data_input_regeneration.xlsx'

d_site_r <- readxl::read_excel(f_reg, sheet = 'site')
d_species_r <- readxl::read_excel(f_reg, sheet = 'species')
d_climate_r <- readxl::read_excel(f_reg, sheet = 'climate')
d_parameters_r <- readxl::read_excel(f_reg, sheet = 'parameters')
d_sizeDist_r <- readxl::read_excel(f_reg, sheet = 'sizeDist')
d_thinning_r <- readxl::read_excel(f_reg, sheet = 'thinning')

# Create a named list using the actual variables
d_regeneration <- list(
  site = d_site_r,
  species = d_species_r,
  climate = d_climate_r,
  parameters = d_parameters_r,
  sizeDist = d_sizeDist_r,
  thinning = d_thinning_r
)

# Save the list as a single data object
usethis::use_data(d_regeneration, internal = FALSE, overwrite = TRUE)



# Defoliation data set ----------------------------------------------------
f_reg <- 'data-raw/data_input_defoliation.xlsx'

# Read data from each sheet
d_site_d <- readxl::read_excel(f_reg, sheet = 'site')
d_species_d <- readxl::read_excel(f_reg, sheet = 'species')
d_climate_d <- readxl::read_excel(f_reg, sheet = 'climate')
d_parameters_d <- readxl::read_excel(f_reg, sheet = 'parameters')
d_sizeDist_d <- readxl::read_excel(f_reg, sheet = 'sizeDist')
d_thinning_d <- readxl::read_excel(f_reg, sheet = 'thinning')
d_defoliation_d <- readxl::read_excel(f_reg, sheet = 'defoliation')

# Create a named list using the actual variables
d_defoliation <- list(
  site = d_site_d,
  species = d_species_d,
  climate = d_climate_d,
  parameters = d_parameters_d,
  sizeDist = d_sizeDist_d,
  thinning = d_thinning_d,
  defoliation = d_defoliation_d
)

# Save the list as a single data object
usethis::use_data(d_defoliation, internal = FALSE, overwrite = TRUE)
