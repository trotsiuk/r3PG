# see an overview of the existing parameter sets
get_parameters(df_out = 'overview')
get_parameters(df_out = 'overview', sp_names = c('Eucalyptus globulus', 'Pinus sylvestris' ) )

# see parameters and source information
get_parameters(df_out = 'source')
get_parameters(df_out = 'source', sp_names = c('Fagus sylvatica', 'Salix dasyclados' ) )

# see parameter sets with full information (species, source, parameters)
get_parameters(df_out = 'full')
get_parameters(df_out = 'full', sp_names = c('Fagus sylvatica', 'Pinus radiata' ) )

# obtain parameter sets in for some species in the format required by run_3PG
get_parameters(df_out = 'run', sp_names = c('Fagus sylvatica', 'Picea abies'))
get_parameters(df_out = 'run', sp_names = 'Fagus sylvatica 34' )
get_parameters(df_out = 'run', sp_names = c('Fagus sylvatica 9', 'Picea abies 37'))


