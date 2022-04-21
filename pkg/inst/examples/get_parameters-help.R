# see an overview of the existing parameter sets
get_parameters(mode = 'overview')
get_parameters(mode = 'overview', sp_names = c('Eucalyptus globulus', 'Pinus sylvestris' ) )

# see existing parameter sets and comments
get_parameters(mode = 'comments')
get_parameters(mode = 'comments', sp_names = c('Eucalyptus globulus', 'Pinus sylvestris' ) )

# see parameters and source information
get_parameters(mode = 'source')

# obtain parameter sets in for some species in the format required by run_3PG
get_parameters(mode = 'parameters', sp_names = c('Fagus sylvatica', 'Picea abies'))
get_parameters(mode = 'parameters', sp_names = 'Fagus sylvatica 34' )
get_parameters(mode = 'sizeDist', sp_names = c('Fagus sylvatica 9', 'Picea abies 37'))

# see parameter sets with full information (species, source, parameters)
get_parameters(mode = 'full')
get_parameters(mode = 'full', sp_names = c('Fagus sylvatica', 'Pinus radiata' ) )


