tranf_vba <- function(sk = 132, n_m = 123, f = '../3PG_examples/3PGmix/ExampleMixtureRuns7.xls', s = 'Shitaioutput' ){
  #' @description tranforming the output of the Excel VBA 3PG run to match the long format
  #'
  #' @param sk number of lines to skip
  #' @param n_m number of lines to read
  #' @param f a location of the file
  #' @param s a sheet name

  readxl::read_xls( f, sheet = s, skip = sk, n_max = n_m) %>%
    # head() %>%
    rename( date = `Year & month`) %>%
    mutate( date = gsub("[[:space:]]", "", date) %>% anytime::anydate(.) ) %>%
    # mutate( date = date + months(1) - 1) %>%
    mutate( date = date - 1) %>%
    mutate_at( vars(-date), list( as.numeric) ) %>%
    gather( variable_vba, value, -date) %>%
    filter( !is.na( value) ) %>%
    # get species id
    mutate( species = stringr::str_sub(variable_vba, -1, -1)) %>%
    rowwise() %>%
    mutate(
      variable_vba =  if_else( is.na( as.numeric(species) ), variable_vba, gsub(species, '', variable_vba)),
      species = if_else( is.na( as.numeric(species) ), '1', species ),
      species = paste0('sp_', species),
      obs = 'vba') %>%
    ungroup() %>%
    inner_join(
      var_names %>%
        select(variable_vba, group = variable_group, variable = variable_name) %>% filter(!is.na(variable_vba)),
      by = 'variable_vba'
    ) %>%
    select(date, species, group, variable, value)
}
