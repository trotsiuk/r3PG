[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/trotsiuk/r3PG.svg?branch=master)](https://travis-ci.org/trotsiuk/r3PG)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# r3PG

A R package that wraps the [3-PG model](https://3pg.forestry.ubc.ca)

### Development release 

If you want to install the current (development) version from this repository, run

```{r}
devtools::install_github(repo = "trotsiuk/r3PG", subdir = "pkg", dependencies = T, 
build_vignettes = T)
```

### Old (stable) releases

You can see previous releases [here](https://github.com/trotsiuk/r3PG/releases). Either download the binary (if provided) install via

```{r}
install.packages("/path/to/binary/r3PG_0.1.0.tar.gz", repos = NULL, type = "source")
```

Or install directly from the download link

```{r}
library(devtools)

install_url("https://github.com/trotsiuk/r3PG/releases/download/v0.1.0/r3PG_0.1.0.tar.gz", 
dependencies = T, build_vignettes = T)
```

## Basic usage

Load r3PG:

``` r
library(r3PG)
?r3PG
```

Run the exmaple with provided data

``` r
out <- run_3PG(
  siteInputs = site_eum, 
  speciesInputs = species_eum, 
  forcingInputs = climate_eum, 
  managementInputs = NULL,
  parameterInputs = parameters_eum, 
  biasInputs = bias_eum,
  settings = list(light_model = 2, transp_model = 2, phys_model = 2, 
    correct_bias = 0, calculate_d13c = 0),
  df_out = FALSE)


out_long <- transf_out( out )
```

Visualize the output

``` r

library(dplyr)
library(ggplot2)


sel_var <- c('biom_stem', 'biom_foliage', 'biom_root')


out_long %>%
  filter( variable %in% sel_var ) %>%
  ggplot( aes(date, value, color = species) ) +
  geom_line() +
  facet_wrap(~variable, scales = 'free') 
```
