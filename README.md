[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/trotsiuk/r3PG.svg?branch=master)](https://travis-ci.org/trotsiuk/r3PG)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/r3PG)](https://cran.r-project.org/package=r3PG)
[![](https://cranlogs.r-pkg.org/badges/grand-total/r3PG)](https://cran.r-project.org/package=r3PG)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)



## Purpose

`r3PG` provides an implementation of the Physiological Processes Predicting Growth ([3-PG](https://3pg.forestry.ubc.ca)) model (Landsberg & Waring, 1997), which simulate forest growth and productivity. The `r3PG` serves as a flexible and easy-to-use interface for the `3-PGpjs` (Sands, 2010) and the `3-PGmix` (Forrester & Tang, 2016) model written in `Fortran`. The package enables fast and easy interaction with the model, and the `Fortran` re-implementation facilitates computationally intensive sensitivity analysis and calibration. The user can flexibly switch between various options and submodules, to use the original `3-PGpjs` model version for monospecific, even-aged and evergreen forests and the `3-PGmix` model, which can also simulate multi-cohort stands (e.g. mixtures, uneven-aged) that contain deciduous species.

## Usage

Below is a basic example, for more extended examples please visit package [vignette](https://htmlpreview.github.io/?https://github.com/trotsiuk/r3PG/blob/master/pkg/vignettes/r3PG-ReferenceManual.html).

The main function is `run_3PG()` which returns all 108 simulated variables for each species at a monthly time-step, either as a 4-dimentional array or a long format data frame.

```r
library(r3PG)
out_3PG <- run_3PG(
  site        = d_site, 
  species     = d_species, 
  climate     = d_climate, 
  thinning    = d_thinning,
  parameters  = d_parameters, 
  size_dist   = d_sizeDist,
  settings    = list(light_model = 2, transp_model = 2, phys_model = 2, 
                height_model = 1, correct_bias = 0, calculate_d13c = 0),
  check_input = TRUE, df_out = TRUE)

head( out_3PG )
```

To visualize the output:
``` r
library(dplyr)
library(ggplot2)

sel_var <- c('biom_stem', 'biom_foliage', 'biom_root')

out_3PG %>%
  filter( variable %in% sel_var ) %>%
  ggplot( aes(date, value, color = species) ) +
  geom_line() +
  facet_wrap(~variable, scales = 'free') +
  theme_classic()
```

If you prefer to use data stored in `Excell`, you can use the following example. Data to reproduce this example are stored in [data-raw/internal_data/data.input.xlsx](https://github.com/trotsiuk/r3PG/blob/master/pkg/data-raw/).

``` r
library(readxl)

f_loc <- 'data.input.xlsx'

run_3PG(
  site        = read_xlsx(f_loc, 'site'),
  species     = read_xlsx(f_loc, 'species'),
  climate     = read_xlsx(f_loc, 'climate'),
  thinning    = read_xlsx(f_loc, 'thinning'),
  parameters  = read_xlsx(f_loc, 'parameters'), 
  size_dist   = read_xlsx(f_loc, 'sizeDist'),
  settings    = list(light_model = 2, transp_model = 2, phys_model = 2, 
                height_model = 1, correct_bias = 0, calculate_d13c = 0),
  check_input = TRUE, df_out = TRUE)
```

## Installation 

### Stable release

`r3PG` is available for instalation from [CRAN](https://cran.r-project.org/web/packages/r3PG/index.html) 

```r
install.packages("r3PG")
```

### Development release

To install the current (development) version from the repository, run the following command:

```r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github(repo = "trotsiuk/r3PG", subdir = "pkg", build_vignettes = T)
```

The unit test status of the master (development) branch is [![Build Status](https://travis-ci.org/trotsiuk/r3PG.svg?branch=master)](https://travis-ci.org/trotsiuk/r3PG)

## Other 3-PG implementations in R

We would like to acknowledge that r3PG is not the only 3-PG implementations in R. We are aware of the following other packages:

| Maintainer          | Source |
| ------------------- | ------ |
| Daniel M. Griffith  | https://github.com/griffithdan/r3PG |
| Georgios Xenakis    | https://github.com/drGeorgeXenakis/fr3PGD |
| Francesco Minunno   | https://github.com/checcomi/threePGN-package |
| Quinn Thomas        | https://github.com/EcoDynForecast/DAPPER |

We explain in a recent publication (Trotsiuk et al, submitted) how this r3PG packages differs and / or improves over these. 

## Issues, suggestions, contributions

Please submit issues, bugs and suggestions in the dedicated [page](https://github.com/trotsiuk/r3PG/issues). Contribution and improvements are always welcome!

## Author and contact

[Volodymyr Trotsiuk; ](volodymyr.trotsiuk@wsl.ch)
[Florian Hartig; ](mailto:florian.hartig@biologie.uni-regensburg.de)
[David I. Forrester](mailto:david.forrester@wsl.ch)


## Citation
Trotsiuk, V., Hartig, F., Forrester, D.I. (2020). r3PG – an R package for simulating forest growth using the 3-PG process-based model. Methods Ecol. Evol., 11, 1470–1475. https://doi.org/10.1111/2041-210X.13474


## References

Forrester, D. I., & Tang, X. (2016). Analysing the spatial and temporal dynamics of species interactions in mixed-species forests and the effects of stand density using the 3-PG model. Ecological Modelling, 319, 233–254. https://doi.org/10.1016/j.ecolmodel.2015.07.010

Landsberg, J. J., & Waring, R. H. (1997). A generalised model of forest productivity using simplified concepts of radiation-use efficiency, carbon balance and partitioning. Forest Ecology and Management, 95(3), 209–228. https://doi.org/10.1016/S0378-1127(97)00026-1

Sands, P. J. (2010). 3PGpjs user manual. Retrieved from https://3pg.sites.olt.ubc.ca/files/2014/04/3PGpjs_UserManual.pdf

Trotsiuk, V., Hartig, F., Cailleret, M., Babst, F., Forrester, D. I., Baltensweiler, A., … Schaub, M. (2020). Assessing the response of forest productivity to climate extremes in Switzerland using model–data fusion. Global Change Biology, 26(4), 2463–2476. https://doi.org/10.1111/gcb.15011
