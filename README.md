[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/trotsiuk/r3PGmix.svg?branch=master)](https://travis-ci.org/trotsiuk/r3PGmix)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# r3PGmix

A R package that wraps the [3-PG model](https://3pg.forestry.ubc.ca)

### Recommended installation

Install the latest stable release from https://github.com/trotsiuk/r3PGmix/releases.

You can download the binary and install it as follows

```{r}
install.packages("/path/to/binary/r3PGmix_0.1.0.tar.gz", repos = NULL, type = "source")
```
Or you can install it directly from the download link

```{r}
library(devtools)
install_url("https://github.com/trotsiuk/r3PGmix/releases/download/v0.1.0/r3PGmix_0.1.0.tar.gz", 
dependencies = T, build_vignettes = T)
library(r3PGmix)
?r3PGmix
```

### Development release 

If you want to install the current (development) version from this repository, run

```{r}
devtools::install_github(repo = "trotsiuk/r3PGmix", subdir = "pkg", dependencies = T, 
build_vignettes = T)
```

## Basic usage

Load r3PGmix:

``` r
library(r3PGmix)
```

To run an example, load meteorological and soil data from the Solling
Beech Experimental site distributed with the package:

``` r
data("slb1_meteo")
data("slb1_soil")
```

Set up lists containing default model control options and model
parameters:

``` r
options.b90 <- setoptions_LWFB90()
param.b90 <- setparam_LWFB90()
```

Set start and end dates in model control options:

``` r
options.b90$startdate <- as.Date("2002-01-01")
options.b90$enddate <- as.Date("2003-12-31")
```

Derive soil hydraulic properties from soil physical properties using a
pedotransfer function:

``` r
soil <- cbind(slb1_soil, hydpar_puh2(clay = slb1_soil$clay,
                                     silt = slb1_soil$silt,
                                     sand = slb1_soil$sand,
                                     bd = slb1_soil$bd,
                                     oc.pct = slb1_soil$c_org))
```

Run LWF-Brook90 with the created model input objects and capture results
in `b90.results.slb1`:

``` r
b90.results.slb1 <- runLWFB90(project.dir = "example_run_b90/",
                              param.b90 = param.b90,
                              options.b90 = options.b90,
                              climate = slb1_meteo,
                              soil = soil)
str(b90.results.slb1, max.level = 1)