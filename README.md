[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/biometry/rLPJGUESS.svg?branch=master)](https://travis-ci.org/biometry/rLPJGUESS)

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
install_url("https://github.com/trotsiuk/r3PGmix/releases/download/v0.1.0/rLPJGUESS_0.1.0.tar.gz", 
dependencies = T, build_vignettes = T)
library(r3PGmix)
?r3PGmix
```

### Development release 

If you want to install the current (development) version from this repository, run

```{r}
devtools::install_github(repo = "trotsiuk/r3PGmix", subdir = "pkg", 
dependencies = T, build_vignettes = T)
```
Below the status of the automatic Travis CI tests on the master branch 

[![Build Status](https://travis-ci.org/biometry/rLPJGUESS.svg?branch=master)](https://travis-ci.org/biometry/rLPJGUESS)


### Build the package yourself 

Clone the package to your computer, and build with hand or Rstudio. If you need help see here http://biometry.github.io/APES/R/R70-PackageDevelopment.html


In Rstudio, the vignette may not be built per default. You will turn this on in your project options, or run 

```{r}
devtools::build_vignettes("rLPJGUESS")
```
