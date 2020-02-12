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
install_url("https://github.com/trotsiuk/r3PGmix/releases/download/v0.1.0/r3PGmix_0.1.0.tar.gz", dependencies = T, build_vignettes = T)
library(r3PGmix)
?r3PGmix
```

### Development release 

If you want to install the current (development) version from this repository, run

```{r}
devtools::install_github(repo = "trotsiuk/r3PGmix", subdir = "pkg", dependencies = T, build_vignettes = T)
```

