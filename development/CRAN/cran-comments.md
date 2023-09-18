# Version 0.1.6

## Submission 1, 18.09.2023

This is a minor update of r3PG.

### Bug fixed

- replace real(kind=8) with real(kind=kind(0.0d0)) in the more modern source files #97

### New function and data sets

### Major changes

-  constrain the `frost_days` to the maximum days in month
    `prepare_climate` #68. Additional, in the main FORTRAN program
    f_frost is now calculated using the number of days per month (not
    standard 30 days as in original program). Consequently this lead to
    the deviation in the output of biomass to the third digit (see
    tests).

### Minor changes

- re-structuring the repository and moving the files to the main directory #95
- adding GitHub CI
    
    
# Version 0.1.5

## Submission 1, 04.07.2023

This is a minor update of r3PG.

### Bug fixed

-   update of the `r3pg_ll` function in the vignette #89. There were
    wrong default parameters used.
-   new mcmc run for the vignette.

### New function and data sets

### Major changes

-   constrain the `frost_days` to the maximum days in month
    `prepare_climate` #68. Additional, in the main FORTRAN program
    f_frost is now calculated using the number of days per month (not
    standard 30 days as in original program). Consequently this lead to
    the deviation in the output of biomass to the third digit (see
    tests).

### Minor changes

-   improved checks for the climate data (whether within hard ranges)
    #90

### Test environments

-   local MAC OS 13.4.1 (Monterey), R 4.0.2
-   <http://win-builder.r-project.org/> - oldrelease / devel / release
-   Linux (Travis CI) - oldrel / release / devel
-   rhub (using `rhub::check_for_cran()`, `rhub::check_with_valgrind()`)

### R CMD check results

0 errors ✓ \| 0 warnings ✓ \| 0 notes ✓

R CMD check succeeded.



# Version 0.1.4

## Submission 1, 19.05.2022

This is a minor update of r3PG.

### Bug fixed

-   minor bug fixes for the `prepare_input.R`, `prepare_climate.R`,
    `prepare_sizeDist.R` #69
-   ensuring that thinning values are withing plausible range
    `prepare_thinning.R` #75
-   correct ordering of species in `prepare_thinning.R` #70
-   insuring that the cumulative volume is not decreasing `md_3PG.f95`
    #63
-   likelihood function in the vignette contained wrong error values
    `r3pg_ll` #54
-   adjusting the vignette for the new `multidplyr` version #47

### New function and data sets

-   `get_parameters` Gets parameters sets for species from published
    studies
-   `param.db` Data.frame containing all parameters from the literature
    review.
-   `prepare_site` to check whether the site data are within the
    plausible range #73.
-   `prepare_species` to check whether the species data are within the
    plausible range #73.

### Minor changes

-   adding the LICENSE
-   renaming and exporting the `param.default`, `var.default` and
    `sizedist.default` internal data

### Test environments

-   local MAC OS 12.2.1 (Monterey), R 4.0.2
-   <http://win-builder.r-project.org/> - oldrelease / devel / release
-   Linux (Travis CI) - oldrel / release / devel
-   rhub (using `rhub::check_for_cran()`, `rhub::check_with_valgrind()`)

### R CMD check results

0 errors ✓ \| 0 warnings ✓ \| 0 notes ✓

R CMD check succeeded.

# Version 0.1.3

## Submission 1, 18.02.2021

This is a minor update of r3PG.

Bug fixed

-   continue simulation even if one of the cohort was removed earlier
    (was only an issue if `correct_bias = 1`)
-   simulation if one of the cohort appear later (sensu regeneration)

Minor changes

-   re-arangement some of the variables in the `fortran` code (no effect
    on input/output)

### Test environments

-   local MAC OS 10.15.7 (Catalina), R 4.0.2
-   <http://win-builder.r-project.org/> - oldrelease / devel / release
-   Linux (Travis CI) - oldrel / release / devel
-   rhub (using `rhub::check_for_cran()`, `rhub::check_with_valgrind()`)

### R CMD check results

0 errors ✓ \| 0 warnings ✓ \| 0 notes ✓

R CMD check succeeded.

# Version 0.1.2

## Submission 1, 01.06.2020

This is a minor update of r3PG.

> There was an error on memory allocation while checking with Valgrind.

-   Done

### Test environments

-   local MAC OS 10.14.1 (Mojave), R 3.6.1
-   <http://win-builder.r-project.org/> - oldrelease / devel / release
-   Linux (Travis CI) - oldrel / release / devel
-   rhub (using `rhub::check_for_cran()`, `rhub::check_with_valgrind()`)

### R CMD check results

0 errors ✓ \| 0 warnings ✓ \| 0 notes ✓

R CMD check succeeded.

# Version 0.1.1

## Submission 1, 19.05.2020

This is a minor update of r3PG, with adjustment to avoid failures with
parallel makes.

### Test environments

-   local MAC OS 10.14.1 (Mojave), R 3.6.1
-   <http://win-builder.r-project.org/> - oldrelease / devel / release
-   Linux (Travis CI) - oldrel / release / devel
-   rhub (using `rhub::check_for_cran()`)

### R CMD check results

0 errors ✓ \| 0 warnings ✓ \| 0 notes ✓

R CMD check succeeded.

# Version 0.1.0

## Submission 3

This is a new (re)submission to CRAN

CRAN MESSAGE

> Please shorten the title to a maximum of 65 characters.

-   Done

> Acronyms/Abbreviations can be used on their own in the title as long
> as they are explained in the description field.

-   I assume this is refferencing to 3-PG. It is explained in the
    description field now Physiological Processes Predicting Growth
    (3-PG). 3-PG is name of the model

> Please add spaces:
> (1997)[doi:10.1016/S0378-1127(97)00026-1](doi:10.1016/S0378-1127(97)00026-1){.uri}
> --\> (1997)
> [doi:10.1016/S0378-1127(97)00026-1](doi:10.1016/S0378-1127(97)00026-1){.uri}
> (2016)<doi:10.1016/j.ecolmodel.2015.07.010> --\> (2016)
> <doi:10.1016/j.ecolmodel.2015.07.010>

-   Done

### Test environments

-   local MAC OS 10.14.1 (Mojave), R 3.6.1
-   <http://win-builder.r-project.org/> - oldrelease / devel / release
-   Linux (Travis CI) - oldrel / release / devel
-   rhub (using `rhub::check_for_cran()`)

### R CMD check results

0 errors ✓ \| 0 warnings ✓ \| 0 notes ✓

R CMD check succeeded.

In addition, rhub platform gave the following note:

checking compilation flags used ... NOTE Compilation used the following
non-portable flag(s): '-mstackrealign'

checking for non-standard things in the check directory ... NOTE Found
the following files/directories: 'examples_i386' 'examples_x64'
'r3PG-Ex_i386.Rout' 'r3PG-Ex_x64.Rout' 'tests_i386' 'tests_x64'

## Submission 2

This is a new (re)submission to CRAN

CRAN MESSAGE

> Please use the description field to explain and elaborate on specific
> terms terms used in the title and description field. Please always
> explain all acronyms in the description text.

-   I have explained acronyms and short tersms in the descriptison.

> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file in the
> form authors (year) \<doi:...\> authors (year) \<arXiv:...\> authors
> (year, ISBN:...) or if those are not available: \<https:...\> with no
> space after 'doi:', 'arXiv:', 'https:' and angle brackets for
> auto-linking. (If you want to add a title as well please put it in
> quotes: "Title")

-   I have added the rederences to the DESCRIPTION

> Please add \value to .Rd files regarding exported methods and explain
> the functions results in the documentation. Please write about the
> structure of the output (class) and also what the output means. (If a
> function does not return a value, please document that too, e.g.
> \value{No return value, called for side effects} or similar)

-   I have added \value to each of the function.

### Test environments

-   local MAC OS 10.14.1 (Mojave), R 3.6.1
-   <http://win-builder.r-project.org/> - oldrelease / devel / release
-   Linux (Travis CI) - oldrel / release / devel
-   rhub (using `rhub::check_for_cran()`)

### R CMD check results

0 errors ✓ \| 0 warnings ✓ \| 0 notes ✓

R CMD check succeeded.

In addition, rhub platform gave the following note:

checking compilation flags used ... NOTE Compilation used the following
non-portable flag(s): '-mstackrealign'

checking for non-standard things in the check directory ... NOTE Found
the following files/directories: 'examples_i386' 'examples_x64'
'r3PG-Ex_i386.Rout' 'r3PG-Ex_x64.Rout' 'tests_i386' 'tests_x64'

## Submission 1

Hi, this is a new submission to CRAN

### Test environments

-   local MAC OS 10.14.1 (Mojave), R 3.6.1
-   <http://win-builder.r-project.org/> - oldrelease / devel / release
-   Linux (Travis CI) - oldrel / release / devel
-   rhub (using `rhub::check_for_cran()`)

### R CMD check results

0 errors ✓ \| 0 warnings ✓ \| 0 notes ✓

R CMD check succeeded.

In addition, rhub platform gave the following note:

> checking compilation flags used ... NOTE Compilation used the
> following non-portable flag(s): '-mstackrealign'

> checking for non-standard things in the check directory ... NOTE Found
> the following files/directories: 'examples_i386' 'examples_x64'
> 'r3PG-Ex_i386.Rout' 'r3PG-Ex_x64.Rout' 'tests_i386' 'tests_x64'
