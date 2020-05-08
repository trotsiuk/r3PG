# Version 0.1.0

## Submission 3

This is a new (re)submission to CRAN

CRAN MESSAGE

> Please shorten the title to a maximum of 65 characters.

- Done

> Acronyms/Abbreviations can be used on their own in the title as long as they are explained in the description field.

- I assume this is refferencing to 3-PG. It is explained in the description field now Physiological Processes Predicting Growth (3-PG). 3-PG is name of the model

> Please add spaces:
(1997)<doi:10.1016/S0378-1127(97)00026-1>
--> (1997) <doi:10.1016/S0378-1127(97)00026-1>
(2016)<doi:10.1016/j.ecolmodel.2015.07.010>
--> (2016) <doi:10.1016/j.ecolmodel.2015.07.010>

- Done

### Test environments

* local MAC OS 10.14.1 (Mojave), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel
* rhub (using `rhub::check_for_cran()`)

### R CMD check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded.

In addition, rhub platform gave the following note:

checking compilation flags used ... NOTE Compilation used the following non-portable flag(s): '-mstackrealign' 
     
checking for non-standard things in the check directory ... NOTE Found the following files/directories: 'examples_i386' 'examples_x64' 'r3PG-Ex_i386.Rout' 'r3PG-Ex_x64.Rout' 'tests_i386' 'tests_x64'

## Submission 2

This is a new (re)submission to CRAN

CRAN MESSAGE

> Please use the description field to explain and elaborate on specific terms terms used in the title and description field. Please always explain all acronyms in the description text.

- I have explained acronyms and short tersms in the descriptison.

> If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

- I have added the rederences to the DESCRIPTION

> Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means.
(If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)

- I have added \value to each of the function.

### Test environments

* local MAC OS 10.14.1 (Mojave), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel
* rhub (using `rhub::check_for_cran()`)

### R CMD check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded.

In addition, rhub platform gave the following note:

checking compilation flags used ... NOTE Compilation used the following non-portable flag(s): '-mstackrealign' 
     
checking for non-standard things in the check directory ... NOTE Found the following files/directories: 'examples_i386' 'examples_x64' 'r3PG-Ex_i386.Rout' 'r3PG-Ex_x64.Rout' 'tests_i386' 'tests_x64'
  
## Submission 1

Hi, this is a new submission to CRAN

### Test environments

* local MAC OS 10.14.1 (Mojave), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel
* rhub (using `rhub::check_for_cran()`)

### R CMD check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded.

In addition, rhub platform gave the following note:

> checking compilation flags used ... NOTE
Compilation used the following non-portable flag(s):
  '-mstackrealign'
     
> checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  'examples_i386' 'examples_x64' 'r3PG-Ex_i386.Rout' 'r3PG-Ex_x64.Rout'
  'tests_i386' 'tests_x64'
