# Version 0.1.0

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