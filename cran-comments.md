### Test environments
* local OS X El Capitan with R 3.2.4
* ubuntu 12.04 (on travis-ci), R 3.2.4
* Winbuilder with R version 3.3.0 beta

### R CMD check results

0 errors | 0 warnings | 0 note

### Winbuilder results

1 Note regarding possible spelling errors in DESCRIPTION

This has been checed and:

* both BDF and EDF are correctly spelled

* BioSemi is correctly spelled

### Travis CI result

exited with 0

### devtools::release()  (Monday April 18)
I received the notification: R is out of date (3.2.4 vs 3.2.5)
However accoring to R itself and accoring to CRAN R is up to date (I am using OSX)

With respect to: Have you fixed all existing problems at 
http://cran.rstudio.com/web/checks/check_results_edfReader.html ?

There was an error with: r-patched-solaris-x86
Package suggested but not available for checking: ‘testthat’ 
I am afraid I cann't fix the absence of 'testthat'

There was an error with: r-oldrel-windows-ix86+x86_64
Sorry again, I donn't know how to fix this on an old platform


