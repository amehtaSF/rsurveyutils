
rsurveyutils
=============

<!-- badges: start -->
<!-- badges: end -->

The goal of rsurveyutils is to provide basic functionality for preprocessing and analyzing data.

<<<<<<< HEAD
Installation
--------------

=======
To install an R package from Github, one must first install the `devtools` package. Then, one can install R packages using the `install_github()` function.
>>>>>>> f755a499286ae5716cad4e325f02257bc004ba85
``` r
install.packages("devtools")
devtools::install_github("amehtaSF/rsurveyutils")
```

## Usage

See help files in R for usage of each function.


<<<<<<< HEAD
Todo
------
[ ] codebook_recoder doesn't work on nature study 2b project when installed from github, but works from globalenv. Particularly, the trycatch in as.numeric conversion
[ ] need tests for `cor_matrix_kbl()` and `recent_date_dir()`



Implementing new function in package
----------------------------------------

Following workflow from [R Packages](https://r-pkgs.org/index.html) e-book. 


* Load `library(usethis)` and `library(testthat)` and `library(devtools)`.
* Call `use_r()` function to create a matched script and test file in appropriate directories (`R/` and `tests/testthat/`). 
* Write function.
* Build, install, and attach package with new function using `load_all()`.
* Run `check()` to test that package fully works. (Also see `test()`.)
* Run `document()` to add documentation. 
* Commit to git. 
* Install package with `install()`.

Functions in packages
------------------------
=======
## Todo
* codebook_recoder produces inexplicable warnings in nature study 2b processing pipeline.
>>>>>>> f755a499286ae5716cad4e325f02257bc004ba85

* `codebook_recoder()`  
* `codebook_renamer()` 
* `tally_scale()` 
* `to_sd_categories()` 
* `cor_matrix_kbl()` 
  * TODO 
* `scale()` 
* `recent_date_dir()` 
