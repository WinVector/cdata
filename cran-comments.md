

## Test environments

### OSX
   

    R CMD check --as-cran cdata_1.2.0.tar.gz
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.2.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Windows


    devtools::check_win_devel()

#### Linux

    rhub::check_for_cran()
    2290#> About to run xvfb-run R CMD check --as-cran cdata_1.2.0.tar.gz
    2296#> * using R version 4.1.0 (2021-05-18)
    2297#> * using platform: x86_64-pc-linux-gnu (64-bit)
    2304#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    2305#> Maintainer: ‘John Mount ’
    2362#> Status: OK

    rhub::check_for_cran()
    1946#> About to run xvfb-run R CMD check --as-cran cdata_1.2.0.tar.gz
    1950#> * using R Under development (unstable) (2021-06-10 r80480)
    1951#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1958#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1959#> Maintainer: ‘John Mount ’
    2016#> Status: OK

## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md

# Notes

    "Zumel" and "coordinatized" are not misspelled.

