

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.1.2.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.1.2’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking package namespace information ... OK
    Status: OK

### Windows

    rhub::check_for_cran()
    620#> * using R Under development (unstable) (2019-08-30 r77101)
    621#> * using platform: x86_64-w64-mingw32 (64-bit)
    622#> * using session charset: ISO8859-1
    623#> * using option '--as-cran'
    624#> * checking for file 'cdata/DESCRIPTION' ... OK
    625#> * checking extension type ... Package
    626#> * this is package 'cdata' version '1.1.2'
    627#> * package encoding: UTF-8
    628#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    629#> Maintainer: 'John Mount '
    685#> Status: OK

## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md


# Notes

    "Zumel" and "coordinatized" are not misspelled.

