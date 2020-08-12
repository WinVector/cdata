

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.1.7.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.1.7’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Windows

    rhub::check_for_cran()
    620#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    621#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    622#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    623#> setting R_REMOTES_STANDALONE to true
    624#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    625#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    626#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    627#> * using log directory 'C:/Users/USERWAqIQGgcsx/cdata.Rcheck'
    628#> * using R Under development (unstable) (2020-07-05 r78784)
    629#> * using platform: x86_64-w64-mingw32 (64-bit)
    630#> * using session charset: ISO8859-1
    631#> * using option '--as-cran'
    632#> * checking for file 'cdata/DESCRIPTION' ... OK
    633#> * checking extension type ... Package
    634#> * this is package 'cdata' version '1.1.7'
    635#> * package encoding: UTF-8
    636#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    637#> Maintainer: 'John Mount '
    693#> Status: OK
 
    devtools::check_win_devel()
    * using R Under development (unstable) (2020-08-10 r79000)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'cdata/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'cdata' version '1.1.7'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md

# Notes

    "Zumel" and "coordinatized" are not misspelled.

