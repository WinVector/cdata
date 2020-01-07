

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.1.4.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.1.4’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

     rhub::check_for_cran()
     466#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     467#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     468#> setting R_REMOTES_STANDALONE to true
     469#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     470#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     471#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     472#> * using log directory 'C:/Users/USERtoINOsAcZk/cdata.Rcheck'
     473#> * using R Under development (unstable) (2019-11-08 r77393)
     474#> * using platform: x86_64-w64-mingw32 (64-bit)
     475#> * using session charset: ISO8859-1
     476#> * using option '--as-cran'
     477#> * checking for file 'cdata/DESCRIPTION' ... OK
     478#> * checking extension type ... Package
     479#> * this is package 'cdata' version '1.1.4'
     480#> * package encoding: UTF-8
     481#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     482#> Maintainer: 'John Mount '
     538#> Status: OK

## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md


# Notes

    "Zumel" and "coordinatized" are not misspelled.

